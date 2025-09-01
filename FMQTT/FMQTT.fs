namespace FMQTT
//Op: Auto
open System
open System.Collections.Generic
open System.Diagnostics
open System.Threading
open System.Threading.Tasks

open MQTTnet.Client
open MQTTnet.Protocol

open ExceptionalCode
open MQTTnet
open Utils
//Op: End
[<AutoOpen>]
module FMQTTCore =
    let (|AsPayloadString|) (x: MqttApplicationMessageReceivedEventArgs) =
        x.ApplicationMessage.PayloadSegment
        |> ToArray

    [<Obsolete("Please update this operator to |& to avoid order of operations bugs")>]
    let (|--) a b = a |> tee b

    type ClientModel<'a> =
        {
            NoLocal : bool
            Retain : bool
            Topic: string
            OnChangeWeak: string -> unit
            OnChangeStrong: 'a -> unit
            SendOnSubcribe: MqttRetainHandling
        }
        static member Create (topic: string) =
            {
                NoLocal = false
                Retain = false
                OnChangeStrong = ignore
                OnChangeWeak = ignore
                SendOnSubcribe = MqttRetainHandling.DoNotSendOnSubscribe
                Topic = topic.TrimStart('/')
            }

    type ClientBuilder =
        static member NoLocal (b: ClientModel<_>)         = { b with NoLocal        = true }
        static member SendOnSubcribe (b: ClientModel<_>)  = { b with SendOnSubcribe = MqttRetainHandling.SendAtSubscribeIfNewSubscriptionOnly }
        static member Retain (b: ClientModel<_>)          = { b with Retain         = true }
        static member Topic x (b: ClientModel<_>)         = { b with Topic          = x }
        static member OnChange fn (b: ClientModel<'a>)    = { b with OnChangeStrong = fn }

    let _connections = new List<unit -> unit>()
    let AddConnection x = _connections.Add x
    let runAll x =
        let connections = new List<unit -> unit>(_connections)
        connections
        |> Seq.iter (fun x -> x())

    let t = new System.Timers.Timer(float 100)
    t.Elapsed.Add runAll
    t.Start()

    type MqttConnectionStatus =
        | New
        | Connecting
        | Connected
    type MqttConnection =
        {
            mutable Status: MqttConnectionStatus
            GUID: string
            BrokerName: string
            Factory: MqttFactory
            Client: IMqttClient
            OptionsBuilder: MqttClientOptionsBuilder
            EventHandlers_: System.Collections.Concurrent.ConcurrentDictionary<string, List<MqttApplicationMessageReceivedEventArgs -> unit>>
        }
        static member New =
            let factory = new MqttFactory()
            let client = factory.CreateMqttClient()
            {
                //Op: Auto
                Status         = MqttConnectionStatus.New
                GUID           = (System.Guid.NewGuid()).ToString()
                BrokerName     = ""
                Factory        = factory
                Client         = client
                OptionsBuilder = new MqttClientOptionsBuilder()
                EventHandlers_ = new System.Collections.Concurrent.ConcurrentDictionary<string, List<MqttApplicationMessageReceivedEventArgs -> unit>>()
                //Op: End
            }
            |& fun x ->
                    x.Client.add_ApplicationMessageReceivedAsync(fun (eventArgs: MqttApplicationMessageReceivedEventArgs) ->
                        if x.EventHandlers_.ContainsKey eventArgs.ApplicationMessage.Topic then
                            TryRepeatedly
                                10
                                10
                                (fun () ->
                                    x.EventHandlers_.[eventArgs.ApplicationMessage.Topic]
                                    |> Seq.toList
                                    |> List.iter (fun topicHandler -> topicHandler eventArgs)
                            )
                            |> ignore
                        Task.CompletedTask
                    )
            |& fun x -> x.EnsureConnected |> AddConnection
        static member SetBrokerName (bn: string) (mq: MqttConnection) = {mq with BrokerName = bn}
        static member SetClientId (clientId: string) (mq: MqttConnection) = {mq with OptionsBuilder = mq.OptionsBuilder.WithClientId(clientId)}
        static member SetUrl (url: string) (port: int) (mq: MqttConnection) = {mq with OptionsBuilder = mq.OptionsBuilder.WithTcpServer(url, port)}
        static member SetCredentials (user: string) (pass: string) (mq: MqttConnection) = {mq with OptionsBuilder = mq.OptionsBuilder.WithCredentials(user, pass)}
        static member UseTLS (mq: MqttConnection) =
            {
                mq with
                    OptionsBuilder =
                        let q = new MqttClientTlsOptions()
                        q.UseTls <- true
                        mq.OptionsBuilder.WithTlsOptions(q)
            }
        static member WithQOS qos (mq: MqttConnection) = {mq with OptionsBuilder = mq.OptionsBuilder.WithWillQualityOfServiceLevel qos}

        member this.EnsureConnected() =
            let rec connect depth mq =
                let _ = mq.Client.IsConnected
                if depth > 50 then failwith "Not that deep"
                try
                    let o = mq.OptionsBuilder.Build()
                    o.ProtocolVersion <- Formatter.MqttProtocolVersion.V500
                    mq.Client.ConnectAsync(o, CancellationToken.None).Wait()
                with ex ->
                    match ex with
                    | :? AggregateException as ex ->
                        ex.InnerExceptions
                        |> Seq.tryPick
                            ^ function
                                | :? InvalidOperationException as ex when ex.Message = "Not allowed to connect while connect/disconnect is pending." -> Some depth
                                | _ -> None
                        |> function
                        | Some depth -> depth + 1
                        | None -> depth + 1
                    | _ -> depth + 1
                    |> fun (newDepth: int) ->
                        if newDepth < 10 then
                            Thread.Sleep 100
                            connect newDepth mq
            if not this.Client.IsConnected then
                try
                    connect 0 this
                with ex -> ()

        static member Connect (mq: MqttConnection) =

            mq.EnsureConnected()
            mq

        member private this.AddEventBase topic fn : unit =
            let handlers = this.EventHandlers_
            if handlers.ContainsKey topic |> not then
                let list = new List<MqttApplicationMessageReceivedEventArgs -> unit>()
                if isNull list then failwith "Null list"
                if isNull topic then failwith "Null topic"
                if isNull handlers then failwith "Null handlers"
                try
                    handlers.TryAdd(topic, list) |> ignore
                with ex ->
                    let _ = 5
                    if isNull list then failwith "Null list"
                    if isNull topic then failwith "Null topic"
                    if isNull handlers then failwith "Null handlers"
            handlers.[topic].Add (fun x -> x |> fn)

        member private this.AddEvent model =
            this.AddEventBase model.Topic (fun m -> m.ApplicationMessage.ConvertPayloadToString() |> model.OnChangeWeak)

        member this.SubscribeToTopicWithModel (model: ClientModel<_>) =
            let subOptions =
                this.Factory.CreateSubscribeOptionsBuilder()
                |> fun x ->
                    MqttTopicFilterBuilder()
                    |> fun x -> x.WithRetainHandling model.SendOnSubcribe
                    |> fun x -> x.WithRetainAsPublished model.Retain
                    |> fun x -> x.WithTopic model.Topic
                    |> fun x -> x.WithNoLocal model.NoLocal
                    |> x.WithTopicFilter
                    |> fun x -> x.Build()
            this.EnsureConnected()
            this.Client.SubscribeAsync(subOptions, CancellationToken.None).Wait()
            this.AddEvent model

        member this.UnsubscribeFromTopic (topic: string) =
            this.Client.UnsubscribeAsync(topic).Wait()

        member this.SubscribeToTopic (topic: string) (fn: MqttApplicationMessageReceivedEventArgs -> unit) =
            this.AddEventBase topic fn
            this.Client.SubscribeAsync(topic).Wait()

        member this.SubscribeToTopicBasic (topic: string) (fn: string -> unit) = this.SubscribeToTopic topic (fun x -> x.ApplicationMessage.ConvertPayloadToString() |> fn)

        member this.PublishMessage (topic: string) (data: string) =
            let topic = topic.Replace("+", "_PLUS_")
            let amb = (new MqttApplicationMessageBuilder()).WithRetainFlag().WithTopic(topic).WithPayload(data).Build()
            try
                this.Client.PublishAsync(amb, CancellationToken.None) |> ignore
            with ex ->
                this.Client.ConnectAsync(this.OptionsBuilder.Build(), CancellationToken.None).Wait()
                this.PublishMessage topic data

        static member GetEnvVars() =
            let envVar n =
                let getVar userLevel =
                    let k = $"MQTT_{n}"
                    let envVars = Environment.GetEnvironmentVariables(userLevel)
                    if envVars.Contains k then Some <| envVars.[k].ToString()
                    else None
                getVar EnvironmentVariableTarget.User
                |> function
                | None -> getVar EnvironmentVariableTarget.Machine
                | Some x -> Some x
                |> Option.defaultValue ""
            {|
                URL = envVar "URL"
                Port = envVar "Port" |> parseInt |> function Some x -> x | _ -> 0
                User = envVar "User"
                Password = envVar "Password"
            |}

        static member ConnectToEnvironmentMQTT() =
            let vars = MqttConnection.GetEnvVars()
            MqttConnection.New
            |> MqttConnection.SetUrl vars.URL vars.Port
            |> MqttConnection.WithQOS MqttQualityOfServiceLevel.AtLeastOnce
            |> MqttConnection.SetCredentials  vars.User vars.Password
            |> MqttConnection.Connect

    type [<AbstractClass>]ObservableGeneric<'a when 'a: equality> internal () =
        member val internal backingValue : 'a option = None with get, set
        member val internal initVal : 'a option = None with get, set
        member val internal serializer : 'a -> string = (fun x -> x.ToString()) with get, set
        member val internal deserializer : string -> 'a = (fun _ -> failwith "needs fn") with get, set
        member val PrevValue : 'a option = None with get, set
        member internal this.SetBackingValue v = this.backingValue <- Some v
        member this.InitialValue() = this.initVal.Value
        member this.SetValue v = this.Value <- v

        abstract Value : 'a with get, set

    type MQTTObservableGeneric<'a when 'a: equality> internal () =
        inherit ObservableGeneric<'a>()
        interface IDisposable with
            member this.Dispose() =
                this.client.Value.UnsubscribeFromTopic this.clientModel.Topic
        member val internal clientModel = ClientModel.Create<'a> "" with get, set
        member val internal hasReceivedCallback : bool = false with get, set
        member val internal client : MqttConnection option = None with get,set
        member this.Topic() : string = this.clientModel.Topic
        member this.Init() : unit =
            let onChange (stringToDeserialize: string) : unit =
                let noopp x = x
                try
                    this.deserializer stringToDeserialize
                with ex ->
                    noopp ex |> ignore
                    this.initVal.Value
                |> this.SetBackingValue
                this.backingValue.Value
                |& fun nv ->
                    match this.PrevValue, nv with
                    | None, nv -> this.clientModel.OnChangeStrong nv
                    | Some xx, nv when xx <> nv -> this.clientModel.OnChangeStrong nv
                    | Some _, _ -> () //Skip, no change
                |> fun x -> this.PrevValue <- Some x

                this.hasReceivedCallback <- true

            {
                this.clientModel with
                    OnChangeWeak = onChange
            }
            |> this.client.Value.SubscribeToTopicWithModel

        member this.Publish() =
            this.client.Value.PublishMessage this.clientModel.Topic (this.serializer this.backingValue.Value)

        static member GetValueFromEXE topic timeout (onOutput: (string -> unit) option) (onError: (string -> unit) option) (cleanOutputFn: string list -> string list) (_: ProcessStartInfo) =
            let _ = MqttConnection.GetEnvVars()
            let psi = new ProcessStartInfo(@"mosquitto_sub.exe")
            psi.Arguments <- $"-h localhost -t {topic}"
            RunProcessStartInfoWithOutputFullest timeout onOutput onError cleanOutputFn psi
            |> ignore
            ()

        member this.WaitForCallback (ms: int) =
            let start = DateTime.Now
            let ms = (float ms)

            while this.hasReceivedCallback |> not && DateTime.Now.Subtract(start).TotalMilliseconds < ms do
                Thread.Sleep 10

        override this.Value
            with get() : 'a =
                this.backingValue
                |> function
                | Some x -> x
                | None ->
                    match this.initVal with
                    | Some x ->
                        this.SetBackingValue x
                        x
                    | None -> failwith "Init Value is not set yet"

            and set(newValue: 'a) =
                this.SetBackingValue newValue
                this.Publish()

        static member Create mqttConnection (serialize: 'a -> string) (deserialize: string -> 'a) (defaultValue: 'a) (client: ClientModel<'a>) =
            let ob = new MQTTObservableGeneric<'a>()
            ob.clientModel <- client
            ob.serializer <- serialize
            ob.deserializer <- deserialize
            ob.initVal <- Some defaultValue
            ob.client <- Some mqttConnection
            ob.Init()
            client.SendOnSubcribe
            |> function
            | MqttRetainHandling.SendAtSubscribe
            | MqttRetainHandling.SendAtSubscribeIfNewSubscriptionOnly -> ob.WaitForCallback 1000
            | MqttRetainHandling.DoNotSendOnSubscribe -> ob.Value <- defaultValue
            | _ -> ()
            ob

        static member CreateRetained<'a> mqttConnection (serialize: 'a -> string) (deserialize: string -> 'a) (onChange: 'a -> unit) (defaultValue: 'a) (topic: string) : MQTTObservableGeneric<'a> =
            ClientModel.Create<'a> topic
            |> ClientBuilder.SendOnSubcribe
            |> ClientBuilder.Retain
            |> ClientBuilder.OnChange onChange
            |> MQTTObservableGeneric.Create mqttConnection serialize deserialize defaultValue

        static member CreateRetainedBool (mqttConnection: MqttConnection) (onChange: bool -> unit) defaultValue topic : MQTTObservableGeneric<bool> =
            mqttConnection.EnsureConnected()
            MQTTObservableGeneric.CreateRetained<bool>
                mqttConnection
                (fun (x: bool) -> x.ToString())
                (fun s ->
                    Boolean.TryParse s
                    |> function
                    | true, x -> x
                    | false, _ -> false
                    )
                onChange
                defaultValue
                topic

        static member CreateRetainedBoolAction (mqttConnection: MqttConnection) (onChange: Action<bool>) defaultValue topic : MQTTObservableGeneric<bool> =
            MQTTObservableGeneric<bool>.CreateRetainedBool mqttConnection (fun x -> onChange.Invoke x) defaultValue topic

    type MQTTObservable =
        static member private CreateWithSerializers s d (mqttConnection: MqttConnection) (onChange: 'a -> unit) defaultValue topic=
            mqttConnection.EnsureConnected()
            MQTTObservableGeneric.CreateRetained<'a>
                mqttConnection
                s
                d
                onChange
                defaultValue
                topic

        static member CreateRetainedString (mqttConnection: MqttConnection) (onChange: string -> unit) defaultValue topic : MQTTObservableGeneric<string> =
            MQTTObservable.CreateWithSerializers
                id
                id
                mqttConnection
                onChange
                defaultValue
                topic

        static member CreateRetainedInt (mqttConnection: MqttConnection) (onChange: int -> unit) defaultValue topic : MQTTObservableGeneric<int> =
            MQTTObservable.CreateWithSerializers
                (fun i -> i.ToString())
                (fun i ->
                    Int32.TryParse i
                    |> function
                    | true, i -> i
                    | false, _ -> 0)
                mqttConnection
                onChange
                defaultValue
                topic

        static member CreateRetainedStringList (mqttConnection: MqttConnection) (onChange: List<string> -> unit) topic : MQTTObservableGeneric<List<string>> =
            let newList = new List<string>()
            MQTTObservable.CreateWithSerializers
                System.Text.Json.JsonSerializer.Serialize
                (fun x ->
                    if (ns x) = "" then
                        newList
                    else
                        System.Text.Json.JsonSerializer.Deserialize<List<string>> x
                )
                mqttConnection
                onChange
                newList
                topic

        static member CreateRetainedBool (mqttConnection: MqttConnection) (onChange: bool -> unit) defaultValue topic : MQTTObservableGeneric<bool> =
            MQTTObservable.CreateWithSerializers
                str
                (
                    Boolean.TryParse
                    >> function
                    | true, x -> x
                    | false, _ -> false
                )
                mqttConnection
                onChange
                defaultValue
                topic
        static member CreateRetainedWide (mqttConnection: MqttConnection) (onChange: obj -> unit) defaultValue topic : MQTTObservableGeneric<obj> =
            MQTTObservable.CreateWithSerializers
                str
                (
                    Boolean.TryParse
                    >> function
                    | true, x -> x
                    | false, _ -> false
                )
                mqttConnection
                onChange
                defaultValue
                topic