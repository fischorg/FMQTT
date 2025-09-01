namespace FMQTT

module Togglers =
    let MakeTogglerBase (mqtt: MqttConnection) name =
        let names = MQTTObservable.CreateRetainedStringList mqtt ignore $"Toggles/.Names"
        let observableToggler = MQTTObservable.CreateRetainedBool mqtt ignore true $"Toggles/{name}"
        if names.Value.Contains name |> not then
            names.Value.Add name
            names.Publish()
        observableToggler
