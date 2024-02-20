open System

open weather.lib
open std

[<Struct>]
type CliArgs =
    | Coordinates of coordinates: struct (float * float)
    | Address of address: string

let tryGetArgsFromEnv () =
    let names =
        [ ["LAT"; "LON"] 
          ["LOCAL_ADDRESS"] ]
    
    let tryGetLatLon () =
        struct (Env.var "LAT", Env.var "LON")

    let tryGetLocalAddr () =
        Env.var "LOCAL_ADDRESS"

    let rec tryGet names =
        match names with
        | ["LAT"; "LON"] :: tail ->
            match tryGetLatLon () with
            | (ValueSome lat, ValueSome lon) ->
                ValueSome <| (Coordinates <| struct (float lat, float lon))
            | _ ->
                tryGet tail
        | ["LOCAL_ADDRESS"] :: tail ->
            match tryGetLocalAddr () with
            | ValueSome localAddress ->
                ValueSome <| (Address localAddress)
            | _ -> 
                ValueNone (* tryGet tail *)
        | _ ->
            ValueNone
    tryGet names

type InvalidCliArgumentsException() =
    inherit InvalidOperationException()

let invalidCliArgs () =
    raise <| InvalidCliArgumentsException()

let printUsage () =
    let message = """usage:
weather address"""
    printfn "%s" message

let cliArgsOfArgv (argv: string[]) =
    match argv with
    | [|_program; address|] -> Address address
    | _ -> invalidCliArgs ()


let headAsync cliArgs = task {

    let! struct (lat, lon) =
        match cliArgs with
        | Coordinates struct (lat, lon) -> task { return struct (lat, lon) }
        | Address address -> GeoCode.coordinates address

    let _ = printfn $"{lat}, {lon}"

    let now = DateTimeOffset.Now
    let events =
        Celestial.events lat lon now
    let struct (rise, noon, set) =
        events

    let _ = printfn "%A" events

    let riseSiderealTime =
        Celestial.localSiderealTime rise lon
    let riseAsc =
        Celestial.getAscendant lat Celestial.ObliquityEcliptic riseSiderealTime
    let _ = printfn $"sun rise in {riseAsc}"

    let noonSiderealTime =
        Celestial.localSiderealTime noon lon
    let noonAsc =
        Celestial.getAscendant lat Celestial.ObliquityEcliptic noonSiderealTime

    let _ = printfn $"sun noon in {noonAsc}"

    let setSiderealTime =
        Celestial.localSiderealTime set lon
    let setAsc =
        Celestial.getAscendant lon Celestial.ObliquityEcliptic setSiderealTime

    printfn $"sun set in {setAsc}"

    let! (forecastUri, forecastZone, fireWeatherZone) =
        Weather.gridInfo lat lon
    let! forecast = 
        Weather.gridForecast forecastUri
        
(*     let! pressure =
        Weather.barometricPressure lat lon *)

    let! alerts =
        Weather.alerts forecastZone
    let! fireAlerts =
        Weather.alerts fireWeatherZone

    printfn "%A" forecast.properties.periods.[0]
(*     printfn "%s" pressure *)
    printfn "%A" alerts
    printfn "%A" fireAlerts

    return ()

}

let head (argv: string []) =

    let cliArgs =
        match tryGetArgsFromEnv () with
        | ValueNone -> cliArgsOfArgv argv
        | ValueSome cliArgs -> cliArgs


    let task = headAsync cliArgs
    
    task.GetAwaiter().GetResult()
    
    printfn "hello world"
    0

try
    Env.argv ()
    |> head
    |> Env.exit
with
| :? InvalidCliArgumentsException ->
    printUsage ()
    Env.exit 1