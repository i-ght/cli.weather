open System

open weather.lib
open std

type InvalidCliArgumentsException() =
    inherit InvalidOperationException()

let invalidCliArgs () =
    raise <| InvalidCliArgumentsException()

let printUsage () =
    let message = """usage:
weather address"""
    printfn "%s" message

let addressOfArgv (argv: string[]) =
    match argv with
    | [|_program; address|] -> address
    | _ -> invalidCliArgs ()

let headAsync address = task {

    let! struct (lat, lon) =
        GeoCode.coordinates address

    let tmp = Solunar.exec lat lon
    printfn "%A" tmp
    
    
    let! (forecastUri, forecastZone) =
        Weather.gridInfo lat lon
    let! forecast = 
        Weather.gridForecast forecastUri
    let! pressure =
        Weather.barometricPressure lat lon
    let! alerts =
        Weather.alerts forecastZone

    printfn "%A" forecast.properties.periods[0]
    printfn "%s" pressure
    printfn "%A" alerts

    return ()

}

let head (argv: string []) =
    
    let address = addressOfArgv argv    

    

    let task = headAsync address
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