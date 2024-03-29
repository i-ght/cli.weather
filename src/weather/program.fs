﻿open System

open weather.lib
open std

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

    let rec tryGet = function
    | ["LAT"; "LON"] :: tail ->
        match tryGetLatLon () with
        | (ValueSome lat, ValueSome lon) ->
            ValueSome <| (Coordinates <| struct (float lat, float lon))
        | _ ->
            tryGet tail
    | ["LOCAL_ADDRESS"] :: [] ->
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
weather address
OR
weather lat lon
OR
set environment variable LOCAL_ADDRESS
OR
set environment variables LAT and LON"""
    printfn $"{message}"

let tryGetCliArgsOfArgv (argv: string[]) =
    match argv with
    | [|_program; address|] ->
        ValueSome <| Address address
    | [|_program; lat; lon|] ->
        ValueSome <| Coordinates (struct (float lat, float lon))
    | _ ->
        ValueNone

let getLatLon cliArgs = task {
    let! struct (lat, lon) =
        match cliArgs with
        | Coordinates (lat, lon) ->
            task { return struct (lat, lon) }
        | Address address ->
            GeoCode.coordinates address
    return struct (lat, lon)
}

let rec gatherEventSigns lat lon acc events =
    match events with
    | date :: tail ->
        gatherEventSigns
        <| lat
        <| lon
        <| Celestial.ascendant lat lon date :: acc 
        <| tail
    | _ ->
        acc
        |> List.rev

let headAsync cliArgs = task {

    let! struct (lat, lon) = getLatLon cliArgs

    let _ = printfn $"{lat}, {lon}"

    let today = DateTimeOffset.Now
    let events =
        Celestial.events lat lon today
    let struct (rise, noon, set) =
        events

    let _ = printfn "%A" events

    let sunEvents =
        [ rise; noon; set ]

    let signs =
        gatherEventSigns lat lon [] sunEvents
    printfn "%A" signs

    let! (forecastUri, forecastZone, fireWeatherZone) =
        Weather.gridInfo lat lon
    let! forecast = 
        Weather.gridForecast forecastUri

    let! alerts =
        Weather.alerts forecastZone
    let! fireAlerts =
        Weather.alerts fireWeatherZone

    printfn "%A" forecast.properties.periods[0]
    printfn "%A" alerts
    printfn "%A" fireAlerts

    return ~0~
}

[<Struct>]
type GetCliArgsPhase =
    | TryArgv
    | TryEnv

let head (argv: string []) =

    let rec tryGetArgs phase =
        match phase with
        | TryArgv -> 
            match tryGetCliArgsOfArgv argv with
            | ValueNone -> tryGetArgs TryEnv
            | ValueSome cliArgs -> ValueSome cliArgs
        | TryEnv ->
            match tryGetArgsFromEnv () with
            | ValueNone -> ValueNone
            | ValueSome cliArgs -> ValueSome cliArgs

    let cliArgs =
        match tryGetArgs TryArgv with
        | ValueSome cliArgs -> cliArgs
        | ValueNone -> invalidCliArgs ()

    let task =
        headAsync cliArgs
    task.GetAwaiter().GetResult()

try
    Env.argv ()
    |> head
    |> Env.exit
with
| :? InvalidCliArgumentsException ->
    printUsage ()
    Env.exit 1