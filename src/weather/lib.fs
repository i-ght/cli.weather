namespace weather.lib

open std
open std.Http
open System

(* 
weather.gov API documentation: https://www.weather.gov/documentation/services-web-api
geocoding.geo.census.gov API documentation: https://geocoding.geo.census.gov/geocoder/Geocoding_Services_API.html
  OR see file Census_Geocoder_User_Guide.pdf in /doc
*)

type AddressMatchCoordinates =
    { x: float; y: float }

type AddressMatch =
    { coordinates: AddressMatchCoordinates }

type AddressMatchResult =
    { addressMatches: AddressMatch [] }

type GeoCodeResult<'a> =
    { result: 'a }

type WeatherResult<'a> =
    { properties: 'a }

type WeatherPointsResult =
    { forecast: string
      forecastGridData: string
      forecastZone: string }

type WeatherValue<'a when 'a : (new: unit -> 'a) and 'a: struct and 'a :> ValueType>  =
  { unitCode: string
    value: 'a Nullable }

type WeatherPeriodData =
    { number: int
      name: string
      startTime: DateTimeOffset
      endTime: DateTimeOffset
      isDaytime: bool
      temperature: int
      temperatureUnit: string
      probabilityOfPrecipitation: WeatherValue<int>
      dewpoint: WeatherValue<float>
      relativeHumidity: WeatherValue<int>
      windSpeed: string
      windDirection: string
      shortForecast: string
      detailedForecast: string }

type WeatherAlertData =
    { sent: DateTimeOffset
      effective: DateTimeOffset
      onset: DateTimeOffset
      expires: DateTimeOffset
      ends: DateTimeOffset
      status: string
      messageType: string
      category: string
      severity: string
      certainty: string
      urgency: string
      event: string
      sender: string
      senderName: string
      headline: string
      description: string
      instruction: string
      response: string }

type WeatherFeaturesResult<'a> =
    { features: 'a WeatherResult [] }

type WeatherForecastResult =
    { updated: DateTimeOffset
      periods: WeatherPeriodData [] }

[<AutoOpen>]
module internal Convenience =
    open System.Net
    let ensureOK (resp: HttpResponse) =
        if resp.StatusCode <> HttpStatusCode.OK then
            invalidOp<unit> 
            <| sprintf "HTTP response returned %A" resp.StatusCode

module GeoCode =

    let [<Literal>] private ApiAuthority = "geocoding.geo.census.gov"

    let coordinates address = task {
        
        let path =
            "/geocoder/locations/onelineaddress"

        let query =
            [ pair<_, _> "address" address
              pair<_, _> "benchmark" "Public_AR_Current"
              pair<_, _> "format" "json" ]
            |> HttpUtils.urlEncodeSeq

        let uri =
            $"https://{ApiAuthority}{path}?{query}"

        let req =
            req "GET" uri
        
        let! resp = Http.retrieve req
        ensureOK resp

        let result =
            Json.deserialize<GeoCodeResult<AddressMatchResult>>
            <| HttpResponse.strContent resp
        

        let ret =
            struct ( result.result.addressMatches.[0].coordinates.y,
                     result.result.addressMatches.[0].coordinates.x )
        return ret
    }

module Weather =
    open System.Text.RegularExpressions
    
    let private ApiAuthority = "api.weather.gov"

    let private pressureRegex =
        Regex(
            "<td class=\"text-right\"><b>Barometer</b></td>\s*<td>(.*?)</td>",
            RegexOptions.Compiled ||| RegexOptions.IgnoreCase
        )

    let barometricPressure (lat: double) (lon: double) = task {
        (* https://forecast.weather.gov/MapClick.php?lat=38.29595769399183&lon=-104.58785803656497 *)
        let query =
            [ pair<_,_> "lat" <| string lat 
              pair<_,_> "lon" <| string lon ]
            |> HttpUtils.urlEncodeSeq

        let uri =
            $"https://forecast.weather.gov/MapClick.php?{query}"
            

        let req =
            req "GET" uri
            |> HttpRequest.headers [ pair<_,_> "User-Agent" "weather" ]

        let! resp = Http.retrieve req
        ensureOK resp
        let html = HttpResponse.strContent resp
        let m = pressureRegex.Match(html)
        let pressure =
            if m.Success then
                m.Groups.[1].Value
            else
                invalidOp<string> "failed to parse barometric pressure value"

        return pressure
    }

    let gridInfo (lat: double) (lon: double) = task {

        let path = $"/points/{lat},{lon}"
        let uri = $"https://{ApiAuthority}{path}"

        let req =
            req "GET" uri
            |> HttpRequest.headers [pair<_,_> "User-Agent" "weather"]

        let! resp = Http.retrieve req
        ensureOK resp

        let result =
            Json.deserialize<WeatherResult<WeatherPointsResult>> 
            <| HttpResponse.strContent resp

        return struct (
            result.properties.forecast,
            result.properties.forecastZone.Split('/') |> Seq.last
        )
    }

    let gridForecast uri = task {

        let req =
            req "GET" uri
            |> HttpRequest.headers [pair<_,_> "User-Agent" "weather"]

        let! resp = Http.retrieve req
        ensureOK resp

        let result =
            Json.deserialize<WeatherResult<WeatherForecastResult>> 
            <| HttpResponse.strContent resp

        return result
    }

    let alerts zone = task {
        let uri =
            $"https://api.weather.gov/alerts/active/zone/{zone}"

        let req =
            req "GET" uri
            |> HttpRequest.headers [pair<_,_> "User-Agent" "weather"]

        let! resp = Http.retrieve req
        ensureOK resp

        let result = 
            Json.deserialize<WeatherFeaturesResult<WeatherAlertData>> 
            <| HttpResponse.strContent resp

      
        return result
    }

type CelestialData =
    { Sunrise: DateTimeOffset
      HighNoon: DateTimeOffset
      Sunset: DateTimeOffset
      Moonrise: DateTimeOffset
      Moonset: DateTimeOffset
      MoonPhase: double
      MoonPhaseDesc: string
      MoonAge: double }

module Solunar =
    open System.Diagnostics
    open System.Text.RegularExpressions

    let private dataRegex =
        Regex(
        "Sunrise\s+(\d{2}:\d{2})\s+High noon (\d{2}:\d{2})\s+.*?\s+Sunset (\d{2}:\d{2}).*?\s+Moonrise (\d{2}:\d{2})\s+Moonset (\d{2}:\d{2})\s+moon phase ([\d.]+), ([A-Za-z ]+)\s+.*?moon age ([\d.]+) days since new",
        RegexOptions.Compiled ||| RegexOptions.IgnoreCase ||| RegexOptions.Singleline
        )

    let exec lat lon =
        let args =
            [ "-l"
              $"{lat}"
              "-o"
              $"{lon}"
              "-f" ]
        let startInfo =
            ProcessStartInfo(
                "solunar",
                args,
                RedirectStandardOutput=true
            )
        use proc = Process.Start(startInfo)
        proc.WaitForExit()
        let stdout = proc.StandardOutput.ReadToEnd()
        let m = dataRegex.Match(stdout)
        if not m.Success then
            invalidOp<unit> "failed to parse data from solunar"
        
        let sunrise = m.Groups.[1].Value |> DateTimeOffset.Parse
        let highNoon = m.Groups.[2].Value |> DateTimeOffset.Parse
        let sunset = m.Groups.[3].Value |> DateTimeOffset.Parse
        let moonrise = m.Groups.[4].Value |> DateTimeOffset.Parse
        let moonset = m.Groups.[5].Value |> DateTimeOffset.Parse
        let moonPhase = m.Groups.[6].Value |> double
        let moonPhaseDesc = m.Groups.[7].Value
        let moonAge = m.Groups.[8].Value |> double
        
        let celestialData = 
            { Sunrise = sunrise
              HighNoon = highNoon
              Sunset = sunset
              Moonrise = moonrise
              Moonset = moonset
              MoonPhase = moonPhase
              MoonPhaseDesc = moonPhaseDesc
              MoonAge = moonAge }
              
        celestialData
