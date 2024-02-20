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
    { fireWeatherZone: string
      forecast: string
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
            Http.req "GET" uri
        
        let! resp = Http.retrieve req
        ensureOK resp

        let result =
            Json.deserialize<GeoCodeResult<AddressMatchResult>>
            <| Http.str resp
        

        let ret =
            struct ( result.result.addressMatches.[0].coordinates.y,
                     result.result.addressMatches.[0].coordinates.x )
        return ret
    }

module Weather =

    let private ApiAuthority = "api.weather.gov"

    let gridInfo (lat: double) (lon: double) = task {

        let struct (lat, lon) =
            (Double.Round(lat, 4), Double.Round(lon, 4))
        let path = $"/points/{lat},{lon}"
        let uri = $"https://{ApiAuthority}{path}"

        let req =
            Http.req "GET" uri
            |> Http.headers [pair<_,_> "User-Agent" "weather toph.ght@gmail.com"]

        let! resp = Http.retrieve req
        ensureOK resp

        let result =
            Json.deserialize<WeatherResult<WeatherPointsResult>> 
            <| Http.str resp

        return struct (
            result.properties.forecast,
            result.properties.forecastZone.Split('/') |> Seq.last,
            result.properties.fireWeatherZone.Split('/') |> Seq.last
        )
    }

    let gridForecast uri = task {

        let req =
            Http.req "GET" uri
            |> Http.headers [pair<_,_> "User-Agent" "weather toph.ght@gmail.com"]

        let! resp = Http.retrieve req
        ensureOK resp

        let result =
            Json.deserialize<WeatherResult<WeatherForecastResult>> 
            <| Http.str resp

        return result
    }

    let alerts zone = task {
        let uri =
            $"https://api.weather.gov/alerts/active/zone/{zone}"

        let req =
            Http.req "GET" uri
            |> Http.headers [pair<_,_> "User-Agent" "weather toph.ght@gmail.com"]

        let! resp = Http.retrieve req
        ensureOK resp

        let result = 
            Json.deserialize<WeatherFeaturesResult<WeatherAlertData>> 
            <| Http.str resp

      
        return result
    }