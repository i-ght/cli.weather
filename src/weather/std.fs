namespace std


open System
open System.Text.Json
open System.Net.Http


type StringPair = ValueTuple<string, string>


[<AutoOpen>]
module Structure =

    let pair<'a, 'b> (a: 'a) (b: 'b) =
        struct (a, b)

    let disregard<'a> a = ignore<'a> a


module Env =

    let argv () =
        Environment.GetCommandLineArgs()

    let exit code =
        Environment.Exit(code)


module Encoding =

    let utf8Str (bytes: ReadOnlySpan<byte>) =
        System.Text.Encoding.UTF8.GetString(bytes)

    let utf8Bytes (str: string) =
        System.Text.Encoding.UTF8.GetBytes(str)

type HttpRequest =
    { Method: string
      Uri: Uri
      Proxy: Uri ValueOption
      Headers: StringPair seq
      Content: ReadOnlyMemory<byte> }

type HttpResponse =
    { StatusCode: Net.HttpStatusCode
      Headers: StringPair seq
      Content: ReadOnlyMemory<byte> }


module Json =
    let deserialize<'a> (json: string) =
        JsonSerializer.Deserialize<'a>(json)


module Http =

    open System.Net

    module HttpUtils =

        open System.Text

        let [<Literal>] private UnreservedHttpChars =
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.~"

        let urlEncode (text: string) =
            let sb = StringBuilder(text.Length + 99)
            for ch in text do
                if UnreservedHttpChars.IndexOf(ch) = -1 then
                    sb.AppendFormat("%{0:X2}", int ch)
                    |> ignore<StringBuilder>
                else
                    sb.Append(ch) |> ignore<StringBuilder>
            sb.ToString()

        let urlEncodePair (pair: StringPair) =
            let struct (a, b) = pair
            $"{urlEncode a}={urlEncode b}"

        let urlEncodeSeq (seq: StringPair seq) =
            String.Join("&", Seq.map urlEncodePair seq)

    module HttpMethod =
        let HEAD = "HEAD"
        let GET = "GET"
        let POST = "POST"
        let PUT = "PUT"
        let DELETE = "DELETE"

    module HttpRequest =
        let construct method uri =
            { Method=method;
              Uri=Uri(uri);
              Proxy=ValueNone;
              Headers=[];
              Content=ReadOnlyMemory<byte>() }

        let proxy proxy req =
            match proxy with
            | "" -> { req with Proxy=ValueNone }
            | _  -> { req with Proxy=ValueSome <| Uri(proxy) }

        let headers (headers: StringPair seq) (req: HttpRequest) =
            { req with Headers=headers}

        let content (content: byte[]) (req: HttpRequest) =
            { req with Content=ReadOnlyMemory<byte>(content) }

        let contentStr (content: string) (req: HttpRequest) =
            { req with Content=ReadOnlyMemory<byte>(Encoding.utf8Bytes content) }


        let internal toReqMsg (req: HttpRequest) =
            let reqMsg = new HttpRequestMessage(
                HttpMethod.Parse(req.Method),
                req.Uri,
                Content=new ReadOnlyMemoryContent(req.Content)
            )

            for (name, value) in req.Headers do
                if not <| reqMsg.Headers.TryAddWithoutValidation(name, value) then
                    if not <| reqMsg.Content.Headers.TryAddWithoutValidation(name, value) then
                        invalidOp<unit> $"failed to add header {name} {value}"

            reqMsg

    module HttpResponse =

        let internal ofRespMsg (content: byte[]) (response: HttpResponseMessage) =
            { StatusCode=response.StatusCode
              Headers=[]
              Content=ReadOnlyMemory<byte>(content) }

        let strContent (response: HttpResponse) =
            Encoding.utf8Str response.Content.Span

    let req = HttpRequest.construct

    module Http =
        let retrieve (req: HttpRequest) = task {
            use handler = new HttpClientHandler()

#if DEBUG
            handler.ServerCertificateCustomValidationCallback <- fun _ _ _ _ -> true
#endif

            match req.Proxy with
            | ValueSome proxy -> handler.Proxy <- WebProxy(proxy)
            | ValueNone -> disregard<unit> ()

#if DEBUG
            handler.Proxy <- WebProxy("127.0.0.1:8080")
#endif

            use client = new HttpClient(handler)
            use req = HttpRequest.toReqMsg req
            use! resp = client.SendAsync(req)
            use content = resp.Content
            let! content = content.ReadAsByteArrayAsync()
            let response = HttpResponse.ofRespMsg content resp
            return response
        }

module Celestial =
    (* translated from https://gml.noaa.gov/grad/solcalc/ *)

    [<AutoOpen>]
    module internal Convenience =

        let radToDeg (angleRad: float) =
            180.0 * angleRad / Math.PI

        let degToRad (angleDeg: float) =
            Math.PI * angleDeg / 180.0

        let calcGeomMeanLongSun (t: float) : float =
            let mutable L0 = 280.46646 + t * (36000.76983 + t * (0.0003032))
            while L0 > 360.0 do
                L0 <- L0 - 360.0
            while L0 < 0.0 do
                L0 <- L0 + 360.0
            L0

        let calcEccentricityEarthOrbit (t: float) : float =
            0.016708634 - t * (0.000042037 + 0.0000001267 * t)

        let calcGeomMeanAnomalySun (t: float) : float =
            357.52911 + t * (35999.05029 - 0.0001537 * t)

        let calcMeanObliquityOfEcliptic (t: float) =
            let seconds = 21.448 - t * (46.8150 + t * (0.00059 - t * (0.001813)))
            let e0 = 23.0 + (26.0 + seconds / 60.0) / 60.0
            e0 // in degrees

        let calcObliquityCorrection (t: float) =
            let e0 = calcMeanObliquityOfEcliptic t
            let omega = 125.04 - 1934.136 * t
            let e = e0 + 0.00256 * cos (degToRad omega)
            e // in degrees

        let calcSunEqOfCenter (t: float) : float =
            let m = calcGeomMeanAnomalySun t
            let mrad = degToRad m
            let sinm = sin mrad
            let sin2m = sin (mrad + mrad)
            let sin3m = sin (mrad + mrad + mrad)
            let C = sinm * (1.914602 - t * (0.004817 + 0.000014 * t)) + sin2m * (0.019993 - 0.000101 * t) + sin3m * 0.000289
            C

        let calcSunTrueLong (t: float) : float =
            let l0 = calcGeomMeanLongSun t
            let c = calcSunEqOfCenter t
            let O = l0 + c
            O

        let calcSunApparentLong (t: float) : float =
            let o = calcSunTrueLong t
            let omega = 125.04 - 1934.136 * t
            let lambda = o - 0.00569 - 0.00478 * sin (degToRad omega)
            lambda

        let calcSunTrueAnomaly (t: float) : float =
            let m = calcGeomMeanAnomalySun t
            let c = calcSunEqOfCenter t
            m + c

        let calcSunDeclination (t: float) : float =
            let e = calcObliquityCorrection t
            let lambda = calcSunApparentLong t
            let sint = sin (degToRad e) * sin (degToRad lambda)
            radToDeg (asin sint)

        let calcSunRadVector (t: float) : float =
            let v = calcSunTrueAnomaly t
            let e = calcEccentricityEarthOrbit t
            (1.000001018 * (1.0 - e * e)) / (1.0 + e * cos (degToRad v))

        let calcRefraction (elev: float) : float =
            if elev > 85.0 then
                0.0
            else
                let te = tan (degToRad elev)
                if elev > 5.0 then
                    58.1 / te - 0.07 / (te * te * te) + 0.000086 / (te * te * te * te * te)
                elif elev > -0.575 then
                    1735.0 + elev * (-518.2 + elev * (103.4 + elev * (-12.79 + elev * 0.711)))
                else
                    -20.774 / te / 3600.0

        let calcEquationOfTime (t: float) =
            let epsilon = calcObliquityCorrection t
            let l0 = calcGeomMeanLongSun t
            let e = calcEccentricityEarthOrbit t
            let m = calcGeomMeanAnomalySun t

            let y = let epsilonRad = degToRad epsilon in
                    let tempY = sin (epsilonRad / 2.0)
                    tempY * tempY

            let sin2l0 = sin (2.0 * degToRad l0)
            let sinm = sin (degToRad m)
            let cos2l0 = cos (2.0 * degToRad l0)
            let sin4l0 = sin (4.0 * degToRad l0)
            let sin2m = sin (2.0 * degToRad m)

            let Etime = y * sin2l0 - 2.0 * e * sinm + 4.0 * e * y * sinm * cos2l0 - 0.5 * y * y * sin4l0 - 1.25 * e * e * sin2m
            radToDeg Etime * 4.0 // in minutes of time

        let calcAzEl (T: float) (localtime: float) (latitude: float) (longitude: float) (zone: float) =
            let eqTime = calcEquationOfTime T
            let theta = calcSunDeclination T

            let solarTimeFix = eqTime + 4.0 * longitude - 60.0 * zone
            let earthRadVec = calcSunRadVector T
            let mutable trueSolarTime = localtime + solarTimeFix
            while trueSolarTime > 1440.0 do
                trueSolarTime <- trueSolarTime - 1440.0

            let hourAngle = trueSolarTime / 4.0 - 180.0
            let mutable haRad = degToRad hourAngle

            if haRad < -180.0 then
                haRad <- haRad + 360.0

            let csz = sin(degToRad latitude) * sin(degToRad theta) + cos(degToRad latitude) * cos(degToRad theta) * cos(haRad)

            let mutable zenith = radToDeg (acos (min (max csz (-1.0)) 1.0))

            let azDenom = cos(degToRad latitude) * sin(degToRad zenith)

            let mutable azimuth =
                if abs azDenom > 0.001 then
                    let azRad = ((sin(degToRad latitude) * cos(degToRad zenith)) - sin(degToRad theta)) / azDenom
                    let clampedAzRad = min (max azRad (-1.0)) 1.0
                    let tempAzimuth = 180.0 - radToDeg (Math.Acos clampedAzRad)
                    if hourAngle > 0.0 then -tempAzimuth else tempAzimuth
                else
                    if latitude > 0.0 then 180.0 else 0.0

            if azimuth < 0.0 then
                azimuth <- azimuth + 360.0

            let exoatmElevation = 90.0 - zenith

            // Atmospheric Refraction correction
            let refractionCorrection = calcRefraction exoatmElevation

            let solarZen = zenith - refractionCorrection
            let elevation = 90.0 - solarZen

            azimuth, elevation

        let calcTimeJulianCent julianDay =
            (julianDay - 2451545.0) / 36525.0

        let timeLocal (dateTimeOffset: DateTimeOffset) =
            let dateTimeOffset = dateTimeOffset.UtcDateTime
            let hour = dateTimeOffset.Hour |> float
            let minute = dateTimeOffset.Minute |> float
            let second = dateTimeOffset.Second |> float
            let timeLocal = hour * 60.0 + minute + second / 60.0
            timeLocal

        let calcHourAngleSunrise (lat: float) (solarDec: float) : float =
            let latRad = degToRad lat
            let sdRad = degToRad solarDec
            let HAarg = (cos (degToRad 90.833) / (cos latRad * cos sdRad) - tan latRad * tan sdRad)
            let HA = acos HAarg
            HA // in radians (for sunset, use -HA)

        let calcSunriseSetUTC (rise: bool) (JD: float) (latitude: float) (longitude: float) : float =
            let t = calcTimeJulianCent JD
            let eqTime = calcEquationOfTime t
            let solarDec = calcSunDeclination t
            let hourAngle = calcHourAngleSunrise latitude solarDec
            let hourAngle' = if rise then hourAngle else -hourAngle
            let delta = longitude + radToDeg hourAngle'
            let timeUTC = 720.0 - (4.0 * delta) - eqTime  // in minutes
            timeUTC

    let julianDay (date: DateTimeOffset) =
        let struct (year, month, day) =
            if date.Month <= 2 then
                struct (date.Year - 1, date.Month + 12, date.Day)
            else
                struct (date.Year, date.Month, date.Day)

        let struct (year, month, day) =
            (float year, float month, float day)

        let a = floor (year / 100.0)
        let b = 2.0 - a + floor (a / 4.0)
        // 	var JD = Math.floor(365.25*(year + 4716)) + Math.floor(30.6001*(month+1)) + day + B - 1524.5
        let jd =
            floor (365.25 * (year + 4716.0) + floor(30.6001 * (month + 1.0))) + day + b - 1524.5
        jd

    let tzOffset (utcOffset: TimeSpan) : float =
        let hours = float utcOffset.Hours
        let minutes = float utcOffset.Minutes
        let tz = hours + minutes / 60.0
        tz

    let calcSunriseSet (rise: bool) (JD: float) (latitude: float) (longitude: float) (timezone: float) =
        let timeUTC = calcSunriseSetUTC rise JD latitude longitude
        let newTimeUTC = calcSunriseSetUTC rise (JD + timeUTC / 1440.0) latitude longitude
        let azimuth, timeLocal, jday =
            let mutable timeLocal = newTimeUTC + (timezone * 60.0)
            let riseT = calcTimeJulianCent (JD + newTimeUTC / 1440.0)
            let (azimuth, _elevation) = calcAzEl riseT timeLocal latitude longitude timezone
            let mutable jday = JD
            if timeLocal < 0.0 || timeLocal >= 1440.0 then
                let increment = if timeLocal < 0 then 1 else -1
                while timeLocal < 0.0 || timeLocal >= 1440.0 do
                    timeLocal <- timeLocal + float increment * 1440.0
                    jday <- jday - float increment
            jday, timeLocal, azimuth
        jday, timeLocal, azimuth
