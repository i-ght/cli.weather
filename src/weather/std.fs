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

    let var name =
        let envVar = Environment.GetEnvironmentVariable(name)
        if envVar |> isNull then
            ValueNone
        else 
            ValueSome envVar

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
            | ValueNone -> ()

#if DEBUG
            handler.Proxy <- WebProxy("127.0.0.1:8080")
#endif

            use client = new HttpClient(handler)
            use req = HttpRequest.toReqMsg req
            use! resp = client.SendAsync(req)
            resp.EnsureSuccessStatusCode()
            |> disregard<HttpResponseMessage>
            use content = resp.Content
            let! content = content.ReadAsByteArrayAsync()
            let response = HttpResponse.ofRespMsg content resp
            return response
        }

type ZodiacSign =
    | Aries = 0
    | Taurus = 30
    | Gemini = 60
    | Cancer = 90
    | Leo = 120
    | Virgo = 150
    | Libra = 180
    | Scorpio = 210
    | Sagittarius = 240
    | Capricorn = 270
    | Aquarius = 300
    | Pisces = 330

module Celestial =

    [<AutoOpen>]
    module internal Sol =

        (* translated from https://gml.noaa.gov/grad/solcalc/ 
          with help from: https://github.com/0xStarcat/CircularNatalHoroscopeJS *)

        let modulo (a: float) (b: float) =
            (a % b + b) % b;
        
        let hourTimeToDecimal (date: DateTimeOffset) =
            let struct (hour, minute)= 
                let floats = 
                    [date.Hour; date.Minute]
                    |> List.map float
                (floats[0], floats[1])
            let timeSpan = TimeSpan.FromMinutes(hour * 60.0 + minute)
            timeSpan.TotalHours

        let radToDeg (angleRad: float) =
            180.0 * angleRad / Math.PI

        let degToRad (angleDeg: float) =
            Math.PI * angleDeg / 180.0
(* 
        let calcGeomMeanLongSun (t: float) : float =
            let mutable L0 = 280.46646 + t * (36000.76983 + t * (0.0003032))
            while L0 > 360.0 do
                L0 <- L0 - 360.0
            while L0 < 0.0 do
                L0 <- L0 + 360.0
            L0
*)        
        let calcGeomMeanLongSun (t: float) : float =
            let L0 = 280.46646 + t * (36000.76983 + t * (0.0003032))
            let rec adjustToRange l =
                if l >= 360.0 then adjustToRange (l - 360.0)
                elif l < 0.0 then adjustToRange (l + 360.0)
                else l
            adjustToRange L0

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
            let trueSolarTime = (localtime + solarTimeFix) % 1440.0

            let hourAngle = trueSolarTime / 4.0 - 180.0
            let haRad = degToRad hourAngle

            let csz = sin (degToRad latitude) * sin (degToRad theta) + cos (degToRad latitude) * cos (degToRad theta) * cos haRad

            let zenith = radToDeg (acos (min (max csz (-1.0)) 1.0))

            let azDenom = cos (degToRad latitude) * sin (degToRad zenith)

            let azimuth =
                if abs azDenom > 0.001 then
                    let azRad = ((sin (degToRad latitude) * cos (degToRad zenith)) - sin (degToRad theta)) / azDenom
                    let clampedAzRad = min (max azRad (-1.0)) 1.0
                    let tempAzimuth = 180.0 - radToDeg (acos clampedAzRad)
                    if hourAngle > 0.0 then -tempAzimuth else tempAzimuth
                else
                    if latitude > 0.0 then 180.0 else 0.0

            let azimuth = (azimuth + 360.0) % 360.0

            let exoatmElevation = 90.0 - zenith

            let refractionCorrection = calcRefraction exoatmElevation

            let solarZen = zenith - refractionCorrection
            let elevation = 90.0 - solarZen

            struct (azimuth, elevation)

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


        let zeroPad (value: float) (length: int) =
            let stringValue = value.ToString()
            if stringValue.Length >= length then stringValue
            else stringValue.PadLeft(length, '0')

        let timeString (minutes: float) (flag: int) =
            if minutes >= 0.0 && minutes < 1440.0 then
                let floatHour = minutes / 60.0
                let hour = floor floatHour
                let floatMinute = 60.0 * (floatHour - hour)
                let minute = floor floatMinute
                let floatSec = 60.0 * (floatMinute - minute)
                let second = floor (floatSec + 0.5)
                let adjustedMinute = if second > 59.0 then minute + 1.0 else minute
                let adjustedHour = if adjustedMinute > 59.0 then hour + 1.0 else hour
                let output =
                    match flag with
                    | 2 when second >= 30.0 -> zeroPad adjustedHour 2 + ":" + zeroPad (adjustedMinute + 1.0) 2
                    | _ -> zeroPad adjustedHour 2 + ":" + zeroPad adjustedMinute 2 + (if flag > 2 then ":" + zeroPad second 2 else "")
                output
            else "error"

        let minutesToDateTimeOffset (date: DateTimeOffset) (minutes: float): DateTimeOffset =
            let dt = date.Date
            let timeSpan = TimeSpan.FromMinutes(minutes)
            DateTimeOffset(dt.Add(timeSpan))

        let tzOffset (date: DateTimeOffset) : float =
            let utcOffset = date.Offset 
            let hours = float utcOffset.Hours
            let minutes = float utcOffset.Minutes
            let tz = hours + minutes / 60.0
            tz

        let calcSunriseSet (rise: bool) (JD: float) (latitude: float) (longitude: float) (timezone: float) =
            let timeUTC = calcSunriseSetUTC rise JD latitude longitude
            let newTimeUTC = calcSunriseSetUTC rise (JD + timeUTC / 1440.0) latitude longitude

            let rec adjustTime (timeLocal: float) (jday: float) =
                match timeLocal with
                | t when t >= 0.0 && t < 1440.0 -> struct (jday, t)
                | t when t < 0.0 -> adjustTime (t + 1440.0) (jday - 1.0)
                | t -> adjustTime (t - 1440.0) (jday + 1.0)

            let riseT = calcTimeJulianCent (JD + newTimeUTC / 1440.0)
            let struct (azimuth, _elevation) = calcAzEl riseT (newTimeUTC + timezone * 60.0) latitude longitude timezone
            let struct (jday, timeLocal) = adjustTime (newTimeUTC + timezone * 60.0) JD
            struct (azimuth, timeLocal, jday)

        let calcSolNoon (jd: float) (longitude: float) (timezone: float) =
            let tnoon = calcTimeJulianCent (jd - longitude / 360.0)
            let eqTime = calcEquationOfTime tnoon
            let solNoonOffset = 720.0 - (longitude * 4.0) - eqTime // in minutes
            let newt = calcTimeJulianCent (jd - 0.5 + solNoonOffset / 1440.0)
            let eqTime = calcEquationOfTime newt
            let solNoonLocal = 720.0 - (longitude * 4.0) - eqTime + (timezone * 60.0) // in minutes

            let rec adjustToRange (time: float) =
                if time < 0.0 then adjustToRange (time + 1440.0)
                elif time >= 1440.0 then adjustToRange (time - 1440.0)
                else time

            adjustToRange solNoonLocal


        let jdPrecise (date: DateTimeOffset) =
            let utc = DateTimeOffset(date.UtcDateTime)
            let ut = hourTimeToDecimal utc
            let struct (year, month, day) =
                let floats =
                    [utc.Year; utc.Month; utc.Day]
                    |> List.map float
                (floats[0], floats[1], floats[2])

            let julianDate =
                (367.0 * year)
                - floor (7.0 * (year + floor ((month + 9.0) / 12.0)) / 4.0)
                - floor (3.0 * (floor ((year + (month - 9.0) / 7.0) / 100.0) + 1.0) / 4.0)
                + floor ((275.0 * month) / 9.0)
                + day
                + 1721028.5
                + (ut / 24.0)

            julianDate

        let jd (date: DateTimeOffset) =
            let struct (year, month, day) =
                if date.Month <= 2 then
                    struct (date.Year - 1, date.Month + 12, date.Day)
                else
                    struct (date.Year, date.Month, date.Day)

            let struct (year, month, day) =
                (float year, float month, float day)

            let a = floor (year / 100.0)
            let b = 2.0 - a + floor (a / 4.0)

            let jd =
                floor (365.25 * (year + 4716.0) + floor(30.6001 * (month + 1.0))) + day + b - 1524.5
            jd

        let sinFromDegrees degrees =
            sin (degToRad degrees)

        let cosFromDegrees degrees =
            cos (degToRad degrees)

        let tanFromDegrees degrees =
            tan (degToRad degrees)

    let localSiderealTime (date: DateTimeOffset) (longitude: float) =
        let jd = jdPrecise date
        let julianDaysJan1st2000 = 2451545.0
        let julianDaysSince2000 = jd - julianDaysJan1st2000
        let tFactor = julianDaysSince2000 / 36525.0 // centuries
        let degreesRotationInSiderealDay = 360.98564736629
        let lst =
            280.46061837
            + (degreesRotationInSiderealDay * julianDaysSince2000)
            + 0.000387933 * (tFactor ** 2.0)
            - ((tFactor ** 3.0) / 38710000.0)
            + longitude

        let modLst = modulo lst 360.0
        modLst

    let [<Literal>] ObliquityEcliptic = 23.4367

    let getAscendant latitude obliquityEcliptic localSiderealTime =

        let a = -cosFromDegrees localSiderealTime
        let b = sinFromDegrees obliquityEcliptic * tanFromDegrees latitude
        let c = cosFromDegrees obliquityEcliptic * sinFromDegrees localSiderealTime
        let d = b + c
        let e = a / d
        let f = atan e

        let mutable ascendant = f * 180.0 / Math.PI

        // Modulation from wikipedia
        // https://en.wikipedia.org/wiki/Ascendant
        // Citation Peter Duffett-Smith, Jonathan Zwart, Practical astronomy with your calculator or spreadsheet-4th ed., p47, 2011

        if d < 0.0 then
            ascendant <- ascendant + 180.0
        else
            ascendant <- ascendant + 360.0

        if ascendant >= 180.0 then
            ascendant <- ascendant - 180.0
        else
            ascendant <- ascendant + 180.0

        let ascendant = ascendant % 360.0
        match ascendant with
        | _ when ascendant >= float ZodiacSign.Pisces -> ZodiacSign.Pisces
        | _ when ascendant >= float ZodiacSign.Aquarius -> ZodiacSign.Aquarius
        | _ when ascendant >= float ZodiacSign.Capricorn -> ZodiacSign.Capricorn
        | _ when ascendant >= float ZodiacSign.Sagittarius -> ZodiacSign.Sagittarius
        | _ when ascendant >= float ZodiacSign.Scorpio -> ZodiacSign.Scorpio
        | _ when ascendant >= float ZodiacSign.Libra -> ZodiacSign.Libra
        | _ when ascendant >= float ZodiacSign.Virgo -> ZodiacSign.Virgo
        | _ when ascendant >= float ZodiacSign.Leo -> ZodiacSign.Leo
        | _ when ascendant >= float ZodiacSign.Cancer -> ZodiacSign.Cancer
        | _ when ascendant >= float ZodiacSign.Gemini -> ZodiacSign.Gemini
        | _ when ascendant >= float ZodiacSign.Taurus -> ZodiacSign.Taurus
        | _ -> ZodiacSign.Aries


    let events lat lon date =
        let offset = tzOffset date
        let jd = jd date
        let struct (_, riseTimeLocal, riseAzimuth) = 
            calcSunriseSet true jd lat lon offset
        let struct (_, setTimeLocal, setAzimuth) = 
            calcSunriseSet false jd lat lon offset
        
        let solNoon =
            calcSolNoon jd lon offset

        struct (
            minutesToDateTimeOffset date riseTimeLocal,
            minutesToDateTimeOffset date solNoon,
            minutesToDateTimeOffset date setTimeLocal
        )
