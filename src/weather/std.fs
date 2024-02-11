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