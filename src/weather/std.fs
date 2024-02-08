namespace weather.lib


open System
open System.Net.Http


type StringPair = ValueTuple<string, string>


[<AutoOpen>]
module Structure =

    let pair<'a, 'b> (a: 'a) (b: 'b) =
        struct (a, b)


module Env =

    let argv () =
        Environment.GetCommandLineArgs()

    let exit code =
        Environment.Exit(code)


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


module Encoding =

    let utf8Str (bytes: ReadOnlySpan<byte>) =
        System.Text.Encoding.UTF8.GetString(bytes)

    let utf8Bytes (str: string) =
        System.Text.Encoding.UTF8.GetBytes(str)


module Http =

    open System.Net

    module Method =
        let HEAD = "HEAD"
        let GET = "GET"
        let POST = "POST"
        let PUT = "PUT"
        let DELETE = "DELETE"

        let internal toHttpMethod (method: string) =
            HttpMethod.Parse(method)

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
                Method.toHttpMethod req.Method,
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

        let strCntnt (response: HttpResponse) =
            Encoding.utf8Str response.Content.Span

    let retrieve (req: HttpRequest) = task {
        use handler = new HttpClientHandler()

#if DEBUG
        handler.ServerCertificateCustomValidationCallback <- fun _ _ _ _ -> true
#endif

        match req.Proxy with
        | ValueSome proxy -> handler.Proxy <- WebProxy(proxy)
        | ValueNone -> ()

        use client = new HttpClient(handler)
        use req = HttpRequest.toReqMsg req
        use! resp = client.SendAsync(req)
        use content = resp.Content
        let! content = content.ReadAsByteArrayAsync()
        let response = HttpResponse.ofRespMsg content resp
        return response
    }