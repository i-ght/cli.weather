open weather.lib
open weather.lib.Http

let httpBin () = task {
    
    let headers =
        [ pair<_, _> "accept" "*/*" ]

    let req =
        HttpRequest.construct "POST" "https://httpbin.org/post"
        |> HttpRequest.proxy "http://localhost:8080/"
        |> HttpRequest.headers headers
        |> HttpRequest.contentStr "hello=world"

    let! resp = Http.retrieve req
    printfn "%A" resp.StatusCode
    return ()
}

let head argv =
    httpBin().GetAwaiter().GetResult()
    printfn "hello world"
    0

Env.argv ()
|> head
|> Env.exit