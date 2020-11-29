module SimpleHttpRequest

open System.Net.Http


let responseString domain = async {
    try
        let client = new HttpClient()
        let! resp = 
            client.GetStringAsync(domain : string)
            |> Async.AwaitTask

        
        return(resp |> Ok)
    with
    | (e : System.Exception) ->
        return(e.Message |> Error)

}  