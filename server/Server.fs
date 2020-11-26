module Server

open Saturn
open Giraffe
open System.IO
open WriteFile
open CmdPrompt
open System.Net.Sockets
open System.Net
open Microsoft.AspNetCore.Builder
open FSharp.Control.Tasks.V2
open System.Text

type WebSocketServerMessage = { Time : System.DateTime; Message : string }

let saveRcoFileAsync init = task {
    update init WriteFile.Initialize
}

let hostEntry = Dns.GetHostEntry("localhost")
let ipe = new IPEndPoint(hostEntry.AddressList.[0] , 3001)

let socket = 
    new Socket(ipe.AddressFamily, SocketType.Stream, ProtocolType.Tcp);

socket.Bind(ipe)
socket.Listen(128)

let executeCommands ( commands : seq<string> ) = task {
    let res =
        commands
        |> Seq.map (fun cmd ->
                match cmd.ToLower().Contains("cd server") with
                | false ->
                    cmd |> executeCommand
                | true ->
                    let currPath = Directory.GetCurrentDirectory()
                    let server_path = $"{currPath}../public"
                    let fullCommand = $"cd {server_path}"

                    fullCommand |> executeCommand
                
            )
        |> String.concat "\n"

    return res
    
}


let defaultView = router {
    post "/api/SaveRCOList" (fun next ctx ->
        task{
            let! init = ctx.BindModelAsync<WriteFile.Model>()
            do! saveRcoFileAsync init
            return! (next ctx)
        })
        
    post "/api/Command" (fun next ctx ->
        task{
            let! commandsAsString = ctx.BindModelAsync<string>()
            let allCommands = commandsAsString.Split(";")

            let! allCommandsRes =
                allCommands
                |> Array.toSeq
                |> executeCommands

            return! json allCommandsRes next ctx
        })

    post "/api/RcoList" (fun next ctx ->
        task{
            
            let file = ctx.Request.Form.Files.Item("file")
            let stream = file.OpenReadStream()
            let! content = 
                stream
                |> UpdateRCOScript.getRCOUpdateAsRcoObj

            return! json content next ctx
        })

    post "/api/ProjectInfo" (fun next ctx ->
        task{
            let! file = ctx.BindModelAsync<Microsoft.AspNetCore.Http.IFormFile>()
            let stream = file.OpenReadStream()
            let content = 
                stream
                |> UpdateRCOScript.getRCOUpdateAsRcoObj

            return! json content next ctx
        })

    post "/api/ChangeName" (fun next ctx ->
        task{
            let! testobj = ctx.BindModelAsync<ChangeNuGetName.Model>()
            ChangeNuGetName.update testobj ChangeNuGetName.Initialize

            return! json "Finished!" next ctx
        })
    post "/api/projectInfo" (fun next ctx ->
        task{
            let stream = new StreamReader(ctx.Request.Body)
            let! projectNameUnRefined = stream.ReadToEndAsync()
            let projectName = projectNameUnRefined.Replace("project=","")
            let dir = Directory.GetCurrentDirectory()
            let generalPath = $"{dir}\..\public\loganalyzer"
            let specificPath = $"{generalPath}\{projectName}\{projectName}.csproj"

            let writeStream = new MemoryStream()
            let file = File.Open(specificPath, FileMode.Open)
            let bt = [|1048756 |> byte|]
            let mutable readByte = file.Read(bt,0,bt.Length)


            while readByte > 0 do
                writeStream.Write(bt,0,readByte)
                readByte <- file.Read(bt, 0, bt.Length)

            let content = Encoding.ASCII.GetString(writeStream.ToArray())

            return! json content next ctx
        })
} 

let app =
    application {
        url "http://localhost:8086"
        use_router defaultView
        memory_cache
        use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
        use_static "../public"
        use_gzip
        app_config (fun ab -> ab.UseWebSockets().UseMiddleware<WebSockets.WebSocketMiddleware>())
    }

run app
