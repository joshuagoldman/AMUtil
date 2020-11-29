module Server

open Saturn
open Giraffe
open System.IO
open WriteFile
open CmdPrompt
open FSharp.Control.Tasks.V2
open System.Text
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open SharedTypes

type WebSocketServerMessage = { Time : System.DateTime; Message : string }

let saveRcoFileAsync init = task {
    update init WriteFile.Initialize
}

WebSocketServer.setupServer
|> Async.StartImmediate

let getRcoAsObject (formFile : obj ) = async{
    let file = formFile :?> Microsoft.AspNetCore.Http.IFormFile
    let stream = file.OpenReadStream()
    let! content = 
        stream
        |> UpdateRCOScript.getAllNewRcoInfo 

    return(content)
}

let executeCommands ( commands : CommandInfo array ) = async {
    let res =
        commands
        |> Array.map (fun cmd ->
                let resp = 
                    let isServerCommand =
                        cmd.Command.ToLower().Contains("cd") &&
                        cmd.Arg.ToLower().Contains("server")
                    match isServerCommand with
                    | false ->
                        cmd.Command 
                        |> executeCommand cmd.Arg
                    | true ->
                        let currPath = Directory.GetCurrentDirectory()
                        let server_path = $"{currPath}../public"
                        let fullCommand = 
                            {cmd with Arg = server_path}

                        fullCommand.Command 
                        |> executeCommand fullCommand.Arg
                {
                    Command = cmd
                    Answer = resp
                }
                
            )

    return res
    
}

let saveRcoListAction init = async{
    let currDir = Directory.GetCurrentDirectory()

    let basePath = $"{currDir}../public/loganalyzer/Ericsson.AM.RcoHandler/EmbeddedResources/RBS6000/Aftermarket"
    let finalDestPath = $"{basePath}/{init.Dest_Path}"
    let newInitModel =
        {init with Dest_Path = finalDestPath}

    update newInitModel WriteFile.Initialize
}

let performNugetNameChange testObj = async{
    ChangeNuGetName.update testObj ChangeNuGetName.Initialize
}

let getProjectInfo ( projectName : string ) =
    async{
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

            return content
        }


let apis = {
    GetRcoObject = getRcoAsObject
    Command = executeCommands
    WriteFile = saveRcoListAction
    ChangeNuGet = performNugetNameChange
    GetProjecInfo = getProjectInfo
    NuGetInfo = SimpleHttpRequest.responseString
}

let defaultView = router {

    post "/api/SaveRCOList" (fun next ctx ->
        task{
            let! init = ctx.BindModelAsync<WriteFileModel>()
            do! saveRcoListAction init
            return! (next ctx)
        })
        
    post "/api/Command" (fun next ctx ->
        task{
            let! allCommands = ctx.BindModelAsync<CommandInfo array>()

            let! allCommandsRes =
                allCommands
                |> executeCommands 

            return! json allCommandsRes next ctx
        })

    post "/api/RcoList" (fun next ctx ->
        task{
            
            let file = ctx.Request.Form.Files.Item("file")
            let stream = file.OpenReadStream()
            let! content = 
                stream
                |> UpdateRCOScript.getAllNewRcoInfo

            return! json content next ctx
        })

    post "/api/ChangeName" (fun next ctx ->
        task{
            let! testobj = ctx.BindModelAsync<NuGetChange.ChangeNugetNameModel>()
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

let ajajRouter = 
    Remoting.createApi()
    |> Remoting.fromValue apis
    |> Remoting.buildHttpHandler
    
    

let app =
    application {
        url "http://localhost:8086"
        use_router defaultView
        use_router ajajRouter
        memory_cache
        use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
        use_static "../public"
        use_gzip
    }

run app
