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
open System.Net.Sockets
open System.Net

type WebSocketServerMessage = { Time : System.DateTime; Message : string }

let saveRcoFileAsync init = task {
    update init WriteFile.Initialize
}

let startSocketServer = 
    let listenSocket = new Socket(AddressFamily.InterNetwork, SocketType.Stream,ProtocolType.Tcp)
    let hostIP = (Dns.GetHostEntry(IPAddress.Parse("127.0.0.1"))).AddressList.[0]
    let ep = IPEndPoint(hostIP, 3001)
    listenSocket.Bind(ep)
    listenSocket.Listen()

    System.Console.WriteLine("Server has started on localhost:3001...")

startSocketServer

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
        |> Array.map (fun cmdChoice ->
            match cmdChoice with
            | IsCd cdCmd ->
                let isServerCommand = 
                    cdCmd.MoveCommand.ToLower().Contains("server")
                
                match isServerCommand with
                | false ->
                    let answer =
                        cdCmd.ResponseCommand 
                        |> executeCommand (Some(cdCmd.MoveCommand))

                    {
                        Command = 
                            cdCmd.MoveCommand + ";" + cdCmd.ResponseCommand
                        Answer = answer
                    }
                | true ->
                    let currPath = Directory.GetCurrentDirectory().Replace("\\","/")
                    let server_path = $"{currPath}/../public"
                    let fullCommand = 
                        {cdCmd with MoveCommand = 
                                    cdCmd.MoveCommand.Replace("server",server_path) }

                    let answer =
                        fullCommand.ResponseCommand 
                        |> executeCommand (Some(fullCommand.MoveCommand))

                    {
                        Command = 
                            fullCommand.MoveCommand + ";" + fullCommand.ResponseCommand
                        Answer = answer
                    }
            | IsResponse cmd ->
                let answer =
                    cmd 
                    |> executeCommand None

                {
                    Command = cmd
                    Answer = answer
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
            let projectNameLongVersion = $"Ericsson.AM.{projectName}"
            let specificPath = $"{generalPath}\{projectNameLongVersion}\{projectNameLongVersion}.csproj"

            let writeStream = new MemoryStream()
            let file = File.Open(specificPath, FileMode.Open)
            let bt = [|1048756 |> byte|]
            let mutable readByte = file.Read(bt,0,bt.Length)


            while readByte > 0 do
                writeStream.Write(bt,0,readByte)
                readByte <- file.Read(bt, 0, bt.Length)

            let content = Encoding.ASCII.GetString(writeStream.ToArray())

            file.Flush()
            file.Close()

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
