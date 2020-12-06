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
open Elmish
open Elmish.Bridge

type WebSocketServerMessage = { Time : System.DateTime; Message : string }

let saveRcoFileAsync init = task {
    update init WriteFile.Initialize
}

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
    GetProjecInfo = getProjectInfo
    NuGetInfo = SimpleHttpRequest.responseString
}

let ajajRouter = 
    Remoting.createApi()
    |> Remoting.fromValue apis
    |> Remoting.buildHttpHandler

let webApp = router {
    get "/api/init" (fun next ctx ->
        task {
            return! text "Yo ma nigga" next ctx
        }) 
    forward "/socket" WebSocketServer.server
    forward "" ajajRouter   
}


let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let app =
    application {
        url ("http://localhost:" + port.ToString() + "/")
        use_router webApp
        disable_diagnostics
        app_config Giraffe.useWebSockets
        use_gzip
        use_iis
    }
run app
