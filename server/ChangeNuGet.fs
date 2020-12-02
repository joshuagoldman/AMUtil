module ChangeNuGetName

open System.IO
open System.Net.Sockets
open System.Text
open System
open System.Text.RegularExpressions
open SharedTypes.NuGetChange
open System.Net
open Elmish
open Elmish.Bridge

type Defined<'a> =
    | Defined of 'a
    | Not_Defined

type Msg =
    | Initialize
    | GetFileContent
    | WriteNewFileContent of string

let generateStreamFromString ( content : string ) =
    let stream = new MemoryStream()
    let writer = new StreamWriter(stream)
    writer.Write content
    writer.Flush()
    stream.Position <- 0 |> int64

    stream

let writeLine (color : ConsoleColor) ( msg : string ) =
    Console.ForegroundColor <- color
    Console.WriteLine msg
    Console.ResetColor()

let receiveStream model ( clientDispatch : Dispatch<SharedTypes.Shared.ClientMsg> )  = 
    let streamWriter = new MemoryStream()
    try
        using(File.Open(model.Paths.SpecificPath, FileMode.Open))( fun file ->
            let bt =
                [|0..model.Rate + 5|]
                |> Array.map (fun _ -> 1048756 |> byte)

            let mutable readByte = file.Read(bt, 0, bt.Length) 

            while readByte > 0 do
                streamWriter.Write(bt,0,readByte)

                let percentage = (file.Position * (100 |> int64) / file.Length)
                Console.ForegroundColor <- ConsoleColor.Green

                let nugetInfo = {
                    SharedTypes.Shared.NuGetInfo.ProjectName = model.Project.ProjectNamePure
                    SharedTypes.Shared.NuGetInfo.Uploaded = percentage |> float
                }

                let socketMsg =
                    nugetInfo |>
                    (
                        SharedTypes.Shared.Process.OnGoing >>
                        SharedTypes.Shared.ChangeNuGet >>
                        SharedTypes.Shared.ClientMsg.ChangeAction
                    )

                socketMsg |> clientDispatch

                readByte <- file.Read(bt, 0, bt.Length) 

            file.Close()

            let regex = Regex("(?<=<Version>).*(?=<\/Version>)")

            let content = System.Text.Encoding.UTF8.GetString(streamWriter.ToArray())

            let oldVersion = regex.Match(content).Value

            let newContent = content.Replace($"<Version>{oldVersion}</Version>",$"<Version>{model.NuGetVersionName}</Version>")

            newContent |> Ok
        )

    with
    | (ex : Exception) ->
        Console.ForegroundColor <- ConsoleColor.Red
        Console.WriteLine("No file info fetched")

        ex.Message
        |> Error

let writeToFile ( model : ChangeNugetNameModel)
                ( clientDispatch : Dispatch<SharedTypes.Shared.ClientMsg>)
                (writeStream : MemoryStream ) = 
        try
            let fsOut = new FileStream(model.Paths.SpecificPath, FileMode.Create)

            let bt =
                [|0..model.Rate + 5|]
                |> Array.map (fun _ -> 1048756 |> byte)

            let mutable readByte : int = writeStream.Read(bt, 0, bt.Length)

            while readByte > 0 do
                fsOut.Write(bt, 0, readByte)

                let percentage = (writeStream.Position * (100 |> int64) / writeStream.Length)
                Console.ForegroundColor <- ConsoleColor.Green

                let nugetInfo = {
                    SharedTypes.Shared.NuGetInfo.ProjectName = model.Project.ProjectNamePure
                    SharedTypes.Shared.NuGetInfo.Uploaded = percentage |> float
                }

                let socketMsg =
                    nugetInfo |>
                    (
                        SharedTypes.Shared.Process.OnGoing >>
                        SharedTypes.Shared.ChangeNuGet >>
                        SharedTypes.Shared.ClientMsg.ChangeAction
                    )

                socketMsg |> clientDispatch

                readByte <- writeStream.Read(bt, 0, bt.Length)

            let socketMsg =
                "finished!" |>
                (
                    Ok >>
                    SharedTypes.Shared.Process.Finished >>
                    SharedTypes.Shared.ChangeNuGet >>
                    SharedTypes.Shared.ClientMsg.ChangeAction
                )

            socketMsg |> clientDispatch

            fsOut.Close()

            Console.ResetColor()
        with
        | (ex : Exception) ->
            Console.ForegroundColor <- ConsoleColor.Red
            Console.WriteLine("No file info fetched")

            let socketMsg =
                ex.Message |>
                (
                    Error >>
                    SharedTypes.Shared.Process.Finished >>
                    SharedTypes.Shared.ChangeNuGet >>
                    SharedTypes.Shared.ClientMsg.ChangeAction
                )

            socketMsg |> clientDispatch

        Console.ResetColor()

let rec update model msg (clientDispatch: Elmish.Dispatch<SharedTypes.Shared.ClientMsg>)  =
    match msg with 
    | Initialize ->
        let projectNameNoEricssonAM = model.Project.ProjectName.Replace("Ericsson.AM.","")

        let currDir = Directory.GetCurrentDirectory().Replace("\\","/")
        let generalPath = 
            $"{currDir}/../public/loganalyzer"

        let specificPath = 
            $"{generalPath}/{model.Project.ProjectName}/{model.Project.ProjectName}.csproj"

        let newProjectInfo = 
            {model.Project with ProjectNamePure = projectNameNoEricssonAM}

        let newPaths = 
            {
                SpecificPath = specificPath
                GeneralPath = generalPath
            }

        let newModel = 
            {model with Project = newProjectInfo
                        Paths = newPaths}

        update newModel GetFileContent clientDispatch
    | GetFileContent ->
        let streamOpt = receiveStream model clientDispatch

        match streamOpt with 
        | Ok stream ->
            update model (stream |> WriteNewFileContent) clientDispatch
        | Error err ->
            let socketMsg =
                err |>
                (
                    Error >>
                    SharedTypes.Shared.Process.Finished >>
                    SharedTypes.Shared.ChangeNuGet >>
                    SharedTypes.Shared.ClientMsg.ChangeAction
                )

            socketMsg |> clientDispatch

    | WriteNewFileContent content ->
        content
        |> generateStreamFromString
        |> writeToFile model clientDispatch



        
    
    

