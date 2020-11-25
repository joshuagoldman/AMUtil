module ChangeNuGetName

open System.IO
open System.Net.Sockets
open System.Text
open System
open System.Text.RegularExpressions

type Defined<'a> =
    | Defined of 'a
    | Not_Defined

type Project = {
    ProjectName : string
    ProjectNamePure : string
}

type Paths = {
    SpecificPath : string
    GeneralPath : string
}

type SocketInfo = {
    Port : int
    URL : string
}

type Model = {
    Project : Project
    NuGetVersionName : string
    Paths : Paths
    Socket : SocketInfo
    Rate : int
}

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

let socket_write port (msg : string ) =
    let serverIp = "localhost"
    let port = port
    try
        let sckt = new TcpClient()
        sckt.Connect(serverIp, port)

        let byte_count = Encoding.ASCII.GetByteCount msg
        let mutable bytesArr = [|byte_count |> byte|]
        bytesArr <- Encoding.ASCII.GetBytes msg
        let stream = sckt.GetStream()
        stream.Write(bytesArr, 0, bytesArr.Length)

        stream.Close()
        sckt.Close()
    with
        | (ex : Exception) ->
            ex.Message
            |> writeLine ConsoleColor.Red

let receiveStream model  = 
    let mutable content = String.Empty
    let memoryStream = new MemoryStream()
    let streamWriter = new StreamWriter(memoryStream)
    try
        using(File.OpenWrite(model.Paths.SpecificPath))( fun file ->
            let bt =
                [|0..model.Rate|]
                |> Array.map (fun _ -> 1048756 |> byte)

            let mutable readByte = file.Read(bt, 0, bt.Length) 

            while readByte > 0 do
                streamWriter.Write(readByte)

                content <- System.Text.Encoding.UTF8.GetString(BitConverter.GetBytes(readByte));

                let percentage = (file.Position * (100 |> int64) / file.Length)
                Console.ForegroundColor <- ConsoleColor.Green

                percentage
                |> string
                |> fun str -> "@message_receive:" + str
                |> socket_write model.Socket.Port

                readByte <- file.Read(bt, 0, bt.Length) 

            file.Close()

            let regex = Regex("(?<=<Version>).*(?=<\/Version>)")

            let oldVersion = regex.Match(content).Value

            content <- content.Replace($"<Version>{oldVersion}</Version>",$"<Version>{model.NuGetVersionName}</Version>")

            content |> Ok
        )

    with
    | (ex : Exception) ->
        Console.ForegroundColor <- ConsoleColor.Red
        Console.WriteLine("No file info fetched")

        ex.Message
        |> Error

let writeToFile model (writeStream : MemoryStream ) = 
        try
            let fsOut = new FileStream(model.Paths.SpecificPath, FileMode.Create)

            let bt =
                [|0..model.Rate|]
                |> Array.map (fun _ -> 1048756 |> byte)

            let mutable readByte : int = writeStream.Read(bt, 0, bt.Length)

            while readByte > 0 do
                fsOut.Write(bt, 0, readByte)

                let percentage = (writeStream.Position * (100 |> int64) / writeStream.Length)
                Console.ForegroundColor <- ConsoleColor.Green

                percentage
                |> string
                |> fun str -> "@message_write:" + str
                |> socket_write model.Socket.Port

                readByte <- writeStream.Read(bt, 0, bt.Length)

            "finished!"
            |> fun str -> "@finished:" + str
            |> socket_write model.Socket.Port

            fsOut.Close()

            Console.ResetColor()
        with
        | (ex : Exception) ->
            Console.ForegroundColor <- ConsoleColor.Red
            Console.WriteLine("No file info fetched")

            ex.Message
            |> fun str -> "@finished:" + str
            |> socket_write model.Socket.Port

        Console.ResetColor()

let rec update model msg  =
    match msg with 
    | Initialize ->
        let projectNameNoEricssonAM = model.Project.ProjectName.Replace("Ericsson.AM.","")

        let generalPath = 
            $"{Directory.GetCurrentDirectory()}/../public/loganalyzer"

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

        update newModel GetFileContent
    | GetFileContent ->
        let streamOpt = receiveStream model

        match streamOpt with 
        | Ok stream ->
            update model (stream |> WriteNewFileContent)
        | Error err ->
            err
            |> fun str -> "@finished:" + str
            |> socket_write model.Socket.Port

    | WriteNewFileContent content ->
        content
        |> generateStreamFromString
        |> writeToFile model



        
    
    

