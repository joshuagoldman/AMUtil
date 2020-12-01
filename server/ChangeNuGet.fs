module ChangeNuGetName

open System.IO
open System.Net.Sockets
open System.Text
open System
open System.Text.RegularExpressions
open SharedTypes.NuGetChange
open System.Net

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

let socket_write serverIp port (msg : string ) =
    try
        let host = Dns.GetHostEntry(serverIp : string) 
        let ipAddress = host.AddressList.[0]
        let remoteEP = new IPEndPoint(ipAddress, port) 
  
            // Create a TCP/IP  socket.    
        let sender = new Socket(ipAddress.AddressFamily, SocketType.Stream, ProtocolType.Tcp); 
        sender.Connect(remoteEP)

        let bytesMsg = Encoding.ASCII.GetBytes msg
        let bytesSent = sender.Send(bytesMsg)

        sender.Shutdown(SocketShutdown.Both) 
        sender.Close()
    with
        | (ex : Exception) ->
            ex.Message
            |> writeLine ConsoleColor.Red

let receiveStream model  = 
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

                percentage
                |> string
                |> fun str -> "@message_receive:" + str
                |> socket_write model.Socket.URL model.Socket.Port

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

let writeToFile ( model : ChangeNugetNameModel) (writeStream : MemoryStream ) = 
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

                percentage
                |> string
                |> fun str -> "@message_write:" + str
                |> socket_write model.Socket.URL model.Socket.Port

                readByte <- writeStream.Read(bt, 0, bt.Length)

            "finished!"
            |> fun str -> "@finished:" + str
            |> socket_write model.Socket.URL model.Socket.Port

            fsOut.Close()

            Console.ResetColor()
        with
        | (ex : Exception) ->
            Console.ForegroundColor <- ConsoleColor.Red
            Console.WriteLine("No file info fetched")

            ex.Message
            |> fun str -> "@finished:" + str
            |> socket_write model.Socket.URL model.Socket.Port

        Console.ResetColor()

let rec update model msg  =
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

        update newModel GetFileContent
    | GetFileContent ->
        let streamOpt = receiveStream model

        match streamOpt with 
        | Ok stream ->
            update model (stream |> WriteNewFileContent)
        | Error err ->
            err
            |> fun str -> "@finished:" + str
            |> socket_write model.Socket.URL model.Socket.Port

    | WriteNewFileContent content ->
        content
        |> generateStreamFromString
        |> writeToFile model



        
    
    

