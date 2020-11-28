module WriteFile

open System
open System.IO
open System.Text.RegularExpressions
open System.Net.Sockets
open System.Text
open SharedTypes

let writeLine (color : ConsoleColor) ( msg : string ) =
    Console.ForegroundColor <- color
    Console.WriteLine msg
    Console.ResetColor()

type Fetch<'a> =
    | Fetched of 'a
    | Not_Fetched

type Msg =
    | Initialize
    | Write_To_File of MemoryStream

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

type Variant =
    | Straight
    | Diagonal

let address_regex = Regex(".*?(?=\s)")
let content_regex = Regex("(?<=\s+.*?)(.|\n)*")

let generateStreamFromString ( content : string ) =
    let stream = new MemoryStream()
    let writer = new StreamWriter(stream)
    writer.Write content
    writer.Flush()
    stream.Position <- 0 |> int64

    stream

let getMatches ( regex : Regex ) str =
    let matches_all = regex.Matches(str)

    let result =
        [0..matches_all.Count]
        |> Seq.map (fun pos ->
                matches_all.[pos].Value
            )

    result

let writeProgress variant (prog : int) =
    let mutable output = ""
    prog
    |> function
        | p when p >= 0 && p < 10 ->
            output <- "\r[          ]"
        | p when p >= 10 && p < 20 ->
            output <- "\r[|         ]"
        | p when p >= 20 && p < 30 ->
            output <- "\r[||        ]"
        | p when p >= 30 && p < 40 ->
            output <- "\r[|||       ]"
        | p when p >= 40 && p < 50 ->
            output <- "\r[||||      ]"
        | p when p >= 50 && p < 60 ->
            output <- "\r[|||||     ]"
        | p when p >= 60 && p < 70 ->
            output <- "\r[||||||    ]"
        | p when p >= 70 && p < 80 ->
            output <- "\r[|||||||   ]"
        | p when p >= 80 && p < 90 ->
            output <- "\r[||||||||  ]"
        | p when p >= 90 && p < 100 ->
            output <- "\r[||||||||| ]"
        | _ ->
            output <- "\r[||||||||||]"

    let last_index = output.LastIndexOf("|")

    match variant with
    | Straight ->
        Console.Write(output)
    | _ ->
        if last_index <> -1
        then
            Console.Write(output.Replace(output.[last_index] |> string, "/"))
        else
            Console.Write(output)

let rec update ( model : WriteFileModel ) ( msg : Msg )  =
    match msg with
    | Initialize ->
        match model.Insert_Text.Length with
        | 0 ->
            "No content could be obtained!"
             |> writeLine ConsoleColor.Red
        | _ ->
            match model.Rate < 1 with
            | true ->
                "No valid rate given. Choose an integer larger than 0"
                |> writeLine ConsoleColor.Red
            | _ ->
                let writeStream =
                    model.Insert_Text
                    |> generateStreamFromString

                writeStream
                |> Write_To_File
                |> update model

    | Write_To_File writeStream ->
        try
            let fsOut = new FileStream(model.Dest_Path, FileMode.Create)

            let bt =
                [|0..model.Rate|]
                |> Array.map (fun _ -> 1048756 |> byte)

            let mutable readByte : int = writeStream.Read(bt, 0, bt.Length)
            let mutable variant = Diagonal

            while readByte > 0 do
                fsOut.Write(bt, 0, readByte)

                variant <-
                    match variant with
                    | Straight -> Diagonal
                    | _ -> Straight

                let percentage = (writeStream.Position * (100 |> int64) / writeStream.Length)
                Console.ForegroundColor <- ConsoleColor.Green

                percentage
                |> string
                |> fun str -> "@message:" + str
                |> socket_write model.SocketPort

                percentage
                |> int
                |> writeProgress variant

                readByte <- writeStream.Read(bt, 0, bt.Length)

            "finished!"
            |> fun str -> "@finished:" + str
            |> socket_write model.SocketPort

            fsOut.Close()

            Console.ResetColor()
        with
        | (ex : Exception) ->
            Console.ForegroundColor <- ConsoleColor.Red
            Console.WriteLine("No file info fetched")

            ex.Message
            |> fun str -> "@finished:" + str
            |> socket_write model.SocketPort

        Console.ResetColor()

