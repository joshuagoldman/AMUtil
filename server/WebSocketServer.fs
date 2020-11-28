module WebSocketServer

open System
open System.Net.Sockets
open System.Net

let _buffer = [|1024 |> byte|]
let mutable _clientSockets = seq[] : seq<Socket>
let _serverSocket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)

let receiveCallback ar =
    let socket = (ar : IAsyncResult).AsyncState
    let received = (socket :?> Socket).EndReceive(ar)
    let dataBuf = [|received |> byte|]
    Array.Copy(_buffer, dataBuf, received)

    let text = System.Text.Encoding.ASCII.GetString(dataBuf)
    Console.Write("Text received: " + text)

let  rec acceptCallback ar =
    let socket = _serverSocket.EndAccept(ar : IAsyncResult)
    _clientSockets <- seq[socket]
    socket.BeginReceive(_buffer, 0, _buffer.Length, SocketFlags.None, new AsyncCallback(receiveCallback), socket) |> ignore


let setupServer = async {
    Console.WriteLine "Setting up socket..."

    _serverSocket.Bind(IPEndPoint(IPAddress.Any, 3001))
    _serverSocket.Listen(5)
    _serverSocket.BeginAccept(new AsyncCallback(acceptCallback), null) |> ignore
}