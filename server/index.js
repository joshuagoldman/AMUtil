
const express = require('express');
const bodyParser = require('body-parser');
var cors = require('cors');
const PORT = process.env.PORT || 3001;
var socket  = require('socket.io');
var shell = require('shelljs');
var net = require('net');
var clientSocket = require('socket.io-client');

const app = express();
app.use(cors())
app.use(express.json());
app.use(express.urlencoded(({extended:true})));
app.use(bodyParser.json());
app.use('/files', express.static(__dirname + '/../public'))
app.use(bodyParser.json({limit: '900mb'}));
app.use(bodyParser.urlencoded({limit: '900mb', extended: true}));

var server = app.listen(PORT, () => {
    console.log( `Server listening on port ${PORT}...`);
});

var io = socket(server);
var newClient = clientSocket.connect('http://localhost:3001');

let netServer = net.createServer(socket => {
    socket.on('data', data => {
        let msg = data.toString();

        newClient.emit('3000', msg);
    });
});


netServer.listen(3000);
console.log("Connected to port 3000!");

io.sockets.on(`connection`, (sckt) => {
    sckt.on('3000', (msgObj) => {
        if(msgObj.includes("@message:")){
            var clean_msg = msgObj.replace('@message:','');
            sckt.broadcast.emit('message', clean_msg);
        }
        else if(msgObj.includes("@finished:")){
            var clean_msg = msgObj.replace('@finished:','');
            sckt.broadcast.emit('finished', clean_msg);
        }
        else console.log("No socket info type found!");
        
        console.log(msgObj);
    });
});


// --------------------------------------------------------------------------------------------------------------
// Shell commands
// --------------------------------------------------------------------------------------------------------------
app.post("/shellcommand", (req, res) => {
    var commandsAsString = req.body.shellCommand;

    console.log(commandsAsString);
    let commands = commandsAsString.split(";");

    let responses = new Array();
    let errors = new Array();

    function decideFaith(ans){
        if(ans.code !== 0){
            errors.push(ans)
        } 
        else{
            responses.push(ans.stdout); 
        } 
    }

    commands.forEach(command => {
        let ans = "";
        if(command.includes("cd ")){
            if(command === "cd server"){
                ans = shell.cd(__dirname.replace(/\\/g,"/") + '/../public');
            }
            else{
                ans = shell.cd(command.replace("cd ",""));
            }
        }
        else{
            ans = shell.exec(command);
        }

        decideFaith(ans);
   
    });

    if(errors.length != 0){
        let errorsAll = errors.map(e =>
            `shell command status code ${e.code}: ${e.stderr}`
        );
        const errorsCombined = errorsAll.join('\n');
        return res.send(errorsCombined);
    }
    else{
        console.log(responses);
        const responsesCombined = responses.join('\n');
        return res.send(responsesCombined);
    }
});