
const express = require('express');
const bodyParser = require('body-parser');
var fs = require('fs');
const Joi = require('joi');
var cors = require('cors');
const PORT = process.env.PORT || 3001;
var socket  = require('socket.io');
var shell = require('shelljs');
var readline = require('readline');
const fileUpload = require('express-fileupload');
var clientSocket = require('socket.io-client');

const app = express();
app.use(cors())
app.use(express.json());
app.use(express.urlencoded(({extended:true})));
app.use(bodyParser.json());
app.use(fileUpload());
app.use('/files', express.static(__dirname + '/../public'))

var server = app.listen(PORT, () => {
    console.log( `Server listening on port ${PORT}...`);
});

var io = socket(server);

io.sockets.on(`connection`, (sckt) => {
    sckt.on('message',(msgObj) =>{
        sckt.broadcast.emit('message', msgObj);
    })

    sckt.on('finished',(msgObj) =>{
        sckt.broadcast.emit('finished', msgObj);
    })
});

// --------------------------------------------------------------------------------------------------------------
// Save new RCO List File
// --------------------------------------------------------------------------------------------------------------

const progress = require('progress-stream');
const streamBuffers = require('stream-buffers');
app.use(bodyParser({limit: '10mb'}));

app.post("/save", (req, res, next) => {
    const rcoPathFromRepo = `Ericsson.AM.RcoHandler/EmbeddedResources/RBS6000/Aftermarket/RBS RCO List.csv` 
    let pathRcoFile = __dirname.replace(/\\/g,"/") + `/../public/loganalyzer/${rcoPathFromRepo}`;

     var buffer = new Buffer(req.body.file, 'base64');

    var myReadableStreamBuffer = new streamBuffers.ReadableStreamBuffer({
        frequency: 1000,      // in milliseconds.
        chunkSize: 100000     // in bytes.
        }); 
    
    var wrStr = fs.createWriteStream(pathRcoFile) ;
    var str = progress({
        length: buffer.length,
        time: 1 /* ms */
    });

    var newClient = clientSocket.connect('http://localhost:3001');

    str.on('progress', function(pr) {
        if(pr.remaining === 0){
            newClient.emit(`finished`,{ Status: 200, Msg: `RCO List file saved!`});
            console.log(`finished uploading`);
        }
        else{
            newClient.emit(`message`,{ Progress : pr.percentage, Remaining: pr.remaining });
            console.log(`${pr.percentage} completed`);
        }
        downloaded = pr.percentage;
    });

    myReadableStreamBuffer.put(buffer);
    myReadableStreamBuffer
    .on('error', (error) =>{
        newClient.emit(`finished`,{ Status: 404, Msg: error.message});
    })
    .pipe(str)
    .pipe(wrStr);

    return res.send("done!");
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

// --------------------------------------------------------------------------------------------------------------
// Parse RCO List file
// --------------------------------------------------------------------------------------------------------------

var Excel = require('exceljs');

function getVal(name,headerArr) {
    var foundVal = headerArr.find(x => x.Header === name);

    if(foundVal != null){
        return foundVal.Position;
    } 
    else{
        return 1;
    }
} 

app.post("/RcoList", (req, res) => {
    
    var wb = new Excel.Workbook();
    var rco = new Buffer(req.files.file.data, 'base64');
    console.log(rco.length);

    try {

        wb.xlsx.load(rco).
        then(function(){
            var sh = wb.getWorksheet("Combined");

            var jsonArr = new Array();
            var headerArr = new Array()

            for (l = 1; l <= 26; l++) {
                var headerName = sh.getRow(1).getCell(l).value.toString().
                replace(/\s+/g,"");
                headerArr.push(
                    {
                        Header : headerName,
                        Position : l
                    }
                );
            }

            console.log(sh.rowCount);
            for (i = 2; i <= sh.rowCount; i++) {
                var jsonObj = {
                    ReleaseDate : sh.getRow(i).getCell(getVal("ReleaseDate",headerArr)).toString() || "",
                    RcoDocument : sh.getRow(i).getCell(getVal("RCOdoc",headerArr)).toString() || "",
                    RcoRevision : sh.getRow(i).getCell(getVal("RCOrev",headerArr)).toString(),
                    BarcodeText : sh.getRow(i).getCell(getVal("MatchthestringinRCO-doc(Barcodetext)",headerArr)).toString() || "",
                    Slogan : sh.getRow(i).getCell(getVal("Slogan",headerArr)).toString() || "",
                    ProductNumber : sh.getRow(i).getCell(getVal("Productnumber",headerArr)).toString() || "",
                    ProductGroup : sh.getRow(i).getCell(getVal("ProductGroup",headerArr)).toString() || "",
                    RStateIn : sh.getRow(i).getCell(getVal("R-stateIN",headerArr)).toString() || "",
                    RStateOut : sh.getRow(i).getCell(getVal("R-stateOUT",headerArr)).toString() || "",
                    RcLatEvaluate : sh.getRow(i).getCell(getVal("RCLAT-Evaluate",headerArr)).toString() || "",
                    RcLatTextOut : sh.getRow(i).getCell(getVal("RCLAT-Textout",headerArr)).toString() || "",
                    ScPrttEvaluate : sh.getRow(i).getCell(getVal("SCPRTT-Evaluate",headerArr)).toString() || "",
                    ScPrttTextOut : sh.getRow(i).getCell(getVal("SCPRTT-Textout",headerArr)).toString() || "",
                    CloudLatEvaluate : sh.getRow(i).getCell(getVal("CloudLAT-Evaluate",headerArr)).toString() || "",
                    CloudLatTextOut : sh.getRow(i).getCell(getVal("CloudLAT-Textout",headerArr)).toString() || "",
                    ExecutionOrder : sh.getRow(i).getCell(getVal("Executionorder",headerArr)).toString() || "",
                    MfgDateFrom : sh.getRow(i).getCell(getVal("Manucfacturingdate(From)",headerArr)).toString() || "",
                    MfgDateTo : sh.getRow(i).getCell(getVal("Manucfacturingdate(To)",headerArr)).toString() || "",
                    ProductFamily : sh.getRow(i).getCell(getVal("Prod.Family",headerArr)).toString() || "",
                    Closed : sh.getRow(i).getCell(getVal("Closed",headerArr)).toString() || "",
                    Cost : sh.getRow(i).getCell(getVal("Cost",headerArr)).toString() || "",
                    Comments : sh.getRow(i).getCell(getVal("Comments",headerArr)).toString() || ""
                };
                jsonArr.push(jsonObj);
            }

            if(jsonArr.length === 0) return res.status(404).send("RCO data invalid.");
            else return res.json(jsonArr);
        })
        
    } catch (error) {
        return res.status(404).send("RCO data invalid.")
    }
    
});