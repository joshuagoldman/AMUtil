
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
const xhr = require('xmlhttprequest');
const stream = require('stream');

const app = express();
app.use(cors())
app.use(express.json());
app.use(express.urlencoded(({extended:true})));
app.use(bodyParser.json());
app.use(fileUpload());
app.use('/files', express.static(__dirname + '/../public'))
app.use(bodyParser.json({limit: '900mb'}));
app.use(bodyParser.urlencoded({limit: '900mb', extended: true}));

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
    var rco_type = req.body.file_type;
    const rcoPathFromRepo = `Ericsson.AM.RcoHandler/EmbeddedResources/RBS6000/Aftermarket/${rco_type}.csv` 
    let pathRcoFile = __dirname.replace(/\\/g,"/") + `/../public/loganalyzer/${rcoPathFromRepo}`;
    console.log(rcoPathFromRepo);

    var buffer = new Buffer(req.body.file, 'utf8');
    console.log(buffer);

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
    var foundVal = headerArr.find(x => x.Header.replace(/\s+/g,"") === name.replace(/\s+/g,""));

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
                var headerName = sh.getRow(1).getCell(l).text;
                console.log(headerName);
                headerArr.push(
                    {
                        Header : headerName,
                        Position : l
                    }
                );
            }

            for (i = 2; i <= sh.rowCount; i++) {
                var jsonObj = {
                    ReleaseDate : sh.getRow(i).getCell(getVal("Release Date",headerArr)).toString().replace("\n","") || "",
                    RcoDocument : sh.getRow(i).getCell(getVal("RCOdoc",headerArr)).toString().replace("\n","") || "",
                    RcoRevision : sh.getRow(i).getCell(getVal("RCO rev",headerArr)).toString().replace("\n","") || "",
                    BarcodeText : sh.getRow(i).getCell(getVal("Match the string in RCO-doc(Barcodetext)",headerArr)).toString().replace("\n","") || "",
                    Slogan : sh.getRow(i).getCell(getVal("Slogan",headerArr)).toString().replace("\n","") || "",
                    ProductNumber : sh.getRow(i).getCell(getVal("Productnumber",headerArr)).toString().replace("\n","") || "",
                    ProductGroup : sh.getRow(i).getCell(getVal("Product Group",headerArr)).toString().replace("\n","") || "",
                    RStateIn : sh.getRow(i).getCell(getVal("R-stateIN",headerArr)).toString().replace("\n","") || "",
                    RStateOut : sh.getRow(i).getCell(getVal("R-stateOUT",headerArr)).toString().replace("\n","") || "",
                    RcLatEvaluate : sh.getRow(i).getCell(getVal("RC LAT - Evaluate",headerArr)).toString().replace("\n","") || "",
                    RcLatTextOut : sh.getRow(i).getCell(getVal("RC LAT - Textout",headerArr)).toString().replace("\n","") || "",
                    ScPrttEvaluate : sh.getRow(i).getCell(getVal("SC PRTT - Evaluate",headerArr)).toString().replace("\n","") || "",
                    ScPrttTextOut : sh.getRow(i).getCell(getVal("SC PRTT – Textout",headerArr)).toString().replace("\n","") || "",
                    CloudLatEvaluate : sh.getRow(i).getCell(getVal("Cloud LAT - Evaluate",headerArr)).toString().replace("\n","") || "",
                    CloudLatTextOut : sh.getRow(i).getCell(getVal("Cloud LAT - Textout",headerArr)).toString().replace("\n","") || "",
                    ExecutionOrder : sh.getRow(i).getCell(getVal("Execution order",headerArr)).toString().replace("\n","") || "",
                    MfgDateFrom : sh.getRow(i).getCell(getVal("Manucfacturing date (From)",headerArr)).toString().replace("\n","") || "",
                    MfgDateTo : sh.getRow(i).getCell(getVal("Manucfacturing date (To)",headerArr)).toString().replace("\n","") || "",
                    ProductFamily : sh.getRow(i).getCell(getVal("Prod. Family",headerArr)).toString().replace("\n","") || "",
                    Closed : sh.getRow(i).getCell(getVal("Closed",headerArr)).toString().replace("\n","") || "",
                    Cost : sh.getRow(i).getCell(getVal("Cost",headerArr)).toString().replace("\n","") || "",
                    Comments : sh.getRow(i).getCell(getVal("Comments",headerArr)).toString().replace("\n","") || ""
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

// --------------------------------------------------------------------------------------------------------------
// Get Project File Content
// --------------------------------------------------------------------------------------------------------------
app.post("/projectInfo", (req, res) => {
    var projectName = req.body.project;

    const generalPath = __dirname.replace(/\\/g,"/") + '/../public/loganalyzer';
    const specificPath = `${generalPath}/${projectName}/${projectName}.csproj`

    fs.readFile(specificPath, function read(err,data){
        if(err) return res.status(404).send(err);
        else return res.send(data);
    });
    
});  
 
// --------------------------------------------------------------------------------------------------------------
// Get NuGet Server Info
// --------------------------------------------------------------------------------------------------------------
app.get("/nugetinfo", (req, res) => {

    let request = new xhr.XMLHttpRequest();
    request.open('GET',"http://segaeesw04.eipu.ericsson.se/nuget/Packages");
    request.onreadystatechange = function() {
        if (request.readyState === 4) return res.send(request.responseText);
      };
    request.send();
    
});  

// --------------------------------------------------------------------------------------------------------------
// Get Project File and Change Name
// --------------------------------------------------------------------------------------------------------------
app.post("/ChangeName", (req, res) => {
    var projectName = req.body.project;
    var newNugetVersionName = req.body.version;
    var projectNameNoEricssonAM = projectName.replace("Ericsson.AM.","");

    const generalPath = __dirname.replace(/\\/g,"/") + '/../public/loganalyzer';
    const specificPath = `${generalPath}/${projectName}/${projectName}.csproj`

    var newClient = clientSocket.connect('http://localhost:3001');

    let data = fs.readFileSync(specificPath,'utf8');

    const pattern = "(?<=<Version>).*(?=<\/Version>)";
    var result = data.match(pattern)[0];
    const newDataAsString = data.replace(`<Version>${result}</Version>`,`<Version>${newNugetVersionName}</Version>`);
    var buffer = Buffer.from(newDataAsString, 'utf8'); 

    var str = progress({
        length: buffer.length,
        time: 1 /* ms */
    });

    str.on('progress', function(pr) {
        if(pr.remaining === 0){
            newClient.emit(`finished`,{ Status: 200, Msg: `Project ${req.body.project} saved!`, ID: projectNameNoEricssonAM});
            console.log(`finished`);
        }
        else{
            newClient.emit(`message`,{ Progress : pr.percentage, Remaining: pr.remaining, ID: projectNameNoEricssonAM });
            console.log(`${pr.percentage} completed`);
        }
        downloaded = pr.percentage;
    });

    var bufferStream = new stream.PassThrough({highWaterMark:5});
    bufferStream.end(buffer);

    bufferStream
    .pipe(str)
    .pipe(fs.createWriteStream(specificPath, {highWaterMark:5}))
    .on('error', (error) =>{
        newClient.emit(`finished`,{ Status: 404, Msg: error.message});
        return res.send("done!")
    });

    return res.send("done!"); 
});  