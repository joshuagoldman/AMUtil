import * as shell from 'shelljs';

export function runCommand(command){
  try{
    var ans = shell.exec(command)

    if(ans.code === 0){
      var resp = 
      {
        Error : ans.toString(),
        Proccess : null
      }
    return resp;
    }
    else{
      var resp = 
      {
        Error : null,
        Proccess : {
          
        }
      }
      return resp;
    }

    
  } 
  catch(error){
    var resp = 
      {
        Error : error.toString(),
        Proccess : null
      }
    return resp;
  }
}

export function outOn(handler, event, batObj){
  try{
 
    batObj.stdout.on(event, data => {
      handler(data);
    });
  
    return batObj;
  }
  catch{
    return null;
  }
}

export function errOn(handler, event, batObj){
  try{
 
    batObj.stderr.on(event, data => {
      handler(data);
    });
  
    return batObj;
  }
  catch{
    return null;
  }
}