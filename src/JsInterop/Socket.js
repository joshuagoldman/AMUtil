import * as io from 'socket.io-client';

export function createSocket(url) {
    try{
        var socket = io.connect(url);
        return {ErrorMessage: null, Socket: socket};
    }
    catch(error){
        return {ErrorMessage: error.message, Socket: null};
    }
}

export function addEventListener(handler, eventName, socketObj) {
    try{
 
        socketObj.on(eventName,
        // When we receive data
            function(data) {
                handler(data);
            }
        );
        return socketObj;
    }
    catch{
        return null;
    }
}

export function emit(eventName, emitObj, socketObj){
    try{
        socketObj.emit(eventName,emitObj);

        return socketObj;
    }
    catch(error){
        return error.message;
    }
};

export function disconnect(socketObj){
    try{
        socketObj.disconnect();
    }
    catch(error){
        return error.message;
    }
};