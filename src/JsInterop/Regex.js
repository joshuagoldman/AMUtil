export function Match(pattern, input) {
    try {
        var result = input.match(pattern)[0];
        if (result.length === 0) {
            return null;
        }
        else {
            return result;
        }
    }
    catch (ex) {
        return null;
    }
}

// Matches : string -> string -> string[] Option
export function Matches(pattern,input) {
    try {
        var result = [...input.matchAll(pattern)];
        if(result.length === 0){
            return null
        }
        else{
            return result.map(x => x[0]);
        }
    } catch(ex) {
        return null;
    }
}
    
// IsMatch : string -> string -> bool Option
export function IsMatch(pattern,input) {
    try {
        var regex = new RegExp(pattern);
        let result = regex.test(input);
        return result
    } catch(ex) {
        return null;
    }
}