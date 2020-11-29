module SharedTypes

module NuGetChange =
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

    type ChangeNugetNameModel = {
        Project : Project
        NuGetVersionName : string
        Paths : Paths
        Socket : SocketInfo
        Rate : int
    }

type WriteFileModel = {
    Insert_Text : string
    Dest_Path : string
    Rate : int
    SocketPort : int
}

type CommandInfo = {
    Command : string
    Arg : string
}

type CommandOptions = {
    Command : CommandInfo
    Answer : string
}

type RcoObject = {
    ReleaseDate : string
    RcoDocument : string
    RcoRevision : string
    BarcodeText : string
    Slogan : string
    ProductNumber : string
    ProductGroup : string
    RStateIn : string
    RStateOut : string
    RcLatEvaluate : string
    RcLatTextOut : string
    ScPrttEvaluate : string
    ScPrttTextOut : string
    CloudLatEvaluate : string
    CloudLatTextOut : string
    ExecutionOrder : string
    MfgDateFrom : string
    MfgDateTo : string
    ProductFamily : string
    Closed : string
    Cost : string
    Comments : string
}

type RCOTabs = {
    RBS6000 : RcoObject array
    ERS : RcoObject array
}


type IApis = {
    GetRcoObject : obj -> Async<Result<RCOTabs,string>>
    Command : CommandInfo array -> Async<CommandOptions array>
    WriteFile : WriteFileModel -> Async<unit>
    ChangeNuGet : NuGetChange.ChangeNugetNameModel -> Async<unit>
    GetProjecInfo : string -> Async<string>
    NuGetInfo : string -> Async<Result<string,string>>
}
