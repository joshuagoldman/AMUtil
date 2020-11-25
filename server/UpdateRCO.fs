module UpdateRCOScript

open System.IO
open FSharp.Control.Tasks.V2.ContextInsensitive
open ExcelDataReader

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
};

let getRCOUpdateAsRcoObj (stream : Stream) = task {
    System.Text.Encoding.RegisterProvider(System.Text.CodePagesEncodingProvider.Instance)
    let datareader = ExcelDataReader.ExcelReaderFactory.CreateOpenXmlReader(stream)
    let table = datareader.AsDataSet().Tables.[0]

    let resultObj = 
        [|1..table.Rows.Count - 1|]
        |> Array.map (fun row ->
                let rowObj = {
                        ReleaseDate = table.Rows.[row].[0].ToString()
                        RcoDocument = table.Rows.[row].[1].ToString()
                        RcoRevision = table.Rows.[row].[2].ToString()
                        BarcodeText = table.Rows.[row].[3].ToString()
                        Slogan = table.Rows.[row].[4].ToString()
                        ProductNumber = table.Rows.[row].[5].ToString()
                        ProductGroup = table.Rows.[row].[6].ToString()
                        RStateIn = table.Rows.[row].[7].ToString()
                        RStateOut = table.Rows.[row].[8].ToString()
                        RcLatEvaluate = table.Rows.[row].[9].ToString()
                        RcLatTextOut = table.Rows.[row].[10].ToString()
                        ScPrttEvaluate = table.Rows.[row].[11].ToString()
                        ScPrttTextOut = table.Rows.[row].[12].ToString()
                        CloudLatEvaluate = table.Rows.[row].[13].ToString()
                        CloudLatTextOut = table.Rows.[row].[14].ToString()
                        ExecutionOrder = table.Rows.[row].[15].ToString()
                        MfgDateFrom = table.Rows.[row].[16].ToString()
                        MfgDateTo = table.Rows.[row].[17].ToString()
                        ProductFamily = table.Rows.[row].[18].ToString()
                        Closed = table.Rows.[row].[19].ToString()
                        Cost = table.Rows.[row].[20].ToString()
                        Comments = table.Rows.[row].[21].ToString()
                }
                rowObj    
            )

    return(resultObj)
}   

