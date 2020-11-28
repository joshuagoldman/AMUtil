module UpdateRCOScript

open System.IO
open FSharp.Control.Tasks.V2.ContextInsensitive
open ExcelDataReader
open SharedTypes

let getRCOUpdateAsRcoObj ( tables : System.Data.DataTableCollection ) tableNum = 
    System.Text.Encoding.RegisterProvider(System.Text.CodePagesEncodingProvider.Instance)
    let table = tables.[tableNum : int]

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

    resultObj 

let getAllNewRcoInfo (stream : Stream) = async{
    System.Text.Encoding.RegisterProvider(System.Text.CodePagesEncodingProvider.Instance)
    let datareader = ExcelDataReader.ExcelReaderFactory.CreateOpenXmlReader(stream)
    let tables = datareader.AsDataSet().Tables

    let getAllNewRcoInfo =
        let res =
            [|0..1|]
            |> Array.map (fun pos ->
                    pos
                    |> getRCOUpdateAsRcoObj tables
                )
        stream.Flush()

        res

    return(getAllNewRcoInfo)
}