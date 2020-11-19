namespace Informedica.PICE.Lib

module MRDM =

    open System
    open FSharp.Interop.Excel

    //[<Literal>]
    //let path = __SOURCE_DIRECTORY__ + "/../mrdm/Export_PICE.xlsx"

    [<Literal>]
    let path = "./../mrdm/Export_PICE.xlsx"

    type MRDMPatient = ExcelFile<path, SheetName = "patient", HasHeaders = true, ForceString = true>
    type MRDMHospital = ExcelFile<path, SheetName = "ziekenhuis-episode", HasHeaders = true, ForceString = true>
    type MRDMPicu = ExcelFile<path, SheetName = "picu-episode", HasHeaders = true, ForceString = true>
    type MRDMDiagnose = ExcelFile<path, SheetName = "bijkomendediagnoses", HasHeaders = true, ForceString = true>

    let mrdmPatient = MRDMPatient path
    let mrdmHospital = MRDMHospital path
    let mrdmPicu = MRDMPicu path
    let mrdmDiagnose = MRDMDiagnose path

