namespace Informedica.PICE.Lib

module MRDM =

    open FSharp.Interop.Excel

#if INTERACTIVE
    [<Literal>]
    let compileTimePath = __SOURCE_DIRECTORY__ + @"/pice.xlsx"
#else
    [<Literal>]
    let compileTimePath = @"pice.xlsx"
#endif

    type MRDMPatient = ExcelFile<compileTimePath, SheetName="patient", HasHeaders=true, ForceString=true>
    type MRDMHospital = ExcelFile<compileTimePath, SheetName="ziekenhuis_episode", HasHeaders=true, ForceString=true>
    type MRDMPicu = ExcelFile<compileTimePath, SheetName="picu_episode", HasHeaders=true, ForceString=true>
    type MRDMDiagnose = ExcelFile<compileTimePath, SheetName="bijkomendediagnoses", HasHeaders=true, ForceString=true>

    let getMrdmPatient path = MRDMPatient path
    let getMrdmHospital path = MRDMHospital path
    let getMrdmPicu path = MRDMPicu path
    let getMrdmDiagnose path = MRDMDiagnose path
