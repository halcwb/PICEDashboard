namespace Informedica.PICE.Lib


module Click =

    open FSharp.Interop.Excel

#if INTERACTIVE
    [<Literal>]
    let path = __SOURCE_DIRECTORY__ + @"/../mrdm/pimprism_hist.xlsx"
#else
    [<Literal>]
    let path = @"./../mrdm/pimprism_hist.xlsx"
#endif
    type PIMPRISMHist = ExcelFile<path, HasHeaders = true, ForceString = true>

    let pimprismHist = PIMPRISMHist path

