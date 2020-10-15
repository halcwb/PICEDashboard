namespace Informedica.PICE.Lib

module Utils =

    open System


    let kiloPascalToMmHg n = n * 7.50061683
    
    
    let calcRiskFromScore score = Math.Exp(score) / (1. + Math.Exp(score))


