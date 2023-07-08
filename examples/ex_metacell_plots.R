data(Exp1_R25_pept, package="DaparToolshedData")
obj <- Exp1_R25_pept

obj <- obj[1:10]
se <- obj[[1]]
design <- colData(obj)


pal <- ExtendPalette(length(unique(design(obj)$Condition)), "Dark2")


metacellPerLinesHisto_HC(object = se, design = design, pattern = "Missing POV")
metacellPerLinesHisto_HC(object = se, design = design)
metacellPerLinesHisto_HC(object = se, design = design, pattern = "Quantified")
metacellPerLinesHisto_HC(object = se, design = design, pattern = "Quant. by direct id")
metacellPerLinesHisto_HC(object = se, design = design, pattern = "Quant. by recovery")
metacellPerLinesHisto_HC(object = se, design = design, pattern = c("Quantified", "Quant. by direct id", "Quant. by recovery"))


metacellPerLinesHistoPerCondition_HC(object = se, design = design, pattern = "Missing POV")
metacellPerLinesHistoPerCondition_HC(object = se, design = design)
metacellPerLinesHistoPerCondition_HC(object = se, design = design, pattern = "Quantified")
metacellPerLinesHistoPerCondition_HC(object = se, design = design, pattern = "Quant. by direct id")
metacellPerLinesHistoPerCondition_HC(object = se, design = design, pattern = "Quant. by recovery")
metacellPerLinesHistoPerCondition_HC(object = se, design = design, pattern = c("Quantified", "Quant. by direct id", "Quant. by recovery"))


metacellHisto_HC(object = se, design = design, pattern = "Missing POV")
metacellHisto_HC(object = se, design = design)
metacellHisto_HC(object = se, design = design, pattern = "Quantified")
metacellHisto_HC(object = se, design = design, pattern = "Quant. by direct id")
metacellHisto_HC(object = se, design = design, pattern = "Quant. by recovery")
metacellHisto_HC(object = se, design = design, pattern = c("Quantified", "Quant. by direct id", "Quant. by recovery"))