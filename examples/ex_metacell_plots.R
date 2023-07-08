data(Exp1_R25_pept, package="DaparToolshedData")
obj <- Exp1_R25_pept

obj <- obj[1:10]
se <- obj[[1]]

qdata <- assay(obj[[1]])
metacell <- qMetacell(obj[[1]])
type <- typeDataset(obj[[1]])
design <- colData(obj)


pal <- ExtendPalette(length(unique(design$Condition)), "Dark2")


metacellPerLinesHisto_HC(qdata, metacell, type, design = design, pattern = "Missing POV")
metacellPerLinesHisto_HC(qdata, metacell, type, design = design)
metacellPerLinesHisto_HC(qdata, metacell, type, design = design, pattern = "Quantified")
metacellPerLinesHisto_HC(qdata, metacell, type, design = design, pattern = "Quant. by direct id")
metacellPerLinesHisto_HC(qdata, metacell, type, design = design, pattern = "Quant. by recovery")
metacellPerLinesHisto_HC(qdata, metacell, type, design = design, pattern = c("Quantified", "Quant. by direct id", "Quant. by recovery"))


metacellPerLinesHistoPerCondition_HC(qdata, metacell, type, design = design, pattern = "Missing POV")
metacellPerLinesHistoPerCondition_HC(qdata, metacell, type, design = design)
metacellPerLinesHistoPerCondition_HC(qdata, metacell, type, design = design, pattern = "Quantified")
metacellPerLinesHistoPerCondition_HC(qdata, metacell, type, design = design, pattern = "Quant. by direct id")
metacellPerLinesHistoPerCondition_HC(qdata, metacell, type, design = design, pattern = "Quant. by recovery")
metacellPerLinesHistoPerCondition_HC(qdata, metacell, type, design = design, pattern = c("Quantified", "Quant. by direct id", "Quant. by recovery"))


metacellHisto_HC(qdata, metacell, type, design = design, pattern = "Missing POV")
metacellHisto_HC(qdata, metacell, type, design = design)
metacellHisto_HC(qdata, metacell, type, design = design, pattern = "Quantified")
metacellHisto_HC(qdata, metacell, type, design = design, pattern = "Quant. by direct id")
metacellHisto_HC(qdata, metacell, type, design = design, pattern = "Quant. by recovery")
metacellHisto_HC(qdata, metacell, type, design = design, pattern = c("Quantified", "Quant. by direct id", "Quant. by recovery"))