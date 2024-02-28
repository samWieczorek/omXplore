# This script aims at regenerate the datasets embedded in the package 
# `DaparViz`.


# Convert `QFeatures` datasets to VizList class
data("Exp1_R2_pept", package = 'DaparToolshedData')
data("Exp1_R2_prot", package = 'DaparToolshedData')
convert2VizList(Exp1_R2_pept)
convert2VizList(Exp1_R2_prot)


# Convert `MSnSet` datasets to VizList class
data("Exp1_R2_pept", package = 'DAPARdata')
data("Exp1_R2_prot", package = 'DAPARdata')
convert2VizList(Exp1_R2_pept)
convert2VizList(Exp1_R2_prot)


# Build data examples from a named list of MSnSet datasets
ll.msnset <- list(original1 = Exp1_R2_pept,
  original2 = Exp1_R2_prot)
convert2VizList(ll.msnset)


# Build data examples from an unnamed list of MSnSet datasets
ll.msnset <- list(Exp1_R2_pept, Exp1_R2_prot)
convert2VizList(ll.msnset)


# Convert `SummarizedExperiment` datasets to VizList class



# Convert `MultiAssayExperiment` datasets to VizList class


# Convert formated list to VizList class