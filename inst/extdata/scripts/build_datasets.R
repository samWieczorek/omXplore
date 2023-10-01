# This script aims at regenerate the datasets embedded in the package 
# `DaparViz`.


library(DaparToolshedData)
library(DAPARdata)
library(QFeatures)
library(MSnbase)

finalSize <- 10

# Build data examples from DaparToolshedData (QFeatures)

data("Exp1_R25_pept", package='DaparToolshedData')
data("Exp1_R25_prot", package='DaparToolshedData')

ft1 <- Exp1_R25_pept[1:finalSize]
ft2 <- Exp1_R25_prot[1:finalSize]

ft <- ft1
ft <- addAssay(ft, ft2[[1]], name = 'processed_1')

vData_ft <- Convert2VizList(ft)

save(ft1, file='data/ft1.RData')
save(ft2, file='data/ft2.RData')
save(ft, file='data/ft.RData')
save(vData_ft, file='data/vData_ft.RData')

# Build data examples from DAPARdata (MSnSet)

data("Exp1_R25_pept", package='DAPARdata')
data("Exp1_R25_prot", package='DAPARdata')

ms1 <- Exp1_R25_pept[1:finalSize]
ms2 <- Exp1_R25_prot[1:finalSize]

ms <- setNames(c(ms1, ms2), nm = c('ms1', 'ms2'))

vData_ms <- convert2viz(ms)

save(ms1, file='data/ms1.RData')
save(ms2, file='data/ms2.RData')
save(ms, file='data/ms.RData')
save(vData_ms, file='data/vData_ms.RData')



# Build example datasets from direct information
library(MSnbase)
library(PSMatch)
data(Exp1_R25_pept, package = 'DAPARdata', envir = environment())
msnset <- Exp1_R25_pept[1:finalSize]
X <- PSMatch::makeAdjacencyMatrix(fData(msnset)[, msnset@experimentData@other$proteinId])
rownames(X) <- rownames(fData(msnset))
connectedComp <- PSMatch::ConnectedComponents(X)

ll.Exp1_R25_pept <- list(
  qdata = exprs(msnset),
  metacell = fData(msnset)[ , msnset@experimentData@other$names_metacell],
  mdata = fData(msnset),
  colID = msnset@experimentData@other$keyId,
  conds = pData(msnset)[, 'Condition'],
  type = msnset@experimentData@other$typeOfData,
  adjMat = as.matrix(X),
  cc = as.list(connectedComp@adjMatrices)
)

data(Exp1_R25_prot, package='DAPARdata', envir = environment())
msnset <- Exp1_R25_prot[1:finalSize]
ll.Exp1_R25_prot <- list(
  qdata = exprs(msnset),
  metacell = fData(msnset)[ , msnset@experimentData@other$names_metacell],
  mdata = fData(msnset),
  colID = msnset@experimentData@other$keyId,
  conds = pData(msnset)[, 'Condition'],
  type = msnset@experimentData@other$typeOfData
)


direct_list <- list(Exp1_R25_pept = ll.Exp1_R25_pept,
               Exp1_R25_prot = ll.Exp1_R25_prot)

vData_list <- convert2viz(direct_list)
save(direct_list, file = 'data/direct_list.RData')
save(vData_list, file = 'data/vData_list.RData')