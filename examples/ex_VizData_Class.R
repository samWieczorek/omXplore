library(MSnbase)
library(DaparToolshed)
library(SummarizedExperiment)
library(DaparViz)
library(Matrix)

# Convert a QFeatures object to an instance of VizList class
data(Exp1_R25_pept, package='DaparToolshedData')
test <- convert2viz(Exp1_R25_pept)

# Convert a list of MSnSet objects to an instance of VizList class
data(Exp1_R25_prot, package='DAPARdata')
data(Exp1_R25_pept, package='DAPARdata')
data(Exp1_R2_pept, package='DAPARdata')
ll.tmp <- setNames(c(Exp1_R25_prot, Exp1_R25_pept, Exp1_R2_pept),
                   nm = c('Exp1_R25_prot', 'Exp1_R25_pept', 'Exp1_R2_pept'))

test <- convert2viz(ll.tmp)



data(Exp1_R25_pept, package='DAPARdata')
msnset <- Exp1_R25_pept
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

data(Exp1_R25_prot, package='DAPARdata')
msnset <- Exp1_R25_prot
ll.Exp1_R25_prot <- list(
   qdata = exprs(msnset),
  metacell = fData(msnset)[ , msnset@experimentData@other$names_metacell],
  mdata = fData(msnset),
  colID = msnset@experimentData@other$keyId,
  conds = pData(msnset)[, 'Condition'],
  type = msnset@experimentData@other$typeOfData
)

data(Exp1_R2_pept, package='DAPARdata')
msnset <- Exp1_R2_pept
X <- PSMatch::makeAdjacencyMatrix(fData(msnset)[, msnset@experimentData@other$proteinId])
rownames(X) <- rownames(fData(msnset))
connectedComp <- PSMatch::ConnectedComponents(X)

ll.Exp1_R2_pept <- list(
  qdata = exprs(msnset),
  metacell = fData(msnset)[ , msnset@experimentData@other$names_metacell],
  mdata = fData(msnset),
  colID = msnset@experimentData@other$keyId,
  conds = pData(msnset)[, 'Condition'],
  type = msnset@experimentData@other$typeOfData,
  adjMat = as.matrix(X),
  cc = as.list(connectedComp@adjMatrices)
  
)


ll.tmp <- list(
  Exp1_R25_pept = ll.Exp1_R25_pept,
  Exp1_R25_prot = ll.Exp1_R25_prot,
  Exp1_R2_pept = ll.Exp1_R2_pept
)

convert2viz(ll.tmp)