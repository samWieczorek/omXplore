library(DaparViz)
library(Matrix)

# Convert a QFeatures object to an instance of VizList class
data(Exp1_R25_pept, package='DaparToolshedData')
convert2viz(Exp1_R25_pept)

# Convert a list of MSnSet objects to an instance of VizList class
data(Exp1_R25_prot, package='DAPARdata')
data(Exp1_R25_pept, package='DAPARdata')
data(Exp1_R2_pept, package='DAPARdata')
ll.tmp <- setNames(c(Exp1_R25_prot, Exp1_R25_pept, Exp1_R2_pept),
                   nm = c('Exp1_R25_prot', 'Exp1_R25_pept', 'Exp1_R2_pept'))

convert2viz(ll.tmp)


# Convert a list of listsof specific items to an instance of VizList class

data(Exp1_R25_pept, package='DAPARdata')
msnset <- Exp1_R25_pept
X <- PSMatch::makeAdjacencyMatrix(MSnbase::fData(msnset)[, msnset@experimentData@other$proteinId])
rownames(X) <- rownames(MSnbase::fData(msnset))
connectedComp <- PSMatch::ConnectedComponents(X)

ll.Exp1_R25_pept <- list(
  qdata = MSnbase::exprs(msnset),
  metacell = MSnbase::fData(msnset)[ , msnset@experimentData@other$names_metacell],
  mdata = MSnbase::fData(msnset),
  colID = msnset@experimentData@other$keyId,
  conds = MSnbase::pData(msnset)[, 'Condition'],
  type = msnset@experimentData@other$typeOfData,
  adjMat = as.matrix(X),
  cc = as.list(connectedComp@adjMatrices)
  )

data(Exp1_R25_prot, package='DAPARdata')
msnset <- Exp1_R25_prot
ll.Exp1_R25_prot <- list(
   qdata = MSnbase::exprs(msnset),
  metacell = MSnbase::fData(msnset)[ , msnset@experimentData@other$names_metacell],
  mdata = MSnbase::fData(msnset),
  colID = msnset@experimentData@other$keyId,
  conds = MSnbase::pData(msnset)[, 'Condition'],
  type = msnset@experimentData@other$typeOfData
)

data(Exp1_R2_pept, package='DAPARdata')
msnset <- Exp1_R2_pept
X <- PSMatch::makeAdjacencyMatrix(MSnbase::fData(msnset)[, msnset@experimentData@other$proteinId])
rownames(X) <- rownames(MSnbase::fData(msnset))
connectedComp <- PSMatch::ConnectedComponents(X)

ll.Exp1_R2_pept <- list(
  qdata = MSnbase::exprs(msnset),
  metacell = MSnbase::fData(msnset)[ , msnset@experimentData@other$names_metacell],
  mdata = MSnbase::fData(msnset),
  colID = msnset@experimentData@other$keyId,
  conds = MSnbase::pData(msnset)[, 'Condition'],
  type = msnset@experimentData@other$typeOfData,
  adjMat = as.matrix(X),
  cc = as.list(connectedComp@adjMatrices)
)


ll.tmp <- list(Exp1_R25_pept = ll.Exp1_R25_pept,
               Exp1_R25_prot = ll.Exp1_R25_prot,
               Exp1_R2_pept = ll.Exp1_R2_pept)

convert2viz(ll.tmp)