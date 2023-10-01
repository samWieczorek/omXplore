library(DaparViz)
library(Matrix)

# Convert a QFeatures object to an instance of VizList class
data(ft1)
vData_ft1 <- convert2viz(ft1)
vData_ft1

# Convert a list of MSnSet objects to an instance of VizList class
data(ms1)
data(ms2)
ll.tmp <- setNames(c(ms1, ms2), nm = c('ms1', 'ms2'))

convert2viz(ll.tmp)


# Convert a list of lists of specific items to an instance of VizList class

data(ms1)
msnset <- ms1
X <- PSMatch::makeAdjacencyMatrix(MSnbase::fData(msnset)[, msnset@experimentData@other$proteinId])
rownames(X) <- rownames(MSnbase::fData(msnset))
connectedComp <- PSMatch::ConnectedComponents(X)

ll.ms1 <- list(
  qdata = MSnbase::exprs(msnset),
  metacell = MSnbase::fData(msnset)[ , msnset@experimentData@other$names_metacell],
  mdata = MSnbase::fData(msnset),
  colID = msnset@experimentData@other$keyId,
  conds = MSnbase::pData(msnset)[, 'Condition'],
  type = msnset@experimentData@other$typeOfData,
  adjMat = as.matrix(X),
  cc = as.list(connectedComp@adjMatrices)
  )

data(ms2)
msnset <- ms2
ll.ms2 <- list(
   qdata = MSnbase::exprs(msnset),
  metacell = MSnbase::fData(msnset)[ , msnset@experimentData@other$names_metacell],
  mdata = MSnbase::fData(msnset),
  colID = msnset@experimentData@other$keyId,
  conds = MSnbase::pData(msnset)[, 'Condition'],
  type = msnset@experimentData@other$typeOfData
)


ll.tmp <- list(ms1 = ll.ms1,
               ms2 = ll.ms2)

convert2viz(ll.tmp)