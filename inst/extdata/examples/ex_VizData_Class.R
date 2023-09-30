library(DaparViz)
library(Matrix)

# Convert a QFeatures object to an instance of VizList class
data(ft)
convert2viz(ft)

# Convert a list of MSnSet objects to an instance of VizList class
data(ms1_pep)
data(ms2_prot)
data(ms3_pep)
ll.tmp <- setNames(c(ms1_pep, ms2_prot, ms3_pep), nm = c('ms1_pep', 'ms2_prot', 'ms3_pep'))

convert2viz(ll.tmp)


# Convert a list of lists of specific items to an instance of VizList class

data(ms1_pep)
msnset <- ms1_pep
X <- PSMatch::makeAdjacencyMatrix(MSnbase::fData(msnset)[, msnset@experimentData@other$proteinId])
rownames(X) <- rownames(MSnbase::fData(msnset))
connectedComp <- PSMatch::ConnectedComponents(X)

ll.ms1_pep <- list(
  qdata = MSnbase::exprs(msnset),
  metacell = MSnbase::fData(msnset)[ , msnset@experimentData@other$names_metacell],
  mdata = MSnbase::fData(msnset),
  colID = msnset@experimentData@other$keyId,
  conds = MSnbase::pData(msnset)[, 'Condition'],
  type = msnset@experimentData@other$typeOfData,
  adjMat = as.matrix(X),
  cc = as.list(connectedComp@adjMatrices)
  )

data(ms2_prot)
msnset <- ms2_prot
ll.ms2_prot <- list(
   qdata = MSnbase::exprs(msnset),
  metacell = MSnbase::fData(msnset)[ , msnset@experimentData@other$names_metacell],
  mdata = MSnbase::fData(msnset),
  colID = msnset@experimentData@other$keyId,
  conds = MSnbase::pData(msnset)[, 'Condition'],
  type = msnset@experimentData@other$typeOfData
)

data(ms3_pep)
msnset <- ms3_pep
X <- PSMatch::makeAdjacencyMatrix(MSnbase::fData(msnset)[, msnset@experimentData@other$proteinId])
rownames(X) <- rownames(MSnbase::fData(msnset))
connectedComp <- PSMatch::ConnectedComponents(X)

ll.ms3_pep <- list(
  qdata = MSnbase::exprs(msnset),
  metacell = MSnbase::fData(msnset)[ , msnset@experimentData@other$names_metacell],
  mdata = MSnbase::fData(msnset),
  colID = msnset@experimentData@other$keyId,
  conds = MSnbase::pData(msnset)[, 'Condition'],
  type = msnset@experimentData@other$typeOfData,
  adjMat = as.matrix(X),
  cc = as.list(connectedComp@adjMatrices)
)


# ll.tmp <- list(Exp1_R25_pept = ll.Exp1_R25_pept,
#                Exp1_R25_prot = ll.Exp1_R25_prot,
#                Exp1_R2_pept = ll.Exp1_R2_pept)
ll.tmp <- list(Exp1_R25_pept = ll.ms1_pep,
               Exp1_R25_prot = ll.ms2_prot,
               Exp1_R2_pept = ll.ms3_pep)

convert2viz(ll.tmp)