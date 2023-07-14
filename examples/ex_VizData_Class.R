library(MSnbase)
library(DaparToolshed)
library(SummarizedExperiment)
library(DaparViz)

# Convert a QFeatures object to an instance of VizList class
data(ft, package='DaparToolshed')
convert2viz(ft)

# Convert a list of MSnSet objects to an instance of VizList class
data(Exp1_R25_prot, package='DAPARdata')
data(Exp1_R25_pept, package='DAPARdata')
data(Exp1_R2_pept, package='DAPARdata')
ll.tmp <- setNames(c(Exp1_R25_prot, Exp1_R25_pept, Exp1_R2_pept),
                   nm = c('Exp1_R25_prot', 'Exp1_R25_pept', 'Exp1_R2_pept'))

convert2viz(ll.tmp)



data(Exp1_R25_pept, package='DAPARdata')
msnset <- Exp1_R25_pept
ll.Exp1_R25_pept <- list(
  qdata = exprs(msnset),
  metacell = fData(msnset)[ , msnset@experimentData@other$names_metacell],
  mdata = fData(msnset),
  colID = msnset@experimentData@other$keyId,
  conds = pData(msnset)[, 'Condition'],
  type = msnset@experimentData@other$typeOfData
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
ll.Exp1_R2_pept <- list(
  qdata = exprs(msnset),
  metacell = fData(msnset)[ , msnset@experimentData@other$names_metacell],
  mdata = fData(msnset),
  colID = msnset@experimentData@other$keyId,
  conds = pData(msnset)[, 'Condition'],
  type = msnset@experimentData@other$typeOfData
)


ll.tmp <- list(
  Exp1_R25_pept = ll.Exp1_R25_pept,
  Exp1_R25_prot = ll.Exp1_R25_prot,
  Exp1_R2_pept = ll.Exp1_R2_pept
)

convert2viz(ll.tmp)