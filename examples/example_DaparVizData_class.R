library(MSnbase)
library(DaparToolshed)


data(Exp1_R25_prot, package='DaparToolshedData')
qf <- Exp1_R25_prot
Build_DaparVizData(qf, 1)



data(Exp1_R25_pept, package='DAPARdata')
msnset <- Exp1_R25_pept
Build_DaparVizData(msnset)

ll <- list(
  name = 'test_list',
  qdata = exprs(msnset),
  metacell = fData(msnset)[ , msnset@experimentData@other$names_metacell],
  conds = pData(msnset)[, 'Condition'],
  type = msnset@experimentData@other$typeOfData
  )

Build_DaparVizData(ll)