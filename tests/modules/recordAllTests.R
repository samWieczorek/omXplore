library(shinytest)

recordTest("tests/modules/DaparViz_plots_tracking", loadTimeout = 100000)

recordTest("tests/modules/DaparViz_density", loadTimeout = 100000)
recordTest("tests/modules/DaparViz_corrmatrix", loadTimeout = 100000)
recordTest("tests/modules/DaparViz_intensity", loadTimeout = 100000)
recordTest("tests/modules/DaparViz_pca", loadTimeout = 100000)
recordTest("tests/modules/DaparViz_plots_tracking", loadTimeout = 100000)
recordTest("tests/modules/DaparViz_tabExplorer", loadTimeout = 100000)
recordTest("tests/modules/DaparViz_variance", loadTimeout = 100000)
recordTest("tests/modules/DaparViz_view_dataset", loadTimeout = 100000)
