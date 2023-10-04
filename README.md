## DaparViz

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/prostarProteomics/DaparViz/workflows/R-CMD-check/badge.svg)](https://github.com/prostarProteomics/DaparViz/actions)
[![R-CMD-check-bioc](https://github.com/prostarProteomics/DaparViz/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/prostarProteomics/DaparViz/actions?query=workflow%3AR-CMD-check-bioc)
[![codecov.io](https://codecov.io/github/prostarProteomics/DaparViz/coverage.svg?branch=master)](https://codecov.io/github/prostarProteomics/DaparViz?branch=master)
[![license](https://img.shields.io/badge/license-Artistic--2.0-brightgreen.svg)](https://opensource.org/licenses/Artistic-2.0)
<!-- badges: end -->



### What is DaparViz?

`DaparViz` is a [Bioconductor
package](http://bioconductor.org/packages/DaparViz) that provides
functions for the visualization and the statistical analysis of proteomics data.
It can deal with several formats as Msnset (used in the `MSnbase` package), 
`QFeatures` (in the QFeatures package) and list of datasets.

It is also possible to write your own plot modules so as to embed it into
the GUI of `DaparViz`.

> Evolving `DAPAR` package towards Shiny modules



### Getting started

The `VizData` and `VizList` classes are used to provide and manage a unique common
structure dataset within the package. See the
[DaparViz introduction](https://prostarproteomics.github.io/DaparViz/articles/DaparViz.html)
to get started with the visualisation of quantitative
mass spectrometry data.

The integration of an external plot module is illustrated in the [Add module](https://prostarproteomics.github.io/DaparViz/articles/addModule.html)
vignette.

### License

The `QFeatures` code is provided under a permissive [Artistic 2.0
license](https://opensource.org/licenses/Artistic-2.0). The
documentation, including the manual pages and the vignettes, are
distributed under a [CC BY-SA
license](https://creativecommons.org/licenses/by-sa/4.0/).


