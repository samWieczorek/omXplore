% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/omXplore_corrmatrix.R
\name{corrmatrix}
\alias{corrmatrix}
\alias{omXplore_corrmatrix_ui}
\alias{omXplore_corrmatrix_server}
\alias{corrMatrix}
\alias{omXplore_corrmatrix}
\title{Displays a correlation matrix of the quantitative data of a
numeric matrix.}
\usage{
omXplore_corrmatrix_ui(id)

omXplore_corrmatrix_server(
  id,
  obj = reactive({
     NULL
 }),
  rate = reactive({
     0.5
 }),
  showValues = reactive({
     FALSE
 })
)

corrMatrix(data, rate = 0.5, showValues = FALSE)

omXplore_corrmatrix(obj)
}
\arguments{
\item{id}{A \code{character(1)} which is the id of the shiny module.}

\item{obj}{An instance of the class \code{VizData}}

\item{rate}{The rate parameter to control the exponential law for
the gradient of colors}

\item{showValues}{xxx}

\item{data}{An object of class 'matrix'}
}
\value{
NA

NA

A plot

A shiny app
}
\description{
xxxx
}
\examples{
if (interactive()) {
  data(vData_ft)
  omXplore_corrmatrix(vData_ft[[1]])
}

data(vData_ft)
qdata <- GetSlotQdata(vData_ft[[1]])
corrMatrix(qdata)

}
