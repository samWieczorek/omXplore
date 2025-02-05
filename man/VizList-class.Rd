% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VizList-class.R
\docType{class}
\name{VizList-class}
\alias{VizList-class}
\alias{VizList}
\alias{show,VizList-method}
\alias{initialize,VizList-method}
\alias{[,VizList,ANY,ANY,ANY-method}
\alias{[[,VizList,ANY,ANY-method}
\alias{names,VizList-method}
\alias{length,VizList-method}
\title{VizList class}
\usage{
\S4method{show}{VizList}(object)

\S4method{initialize}{VizList}(.Object, ll.VizData = list())

\S4method{[}{VizList,ANY,ANY,ANY}(x, i)

\S4method{[[}{VizList,ANY,ANY}(x, i)

\S4method{names}{VizList}(x)

\S4method{length}{VizList}(x)
}
\arguments{
\item{object}{An instance of the class \code{VizList}}

\item{.Object}{xxx}

\item{ll.VizData}{xxx}

\item{x}{An instance of class \link{QFeatures}.}

\item{i}{\code{character()}, \code{integer()}, \code{logical()} or \code{GRanges()}
object for subsetting by rows.}

\item{j}{\code{character()}, \code{logical()}, or \code{numeric()} vector for
subsetting by \code{colData} rows.}

\item{k}{\code{character()}, \code{logical()}, or \code{numeric()} vector for
subsetting by assays}

\item{l}{\code{character()}, \code{logical()}, or \code{numeric()} vector for
subsetting by assays}

\item{...}{Additional parameters}

\item{drop}{logical (default \code{TRUE}) whether to drop empty assay
elements in the \code{ExperimentList}.}
}
\value{
See individual method description for the return value.

An instance of the class \code{VizList}

NA

An instance of the class \code{VizList}

NA

NA

NA

NA
}
\description{
Conceptually, a \code{VizList} object is a standard representation of all
elements from quantitative other structured data used in proteomics, such
as \code{MSnset}, \code{QFeatures}, \code{SummarizedExperiment} or \code{MultAssayExperiment}.
It allows to use it as a generic converter.

\code{VizData} objects are not usually created by hand but are created from
the \code{VizList} which is the class accessible by the user.

The recommended way to create \code{VizData} objects is the use the
function \code{convert2viz()} of the class \code{VizList}
}
\section{Slots}{

\describe{
\item{\code{ll.VizData}}{A matrix containing the quantitative data}
}}

\examples{
## ------------------------
## An empty VizData object
## ------------------------

VizList()

## -----------------------------------
## Creating a VizData object manually
## -----------------------------------


}
\seealso{
The \code{VizData()} constructor and the \code{convert2Viz()}
function. The \emph{VizData} vignette provides an extended example.
}
\author{
Samuel Wieczorek
}
