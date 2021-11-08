\name{CreateRscriptSec}
\alias{CreateRscriptSec}

\title{
 Add new sections to an existing R script.
}

\description{
 This function allows to add new sections to an existing R script. 
}

\usage{CreateRscriptSec(file, sections)}

\arguments{
	\item{file }{Path and file name for the R script.}

	\item{sections }{Character vector with names of main sections to be included in the script.}
}

\details{
This function does not delete any existing code in the script, but only appends the additional sections at the end of the file.
}

\author{Fernando Colchero \email{colchero@imada.sdu.dk}}

\seealso{\code{\link{CreateRscript}} to create a new R script, \code{\link{CreateRproj}} to create a new R project, and \code{\link{CreateRpackage}} to create a new R package.}

