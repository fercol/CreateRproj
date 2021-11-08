\name{CreateRscript}
\alias{CreateRscript}

\title{
 Function to create an R script template.
}

\description{
 Creates an R script template with either default sections or sections defined by the user.
}

\usage{CreateRscript(file, sections = NULL)}

\arguments{
	\item{file }{Path and file name for the R script.}

	\item{sections }{Alternative - character vector with names of main sections to be included in the script. Default is \code{NULL} (see \code{details}).}

}

\details{
If argument \code{sections} is \code{NULL}, then only an initial \dQuote{\code{SETUP}} section is included, with subheadings: \dQuote{\code{Libraries}}, \dQuote{\code{Working directory}}, \dQuote{\code{Logical for saving results}}, and \dQuote{\code{Sourced files
}}. 
}

\author{Fernando Colchero \email{colchero@imada.sdu.dk}}

\seealso{\code{\link{CreateRscriptSec}} to add new sections to existing scripts, \code{\link{CreateRproj}} to create a new R project, and \code{\link{CreateRpackage}} to create a new R package.}

\keyword{Create R package}

