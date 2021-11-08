\name{CreateRproj}
\alias{CreateRproj}

\title{
 Creates the file structure for a new R project.
}

\description{
 This function creates a new R project with a specific file structure.
}

\usage{CreateRproj(projName, mainDir, git = FALSE, RstProj = FALSE)}

\arguments{
	\item{projName }{Character string with the name of the project.}

	\item{mainDir }{Path of the directory where the project will be stored.}

	\item{git }{Logical for starting a git repository for the project. Default is \code{FALSE}.}

	\item{RstProj }{Logical for starting an RstudioProject for the project. Default is \code{FALSE}.}

}

\details{
Argument \code{projName} only requires the name of the project as it will appear in the file structure. Thus, be aware of file conventions when you assign the name of the project.

The file structure includes the following four main directories:
- \code{01docs}: where any documents (descriptions, documentations, etc.) can be stored;

- \code{02code}: where all the scripts should be stored;

- \code{03data}: a main directory for storing data, which has the following sub-directories: \code{rdata} and \code{tables};

- \code{04results}: a main directory to store results, with the following sub-directories: \code{plots}, \code{rdata} and \code{tables}.
}

\author{Fernando Colchero \email{colchero@imada.sdu.dk}}

\seealso{\code{\link{CreateRscript}} to create a new R script, \code{\link{CreateRscriptSec}} to add new sections to existing scripts, and \code{\link{CreateRpackage}} to create a new R package.}

\keyword{create R project}

