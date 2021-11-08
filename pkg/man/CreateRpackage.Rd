\name{CreateRpackage}
\alias{CreateRpackage}

\title{
 Creates the directory structure for an R package.
}

\description{
 This function creates the directory structure for an R package, including directories for development and latest build version of the package. 
}

\usage{CreateRpackage(pkgName, mainDir, git = FALSE, RstProj = FALSE)}

\arguments{
	\item{pkgName }{Name of the package}

	\item{mainDir }{Path to the directory where the package is stored (not including package name).}
	
	\item{git }{Logical for starting a git repository for the package. Default is \code{FALSE}.}

	\item{RstProj }{Logical for starting an RstudioProject for the package. Default is \code{FALSE}.}

}

\details{
This function creates the following file structure:
- \code{devel}: where development (e.g., temporary) scripts can be stored;

- \code{latestBuild}: where the latest build can be stored;

- \code{pkg}: basic package structure for an R package.
}

\author{Fernando Colchero \email{colchero@imada.sdu.dk}}

\seealso{\code{\link{CreatePkgDescrip}} to create a template for the DESCRIPTION file, \code{\link{CreateNamespace}} to create the NAMESPACE file based on the functions of a R script file, and \code{\link{CreateRscript}} to create a new R script}

\keyword{Create R package}

