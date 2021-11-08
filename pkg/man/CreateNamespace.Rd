\name{CreateNamespace}
\alias{CreateNamespace}

\title{
 Creates a NAMESPACE file from functions within a script file.
}

\description{
 This function identifies functions embebed in an R script and creates the NAMESPACE file from them. 
}

\usage{CreateNamespace(pkgName, mainDir, scriptFile, import = NULL)}

\arguments{
	\item{pkgName }{Name of the package}

	\item{mainDir }{Path to the directory where the package is stored (not including package name).}

	\item{scriptFile }{Name of the script with the functions from which the .Rd files will be created (do not include the .R file extension).}

	\item{import }{Character vector of functions imported.}

}

\details{
Note that the arguments \code{pkgName} and \code{scriptFile} do not require the entire path but only the names, without extensions. Furthermore, the function assumes that the file structure follows the template created by this package. 
}

\author{Fernando Colchero \email{colchero@imada.sdu.dk}}

\seealso{\code{\link{CreateRpackage}} to produce an empty file structure for the package, \code{\link{CreatePkgDescrip}} to create a template for the DESCRIPTION file, and \code{\link{CreatePkgDescrip}} to create a template for the DESCRIPTION file.}

\keyword{Create R package}

