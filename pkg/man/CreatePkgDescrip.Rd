\name{CreatePkgDescrip}
\alias{CreatePkgDescrip}

\title{
 Creates a template DESCRIPTION file.
}

\description{
 This function creates a template for the DESCRIPTION file of the package.
}

\usage{CreatePkgDescrip(pkgName, mainDir, title = NULL, 
                             version = NULL, authors = NULL,
                             maintainer = NULL, license = NULL)}

\arguments{
	\item{pkgName }{Name of the package}

	\item{mainDir }{Path to the directory where the package is stored (not including package name).}
	
	\item{title }{Character string with the title of the package. Default \code{NULL}.}
	
	\item{version }{Character string with the version of the package. Default \code{NULL}.}
	
	\item{authors }{Character vector with the names of the authors of the package. Default \code{NULL}.}
	
	\item{maintainer }{Character vector with the names and email addresses of the maintainers of the package. Default \code{NULL}}
	
	\item{license }{Character verctor with the names of the authors of the package. Default is \dQuote{GPL}.}

}

\details{
Note that the argument \code{pkgName} does not require the entire path but only the name of the package. Furthermore, the function assumes that the file structure follows the template created by this package. 

Argument \code{maintainer} require a character vector where each element includes the author name and address following this template: \dQuote{First Last <email>}.

}

\author{Fernando Colchero \email{colchero@imada.sdu.dk}}

\seealso{\code{\link{CreateRpackage}} to produce an empty file structure for the package, \code{\link{CreateNamespace}} to create the NAMESPACE file based on the functions of a R script file, and \code{\link{CreateRscript}} to create a new R script}

\keyword{Create R package}