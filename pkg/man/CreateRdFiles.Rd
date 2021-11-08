\name{CreateRdFiles}
\alias{CreateRdFiles}

\title{
 Creating help .Rd files from a code script.
}

\description{
 This function detects the functions embebed in the script and creates an .Rd file for each function.  
}

\usage{CreateRdFiles(pkgName, mainDir, scriptFile, authorNames = NULL,
                          authorEmails = NULL, license = NULL)}

\arguments{
	\item{pkgName }{Name of the package}

	\item{mainDir }{Path to the directory where the package is stored (not including package name).}

	\item{scriptFile }{Name of the script with the functions from which the .Rd files will be created (do not include the .R file extension).}

	\item{authorNames }{Optional, character vector with the names of the authors.}

	\item{authorEmails }{Optional, character vector with the email addresses of the authors.}
	
	\item{license }{License details; default is \dQuote{GNU General Public Licence}.}
}

\details{
Note that the arguments \code{pkgName} and \code{scriptFile} do not require the entire path but only the names, without extensions. Furthermore, the function assumes that the file structure follows the template created by this package. 

If \code{authorNames} are provided, make sure to also provide \code{authorEmails}. The default for both is \code{NULL} and will only produce a dummy record to be later changed by the user.
}

\author{Fernando Colchero \email{colchero@imada.sdu.dk}}

\seealso{\code{\link{CreateRpackage}} to produce an empty file structure for the package, \code{\link{CreatePkgDescrip}} to create a template for the DESCRIPTION file, and \code{\link{CreateNamespace}} to create the NAMESPACE file based on the functions of a R script file.}

\keyword{R packages}

