# =============================== CODE METADATA ============================== #
# AUTHOR: Fernando Colchero
# AFFILIATION: Department of Mathematics and Computer Science and 
#              Interdisciplinary Center on Population Dynamics,
#              Univeristy of Southern Denmark.
# DATE CREATED: 2021-03-23
# DESCRIPTION: Code to create a new R project, R code templates and an empty
#              R package.
# COMMENTS:
# ================================ START CODE ================================ #
# ================================== #
# ==== FUNCTIONS FOR R PROJECT: ====
# ================================== #
# Function to create R project:
CreateRproj <- function(projName, mainDir, git = FALSE, RstProj = FALSE) {
  # Project directory:
  projDir <- sprintf("%s%s/", mainDir, projName)
  
  # Directory levels:
  dirLevs <- list()
  
  # Sub-directories for project:
  dirLevs$lev01 <- c("01docs", "02code", "03data", "04results")
  
  # Data sub-directories:
  dirLevs$lev02 <- list("03data" = c("tables", "rdata"),
                        "04results" = c("plots", "tables", "rdata"))
  
  # ============================= #
  # ==== CREATE DIRECTORIES: ====
  # ============================= #
  # Create directory if the project doesn't exist:
  if (!dir.exists(projDir)) {
    dir.create(projDir)
  } else {
    stop(sprintf("Project %s already exists.", projName), call. = FALSE)
  }
  
  # Create files:
  for (lev1 in dirLevs$lev01) {
    dir.create(sprintf("%s%s/", projDir, lev1), showWarnings = FALSE)
  }
  for (ii in 1:length(dirLevs$lev02)) {
    lev1 <- names(dirLevs$lev02)[ii]
    for (jj in 1:length(dirLevs$lev02[[lev1]])) {
      lev2 <- dirLevs$lev02[[lev1]][jj]
      dir.create(sprintf("%s%s/%s/", projDir, lev1, lev2), showWarnings = FALSE)
    }
  }
  
  # Create R studio project file:
  if (RstProj) {
    RprojCont <- "Version: 1.0\nRestoreWorkspace: Default\nSaveWorkspace: Default\nAlwaysSaveHistory: Default\n\nEnableCodeIndexing: Yes\nUseSpacesForTab: Yes\nNumSpacesForTab: 2\nEncoding: UTF-8\n\nRnwWeave: knitr\nLaTeX: pdfLaTeX"
    cat(RprojCont, file = sprintf("%s%s.Rproj",projDir, projName))
  }
  
  # Setup git 
  if (git) {
    system(sprintf("cd %s\ngit init", projDir))
  }
}

# Function to create a template for R code:
CreateRscript <- function(file, sections = NULL) {
  # ----------------------- #
  # ---- Header setup: ----
  # ----------------------- #
  # Metadata headdings:
  metHeads <- c(" CODE METADATA ", " CODE START ")
  
  # Header sections:
  headLabs <- c("AUTHOR: ", "DATE CREATED: ", "DESCRIPTION: ", "COMMENTS: ")
  
  # Number of characters in each:
  nchHeads <- nchar(metHeads)
  
  # Number of '=' signs:
  nEq <- 76
  
  # Header start:
  eqStart <- floor((nEq - nchHeads[1])/2)
  eqEnd <- ceiling((nEq - nchHeads[1])/2)
  cat(sprintf("# %s%s%s #\n", paste(rep("=", eqStart), collapse = ""),
              metHeads[1], paste(rep("=", eqEnd), collapse = "")),
      file = file)
  
  for (hlabs in headLabs) {
    cat(sprintf("# %s\n", hlabs), file = file, append = TRUE)  
  }
  
  # Header end:
  eqStart <- floor((nEq - nchHeads[2])/2)
  eqEnd <- ceiling((nEq - nchHeads[2])/2)
  cat(sprintf("# %s%s%s #\n", paste(rep("=", eqStart), collapse = ""),
              metHeads[2], paste(rep("=", eqEnd), collapse = "")),
      file = file, append = TRUE)
  
  # ------------------------- #
  # ---- setup sections: ----
  # ------------------------- #
  # Initial setup section:
  setupSub <- c("Libraries", "Working directory", "Logical for saving results",
                "Sourced files")
  sec <- " ==== SETUP: ==== "
  cat(sprintf("# %s #\n", paste(rep("=", nchar(sec) - 2), collapse = "")),
      file = file, append = TRUE)
  cat(sprintf("#%s\n", sec), file = file, append = TRUE)
  cat(sprintf("# %s #\n", paste(rep("=", nchar(sec) - 2), collapse = "")),
      file = file, append = TRUE)
  for (setsub in setupSub) {
    cat(sprintf("# %s:\n\n", setsub), file = file, append = TRUE)
  }
  
  # Additional sections:
  if (!is.null(sections)) {
    CreateRscriptSec(file = file, sections = sections)
  }
  
  # ----------------- #
  # ---- Footer: ----
  # ----------------- #
  # footer:
  foot <- " CODE END "
  nfoot <- nchar(foot)
  
  # write footer:
  eqStart <- floor((nEq - nfoot)/2)
  eqEnd <- ceiling((nEq - nfoot)/2)
  cat(sprintf("# %s%s%s #\n", paste(rep("=", eqStart), collapse = ""),
              foot, paste(rep("=", eqEnd), collapse = "")),
      file = file, append = TRUE)
}

# Function to add more sections:
CreateRscriptSec <- function(file, sections) {
  # Add sections to code:
  for (sec in sections) {
    sec <- sprintf(" ==== %s: ====", toupper(sec))
    cat(sprintf("# %s #\n", paste(rep("=", nchar(sec) - 1), collapse = "")),
        file = file, append = TRUE)
    cat(sprintf("#%s\n", sec), file = file, append = TRUE)
    cat(sprintf("# %s #\n\n", paste(rep("=", nchar(sec) - 1), collapse = "")),
        file = file, append = TRUE)
  }
}

# ================================== #
# ==== FUNCTIONS FOR R PACKAGE: ====
# ================================== #
# Function to create a package:
CreateRpackage <- function(pkgName, mainDir, git = FALSE, RstProj = FALSE) {
  # Project directory:
  pkgDir <- sprintf("%s%s/", mainDir, pkgName)
  
  # Create directory if the project doesn't exist:
  if (!dir.exists(pkgDir)) {
    dir.create(pkgDir)
  } else {
    stop(sprintf("Package %s already exists.", pkgName), call. = FALSE)
  }
  
  # Directory levels:
  dirLevs <- list()
  
  # Sub-directories for project:
  dirLevs$lev01 <- c("devel", "latestBuild", "pkg")
  
  # Data sub-directories:
  dirLevs$lev02 <- list(pkg = c("data", "inst", "man", "R"),
                        inst = "doc")
  
  # Create files:
  for (lev1 in dirLevs$lev01) {
    dir.create(sprintf("%s%s/", pkgDir, lev1), showWarnings = FALSE)
  }
  for (ii in 1:length(dirLevs$lev02)) {
    lev1 <- names(dirLevs$lev02)[ii]
    for (jj in 1:length(dirLevs$lev02[[lev1]])) {
      lev2 <- dirLevs$lev02[[lev1]][jj]
      dir.create(sprintf("%s%s/%s/", pkgDir, lev1, lev2), showWarnings = FALSE)
    }
  }
  
  # Create R studio project file:
  if (RstProj) {
    RprojCont <- "Version: 1.0\nRestoreWorkspace: Default\nSaveWorkspace: Default\nAlwaysSaveHistory: Default\n\nEnableCodeIndexing: Yes\nUseSpacesForTab: Yes\nNumSpacesForTab: 2\nEncoding: UTF-8\n\nRnwWeave: knitr\nLaTeX: pdfLaTeX"
    cat(RprojCont, file = sprintf("%s%s.Rproj", pkgDir, pkgName))
  }
  
  # Setup git 
  if (git) {
    system(sprintf("cd %s\ngit init", pkgDir))
  }
}

# Create description file:
CreatePkgDescrip <- function(pkgName, mainDir, title = NULL, 
                             version = NULL, authors = NULL,
                             maintainer = NULL, license = NULL) {
  # Project directory:
  pkgDir <- sprintf("%s%s/", mainDir, pkgName)
  
  # sections:
  sections <- c("Package", "Type", "Title", "Version",
                "Date", "Author", "Maintainer", "Depends",
                "Imports", "Description", "License", "LazyLoad", "LazyData")
  
  # Section content:
  secCont <- c(pkgName, "Package", "", "", as.character(Sys.Date()),
               "", "", "R (>= 2.10)", "", "", "GPL", "yes", "yes")
  
  # Title:
  if (!is.null(title)) {
    secCont[which(sections == "Title")] <- title
  }
  
  # Title:
  if (!is.null(version)) {
    secCont[which(sections == "Version")] <- version
  }
  
  # Author names:
  if (!is.null(authors)) {
    secCont[which(sections == "Author")] <- paste(authors, collapse = ", ")
  }
  
  # Maintainer:
  if (!is.null(maintainer)) {
    secCont[which(sections == "Maintainer")] <- paste(maintainer, 
                                                      collapse = ", ")
  }
  
  # License:
  if (!is.null(license)) {
    secCont[which(sections == "License")] <- license
  }
  
  # File path:
  descPath <- sprintf("%spkg/DESCRIPTION", pkgDir)
  
  for (ii in 1:length(sections)) {
    if (ii == 1) append <- FALSE else append <- TRUE
    cat(sprintf("%s: %s\n", sections[ii], secCont[ii]),
        file = descPath, append = append)
  }
}

# Create namespace file:
CreateNamespace <- function(pkgName, mainDir, scriptFile, import = NULL) {
  # Project directory:
  pkgDir <- sprintf("%s%s/", mainDir, pkgName)
  
  # code file path:
  scriptPath <- sprintf("%spkg/R/%s.R", pkgDir, scriptFile)
  
  # Namespace path:
  nmspPath <- sprintf("%spkg/NAMESPACE", pkgDir)
  
  # Find functions in scriptFile:
  env <- attach(NULL, name = "tempenv")
  sys.source(scriptPath, envir = env)
  funNames <- ls(envir = env)
  detach("tempenv")
  
  # Create export:
  cat("#Exports:\n", file = nmspPath)
  cat(sprintf("export(%s)\n\n", paste(paste('\"', funNames, '\"', sep = ""), 
                               collapse = ", ")), file = nmspPath, 
      append = TRUE)
  
  # Imports:
  if (!is.null(import)) {
    cat("Imports:\n", file = nmspPath, append = TRUE)
    for (ii in 1:length(import)) {
      cat(sprintf("import(%s)\n", import[ii]), file = nmspPath, 
          append = TRUE)
    }
  }
  
  # Find S3 methods:
  idS3 <- grep("[[:alnum:]]{+}[[:punct:]]{1}[[:alnum:]]{+}", funNames)
  if (length(idS3) > 0) {
    cat("\n#S3 methods:\n", file = nmspPath, append = TRUE)
    for (ii in idS3) {
      tempFun <- strsplit(funNames[ii], "[[:punct:]]")[[1]]
      cat(sprintf("S3method(\"%s\", \"%s\")\n", tempFun[1], tempFun[2]), 
          file = nmspPath, append = TRUE)
    }
  }
}

# Create .Rd files:
CreateRdFiles <- function(pkgName, mainDir, scriptFile, authorNames = NULL,
                          authorEmails = NULL, license = NULL) {
  # Project directory:
  pkgDir <- sprintf("%s%s/", mainDir, pkgName)
  
  # ---------------------------------- #
  # ---- Create general .Rd file: ----
  # ---------------------------------- #
  rdPath <- sprintf("%spkg/man/%s-package.Rd", pkgDir, pkgName)
  # Name and aliases:
  cat(sprintf("\\name{%s-package}\n", pkgName), file = rdPath)
  cat(sprintf("\\alias{%s-package}\n", pkgName), file = rdPath, append = TRUE)
  cat(sprintf("\\alias{%s}\n\n", pkgName), file = rdPath, append = TRUE)
  
  # Doctype:
  cat("\\docType{package}\n\n", file = rdPath, append = TRUE)
  
  # Title and description:
  cat("\\title{\n FILL UP\n}\n\n\\description{\n FILL UP\n}\n\n", 
      file = rdPath, append = TRUE)
  
  # Details:
  cat("\\details{\n", file = rdPath, append = TRUE)
  cat("\\tabular{ll}{\n", file = rdPath, append = TRUE)
  cat(sprintf("\tPackage: \\tab %s \\cr\n", pkgName), file = rdPath, 
      append = TRUE)
  cat("\tType: \\tab Package\\cr\n", file = rdPath, append = TRUE)
  cat("\tVersion: \\tab 1.0.0\\cr\n", file = rdPath, append = TRUE)
  cat(sprintf("\tDate: \\tab %s\\cr\n", as.character(Sys.Date())), file = rdPath, 
      append = TRUE)
  if (is.null(license)) {
    cat("\tLicense: \\tab GNU General Public Licence\\cr\n", 
        file = rdPath, append = TRUE)
  } else {
    cat(sprintf("\tLicense: \\tab %s\\cr\n", license), file = rdPath, 
        append = TRUE)
  }
  cat("\tLazyLoad: \\tab yes\\cr\n", file = rdPath, append = TRUE)
  cat("}\n\n", file = rdPath, append = TRUE)
  cat("FILL UP DETAILS \n}\n\n", file = rdPath, append = TRUE)
  
  # Author:
  if (is.null(authorNames)) {
    cat("\\author{AUTHOR(S) NAME(S) \\email{authoremail}}\n\n", 
        file = rdPath, append = TRUE)
  } else {
    cat(sprintf("\\author{%s}\n\n", paste(sprintf("%s \\email{%s}", 
                                                  authorNames, authorEmails), 
                                          collapse = ", ")), 
        file = rdPath, append = TRUE)
  }
  # References:
  cat("\\references{\nFILL UP\n}", file = rdPath, append = TRUE)
  
  
  # ----------------------------------- #
  # ---- Create specific .Rd file: ----
  # ----------------------------------- #
  # code file path:
  scriptPath <- sprintf("%spkg/R/%s.R", pkgDir, scriptFile)
  
  env <- attach(NULL, name = "tempenv")
  sys.source(scriptPath, envir = env)
  funNames <- sort(ls(envir = env))
  nfuns <- length(funNames)
  
  # Create function list:
  funList <- list()
  
  # index for Rd file:
  idRdCount <- 0
  
  # Find Default methods:
  idDefault <- grep("default", funNames)
  nDefault <- length(idDefault)
  idSkip <- c()
  if (nDefault > 0) {
    for (idd in idDefault) {
      defFun <- gsub(".default", "", funNames[idd])
      idDefFun <- which(grepl(defFun, funNames) & !grepl(sprintf(".%s", defFun), 
                                                         funNames))
      idDefFun <- idDefFun[which(idDefFun != idd)]
      funArgs <- .ExtractArgs(funNames[idd])
      idsDef <- c(idDefFun, idd)
      idRdCount <- idRdCount + 1
      temp <- data.frame(name = defFun, method = "default", alias = funNames[idd],
                         idRd = idRdCount, stringsAsFactors = FALSE)
      if (idRdCount - 1 == 0) {
        funTab <- temp
      } else {
        funTab <- rbind(funTab, temp)
      }
      
      metl <- list(fun = defFun, method = "default",  call = funArgs$call, 
                   args = funArgs$args)
      funList[[defFun]] <- metl
      idSkip <- c(idSkip, idsDef)
    }
    remFuns <- funNames[-idSkip]
  } else {
    remFuns <- funNames
  }
  
  # Find remaining functions:
  nrem <- length(remFuns) 
  if (nrem > 0) {
    for (irem in 1:nrem) {
      ifun <- remFuns[irem]
      funArgs <- .ExtractArgs(ifun)
      if (grepl("[[:alnum:]]{1,}[[:punct:]]{1}[[:alnum:]]{1,}", 
                ifun)) {
        nameMet <- strsplit(ifun, "[[:punct:]]{1}")[[1]]
      } else {
        nameMet <- c(ifun, NA)
      }
      if (imet == 1 | !nameMet[2] %in% funTab$method | is.na(nameMet[2])) {
        idRdCount <- idRdCount + 1
        idRd <- idRdCount
      } else if (nameMet[2] %in% funTab$method) {
        idRd <- funTab$idRd[which(funTab$method == nameMet[2])[1]]
      }
      temp <- data.frame(name = ifun, method = nameMet[2], alias = ifun,
                         idRd = idRd, stringsAsFactors = FALSE)
      if (idRdCount - 1 == 0) {
        funTab <- temp
      } else {
        funTab <- rbind(funTab, temp)
      }
      
      metl <- list(fun = nameMet[1], method = nameMet[2], call = funArgs$call, 
                   args = funArgs$args)
      funList[[ifun]] <- metl
    }
  }
  
  detach("tempenv")
  
  # Index of Rd files:
  idRdVec <- sort(unique(funTab$idRd))
  
  for (iRd in idRdVec) {
    
    # Find functions for Rd file:
    idFuns <- which(funTab$idRd == iRd)
    
    # First function:
    fn <- funTab$name[idFuns[1]]
    
    # Path to .Rd file:
    rdPath <- sprintf("%spkg/man/%s.Rd", pkgDir, fn)
    
    # Name and aliases:
    cat(sprintf("\\name{%s}\n", fn), file = rdPath)
    cat(sprintf("\\alias{%s}\n", fn), file = rdPath, 
        append = TRUE)
    for (iif in idFuns) {
      cat(sprintf("\\alias{%s}\n", funTab$alias[iif]), file = rdPath, 
          append = TRUE)
    }
    
    # Title and description:
    cat("\n\\title{\n FILL UP\n}\n\n\\description{\n FILL UP\n}\n\n", 
        file = rdPath, append = TRUE)
    
    
    # Usage:
    cat("\\usage{\n ", file = rdPath, append = TRUE)
    for (iif in idFuns) {
      if (!is.na(funTab$method[iif])) {
        if (funTab$method[iif] == "default") {
          cat(sprintf("%s(%s, \\dots)\n\n", funTab$name[iif], 
                      gsub("[[:space:]]", "", funList[[iif]]$args[1])), 
              file = rdPath, append = TRUE)
        }
        cat(sprintf("\\method{%s}{%s}%s\n\n", funList[[iif]]$fun, 
                    funList[[iif]]$method, funList[[iif]]$call), file = rdPath,
            append = TRUE)
      } else {
        cat(sprintf("\\%s%s\n\n", funList[[iif]]$fun, funList[[iif]]$call), 
            file = rdPath, append = TRUE)
      }
    }
    cat("}\n\n ", file = rdPath, append = TRUE)
    
    # Arguments:
    for (iif in idFuns) {
      iargs <- gsub("[[:space:]]", "", funList[[iif]]$args)
      temp <- data.frame(args = iargs, id = 1:length(iargs))
      if (iif == idFuns[1]) {
        argOrd <- temp
      } else {
        argOrd <- rbind(argOrd, temp)
      }
    }
    allArgs <- unique(argOrd$args[sort.int(argOrd$id, index.return = TRUE)$ix])
    iddots <- which(allArgs == "...")
    if (length(iddots) == 1) {
      allArgs <- c(allArgs[-iddots], "\\dots")
    }
    
    cat("\\arguments{\n", file = rdPath, append = TRUE)
    for (ar in allArgs) {
      cat(sprintf("\t\\item{%s }{FILL UP}\n\n", ar), file = rdPath, 
          append = TRUE)
    }
    
    cat("}\n\n", file = rdPath, append = TRUE)
    
    # Details:
    cat("\\details{\nFILL UP \n}\n\n", file = rdPath, append = TRUE)
    
    # Value:
    cat("\\value{\n\t\\item{fill up }{FILL UP}\n}\n\n", file = rdPath, 
        append = TRUE)
    
    # Author:
    if (is.null(authorNames)) {
      cat("\\author{AUTHOR(S) NAME(S) \\email{authoremail}}\n\n", 
          file = rdPath, append = TRUE)
    } else {
      cat(sprintf("\\author{%s}\n\n", paste(sprintf("%s \\email{%s}", 
                                                    authorNames, 
                                                    authorEmails), 
                                            collapse = ", ")), 
          file = rdPath, append = TRUE)
    }
    
    # See also:
    cat("\\seealso{FILL UP}\n\n", file = rdPath, append = TRUE)
    
    # Examples:
    cat("\\examples{FILL UP}\n\n", file = rdPath, append = TRUE)
    
    # Keywords:
    cat("\\keyword{FILL UP}\n\n", file = rdPath, append = TRUE)
  }
}

# Hidden function to extract function arguments:
.ExtractArgs <- function(fn) {
  fnchar <- deparse(get(fn))
  
  notp <- FALSE
  ni <- 0
  while (!notp) {
    ni <- ni + 1
    notp <- grepl(")", fnchar[ni])
  }
  
  arcall <- c()
  arargs <- c()
  for (ii in 1:ni) {
    ar1 <- fnchar[ii]
    if (ii == 1) {
      ar <- gsub(pattern = "function[[:space:]]{1}[[:punct:]]", 
                 replacement = "", x = ar1)
      ar1 <- gsub(pattern = "function[[:space:]]{1}", replacement = "", 
                  x = ar1)
    } else {
      ar <- gsub(pattern = "[[:space:]]{2,}", replacement = "", x = ar1)
      ar1 <- gsub(pattern = "[[:space:]]{2,}", replacement = "", x = ar1)
    }
    if (ii == ni) {
      ar1 <- gsub(pattern = ") ", replacement = ")", x = ar1)
      ar <- gsub(pattern = ") ", replacement = "", x = ar)
    }
    ar <- gsub(pattern = "[[:space:]]{1}[[:punct:]]{1}[[:space:]]{1}[[:alpha:]]{2,}", replacement = "", x = ar)
    arcall <- paste(arcall, ar1, collapse = ", ")
    arargs <- paste(arargs, ar, collapse = ", ")
  }
  
  arargs <- strsplit(arargs, ", ")[[1]]
  for (jj in 1:length(arargs)) {
    if (grepl(" = ", arargs[jj])) {
      arargs[jj] <- strsplit(arargs[jj], " = ")[[1]][1] 
    }
  }
  arlist <- list(call = arcall, args = arargs)
  return(arlist)
}

