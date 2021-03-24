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
CreateRcode <- function(codeFile, projName, mainDir, sections = NULL) {
  # Code file name:
  codeFilePath <- sprintf("%s%s/02code/%s.R", mainDir, projName, codeFile)
  
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
      file = codeFilePath)
  
  for (hlabs in headLabs) {
    cat(sprintf("# %s\n", hlabs), file = codeFilePath, append = TRUE)  
  }
  
  # Header end:
  eqStart <- floor((nEq - nchHeads[2])/2)
  eqEnd <- ceiling((nEq - nchHeads[2])/2)
  cat(sprintf("# %s%s%s #\n", paste(rep("=", eqStart), collapse = ""),
              metHeads[2], paste(rep("=", eqEnd), collapse = "")),
      file = codeFilePath, append = TRUE)
  
  # ------------------------- #
  # ---- setup sections: ----
  # ------------------------- #
  # Initial setup section:
  setupSub <- c("Libraries", "Working directory", "Logical for saving results",
                "Sourced files")
  sec <- " ==== SETUP: ==== "
  cat(sprintf("# %s #\n", paste(rep("=", nchar(sec) - 2), collapse = "")),
      file = codeFilePath, append = TRUE)
  cat(sprintf("#%s\n", sec), file = codeFilePath, append = TRUE)
  cat(sprintf("# %s #\n", paste(rep("=", nchar(sec) - 2), collapse = "")),
      file = codeFilePath, append = TRUE)
  for (setsub in setupSub) {
    cat(sprintf("# %s:\n\n", setsub), file = codeFilePath, append = TRUE)
  }
  
  # Additional sections:
  if (!is.null(sections)) {
    CreateRcodeSec(codeFile = codeFile, mainDir = mainDir, projName = projName, 
                   sections = sections)
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
      file = codeFilePath, append = TRUE)
}

# Function to add more sections:
CreateRcodeSec <- function(codeFile, projName, mainDir, sections) {
  
  # Code file name:
  codeFilePath <- sprintf("%s%s/02Code/%s.R", mainDir, projName, codeFile)
  
  # Add sections to code:
  for (sec in sections) {
    sec <- sprintf(" ==== %s: ====", toupper(sec))
    cat(sprintf("# %s #\n", paste(rep("=", nchar(sec) - 2), collapse = "")),
        file = codeFilePath, append = TRUE)
    cat(sprintf("#%s\n", sec), file = codeFilePath, append = TRUE)
    cat(sprintf("# %s #\n\n", paste(rep("=", nchar(sec) - 2), collapse = "")),
        file = codeFilePath, append = TRUE)
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
CreatePkgDescrip <- function(pkgName, mainDir) {
  # Project directory:
  pkgDir <- sprintf("%s%s/", mainDir, pkgName)
  
  # sections:
  sections <- c("Package", "Type", "Title", "Version",
                "Date", "Author", "Maintainer", "Depends",
                "Imports", "Description", "License", "LazyData")
  
  # File path:
  descPath <- sprintf("%spkg/DESCRIPTION", pkgDir)
  
  for (ii in 1:length(sections)) {
    if (ii == 1) {
      cat(sprintf("%s: %s\n", sections[ii], pkgName),
          file = descPath)
    } else if (ii == 2) {
      cat(sprintf("%s: Package\n", sections[ii]),
          file = descPath, append = TRUE)
    } else if (sections[ii] == "Date") {
      cat(sprintf("%s: %s\n", sections[ii], as.character(Sys.Date())),
          file = descPath, append = TRUE)
    } else if (sections[ii] == "License") {
      cat(sprintf("%s: %s\n", sections[ii], "GPL"),
          file = descPath, append = TRUE)
    } else if (grepl("Lazy", sections[ii])) {
      cat(sprintf("%s: %s\n", sections[ii], "yes"),
          file = descPath, append = TRUE)
    } else {
      cat(sprintf("%s: \n", sections[ii]),
          file = descPath, append = TRUE)
    }
  }
}

# Create namespace file:
CreateNamespace <- function(pkgName, mainDir, codeFile, import = NULL) {
  # Project directory:
  pkgDir <- sprintf("%s%s/", mainDir, pkgName)
  
  # code file path:
  codePath <- sprintf("%spkg/R/%s.R", pkgDir, codeFile)
  
  # Namespace path:
  nmspPath <- sprintf("%spkg/NAMESPACE", pkgDir)
  
  # Find functions in codeFile:
  env <- attach(NULL, name = "tempenv")
  sys.source(codePath, envir = env)
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
    cat("#S3 methods:\n", file = nmspPath, append = TRUE)
    for (ii in idS3) {
      cat(sprintf("S3method(\"%s\")\n", funNames[ii]), file = nmspPath, 
          append = TRUE)
    }
  }
}

# Create .Rd files:
CreateRdFiles <- function(pkgName, mainDir, codeFile) {
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
  cat("\tLicense: \\tab GNU General Public Licence\\cr\n", file = rdPath, 
      append = TRUE)
  cat("\tLazyLoad: \\tab yes\\cr\n", file = rdPath, append = TRUE)
  cat("}\n\n", file = rdPath, append = TRUE)
  cat("FILL UP DETAILS \n}\n\n", file = rdPath, append = TRUE)
  
  # Author:
  cat("\\author{AUTHOR(S) NAME(S) \\email{authoremail}}\n\n", 
      file = rdPath, append = TRUE)
  
  # References:
  cat("\\references{\nFILL UP\n}", file = rdPath, append = TRUE)
  
  
  # ----------------------------------- #
  # ---- Create specific .Rd file: ----
  # ----------------------------------- #
  # code file path:
  codePath <- sprintf("%spkg/R/%s.R", pkgDir, codeFile)
  
  # Find functions in codeFile:
  env <- attach(NULL, name = "tempenv")
  sys.source(codePath, envir = env)
  funNames <- sort(ls(envir = env))
  arlist <- list()
  for (fn in funNames) {
    ar1 <- deparse(get(fn))[1]
    ar <- gsub(pattern = "function[[:space:]]{1}[[:punct:]]", 
               replacement = "", x = ar1)
    ar1 <- gsub(pattern = "function[[:space:]]{1}", replacement = "", x = ar1)
    ar1 <- gsub(pattern = ") ", replacement = ")", x = ar1)
    ar <- gsub(pattern = ") ", replacement = "", x = ar)
    ar <- gsub(pattern = "[[:space:]]{1}[[:punct:]]{1}[[:space:]]{1}[[:alpha:]]{2,}", replacement = "", x = ar)
    ar2 <- strsplit(ar, ", ")[[1]]
    arlist[[fn]] <- list(call = ar1, args = ar2)
  }
  detach("tempenv")
  
  # Number of functions:
  nfuns <- length(funNames)
  
  # Create .Rd files:
  skipfun <- c()
  for (fn in funNames) {
    if (!fn %in% skipfun) {
      idfun <- which(funNames == fn)
      alfuns <- NA
      # Find S3 methods for original function (aliases):
      if (idfun < nfuns) {
        idrem <- (idfun + 1):nfuns
        idS3funs <- grep(fn, funNames[idrem])
        if (length(idS3funs) > 0) {
          idS3 <- grep("[[:alnum:]]{+}[[:punct:]]{1}[[:alnum:]]{+}", 
                       funNames[idS3funs])
          alfuns <- funNames[idrem[idS3]]
          skipfun <- c(skipfun, alfuns)
        } else {
          alfuns <- NA
        }
      } 
      rdPath <- sprintf("%spkg/man/%s.Rd", pkgDir, fn)
      
      # Name and aliases:
      cat(sprintf("\\name{%s}\n", fn), file = rdPath)
      cat(sprintf("\\alias{%s}\n", fn), file = rdPath, append = TRUE)
      if (!is.na(alfuns[1])) {
        for (alfn in alfuns) {
          cat(sprintf("\\alias{%s}\n", alfn), file = rdPath, append = TRUE)
        }
      }
      # Title and description:
      cat("\n\\title{\n FILL UP\n}\n\n\\description{\n FILL UP\n}\n\n", 
          file = rdPath, append = TRUE)
      
      # Usage:
      alist <- arlist[[fn]]
      def <- FALSE
      if (!is.na(alfuns[1])) {
        idefault <- grep("default", alfuns)
        if (length(idefault) > 0) {
          alist <- arlist[[alfuns[idefault]]]
          def <- TRUE
        } 
      }
      
      if (def) {
        usage <- sprintf("\\usage{\\method{%s}{default}%s}\n\n", fn, alist$call)
      } else {
        usage <- sprintf("\\usage{%s%s}\n\n", fn, alist$call)
      }
      
      cat(usage, file = rdPath, append = TRUE)
      
      # Arguments:
      cat("\\arguments{\n", file = rdPath, append = TRUE)
      for (ar in alist$args) {
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
      cat("\\author{AUTHOR(S) NAME(S) \\email{authoremail}}\n\n", 
          file = rdPath, append = TRUE)
      
      # See also:
      cat("\\seealso{FILL UP}\n\n", file = rdPath, append = TRUE)
      
      # Examples:
      cat("\\examples{FILL UP}\n\n", file = rdPath, append = TRUE)
      
      # Keywords:
      cat("\\keyword{FILL UP}\n\n", file = rdPath, append = TRUE)
    }
    
  }
  
}
