# ================ #
# ==== SETUP: ====
# ================ #
# library:
library(CreateRproj)

# Project name:
projName <- "EvolInvAgeRate"

# Main directory:
mainDir <- "~/FERNANDO/PROJECTS/1.ACTIVE/"

# Code file name:
codeFile <- "invAgeRateTest"

# Sections for code:
sections <- c("functions", "analysis", "results")

# Source files:
# source("~/FERNANDO/PROJECTS/4.PACKAGES/CreateRproj/pkg/R/CreateRproj.R")

# ================================== #
# ==== CREATE PROJECT AND CODE: ====
# ================================== #
# Project:
CreateRproj(projName = projName, mainDir = mainDir, git = T, RstProj = T)

# R code:
CreateRscript(file = sprintf("%s%s/02code/%s.R", mainDir, 
                             projName, codeFile), sections = sections)

# Add sections:
CreateRcodeSec(codeFile = codeFile, projName = projName, mainDir = mainDir,
               sections = sections)

# ================================= #
# ==== CREATE EMPTY R-PACKAGE: ====
# ================================= #
# Package name:
pkgName <- "basta2.0" 
mainDir <- "~/FERNANDO/PROJECTS/4.PACKAGES/BaSTA2.0/"
# Create package:
CreateRpackage(pkgName = pkgName, mainDir = mainDir, git = TRUE, 
               RstProj = TRUE)

# Create package description:
CreatePkgDescrip(pkgName = pkgName, mainDir = mainDir)

# Create namespace file (Requires a code file):
# pkgName <- "CreateRproj"
# codeFile <- "CreateRproj"
scriptFile <- "basta"
CreateNamespace(pkgName = pkgName, mainDir = mainDir, scriptFile = scriptFile, 
                import = "snowfall")

# Create .Rd (help) files (requires a code file):
CreateRdFiles(pkgName = pkgName, mainDir = mainDir, scriptFile = scriptFile,
              authorNames = "Fernando Colchero", 
              authorEmails = "colchero@imada.sdu.dk")

