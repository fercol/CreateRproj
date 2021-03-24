# ================ #
# ==== SETUP: ====
# ================ #
# Project name:
projName <- "TestRproj"

# Main directory:
mainDir <- "~/FERNANDO/PROJECTS/4.PACKAGES/"

# Code file name:
codeFile <- "test"

# Sections for code:
sections <- c("functions", "analysis", "results")

# Source files:
source("~/FERNANDO/PROJECTS/4.PACKAGES/CreateRproj/pkg/R/CreateRproj.R")

# ================================== #
# ==== CREATE PROJECT AND CODE: ====
# ================================== #
# Project:
CreateRproj(projName = projName, mainDir = mainDir)

# R code:
CreateRcode(codeFile = codeFile, projName = projName, mainDir = mainDir,
            sections = sections)

# Add sections:
CreateRcodeSec(codeFile = codeFile, projName = projName, mainDir = mainDir,
               sections = sections)

# ================================= #
# ==== CREATE EMPTY R-PACKAGE: ====
# ================================= #
# Package name:
pkgName <- "TestPackage" 

# Create package:
CreateRpackage(pkgName = pkgName, mainDir = mainDir)

# Create package description:
CreatePkgDescrip(pkgName = pkgName, mainDir = mainDir)

# Create namespace file (Requires a code file):
# pkgName <- "CreateRproj"
# codeFile <- "CreateRproj"
CreateNamespace(pkgName = pkgName, mainDir = mainDir, codeFile = codeFile, 
                import = NULL)

# Create .Rd (help) files (requires a code file):
CreateRdFiles(pkgName = pkgName, mainDir = mainDir, codeFile = codeFile)

