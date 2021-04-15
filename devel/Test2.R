library(CreateRproj)

# Main directory:
mainDir <- "~/FERNANDO/PROJECTS/4.PACKAGES/"

# package name:
pkgName <- "BaFTA"

# Create package:
CreateRpackage(pkgName = pkgName, mainDir = mainDir, git = TRUE, 
               RstProj = TRUE)

# Create package description:
CreatePkgDescrip(pkgName = pkgName, mainDir = mainDir)

# Create project for gorilla disease project:
projName <- "popDisease"

# Project:
CreateRproj(projName = projName, mainDir = mainDir, git = TRUE, 
            RstProj = TRUE)

# Code name:
codeName <- "popDiseaseFunctions"

# Create empty code for functions:
CreateRcode(codeFile = codeName, projName = projName, mainDir = mainDir, 
            sections = c("Population projection", "Demographic functions", 
                         "results management", "Internal functions"))

# Create empty code to run models:
codeName <- "popDiseaseRuns"
CreateRcode(codeFile = codeName, projName = projName, mainDir = mainDir, 
            sections = c("demographic data object", "Single model run", 
                         "Multiple model runs"))

