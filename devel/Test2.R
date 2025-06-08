library(CreateRproj)

# ================== #
# ==== PACKAGE: ====
# ================== #
# Main directory:
mainDir <- "~/FERNANDO/PROJECTS/4.PACKAGES/"

# package name:
pkgName <- "ageToSize"

# Create package:
CreateRpackage(pkgName = pkgName, mainDir = mainDir, git = TRUE, 
               RstProj = TRUE)

# Create package description:
descrTitle <- "Estimation of age to size functional forms with quantiles"
CreatePkgDescrip(pkgName = pkgName, mainDir = mainDir, title = descrTitle,
                 version = "1.0.0", 
                 authors = "Fernando Colchero <fernando_colchero@eva.mpg.de>",
                 maintainer = "Fernando Colchero <fernando_colchero@eva.mpg.de>")

# Script:

# Create project for gorilla disease project:
projName <- "testudinesSenescenceRcode"

# ================== #
# ==== PROJECT: ====
# ================== #
# Project:
CreateRproj(projName = projName, mainDir = mainDir, git = TRUE, 
            RstProj = TRUE)

# Code name:
codeName <- "SexDiffsRegressions"

# Create empty code for functions:
CreateRcode(codeFile = codeName, projName = projName, mainDir = mainDir, 
            sections = c("Population projection", "Demographic functions", 
                         "results management", "Internal functions"))

# Create empty code to run models:
codeName <- "popDiseaseRuns"
CreateRcode(codeFile = codeName, projName = projName, mainDir = mainDir, 
            sections = c("demographic data object", "Single model run", 
                         "Multiple model runs"))

