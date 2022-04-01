library(CreateRproj)

# Main directory:
mainDir <- "~/FERNANDO/PROJECTS/1.ACTIVE/MaleFemaleSexDiff/CODE/"

# Project name (folder for code):
projName <- "08.RegressionAnalysis/"

# Code name:
codeName <- "SexDiffsRegressions"

# Create empty code for functions:
CreateRcode(codeFile = codeName, projName = projName, mainDir = mainDir, 
            sections = c("General setup", "Load data", 
                         "Data prep.", "Analysis", "Explore results"))

