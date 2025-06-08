library(CreateRproj)

# Main directory:
mainDir <- "~/FERNANDO/PROJECTS/1.ACTIVE/Species360/Projects/IndMemberReports/analysis/Species360GitHub/"

# Project name (folder for code):
projName <- "SRGanalytics"

# Create project:
CreateRproj(projName = projName, mainDir = mainDir, git = TRUE, 
            RstProj = TRUE)
# Code name:
codeName <- "ISRinstitutionAnalysis"

# Create empty code for functions:
CreateRscript(file = sprintf("%s/%s/02code/%s", mainDir, 
                                 projName, codeName), 
              sections = c("General setup", "Load data", "Data prep.", 
                           "Analysis"))

