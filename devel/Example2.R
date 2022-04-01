library(CreateRproj)

# Main directory:
mainDir <- "~/FERNANDO/PROJECTS/1.ACTIVE/BaboonEarlyAdversity/analysis/"
mainDir <- "~/FERNANDO/PROJECTS/1.ACTIVE/cancer/"

# set wd:
setwd(mainDir)

# project name:
projName <- "cancerAgeing"

# Create project:
if (!projName %in% list.files()) {
  CreateRproj(projName = projName, mainDir = mainDir, git = TRUE, 
              RstProj = TRUE)
}
# Script name:
scriptName <- "prepMorbMortdata"

# Create R script:
CreateRscript(file = sprintf("cancerAgeing/02code/%s.R", scriptName), 
              sections = c("FUNCTIONS", "DATA PREP."))