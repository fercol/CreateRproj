library(CreateRproj)

# Main directory:
mainDir <- "~/FERNANDO/PROJECTS/1.ACTIVE/BaboonEarlyAdversity/analysis/"
mainDir <- "~/FERNANDO/PROJECTS/1.ACTIVE/ContraceptionOnMort/"
mainDir <- "~/FERNANDO/PROJECTS/4.PACKAGES/"
# mainDir <- "~/Nextcloud/Shared/Projects/"

# set wd:
setwd(mainDir)

# project name:
projName <- "SlowFastMor"

# Create project:
if (!projName %in% list.files()) {
  CreateRproj(projName = projName, mainDir = mainDir, git = TRUE, 
              RstProj = TRUE)
}


# Script name:
scriptName <- "pHackingBiasExplore"

# Create R script:
CreateRscript(file = sprintf("%s/02code/%s.R", projName, scriptName), 
              sections = c("FUNCTIONS", "DATA PREP.", "ANALYSES"))