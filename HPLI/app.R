# app.R

# Noé Vandevoorde
# octobre 2025


#### Packages ##################################################################

load_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!require(
      pkg,
      character.only = TRUE,
      quietly = TRUE,
      warn.conflicts = FALSE)) {
      install.packages(
        pkg,
        dependencies = TRUE)
      library(
        pkg,
        character.only = TRUE,
        warn.conflicts = FALSE)
    }
  }
}

load_packages(c(
  "tidyverse"
  ,"shiny"
  ,"shinydashboard"
))


#### Sources ###################################################################

source("HPLI/prep_utils.R")
source("HPLI/ui.R")
source("HPLI/server.R")


#### Run the app ###############################################################

shinyApp(ui = ui, server = server)
