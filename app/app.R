
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


source("config.R")
source("process_agency.R")
source("ui.R")
source("server.R")

shinyApp(ui, server)
