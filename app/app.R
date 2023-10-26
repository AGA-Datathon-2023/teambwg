
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


source("config.R")
source("process_data.R")
source("ui.R")
source("server.R")

shinyApp(ui, server, options = (list(launch.browser = T)))
