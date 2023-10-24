server <- function(input, output, session) {
  source("C:/Users/gdlev/Desktop/Ideas/aga/shiny.R")
  output$dropdown <- renderText(input$mtcars_dropdown)
  output$agency_perc <- renderEcharts4r({
    graph_agency(agency_base, input$agencyselect)$e
  })
}