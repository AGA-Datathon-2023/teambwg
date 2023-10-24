server <- function(input, output, session) {
  output$dropdown <- renderText(input$mtcars_dropdown)
  output$agency_perc <- renderEcharts4r({
    graph_agency(agency_base, input$agencyselect)$e
  })
}