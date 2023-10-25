server <- function(input, output, session) {
  output$agency_perc <- renderEcharts4r({
    
    if (input$agency_switch == T) {
      agency_df <- filter_df(agency_base, input$agencyselect, "agency", oblig_switch = T)
    } else {
      agency_df <- filter_df(agency_base, input$agencyselect, "agency", oblig_switch = F)
    }
    graph_data(agency_df, "agency")$e
    
  })
  output$state_perc <- renderEcharts4r({
    
    if (input$state_switch == T) {
      state_df <- filter_df(state_base, input$stateselect, "state", oblig_switch = T)
    } else {
      state_df <- filter_df(state_base, input$stateselect, "state", oblig_switch = F)
    }
    graph_data(state_df, "state")$e
    
  })
}