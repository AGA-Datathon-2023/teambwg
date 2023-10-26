server <- function(input, output, session) {
  output$agency_perc <- renderEcharts4r({
    
    if (input$agency_switch == T) {
      agency_df <- filter_df(agency_base, input$agencyselect, "agency", oblig_switch = T)
    } else {
      agency_df <- filter_df(agency_base, input$agencyselect, "agency", oblig_switch = F)
    }
    graph_data(agency_df, "agency")$e %>% 
      e_title(subtext = "Source: usaspending.gov FY 2024", sublink = "https://www.usaspending.gov/search/?hash=a071e13e9c5f880f1e9e8c76bc6900e8")
    
  })
  output$state_perc <- renderEcharts4r({
    
    if (input$state_switch == T) {
      state_df <- filter_df(state_base, input$stateselect, "state", oblig_switch = T)
    } else {
      state_df <- filter_df(state_base, input$stateselect, "state", oblig_switch = F)
    }
    graph_data(state_df, "state")$e %>% 
      e_title(subtext = "Source: usaspending.gov FY 2024", sublink = "https://www.usaspending.gov/search/?hash=a071e13e9c5f880f1e9e8c76bc6900e8")
    
  })
  output$budget_agency <- renderEcharts4r({
    
    if (input$switch_compare == F & input$switch_type == T) {
    
      e <- graph_pie(agency_base, input$agencyselect_compare, category = "obligations", drill = "agency", "donut")
      
    } else if (input$switch_compare == F & input$switch_type == F) {
    
      e <- graph_pie(agency_base, input$agencyselect_compare, category = "obligations", drill = "agency", "radial")
    } else if (input$switch_compare == T & input$switch_type == T) {
    
      e <- graph_pie(agency_base, input$agencyselect_compare, "count", "agency", "donut")
      
    } else if (input$switch_compare == T & input$switch_type == F) {
    
      e <- graph_pie(agency_base, input$agencyselect_compare, "count", "agency", "radial")
    }
    
    title_short <- agency_base %>% 
      filter(awarding_agency_name %in% input$agencyselect_compare) %>% 
      head(1) %>% 
      pull(agency_short)
    
    e %>% 
      e_title(title_short,
              subtext = "Source: usaspending.gov FY 2024", sublink = "https://www.usaspending.gov/search/?hash=a071e13e9c5f880f1e9e8c76bc6900e8")
    
  })
  
  output$budget_state <- renderEcharts4r({
    
    if (input$switch_compare == F & input$switch_type == T) {
    
      e <- graph_pie(state_base, input$stateselect_compare, category = "obligations", drill = "state", "donut")
      
    } else if (input$switch_compare == F & input$switch_type == F) {
    
      e <- graph_pie(state_base, input$stateselect_compare, category = "obligations", drill = "state", "radial")
    } else if (input$switch_compare == T & input$switch_type == T) {
    
      e <- graph_pie(state_base, input$stateselect_compare, "count", "state", "donut")
      
    } else if (input$switch_compare == T & input$switch_type == F) {
    
      e <- graph_pie(state_base, input$stateselect_compare, "count", "state", "radial")
    }
    e %>% 
      e_title(input$stateselect_compare,
              subtext = "Source: usaspending.gov FY 2024", sublink = "https://www.usaspending.gov/search/?hash=a071e13e9c5f880f1e9e8c76bc6900e8")
  })
  
  
}