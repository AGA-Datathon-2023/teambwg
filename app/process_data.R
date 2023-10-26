## Read in the data
data <- read.csv("../data/All_Prime_Awards_FY2024.csv", colClasses = "factor")


base_chart <- data %>%
  filter(recipient_country_code == "USA") %>% 
  select(total_obligated_amount, awarding_agency_name, recipient_state_name, awarding_agency_name, veteran_owned_business,
         woman_owned_business, subcontinent_asian_asian_indian_american_owned_business,
         asian_pacific_american_owned_business,	black_american_owned_business,
         hispanic_american_owned_business,	native_american_owned_business,
         other_minority_owned_business) %>% 
  mutate(total_obligated_amount = as.numeric(total_obligated_amount)) %>% 
  mutate(designation = case_when(
    veteran_owned_business == "t" ~ "Veteran",
    woman_owned_business == "t" ~ "Woman",
    subcontinent_asian_asian_indian_american_owned_business == "t" ~ "Subcontinent Asian Asian Indian American",
    asian_pacific_american_owned_business == "t" ~ "Asian Pacific American",
    black_american_owned_business == "t" ~ "Black American",
    hispanic_american_owned_business == "t" ~ "Hispanic American",
    native_american_owned_business == "t" ~ "Native American",
    other_minority_owned_business == "t" ~ "Other Minority",
    T ~ "No Designation"
  )) %>% 
  mutate(
    agency_short = case_when(
      awarding_agency_name == "Agency for International Development" ~ "USAID",                                  
      awarding_agency_name == "Committee for Purchase from People Who Are Blind or Severely Disabled" ~ "USAbilityOne", 
      awarding_agency_name == "Commodity Futures Trading Commission" ~ "CFTC",                                  
      awarding_agency_name == "Consumer Financial Protection Bureau" ~ "CFPB",                                  
      awarding_agency_name == "Consumer Product Safety Commission" ~ "CPSC",                                    
      awarding_agency_name == "Corporation for National and Community Service" ~ "CNCS",                        
      awarding_agency_name == "Council of the Inspectors General on Integrity and Efficiency" ~ "CIGIE",         
      awarding_agency_name == "Court Services and Offender Supervision Agency" ~ "CSOSA",                        
      awarding_agency_name == "Defense Nuclear Facilities Safety Board" ~ "DNFSB",                               
      awarding_agency_name == "Department of Agriculture" ~ "USDA",                                             
      awarding_agency_name == "Department of Commerce" ~ "DOC",                                                
      awarding_agency_name == "Department of Defense" ~ "DOD",                                                 
      awarding_agency_name == "Department of Education" ~ "ED",                                               
      awarding_agency_name == "Department of Energy" ~ "DOE",                                                  
      awarding_agency_name == "Department of Health and Human Services" ~ "DHHS",                               
      awarding_agency_name == "Department of Homeland Security" ~ "DHS",                                       
      awarding_agency_name == "Department of Housing and Urban Development" ~ "HUD",                           
      awarding_agency_name == "Department of Justice" ~ "DOJ",                                                 
      awarding_agency_name == "Department of Labor" ~ "DOL",                                                   
      awarding_agency_name == "Department of State" ~ "DOS",                                                   
      awarding_agency_name == "Department of the Interior" ~ "DOI",                                            
      awarding_agency_name == "Department of the Treasury" ~ "TREAS",                                            
      awarding_agency_name == "Department of Transportation" ~ "DOT",                                          
      awarding_agency_name == "Department of Veterans Affairs" ~ "VA",                                        
      awarding_agency_name == "District of Columbia Courts" ~ "DDC",                                           
      awarding_agency_name == "Environmental Protection Agency" ~ "EPA",                                       
      awarding_agency_name == "Equal Employment Opportunity Commission" ~ "EEOC",                               
      awarding_agency_name == "Executive Office of the President" ~ "EOP",                                     
      awarding_agency_name == "Export-Import Bank of the United States" ~ "EIB",                               
      awarding_agency_name == "Federal Communications Commission" ~ "FCC",                                     
      awarding_agency_name == "Federal Maritime Commission" ~ "FMC",                                           
      awarding_agency_name == "Federal Trade Commission" ~ "FTC",                                              
      awarding_agency_name == "General Services Administration" ~ "GSA",                                       
      awarding_agency_name == "Government Accountability Office" ~ "GAO",                                      
      awarding_agency_name == "International Trade Commission" ~ "ITC",                                        
      awarding_agency_name == "Merit Systems Protection Board" ~ "MSPB",                                        
      awarding_agency_name == "Millennium Challenge Corporation" ~ "MCC",                                      
      awarding_agency_name == "Morris K. Udall and Stewart L. Udall Foundation" ~ "UDALL",                       
      awarding_agency_name == "National Aeronautics and Space Administration" ~ "NASA",                         
      awarding_agency_name == "National Archives and Records Administration" ~ "NARA",                          
      awarding_agency_name == "National Gallery of Art" ~ "NGOA",                                               
      awarding_agency_name == "National Labor Relations Board" ~ "NLRB",                                        
      awarding_agency_name == "National Science Foundation" ~ "NSF",                                           
      awarding_agency_name == "National Transportation Safety Board" ~ "NTSB",                                  
      awarding_agency_name == "Nuclear Regulatory Commission" ~ "NRC",                                         
      awarding_agency_name == "Office of Personnel Management" ~ "OPM",                                        
      awarding_agency_name == "Peace Corps" ~ "PEACE",                                                           
      awarding_agency_name == "Pension Benefit Guaranty Corporation" ~ "PBGC",                                  
      awarding_agency_name == "Railroad Retirement Board" ~ "RRB",                                             
      awarding_agency_name == "Securities and Exchange Commission" ~ "SEC",                                    
      awarding_agency_name == "Small Business Administration" ~ "SBA",                                         
      awarding_agency_name == "Smithsonian Institution" ~ "SI",                                               
      awarding_agency_name == "Social Security Administration" ~ "SSA",                                        
      awarding_agency_name == "U.S. Agency for Global Media" ~ "USAGM",                                          
      awarding_agency_name == "U.S. International Development Finance Corporation" ~ "USIDFC",                    
      awarding_agency_name == "United States Chemical Safety Board" ~ "USCSB",                                   
      awarding_agency_name == "United States Trade and Development Agency" ~ "USTDA",
      T ~ "None"
    )
  )
  

agency_base <- base_chart %>% 
  group_by(awarding_agency_name,agency_short,designation) %>% 
  summarise(sum_obl = sum(total_obligated_amount), count_agency = n()) %>% 
  mutate(perc_oblig = sum_obl/sum(sum_obl),
         perc_count = count_agency/sum(count_agency)) %>% 
  ungroup()

state_base <- base_chart %>% 
  group_by(recipient_state_name,designation) %>% 
  summarise(sum_obl = sum(total_obligated_amount), count_state = n()) %>% 
  mutate(perc_oblig = sum_obl/sum(sum_obl),
         perc_count = count_state/sum(count_state)) %>% 
  ungroup() %>% 
  mutate(recipient_state_name = str_to_title(recipient_state_name))



filter_df <- function(df, long_names, drill = "agency", oblig_switch = T) {
  
  if (oblig_switch == T) {
    
    if (drill == "agency") {
      filters <- df %>% 
        filter(awarding_agency_name %in% long_names) %>% 
        distinct(agency_short) %>% 
        pull(agency_short)
      
      final_df <- df %>% 
        select(agency_short,designation,perc_oblig) %>% 
        filter(agency_short %in% filters) %>% 
        pivot_wider(
          names_from = "designation",
          values_from = "perc_oblig"
        )
      
      diff_cols <- setdiff(check_cols,colnames(final_df))
      
      if (length(diff_cols) > 0) {
        final_df[, diff_cols] <- 0
      }
      
      
    } else if (drill == "state") {
      filters <- df %>% 
        filter(recipient_state_name %in% long_names) %>% 
        distinct(recipient_state_name) %>% 
        pull(recipient_state_name)
      
      final_df <- df %>% 
        select(recipient_state_name,designation,perc_oblig) %>% 
        filter(recipient_state_name %in% filters) %>% 
        pivot_wider(
          names_from = "designation",
          values_from = "perc_oblig"
        )
      
      diff_cols <- setdiff(check_cols,colnames(final_df))
      
      if (length(diff_cols) > 0) {
        final_df[, diff_cols] <- 0
      }
      
      
    }
  } else {
    if (drill == "agency") {
      filters <- df %>% 
        filter(awarding_agency_name %in% long_names) %>% 
        distinct(agency_short) %>% 
        pull(agency_short)
      
      final_df <- df %>% 
        select(agency_short,designation,perc_count) %>% 
        filter(agency_short %in% filters) %>% 
        pivot_wider(
          names_from = "designation",
          values_from = "perc_count"
        )
      
      diff_cols <- setdiff(check_cols,colnames(final_df))
      
      if (length(diff_cols) > 0) {
        final_df[, diff_cols] <- 0
      }
      
      
    } else if (drill == "state") {
      filters <- df %>% 
        filter(recipient_state_name %in% long_names) %>% 
        distinct(recipient_state_name) %>% 
        pull(recipient_state_name)
      
      final_df <- df %>% 
        select(recipient_state_name,designation,perc_count) %>% 
        filter(recipient_state_name %in% filters) %>% 
        pivot_wider(
          names_from = "designation",
          values_from = "perc_count"
        )
      
      diff_cols <- setdiff(check_cols,colnames(final_df))
      
      if (length(diff_cols) > 0) {
        final_df[, diff_cols] <- 0
      }
      
      
    }
    
  }
  
  final_df <- final_df %>% 
    ungroup() %>% 
    mutate_at(vars(2:ncol(.)), ~replace_na(.,0))
  
  return (final_df)
}

graph_data <- function(df, drill = "agency") {
  
  if (drill == "agency") {
    
    e <- df %>% 
      e_charts(agency_short) %>% 
      e_bar(`Asian Pacific American`, stack="grp") %>%
      e_bar(`Black American`, stack="grp") %>%
      e_bar(`Hispanic American`, stack="grp") %>%
      e_bar(`Native American`, stack="grp") %>%
      e_bar(`Subcontinent Asian Asian Indian American`, stack="grp") %>%
      e_bar(Veteran, stack="grp") %>%
      e_bar(Woman, stack="grp") %>% 
      e_bar(`No Designation`, stack="grp") %>% 
      e_flip_coords() %>% 
      e_x_axis(
        max = 1,
        formatter = e_axis_formatter("percent", digits = 0)
      ) %>% 
      e_tooltip(trigger = c("item"),
                formatter = e_tooltip_item_formatter("percent", digits = 3))
  } else if (drill == "state") {
    
    e <- df %>% 
      e_charts(recipient_state_name) %>% 
      e_bar(`Asian Pacific American`, stack="grp") %>%
      e_bar(`Black American`, stack="grp") %>%
      e_bar(`Hispanic American`, stack="grp") %>%
      e_bar(`Native American`, stack="grp") %>%
      e_bar(`Subcontinent Asian Asian Indian American`, stack="grp") %>%
      e_bar(Veteran, stack="grp") %>%
      e_bar(Woman, stack="grp") %>% 
      e_bar(`No Designation`, stack="grp") %>% 
      e_flip_coords() %>% 
      e_x_axis(
        max = 1,
        formatter = e_axis_formatter("percent", digits = 0)
      ) %>% 
      e_tooltip(trigger = c("item"),
                formatter = e_tooltip_item_formatter("percent", digits = 3))
  }
  
  list(
    e = e
  )
  
}


graph_pie <- function(df, filter, category = "obligations", drill = "agency", type = "donut") {
  
  if (drill == "agency") {
    filters <- df %>% 
      filter(awarding_agency_name %in% filter) %>% 
      distinct(agency_short) %>% 
      pull(agency_short)
    
    final_df <- df %>% 
      filter(agency_short %in% filters) %>% 
      #filter(designation != "No Designation") %>% 
      rename("count" = count_agency)
    
    
  } else if (drill == "state") {
    
    final_df <- df %>% 
      filter(recipient_state_name %in% filter) %>% 
      #filter(designation != "No Designation") %>% 
      rename("count" = count_state)
    
  }
  if (category == "obligations") {
    
    if (type == "donut") {
      
      e <- final_df %>% 
        rename("Sum of Obligations" = sum_obl) %>% 
        e_charts(designation) %>% 
        e_pie(`Sum of Obligations`, radius = c("50%", "80%")) %>% 
        e_tooltip(
          trigger = "item"
        ) %>% 
        e_legend_unselect("No Designation")
    } else {
      e <- final_df %>% 
        rename("Sum of Obligations" = sum_obl) %>% 
        e_charts(designation) %>% 
        e_pie(`Sum of Obligations`, roseType = "radius") %>% 
        e_tooltip(
          trigger = "item"
        ) %>% 
        e_legend_unselect("No Designation")
      
    }
    
    
  } else if (category == "count") {
    
    if (type == "donut") {
      
      e <- final_df %>% 
        rename("Number of Contracts" = count) %>% 
        e_charts(designation) %>% 
        e_pie(`Number of Contracts`, radius = c("50%", "80%")) %>% 
        e_tooltip(
          trigger = "item"
        ) %>% 
        e_legend_unselect("No Designation")
      
    } else {
      e <- final_df %>% 
        rename("Number of Contracts" = count) %>% 
        e_charts(designation) %>% 
        e_pie(`Number of Contracts`, roseType = "radius") %>% 
        e_tooltip(
          trigger = "item"
        ) %>% 
        e_legend_unselect("No Designation")
      
    }
    
  }
  
  return(e)
}























# 
# agencies_long <- "Commodity Futures Trading Commission"
# agency_df <- agency_base
# 
# graph_agency <- function(agency_df, agencies_long) {
#   
#   agencies_short <- agency_df %>% 
#     filter(awarding_agency_name %in% agencies_long) %>% 
#     distinct(agency_short) %>% 
#     pull(agency_short)
#   
#   agency_graph <- agency_df %>% 
#     select(agency_short,designation,perc_oblig) %>% 
#     filter(agency_short %in% agencies_short) %>% 
#     pivot_wider(
#       names_from = "designation",
#       values_from = "perc_oblig"
#     )
#   
#   diff_cols <- setdiff(check_cols,colnames(agency_graph))
#   
#   if (length(diff_cols) > 0) {
#     agency_graph[, diff_cols] <- 0
#   }
#   
#   e <- agency_graph %>%
#     ungroup() %>% 
#     mutate_at(vars(2:ncol(.)), ~replace_na(.,0)) %>% 
#     e_charts(agency_short) %>% 
#     e_bar(`Asian Pacific American`, stack="grp") %>%
#     e_bar(`Black American`, stack="grp") %>%
#     e_bar(`Hispanic American`, stack="grp") %>%
#     e_bar(`Native American`, stack="grp") %>%
#     e_bar(`Subcontinent Asian Asian Indian American`, stack="grp") %>%
#     e_bar(Veteran, stack="grp") %>%
#     e_bar(Woman, stack="grp") %>% 
#     e_bar(`No Designation`, stack="grp") %>% 
#     e_flip_coords()
#   
#   list(
#     e = e
#   )
#   
# }
# desig <- base_chart %>% 
#   group_by(designation) %>% 
#   summarise(sum_obl = sum(total_obligated_amount), count_desig = n())
# 
# 
# 
# state <- base_chart %>%
#   group_by(recipient_state_name) %>% 
#   summarise(sum_obl = sum(total_obligated_amount), count_state = n())

# agency <- base_chart %>% 
#   group_by(agency_short) %>% 
#   summarise(sum_obl = sum(total_obligated_amount), count_agency = n()) %>% 
#   mutate(perc_oblig = sum_obl/sum(sum_obl),
#          perc_count = count_agency/sum(count_agency))