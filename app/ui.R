iconlist <- lapply(X = 1:3, FUN = argonIcon, name = "chart-bar-32")
iconlist[[3]]$attribs$class <- "ni text-3 ni-chart-pie-35"

agencies_compare <- agency_base %>% 
  filter(
    designation != "No Designation"
  ) %>% 
  distinct(awarding_agency_name) %>% 
  pull() %>% 
  as.character()

states_compare <- state_base %>% 
  filter(
    designation != "No Designation"
  ) %>% 
  distinct(recipient_state_name) %>% 
  pull() %>% 
  as.character()

overall_tab <- argonTabItem(
  tabName = "explore",
  argonH1("DEIA Contracting Analysis", display = 4),
  div(style="display:inline-block",
      
  ),
  div(style = "display:inline-block;
      margin-left:60px;"
  ),
  argonRow(
    argonColumn(
      argonTabSet(
        id = "tab-exp",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "sm",
        width = 12,
        iconList = iconlist,
        argonTab(
          tabName = "Agency Percentage",
          active = T,
          argonColumn(
            width = 4,
            pickerInput(
              inputId = "agencyselect",
              label = "Select Agency to Analyze",
              multiple = T,
              selected = "Agency for International Development",
              choices = agencies,
              options = list(
                `actions-box` = T,
                `non-selected-text` = "Nothing Selected",
                `selected-text-format` = 'count > 3',
                `live-search` = T
              )
              
            ),
            prettyToggle(
              inputId = "agency_switch",
              label_on = "Contract Count",
              label_off = "Contract Obligations",
              inline = T
            )),
          argonColumn(
            width = 12,
            echarts4r::echarts4rOutput(outputId = "agency_perc", height = "700px") %>% shinycssloaders::withSpinner(),
            height = "700px"
          )
        ),
        argonTab(
          tabName = "State Percentage",
          active = F,
          argonColumn(
            width = 4,
            pickerInput(
              inputId = "stateselect",
              label = "Select State to Analyze",
              multiple = T,
              selected = "Alabama",
              choices = str_to_title(states),
              options = list(
                `actions-box` = T,
                `non-selected-text` = "Nothing Selected",
                `selected-text-format` = 'count > 3',
                `live-search` = T
              )
              
            ),
            prettyToggle(
              inputId = "state_switch",
              label_on = "Contract Count",
              label_off = "Contract Obligations",
              inline = T
            )),
          argonColumn(
            width = 12,
            echarts4r::echarts4rOutput(outputId = "state_perc", height = "700px") %>% shinycssloaders::withSpinner(),
            height = "700px"
          )
        ),
        argonTab(
          tabName = "Budget Compare",
          active = F,
          argonColumn(
            width = 4,
            pickerInput(
              inputId = "agencyselect_compare",
              label = "Select Agency to Analyze",
              multiple = F,
              selected = "Agency for International Development",
              choices = agencies_compare,
              options = list(
                `actions-box` = T,
                `non-selected-text` = "Nothing Selected",
                `selected-text-format` = 'count > 3',
                `live-search` = T
              )
              
            ),
            pickerInput(
              inputId = "stateselect_compare",
              label = "Select State to Analyze",
              multiple = F,
              selected = "Alabama",
              choices = str_to_title(states_compare),
              options = list(
                `actions-box` = T,
                `non-selected-text` = "Nothing Selected",
                `selected-text-format` = 'count > 3',
                `live-search` = T
              )
              
            ),
            prettyToggle(
              inputId = "switch_compare",
              label_on = "Contract Count",
              label_off = "Contract Obligations",
              inline = T
            ),
            prettyToggle(
              inputId = "switch_type",
              label_on = "Donut Chart",
              label_off = "Radial Chart",
              inline = T
            )
            ),
          argonColumn(
            width = 12,
            echarts4r::echarts4rOutput(outputId = "budget_agency", height = "700px") %>% shinycssloaders::withSpinner(),
            height = "700px"
          ),
          argonColumn(
            width = 12,
            echarts4r::echarts4rOutput(outputId = "budget_state", height = "700px") %>% shinycssloaders::withSpinner(),
            height = "700px"
          )
        )
        
        
        
      )
    )
  )
)


ui <- argonDashPage(
  title = "DEIA Contracting Analysis",
  # header = argonHeader,
  # sidebar = argonSidebar,
  body = argonDashBody(
    argonTabItems(
      overall_tab
    )
  )
)