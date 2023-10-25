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
        iconList = lapply(X = 1:4, FUN = argonIcon, name = "atom"),
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
          active = T,
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