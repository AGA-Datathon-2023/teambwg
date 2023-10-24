overall_tab <- argonTabItem(
  tabName = "explore",
  argonH1("DEI Analysis", display = 4),
  div(style="display:inline-block",
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
        
      )
  ),
  div(style = "display:inline-block;
      margin-left:60px;",
      pickerInput(
        inputId = "stateselect",
        label = "Select State to Analyze",
        multiple = T,
        selected = "ALABAMA",
        choices = str_to_title(states),
        options = list(
          `actions-box` = T,
          `non-selected-text` = "Nothing Selected",
          `selected-text-format` = 'count > 3',
          `live-search` = T
        )
        
      )
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
          tabName = "Obligation Percentage",
          active = T,
          argonColumn(
            width = 12,
            echarts4r::echarts4rOutput(outputId = "agency_perc", height = "700px") %>% shinycssloaders::withSpinner(),
            height = "700px"
          )
        )
      )
    )
  )
)


ui <- argonDashPage(
  title = "Current Events",
  # header = argonHeader,
  # sidebar = argonSidebar,
  body = argonDashBody(
    argonTabItems(
      overall_tab
    )
  )
)