library(shiny)
library(shinydashboard)
library(dygraphs)
options(scipen = 500)
source("functions.R")

#Header elements for the visualisation
header <- dashboardHeader(title = "Wikipedia Portal Traffic", disable = FALSE)

sidebar <- dashboardSidebar(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
    tags$script(src = "custom.js")
  ),
  sidebarMenu(menuItem(text = "Traffic"),
              menuSubItem(text = "Clickthrough rate", tabName = "clickthrough_rate"),
              menuSubItem(text = "Breakdown", tabName = "action_breakdown"),
              menuSubItem(text = "Dwell time", tabName = "dwell_data"),
              menuSubItem(text = "Geographic breakdown", tabName = "country_breakdown")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "clickthrough_rate",
            dygraphOutput("clickthrough_rate_dygraph"),
            includeMarkdown("./tab_documentation/clickthrough_rate.md")
    ),
    tabItem(tabName = "action_breakdown",
            div(dygraphOutput("action_breakdown_dygraph"),
                div(id = "action_breakdown_legend",
                    style = "height: 60px; padding-top: 30px; padding-left: 20px;")),
            includeMarkdown("./tab_documentation/breakdown.md")
    ),
    tabItem(tabName = "dwell_data",
            dygraphOutput("dwelltime_dygraph"),
            includeMarkdown("./tab_documentation/dwelltime.md")
    ),
    tabItem(tabName = "country_breakdown",
            div(dygraphOutput("country_breakdown_dygraph"),
                div(id = "country_breakdown_legend",
                    style = "height: 60px; padding-top: 30px; padding-left: 20px;"),
                style = "width: 100%; background-color: #222D32; color: #ECF0F5; padding-top: 10px;"),
            includeMarkdown("./tab_documentation/geography.md")
    )
  )
)

dashboardPage(header, sidebar, body, skin = "black",
              title = "Portal Traffic Dashboard | Discovery | Engineering | Wikimedia Foundation")
