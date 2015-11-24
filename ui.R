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
              menuSubItem(text = "Breakdown", tabName = "action_breakdown")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "clickthrough_rate",
            dygraphOutput("clickthrough_rate_dygraph"),
            includeMarkdown("./tab_documentation/clickthrough_rate.md")
    ),
    tabItem(tabName = "action_breakdown",
            dygraphOutput("action_breakdown_dygraph"),
            includeMarkdown("./tab_documentation/breakdown.md")
            
    )
  )
)

dashboardPage(header, sidebar, body, skin = "black",
              title = "Portal Traffic Dashboard | Discovery | Engineering | Wikimedia Foundation")
