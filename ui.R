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
  sidebarMenu(menuItem(text = "Clickthrough rate", tabName = "clickthrough_rate"),
              menuItem(text = "Action breakdown", tabName = "action_breakdown"),
              menuItem(text = "Dwell time", tabName = "dwell_data"),
              menuItem(text = "Geographic breakdown", tabName = "country_breakdown"),
              menuItem(text = "Browser breakdown", tabName = "browser_breakdown", badgeColor = "light-blue", badgeLabel = "New!"),
              menuItem(text = "Pageviews", tabName = "pageview_tab", badgeColor = "light-blue", badgeLabel = "New!")
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
    ),
    tabItem(tabName = "browser_breakdown",
            fluidRow(column(selectInput("browser_order", "Sort", selected = "growth",
                                        choices = list("Alphabetically" = "alphabet",
                                                       "By popularity growth rate" = "growth",
                                                       "By popularity decay rate" = "decay",
                                                       "Last recorded percentage" = "last",
                                                       "Number of times it appears in data" = "times")),
                            textInput("browser_filter", "Filter", placeholder = "IE, firefox"),
                            helpText("Case insensitive & accepts comma-separated input."),
                            uiOutput("browser_selector_container"),
                            width = 3),
                     column(div(dygraphOutput("browser_breakdown_dygraph"),
                                div(id = "browser_breakdown_legend",
                                    style = "height: 60px; padding-top: 30px; padding-left: 20px;"),
                                style = "width: 100%; background-color: #222D32; color: #ECF0F5; padding-top: 10px;"),
                            width = 9)),
            includeMarkdown("./tab_documentation/browsers.md")
    ),
    tabItem(tabName = "pageview_tab",
            dygraphOutput("pageview_dygraph"),
            includeMarkdown("./tab_documentation/pageviews.md")
    )
  )
)

dashboardPage(header, sidebar, body, skin = "black",
              title = "Portal Traffic Dashboard | Discovery | Engineering | Wikimedia Foundation")
