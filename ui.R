library(shiny)
library(shinydashboard)
library(dygraphs)
library(shinyURL)
options(scipen = 500)

#Header elements for the visualisation
header <- dashboardHeader(title = "Wikipedia Portal Traffic", disable = FALSE)

sidebar <- dashboardSidebar(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
    tags$script(src = "custom.js")
  ),
  sidebarMenu(menuItem("Traffic",
                       menuSubItem(text = "Clickthrough rate", tabName = "clickthrough_rate"),
                       menuSubItem(text = "Action breakdown", tabName = "action_breakdown"),
                       menuSubItem(text = "Dwell time", tabName = "dwell_data"),
                       menuSubItem(text = "Geographic breakdown", tabName = "country_breakdown"),
                       menuSubItem(text = "Browser breakdown", tabName = "browser_breakdown"),
                       menuSubItem(text = "Pageviews", tabName = "pageview_tab"),
                       icon = icon("line-chart")),
              menuItem(text = "Global Settings",
                       selectInput(inputId = "smoothing_global", label = "Smoothing", selectize = TRUE, selected = "day",
                                   choices = c("No Smoothing" = "day", "Weekly Median" = "week", "Monthly Median" = "month")),
                       br(style = "line-height:25%;"), icon = icon("cog", lib = "glyphicon")),
              menuItem(text = "Sharing Options", shinyURL.ui(tinyURL = FALSE),
                       p("Dashboard settings stored in URL.", style = "padding-bottom: 10px;"),
                       icon = icon("share-alt", lib = "glyphicon"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "clickthrough_rate",
            polloi::smooth_select("smoothing_clickthrough_rate"),
            dygraphOutput("clickthrough_rate_dygraph"),
            includeMarkdown("./tab_documentation/clickthrough_rate.md")
    ),
    tabItem(tabName = "action_breakdown",
            polloi::smooth_select("smoothing_action_breakdown"),
            div(dygraphOutput("action_breakdown_dygraph"),
                div(id = "action_breakdown_legend",
                    style = "height: 60px; padding-top: 30px; padding-left: 20px;")),
            includeMarkdown("./tab_documentation/breakdown.md")
    ),
    tabItem(tabName = "dwell_data",
            polloi::smooth_select("smoothing_dwelltime"),
            dygraphOutput("dwelltime_dygraph"),
            includeMarkdown("./tab_documentation/dwelltime.md")
    ),
    tabItem(tabName = "country_breakdown",
            polloi::smooth_select("smoothing_country_breakdown"),
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
                     column(div(polloi::smooth_select("smoothing_browser_breakdown"),
                                dygraphOutput("browser_breakdown_dygraph"),
                                div(id = "browser_breakdown_legend",
                                    style = "height: 60px; padding-top: 30px; padding-left: 20px;"),
                                style = "width: 100%; background-color: #222D32; color: #ECF0F5; padding-top: 10px;"),
                            width = 9)),
            includeMarkdown("./tab_documentation/browsers.md")
    ),
    tabItem(tabName = "pageview_tab",
            polloi::smooth_select("smoothing_pageviews"),
            dygraphOutput("pageview_dygraph"),
            includeMarkdown("./tab_documentation/pageviews.md")
    )
  )
)

dashboardPage(header, sidebar, body, skin = "black",
              title = "Portal Traffic Dashboard | Discovery | Engineering | Wikimedia Foundation")
