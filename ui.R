library(shiny)
library(shinydashboard)
library(dygraphs)
library(shinyURL)
options(scipen = 500)

#Header elements for the visualisation
header <- dashboardHeader(title = "Wikipedia.org Portal", disable = FALSE)

sidebar <- dashboardSidebar(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
    tags$script(src = "custom.js")
  ),
  sidebarMenu(menuItem("Engagement",
                       menuSubItem(text = "Clickthrough rate", tabName = "clickthrough_rate"),
                       menuSubItem(text = "Last action", tabName = "action_breakdown"),
                       menuSubItem(text = "Most common section", tabName = "most_common"),
                       menuSubItem(text = "First visit", tabName = "first_visit"),
                       menuSubItem(text = "Dwell time", tabName = "dwell_data"),
                       icon = icon("hand-o-up")),
              menuItem("Traffic",
                       menuSubItem(text = "Geographic breakdown", tabName = "country_breakdown"),
                       menuSubItem(text = "Browser breakdown", tabName = "browser_breakdown"),
                       menuSubItem(text = "Pageviews", tabName = "pageview_tab"),
                       icon = icon("line-chart")),
              menuItem("Languages",
                       menuSubItem(text = "Summary", tabName = "languages_summary"),
                       menuSubItem(text = "Languages visited", tabName = "languages_visited"),
                       icon = icon("globe", lib = "glyphicon")),
              menuItem("External Referrals",
                       menuSubItem(text = "Overall Referral Traffic", tabName = "referrals_summary"),
                       menuSubItem(text = "Breakdown by Search Engine", tabName = "search_engines"),
                       icon = icon("external-link")),
              menuItem(text = "Global Settings",
                       selectInput(inputId = "smoothing_global", label = "Smoothing", selectize = TRUE, selected = "day",
                                   choices = c("No Smoothing" = "day", "Weekly Median" = "week", "Monthly Median" = "month", "Splines" = "gam")),
                       br(style = "line-height:25%;"), icon = icon("cog", lib = "glyphicon")),
              menuItem(text = "Sharing Options", shinyURL.ui(tinyURL = FALSE),
                       p("Dashboard settings stored in URL.", style = "padding-bottom: 10px;"),
                       icon = icon("share-alt", lib = "glyphicon"))
  ),
  div(icon("info-sign", lib = "glyphicon"), HTML("<strong>Tip</strong>: you can drag on the graphs with your mouse to zoom in on a particular date range."), style = "padding: 10px; color: black;")
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "clickthrough_rate",
            fluidRow(
              column(polloi::smooth_select("smoothing_clickthrough_rate"), width = 3),
              column(div(id = "clickthrough_rate_legend", class = "dy-legend large"), width = 9)
            ),
            dygraphOutput("clickthrough_rate_dygraph"),
            includeMarkdown("./tab_documentation/clickthrough_rate.md")
    ),
    tabItem(tabName = "action_breakdown",
            fluidRow(
              column(polloi::smooth_select("smoothing_action_breakdown"), width = 3),
              column(div(id = "action_breakdown_legend", class = "dy-legend large"), width = 9)
            ),
            dygraphOutput("action_breakdown_dygraph"),
            includeMarkdown("./tab_documentation/action_breakdown.md")
    ),
    tabItem(tabName = "most_common",
            fluidRow(
              column(polloi::smooth_select("smoothing_most_common"), width = 3),
              column(div(id = "most_common_legend", class = "dy-legend large"), width = 9)
            ),
            dygraphOutput("most_common_dygraph"),
            includeMarkdown("./tab_documentation/most_common.md")
    ),
    tabItem(tabName = "first_visit",
            fluidRow(
              column(polloi::smooth_select("smoothing_first_visit"), width = 3),
              column(div(id = "first_visit_legend", class = "dy-legend large"), width = 9)
            ),
            dygraphOutput("first_visit_dygraph"),
            includeMarkdown("./tab_documentation/first_visit.md")
    ),
    tabItem(tabName = "dwell_data",
            fluidRow(
              column(polloi::smooth_select("smoothing_dwelltime"), width = 3),
              column(div(id = "dwelltime_legend", class = "dy-legend large"), width = 9)
            ),
            dygraphOutput("dwelltime_dygraph"),
            includeMarkdown("./tab_documentation/dwelltime.md")
    ),
    tabItem(tabName = "country_breakdown",
            fluidRow(column(polloi::smooth_select("smoothing_country_breakdown"), width = 4),
                     column(checkboxInput("group_us_regions", "Group U.S. regions", value = FALSE), width = 2),
                     column(checkboxInput("hide_less_than_5", "Hide countries with <5% traffic share", value = FALSE), width = 2),
                     column(checkboxInput("hide_more_than_15", "Hide countries with >15% traffic share", value = FALSE), width = 2)),
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
                            checkboxInput("group_browsers", "Group browser versions", FALSE),
                            conditionalPanel("input.group_browsers",
                                             radioButtons("browser_grouping", "Group by", inline = TRUE,
                                                          choices = list("Browser Family" = "family",
                                                                         "MediaWiki Support" = "support"))),
                            uiOutput("browser_selector_container"),
                            width = 3),
                     column(
                       div(polloi::smooth_select("smoothing_browser_breakdown"),
                                dygraphOutput("browser_breakdown_dygraph"),
                                div(id = "browser_breakdown_legend", class = "large",
                                    style = "padding: 30px 0 10px 20px;"),
                                style = "width: 100%; background-color: #222D32; color: #ECF0F5; padding-top: 10px;"),
                            width = 9)),
            includeMarkdown("./tab_documentation/browsers.md")
    ),
    tabItem(tabName = "pageview_tab",
            fluidRow(
              column(polloi::smooth_select("smoothing_pageviews"), width = 3),
              column(HTML("<label style = \"margin-bottom: -10px;\">Scale</label>"),
                     checkboxInput("pageview_logscale", "Use Log scale", FALSE),
                     width = 4),
              column(div(id = "pageview_legend", class = "dy-legend large"), width = 5)
            ),
            dygraphOutput("pageview_dygraph"),
            includeMarkdown("./tab_documentation/pageviews.md")
    ),
    tabItem(tabName = "referrals_summary",
            fluidRow(column(polloi::smooth_select("smoothing_referer_summary"), width = 3),
                     column(div(id = "referer_summary_legend", class = "dy-legend"), width = 9)),
            dygraphOutput("referer_summary_dygraph"),
            includeMarkdown("./tab_documentation/referers_summary.md")
    ),
    tabItem(tabName = "search_engines",
            fluidRow(column(polloi::smooth_select("smoothing_search_engines"), width = 3),
                     column(div(id = "search_engines_legend", class = "dy-legend large"), width = 9)),
            dygraphOutput("search_engines_dygraph"),
            includeMarkdown("./tab_documentation/referers_byengine.md")
    ),
    tabItem(
      tabName = "languages_summary",
      fluidRow(
        column(polloi::smooth_select("smoothing_summary"), width = 3),
        column(radioButtons("s_response", "Data", list("Clicks" = "clicks", "Users" = "users"), inline = TRUE), width = 2),
        column(HTML("<label class = \"control-label\" style = \"margin-bottom:-30px;\">Filter</label>"),
               checkboxInput("s_enwiki", "Include English Wikipedia", TRUE), width = 3),
        column(conditionalPanel("input.s_response === 'clicks'", radioButtons("s_type", "Type", list("Counts" = "count", "Proportions" = "prop"), inline = TRUE)), width = 2),
        column(conditionalPanel("input.s_response === 'clicks' & input.s_type === 'count'",
                                HTML("<label class = \"control-label\" style = \"margin-bottom:-30px;\">Scale</label>"),
                                checkboxInput("s_logscale", "Use Log scale", FALSE)), width = 2)
      ),
      dygraphOutput("s_dygraph"),
      div(id = "s_legend", class = "large", style = "text-align: center; padding: 0 10px;"),
      includeMarkdown("tab_documentation/languages_summary.md")
    ),
    tabItem(
      tabName = "languages_visited",
      fluidRow(
        column(polloi::smooth_select("smoothing_lv"), width = 2),
        column(radioButtons("lv_response", "Data", list("Clicks" = "clicks", "Users" = "users"), inline = TRUE), width = 2),
        column(selectInput("lv_sort", "Sort languages",
                           list("Top 10" = "top10",
                                "Bottom 50" = "bottom50",
                                "Overall Clicks (High to Low)" = "clicks_high2low",
                                "Overall Clicks (Low to High)" = "clicks_low2high",
                                "Daily Clicks (High to Low)" = "avg_clicks_high2low",
                                "Daily Clicks (Low to High)" = "avg_clicks_low2high",
                                "Alphabetically (A-Z)" = "alphabet_az",
                                "Alphabetically (Z-A)" = "alphabet_za"),
                           "top10"),
               width = 3),
        column(
          uiOutput("lv_languages_container", style = "width: 80%; display: inline-block; float: left;",
                   title = "Type to find a language. Use backspace key to remove a selected language. Up to 12 languages can be selected at the same time."),
                   div(icon("question-circle", class = "fa-lg"),
                       title = "Type to find a language. Use backspace key to remove a selected language.",
                       style="width: 12.5%; margin-left: 2.5%; padding-top: 30px; height: 34px; display: inline-block; float: left;"),
          width = 3),
        column(
          conditionalPanel("input.lv_response === 'clicks' & input.lv_languages.length > 1 & input.lv_sort != 'top10'", HTML("<label class = \"control-label\" style = \"margin-bottom:-30px;\">Aggregation</label>"), checkboxInput("lv_combine", "Combine languages", FALSE)),
          conditionalPanel("input.lv_response === 'clicks' & input.lv_languages.length < 2", radioButtons("lv_type", "Type", list("Counts" = "count", "Proportions" = "prop"), inline = TRUE)),
          conditionalPanel("input.lv_type === 'count'", HTML("<label class = \"control-label\" style = \"margin-bottom:-30px;\">Scale</label>"), checkboxInput("lv_logscale", "Use Log scale", FALSE)),
          width = 2)
      ),
      dygraphOutput("lv_dygraph"),
      div(id = "lv_legend", class = "large", style = "text-align: center; padding: 15px 10px 0 10px;"),
      includeMarkdown("tab_documentation/languages_visited.md")
    )
  )
)

dashboardPage(header, sidebar, body, skin = "black",
              title = "Portal Traffic Dashboard | Discovery | Engineering | Wikimedia Foundation")
