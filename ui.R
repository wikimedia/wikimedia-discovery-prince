library(shiny)
library(shinydashboard)

source("extras.R")

function(request) {
  dashboardPage(

    dashboardHeader(title = "Wikipedia.org Portal", disable = FALSE),

    dashboardSidebar(
      shinyjs::useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
        tags$script(src = "custom.js")
      ),
      sidebarMenu(id = "tabs",
                  menuItem("Engagement",
                           menuSubItem(text = "Clickthrough rate", tabName = "clickthrough_rate"),
                           menuSubItem(text = "Last action", tabName = "action_breakdown"),
                           menuSubItem(text = "Most common section", tabName = "most_common"),
                           menuSubItem(text = "First visit", tabName = "first_visit"),
                           menuSubItem(text = "Dwell time", tabName = "dwell_data"),
                           menuSubItem(text = "Sister projects", tabName = "sister_projects"),
                           menuSubItem(text = "Mobile app links", tabName = "app_links"),
                           icon = icon("hand-o-up")),
                  menuItem("Traffic",
                           menuSubItem(text = "Geographic breakdown", tabName = "country_breakdown"),
                           menuSubItem(text = "Browser breakdown", tabName = "browser_breakdown"),
                           menuSubItem(text = "Pageviews", tabName = "pageviews"),
                           icon = icon("line-chart")),
                  menuItem("Languages",
                           menuSubItem(text = "Summary", tabName = "languages_summary"),
                           menuSubItem(text = "Languages visited", tabName = "languages_visited"),
                           icon = icon("globe", lib = "glyphicon")),
                  menuItem("External Referrals",
                           menuSubItem(text = "Overall Referral Traffic", tabName = "referrals_summary"),
                           menuSubItem(text = "Breakdown by Search Engine", tabName = "search_engines"),
                           icon = icon("external-link")),
                  menuItem("Geographical Breakdowns",
                           menuSubItem(text = "Traffic and Clickthrough Rate", tabName = "all_country"),
                           menuSubItem(text = "First Visit", tabName = "first_visits_by_country"),
                           menuSubItem(text = "Last Action", tabName = "last_action_by_country"),
                           menuSubItem(text = "Most Common Section", tabName = "most_common_by_country"),
                           icon = icon("globe", lib = "glyphicon")),
                  menuItem(text = "Global Settings",
                           selectizeInput(inputId = "smoothing_global", label = "Smoothing", selected = "day",
                                          choices = c("No Smoothing" = "day", "Weekly Median" = "week", "Monthly Median" = "month", "Splines" = "gam")),
                           br(style = "line-height:25%;"), icon = icon("cog", lib = "glyphicon"))
      ),
      div(icon("info-sign", lib = "glyphicon"), HTML("<strong>Tip</strong>: you can drag on the graphs with your mouse to zoom in on a particular date range."), style = "padding: 10px; color: black;"),
      div(bookmarkButton(), style = "text-align: center;")
    ),

    dashboardBody(
      tabItems(
        tabItem(tabName = "clickthrough_rate",
                fluidRow(
                  column(polloi::smooth_select("smoothing_clickthrough_rate"), width = 3),
                  column(div(id = "clickthrough_rate_legend", class = "dy-legend large"), width = 9)
                ),
                dygraphs::dygraphOutput("clickthrough_rate_dygraph"),
                includeMarkdown("./tab_documentation/clickthrough_rate.md")
        ),
        tabItem(tabName = "action_breakdown",
                fluidRow(
                  column(polloi::smooth_select("smoothing_action_breakdown"), width = 3),
                  column(div(id = "action_breakdown_legend", class = "dy-legend large"), width = 9)
                ),
                dygraphs::dygraphOutput("action_breakdown_dygraph"),
                includeMarkdown("./tab_documentation/action_breakdown.md")
        ),
        tabItem(tabName = "most_common",
                fluidRow(
                  column(polloi::smooth_select("smoothing_most_common"), width = 3),
                  column(div(id = "most_common_legend", class = "dy-legend large"), width = 9)
                ),
                dygraphs::dygraphOutput("most_common_dygraph"),
                includeMarkdown("./tab_documentation/most_common.md")
        ),
        tabItem(tabName = "first_visit",
                fluidRow(
                  column(polloi::smooth_select("smoothing_first_visit"), width = 3),
                  column(div(id = "first_visit_legend", class = "dy-legend large"), width = 9)
                ),
                dygraphs::dygraphOutput("first_visit_dygraph"),
                includeMarkdown("./tab_documentation/first_visit.md")
        ),
        tabItem(tabName = "dwell_data",
                fluidRow(
                  column(polloi::smooth_select("smoothing_dwelltime"), width = 3),
                  column(div(id = "dwelltime_legend", class = "dy-legend large"), width = 9)
                ),
                dygraphs::dygraphOutput("dwelltime_dygraph"),
                includeMarkdown("./tab_documentation/dwelltime.md")
        ),
        tabItem(tabName = "sister_projects",
                fluidRow(
                  column(polloi::smooth_select("smoothing_sisproj"), width = 3),
                  column(radioButtons("sisproj_metric", "Metric", list("Total clicks" = "Clicks", "Unique users who clicked" = "Users"), inline = TRUE), width = 3),
                  column(radioButtons("sisproj_type", "Type", list("Raw counts" = "counts", "Proportions" = "prop"), inline = TRUE), width = 3)
                ),
                dygraphs::dygraphOutput("sisproj_dygraph"),
                div(id = "sisproj_legend", class = "dy-legend small"),
                includeMarkdown("./tab_documentation/sisproj.md")
        ),
        tabItem(tabName = "app_links",
                fluidRow(
                  column(polloi::smooth_select("smoothing_applinks"), width = 3),
                  column(checkboxInput("applinks_platform_split", "Split by device", TRUE),
                         conditionalPanel("input.applinks_platform_split",
                                          checkboxGroupInput("applinks_platform", "Device",
                                                             list("Desktop", "Mobile"), selected = c("Desktop", "Mobile"), inline = TRUE)),
                         width = 3),
                  column(checkboxInput("applinks_destination_split", "Split by link clicked", TRUE),
                         conditionalPanel("input.applinks_destination_split",
                                          checkboxGroupInput("applinks_destination", "Links clicked",
                                                             list("iOS app" = "iOS app link", "Android app" = "Android app link", "List of apps" = "List of apps"),
                                                             selected = c("iOS app link", "Android app link", "List of apps"), inline = TRUE)),
                         width = 3),
                  column(radioButtons("applinks_type", "Type", list("Raw counts" = "counts", "Proportions" = "prop"), inline = TRUE), width = 3)
                ),
                dygraphs::dygraphOutput("applinks_dygraph"),
                div(id = "applinks_legend", class = "dy-legend small"),
                includeMarkdown("./tab_documentation/applinks.md")
        ),
        tabItem(tabName = "country_breakdown",
                fluidRow(column(polloi::smooth_select("smoothing_country_breakdown"), width = 4),
                         column(checkboxInput("group_us_regions", "Group U.S. regions", value = FALSE), width = 2),
                         column(checkboxInput("hide_less_than_5", "Hide countries with <5% traffic share", value = FALSE), width = 2),
                         column(checkboxInput("hide_more_than_15", "Hide countries with >15% traffic share", value = FALSE), width = 2)),
                div(dygraphs::dygraphOutput("country_breakdown_dygraph"),
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
                               dygraphs::dygraphOutput("browser_breakdown_dygraph"),
                               div(id = "browser_breakdown_legend", class = "large",
                                   style = "padding: 30px 0 10px 20px;"),
                               style = "width: 100%; background-color: #222D32; color: #ECF0F5; padding-top: 10px;"),
                           width = 9)),
                includeMarkdown("./tab_documentation/browsers.md")
        ),
        tabItem(tabName = "pageviews",
                fluidRow(
                  column(polloi::smooth_select("smoothing_pageviews"), width = 3),
                  column(HTML("<label style = \"margin-bottom: -10px;\">Scale</label>"),
                         checkboxInput("pageview_logscale", "Use Log scale", FALSE),
                         width = 4),
                  column(div(id = "pageview_legend", class = "dy-legend large"), width = 5)
                ),
                dygraphs::dygraphOutput("pageview_dygraph"),
                includeMarkdown("./tab_documentation/pageviews.md")
        ),
        tabItem(tabName = "referrals_summary",
                fluidRow(column(polloi::smooth_select("smoothing_referer_summary"), width = 3),
                         column(div(id = "referer_summary_legend", class = "dy-legend"), width = 9)),
                dygraphs::dygraphOutput("referer_summary_dygraph"),
                includeMarkdown("./tab_documentation/referers_summary.md")
        ),
        tabItem(tabName = "search_engines",
                fluidRow(column(polloi::smooth_select("smoothing_search_engines"), width = 3),
                         column(div(id = "search_engines_legend", class = "dy-legend large"), width = 9)),
                dygraphs::dygraphOutput("search_engines_dygraph"),
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
          dygraphs::dygraphOutput("s_dygraph"),
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
                  style = "width: 12.5%; margin-left: 2.5%; padding-top: 30px; height: 34px; display: inline-block; float: left;"),
              width = 3),
            column(
              conditionalPanel("input.lv_response === 'clicks' && input.lv_languages.length > 1 && input.lv_sort != 'top10'", HTML("<label class = \"control-label\" style = \"margin-bottom:-30px;\">Aggregation</label>"), checkboxInput("lv_combine", "Combine languages", FALSE)),
              conditionalPanel("input.lv_response === 'clicks' && input.lv_languages.length < 2", radioButtons("lv_type", "Type", list("Counts" = "count", "Proportions" = "prop"), inline = TRUE)),
              conditionalPanel("input.lv_type === 'count'", HTML("<label class = \"control-label\" style = \"margin-bottom:-30px;\">Scale</label>"), checkboxInput("lv_logscale", "Use Log scale", FALSE)),
              width = 2)
          ),
          dygraphs::dygraphOutput("lv_dygraph"),
          div(id = "lv_legend", class = "large", style = "text-align: center; padding: 15px 10px 0 10px;"),
          includeMarkdown("tab_documentation/languages_visited.md")
        ),
        tabItem(
          tabName = "all_country",
          fluidRow(
            column(width = 3,
                   dateRangeInput("date_all_country", "Date Range",
                                  start = Sys.Date() - 60,
                                  end = Sys.Date() - 1,
                                  startview = "month",
                                  separator = " to ")),
            column(width = 4,
                   selectInput("traffic_select", label = "Metrics",
                               choices = list("Number of Events" = 'events',
                                              "Number of Visits" = 'visits', "Number of Sessions" = 'sessions',
                                              "Overall Clickthrough Rate"='ctr_all',
                                              "Clickthrough Rate Per Visit"='ctr_vst',
                                              "Clickthrough Rate Per Session"='ctr_ses'),
                               selected = 'events')),
            column(width = 2,
                   selectInput("cntr_sort_a", "Group of Countries",
                               list("Top 10" = "top10_a",
                                    "Bottom 50" = "bottom50_a",
                                    "United States" = "us_a",
                                    "All" = "all_a",
                                    "All but US" = "all_nus_a",
                                    "Custom" = "custom_a"),
                               "all_a")),
            column(width = 3,
                   polloi::smooth_select("smoothing_cntr_a"))
          ),
          fluidRow(
            column(width = 3,
                   checkboxInput("prop_a", "Use Proportion in All Countries", FALSE)),
            column(width = 2,
                   conditionalPanel("!(input.cntr_a.length<=1 & input.cntr_sort_a == 'custom_a')", checkboxInput("cntr_combine_a", "Combine Countries", FALSE))),
            column(width = 2,
                   conditionalPanel("(input.traffic_select=='events' || input.traffic_select=='visits' || input.traffic_select=='sessions') && !input.prop_a", checkboxInput("cntr_logscale_a", "Use Log scale", FALSE))),
            column(width = 5,
                   conditionalPanel("input.cntr_sort_a == 'custom_a'",
                                    selectizeInput("cntr_a", "Countries", choices = sort(all_country_names), selected = c("United Kingdom", "Germany", "India", "Canada", "U.S. (South)"), multiple = TRUE, options = list(plugins = list("remove_button")), width = "100%")))
          ),
          fluidRow(
            highcharter::highchartOutput("traffic_pie_pl", height = "500px"),
            br(),
            dygraphs::dygraphOutput("all_country_dygraph"),
            div(id = "cntr_a_legend", class = "large", style = "text-align: center; padding: 15px 10px 0 10px;"),
            DT::dataTableOutput("all_country_tbl"),
            p(class = 'text-center', downloadButton('all_country_dl', 'Download Filtered Data'))
          ),
          includeMarkdown("./tab_documentation/traffic_ctr_geo.md")
        ),
        tabItem(
          tabName = "first_visits_by_country",
          fluidRow(
            column(width = 3,
                   dateRangeInput("date_first_visit", "Date Range",
                                  start = Sys.Date() - 60,
                                  end = Sys.Date() - 1,
                                  startview = "month",
                                  separator = " to ")),
            column(width = 4,
                   selectInput("action_select_f", label = "Actions",
                               choices = list("No Action" = 'nact_f',
                                              "Other Languages" = 'olv_f', "Other Projects" = 'oproj_f',
                                              "Primary Links"='prln_f',
                                              "Search"='search_f',
                                              "Secondary Links"='secln_f'),
                               selected = 'search_f')),
            column(width = 2,
                   selectInput("cntr_sort_f", "Group of Countries",
                               list("Top 10" = "top10_f",
                                    "Bottom 50" = "bottom50_f",
                                    "United States" = "us_f",
                                    "All" = "all_f",
                                    "All but US" = "all_nus_f",
                                    "Custom" = "custom_f"),
                               "all_f")),
            column(width = 3,
                   polloi::smooth_select("smoothing_cntr_f"))
          ),
          fluidRow(
            column(width = 3,
                   checkboxInput("prop_f", "Use Proportion in All Countries", FALSE)),
            column(width = 2,
                   conditionalPanel("!(input.cntr_f.length<=1 & input.cntr_sort_f == 'custom_f')", checkboxInput("cntr_combine_f", "Combine Countries", FALSE))),
            column(width = 2,
                   conditionalPanel("!input.prop_f", checkboxInput("cntr_logscale_f", "Use Log scale", FALSE))),
            column(width = 5,
                   conditionalPanel("input.cntr_sort_f == 'custom_f'",
                                    selectizeInput("cntr_f", "Countries", choices = sort(all_country_names), selected = c("United Kingdom", "Germany", "India", "Canada", "U.S. (South)"), multiple = TRUE, options = list(plugins = list("remove_button")), width = "100%")))
          ),
          fluidRow(
            highcharter::highchartOutput("first_visit_pie_pl", height = "500px"),
            br(),
            dygraphs::dygraphOutput("first_visit_country_dygraph"),
            div(id = "cntr_f_legend", class = "large", style = "text-align: center; padding: 15px 10px 0 10px;"),
            DT::dataTableOutput("first_visits_by_country_tbl"),
            p(class = 'text-center', downloadButton('first_visits_dl', 'Download Filtered Data'))
          ),
          includeMarkdown("./tab_documentation/first_visit_geo.md")
        ),
        tabItem(
          tabName = "last_action_by_country",
          fluidRow(
            column(width = 3,
                   dateRangeInput("date_last_action", "Date Range",
                                  start = Sys.Date() - 60,
                                  end = Sys.Date() - 1,
                                  startview = "month",
                                  separator = " to ")),
            column(width = 4,
                   selectInput("action_select_l", label = "Actions",
                               choices = list("No Action" = 'nact_l',
                                              "Other Languages" = 'olv_l', "Other Projects" = 'oproj_l',
                                              "Primary Links"='prln_l',
                                              "Search"='search_l',
                                              "Secondary Links"='secln_l'),
                               selected = 'search_l')),
            column(width = 2,
                   selectInput("cntr_sort_l", "Group of Countries",
                               list("Top 10" = "top10_l",
                                    "Bottom 50" = "bottom50_l",
                                    "United States" = "us_l",
                                    "All" = "all_l",
                                    "All but US" = "all_nus_l",
                                    "Custom" = "custom_l"),
                               "all_l")),
            column(width = 3, polloi::smooth_select("smoothing_cntr_l"))
          ),
          fluidRow(
            column(width = 3,
                   checkboxInput("prop_l", "Use Proportion in All Countries", FALSE)),
            column(width = 2,
                   conditionalPanel("!(input.cntr_l.length<=1 & input.cntr_sort_l == 'custom_l')", checkboxInput("cntr_combine_l", "Combine Countries", FALSE))),
            column(width = 2,
                   conditionalPanel("!input.prop_l", checkboxInput("cntr_logscale_l", "Use Log scale", FALSE))),
            column(width = 5,
                   conditionalPanel("input.cntr_sort_l == 'custom_l'",
                                    selectizeInput("cntr_l", "Countries", choices = sort(all_country_names), selected = c("United Kingdom", "Germany", "India", "Canada", "U.S. (South)"), multiple = TRUE, options = list(plugins = list("remove_button")), width = "100%")))
          ),
          fluidRow(
            highcharter::highchartOutput("last_action_pie_pl", height = "500px"),
            br(),
            dygraphs::dygraphOutput("last_action_dygraph"),
            div(id = "cntr_l_legend", class = "large", style = "text-align: center; padding: 15px 10px 0 10px;"),
            DT::dataTableOutput("last_action_by_country_tbl"),
            p(class = 'text-center', downloadButton('last_action_dl', 'Download Filtered Data'))
          ),
          includeMarkdown("./tab_documentation/last_action_geo.md")
        ),
        tabItem(
          tabName = "most_common_by_country",
          fluidRow(
            column(width = 3,
                   dateRangeInput("date_most_common", "Date Range",
                                  start = Sys.Date() - 60,
                                  end = Sys.Date() - 1,
                                  startview = "month",
                                  separator = " to ")),
            column(width = 4,
                   selectInput("action_select_m", label = "Actions",
                               choices = list("Other Languages" = 'olv_m',
                                              "Other Projects" = 'oproj_m',
                                              "Primary Links"='prln_m',
                                              "Search"='search_m',
                                              "Secondary Links"='secln_m'),
                               selected = 'search_m')),
            column(width = 2,
                   selectInput("cntr_sort_m", "Group of Countries",
                               list("Top 10" = "top10_m",
                                    "Bottom 50" = "bottom50_m",
                                    "United States" = "us_m",
                                    "All" = "all_m",
                                    "All but US" = "all_nus_m",
                                    "Custom" = "custom_m"),
                               "all_m")),
            column(width = 3, polloi::smooth_select("smoothing_cntr_m"))
          ),
          fluidRow(
            column(width = 3,
                   checkboxInput("prop_m", "Use Proportion in All Countries", FALSE)),
            column(width = 2,
                   conditionalPanel("!(input.cntr_m.length <= 1 & input.cntr_sort_m == 'custom_m')", checkboxInput("cntr_combine_m", "Combine Countries", FALSE))),
            column(width = 2,
                   conditionalPanel("!input.prop_m", checkboxInput("cntr_logscale_m", "Use Log scale", FALSE))),
            column(width = 5,
                   conditionalPanel("input.cntr_sort_m == 'custom_m'",
                                    selectizeInput("cntr_m", "Countries", choices = sort(all_country_names), selected = c("United Kingdom", "Germany", "India", "Canada", "U.S. (South)"), multiple = TRUE, options = list(plugins = list("remove_button")), width = "100%")))
          ),
          fluidRow(
            highcharter::highchartOutput("most_common_pie_pl", height = "500px"),
            br(),
            dygraphs::dygraphOutput("most_common_country_dygraph"),
            div(id = "cntr_m_legend", class = "large", style = "text-align: center; padding: 15px 10px 0 10px;"),
            DT::dataTableOutput("most_common_by_country_tbl"),
            p(class = 'text-center', downloadButton('most_common_dl', 'Download Filtered Data'))
          ),
          includeMarkdown("./tab_documentation/most_common_geo.md")
        )
      )
    ),

    skin = "black", title = "Portal Traffic Dashboard | Discovery | Audiences | Wikimedia Foundation")
}
