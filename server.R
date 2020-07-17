library(shiny)
library(shinydashboard)
library(dygraphs)
library(shinyjs)
library(highcharter)

source("utils.R")

existing_date <- Sys.Date() - 1

shinyServer(function(input, output, session) {

  if (Sys.Date() != existing_date) {
    progress <- shiny::Progress$new(session, min = 0, max = 1)
    on.exit(progress$close())
    progress$set(message = "Downloading clickthrough data...", value = 0)
    read_clickthrough()
    progress$set(message = "Downloading language visit data...", value = 1/10)
    read_langs()
    progress$set(message = "Downloading dwell-time data...", value = 2/10)
    read_dwelltime()
    progress$set(message = "Downloading country data data...", value = 3/10)
    read_country()
    progress$set(message = "Downloading user-agent data...", value = 4/10)
    read_useragents()
    progress$set(message = "Downloading pageview data...", value = 5/10)
    read_pageviews()
    progress$set(message = "Downloading referral data...", value = 6/10)
    read_referrals()
    progress$set(message = "Downloading sister project click data...", value = 7/10)
    read_sisproj()
    progress$set(message = "Downloading app link click data...", value = 8/10)
    read_applinks()
    progress$set(message = "Downloading geographical breakdown data...", value = 9/10)
    read_geo()
    progress$set(message = "Finished downloading datasets.", value = 1)
    existing_date <<- Sys.Date()
  }

  # Engagement
  source("modules/engagement/clickthrough_rate.R", local = TRUE) # Clickthrough rate
  source("modules/engagement/action_breakdown.R", local = TRUE) # Last action
  source("modules/engagement/most_common.R", local = TRUE) # Most common section
  source("modules/engagement/first_visit.R", local = TRUE) # First visit
  source("modules/engagement/dwelltime.R", local = TRUE) # Dwell time
  source("modules/engagement/sister_projects.R", local = TRUE) # Sister projects
  source("modules/engagement/app_links.R", local = TRUE) # Mobile app links
  # Traffic
  source("modules/traffic/country_breakdown.R", local = TRUE) # Geographic breakdown
  source("modules/traffic/browser_breakdown.R", local = TRUE) # Browser breakdown
  source("modules/traffic/pageviews.R", local = TRUE) # Pageviews
  # External Referrers: Overall Referral Traffic + Breakdown by Search Engine
  source("modules/external_referrers.R", local = TRUE)
  # Languages
  source("modules/languages/summary.R", local = TRUE) # Summary
  source("modules/languages/languages_visited.R", local = TRUE) # Languages visited
  # General Geo Breakdowns
  source("modules/geographical_breakdowns/traffic_clickthroughs.R", local = TRUE) # Traffic and Clickthrough Rate
  source("modules/geographical_breakdowns/first_visit.R", local = TRUE) # First Visit
  source("modules/geographical_breakdowns/last_action.R", local = TRUE) # Last Action
  source("modules/geographical_breakdowns/most_common.R", local = TRUE) # Most Common Section

})
