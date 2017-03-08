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

  output$clickthrough_rate_dygraph <- renderDygraph({
    clickthrough_rate %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_clickthrough_rate)) %>%
      polloi::make_dygraph(xlab = "Date", ylab = "Clickthrough rate (%)",
                           title = "Wikipedia.org Portal Clickthrough Rate") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyLegend(labelsDiv = "clickthrough_rate_legend", show = "always") %>%
      dyRangeSelector(strokeColor = "white", fillColor = "gray", retainDateWindow = TRUE) %>%
      dyEvent(as.Date("2015-12-07"), "A (sampling change)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-03-10"), "Search Box Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-18"), "Sister Links Updated", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-08-16"), "Secondary Links Collapsed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-09-13"), "B (schema switch)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })

  output$action_breakdown_dygraph <- renderDygraph({
    action_breakdown %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_action_breakdown)) %>%
      polloi::make_dygraph(xlab = "Date", ylab = "Actions (%)",
                           title = "Actions on the Wikipedia Portal") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyLegend(labelsDiv = "action_breakdown_legend", show = "always") %>%
      dyRangeSelector(fillColor = "", strokeColor = "", retainDateWindow = TRUE) %>%
      dyEvent(as.Date("2015-12-07"), "A (sampling change)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-03-10"), "Search Box Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-18"), "Sister Links Updated", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-08-16"), "Secondary Links Collapsed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-09-13"), "B (schema switch)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })

  output$most_common_dygraph <- renderDygraph({
    most_common %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_most_common)) %>%
      polloi::make_dygraph(xlab = "Date", ylab = "Visits (%)",
                           title = "Most Common Section Per Visit") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyLegend(labelsDiv = "most_common_legend", show = "always") %>%
      dyRangeSelector(fillColor = "", strokeColor = "", retainDateWindow = TRUE) %>%
      dyEvent(as.Date("2016-05-18"), "Sister Links Updated", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-08-16"), "Secondary Links Collapsed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-09-13"), "A (schema switch)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })

  output$first_visit_dygraph <- renderDygraph({
    first_visit_ctrs %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_first_visit)) %>%
      polloi::make_dygraph(xlab = "Date", ylab = "Actions (%)",
                           title = "Actions on the first visit to Wikipedia Portal") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyLegend(labelsDiv = "first_visit_legend", show = "always") %>%
      dyRangeSelector(fillColor = "", strokeColor = "", retainDateWindow = TRUE) %>%
      dyEvent(as.Date("2016-03-10"), "Search Box Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-18"), "Sister Links Updated", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-08-16"), "Secondary Links Collapsed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-09-13"), "A (schema switch)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })

  output$dwelltime_dygraph <- renderDygraph({
    dwelltime_data %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_dwelltime)) %>%
      polloi::make_dygraph(xlab = "Date", ylab = "Dwell Time (Seconds)", title = "Time spent on the Wikipedia portal") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyLegend(labelsDiv = "dwelltime_legend", show = "always") %>%
      dyRangeSelector(fillColor = "", strokeColor = "", retainDateWindow = TRUE) %>%
      dyEvent(as.Date("2015-12-07"), "A (sampling change)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-03-10"), "Search Box Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-18"), "Sister Links Updated", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-08-16"), "Secondary Links Collapsed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-09-13"), "B (schema switch)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })

  output$sisproj_dygraph <- renderDygraph({
    temp <- dplyr::select_(sisproj_clicks, .dots = list(quote(date), quote(destination), value = tolower(input$sisproj_metric)))
    if (input$sisproj_type == "prop") {
      temp <- temp %>%
        dplyr::group_by(date) %>%
        dplyr::mutate(value = 100*round(value/sum(value), 4)) %>%
        dplyr::ungroup()
    }
    latest_values <- temp %>%
      tidyr::spread(destination, value, fill = 0) %>%
      polloi::safe_tail(1) %>%
      dplyr::select(-date) %>%
      unlist(use.names = TRUE)
    dy <- temp %>%
      tidyr::spread(destination, value, fill = 0) %>%
      { .[, c("date", names(latest_values)[order(latest_values, decreasing = TRUE)])] } %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_sisproj), rename = FALSE) %>%
      polloi::make_dygraph("Date", ifelse(input$sisproj_type == "prop", "Proportion (%)", input$sisproj_metric),
                           paste(ifelse(input$sisproj_metric == "Clicks", "Clicks", "Users who clicked"), "on links other Wikimedia Foundation projects")) %>%
      dyLegend(labelsDiv = "sisproj_legend", show = "always", width = 600) %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
    return(dy)
  })

  output$applinks_dygraph <- renderDygraph({
    if (input$applinks_destination_split && input$applinks_platform_split) {
      if (length(input$applinks_destination) == 0 || length(input$applinks_platform) == 0) {
        return(NULL)
      }
      temp <- applink_clicks %>%
        dplyr::filter(clicked %in% input$applinks_destination, device %in% input$applinks_platform) %>%
        dplyr::mutate(group = paste("Clicked on the ", clicked, "on a", dplyr::if_else(device == "Mobile", "mobile device", "desktop"))) %>%
        dplyr::group_by(date, group) %>%
        dplyr::summarize(clicks = sum(clicks))
    } else if (input$applinks_destination_split && !input$applinks_platform_split) {
      if (length(input$applinks_destination) == 0) {
        return(NULL)
      }
      temp <- applink_clicks %>%
        dplyr::filter(clicked %in% input$applinks_destination) %>%
        dplyr::mutate(group = paste("Clicked on the ", clicked)) %>%
        dplyr::group_by(date, group) %>%
        dplyr::summarize(clicks = sum(clicks))
    } else if (!input$applinks_destination_split && input$applinks_platform_split) {
      if (length(input$applinks_platform) == 0) {
        return(NULL)
      }
      temp <- applink_clicks %>%
        dplyr::filter(device %in% input$applinks_platform) %>%
        dplyr::mutate(group = paste("Clicked on the links from a", dplyr::if_else(device == "Mobile", "mobile device", "desktop"))) %>%
        dplyr::group_by(date, group) %>%
        dplyr::summarize(clicks = sum(clicks))
    } else {
      temp <- applink_clicks %>%
        dplyr::group_by(date) %>%
        dplyr::summarize(group = "Clicks", clicks = sum(clicks))
    }
    if (input$applinks_type == "prop") {
      temp <- temp %>%
        dplyr::group_by(date) %>%
        dplyr::mutate(total = sum(clicks)) %>%
        dplyr::group_by(date, group) %>%
        dplyr::mutate(clicks = 100*round(clicks/total, 4)) %>%
        dplyr::select(-total)
    }
    dy <- temp %>%
      dplyr::ungroup() %>%
      tidyr::spread(group, clicks, fill = 0) %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_applinks), rename = FALSE) %>%
      polloi::make_dygraph("Date", ifelse(input$applinks_type == "prop", "Proportion (%)", "Clicks"), "Clicks on Wikipedia mobile app links") %>%
      dyLegend(labelsDiv = "applinks_legend", show = "always", width = 600) %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
    return(dy)
  })

  output$country_breakdown_dygraph <- renderDygraph({
    if (input$group_us_regions) {
      temp <- country_data
      temp$`United States` <- rowSums(temp[, grepl("(United States)|(U\\.S\\. )", colnames(temp)), drop = FALSE])
      temp <- temp[, grep("U.S.", colnames(temp), value = TRUE, invert = TRUE, fixed = TRUE)]
    } else {
      temp <- country_data
    }
    if (input$hide_less_than_5) {
      temp[, -1] <- apply(temp[, -1], 2, function(y) {
        return(replace(y, y < 5, NA))
      })
    }
    if (input$hide_more_than_15) {
      temp[, -1] <- apply(temp[, -1], 2, function(y) {
        return(replace(y, y > 15, NA))
      })
    }
    temp %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_country_breakdown)) %>%
      { .[, apply(., 2, function(y) { return(sum(!is.na(y))) }) > 0] } %>%
      polloi::make_dygraph(., xlab = "Date", ylab = "Users (%)", title = "Geographic breakdown of portal visitors") %>%
      dyLegend(labelsDiv = "country_breakdown_legend", show = "always", width = 400) %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyCSS(css = "www/inverse.css") %>%
      dyRangeSelector(strokeColor = "white", fillColor = "gray", retainDateWindow = TRUE) %>%
      dyEvent(as.Date("2016-03-10"), "Search Box Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-18"), "Sister Links Updated", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-28"), "A (regional U.S.)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-08-16"), "Secondary Links Collapsed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-09-13"), "B (schema switch)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })

  output$browser_selector_container <- renderUI({
    browsers <- switch(input$browser_order,
                       "alphabet" = {
                         if (input$group_browsers) {
                           if (input$browser_grouping == "family") {
                             sort(browser_rates$browser, na.last = TRUE)
                           } else { # "support"
                             sort(support_rates$support, na.last = TRUE)
                           }
                         } else {
                           sort(version_rates$version, na.last = TRUE)
                         }
                       },
                       "growth" = {
                         if (input$group_browsers) {
                           if (input$browser_grouping == "family") {
                             browser_rates$browser[order(browser_rates$rate, decreasing = TRUE, na.last = TRUE)]
                           } else { # "support"
                             support_rates$support[order(support_rates$rate, decreasing = TRUE, na.last = TRUE)]
                           }
                         } else {
                           version_rates$version[order(version_rates$rate, decreasing = TRUE, na.last = TRUE)]
                         }
                       },
                       "decay" = {
                         if (input$group_browsers) {
                           if (input$browser_grouping == "family") {
                             browser_rates$browser[order(browser_rates$rate, decreasing = FALSE, na.last = TRUE)]
                           } else { # "support"
                             support_rates$support[order(support_rates$rate, decreasing = FALSE, na.last = TRUE)]
                           }
                         } else {
                           version_rates$version[order(version_rates$rate, decreasing = FALSE, na.last = TRUE)]
                         }
                       },
                       "last" = {
                         if (input$group_browsers) {
                           if (input$browser_grouping == "family") {
                             browser_rates$browser[order(browser_rates$last, decreasing = TRUE, na.last = TRUE)]
                           } else { # "support"
                             support_rates$support[order(support_rates$last, decreasing = TRUE, na.last = TRUE)]
                           }
                         } else {
                           version_rates$version[order(version_rates$last, decreasing = TRUE, na.last = TRUE)]
                         }
                       },
                       "times" = {
                         if (input$group_browsers) {
                           if (input$browser_grouping == "family") {
                             browser_rates$browser[order(browser_rates$times, decreasing = TRUE, na.last = TRUE)]
                           } else { # "support"
                             support_rates$support[order(support_rates$times, decreasing = TRUE, na.last = TRUE)]
                           }
                         } else {
                           version_rates$version[order(version_rates$times, decreasing = TRUE, na.last = TRUE)]
                         }
                       })
    if (input$browser_filter != "") {
      if (grepl(",\\s?", input$browser_filter, fixed = FALSE)) {
        browser_filter <- gsub(", ", ",", input$browser_filter, fixed = TRUE)
        browser_filter <- strsplit(browser_filter, ",")[[1]]
        browsers <- browsers[browsers %in% unlist(lapply(browser_filter, function(browser) {
          browsers[grepl(browser, browsers, ignore.case = TRUE)]
        }))]
      } else {
        browsers <- browsers[grepl(input$browser_filter, browsers, ignore.case = TRUE)]
      }
    }
    if (!is.null(input$browser_selector)) {
      selected <- input$browser_selector
    } else {
      selected <- browsers[1]
    }
    return(selectInput("browser_selector", "Browser", selected = selected, choices = browsers,
                       multiple = TRUE, selectize = FALSE, size = 15))
  })

  output$browser_breakdown_dygraph <- renderDygraph({
    if (input$group_browsers) {
      if (input$browser_grouping == "family") {
        temp <- ua_data[ua_data$browser %in% input$browser_selector,
                        j = list(percent = sum(percent)),
                        by = c("date", "browser")] %>%
          reshape2::dcast(date ~ browser, fun.aggregate = sum)
      } else { # "support"
        temp <- ua_data[ua_data$support %in% input$browser_selector,
                        j = list(percent = sum(percent)),
                        by = c("date", "support")] %>%
          reshape2::dcast(date ~ support, fun.aggregate = sum)
      }
    } else {
      temp <- ua_data[ua_data$version %in% input$browser_selector,
                      j = list(percent = sum(percent)),
                      by = c("date", "version")] %>%
        reshape2::dcast(date ~ version, fun.aggregate = sum)
    }
    temp %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_browser_breakdown)) %>%
      polloi::make_dygraph(xlab = "Date", ylab = "Share (%)", title = "Browser breakdown of portal visitors") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyLegend(labelsDiv = "browser_breakdown_legend", show = "always", width = 400) %>%
      dyRangeSelector(fillColor = "", strokeColor = "", retainDateWindow = TRUE) %>%
      dyEvent(as.Date("2016-03-10"), "Search Box Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-18"), "Sister Links Updated", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-08-16"), "Secondary Links Collapsed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })

  output$pageview_dygraph <- renderDygraph({
    pageview_data %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_pageviews)) %>%
      polloi::make_dygraph(xlab = "Date", ylab = "Pageviews", title = "Pageviews to the Wikipedia Portal") %>%
      dyLegend(labelsDiv = "pageview_legend", show = "always") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyAxis("y", logscale = input$pageview_logscale) %>%
      dyRangeSelector(strokeColor = "white", fillColor = "gray", retainDateWindow = TRUE) %>%
      dyEvent(as.Date("2016-03-10"), "Search Box Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-01"), "A (search-redirect.php)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-18"), "Sister Links Updated", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-09"), "B (unexplainable rise)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-22"), "C (pageview redefined)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-07-11"), "D (split-pageviews)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-08-16"), "Secondary Links Collapsed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })

  output$referer_summary_dygraph <- renderDygraph({
    summary_traffic_data %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_referer_summary)) %>%
      polloi::make_dygraph(xlab = "Date", ylab = "% of Pageviews",
                           title = "Traffic to Wikipedia Portal brown down by origin") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyAxis("y", valueFormatter = 'function(x) { return x + "%"; }') %>%
      dyLegend(labelsDiv = "referer_summary_legend", show = "always") %>%
      dyRangeSelector(fillColor = "", strokeColor = "", retainDateWindow = TRUE) %>%
      dyEvent(as.Date("2016-03-07"), "A (UDF switch)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-01"), "B (search-redirect.php)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })

  output$search_engines_dygraph <- renderDygraph({
    bysearch_traffic_data %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_search_engines)) %>%
      polloi::make_dygraph(xlab = "Date", ylab = "% of Pageviews",
                           title = "Traffic to Wikipedia Portal broken down by search engine") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyAxis("y", valueFormatter = 'function(x) { return x + "%"; }') %>%
      dyLegend(labelsDiv = "search_engines_legend", show = "always") %>%
      dyRangeSelector(fillColor = "", strokeColor = "", retainDateWindow = TRUE) %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })

  output$s_dygraph <- renderDygraph({

    if (!input$s_enwiki) {
      idx <- langs_visited$prefix != "en"
    } else {
      idx <- TRUE
    }

    if (input$s_response == "clicks") {
      if (input$s_type == "count") {
        data4dygraph <- langs_visited[idx,
                                      list(
                                        "total clicks" = sum(clicks),
                                        search = sum(search),
                                        primary = sum(primary),
                                        secondary = sum(secondary)
                                      ),
                                      by = c("date")]
      } else { # == "prop"
        data4dygraph <- langs_visited[idx,
                                      list(
                                        search = round(100*sum(search)/sum(clicks), 2),
                                        primary = round(100*sum(primary)/sum(clicks), 2),
                                        secondary = round(100*sum(secondary)/sum(clicks), 2)
                                      ),
                                      by = c("date")]
      }
      dyout <- data4dygraph %>%
        fill_out(start_date = min(langs_visited$date), end_date = max(langs_visited$date)) %>%
        polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_summary)) %>%
        polloi::make_dygraph(xlab = "Date",
                             ylab = ifelse(input$s_type == "prop", "Proportion of total clicks (%)", "Clicks"),
                             title = paste("Clicks to Wikipedias ", ifelse(input$s_enwiki, "(All Languages)", "(All Except English)"), " from Wikipedia.org")) %>%
        dyRangeSelector(
          fillColor = ifelse(input$s_type == "prop", "", "gray"),
          strokeColor = ifelse(input$s_type == "prop", "", "white"),
          retainDateWindow = TRUE)
    } else { # == "users"
      dyout <- langs_visited[idx, list(users = sum(sessions)), by = "date"] %>%
        fill_out(start_date = min(langs_visited$date), end_date = max(langs_visited$date)) %>%
        polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_summary)) %>%
        polloi::make_dygraph(xlab = "Date", ylab = "Unique sessions", title = paste("Users who went to Wikipedias ", ifelse(input$s_enwiki, "(All Languages)", "(All Except English)"), " from Wikipedia.org")) %>%
        dyCSS(css = "www/inverse.css") %>%
        dyRangeSelector(fillColor = "gray", strokeColor = "white", retainDateWindow = TRUE)
    }
    dyout %>%
      dyOptions(strokeWidth = 3, labelsKMB = TRUE, drawPoints = FALSE, pointSize = 3, includeZero = TRUE,
                logscale = input$s_response == "clicks" && input$s_type == "count" && input$s_logscale) %>%
      dyLegend(width = 400, labelsDiv = "s_legend", show = "always", showZeroValues = FALSE) %>%
      dyCSS(css = "www/inverse.css") %>%
      dyAxis("x", rangePad = 8) %>%
      dyEvent(as.Date("2015-11-18"), "A1 (Started EL)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-03-10"), "Search Box Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-10"), "A2 (Unfiltered Counts)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-18"), "Sister Links Updated", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-08-16"), "Secondary Links Collapsed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-09-13"), "B (schema switch)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })

  lv_reactive <- reactiveValues(choices = NULL, selected_langs = NULL)

  observeEvent(input$lv_sort, {
    if (input$lv_sort %in% c("alphabet_az", "alphabet_za")) {
      lv_reactive$choices <- sort(unique(langs_visited$language), decreasing = input$lv_sort == "alphabet_za")
    } else {
      languages <- langs_visited[, list(
        clicks = sum(clicks),
        users = sum(sessions),
        avg_daily_clicks = as.integer(median(clicks)),
        avg_daily_users = as.integer(median(sessions))),
        by = "language"]
      lv_reactive$choices <- switch(input$lv_sort,
                                    clicks_high2low = {
                                      languages$language[order(languages[[input$lv_response]], decreasing = TRUE)]
                                    },
                                    clicks_low2high = {
                                      languages$language[order(languages[[input$lv_response]], decreasing = FALSE)]
                                    },
                                    avg_clicks_high2low = {
                                      languages$language[order(languages[[ifelse(input$lv_response == "clicks", "avg_daily_clicks", "avg_daily_users")]], decreasing = TRUE)]
                                    },
                                    avg_clicks_low2high = {
                                      languages$language[order(languages[[ifelse(input$lv_response == "clicks", "avg_daily_clicks", "avg_daily_users")]], decreasing = FALSE)]
                                    },
                                    top10 = {
                                      if (input$lv_response == "users") {
                                        dplyr::arrange(languages[languages[[input$lv_response]] %in% head(sort(languages[[input$lv_response]], decreasing = TRUE), 10), ], desc(users), language)$language
                                      } else {
                                        dplyr::arrange(languages[languages[[input$lv_response]] %in% head(sort(languages[[input$lv_response]], decreasing = TRUE), 10), ], desc(clicks), language)$language
                                      }
                                    },
                                    bottom50 = {
                                      if (input$lv_response == "users") {
                                        dplyr::arrange(languages[languages[[input$lv_response]] %in% head(sort(languages[[input$lv_response]], decreasing = FALSE), 50), ], desc(users), language)$language
                                      } else {
                                        dplyr::arrange(languages[languages[[input$lv_response]] %in% head(sort(languages[[input$lv_response]], decreasing = FALSE), 50), ], desc(clicks), language)$language
                                      }
                                    })
    }
    if (input$lv_sort %in% c("top10", "bottom50")) {
      lv_reactive$selected_langs <- lv_reactive$choices
    } else {
      if (!is.null(input$lv_languages)) {
        if (sum(input$lv_languages %in% lv_reactive$choices) == 0) {
          lv_reactive$selected_langs <- lv_reactive$choices[1]
        } else {
          # Lets us keep selected languages when switching betweeen clicks and users
          lv_reactive$selected_langs <- intersect(input$lv_languages, lv_reactive$choices)
        }
      } else {
        lv_reactive$selected_langs <- lv_reactive$choices[1]
      }
    }
  })

  observeEvent(input$lv_languages, {
    lv_reactive$selected_langs <- input$lv_languages
  })

  output$lv_languages_container <- renderUI({
    if (input$lv_sort %in% c("top10", "bottom50")) {
      hidden(disabled(selectizeInput("lv_languages", "Wikipedia languages", lv_reactive$choices, lv_reactive$selected_langs, multiple = TRUE)))
    } else {
      selectizeInput("lv_languages", "Wikipedia languages (12 max)", lv_reactive$choices, lv_reactive$selected_langs, multiple = TRUE, options = list(maxItems = 12))
    }
  })

  outputOptions(output, "lv_languages_container", suspendWhenHidden=FALSE)

  observeEvent(input$lv_sort, {

    # Make legend small in case of Top 10 or Bottom 50, otherwise make it big:
    toggleClass("lv_legend", "small", input$lv_sort %in% c("top10", "bottom50"))
    toggleClass("lv_legend", "large", !input$lv_sort %in% c("top10", "bottom50"))

    if (input$lv_sort == "bottom50") {
      updateCheckboxInput(session, "lv_combine", value = TRUE)
    } else {
      updateCheckboxInput(session, "lv_combine", value = FALSE)
    }

  })

  output$lv_dygraph <- renderDygraph({
    if (input$lv_response == "clicks") {
      if (length(input$lv_languages) > 1) {
        data4dygraph <- langs_visited[langs_visited$language %in% input$lv_languages, c("date", "language", "clicks"), with = FALSE] %>%
          {
            if (input$lv_sort != "top10" && input$lv_combine && length(input$lv_languages) > 1) {
              dplyr::summarize(group_by(., date), `total clicks` = sum(clicks))
            } else {
              tidyr::spread(., language, clicks, fill = 0)
            }
          } %>%
          fill_out(start_date = min(langs_visited$date), end_date = max(langs_visited$date)) %>%
          {
            if (input$lv_sort != "top10" && input$lv_combine && length(input$lv_languages) > 1) {
              .[, union("date", names(.))]
            } else {
              .[, c("date", input$lv_languages)]
            }
          }
      } else {
        if (input$lv_type == "count") {
          data4dygraph <- langs_visited[langs_visited$language == input$lv_languages, c("date", "clicks", "search", "primary", "secondary"), with = FALSE] %>%
            fill_out(start_date = min(langs_visited$date), end_date = max(langs_visited$date))
        } else { # == "prop"
          data4dygraph <- langs_visited[langs_visited$language == input$lv_languages, c("date", "clicks", "search", "primary", "secondary"), with = FALSE] %>%
            fill_out(start_date = min(langs_visited$date), end_date = max(langs_visited$date))
          data4dygraph$search <- round(100*data4dygraph$search/data4dygraph$clicks, 2)
          data4dygraph$primary <- round(100*data4dygraph$primary/data4dygraph$clicks, 2)
          data4dygraph$secondary <- round(100*data4dygraph$secondary/data4dygraph$clicks, 2)
          data4dygraph$clicks <- NULL
        }
      }
    } else { # == "users"
      data4dygraph <- langs_visited[langs_visited$language %in% input$lv_languages, c("date", "language", "sessions"), with = FALSE] %>%
        tidyr::spread(language, sessions, fill = 0) %>%
        fill_out(start_date = min(langs_visited$date), end_date = max(langs_visited$date)) %>%
        {
          .[, c("date", input$lv_languages)]
        }
    }
    data4dygraph[is.na(data4dygraph)] <- 0
    data4dygraph %>%
    {
      tryCatch(polloi::smoother(., smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_lv)),
               error = function(e) {
                 stop("Cannot apply spline smoothing on one or more of the selected languages.")
               }, finally = NULL)
    } %>%
    polloi::make_dygraph(xlab = "Date", ylab = ifelse(input$lv_response == "clicks", ifelse(input$lv_type == "count", "Clicks", "Proportion of total clicks (%)"), "Unique sessions"),
                           title = paste0(ifelse(input$lv_response == "clicks", "Clicks", "Users who went"), " to ", paste0(input$lv_languages, collapse = ", ")," Wikipedia", ifelse(length(input$lv_languages) > 1, "s", ""), " from Portal")) %>%
      dyRangeSelector(fillColor = ifelse(input$lv_type == "prop", "", "gray"),
                      strokeColor = ifelse(input$lv_type == "prop", "", "white"),
                      retainDateWindow = TRUE) %>%
      dyOptions(strokeWidth = 3, labelsKMB = TRUE, drawPoints = FALSE, pointSize = 3, includeZero = TRUE,
                logscale = input$lv_logscale && input$lv_type == "count") %>%
      dyAxis("x", rangePad = 8) %>%
      dyLegend(width = 400, labelsDiv = "lv_legend", show = "always", showZeroValues = FALSE) %>%
      dyCSS(css = "www/inverse.css") %>%
      dyEvent(as.Date("2015-11-18"), "A1 (Started EL)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-03-10"), "Search Box Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-10"), "A2 (Unfiltered Counts)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-18"), "Sister Links Updated", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-08-16"), "Secondary Links Collapsed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-09-13"), "B (schema switch)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })

  # General Geo Breakdowns
  cntr_reactive <- reactiveValues(selected_metric_a = NULL, selected_metric_f = NULL, selected_metric_l = NULL, selected_metric_m = NULL)

  all_country_data_dt <- reactive({
    if (input$cntr_sort_a %in% c("us_a", "custom_a")){
      fnl_tbl <- all_country_data %>%
        rbind(us_data) %>%
        dplyr::filter(Date >= input$date_all_country[1]
               & Date <= input$date_all_country[2]
               & Country %in% selected_country_a()) %>%
        dplyr::arrange(Date, Country)
    } else{
      fnl_tbl <- all_country_data %>%
        dplyr::filter(Date >= input$date_all_country[1]
               & Date <= input$date_all_country[2]
               & Country %in% selected_country_a())
    }
    return (fnl_tbl)
  })
  all_country_data_prop_dt <- reactive({
    if (input$cntr_sort_a %in% c("us_a", "custom_a")){
      fnl_tbl <- all_country_data_prop %>%
        rbind(us_data_prop) %>%
        dplyr::filter(Date >= input$date_all_country[1]
               & Date <= input$date_all_country[2]
               & Country %in% selected_country_a()) %>%
        dplyr::arrange(Date, Country)
    } else{
      fnl_tbl <- all_country_data_prop %>%
        dplyr::filter(Date >= input$date_all_country[1]
               & Date <= input$date_all_country[2]
               & Country %in% selected_country_a())
    }
    return (fnl_tbl)
  })
  observeEvent(input$traffic_select, {
    cntr_reactive$selected_metric_a <- switch(input$traffic_select,
                                              events = "Number of Events",
                                              visits = "Number of Visits",
                                              sessions = "Number of Sessions",
                                              ctr_all = "Overall Clickthrough Rate",
                                              ctr_vst = "Clickthrough Rate Per Visit",
                                              ctr_ses = "Clickthrough Rate Per Session"
    )
  })
  selected_country_a <- reactive({
    all_country_temp<- all_country_data %>%
      dplyr::filter(all_country_data$Date >= input$date_all_country[1]
             & all_country_data$Date <= input$date_all_country[2]) %>%
      dplyr::select_(.dots=c("Country",paste0("`",cntr_reactive$selected_metric_a,"`"))) %>%
      dplyr::group_by(Country) %>%
      dplyr::summarize_(.dots =
                   if(input$traffic_select %in% c('events','visits','sessions')) as.formula(paste0("~ sum(`",cntr_reactive$selected_metric_a,"`)")) else as.formula(paste0("~ median(`",cntr_reactive$selected_metric_a,"`)"))
      ) %>%
      dplyr::ungroup() %>%
      as.data.frame()
    all_country_temp <- all_country_temp[order(all_country_temp[,2], all_country_temp[,1]),]
    result <- switch(input$cntr_sort_a,
                     top10_a = {tail(all_country_temp, 10)$Country},
                     bottom50_a = {head(all_country_temp, 50)$Country},
                     us_a = unique(us_data$Country),
                     all_a = unique(all_country_data$Country),
                     all_nus_a = unique(all_country_data$Country)[!grepl("United States", unique(all_country_data$Country))],
                     custom_a = input$cntr_a
    )
    return(result)
  })
  observeEvent(input$cntr_sort_a, {
    toggleClass("cntr_a_legend", "small", length(selected_country_a())>7 & input$cntr_combine_a==F)
    toggleClass("cntr_a_legend", "large", length(selected_country_a())<=7 | input$cntr_combine_a)
    if (input$cntr_sort_a %in% c("bottom50_a","all_a","all_nus_a")) {
      updateCheckboxInput(session, "cntr_combine_a", value = TRUE)
    } else {
      updateCheckboxInput(session, "cntr_combine_a", value = FALSE)
    }
  })
  output$all_country_dygraph <- renderDygraph({
    if (input$cntr_combine_a == T){
      if (input$traffic_select == 'ctr_all'){
        data4dygraph <- all_country_data_dt() %>%
          dplyr::mutate(clicks=`Number of Events`*`Overall Clickthrough Rate`) %>%
          dplyr::group_by(Date) %>%
          dplyr::summarize("Overall Clickthrough Rate"=sum(clicks)/sum(`Number of Events`)) %>%
          dplyr::rename(date=Date) %>%
          fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
      } else if (input$traffic_select == 'ctr_vst'){
        data4dygraph <- all_country_data_dt() %>%
          dplyr::mutate(clicks=`Number of Visits`*`Clickthrough Rate Per Visit`) %>%
          dplyr::group_by(Date) %>%
          dplyr::summarize("Clickthrough Rate Per Visit"=sum(clicks)/sum(`Number of Visits`)) %>%
          dplyr::rename(date=Date) %>%
          fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
      } else if(input$traffic_select == 'ctr_ses'){
        data4dygraph <- all_country_data_dt() %>%
          dplyr::mutate(clicks=`Number of Sessions`*`Clickthrough Rate Per Session`) %>%
          dplyr::group_by(Date) %>%
          dplyr::summarize("Clickthrough Rate Per Session"=sum(clicks)/sum(`Number of Sessions`)) %>%
          dplyr::rename(date=Date) %>%
          fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
      } else{
        data4dygraph <- {
          if(input$prop_a) all_country_data_prop_dt() else all_country_data_dt()
        } %>%
          dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_a,"`"))) %>%
          dplyr::group_by(Date) %>%
          dplyr::summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_a,"`)")) %>%
          dplyr::rename(date=Date) %>%
          fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
        names(data4dygraph)[2] <- paste0(isolate(cntr_reactive$selected_metric_a))
      }
    } else{
      if (length(selected_country_a())>1){
        data4dygraph <- {
          if(input$prop_a) all_country_data_prop_dt() else all_country_data_dt()
        } %>%
          dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_a,"`"))) %>%
          tidyr::spread_(., key_col="Country", value_col=cntr_reactive$selected_metric_a, fill=0) %>%
          dplyr::rename(date=Date) %>%
          fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
      } else{
        data4dygraph <- {
          if(input$prop_a) all_country_data_prop_dt() else all_country_data_dt()
        } %>%
          dplyr::select_(.dots=c("Date", paste0("`",cntr_reactive$selected_metric_a,"`"))) %>%
          dplyr::rename(date=Date) %>%
          fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
        names(data4dygraph)[2] <- paste0(isolate(selected_country_a()))
      }
    }
    data4dygraph[is.na(data4dygraph)] <- 0
    if (input$traffic_select %in% c('ctr_all','ctr_vst','ctr_ses')){
      data4dygraph[, colnames(data4dygraph) != "date"] <- data4dygraph[, colnames(data4dygraph) != "date"]*100
    }
    data4dygraph %>%
    {
      tryCatch(polloi::smoother(., smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_cntr_a)),
               error = function(e) {
                 stop("Cannot apply spline smoothing on one or more of the selected countries.")
               }, finally = NULL)
    } %>%
      polloi::make_dygraph(xlab = "Date",
                           ylab = ifelse(input$prop_a & input$traffic_select %in% c('events','visits','sessions'),
                                         paste0("Proportion of ", cntr_reactive$selected_metric_a, " (%)"),
                                         ifelse(input$traffic_select %in% c('ctr_all','ctr_vst','ctr_ses'), paste0(cntr_reactive$selected_metric_a, " (%)"), cntr_reactive$selected_metric_a)),
                           title = paste0(ifelse(input$prop_a & input$traffic_select %in% c('events','visits','sessions'),
                                                 paste0("Out of all countries, the proportion of ", cntr_reactive$selected_metric_a, " (%)"),
                                                 cntr_reactive$selected_metric_a),
                                          " from ", ifelse(input$cntr_sort_a %in% c("all_a","all_nus_a"), ifelse(input$cntr_sort_a=="all_a","All Countries","All Countries but U.S."), paste0(selected_country_a(), collapse=", ")))) %>%
      dyRangeSelector(fillColor = "gray",
                      strokeColor = "white",
                      retainDateWindow = TRUE) %>%
      dyOptions(strokeWidth = 3, labelsKMB = TRUE, drawPoints = FALSE, pointSize = 3, includeZero = TRUE,
                titleHeight = 60, logscale = input$cntr_logscale_a) %>%
      dyAxis("x", rangePad = 8) %>%
      dyLegend(width = 400, labelsDiv = "cntr_a_legend", show = "always", showZeroValues = FALSE) %>%
      dyCSS(css = "www/inverse.css") %>%
      dyEvent(as.Date("2016-09-13"), "Schema Switch", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })
  output$traffic_pie_pl <- renderHighchart({
    if(input$traffic_select %in% c('events','ctr_all')){
      this_metric <- "Number of Events"
    } else if(input$traffic_select %in% c('visits','ctr_vst')){
      this_metric <- "Number of Visits"
    } else{
      this_metric <- "Number of Sessions"
    }
    data4pie <- all_country_data_dt() %>%
      dplyr::group_by(Region) %>%
      dplyr::mutate_(region_total=paste0("sum(`",this_metric,"`)")) %>%
      dplyr::group_by(Region, Country) %>%
      dplyr::mutate_(country_total=paste0("sum(`",this_metric,"`)")) %>%
      dplyr::select(Region, Country, country_total, region_total) %>%
      unique() %>% dplyr::ungroup() %>% dplyr::arrange(Region, Country)
    data4pie_us <- us_data %>%
      dplyr::filter(us_data$Date >= input$date_all_country[1]
             & us_data$Date <= input$date_all_country[2]) %>%
      dplyr::group_by(Country) %>%
      dplyr::summarize_(us_total=paste0("sum(`",this_metric,"`)")) %>% dplyr::ungroup()
    hc <- highchart() %>%
      hc_chart(type = "pie", plotBackgroundColor=NULL, plotBorderWidth=NULL, plotShadow=F) %>%
      hc_title(text = paste0(this_metric, " by Country"),
               style = list(color = "#ECF0F5")) %>%
      hc_tooltip(pointFormat = '{point.y} ({point.percentage:.1f}%)') %>%
      hc_plotOptions(
        pie = list(
          shadow = FALSE,
          center = c("50%", "50%"),
          dataLabels = list(
            style = list(
              color = '#ffffff',
              fontSize = '12px'
            )
          )
        )
      ) %>%
      hc_add_series(data = data4pie %>% dplyr::select(name=Region, y=region_total) %>%
                      dplyr::mutate(drilldown=tolower(name)) %>% unique() %>% list_parse(),
                    size = '60%',
                    name = "All Continents",
                    dataLabels = list(distance = -60,
                                      color = 'black',
                                      formatter = JS("function () {
                                                     return this.percentage > 5 ? this.point.name : null;
  }"))
                   ) %>%
      hc_add_series(data = data4pie %>% dplyr::select(name=Country, y=country_total) %>%
                      dplyr::mutate(drilldown=ifelse(name=="United States","united states", NA)) %>% unique() %>% list_parse(),
                    name = "All Countries", size = '100%', innerSize = "60%") %>%
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = data4pie %>% dplyr::mutate(id=tolower(Region)) %>%
          dplyr::select(name=Country, y=country_total, id) %>%
          unique() %>%
          split(.$id) %>%
          lapply(function(x){
            list(id= as.character(unique(x[,3])),
                 data=list_parse(x[,-3]))
          }) %>%
          magrittr::set_names(NULL) %>%
          append(list(list(
            id="united states",
            data=list_parse2(data4pie_us)
          ))),
        activeDataLabelStyle = list(
          color = '#1E90FF',
          fontSize = '12px',
          fontWeight = 'bold',
          textShadow = 'none'
        )
      )
    hc
  })
  output$all_country_tbl <- DT::renderDataTable(
    {if(input$prop_a){
      fnl_dt <- all_country_data_prop_dt()
      colnames(fnl_dt) <- c("Date", "Country", "Region", "Events Proportion",
                            "Overall Clickthrough Rate", "Visits Proportion", "Clickthrough Rate Per Visit",
                            "Sessions Proportion", "Clickthrough Rate Per Session")
    } else{
      fnl_dt <- all_country_data_dt()
    }
      ctr_mask <- colnames(fnl_dt) %in% c("Overall Clickthrough Rate", "Clickthrough Rate Per Visit", "Clickthrough Rate Per Session")
      fnl_dt[, ctr_mask] <- fnl_dt[, ctr_mask]*100
      colnames(fnl_dt)[ctr_mask] <- paste(colnames(fnl_dt)[ctr_mask], "(%)")
      fnl_dt},
    options = list(lengthMenu = list(c(15, 30, 60), c('15', '30', '60')), pageLength = 15)
  )
  # download the filtered data
  output$all_country_dl = downloadHandler('all_country_filtered.csv', content = function(file) {
    s <- input$all_country_tbl_rows_all
    if(input$prop_a){
      fnl_dt <- all_country_data_prop_dt()
      colnames(fnl_dt) <- c("Date", "Country", "Region", "Events Proportion",
                            "Overall Clickthrough Rate", "Visits Proportion", "Clickthrough Rate Per Visit",
                            "Sessions Proportion", "Clickthrough Rate Per Session")
    } else{
      fnl_dt <- all_country_data_dt()
    }
    ctr_mask <- colnames(fnl_dt) %in% c("Overall Clickthrough Rate", "Clickthrough Rate Per Visit", "Clickthrough Rate Per Session")
    fnl_dt[, ctr_mask] <- fnl_dt[, ctr_mask]*100
    colnames(fnl_dt)[ctr_mask] <- paste(colnames(fnl_dt)[ctr_mask], "(%)")
    write.csv(fnl_dt[s, , drop = FALSE], file, row.names=FALSE)
  })


  first_visits_country_dt <- reactive({
    if (input$cntr_sort_f %in% c("us_f", "custom_f")){
      fnl_tbl <- first_visits_country %>%
        rbind(first_visits_us) %>%
        dplyr::filter(Date >= input$date_first_visit[1]
               & Date <= input$date_first_visit[2]
               & Country %in% selected_country_f()) %>%
        dplyr::arrange(Date, Country)
    } else{
      fnl_tbl <- first_visits_country %>%
        dplyr::filter(Date >= input$date_first_visit[1]
               & Date <= input$date_first_visit[2]
               & Country %in% selected_country_f())
    }
    return (fnl_tbl)
  })
  first_visits_country_prop_dt <- reactive({
    if (input$cntr_sort_f %in% c("us_f", "custom_f")){
      fnl_tbl <- first_visits_country_prop %>%
        rbind(first_visits_us_prop) %>%
        dplyr::filter(Date >= input$date_first_visit[1]
               & Date <= input$date_first_visit[2]
               & Country %in% selected_country_f()) %>%
        dplyr::arrange(Date, Country)
    } else{
      fnl_tbl <- first_visits_country_prop %>%
        dplyr::filter(Date >= input$date_first_visit[1]
               & Date <= input$date_first_visit[2]
               & Country %in% selected_country_f())
    }
    return (fnl_tbl)
  })
  observeEvent(input$action_select_f, {
    cntr_reactive$selected_metric_f <- switch(input$action_select_f,
                                              nact_f = "No Action",
                                              olv_f = "Other Languages",
                                              oproj_f = "Other Projects",
                                              prln_f = "Primary Links",
                                              search_f = "Search",
                                              secln_f = "Secondary Links"
    )
  })
  selected_country_f <- reactive({
    all_country_temp<- first_visits_country %>%
      dplyr::filter(first_visits_country$Date >= input$date_first_visit[1]
             & first_visits_country$Date <= input$date_first_visit[2]) %>%
      dplyr::select_(.dots=c("Country",paste0("`",cntr_reactive$selected_metric_f,"`"))) %>%
      dplyr::group_by(Country) %>%
      dplyr::summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_f,"`)")) %>%
      dplyr::arrange(value, Country)
    result <- switch(input$cntr_sort_f,
                     top10_f = {tail(all_country_temp, 10)$Country},
                     bottom50_f = {head(all_country_temp, 50)$Country},
                     us_f = unique(first_visits_us$Country),
                     all_f = unique(first_visits_country$Country),
                     all_nus_f = unique(first_visits_country$Country)[!grepl("United States", unique(first_visits_country$Country))],
                     custom_f = input$cntr_f
    )
    return(result)
  })
  observeEvent(input$cntr_sort_f, {
    toggleClass("cntr_f_legend", "small", length(selected_country_f())>7 & input$cntr_combine_f==F)
    toggleClass("cntr_f_legend", "large", length(selected_country_f())<=7 | input$cntr_combine_f)
    if (input$cntr_sort_f %in% c("bottom50_f","all_f","all_nus_f")) {
      updateCheckboxInput(session, "cntr_combine_f", value = TRUE)
    } else {
      updateCheckboxInput(session, "cntr_combine_f", value = FALSE)
    }
  })
  output$first_visit_country_dygraph <- renderDygraph({
    if (input$cntr_combine_f == T){
      data4dygraph <- {
        if(input$prop_f) first_visits_country_prop_dt() else first_visits_country_dt()
      } %>%
        dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_f,"`"))) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_f,"`)")) %>%
        dplyr::rename(date=Date) %>%
        fill_out(start_date = input$date_first_visit[1], end_date = input$date_first_visit[2])
      names(data4dygraph)[2] <- paste0(isolate(cntr_reactive$selected_metric_f))
    } else{
      if (length(selected_country_f())>1){
        data4dygraph <- {
          if(input$prop_f) first_visits_country_prop_dt() else first_visits_country_dt()
        } %>%
          dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_f,"`"))) %>%
          tidyr::spread_(., key_col="Country", value_col=cntr_reactive$selected_metric_f, fill=0) %>%
          dplyr::rename(date=Date) %>%
          fill_out(start_date = input$date_first_visit[1], end_date = input$date_first_visit[2])
      } else{
        data4dygraph <- {
          if(input$prop_f) first_visits_country_prop_dt() else first_visits_country_dt()
        } %>%
          dplyr::select_(.dots=c("Date", paste0("`",cntr_reactive$selected_metric_f,"`"))) %>%
          dplyr::rename(date=Date) %>%
          fill_out(start_date = input$date_first_visit[1], end_date = input$date_first_visit[2])
        names(data4dygraph)[2] <- paste0(isolate(selected_country_f()))
      }
    }
    data4dygraph[is.na(data4dygraph)] <- 0
    data4dygraph %>%
    {
      tryCatch(polloi::smoother(., smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_cntr_f)),
               error = function(e) {
                 stop("Cannot apply spline smoothing on one or more of the selected countries.")
               }, finally = NULL)
    } %>%
      polloi::make_dygraph(xlab = "Date",
                           ylab = ifelse(input$prop_f, paste0("Proportion of ", cntr_reactive$selected_metric_f, " (%)"), cntr_reactive$selected_metric_f),
                           title = paste0(ifelse(input$prop_f, "Out of all countries, the proportion of clicks to ", "The number of clicks to "), cntr_reactive$selected_metric_f, " from ", ifelse(input$cntr_sort_f %in% c("all_f","all_nus_f"), ifelse(input$cntr_sort_f=="all_f","All Countries","All Countries but U.S."), paste0(selected_country_f(), collapse=", ")), " at first visit")) %>%
      dyRangeSelector(fillColor = "gray",
                      strokeColor = "white",
                      retainDateWindow = TRUE) %>%
      dyOptions(strokeWidth = 3, labelsKMB = TRUE, drawPoints = FALSE, pointSize = 3, includeZero = TRUE,
                titleHeight = 60, logscale = input$cntr_logscale_f) %>%
      dyAxis("x", rangePad = 8) %>%
      dyLegend(width = 400, labelsDiv = "cntr_f_legend", show = "always", showZeroValues = FALSE)  %>%
      dyCSS(css = "www/inverse.css") %>%
      dyEvent(as.Date("2016-09-13"), "Schema Switch", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })
  output$first_visit_pie_pl <- renderHighchart({
    data4pie <- first_visits_country_dt() %>%
      dplyr::group_by(Region) %>%
      dplyr::mutate_(region_total=paste0("sum(`",cntr_reactive$selected_metric_f,"`)")) %>%
      dplyr::group_by(Region, Country) %>%
      dplyr::mutate_(country_total=paste0("sum(`",cntr_reactive$selected_metric_f,"`)")) %>%
      dplyr::select(Region, Country, country_total, region_total) %>%
      unique() %>% dplyr::ungroup() %>% dplyr::arrange(Region, Country)
    data4pie_us <- first_visits_us %>%
      dplyr::filter(first_visits_us$Date >= input$date_first_visit[1]
             & first_visits_us$Date <= input$date_first_visit[2]) %>%
      dplyr::group_by(Country) %>%
      dplyr::summarize_(us_total=paste0("sum(`",cntr_reactive$selected_metric_f,"`)")) %>% dplyr::ungroup()
    hc <- highchart() %>%
      hc_chart(type = "pie", plotBackgroundColor=NULL, plotBorderWidth=NULL, plotShadow=F) %>%
      hc_title(text = paste0("Number of ", cntr_reactive$selected_metric_f, " by Country"),
               style = list(color = "#ECF0F5")) %>%
      hc_tooltip(pointFormat = '{point.y} ({point.percentage:.1f}%)') %>%
      hc_plotOptions(
        pie = list(
          shadow = FALSE,
          center = c("50%", "50%"),
          dataLabels = list(
            style = list(
              color = '#ffffff',
              fontSize = '12px'
            )
            )
        )
      ) %>%
      hc_add_series(data = data4pie %>% dplyr::select(name=Region, y=region_total) %>%
                      dplyr::mutate(drilldown=tolower(name)) %>% unique() %>% list_parse(),
                    size = '60%',
                    name = "All Continents",
                    dataLabels = list(distance = -60,
                                      color = 'black',
                                      formatter = JS("function () {
                                                     return this.percentage > 5 ? this.point.name : null;
  }"))
                   ) %>%
      hc_add_series(data = data4pie %>% dplyr::select(name=Country, y=country_total) %>%
                      dplyr::mutate(drilldown=ifelse(name=="United States","united states", NA)) %>% unique() %>% list_parse(),
                    name = "All Countries", size = '100%', innerSize = "60%") %>%
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = data4pie %>% dplyr::mutate(id=tolower(Region)) %>%
          dplyr::select(name=Country, y=country_total, id) %>%
          unique() %>%
          split(.$id) %>%
          lapply(function(x){
            list(id= as.character(unique(x[,3])),
                 data=list_parse(x[,-3]))
          }) %>%
          magrittr::set_names(NULL) %>%
          append(list(list(
            id="united states",
            data=list_parse2(data4pie_us)
          ))),
        activeDataLabelStyle = list(
          color = '#1E90FF',
          fontSize = '12px',
          fontWeight = 'bold',
          textShadow = 'none'
        )
      )
    hc
  })
  output$first_visits_by_country_tbl <- DT::renderDataTable(
    {if(input$prop_f) first_visits_country_prop_dt() else first_visits_country_dt()},
    options = list(lengthMenu = list(c(15, 30, 60), c('15', '30', '60')), pageLength = 15)
  )
  # download the filtered data
  output$first_visits_dl = downloadHandler('first_visits_filtered.csv', content = function(file) {
    s <- input$first_visits_by_country_tbl_rows_all
    if(input$prop_f){
      fnl_dt <- first_visits_country_prop_dt()
    } else{
      fnl_dt <- first_visits_country_dt()
    }
    write.csv(fnl_dt[s, , drop = FALSE], file, row.names=FALSE)
  })


  last_action_country_dt <- reactive({
    if (input$cntr_sort_l %in% c("us_l", "custom_l")){
      fnl_tbl <- last_action_country %>%
        rbind(last_action_us) %>%
        dplyr::filter(Date >= input$date_last_action[1]
               & Date <= input$date_last_action[2]
               & Country %in% selected_country_l()) %>%
        dplyr::arrange(Date, Country)
    } else{
      fnl_tbl <- last_action_country %>%
        dplyr::filter(Date >= input$date_last_action[1]
               & Date <= input$date_last_action[2]
               & Country %in% selected_country_l())
    }
    return (fnl_tbl)
  })
  last_action_country_prop_dt <- reactive({
    if (input$cntr_sort_l %in% c("us_l", "custom_l")){
      fnl_tbl <- last_action_country_prop %>%
        rbind(last_action_us_prop) %>%
        dplyr::filter(Date >= input$date_last_action[1]
               & Date <= input$date_last_action[2]
               & Country %in% selected_country_l()) %>%
        dplyr::arrange(Date, Country)
    } else{
      fnl_tbl <- last_action_country_prop %>%
        dplyr::filter(Date >= input$date_last_action[1]
               & Date <= input$date_last_action[2]
               & Country %in% selected_country_l())
    }
    return (fnl_tbl)
  })
  observeEvent(input$action_select_l, {
    cntr_reactive$selected_metric_l <- switch(input$action_select_l,
                                              nact_l = "No Action",
                                              olv_l = "Other Languages",
                                              oproj_l = "Other Projects",
                                              prln_l = "Primary Links",
                                              search_l = "Search",
                                              secln_l = "Secondary Links"
    )
  })
  selected_country_l <- reactive({
    all_country_temp<- last_action_country %>%
      dplyr::filter(last_action_country$Date >= input$date_last_action[1]
             & last_action_country$Date <= input$date_last_action[2]) %>%
      dplyr::select_(.dots=c("Country",paste0("`",cntr_reactive$selected_metric_l,"`"))) %>%
      dplyr::group_by(Country) %>%
      dplyr::summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_l,"`)")) %>%
      dplyr::arrange(value, Country)
    result <- switch(input$cntr_sort_l,
                     top10_l = {tail(all_country_temp, 10)$Country},
                     bottom50_l = {head(all_country_temp, 50)$Country},
                     us_l = unique(last_action_us$Country),
                     all_l = unique(last_action_country$Country),
                     all_nus_l = unique(last_action_country$Country)[!grepl("United States", unique(last_action_country$Country))],
                     custom_l = input$cntr_l
    )
    return(result)
  })
  observeEvent(input$cntr_sort_l, {
    toggleClass("cntr_l_legend", "small", length(selected_country_l())>7 & input$cntr_combine_l==F)
    toggleClass("cntr_l_legend", "large", length(selected_country_l())<=7 | input$cntr_combine_l)
    if (input$cntr_sort_l %in% c("bottom50_l","all_l","all_nus_l")) {
      updateCheckboxInput(session, "cntr_combine_l", value = TRUE)
    } else {
      updateCheckboxInput(session, "cntr_combine_l", value = FALSE)
    }
  })
  output$last_action_dygraph <- renderDygraph({
    if (input$cntr_combine_l == T){
      data4dygraph <- {
        if(input$prop_l) last_action_country_prop_dt() else last_action_country_dt()
      } %>%
        dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_l,"`"))) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_l,"`)")) %>%
        dplyr::rename(date=Date) %>%
        fill_out(start_date = input$date_last_action[1], end_date = input$date_last_action[2])
      names(data4dygraph)[2] <- paste0(isolate(cntr_reactive$selected_metric_l))
    } else{
      if (length(selected_country_l())>1){
        data4dygraph <- {
          if(input$prop_l) last_action_country_prop_dt() else last_action_country_dt()
        } %>%
          dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_l,"`"))) %>%
          tidyr::spread_(., key_col="Country", value_col=cntr_reactive$selected_metric_l, fill=0) %>%
          dplyr::rename(date=Date) %>%
          fill_out(start_date = input$date_last_action[1], end_date = input$date_last_action[2])
      } else{
        data4dygraph <- {
          if(input$prop_l) last_action_country_prop_dt() else last_action_country_dt()
        } %>%
          dplyr::select_(.dots=c("Date", paste0("`",cntr_reactive$selected_metric_l,"`"))) %>%
          dplyr::rename(date=Date) %>%
          fill_out(start_date = input$date_last_action[1], end_date = input$date_last_action[2])
        names(data4dygraph)[2] <- paste0(isolate(selected_country_l()))
      }
    }
    data4dygraph[is.na(data4dygraph)] <- 0
    data4dygraph %>%
    {
      tryCatch(polloi::smoother(., smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_cntr_l)),
               error = function(e) {
                 stop("Cannot apply spline smoothing on one or more of the selected countries.")
               }, finally = NULL)
    } %>%
      polloi::make_dygraph(xlab = "Date",
                           ylab = ifelse(input$prop_l, paste0("Proportion of ", cntr_reactive$selected_metric_l, " (%)"), cntr_reactive$selected_metric_l),
                           title = paste0(ifelse(input$prop_l, "Out of all countries, the proportion of sessions whose last action is ", "The number of sessions whose last action is "), cntr_reactive$selected_metric_l, " from ", ifelse(input$cntr_sort_l %in% c("all_l","all_nus_l"), ifelse(input$cntr_sort_l=="all_l","All Countries","All Countries but U.S."), paste0(selected_country_l(), collapse=", ")))) %>%
      dyRangeSelector(fillColor = "gray",
                      strokeColor = "white",
                      retainDateWindow = TRUE) %>%
      dyOptions(strokeWidth = 3, labelsKMB = TRUE, drawPoints = FALSE, pointSize = 3, includeZero = TRUE,
                titleHeight = 60, logscale = input$cntr_logscale_l) %>%
      dyAxis("x", rangePad = 8) %>%
      dyLegend(width = 400, labelsDiv = "cntr_l_legend", show = "always", showZeroValues = FALSE)  %>%
      dyCSS(css = "www/inverse.css") %>%
      dyEvent(as.Date("2016-09-13"), "Schema Switch", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })
  output$last_action_pie_pl <- renderHighchart({
    data4pie <- last_action_country_dt() %>%
      dplyr::group_by(Region) %>%
      dplyr::mutate_(region_total=paste0("sum(`",cntr_reactive$selected_metric_l,"`)")) %>%
      dplyr::group_by(Region, Country) %>%
      dplyr::mutate_(country_total=paste0("sum(`",cntr_reactive$selected_metric_l,"`)")) %>%
      dplyr::select(Region, Country, country_total, region_total) %>%
      unique() %>% dplyr::ungroup() %>% dplyr::arrange(Region, Country)
    data4pie_us <- last_action_us %>%
      dplyr::filter(last_action_us$Date >= input$date_last_action[1]
             & last_action_us$Date <= input$date_last_action[2]) %>%
      dplyr::group_by(Country) %>%
      dplyr::summarize_(us_total=paste0("sum(`",cntr_reactive$selected_metric_l,"`)")) %>% dplyr::ungroup()
    hc <- highchart() %>%
      hc_chart(type = "pie", plotBackgroundColor=NULL, plotBorderWidth=NULL, plotShadow=F) %>%
      hc_title(text = paste0("Number of ", cntr_reactive$selected_metric_l, " by Country"),
               style = list(color = "#ECF0F5")) %>%
      hc_tooltip(pointFormat = '{point.y} ({point.percentage:.1f}%)') %>%
      hc_plotOptions(
        pie = list(
          shadow = FALSE,
          center = c("50%", "50%"),
          dataLabels = list(
            style = list(
              color = '#ffffff',
              fontSize = '12px'
            )
          )
        )
      ) %>%
      hc_add_series(data = data4pie %>% dplyr::select(name=Region, y=region_total) %>%
                      dplyr::mutate(drilldown=tolower(name)) %>% unique() %>% list_parse(),
                    size = '60%',
                    name = "All Continents",
                    dataLabels = list(distance = -60,
                                      color = 'black',
                                      formatter = JS("function () {
                                                     return this.percentage > 5 ? this.point.name : null;
  }"))
                   ) %>%
      hc_add_series(data = data4pie %>% dplyr::select(name=Country, y=country_total) %>%
                      dplyr::mutate(drilldown=ifelse(name=="United States","united states", NA)) %>% unique() %>% list_parse(),
                    name = "All Countries", size = '100%', innerSize = "60%") %>%
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = data4pie %>% dplyr::mutate(id=tolower(Region)) %>%
          dplyr::select(name=Country, y=country_total, id) %>%
          unique() %>%
          split(.$id) %>%
          lapply(function(x){
            list(id= as.character(unique(x[,3])),
                 data=list_parse(x[,-3]))
          }) %>%
          magrittr::set_names(NULL) %>%
          append(list(list(
            id="united states",
            data=list_parse2(data4pie_us)
          ))),
        activeDataLabelStyle = list(
          color = '#1E90FF',
          fontSize = '12px',
          fontWeight = 'bold',
          textShadow = 'none'
        )
      )
    hc
  })
  output$last_action_by_country_tbl <- DT::renderDataTable(
    {if(input$prop_l) last_action_country_prop_dt() else last_action_country_dt()},
    options = list(lengthMenu = list(c(15, 30, 60), c('15', '30', '60')), pageLength = 15)
  )
  # download the filtered data
  output$last_action_dl = downloadHandler('last_action_filtered.csv', content = function(file) {
    s <- input$last_action_by_country_tbl_rows_all
    if(input$prop_l){
      fnl_dt <- last_action_country_prop_dt()
    } else{
      fnl_dt <- last_action_country_dt()
    }
    write.csv(fnl_dt[s, , drop = FALSE], file, row.names=FALSE)
  })


  most_common_country_dt <- reactive({
    if (input$cntr_sort_m %in% c("us_m", "custom_m")){
      fnl_tbl <- most_common_country %>%
        rbind(most_common_us) %>%
        dplyr::filter(Date >= input$date_most_common[1]
               & Date <= input$date_most_common[2]
               & Country %in% selected_country_m()) %>%
        dplyr::arrange(Date, Country)
    } else{
      fnl_tbl <- most_common_country %>%
        dplyr::filter(Date >= input$date_most_common[1]
               & Date <= input$date_most_common[2]
               & Country %in% selected_country_m())
    }
    return (fnl_tbl)
  })
  most_common_country_prop_dt <- reactive({
    if (input$cntr_sort_m %in% c("us_m", "custom_m")){
      fnl_tbl <- most_common_country_prop %>%
        rbind(most_common_us_prop) %>%
        dplyr::filter(Date >= input$date_most_common[1]
               & Date <= input$date_most_common[2]
               & Country %in% selected_country_m()) %>%
        dplyr::arrange(Date, Country)
    } else{
      fnl_tbl <- most_common_country_prop %>%
        dplyr::filter(Date >= input$date_most_common[1]
               & Date <= input$date_most_common[2]
               & Country %in% selected_country_m())
    }
    return (fnl_tbl)
  })
  observeEvent(input$action_select_m, {
    cntr_reactive$selected_metric_m <- switch(input$action_select_m,
                                              olv_m = "Other Languages",
                                              oproj_m = "Other Projects",
                                              prln_m = "Primary Links",
                                              search_m = "Search",
                                              secln_m = "Secondary Links"
    )
  })
  selected_country_m <- reactive({
    all_country_temp<- most_common_country %>%
      dplyr::filter(most_common_country$Date >= input$date_most_common[1]
             & most_common_country$Date <= input$date_most_common[2]) %>%
      dplyr::select_(.dots=c("Country",paste0("`",cntr_reactive$selected_metric_m,"`"))) %>%
      dplyr::group_by(Country) %>%
      dplyr::summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_m,"`)")) %>%
      dplyr::arrange(value, Country)
    result <- switch(input$cntr_sort_m,
                     top10_m = {tail(all_country_temp, 10)$Country},
                     bottom50_m = {head(all_country_temp, 50)$Country},
                     us_m = unique(most_common_us$Country),
                     all_m = unique(most_common_country$Country),
                     all_nus_m = unique(most_common_country$Country)[!grepl("United States", unique(most_common_country$Country))],
                     custom_m = input$cntr_m
    )
    return(result)
  })
  observeEvent(input$cntr_sort_m, {
    toggleClass("cntr_m_legend", "small", length(selected_country_m())>7 & input$cntr_combine_m==F)
    toggleClass("cntr_m_legend", "large", length(selected_country_m())<=7 | input$cntr_combine_m)
    if (input$cntr_sort_m %in% c("bottom50_m","all_m","all_nus_m")) {
      updateCheckboxInput(session, "cntr_combine_m", value = TRUE)
    } else {
      updateCheckboxInput(session, "cntr_combine_m", value = FALSE)
    }
  })
  output$most_common_country_dygraph <- renderDygraph({
    if (input$cntr_combine_m == T){
      data4dygraph <- {
        if(input$prop_m) most_common_country_prop_dt() else most_common_country_dt()
      } %>%
        dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_m,"`"))) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_m,"`)")) %>%
        dplyr::rename(date=Date) %>%
        fill_out(start_date = input$date_most_common[1], end_date = input$date_most_common[2])
      names(data4dygraph)[2] <- paste0(isolate(cntr_reactive$selected_metric_m))
    } else{
      if (length(selected_country_m())>1){
        data4dygraph <- {
          if(input$prop_m) most_common_country_prop_dt() else most_common_country_dt()
        } %>%
          dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_m,"`"))) %>%
          tidyr::spread_(., key_col="Country", value_col=cntr_reactive$selected_metric_m, fill=0) %>%
          dplyr::rename(date=Date) %>%
          fill_out(start_date = input$date_most_common[1], end_date = input$date_most_common[2])
      } else{
        data4dygraph <- {
          if(input$prop_m) most_common_country_prop_dt() else most_common_country_dt()
        } %>%
          dplyr::select_(.dots=c("Date", paste0("`",cntr_reactive$selected_metric_m,"`"))) %>%
          dplyr::rename(date=Date) %>%
          fill_out(start_date = input$date_most_common[1], end_date = input$date_most_common[2])
        names(data4dygraph)[2] <- paste0(isolate(selected_country_m()))
      }
    }
    data4dygraph[is.na(data4dygraph)] <- 0
    data4dygraph %>%
    {
      tryCatch(polloi::smoother(., smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_cntr_m)),
               error = function(e) {
                 stop("Cannot apply spline smoothing on one or more of the selected countries.")
               }, finally = NULL)
    } %>%
      polloi::make_dygraph(xlab = "Date",
                           ylab = ifelse(input$prop_m, paste0("Proportion of ", cntr_reactive$selected_metric_m, " (%)"), cntr_reactive$selected_metric_m),
                           title = paste0(ifelse(input$prop_m, "Out of all countries, the proportion of visits whose most common section clicked on is ", "The number of visits whose most common section clicked on is "), cntr_reactive$selected_metric_m, " from ", ifelse(input$cntr_sort_m %in% c("all_m","all_nus_m"), ifelse(input$cntr_sort_m=="all_m","All Countries","All Countries but U.S."), paste0(selected_country_m(), collapse=", ")))) %>%
      dyRangeSelector(fillColor = "gray",
                      strokeColor = "white",
                      retainDateWindow = TRUE) %>%
      dyOptions(strokeWidth = 3, labelsKMB = TRUE, drawPoints = FALSE, pointSize = 3, includeZero = TRUE,
                titleHeight = 60, logscale = input$cntr_logscale_m) %>%
      dyAxis("x", rangePad = 8) %>%
      dyLegend(width = 400, labelsDiv = "cntr_m_legend", show = "always", showZeroValues = FALSE)  %>%
      dyCSS(css = "www/inverse.css") %>%
      dyEvent(as.Date("2016-09-13"), "Schema Switch", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
  })
  output$most_common_pie_pl <- renderHighchart({
    data4pie <- most_common_country_dt() %>%
      dplyr::group_by(Region) %>%
      dplyr::mutate_(region_total=paste0("sum(`",cntr_reactive$selected_metric_m,"`)")) %>%
      dplyr::group_by(Region, Country) %>%
      dplyr::mutate_(country_total=paste0("sum(`",cntr_reactive$selected_metric_m,"`)")) %>%
      dplyr::select(Region, Country, country_total, region_total) %>%
      unique() %>% dplyr::ungroup() %>% dplyr::arrange(Region, Country)
    data4pie_us <- most_common_us %>%
      dplyr::filter(most_common_us$Date >= input$date_most_common[1]
             & most_common_us$Date <= input$date_most_common[2]) %>%
      dplyr::group_by(Country) %>%
      dplyr::summarize_(us_total=paste0("sum(`",cntr_reactive$selected_metric_m,"`)")) %>% dplyr::ungroup()
    hc <- highchart() %>%
      hc_chart(type = "pie", plotBackgroundColor=NULL, plotBorderWidth=NULL, plotShadow=F) %>%
      hc_title(text = paste0("Number of ", cntr_reactive$selected_metric_m, " by Country"),
               style = list(color = "#ECF0F5")) %>%
      hc_tooltip(pointFormat = '{point.y} ({point.percentage:.1f}%)') %>%
      hc_plotOptions(
        pie = list(
          shadow = FALSE,
          center = c("50%", "50%"),
          dataLabels = list(
            style = list(
              color = '#ffffff',
              fontSize = '12px'
            )
          )
        )
      ) %>%
      hc_add_series(data = data4pie %>% dplyr::select(name=Region, y=region_total) %>%
                      dplyr::mutate(drilldown=tolower(name)) %>% unique() %>% list_parse(),
                    size = '60%',
                    name = "All Continents",
                    dataLabels = list(distance = -60,
                                      color = 'black',
                                      formatter = JS("function () {
                                                     return this.percentage > 5 ? this.point.name : null;
  }"))
                 ) %>%
      hc_add_series(data = data4pie %>% dplyr::select(name=Country, y=country_total) %>%
                      dplyr::mutate(drilldown=ifelse(name=="United States","united states", NA)) %>% unique() %>% list_parse(),
                    name = "All Countries", size = '100%', innerSize = "60%") %>%
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = data4pie %>% dplyr::mutate(id=tolower(Region)) %>%
          dplyr::select(name=Country, y=country_total, id) %>%
          unique() %>%
          split(.$id) %>%
          lapply(function(x){
            list(id= as.character(unique(x[,3])),
                 data=list_parse(x[,-3]))
          }) %>%
          magrittr::set_names(NULL) %>%
          append(list(list(
            id="united states",
            data=list_parse2(data4pie_us)
          ))),
        activeDataLabelStyle = list(
          color = '#1E90FF',
          fontSize = '12px',
          fontWeight = 'bold',
          textShadow = 'none'
        )
      )
    hc
  })
  output$most_common_by_country_tbl <- DT::renderDataTable(
    {if(input$prop_m) most_common_country_prop_dt() else most_common_country_dt()},
    options = list(lengthMenu = list(c(15, 30, 60), c('15', '30', '60')), pageLength = 15)
  )
  # download the filtered data
  output$most_common_dl = downloadHandler('most_common_filtered.csv', content = function(file) {
    s <- input$most_common_by_country_tbl_rows_all
    if(input$prop_m){
      fnl_dt <- most_common_country_prop_dt()
    } else{
      fnl_dt <- most_common_country_dt()
    }
    write.csv(fnl_dt[s, , drop = FALSE], file, row.names=FALSE)
  })
})
