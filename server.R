library(shiny)
library(shinydashboard)
library(dygraphs)
library(shinyURL)

source("functions.R")
options(scipen = 500)

existing_date <- Sys.Date() - 1

shinyServer(function(input, output, session){
  
  if (Sys.Date() != existing_date) {
    read_clickthrough()
    read_langs()
    read_dwelltime()
    read_country()
    read_useragents()
    read_pageviews()
    read_referrals()
    existing_date <<- Sys.Date()
  }
  
  shinyURL.server(session)
  
  output$clickthrough_rate_dygraph <- renderDygraph({
    clickthrough_rate %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_clickthrough_rate)) %>%
      polloi::make_dygraph(xlab = "Date", ylab = "Clickthrough rate (%)",
                           title = "Wikipedia portal clickthrough rate") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyLegend(labelsDiv = "clickthrough_rate_legend", show = "always") %>%
      dyRangeSelector(strokeColor = "white", fillColor = "gray", retainDateWindow = TRUE) %>%
      dyEvent(as.Date("2015-12-07"), "A (sampling change)", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-03-10"), "Search Box Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-18"), "Sister Links Updated", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white")
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
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white")
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
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white")
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
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white")
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
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white")
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
      {
        if (ncol(.) > 13) { # Maximum colors that color brewer supports is 12
          # If we have more than 12 countries, we need to make the dygraph manually:
          . <- xts::xts(.[, -1], order.by = .$date)
          dg <- dygraph(., xlab = "Date", ylab = "Users (%)", main = "Geographic breakdown of portal visitors")
          dg <- dyOptions(dg, strokeWidth = 3, colors = colorspace::rainbow_hcl(ncol(temp)-1),
                          drawPoints = FALSE, pointSize = 3, labelsKMB = TRUE, includeZero = TRUE)
          dg
        } else {
          polloi::make_dygraph(., xlab = "Date", ylab = "Users (%)", title = "Geographic breakdown of portal visitors")
        }
      } %>%
      dyLegend(labelsDiv = "country_breakdown_legend", show = "always", width = 400) %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyCSS(css = "www/inverse.css") %>%
      dyRangeSelector(strokeColor = "white", fillColor = "gray", retainDateWindow = TRUE) %>%
      dyEvent(as.Date("2016-03-10"), "Search Box Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-18"), "Sister Links Updated", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-28"), "A (regional U.S.)", labelLoc = "bottom", color = "white")
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
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white")
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
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white")
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
      dyEvent(as.Date("2016-05-01"), "B (search-redirect.php)", labelLoc = "bottom", color = "white")
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
      dyRangeSelector(fillColor = "", strokeColor = "", retainDateWindow = TRUE)
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
      dyLegend(width = 400, labelsDiv = "s_legend", show = "always") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyEvent(as.Date("2016-03-10"), "Search Box Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-18"), "Sister Links Updated", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white")
  })
  
  lv_reactive <- reactiveValues(choices = NULL, selected_langs = NULL)
  
  observeEvent(input$lv_selectall, {
    lv_reactive$selected_langs <- lv_reactive$choices
  })
  
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
                                      dplyr::arrange(languages[languages[[input$lv_response]] %in% head(sort(languages[[input$lv_response]], decreasing = TRUE), 10), ], desc(clicks))$language
                                    },
                                    bottom50 = {
                                      dplyr::arrange(languages[languages[[input$lv_response]] %in% head(sort(languages[[input$lv_response]], decreasing = FALSE), 50), ], desc(clicks))$language
                                    })
    }
    if (!is.null(input$lv_languages)) {
      if (sum(input$lv_languages %in% lv_reactive$choices) == 0) {
        if (input$lv_sort == "top10") {
          lv_reactive$selected_langs <- lv_reactive$choices
        } else {
          lv_reactive$selected_langs <- lv_reactive$choices[1]
        }
      } else {
        # Lets us keep selected languages when switching betweeen clicks and users
        lv_reactive$selected_langs <- intersect(input$lv_languages, lv_reactive$choices)
      }
    } else {
      if (input$lv_sort == "top10") {
        lv_reactive$selected_langs <- lv_reactive$choices
      } else {
        lv_reactive$selected_langs <- lv_reactive$choices[1]
      }
    }
  })
  
  output$lv_languages_container <- renderUI({
    selectizeInput("lv_languages", "Wikipedia languages", lv_reactive$choices, lv_reactive$selected_langs, multiple = TRUE, options = list(maxItems = 12))
  })
  
  observeEvent(input$lv_languages, {
    lv_reactive$selected_langs <- input$lv_languages
  })
  
  output$lv_dygraph <- renderDygraph({
    if (input$lv_response == "clicks") {
      if (length(input$lv_languages) > 1) {
        data4dygraph <- langs_visited[langs_visited$language %in% input$lv_languages, c("date", "language", "clicks"), with = FALSE] %>%
          tidyr::spread(language, clicks, fill = 0) %>%
          fill_out(start_date = min(langs_visited$date), end_date = max(langs_visited$date)) %>%
          { .[, c("date", input$lv_languages)] }
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
        { .[, c("date", input$lv_languages)] }
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
      dyLegend(width = 400, labelsDiv = "lv_legend", show = "always") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyEvent(as.Date("2016-03-10"), "Search Box Deployed", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-05-18"), "Sister Links Updated", labelLoc = "bottom", color = "white") %>%
      dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white")
  })
  
})
