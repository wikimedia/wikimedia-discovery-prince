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
