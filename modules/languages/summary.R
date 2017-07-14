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
