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
