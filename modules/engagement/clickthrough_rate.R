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
