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
