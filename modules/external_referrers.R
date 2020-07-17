output$referer_summary_dygraph <- renderDygraph({
  dplyr::select(summary_traffic_data, -Total) %>%
    polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_referer_summary)) %>%
    polloi::make_dygraph(xlab = "Date", ylab = "% of Pageviews",
                         title = "Traffic to Wikipedia Portal brown down by origin") %>%
    dyCSS(css = "www/inverse.css") %>%
    dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
    dyAxis("y", valueFormatter = 'function(x) { return Math.round(x * 100)/100 + "%"; }') %>%
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
    dyAxis("y", valueFormatter = 'function(x) { return Math.round(x * 100)/100 + "%"; }') %>%
    dyLegend(labelsDiv = "search_engines_legend", show = "always") %>%
    dyRangeSelector(fillColor = "", strokeColor = "", retainDateWindow = TRUE) %>%
    dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
})
