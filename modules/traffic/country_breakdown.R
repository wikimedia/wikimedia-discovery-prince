output$country_breakdown_dygraph <- renderDygraph({
  if (input$group_us_regions) {
    temp <- country_data
    temp$`United States` <- rowSums(temp[, grepl("(United States)|(U\\.S\\. )", colnames(temp)), drop = FALSE], na.rm = TRUE)
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
