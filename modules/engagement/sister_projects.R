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
