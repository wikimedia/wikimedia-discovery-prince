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
