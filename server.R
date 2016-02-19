source("functions.R")
options(scipen = 500)

existing_date <- Sys.Date() - 1

shinyServer(function(input, output, session){
  
  if(Sys.Date() != existing_date) {
    read_clickthrough()
    read_dwelltime()
    read_country()
		read_useragents()
		read_pageviews()
    existing_date <<- Sys.Date()
  }
  
  shinyURL.server(session)
  
  output$clickthrough_rate_dygraph <- renderDygraph({
    clickthrough_rate %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_clickthrough_rate)) %>%
      polloi::make_dygraph(xlab = "Date", ylab = "Clickthrough rate (%)", title = "Wikipedia portal clickthrough rate") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyAnnotation(as.Date("2015-12-07"), text = "A",
                   tooltip = "Sampling change - see below",
                   width = 12, height = 20, attachAtBottom = FALSE)
  })
  
  output$action_breakdown_dygraph <- renderDygraph({
    action_breakdown %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_action_breakdown)) %>%
      polloi::make_dygraph(xlab = "Date", ylab = "Actions (%)", title = "Actions on the Wikipedia portal") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyLegend(labelsDiv = "action_breakdown_legend", show = "always", width = 400) %>%
      dyAnnotation(as.Date("2015-12-07"), text = "A",
                   tooltip = "Sampling change - see below",
                   width = 12, height = 20, attachAtBottom = FALSE)
  })
  
  output$dwelltime_dygraph <- renderDygraph({
    dwelltime_data %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_dwelltime)) %>%
      polloi::make_dygraph(xlab = "Date", ylab = "Dwell Time (Seconds)", title = "Time spent on the Wikipedia portal") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyAnnotation(as.Date("2015-12-07"), text = "A",
                   tooltip = "Sampling change - see below",
                   width = 12, height = 20, attachAtBottom = FALSE)
  })
  
  output$country_breakdown_dygraph <- renderDygraph({
    country_data %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_country_breakdown)) %>%
      polloi::make_dygraph(xlab = "", ylab = "Users (%)", title = "Geographic breakdown of portal visitors") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyLegend(labelsDiv = "country_breakdown_legend", show = "always", width = 400)
  })
  
  output$browser_selector_container <- renderUI({
    browsers <- switch(input$browser_order,
                       "alphabet" = {
                         sort(browser_rates$browser)
                       },
                       "growth" = {
                         browser_rates$browser[order(browser_rates$rate, decreasing = TRUE)]
                       },
                       "decay" = {
                         browser_rates$browser[order(browser_rates$rate, decreasing = FALSE)]
                       },
                       "last" = {
                         browser_rates$browser[order(browser_rates$last, decreasing = TRUE)]
                       },
                       "times" = {
                         browser_rates$browser[order(browser_rates$times, decreasing = TRUE)]
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
    ua_data[ua_data$browser %in% input$browser_selector, , ] %>%
      reshape2::dcast(date ~ browser, fun.aggregate = sum) %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_browser_breakdown)) %>%
      polloi::make_dygraph(xlab = "Date", ylab = "Share (%)", title = "Browser breakdown of portal visitors") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyLegend(labelsDiv = "browser_breakdown_legend", show = "always", width = 400)
  })
  
  output$pageview_dygraph <- renderDygraph({
    pageview_data %>%
      polloi::smoother(smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_pageviews)) %>%
      polloi::make_dygraph(xlab = "Date", ylab = "Pageviews", title = "Pageviews to the Wikipedia Portal") %>%
      dyCSS(css = "www/inverse.css") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70)
  })
})
