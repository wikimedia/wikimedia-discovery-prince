source("functions.R")
options(scipen = 500)

existing_date <- Sys.Date() - 1

shinyServer(function(input, output){
  
  if(Sys.Date() != existing_date) {
    read_clickthrough()
    read_dwelltime()
    read_country()
    existing_date <<- Sys.Date()
  }
  
  output$clickthrough_rate_dygraph <- renderDygraph({
    polloi::make_dygraph(
      data = clickthrough_rate,
      xlab = "Date", ylab = "Clickthrough rate (%)", title = "Wikipedia portal clickthrough rate") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyAnnotation(as.Date("2015-12-07"), text = "A",
                   tooltip = "Sampling change - see below",
                   width = 12, height = 20, attachAtBottom = FALSE)
  })
  
  output$action_breakdown_dygraph <- renderDygraph({
    polloi::make_dygraph(
      data = action_breakdown,
      xlab = "Date", ylab = "Actions (%)", title = "Actions on the Wikipedia portal") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyLegend(labelsDiv = "action_breakdown_legend", show = "always", width = 400) %>%
      dyAnnotation(as.Date("2015-12-07"), text = "A",
                   tooltip = "Sampling change - see below",
                   width = 12, height = 20, attachAtBottom = FALSE)
  })
  
  output$dwelltime_dygraph <- renderDygraph({
    polloi::make_dygraph(
      data = dwelltime_data,
      xlab = "Date", ylab = "Dwell Time (Seconds)", title = "Time spent on the Wikipedia portal") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyAnnotation(as.Date("2015-12-07"), text = "A",
                   tooltip = "Sampling change - see below",
                   width = 12, height = 20, attachAtBottom = FALSE)
  })
  
  output$country_breakdown_dygraph <- renderDygraph({
    country_data %>%
      polloi::make_dygraph(xlab = "", ylab = "Users (%)", title = "Geographic breakdown of portal visitors") %>%
      dyAxis("x", axisLabelFormatter = polloi::custom_axis_formatter, axisLabelWidth = 70) %>%
      dyLegend(labelsDiv = "country_breakdown_legend", show = "always", width = 400) %>%
      dyCSS(css = "www/inverse.css")
  })
})
