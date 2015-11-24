source("functions.R")
options(scipen = 500)

existing_date <- Sys.Date() - 1

shinyServer(function(input, output){
  
  if (Sys.Date() != existing_date) {
    read_clickthrough()
    existing_date <<- Sys.Date()
  }
  
  output$clickthrough_rate_dygraph <- renderDygraph({
    polloi::make_dygraph(
      data = clickthrough_rate,
      xlab = "Date", ylab = "Clickthrough rate (%)", title = "Wikipedia portal clickthrough rate")
  })
  
  output$action_breakdown_dygraph <- renderDygraph({
    polloi::make_dygraph(
      data = action_breakdown,
      xlab = "Date", ylab = "Actions (%)", title = "Actions on the Wikipedia portal")
  })
})
