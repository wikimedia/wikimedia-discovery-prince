library(polloi)
library(data.table)
library(reshape2)
library(magrittr)

# Read in the traffic data
read_clickthrough <- function(){
  
  # Read in and format the high-level data
  data <- as.data.table(polloi::read_dataset(path = "portal/clickthrough_rate.tsv"))
  clickthrough_rate <<- data[, j = list(success_rate = (events[type=="clickthrough"]/sum(events))*100), by = "date"]
  
  # Read in and format the breakdown data
  data <- as.data.table(polloi::read_dataset(path = "portal/clickthrough_breakdown.tsv"))
  data <- data[,j=list(section_used = section_used, proportion = (events/sum(events))*100), by = "date"]
  action_breakdown <<- reshape2::dcast(data, formula = date ~ section_used, fun.aggregate = sum)
  
  return(invisible())
}

read_dwelltime <- function(){
  dwelltime_data <<- as.data.table(polloi::read_dataset(path = "portal/dwell_metrics.tsv"))
}

read_country <- function(){
  data <- as.data.table(polloi::read_dataset(path = "portal/country_data.tsv"))
  
  country_data <<- reshape2::dcast(data[,list(country = country, events = round(events/sum(events)*100,2)),
                                        by = "date"],
                                   formula = date ~ country, fun.aggregate = sum)
}

# Fits an exponential model to the data and returns the rate of growth (or decay!)
get_exp_rate <- function(dates, y) {
  time_frame <- range(dates)
  days <- as.character(seq(time_frame[1], time_frame[2], "day"))
  if (length(days) < 3) {
    return(as.double(NA))
  } else {
    x <- as.numeric(as.character(factor(as.character(dates), days, 1:length(days))))
    return(tryCatch({
      fit <- nls(y ~ exp(a + b * x), start = list(a = 0, b = 0))
      round(as.double(fit$m$getPars()['b']), 4)
    }, error = function(e) {}, finally = as.double(NA)))
  }
  # plot(x, y, type = "l")
  # lines(x, predict(fit, list(x = x)), lty = "dashed", col = "blue")
}

read_useragents <- function(){
  data <- as.data.table(polloi::read_dataset(path = "portal/user_agent_data.tsv"))
  data$browser <- paste(data$browser, data$version)
  data$version <- NULL
  data <- data[order(data$date, data$browser),,]
  ua_data <<- data
  browser_rates <<- data[, list(rate = get_exp_rate(date, percent),
                                last = tail(percent, 1),
                                times = length(percent)),
                           by = "browser"]
}
