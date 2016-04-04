library(polloi)
library(data.table)
library(reshape2)
library(magrittr)

source("extras.R")

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

read_useragents <- function(){
  data <- polloi::read_dataset(path = "portal/user_agent_data.tsv", col_types = "Dccd")
  data$browser[data$browser == "Chrome Mobile"] <- "Chrome Mobile (Android)"
  data$browser[data$browser == "Chrome Mobile iOS"] <- "Chrome Mobile (iOS)"
  data$browser[data$browser == "Mobile Safari"] <- "Safari Mobile"
  data$version <- paste(data$browser, data$version)
  # Add the extra browser_support_matrix information:
  data$major_version <- suppressWarnings(as.numeric(sub(".*\\s([0-9]+)", "\\1", data$version)))
  data$support <- suppressWarnings(apply(data[, c("browser", "major_version")], 1, function(row) {
    return(browser_support_matrix$support[min(which(browser_support_matrix$browser %in% row['browser'] & browser_support_matrix$version <= as.numeric(row['major_version'])))])
  }))
  data$support[is.na(data$support)] <- "Unknown"
  data <- as.data.table(data)
  data <- data[order(data$date, data$version),,]
  ua_data <<- data
  interim <- data[, list(percent = sum(percent)), by = c("date", "browser")]
  # calculate approximate browser family growth rates
  browser_rates <<- interim[, list(rate = get_exp_rate(date, percent),
                                   last = tail(percent, 1),
                                   times = length(percent)),
                              by = "browser"]
  # same as above but for versions
  version_rates <<- as.data.frame(data[, list(rate = get_exp_rate(date, percent),
                                              last = tail(percent, 1),
                                              times = length(percent)),
                                         by = "version"])
  # now calculate the growth rates for the categories of browsers as grouped by the
  #   browser_support_matrix in extras.R, which requires a few additional steps...
  support_rates <<- data[, list(rate = get_exp_rate(date, percent),
                                last = tail(percent, 1),
                                times = length(percent)),
                           by = "support"]
}

read_pageviews <- function(){
  pageview_data <<- polloi::read_dataset(path = "portal/portal_pageviews.tsv")
}

read_referrals <- function(){
  
  # Read in the initial data
  data <- as.data.table(polloi::read_dataset(path = "portal/portal_referer_data.tsv"))
  
  # Format
  data$is_search <- ifelse(data$is_search, "Referred by search", "Not referred by search")
  data$search_engine[data$search_engine == "none"] <- "Not referred by search"
  
  
  # Write out the overall values for traffic
  interim <- data[, j = list(pageviews = sum(pageviews)),
                    by = c("date", "referer_class")] %>%
    reshape2::dcast(formula = date ~ referer_class, fun.aggregate = sum)
  interim$total <- apply(interim[, -1], 1, sum)
  interim$none <- round(100*interim$none/interim$total, 2)
  interim$unknown <- round(100*interim$unknown/interim$total, 2)
  interim$`external (search engine)` <- round(100*interim$`external (search engine)`/interim$total, 2)
  interim$external <- round(100*interim$external/interim$total, 2)
  interim$internal <- round(100*interim$internal/interim$total, 2)
  interim$`internal+external+unknown` <- round(100*interim$`internal+external+unknown`/interim$total, 2)
  names(interim) <- c("date",
                      "Referred by something other than search engine",
                      "Referred by a search engine",
                      "Referred internally (itself or a sister wiki)",
                      "Referred internally, non-search-engine, and unknown",
                      "Direct (not referred by anything)",
                      "Unknown referers",
                      "Total")
  summary_traffic_data <<- interim[, 1:7]
  
  # Generate per-engine values
  interim <- data[data$search_engine != "Not referred by search",
                  j = list(pageviews = sum(pageviews)),
                  by = c("date", "search_engine")] %>%
    reshape2::dcast(formula = date ~ search_engine, fun.aggregate = sum)
  bysearch_traffic_data <<- cbind(date = interim$date, data.frame(round(100*t(apply(interim[, -1], 1, function(x) { x/sum(x) })), 2)))
  
  return(invisible())
  
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
