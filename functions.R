library(polloi)
library(ggplot2)
library(data.table)
library(reshape2)
library(magrittr)
library(toOrdinal)
library(xts)
library(dplyr)
library(tidyr)

source("extras.R")

# Capitalize the first letter
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# Read in the traffic data
read_clickthrough <- function(){

  # Read in and format the high-level data
  data <- as.data.table(polloi::read_dataset(path = "portal/clickthrough_rate.tsv"))
  clickthrough_rate <<- data[, j = list(`clickthrough rate` = (events[type == "clickthrough"]/sum(events))*100), by = "date"]

  # Read in and format the breakdown data
  data <- as.data.table(polloi::read_dataset(path = "portal/clickthrough_breakdown.tsv"))
  data <- data[, j = list(section_used = section_used, proportion = (events/sum(events))*100), by = "date"]
  action_breakdown <<- reshape2::dcast(data, formula = date ~ section_used, fun.aggregate = sum)

  # Read in most common section per visit data
  data <- as.data.table(polloi::read_dataset(path = "portal/most_common_per_visit.tsv"))
  data <- data[, j = list(section_used = section_used, proportion = (visits/sum(visits))*100), by = "date"]
  most_common <<- reshape2::dcast(data, formula = date ~ section_used, fun.aggregate = sum)

  # Read in first visit clickthrough rates
  data <- polloi::read_dataset(path = "portal/clickthrough_firstvisit.tsv")
  data[, -1] <- data[, -1]*100 # first column is always going to be the date
  data$`language search` <- 0
  first_visit_ctrs <<- as.data.frame(data[, names(action_breakdown)])

  return(invisible())
}

read_langs <- function() {
  data <- as.data.table(read_dataset(path = "portal/language_destination.tsv"))
  langs_visited <<- as.data.table(dplyr::left_join(data, get_prefixes()[, -2], by = "prefix"))
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
  pageview_data <<- polloi::read_dataset(path = "portal/portal_pageviews.tsv", col_types = "Diii-") %>%
    set_names(c("date", "total pageviews", "high-volume clients' PVs", "low-volume clients' PVs"))
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

read_geo <- function() {

  all_country_data <- polloi::read_dataset("portal/all_country_data.tsv", col_types = "Dcididid")
  first_visits_country <- polloi::read_dataset("portal/first_visits_country.tsv", col_types = "Dccid")
  last_action_country <- polloi::read_dataset("portal/last_action_country.tsv", col_types = "Dccid")
  most_common_country <- polloi::read_dataset("portal/most_common_country.tsv", col_types = "Dccid")
  all_country_data$country[all_country_data$country=="Cape Verde"] <- "Cabo Verde"
  first_visits_country$country[first_visits_country$country=="Cape Verde"] <- "Cabo Verde"
  last_action_country$country[last_action_country$country=="Cape Verde"] <- "Cabo Verde"
  most_common_country$country[most_common_country$country=="Cape Verde"] <- "Cabo Verde"
  all_country_data$country[all_country_data$country=="Czechia"] <- "Czech Republic"
  first_visits_country$country[first_visits_country$country=="Czechia"] <- "Czech Republic"
  last_action_country$country[last_action_country$country=="Czechia"] <- "Czech Republic"
  most_common_country$country[most_common_country$country=="Czechia"] <- "Czech Republic"
  data("countrycode_data", package="countrycode")
  countrycode_data$country.name[c(54,143)] <- c("Congo, The Democratic Republic of the", "Macedonia, Republic of" )
  countrycode_data$continent[countrycode_data$country.name %in% c("British Indian Ocean Territory","Christmas Island","Taiwan, Province of China")] <- "Asia"
  countrycode_data$continent[countrycode_data$region == "South America"] <- "South America"
  countrycode_data$continent[countrycode_data$continent == "Americas"] <- "North America"


  all_country_data <- all_country_data[!duplicated(all_country_data[,1:2],fromLast=T),]
  all_country_data_prop <- all_country_data %>%
    group_by(date) %>%
    mutate(event_prop=round(events/sum(events),4)*100, visit_prop=round(n_visit/sum(n_visit),4)*100, session_prop=round(n_session/sum(n_session),4)*100) %>%
    select(date,country,event_prop,ctr,visit_prop,ctr_visit,session_prop,ctr_session) %>% ungroup()
  us_mask <- grepl("^U\\.S\\.", all_country_data$country)
  us_data <- all_country_data[us_mask,]
  all_country_data <- us_data %>%
    mutate(clicks = events*ctr, click_v=n_visit*ctr_visit, click_s=n_session*ctr_session) %>%
    group_by(date) %>%
    summarise(country="United States", events=sum(events), ctr=round(sum(clicks)/sum(events),4),
              n_visit=sum(n_visit), ctr_visit=round(sum(click_v)/sum(n_visit),4),
              n_session=sum(n_session), ctr_session=round(sum(click_s)/sum(n_session),4)) %>%
    rbind(all_country_data[!us_mask,]) %>%
    arrange(date, country)
  us_mask <- grepl("^U\\.S\\.", all_country_data_prop$country)
  us_data_prop <- all_country_data_prop[us_mask,]
  all_country_data_prop <- us_data_prop %>%
    group_by(date) %>%
    summarise(country="United States", event_prop=sum(event_prop),
              visit_prop=sum(visit_prop), session_prop=sum(session_prop)) %>%
    left_join(all_country_data[, c("date","country","ctr","ctr_visit","ctr_session")], by=c("date","country")) %>%
    rbind(all_country_data_prop[!us_mask,]) %>%
    select(date,country,event_prop,ctr,visit_prop,ctr_visit,session_prop,ctr_session) %>%
    arrange(date, country)
  colnames(all_country_data) <- c("Date", "Country", "Number of Events",
                                  "Overall Clickthrough Rate", "Number of Visits", "Clickthrough Rate Per Visit",
                                  "Number of Sessions", "Clickthrough Rate Per Session")
  colnames(all_country_data_prop) <- c("Date", "Country", "Number of Events",
                                       "Overall Clickthrough Rate", "Number of Visits", "Clickthrough Rate Per Visit",
                                       "Number of Sessions", "Clickthrough Rate Per Session")
  colnames(us_data) <- c("Date", "Country", "Number of Events",
                         "Overall Clickthrough Rate", "Number of Visits", "Clickthrough Rate Per Visit",
                         "Number of Sessions", "Clickthrough Rate Per Session")
  colnames(us_data_prop) <- c("Date", "Country", "Number of Events",
                              "Overall Clickthrough Rate", "Number of Visits", "Clickthrough Rate Per Visit",
                              "Number of Sessions", "Clickthrough Rate Per Session")
  region_mask <- match(stringi::stri_trans_general(all_country_data$Country, "Latin-ASCII"), countrycode_data$country.name)
  all_country_data$Region <- countrycode_data$continent[region_mask]
  all_country_data$Region[is.na(all_country_data$Region)] <- "Other"
  region_mask <- match(stringi::stri_trans_general(all_country_data_prop$Country, "Latin-ASCII"), countrycode_data$country.name)
  all_country_data_prop$Region <- countrycode_data$continent[region_mask]
  all_country_data_prop$Region[is.na(all_country_data_prop$Region)] <- "Other"
  all_country_data <<- all_country_data[, c(1:2, 9, 3:8)] %>% arrange(Date, Country)
  all_country_data_prop <<- all_country_data_prop[, c(1:2, 9, 3:8)] %>% arrange(Date, Country)
  us_data <<- us_data %>% mutate(Region="North America") %>% select(c(1:2, 9, 3:8)) %>% arrange(Date, Country)
  us_data_prop <<- us_data_prop %>% mutate(Region="North America") %>% select(c(1:2, 9, 3:8)) %>% arrange(Date, Country)


  first_visits_country <- first_visits_country[!duplicated(first_visits_country[,1:3],fromLast=T),]
  colnames(first_visits_country) <- c("Date", "Action", "Country", "Number of Sessions", "Proportion")
  first_visits_country$Proportion <- first_visits_country$Proportion*100
  first_visits_country_prop <- tidyr::spread(first_visits_country[,-4], key=Action, value=Proportion, fill=0)
  first_visits_country <- tidyr::spread(first_visits_country[,-5], key=Action, value=`Number of Sessions`, fill=0)
  colnames(first_visits_country_prop) <- sapply(colnames(first_visits_country_prop), simpleCap)
  colnames(first_visits_country) <- sapply(colnames(first_visits_country), simpleCap)
  us_mask <- grepl("^U\\.S\\.", first_visits_country$Country)
  first_visits_us <- first_visits_country[us_mask, ]
  first_visits_country <- first_visits_us %>% select(-Country) %>% group_by(Date) %>%
    summarise_each(funs(sum)) %>% mutate(Country="United States") %>%
    rbind(first_visits_country[!us_mask,])
  us_mask <- grepl("^U\\.S\\.", first_visits_country_prop$Country)
  first_visits_us_prop <- first_visits_country_prop[us_mask, ]
  first_visits_country_prop <- first_visits_us_prop %>% select(-Country) %>% group_by(Date) %>%
    summarise_each(funs(sum)) %>% mutate(Country="United States") %>%
    rbind(first_visits_country_prop[!us_mask,])
  region_mask <- match(stringi::stri_trans_general(first_visits_country$Country, "Latin-ASCII"), countrycode_data$country.name)
  first_visits_country$Region <- countrycode_data$continent[region_mask]
  first_visits_country$Region[is.na(first_visits_country$Region)] <- "Other"
  region_mask <- match(stringi::stri_trans_general(first_visits_country_prop$Country, "Latin-ASCII"), countrycode_data$country.name)
  first_visits_country_prop$Region <- countrycode_data$continent[region_mask]
  first_visits_country_prop$Region[is.na(first_visits_country_prop$Region)] <- "Other"
  first_visits_country <<- first_visits_country[, c(1, 8:9, 2:7)] %>% arrange(Date, Country)
  first_visits_country_prop <<- first_visits_country_prop[, c(1, 8:9, 2:7)] %>% arrange(Date, Country)
  first_visits_us <<- first_visits_us %>% mutate(Region="North America") %>% select(c(1:2, 9, 3:8)) %>% arrange(Date, Country)
  first_visits_us_prop <<- first_visits_us_prop %>% mutate(Region="North America") %>% select(c(1:2, 9, 3:8)) %>% arrange(Date, Country)


  last_action_country <- last_action_country[!duplicated(last_action_country[,1:3],fromLast=T),]
  colnames(last_action_country) <- c("Date", "Action", "Country", "Events", "Proportion")
  last_action_country$Proportion <- last_action_country$Proportion*100
  last_action_country_prop <- tidyr::spread(last_action_country[,-4], key=Action, value=Proportion, fill=0)
  last_action_country <- tidyr::spread(last_action_country[,-5], key=Action, value=Events, fill=0)
  colnames(last_action_country_prop) <- sapply(colnames(last_action_country_prop), simpleCap)
  colnames(last_action_country) <- sapply(colnames(last_action_country), simpleCap)
  us_mask <- grepl("^U\\.S\\.", last_action_country$Country)
  last_action_us <- last_action_country[us_mask, ]
  last_action_country <- last_action_us %>% select(-Country) %>% group_by(Date) %>%
    summarise_each(funs(sum)) %>% mutate(Country="United States") %>%
    rbind(last_action_country[!us_mask,])
  us_mask <- grepl("^U\\.S\\.", last_action_country_prop$Country)
  last_action_us_prop <- last_action_country_prop[us_mask, ]
  last_action_country_prop <- last_action_us_prop %>% select(-Country) %>% group_by(Date) %>%
    summarise_each(funs(sum)) %>% mutate(Country="United States") %>%
    rbind(last_action_country_prop[!us_mask,])
  region_mask <- match(stringi::stri_trans_general(last_action_country$Country, "Latin-ASCII"), countrycode_data$country.name)
  last_action_country$Region <- countrycode_data$continent[region_mask]
  last_action_country$Region[is.na(last_action_country$Region)] <- "Other"
  region_mask <- match(stringi::stri_trans_general(last_action_country_prop$Country, "Latin-ASCII"), countrycode_data$country.name)
  last_action_country_prop$Region <- countrycode_data$continent[region_mask]
  last_action_country_prop$Region[is.na(last_action_country_prop$Region)] <- "Other"
  last_action_country <<- last_action_country[, c(1, 8:9, 2:7)] %>% arrange(Date, Country)
  last_action_country_prop <<- last_action_country_prop[, c(1, 8:9, 2:7)] %>% arrange(Date, Country)
  last_action_us <<- last_action_us %>% mutate(Region="North America") %>% select(c(1:2, 9, 3:8)) %>% arrange(Date, Country)
  last_action_us_prop <<- last_action_us_prop %>% mutate(Region="North America") %>% select(c(1:2, 9, 3:8)) %>% arrange(Date, Country)


  most_common_country <- most_common_country[!duplicated(most_common_country[,1:3],fromLast=T),]
  colnames(most_common_country) <- c("Date", "Action", "Country", "Number of Visits", "Proportion")
  most_common_country$Proportion <- most_common_country$Proportion*100
  most_common_country_prop <- tidyr::spread(most_common_country[,-4], key=Action, value=Proportion, fill=0)
  most_common_country <- tidyr::spread(most_common_country[,-5], key=Action, value=`Number of Visits`, fill=0)
  colnames(most_common_country_prop) <- sapply(colnames(most_common_country_prop), simpleCap)
  colnames(most_common_country) <- sapply(colnames(most_common_country), simpleCap)
  us_mask <- grepl("^U\\.S\\.", most_common_country$Country)
  most_common_us <- most_common_country[us_mask, ]
  most_common_country <- most_common_us %>% select(-Country) %>% group_by(Date) %>%
    summarise_each(funs(sum)) %>% mutate(Country="United States") %>%
    rbind(most_common_country[!us_mask,])
  us_mask <- grepl("^U\\.S\\.", most_common_country_prop$Country)
  most_common_us_prop <- most_common_country_prop[us_mask, ]
  most_common_country_prop <- most_common_us_prop %>% select(-Country) %>% group_by(Date) %>%
    summarise_each(funs(sum)) %>% mutate(Country="United States") %>%
    rbind(most_common_country_prop[!us_mask,])
  region_mask <- match(stringi::stri_trans_general(most_common_country$Country, "Latin-ASCII"), countrycode_data$country.name)
  most_common_country$Region <- countrycode_data$continent[region_mask]
  most_common_country$Region[is.na(most_common_country$Region)] <- "Other"
  region_mask <- match(stringi::stri_trans_general(most_common_country_prop$Country, "Latin-ASCII"), countrycode_data$country.name)
  most_common_country_prop$Region <- countrycode_data$continent[region_mask]
  most_common_country_prop$Region[is.na(most_common_country_prop$Region)] <- "Other"
  most_common_country <<- most_common_country[, c(1, 7:8, 2:6)] %>% arrange(Date, Country)
  most_common_country_prop <<- most_common_country_prop[, c(1, 7:8, 2:6)] %>% arrange(Date, Country)
  most_common_us <<- most_common_us %>% mutate(Region="North America") %>% select(c(1:2, 8, 3:7)) %>% arrange(Date, Country)
  most_common_us_prop <<- most_common_us_prop %>% mutate(Region="North America") %>% select(c(1:2, 8, 3:7)) %>% arrange(Date, Country)
}

fill_out <- function(x, start_date, end_date, fill = 0) {
  temp <- data.frame(date = seq(start_date, end_date, "day"))
  y <- dplyr::right_join(x, temp, by = "date")
  y[is.na(y)] <- fill
  return(y)
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
