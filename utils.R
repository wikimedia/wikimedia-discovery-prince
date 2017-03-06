library(magrittr)

source("extras.R")

# Read in the traffic data
read_clickthrough <- function(){
  # Read in and format the high-level data
  clickthrough_rate <<- data.table::as.data.table(
    polloi::read_dataset("discovery/portal/clickthrough_rate.tsv", col_types = "Dci")
  )[, j = list(`clickthrough rate` = 100 * (events[type == "clickthrough"]/sum(events))), by = "date"]
  # Read in and format the breakdown data
  interim <- data.table::as.data.table(
    polloi::read_dataset("discovery/portal/clickthrough_breakdown.tsv", col_types = "Dci")
  )[, j = list(section_used = section_used, proportion = 100 * (events/sum(events))), by = "date"]
  action_breakdown <<- tidyr::spread(dplyr::distinct(interim, date, section_used, .keep_all = TRUE),
                                     section_used, proportion, fill = 0)
  # Read in most common section per visit data
  interim <- data.table::as.data.table(
    polloi::read_dataset("discovery/portal/most_common_per_visit.tsv", col_types = "Dci")
  )[, j = list(section_used = section_used, proportion = 100 * (visits/sum(visits))), by = "date"]
  most_common <<- tidyr::spread(dplyr::distinct(interim, date, section_used, .keep_all = TRUE),
                                section_used, proportion, fill = 0)
  # Read in first visit clickthrough rates
  interim <- polloi::read_dataset("discovery/portal/clickthrough_firstvisit.tsv", col_types = "Ddddddd")
  interim[, -1] <- 100 * interim[, -1] # first column is always going to be the date
  interim$`language search` <- 0
  first_visit_ctrs <<- interim[, names(action_breakdown)]
  return(invisible())
}

read_langs <- function() {
  interim <- polloi::read_dataset("discovery/portal/language_destination.tsv", col_types = "Dciiiii")
  suppressMessages({
    prefixes <- polloi::get_prefixes()[, -2]
  })
  langs_visited <<- data.table::as.data.table(dplyr::filter(dplyr::left_join(interim, prefixes, by = "prefix"), !is.na(prefix) & !is.na(clicks)))
  return(invisible())
}

read_dwelltime <- function(){
  dwelltime_data <<- data.table::as.data.table(polloi::read_dataset("discovery/portal/dwell_metrics.tsv", col_types = "Dddd"))
  return(invisible())
}

read_country <- function(){
  interim <- data.table::as.data.table(
    polloi::read_dataset("discovery/portal/country_data.tsv", col_types = "Dci")
  )[, list(country = country, events = 100 * (events/sum(events))), by = "date"]
  country_data <<- tidyr::spread(
    dplyr::distinct(interim, date, country, .keep_all = TRUE),
    country, events, fill = NA
  ) %>% as.data.frame()
  return(invisible())
}

read_useragents <- function(){
  interim <- polloi::read_dataset("discovery/portal/user_agent_data.tsv", col_types = "Dccd")
  interim$browser[interim$browser == "Chrome Mobile"] <- "Chrome Mobile (Android)"
  interim$browser[interim$browser == "Chrome Mobile iOS"] <- "Chrome Mobile (iOS)"
  interim$browser[interim$browser == "Mobile Safari"] <- "Safari Mobile"
  interim$major_version <- as.numeric(dplyr::if_else(interim$version == "Other", as.character(NA), interim$version))
  interim$version <- paste(interim$browser, interim$version)
  interim$support <- apply(interim[, c("browser", "major_version")], 1, function(row) {
    if (!row['browser'] %in% browser_support_matrix$browser) {
      return("Unknown")
    } else {
      idx <- (browser_support_matrix$browser %in% row['browser']) & (browser_support_matrix$version <= row['major_version'])
      if (all(!idx)) {
        return("Unknown")
      } else {
        return(browser_support_matrix$support[min(which(idx))])
      }
    }
  })
  ua_data <<- data.table::as.data.table(interim[order(interim$date, interim$version), ])
  # calculate approximate browser family growth rates
  browser_rates <<- ua_data[, list(rate = get_exp_rate(date, percent),
                                   last = tail(percent, 1),
                                   times = length(percent)),
                              by = "browser"]
  # same as above but for versions
  version_rates <<- ua_data[, list(rate = get_exp_rate(date, percent),
                                   last = tail(percent, 1),
                                   times = length(percent)),
                            by = "version"]
  # now calculate the growth rates for the categories of browsers as grouped by the
  #   browser_support_matrix in extras.R, which requires a few additional steps...
  support_rates <<- ua_data[, list(rate = get_exp_rate(date, percent),
                                   last = tail(percent, 1),
                                   times = length(percent)),
                            by = "support"]
  return(invisible())
}

read_pageviews <- function(){
  pageview_data <<- dplyr::distinct(polloi::read_dataset(
    "discovery/portal/pageviews.tsv", col_types = "Diii-", skip = 1,
    col_names = c("date", "total pageviews", "high-volume clients' PVs", "low-volume clients' PVs")
  ), date, .keep_all = TRUE)
  return(invisible())
}

read_referrals <- function(){
  # Read in the initial data
  interim <- polloi::read_dataset("discovery/portal/referer_data.tsv", col_types = "Dlcci")
  interim$search_engine[interim$search_engine == "none"] <- "Not referred by search"
  # Write out the overall values for traffic
  summary_traffic_data <<- interim %>%
    dplyr::mutate(referer_class = forcats::fct_recode(
      referer_class,
      `Referred by something other than search engine` = "external",
      `Referred by a search engine` = "external (search engine)",
      `Referred internally (itself or a sister wiki)` = "internal",
      `Referred internally, non-search-engine, and unknown` = "internal+external+unknown",
      `Direct (not referred by anything)` = "none",
      `Unknown referrer` = "unknown"
    )) %>%
    dplyr::group_by(date, referer_class) %>%
    dplyr::summarize(pageviews = sum(pageviews)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(referer_class), !is.na(pageviews)) %>%
    dplyr::distinct(date, referer_class, .keep_all = TRUE) %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(Total = sum(pageviews), pageviews = 100 * (pageviews/Total)) %>%
    tidyr::spread(referer_class, pageviews, fill = NA)
  # Generate per-engine values
  bysearch_traffic_data <<- interim %>%
    dplyr::filter(is_search) %>%
    dplyr::distinct(date, search_engine, .keep_all = TRUE) %>%
    dplyr::group_by(date, search_engine) %>%
    dplyr::summarize(pageviews = sum(pageviews)) %>%
    dplyr::mutate(total = sum(pageviews), pageviews = 100 * (pageviews/sum(pageviews))) %>%
    dplyr::select(-total) %>%
    tidyr::spread(search_engine, pageviews, fill = NA)
  return(invisible())
}

read_sisproj <- function() {
  sisproj_clicks <<- polloi::read_dataset("discovery/portal/clickthrough_sisterprojects.tsv", col_types = "Dcii") %>%
    dplyr::filter(!grepl("(Privacy_policy|Terms_of_Use|.com|servlet)", destination), !grepl("^http", destination))
}

read_applinks <- function() {
  applink_clicks <<- polloi::read_dataset("discovery/portal/app_link_clicks.tsv", col_types = "Dcci")
}

read_geo <- function() {

  all_country_data <- polloi::read_dataset("discovery/portal/all_country_data.tsv", col_types = "Dcididid")
  first_visits_country <- polloi::read_dataset("discovery/portal/first_visits_country.tsv", col_types = "Dccid")
  last_action_country <- polloi::read_dataset("discovery/portal/last_action_country.tsv", col_types = "Dccid")
  most_common_country <- polloi::read_dataset("discovery/portal/most_common_country.tsv", col_types = "Dccid")
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
    dplyr::group_by(date) %>%
    dplyr::mutate(event_prop=round(events/sum(events),4)*100, visit_prop=round(n_visit/sum(n_visit),4)*100, session_prop=round(n_session/sum(n_session),4)*100) %>%
    dplyr::select(date,country,event_prop,ctr,visit_prop,ctr_visit,session_prop,ctr_session) %>% dplyr::ungroup()
  us_mask <- grepl("^U\\.S\\.", all_country_data$country)
  us_data <- all_country_data[us_mask,]
  all_country_data <- us_data %>%
    dplyr::mutate(clicks = events*ctr, click_v=n_visit*ctr_visit, click_s=n_session*ctr_session) %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(country="United States", events=sum(events), ctr=round(sum(clicks)/sum(events),4),
              n_visit=sum(n_visit), ctr_visit=round(sum(click_v)/sum(n_visit),4),
              n_session=sum(n_session), ctr_session=round(sum(click_s)/sum(n_session),4)) %>%
    rbind(all_country_data[!us_mask,]) %>%
    dplyr::arrange(date, country)
  us_mask <- grepl("^U\\.S\\.", all_country_data_prop$country)
  us_data_prop <- all_country_data_prop[us_mask,]
  all_country_data_prop <- us_data_prop %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(country="United States", event_prop=sum(event_prop),
              visit_prop=sum(visit_prop), session_prop=sum(session_prop)) %>%
    dplyr::left_join(all_country_data[, c("date","country","ctr","ctr_visit","ctr_session")], by=c("date","country")) %>%
    rbind(all_country_data_prop[!us_mask,]) %>%
    dplyr::select(date,country,event_prop,ctr,visit_prop,ctr_visit,session_prop,ctr_session) %>%
    dplyr::arrange(date, country)
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
  all_country_data <<- all_country_data[, c(1:2, 9, 3:8)] %>% dplyr::arrange(Date, Country)
  all_country_data_prop <<- all_country_data_prop[, c(1:2, 9, 3:8)] %>% dplyr::arrange(Date, Country)
  us_data <<- us_data %>% dplyr::mutate(Region="North America") %>% dplyr::select(c(1:2, 9, 3:8)) %>% dplyr::arrange(Date, Country)
  us_data_prop <<- us_data_prop %>% dplyr::mutate(Region="North America") %>% dplyr::select(c(1:2, 9, 3:8)) %>% dplyr::arrange(Date, Country)


  first_visits_country <- first_visits_country[!duplicated(first_visits_country[,1:3],fromLast=T),]
  colnames(first_visits_country) <- c("Date", "Action", "Country", "Number of Sessions", "Proportion")
  first_visits_country$Proportion <- first_visits_country$Proportion*100
  first_visits_country_prop <- tidyr::spread(first_visits_country[,-4], key=Action, value=Proportion, fill=0)
  first_visits_country <- tidyr::spread(first_visits_country[,-5], key=Action, value=`Number of Sessions`, fill=0)
  colnames(first_visits_country_prop) <- sapply(colnames(first_visits_country_prop), capitalize_first_letter)
  colnames(first_visits_country) <- sapply(colnames(first_visits_country), capitalize_first_letter)
  us_mask <- grepl("^U\\.S\\.", first_visits_country$Country)
  first_visits_us <- first_visits_country[us_mask, ]
  first_visits_country <- first_visits_us %>% dplyr::select(-Country) %>% dplyr::group_by(Date) %>%
    dplyr::summarize_each(dplyr::funs(sum)) %>% dplyr::mutate(Country="United States") %>%
    rbind(first_visits_country[!us_mask,])
  us_mask <- grepl("^U\\.S\\.", first_visits_country_prop$Country)
  first_visits_us_prop <- first_visits_country_prop[us_mask, ]
  first_visits_country_prop <- first_visits_us_prop %>% dplyr::select(-Country) %>% dplyr::group_by(Date) %>%
    dplyr::summarize_each(dplyr::funs(sum)) %>% dplyr::mutate(Country="United States") %>%
    rbind(first_visits_country_prop[!us_mask,])
  region_mask <- match(stringi::stri_trans_general(first_visits_country$Country, "Latin-ASCII"), countrycode_data$country.name)
  first_visits_country$Region <- countrycode_data$continent[region_mask]
  first_visits_country$Region[is.na(first_visits_country$Region)] <- "Other"
  region_mask <- match(stringi::stri_trans_general(first_visits_country_prop$Country, "Latin-ASCII"), countrycode_data$country.name)
  first_visits_country_prop$Region <- countrycode_data$continent[region_mask]
  first_visits_country_prop$Region[is.na(first_visits_country_prop$Region)] <- "Other"
  first_visits_country <<- first_visits_country[, c(1, 8:9, 2:7)] %>% dplyr::arrange(Date, Country)
  first_visits_country_prop <<- first_visits_country_prop[, c(1, 8:9, 2:7)] %>% dplyr::arrange(Date, Country)
  first_visits_us <<- first_visits_us %>% dplyr::mutate(Region="North America") %>% dplyr::select(c(1:2, 9, 3:8)) %>% dplyr::arrange(Date, Country)
  first_visits_us_prop <<- first_visits_us_prop %>% dplyr::mutate(Region="North America") %>% dplyr::select(c(1:2, 9, 3:8)) %>% dplyr::arrange(Date, Country)


  last_action_country <- last_action_country[!duplicated(last_action_country[,1:3],fromLast=T),]
  colnames(last_action_country) <- c("Date", "Action", "Country", "Events", "Proportion")
  last_action_country$Proportion <- last_action_country$Proportion*100
  last_action_country_prop <- tidyr::spread(last_action_country[,-4], key=Action, value=Proportion, fill=0)
  last_action_country <- tidyr::spread(last_action_country[,-5], key=Action, value=Events, fill=0)
  colnames(last_action_country_prop) <- sapply(colnames(last_action_country_prop), capitalize_first_letter)
  colnames(last_action_country) <- sapply(colnames(last_action_country), capitalize_first_letter)
  us_mask <- grepl("^U\\.S\\.", last_action_country$Country)
  last_action_us <- last_action_country[us_mask, ]
  last_action_country <- last_action_us %>% dplyr::select(-Country) %>% dplyr::group_by(Date) %>%
    dplyr::summarize_each(dplyr::funs(sum)) %>% dplyr::mutate(Country="United States") %>%
    rbind(last_action_country[!us_mask,])
  us_mask <- grepl("^U\\.S\\.", last_action_country_prop$Country)
  last_action_us_prop <- last_action_country_prop[us_mask, ]
  last_action_country_prop <- last_action_us_prop %>% dplyr::select(-Country) %>% dplyr::group_by(Date) %>%
    dplyr::summarize_each(dplyr::funs(sum)) %>% dplyr::mutate(Country="United States") %>%
    rbind(last_action_country_prop[!us_mask,])
  region_mask <- match(stringi::stri_trans_general(last_action_country$Country, "Latin-ASCII"), countrycode_data$country.name)
  last_action_country$Region <- countrycode_data$continent[region_mask]
  last_action_country$Region[is.na(last_action_country$Region)] <- "Other"
  region_mask <- match(stringi::stri_trans_general(last_action_country_prop$Country, "Latin-ASCII"), countrycode_data$country.name)
  last_action_country_prop$Region <- countrycode_data$continent[region_mask]
  last_action_country_prop$Region[is.na(last_action_country_prop$Region)] <- "Other"
  last_action_country <<- last_action_country[, c(1, 8:9, 2:7)] %>% dplyr::arrange(Date, Country)
  last_action_country_prop <<- last_action_country_prop[, c(1, 8:9, 2:7)] %>% dplyr::arrange(Date, Country)
  last_action_us <<- last_action_us %>% dplyr::mutate(Region="North America") %>% dplyr::select(c(1:2, 9, 3:8)) %>% dplyr::arrange(Date, Country)
  last_action_us_prop <<- last_action_us_prop %>% dplyr::mutate(Region="North America") %>% dplyr::select(c(1:2, 9, 3:8)) %>% dplyr::arrange(Date, Country)


  most_common_country <- most_common_country[!duplicated(most_common_country[,1:3],fromLast=T),]
  colnames(most_common_country) <- c("Date", "Action", "Country", "Number of Visits", "Proportion")
  most_common_country$Proportion <- most_common_country$Proportion*100
  most_common_country_prop <- tidyr::spread(most_common_country[,-4], key=Action, value=Proportion, fill=0)
  most_common_country <- tidyr::spread(most_common_country[,-5], key=Action, value=`Number of Visits`, fill=0)
  colnames(most_common_country_prop) <- sapply(colnames(most_common_country_prop), capitalize_first_letter)
  colnames(most_common_country) <- sapply(colnames(most_common_country), capitalize_first_letter)
  us_mask <- grepl("^U\\.S\\.", most_common_country$Country)
  most_common_us <- most_common_country[us_mask, ]
  most_common_country <- most_common_us %>% dplyr::select(-Country) %>% dplyr::group_by(Date) %>%
    dplyr::summarize_each(dplyr::funs(sum)) %>% dplyr::mutate(Country="United States") %>%
    rbind(most_common_country[!us_mask,])
  us_mask <- grepl("^U\\.S\\.", most_common_country_prop$Country)
  most_common_us_prop <- most_common_country_prop[us_mask, ]
  most_common_country_prop <- most_common_us_prop %>% dplyr::select(-Country) %>% dplyr::group_by(Date) %>%
    dplyr::summarize_each(dplyr::funs(sum)) %>% dplyr::mutate(Country="United States") %>%
    rbind(most_common_country_prop[!us_mask,])
  region_mask <- match(stringi::stri_trans_general(most_common_country$Country, "Latin-ASCII"), countrycode_data$country.name)
  most_common_country$Region <- countrycode_data$continent[region_mask]
  most_common_country$Region[is.na(most_common_country$Region)] <- "Other"
  region_mask <- match(stringi::stri_trans_general(most_common_country_prop$Country, "Latin-ASCII"), countrycode_data$country.name)
  most_common_country_prop$Region <- countrycode_data$continent[region_mask]
  most_common_country_prop$Region[is.na(most_common_country_prop$Region)] <- "Other"
  most_common_country <<- most_common_country[, c(1, 7:8, 2:6)] %>% dplyr::arrange(Date, Country)
  most_common_country_prop <<- most_common_country_prop[, c(1, 7:8, 2:6)] %>% dplyr::arrange(Date, Country)
  most_common_us <<- most_common_us %>% dplyr::mutate(Region="North America") %>% dplyr::select(c(1:2, 8, 3:7)) %>% dplyr::arrange(Date, Country)
  most_common_us_prop <<- most_common_us_prop %>% dplyr::mutate(Region="North America") %>% dplyr::select(c(1:2, 8, 3:7)) %>% dplyr::arrange(Date, Country)
}
