library(polloi)
library(data.table)
library(reshape2)

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
