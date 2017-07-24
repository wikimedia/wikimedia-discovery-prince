# Capitalize the first letter | by Andrie (http://stackoverflow.com/a/6364905/1091835)
capitalize_first_letter <- function(x) {
  s <- strsplit(x, " ")[[1]]
  return(paste(toupper(substring(s, 1,1)), substring(s, 2), sep = "", collapse = " "))
}

browser_support_matrix <- dplyr::data_frame(
  browser = c("Chrome",
              rep("IE", 2),
              rep("Firefox", 2),
              rep("Safari", 2),
              rep("Opera", 2),
              "Safari Mobile",
              "Android"),
  version = c(1, # Chrome
              9, 6, # IE
              20, 3, # Firefox
              5, 3, # Safari
              12, 10, # Opera
              6, # Safari Mobile (iOS)
              4), # Android
  support = c("Modern", # Chrome (all versions, apparently?)
              "Modern", "Basic", # IE 9+, IE 6+
              "Modern", "Basic", # Firefox 20+, Firefox 3+
              "Modern", "Basic", # Safari 5.1+, Safari 3+
              "Modern", "Basic", # Opera 12+, Opera 10+
              "Modern", # Safari Mobile 6.1+
              "Modern") # Android 4+
)

# For selectizeInput in ui.R
data(portal_regions, package = "polloi")
all_country_names <- portal_regions

fill_out <- function(x, start_date, end_date, fill = 0) {
  temp <- dplyr::data_frame(date = seq(start_date, end_date, "day"))
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
