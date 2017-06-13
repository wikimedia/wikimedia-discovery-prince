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
all_country_names <- c("Zimbabwe", "Zambia", "Yemen", "Virgin Islands, British", "Viet Nam", "Venezuela, Bolivarian Republic of", "Uzbekistan", "U.S. (West)", "U.S. (South)", "U.S. (Pacific)", "U.S. (Other)", "U.S. (Northeast)", "U.S. (Midwest)", "Uruguay", "United Kingdom", "United Arab Emirates", "Ukraine", "Uganda", "Turkmenistan", "Turkey", "Tunisia", "Trinidad and Tobago", "Timor-Leste", "Thailand", "Tanzania, United Republic of", "Tajikistan", "Taiwan, Province of China", "Syrian Arab Republic", "Switzerland", "Sweden", "Suriname", "Sudan", "Sri Lanka", "Spain", "South Africa", "Somalia", "Slovenia", "Slovakia", "Singapore", "Seychelles", "Serbia", "Senegal", "Saudi Arabia", "Rwanda", "Russian Federation", "Romania", "Qatar", "Portugal", "Poland", "Philippines", "Peru", "Paraguay", "Papua New Guinea", "Panama", "Palestine, State of", "Pakistan", "Other", "Oman", "Norway", "Nigeria", "Niger", "Nicaragua", "New Zealand", "Netherlands", "Nepal", "Namibia", "Myanmar", "Mozambique", "Morocco", "Montenegro", "Mongolia", "Moldova, Republic of", "Mexico", "Mauritius", "Mauritania", "Martinique", "Mali", "Malaysia", "Malawi", "Madagascar", "Macedonia, Republic of", "Macao", "Luxembourg", "Lithuania", "Libya", "Lebanon", "Latvia", "Lao People's Democratic Republic", "Kyrgyzstan", "Kuwait", "Korea, Republic of", "Kenya", "Kazakhstan", "Jordan", "Jersey", "Japan", "Jamaica", "Italy", "Israel", "Ireland", "Iraq", "Iran, Islamic Republic of", "Indonesia", "India", "Iceland", "Hungary", "Hong Kong", "Honduras", "Haiti", "Guernsey", "Guatemala", "Greenland", "Greece", "Ghana", "Germany", "Georgia", "French Polynesia", "France", "Finland", "Fiji", "Ethiopia", "Estonia", "El Salvador", "Egypt", "Ecuador", "Dominican Republic", "Dominica", "Djibouti", "Denmark", "Czechia", "Cyprus", "Curacao", "Cuba", "Croatia", "Cote d'Ivoire", "Costa Rica", "Congo, The Democratic Republic of the", "Colombia", "China", "Chile", "Canada", "Cameroon", "Cambodia", "Burkina Faso", "Bulgaria", "British Indian Ocean Territory", "Brazil", "Botswana", "Bolivia, Plurinational State of", "Bhutan", "Benin", "Belgium", "Belarus", "Barbados", "Bangladesh", "Bahrain", "Azerbaijan", "Austria", "Australia", "Aruba", "Armenia", "Argentina", "Angola", "Algeria", "Albania", "Afghanistan", "Togo", "Malta", "Guadeloupe", "Gibraltar", "Gabon", "Faroe Islands", "Congo", "Cayman Islands", "Brunei Darussalam", "Bosnia and Herzegovina", "Bahamas", "Reunion", "Maldives", "Guyana", "Guinea", "Cabo Verde", "Burundi", "Antigua and Barbuda", "Swaziland", "Saint Lucia", "Isle of Man", "Gambia", "Central African Republic", "Belize", "Vanuatu", "Sierra Leone", "Saint Kitts and Nevis", "New Caledonia", "Lesotho", "Solomon Islands", "French Guiana", "Chad", "Bermuda", "Turks and Caicos Islands", "Liberia", "Comoros", "Bonaire, Sint Eustatius and Saba", "Aland Islands", "Grenada", "Mayotte", "Liechtenstein", "Samoa", "Equatorial Guinea", "Andorra", "South Sudan", "Saint Martin (French part)", "Saint Vincent and the Grenadines", "Holy See (Vatican City State)", "Guinea-Bissau", "Eritrea", "Saint Barthelemy", "Cook Islands", "Sint Maarten (Dutch part)", "Sao Tome and Principe", "Anguilla", "Monaco", "Kiribati", "Micronesia, Federated States of", "San Marino", "United States")

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
