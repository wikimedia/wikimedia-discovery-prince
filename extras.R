browser_support_matrix <- data.frame(browser = c("Chrome",
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
                                                 "Modern"), # Android 4+
                                     stringsAsFactors = FALSE)
