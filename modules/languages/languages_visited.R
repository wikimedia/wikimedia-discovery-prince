lv_reactive <- reactiveValues(choices = NULL, selected_langs = NULL)

observeEvent(input$lv_sort, {
  if (input$lv_sort %in% c("alphabet_az", "alphabet_za")) {
    lv_reactive$choices <- sort(unique(langs_visited$language), decreasing = input$lv_sort == "alphabet_za")
  } else {
    languages <- langs_visited[, list(
      clicks = sum(clicks),
      users = sum(sessions),
      avg_daily_clicks = as.integer(median(clicks)),
      avg_daily_users = as.integer(median(sessions))),
      by = "language"]
    lv_reactive$choices <- switch(input$lv_sort,
                                  clicks_high2low = {
                                    languages$language[order(languages[[input$lv_response]], decreasing = TRUE)]
                                  },
                                  clicks_low2high = {
                                    languages$language[order(languages[[input$lv_response]], decreasing = FALSE)]
                                  },
                                  avg_clicks_high2low = {
                                    languages$language[order(languages[[ifelse(input$lv_response == "clicks", "avg_daily_clicks", "avg_daily_users")]], decreasing = TRUE)]
                                  },
                                  avg_clicks_low2high = {
                                    languages$language[order(languages[[ifelse(input$lv_response == "clicks", "avg_daily_clicks", "avg_daily_users")]], decreasing = FALSE)]
                                  },
                                  top10 = {
                                    if (input$lv_response == "users") {
                                      dplyr::arrange(languages[languages[[input$lv_response]] %in% head(sort(languages[[input$lv_response]], decreasing = TRUE), 10), ], desc(users), language)$language
                                    } else {
                                      dplyr::arrange(languages[languages[[input$lv_response]] %in% head(sort(languages[[input$lv_response]], decreasing = TRUE), 10), ], desc(clicks), language)$language
                                    }
                                  },
                                  bottom50 = {
                                    if (input$lv_response == "users") {
                                      dplyr::arrange(languages[languages[[input$lv_response]] %in% head(sort(languages[[input$lv_response]], decreasing = FALSE), 50), ], desc(users), language)$language
                                    } else {
                                      dplyr::arrange(languages[languages[[input$lv_response]] %in% head(sort(languages[[input$lv_response]], decreasing = FALSE), 50), ], desc(clicks), language)$language
                                    }
                                  })
  }
  if (input$lv_sort %in% c("top10", "bottom50")) {
    lv_reactive$selected_langs <- lv_reactive$choices
  } else {
    if (!is.null(input$lv_languages)) {
      if (sum(input$lv_languages %in% lv_reactive$choices) == 0) {
        lv_reactive$selected_langs <- lv_reactive$choices[1]
      } else {
        # Lets us keep selected languages when switching betweeen clicks and users
        lv_reactive$selected_langs <- intersect(input$lv_languages, lv_reactive$choices)
      }
    } else {
      lv_reactive$selected_langs <- lv_reactive$choices[1]
    }
  }
})

observeEvent(input$lv_languages, {
  lv_reactive$selected_langs <- input$lv_languages
})

output$lv_languages_container <- renderUI({
  if (input$lv_sort %in% c("top10", "bottom50")) {
    hidden(disabled(selectizeInput("lv_languages", "Wikipedia languages", lv_reactive$choices, lv_reactive$selected_langs, multiple = TRUE)))
  } else {
    selectizeInput("lv_languages", "Wikipedia languages (12 max)", lv_reactive$choices, lv_reactive$selected_langs, multiple = TRUE, options = list(maxItems = 12, plugins = list("remove_button")))
  }
})

outputOptions(output, "lv_languages_container", suspendWhenHidden=FALSE)

observeEvent(input$lv_sort, {

  # Make legend small in case of Top 10 or Bottom 50, otherwise make it big:
  toggleClass("lv_legend", "small", input$lv_sort %in% c("top10", "bottom50"))
  toggleClass("lv_legend", "large", !input$lv_sort %in% c("top10", "bottom50"))

  if (input$lv_sort == "bottom50") {
    updateCheckboxInput(session, "lv_combine", value = TRUE)
  } else {
    updateCheckboxInput(session, "lv_combine", value = FALSE)
  }

})

output$lv_dygraph <- renderDygraph({
  if (input$lv_response == "clicks") {
    if (length(input$lv_languages) > 1) {
      data4dygraph <- langs_visited[langs_visited$language %in% input$lv_languages, c("date", "language", "clicks"), with = FALSE] %>%
      {
        if (input$lv_sort != "top10" && input$lv_combine && length(input$lv_languages) > 1) {
          dplyr::summarize(group_by(., date), `total clicks` = sum(clicks))
        } else {
          tidyr::spread(., language, clicks, fill = 0)
        }
      } %>%
        fill_out(start_date = min(langs_visited$date), end_date = max(langs_visited$date)) %>%
        {
          if (input$lv_sort != "top10" && input$lv_combine && length(input$lv_languages) > 1) {
            .[, union("date", names(.))]
          } else {
            .[, c("date", input$lv_languages)]
          }
        }
    } else {
      if (input$lv_type == "count") {
        data4dygraph <- langs_visited[langs_visited$language == input$lv_languages, c("date", "clicks", "search", "primary", "secondary"), with = FALSE] %>%
          fill_out(start_date = min(langs_visited$date), end_date = max(langs_visited$date))
      } else { # == "prop"
        data4dygraph <- langs_visited[langs_visited$language == input$lv_languages, c("date", "clicks", "search", "primary", "secondary"), with = FALSE] %>%
          fill_out(start_date = min(langs_visited$date), end_date = max(langs_visited$date))
        data4dygraph$search <- round(100*data4dygraph$search/data4dygraph$clicks, 2)
        data4dygraph$primary <- round(100*data4dygraph$primary/data4dygraph$clicks, 2)
        data4dygraph$secondary <- round(100*data4dygraph$secondary/data4dygraph$clicks, 2)
        data4dygraph$clicks <- NULL
      }
    }
  } else { # == "users"
    data4dygraph <- langs_visited[langs_visited$language %in% input$lv_languages, c("date", "language", "sessions"), with = FALSE] %>%
      tidyr::spread(language, sessions, fill = 0) %>%
      fill_out(start_date = min(langs_visited$date), end_date = max(langs_visited$date)) %>%
      {
        .[, c("date", input$lv_languages)]
      }
  }
  data4dygraph[is.na(data4dygraph)] <- 0
  data4dygraph %>%
  {
    tryCatch(polloi::smoother(., smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_lv)),
             error = function(e) {
               stop("Cannot apply spline smoothing on one or more of the selected languages.")
             }, finally = NULL)
  } %>%
    polloi::make_dygraph(xlab = "Date", ylab = ifelse(input$lv_response == "clicks", ifelse(input$lv_type == "count", "Clicks", "Proportion of total clicks (%)"), "Unique sessions"),
                         title = paste0(ifelse(input$lv_response == "clicks", "Clicks", "Users who went"), " to ", paste0(input$lv_languages, collapse = ", ")," Wikipedia", ifelse(length(input$lv_languages) > 1, "s", ""), " from Portal")) %>%
    dyRangeSelector(fillColor = ifelse(input$lv_type == "prop", "", "gray"),
                    strokeColor = ifelse(input$lv_type == "prop", "", "white"),
                    retainDateWindow = TRUE) %>%
    dyOptions(strokeWidth = 3, labelsKMB = TRUE, drawPoints = FALSE, pointSize = 3, includeZero = TRUE,
              logscale = input$lv_logscale && input$lv_type == "count") %>%
    dyAxis("x", rangePad = 8) %>%
    dyLegend(width = 400, labelsDiv = "lv_legend", show = "always", showZeroValues = FALSE) %>%
    dyCSS(css = "www/inverse.css") %>%
    dyEvent(as.Date("2015-11-18"), "A1 (Started EL)", labelLoc = "bottom", color = "white") %>%
    dyEvent(as.Date("2016-03-10"), "Search Box Deployed", labelLoc = "bottom", color = "white") %>%
    dyEvent(as.Date("2016-05-10"), "A2 (Unfiltered Counts)", labelLoc = "bottom", color = "white") %>%
    dyEvent(as.Date("2016-05-18"), "Sister Links Updated", labelLoc = "bottom", color = "white") %>%
    dyEvent(as.Date("2016-06-02"), "Detect Language Deployed", labelLoc = "bottom", color = "white") %>%
    dyEvent(as.Date("2016-08-16"), "Secondary Links Collapsed", labelLoc = "bottom", color = "white") %>%
    dyEvent(as.Date("2016-09-13"), "B (schema switch)", labelLoc = "bottom", color = "white") %>%
    dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
})
