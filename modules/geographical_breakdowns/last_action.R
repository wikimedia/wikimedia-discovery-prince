last_action_country_dt <- reactive({
  if (input$cntr_sort_l %in% c("us_l", "custom_l")){
    fnl_tbl <- last_action_country %>%
      rbind(last_action_us) %>%
      dplyr::filter(Date >= input$date_last_action[1]
                    & Date <= input$date_last_action[2]
                    & Country %in% selected_country_l()) %>%
      dplyr::arrange(Date, Country)
  } else{
    fnl_tbl <- last_action_country %>%
      dplyr::filter(Date >= input$date_last_action[1]
                    & Date <= input$date_last_action[2]
                    & Country %in% selected_country_l())
  }
  return (fnl_tbl)
})
last_action_country_prop_dt <- reactive({
  if (input$cntr_sort_l %in% c("us_l", "custom_l")){
    fnl_tbl <- last_action_country_prop %>%
      rbind(last_action_us_prop) %>%
      dplyr::filter(Date >= input$date_last_action[1]
                    & Date <= input$date_last_action[2]
                    & Country %in% selected_country_l()) %>%
      dplyr::arrange(Date, Country)
  } else{
    fnl_tbl <- last_action_country_prop %>%
      dplyr::filter(Date >= input$date_last_action[1]
                    & Date <= input$date_last_action[2]
                    & Country %in% selected_country_l())
  }
  return (fnl_tbl)
})
observeEvent(input$action_select_l, {
  cntr_reactive$selected_metric_l <- switch(input$action_select_l,
                                            nact_l = "No Action",
                                            olv_l = "Other Languages",
                                            oproj_l = "Other Projects",
                                            prln_l = "Primary Links",
                                            search_l = "Search",
                                            secln_l = "Secondary Links"
  )
})
selected_country_l <- reactive({
  all_country_temp<- last_action_country %>%
    dplyr::filter(last_action_country$Date >= input$date_last_action[1]
                  & last_action_country$Date <= input$date_last_action[2]) %>%
    dplyr::select_(.dots=c("Country",paste0("`",cntr_reactive$selected_metric_l,"`"))) %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_l,"`)")) %>%
    dplyr::arrange(value, Country)
  result <- switch(input$cntr_sort_l,
                   top10_l = {tail(all_country_temp, 10)$Country},
                   bottom50_l = {head(all_country_temp, 50)$Country},
                   us_l = unique(last_action_us$Country),
                   all_l = unique(last_action_country$Country),
                   all_nus_l = unique(last_action_country$Country)[!grepl("United States", unique(last_action_country$Country))],
                   custom_l = input$cntr_l
  )
  return(result)
})
observeEvent(input$cntr_sort_l, {
  toggleClass("cntr_l_legend", "small", length(selected_country_l())>7 & input$cntr_combine_l==F)
  toggleClass("cntr_l_legend", "large", length(selected_country_l())<=7 | input$cntr_combine_l)
  if (input$cntr_sort_l %in% c("bottom50_l","all_l","all_nus_l")) {
    updateCheckboxInput(session, "cntr_combine_l", value = TRUE)
  } else {
    updateCheckboxInput(session, "cntr_combine_l", value = FALSE)
  }
})
output$last_action_dygraph <- renderDygraph({
  if (input$cntr_combine_l == T){
    data4dygraph <- {
      if(input$prop_l) last_action_country_prop_dt() else last_action_country_dt()
    } %>%
      dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_l,"`"))) %>%
      dplyr::group_by(Date) %>%
      dplyr::summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_l,"`)")) %>%
      dplyr::rename(date=Date) %>%
      fill_out(start_date = input$date_last_action[1], end_date = input$date_last_action[2])
    names(data4dygraph)[2] <- paste0(isolate(cntr_reactive$selected_metric_l))
  } else{
    if (length(selected_country_l())>1){
      data4dygraph <- {
        if(input$prop_l) last_action_country_prop_dt() else last_action_country_dt()
      } %>%
        dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_l,"`"))) %>%
        tidyr::spread_(., key_col="Country", value_col=cntr_reactive$selected_metric_l, fill=0) %>%
        dplyr::rename(date=Date) %>%
        fill_out(start_date = input$date_last_action[1], end_date = input$date_last_action[2])
    } else{
      data4dygraph <- {
        if(input$prop_l) last_action_country_prop_dt() else last_action_country_dt()
      } %>%
        dplyr::select_(.dots=c("Date", paste0("`",cntr_reactive$selected_metric_l,"`"))) %>%
        dplyr::rename(date=Date) %>%
        fill_out(start_date = input$date_last_action[1], end_date = input$date_last_action[2])
      names(data4dygraph)[2] <- paste0(isolate(selected_country_l()))
    }
  }
  data4dygraph[is.na(data4dygraph)] <- 0
  data4dygraph %>%
  {
    tryCatch(polloi::smoother(., smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_cntr_l)),
             error = function(e) {
               stop("Cannot apply spline smoothing on one or more of the selected countries.")
             }, finally = NULL)
  } %>%
    polloi::make_dygraph(xlab = "Date",
                         ylab = ifelse(input$prop_l, paste0("Proportion of ", cntr_reactive$selected_metric_l, " (%)"), cntr_reactive$selected_metric_l),
                         title = paste0(ifelse(input$prop_l, "Out of all countries, the proportion of sessions whose last action is ", "The number of sessions whose last action is "), cntr_reactive$selected_metric_l, " from ", ifelse(input$cntr_sort_l %in% c("all_l","all_nus_l"), ifelse(input$cntr_sort_l=="all_l","All Countries","All Countries but U.S."), paste0(selected_country_l(), collapse=", ")))) %>%
    dyRangeSelector(fillColor = "gray",
                    strokeColor = "white",
                    retainDateWindow = TRUE) %>%
    dyOptions(strokeWidth = 3, labelsKMB = TRUE, drawPoints = FALSE, pointSize = 3, includeZero = TRUE,
              titleHeight = 60, logscale = input$cntr_logscale_l) %>%
    dyAxis("x", rangePad = 8) %>%
    dyLegend(width = 400, labelsDiv = "cntr_l_legend", show = "always", showZeroValues = FALSE)  %>%
    dyCSS(css = "www/inverse.css") %>%
    dyEvent(as.Date("2016-09-13"), "Schema Switch", labelLoc = "bottom", color = "white") %>%
    dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
})
output$last_action_pie_pl <- renderHighchart({
  data4pie <- last_action_country_dt() %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate_(region_total=paste0("sum(`",cntr_reactive$selected_metric_l,"`)")) %>%
    dplyr::group_by(Region, Country) %>%
    dplyr::mutate_(country_total=paste0("sum(`",cntr_reactive$selected_metric_l,"`)")) %>%
    dplyr::select(Region, Country, country_total, region_total) %>%
    unique() %>% dplyr::ungroup() %>% dplyr::arrange(Region, Country)
  data4pie_us <- last_action_us %>%
    dplyr::filter(last_action_us$Date >= input$date_last_action[1]
                  & last_action_us$Date <= input$date_last_action[2]) %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize_(us_total=paste0("sum(`",cntr_reactive$selected_metric_l,"`)")) %>% dplyr::ungroup()
  hc <- highchart() %>%
    hc_chart(type = "pie", plotBackgroundColor=NULL, plotBorderWidth=NULL, plotShadow=F) %>%
    hc_title(text = paste0("Number of ", cntr_reactive$selected_metric_l, " by Country"),
             style = list(color = "#ECF0F5")) %>%
    hc_tooltip(pointFormat = '{point.y} ({point.percentage:.1f}%)') %>%
    hc_plotOptions(
      pie = list(
        shadow = FALSE,
        center = c("50%", "50%"),
        dataLabels = list(
          style = list(
            color = '#ffffff',
            fontSize = '12px'
          )
        )
      )
    ) %>%
    hc_add_series(data = data4pie %>% dplyr::select(name=Region, y=region_total) %>%
                    dplyr::mutate(drilldown=tolower(name)) %>% unique() %>% list_parse(),
                  size = '60%',
                  name = "All Continents",
                  dataLabels = list(distance = -60,
                                    color = 'black',
                                    formatter = JS("function () {
                                                     return this.percentage > 5 ? this.point.name : null;
  }"))
    ) %>%
    hc_add_series(data = data4pie %>% dplyr::select(name=Country, y=country_total) %>%
                    dplyr::mutate(drilldown=ifelse(name=="United States","united states", NA)) %>% unique() %>% list_parse(),
                  name = "All Countries", size = '100%', innerSize = "60%") %>%
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = data4pie %>% dplyr::mutate(id=tolower(Region)) %>%
        dplyr::select(name=Country, y=country_total, id) %>%
        unique() %>%
        split(.$id) %>%
        lapply(function(x){
          list(id= as.character(unique(x[,3])),
               data=list_parse(x[,-3]))
        }) %>%
        magrittr::set_names(NULL) %>%
        append(list(list(
          id="united states",
          data=list_parse2(data4pie_us)
        ))),
      activeDataLabelStyle = list(
        color = '#1E90FF',
        fontSize = '12px',
        fontWeight = 'bold',
        textShadow = 'none'
      )
    )
  hc
})
output$last_action_by_country_tbl <- DT::renderDataTable(
  {if(input$prop_l) last_action_country_prop_dt() else last_action_country_dt()},
  options = list(lengthMenu = list(c(15, 30, 60), c('15', '30', '60')), pageLength = 15)
)
# download the filtered data
output$last_action_dl = downloadHandler('last_action_filtered.csv', content = function(file) {
  s <- input$last_action_by_country_tbl_rows_all
  if(input$prop_l){
    fnl_dt <- last_action_country_prop_dt()
  } else{
    fnl_dt <- last_action_country_dt()
  }
  write.csv(fnl_dt[s, , drop = FALSE], file, row.names=FALSE)
})
