most_common_country_dt <- reactive({
  if (input$cntr_sort_m %in% c("us_m", "custom_m")){
    fnl_tbl <- most_common_country %>%
      rbind(most_common_us) %>%
      dplyr::filter(Date >= input$date_most_common[1]
                    & Date <= input$date_most_common[2]
                    & Country %in% selected_country_m()) %>%
      dplyr::arrange(Date, Country)
  } else{
    fnl_tbl <- most_common_country %>%
      dplyr::filter(Date >= input$date_most_common[1]
                    & Date <= input$date_most_common[2]
                    & Country %in% selected_country_m())
  }
  return (fnl_tbl)
})
most_common_country_prop_dt <- reactive({
  if (input$cntr_sort_m %in% c("us_m", "custom_m")){
    fnl_tbl <- most_common_country_prop %>%
      rbind(most_common_us_prop) %>%
      dplyr::filter(Date >= input$date_most_common[1]
                    & Date <= input$date_most_common[2]
                    & Country %in% selected_country_m()) %>%
      dplyr::arrange(Date, Country)
  } else{
    fnl_tbl <- most_common_country_prop %>%
      dplyr::filter(Date >= input$date_most_common[1]
                    & Date <= input$date_most_common[2]
                    & Country %in% selected_country_m())
  }
  return (fnl_tbl)
})
observeEvent(input$action_select_m, {
  cntr_reactive$selected_metric_m <- switch(input$action_select_m,
                                            olv_m = "Other Languages",
                                            oproj_m = "Other Projects",
                                            prln_m = "Primary Links",
                                            search_m = "Search",
                                            secln_m = "Secondary Links"
  )
})
selected_country_m <- reactive({
  all_country_temp<- most_common_country %>%
    dplyr::filter(most_common_country$Date >= input$date_most_common[1]
                  & most_common_country$Date <= input$date_most_common[2]) %>%
    dplyr::select_(.dots=c("Country",paste0("`",cntr_reactive$selected_metric_m,"`"))) %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_m,"`)")) %>%
    dplyr::arrange(value, Country)
  result <- switch(input$cntr_sort_m,
                   top10_m = {tail(all_country_temp, 10)$Country},
                   bottom50_m = {head(all_country_temp, 50)$Country},
                   us_m = unique(most_common_us$Country),
                   all_m = unique(most_common_country$Country),
                   all_nus_m = unique(most_common_country$Country)[!grepl("United States", unique(most_common_country$Country))],
                   custom_m = input$cntr_m
  )
  return(result)
})
observeEvent(input$cntr_sort_m, {
  toggleClass("cntr_m_legend", "small", length(selected_country_m())>7 & input$cntr_combine_m==F)
  toggleClass("cntr_m_legend", "large", length(selected_country_m())<=7 | input$cntr_combine_m)
  if (input$cntr_sort_m %in% c("bottom50_m","all_m","all_nus_m")) {
    updateCheckboxInput(session, "cntr_combine_m", value = TRUE)
  } else {
    updateCheckboxInput(session, "cntr_combine_m", value = FALSE)
  }
})
output$most_common_country_dygraph <- renderDygraph({
  if (input$cntr_combine_m == T){
    data4dygraph <- {
      if(input$prop_m) most_common_country_prop_dt() else most_common_country_dt()
    } %>%
      dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_m,"`"))) %>%
      dplyr::group_by(Date) %>%
      dplyr::summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_m,"`)")) %>%
      dplyr::rename(date=Date) %>%
      fill_out(start_date = input$date_most_common[1], end_date = input$date_most_common[2])
    names(data4dygraph)[2] <- paste0(isolate(cntr_reactive$selected_metric_m))
  } else{
    if (length(selected_country_m())>1){
      data4dygraph <- {
        if(input$prop_m) most_common_country_prop_dt() else most_common_country_dt()
      } %>%
        dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_m,"`"))) %>%
        tidyr::spread_(., key_col="Country", value_col=cntr_reactive$selected_metric_m, fill=0) %>%
        dplyr::rename(date=Date) %>%
        fill_out(start_date = input$date_most_common[1], end_date = input$date_most_common[2])
    } else{
      data4dygraph <- {
        if(input$prop_m) most_common_country_prop_dt() else most_common_country_dt()
      } %>%
        dplyr::select_(.dots=c("Date", paste0("`",cntr_reactive$selected_metric_m,"`"))) %>%
        dplyr::rename(date=Date) %>%
        fill_out(start_date = input$date_most_common[1], end_date = input$date_most_common[2])
      names(data4dygraph)[2] <- paste0(isolate(selected_country_m()))
    }
  }
  data4dygraph[is.na(data4dygraph)] <- 0
  data4dygraph %>%
  {
    tryCatch(polloi::smoother(., smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_cntr_m)),
             error = function(e) {
               stop("Cannot apply spline smoothing on one or more of the selected countries.")
             }, finally = NULL)
  } %>%
    polloi::make_dygraph(xlab = "Date",
                         ylab = ifelse(input$prop_m, paste0("Proportion of ", cntr_reactive$selected_metric_m, " (%)"), cntr_reactive$selected_metric_m),
                         title = paste0(ifelse(input$prop_m, "Out of all countries, the proportion of visits whose most common section clicked on is ", "The number of visits whose most common section clicked on is "), cntr_reactive$selected_metric_m, " from ", ifelse(input$cntr_sort_m %in% c("all_m","all_nus_m"), ifelse(input$cntr_sort_m=="all_m","All Countries","All Countries but U.S."), paste0(selected_country_m(), collapse=", ")))) %>%
    dyRangeSelector(fillColor = "gray",
                    strokeColor = "white",
                    retainDateWindow = TRUE) %>%
    dyOptions(strokeWidth = 3, labelsKMB = TRUE, drawPoints = FALSE, pointSize = 3, includeZero = TRUE,
              titleHeight = 60, logscale = input$cntr_logscale_m) %>%
    dyAxis("x", rangePad = 8) %>%
    dyLegend(width = 400, labelsDiv = "cntr_m_legend", show = "always", showZeroValues = FALSE)  %>%
    dyCSS(css = "www/inverse.css") %>%
    dyEvent(as.Date("2016-09-13"), "Schema Switch", labelLoc = "bottom", color = "white") %>%
    dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
})
output$most_common_pie_pl <- renderHighchart({
  data4pie <- most_common_country_dt() %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate_(region_total=paste0("sum(`",cntr_reactive$selected_metric_m,"`)")) %>%
    dplyr::group_by(Region, Country) %>%
    dplyr::mutate_(country_total=paste0("sum(`",cntr_reactive$selected_metric_m,"`)")) %>%
    dplyr::select(Region, Country, country_total, region_total) %>%
    unique() %>% dplyr::ungroup() %>% dplyr::arrange(Region, Country)
  data4pie_us <- most_common_us %>%
    dplyr::filter(most_common_us$Date >= input$date_most_common[1]
                  & most_common_us$Date <= input$date_most_common[2]) %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize_(us_total=paste0("sum(`",cntr_reactive$selected_metric_m,"`)")) %>% dplyr::ungroup()
  hc <- highchart() %>%
    hc_chart(type = "pie", plotBackgroundColor=NULL, plotBorderWidth=NULL, plotShadow=F) %>%
    hc_title(text = paste0("Number of ", cntr_reactive$selected_metric_m, " by Country"),
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
output$most_common_by_country_tbl <- DT::renderDataTable(
  {if(input$prop_m) most_common_country_prop_dt() else most_common_country_dt()},
  options = list(lengthMenu = list(c(15, 30, 60), c('15', '30', '60')), pageLength = 15)
)
# download the filtered data
output$most_common_dl = downloadHandler('most_common_filtered.csv', content = function(file) {
  s <- input$most_common_by_country_tbl_rows_all
  if(input$prop_m){
    fnl_dt <- most_common_country_prop_dt()
  } else{
    fnl_dt <- most_common_country_dt()
  }
  write.csv(fnl_dt[s, , drop = FALSE], file, row.names=FALSE)
})
