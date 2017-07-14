first_visits_country_dt <- reactive({
  if (input$cntr_sort_f %in% c("us_f", "custom_f")){
    fnl_tbl <- first_visits_country %>%
      rbind(first_visits_us) %>%
      dplyr::filter(Date >= input$date_first_visit[1]
                    & Date <= input$date_first_visit[2]
                    & Country %in% selected_country_f()) %>%
      dplyr::arrange(Date, Country)
  } else{
    fnl_tbl <- first_visits_country %>%
      dplyr::filter(Date >= input$date_first_visit[1]
                    & Date <= input$date_first_visit[2]
                    & Country %in% selected_country_f())
  }
  return (fnl_tbl)
})
first_visits_country_prop_dt <- reactive({
  if (input$cntr_sort_f %in% c("us_f", "custom_f")){
    fnl_tbl <- first_visits_country_prop %>%
      rbind(first_visits_us_prop) %>%
      dplyr::filter(Date >= input$date_first_visit[1]
                    & Date <= input$date_first_visit[2]
                    & Country %in% selected_country_f()) %>%
      dplyr::arrange(Date, Country)
  } else{
    fnl_tbl <- first_visits_country_prop %>%
      dplyr::filter(Date >= input$date_first_visit[1]
                    & Date <= input$date_first_visit[2]
                    & Country %in% selected_country_f())
  }
  return (fnl_tbl)
})
observeEvent(input$action_select_f, {
  cntr_reactive$selected_metric_f <- switch(input$action_select_f,
                                            nact_f = "No Action",
                                            olv_f = "Other Languages",
                                            oproj_f = "Other Projects",
                                            prln_f = "Primary Links",
                                            search_f = "Search",
                                            secln_f = "Secondary Links"
  )
})
selected_country_f <- reactive({
  all_country_temp<- first_visits_country %>%
    dplyr::filter(first_visits_country$Date >= input$date_first_visit[1]
                  & first_visits_country$Date <= input$date_first_visit[2]) %>%
    dplyr::select_(.dots=c("Country",paste0("`",cntr_reactive$selected_metric_f,"`"))) %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_f,"`)")) %>%
    dplyr::arrange(value, Country)
  result <- switch(input$cntr_sort_f,
                   top10_f = {tail(all_country_temp, 10)$Country},
                   bottom50_f = {head(all_country_temp, 50)$Country},
                   us_f = unique(first_visits_us$Country),
                   all_f = unique(first_visits_country$Country),
                   all_nus_f = unique(first_visits_country$Country)[!grepl("United States", unique(first_visits_country$Country))],
                   custom_f = input$cntr_f
  )
  return(result)
})
observeEvent(input$cntr_sort_f, {
  toggleClass("cntr_f_legend", "small", length(selected_country_f())>7 & input$cntr_combine_f==F)
  toggleClass("cntr_f_legend", "large", length(selected_country_f())<=7 | input$cntr_combine_f)
  if (input$cntr_sort_f %in% c("bottom50_f","all_f","all_nus_f")) {
    updateCheckboxInput(session, "cntr_combine_f", value = TRUE)
  } else {
    updateCheckboxInput(session, "cntr_combine_f", value = FALSE)
  }
})
output$first_visit_country_dygraph <- renderDygraph({
  if (input$cntr_combine_f == T){
    data4dygraph <- {
      if(input$prop_f) first_visits_country_prop_dt() else first_visits_country_dt()
    } %>%
      dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_f,"`"))) %>%
      dplyr::group_by(Date) %>%
      dplyr::summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_f,"`)")) %>%
      dplyr::rename(date=Date) %>%
      fill_out(start_date = input$date_first_visit[1], end_date = input$date_first_visit[2])
    names(data4dygraph)[2] <- paste0(isolate(cntr_reactive$selected_metric_f))
  } else{
    if (length(selected_country_f())>1){
      data4dygraph <- {
        if(input$prop_f) first_visits_country_prop_dt() else first_visits_country_dt()
      } %>%
        dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_f,"`"))) %>%
        tidyr::spread_(., key_col="Country", value_col=cntr_reactive$selected_metric_f, fill=0) %>%
        dplyr::rename(date=Date) %>%
        fill_out(start_date = input$date_first_visit[1], end_date = input$date_first_visit[2])
    } else{
      data4dygraph <- {
        if(input$prop_f) first_visits_country_prop_dt() else first_visits_country_dt()
      } %>%
        dplyr::select_(.dots=c("Date", paste0("`",cntr_reactive$selected_metric_f,"`"))) %>%
        dplyr::rename(date=Date) %>%
        fill_out(start_date = input$date_first_visit[1], end_date = input$date_first_visit[2])
      names(data4dygraph)[2] <- paste0(isolate(selected_country_f()))
    }
  }
  data4dygraph[is.na(data4dygraph)] <- 0
  data4dygraph %>%
  {
    tryCatch(polloi::smoother(., smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_cntr_f)),
             error = function(e) {
               stop("Cannot apply spline smoothing on one or more of the selected countries.")
             }, finally = NULL)
  } %>%
    polloi::make_dygraph(xlab = "Date",
                         ylab = ifelse(input$prop_f, paste0("Proportion of ", cntr_reactive$selected_metric_f, " (%)"), cntr_reactive$selected_metric_f),
                         title = paste0(ifelse(input$prop_f, "Out of all countries, the proportion of clicks to ", "The number of clicks to "), cntr_reactive$selected_metric_f, " from ", ifelse(input$cntr_sort_f %in% c("all_f","all_nus_f"), ifelse(input$cntr_sort_f=="all_f","All Countries","All Countries but U.S."), paste0(selected_country_f(), collapse=", ")), " at first visit")) %>%
    dyRangeSelector(fillColor = "gray",
                    strokeColor = "white",
                    retainDateWindow = TRUE) %>%
    dyOptions(strokeWidth = 3, labelsKMB = TRUE, drawPoints = FALSE, pointSize = 3, includeZero = TRUE,
              titleHeight = 60, logscale = input$cntr_logscale_f) %>%
    dyAxis("x", rangePad = 8) %>%
    dyLegend(width = 400, labelsDiv = "cntr_f_legend", show = "always", showZeroValues = FALSE)  %>%
    dyCSS(css = "www/inverse.css") %>%
    dyEvent(as.Date("2016-09-13"), "Schema Switch", labelLoc = "bottom", color = "white") %>%
    dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
})
output$first_visit_pie_pl <- renderHighchart({
  data4pie <- first_visits_country_dt() %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate_(region_total=paste0("sum(`",cntr_reactive$selected_metric_f,"`)")) %>%
    dplyr::group_by(Region, Country) %>%
    dplyr::mutate_(country_total=paste0("sum(`",cntr_reactive$selected_metric_f,"`)")) %>%
    dplyr::select(Region, Country, country_total, region_total) %>%
    unique() %>% dplyr::ungroup() %>% dplyr::arrange(Region, Country)
  data4pie_us <- first_visits_us %>%
    dplyr::filter(first_visits_us$Date >= input$date_first_visit[1]
                  & first_visits_us$Date <= input$date_first_visit[2]) %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize_(us_total=paste0("sum(`",cntr_reactive$selected_metric_f,"`)")) %>% dplyr::ungroup()
  hc <- highchart() %>%
    hc_chart(type = "pie", plotBackgroundColor=NULL, plotBorderWidth=NULL, plotShadow=F) %>%
    hc_title(text = paste0("Number of ", cntr_reactive$selected_metric_f, " by Country"),
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
output$first_visits_by_country_tbl <- DT::renderDataTable(
  {if(input$prop_f) first_visits_country_prop_dt() else first_visits_country_dt()},
  options = list(lengthMenu = list(c(15, 30, 60), c('15', '30', '60')), pageLength = 15)
)
# download the filtered data
output$first_visits_dl = downloadHandler('first_visits_filtered.csv', content = function(file) {
  s <- input$first_visits_by_country_tbl_rows_all
  if(input$prop_f){
    fnl_dt <- first_visits_country_prop_dt()
  } else{
    fnl_dt <- first_visits_country_dt()
  }
  write.csv(fnl_dt[s, , drop = FALSE], file, row.names=FALSE)
})
