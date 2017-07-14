cntr_reactive <- reactiveValues(selected_metric_a = NULL, selected_metric_f = NULL, selected_metric_l = NULL, selected_metric_m = NULL)

all_country_data_dt <- reactive({
  if (input$cntr_sort_a %in% c("us_a", "custom_a")){
    fnl_tbl <- all_country_data %>%
      rbind(us_data) %>%
      dplyr::filter(Date >= input$date_all_country[1]
                    & Date <= input$date_all_country[2]
                    & Country %in% selected_country_a()) %>%
      dplyr::arrange(Date, Country)
  } else{
    fnl_tbl <- all_country_data %>%
      dplyr::filter(Date >= input$date_all_country[1]
                    & Date <= input$date_all_country[2]
                    & Country %in% selected_country_a())
  }
  return (fnl_tbl)
})
all_country_data_prop_dt <- reactive({
  if (input$cntr_sort_a %in% c("us_a", "custom_a")){
    fnl_tbl <- all_country_data_prop %>%
      rbind(us_data_prop) %>%
      dplyr::filter(Date >= input$date_all_country[1]
                    & Date <= input$date_all_country[2]
                    & Country %in% selected_country_a()) %>%
      dplyr::arrange(Date, Country)
  } else{
    fnl_tbl <- all_country_data_prop %>%
      dplyr::filter(Date >= input$date_all_country[1]
                    & Date <= input$date_all_country[2]
                    & Country %in% selected_country_a())
  }
  return (fnl_tbl)
})
observeEvent(input$traffic_select, {
  cntr_reactive$selected_metric_a <- switch(input$traffic_select,
                                            events = "Number of Events",
                                            visits = "Number of Visits",
                                            sessions = "Number of Sessions",
                                            ctr_all = "Overall Clickthrough Rate",
                                            ctr_vst = "Clickthrough Rate Per Visit",
                                            ctr_ses = "Clickthrough Rate Per Session"
  )
})
selected_country_a <- reactive({
  all_country_temp<- all_country_data %>%
    dplyr::filter(all_country_data$Date >= input$date_all_country[1]
                  & all_country_data$Date <= input$date_all_country[2]) %>%
    dplyr::select_(.dots=c("Country",paste0("`",cntr_reactive$selected_metric_a,"`"))) %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize_(.dots =
                        if(input$traffic_select %in% c('events','visits','sessions')) as.formula(paste0("~ sum(`",cntr_reactive$selected_metric_a,"`)")) else as.formula(paste0("~ median(`",cntr_reactive$selected_metric_a,"`)"))
    ) %>%
    dplyr::ungroup() %>%
    as.data.frame()
  all_country_temp <- all_country_temp[order(all_country_temp[,2], all_country_temp[,1]),]
  result <- switch(input$cntr_sort_a,
                   top10_a = {tail(all_country_temp, 10)$Country},
                   bottom50_a = {head(all_country_temp, 50)$Country},
                   us_a = unique(us_data$Country),
                   all_a = unique(all_country_data$Country),
                   all_nus_a = unique(all_country_data$Country)[!grepl("United States", unique(all_country_data$Country))],
                   custom_a = input$cntr_a
  )
  return(result)
})
observeEvent(input$cntr_sort_a, {
  toggleClass("cntr_a_legend", "small", length(selected_country_a())>7 & input$cntr_combine_a==F)
  toggleClass("cntr_a_legend", "large", length(selected_country_a())<=7 | input$cntr_combine_a)
  if (input$cntr_sort_a %in% c("bottom50_a","all_a","all_nus_a")) {
    updateCheckboxInput(session, "cntr_combine_a", value = TRUE)
  } else {
    updateCheckboxInput(session, "cntr_combine_a", value = FALSE)
  }
})
output$all_country_dygraph <- renderDygraph({
  if (input$cntr_combine_a == T){
    if (input$traffic_select == 'ctr_all'){
      data4dygraph <- all_country_data_dt() %>%
        dplyr::mutate(clicks=`Number of Events`*`Overall Clickthrough Rate`) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarize("Overall Clickthrough Rate"=sum(clicks)/sum(`Number of Events`)) %>%
        dplyr::rename(date=Date) %>%
        fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
    } else if (input$traffic_select == 'ctr_vst'){
      data4dygraph <- all_country_data_dt() %>%
        dplyr::mutate(clicks=`Number of Visits`*`Clickthrough Rate Per Visit`) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarize("Clickthrough Rate Per Visit"=sum(clicks)/sum(`Number of Visits`)) %>%
        dplyr::rename(date=Date) %>%
        fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
    } else if(input$traffic_select == 'ctr_ses'){
      data4dygraph <- all_country_data_dt() %>%
        dplyr::mutate(clicks=`Number of Sessions`*`Clickthrough Rate Per Session`) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarize("Clickthrough Rate Per Session"=sum(clicks)/sum(`Number of Sessions`)) %>%
        dplyr::rename(date=Date) %>%
        fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
    } else{
      data4dygraph <- {
        if(input$prop_a) all_country_data_prop_dt() else all_country_data_dt()
      } %>%
        dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_a,"`"))) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarize_(value=paste0("sum(`",cntr_reactive$selected_metric_a,"`)")) %>%
        dplyr::rename(date=Date) %>%
        fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
      names(data4dygraph)[2] <- paste0(isolate(cntr_reactive$selected_metric_a))
    }
  } else{
    if (length(selected_country_a())>1){
      data4dygraph <- {
        if(input$prop_a) all_country_data_prop_dt() else all_country_data_dt()
      } %>%
        dplyr::select_(.dots=c("Date","Country",paste0("`",cntr_reactive$selected_metric_a,"`"))) %>%
        tidyr::spread_(., key_col="Country", value_col=cntr_reactive$selected_metric_a, fill=0) %>%
        dplyr::rename(date=Date) %>%
        fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
    } else{
      data4dygraph <- {
        if(input$prop_a) all_country_data_prop_dt() else all_country_data_dt()
      } %>%
        dplyr::select_(.dots=c("Date", paste0("`",cntr_reactive$selected_metric_a,"`"))) %>%
        dplyr::rename(date=Date) %>%
        fill_out(start_date = input$date_all_country[1], end_date = input$date_all_country[2])
      names(data4dygraph)[2] <- paste0(isolate(selected_country_a()))
    }
  }
  data4dygraph[is.na(data4dygraph)] <- 0
  if (input$traffic_select %in% c('ctr_all','ctr_vst','ctr_ses')){
    data4dygraph[, colnames(data4dygraph) != "date"] <- data4dygraph[, colnames(data4dygraph) != "date"]*100
  }
  data4dygraph %>%
  {
    tryCatch(polloi::smoother(., smooth_level = polloi::smooth_switch(input$smoothing_global, input$smoothing_cntr_a)),
             error = function(e) {
               stop("Cannot apply spline smoothing on one or more of the selected countries.")
             }, finally = NULL)
  } %>%
    polloi::make_dygraph(xlab = "Date",
                         ylab = ifelse(input$prop_a & input$traffic_select %in% c('events','visits','sessions'),
                                       paste0("Proportion of ", cntr_reactive$selected_metric_a, " (%)"),
                                       ifelse(input$traffic_select %in% c('ctr_all','ctr_vst','ctr_ses'), paste0(cntr_reactive$selected_metric_a, " (%)"), cntr_reactive$selected_metric_a)),
                         title = paste0(ifelse(input$prop_a & input$traffic_select %in% c('events','visits','sessions'),
                                               paste0("Out of all countries, the proportion of ", cntr_reactive$selected_metric_a, " (%)"),
                                               cntr_reactive$selected_metric_a),
                                        " from ", ifelse(input$cntr_sort_a %in% c("all_a","all_nus_a"), ifelse(input$cntr_sort_a=="all_a","All Countries","All Countries but U.S."), paste0(selected_country_a(), collapse=", ")))) %>%
    dyRangeSelector(fillColor = "gray",
                    strokeColor = "white",
                    retainDateWindow = TRUE) %>%
    dyOptions(strokeWidth = 3, labelsKMB = TRUE, drawPoints = FALSE, pointSize = 3, includeZero = TRUE,
              titleHeight = 60, logscale = input$cntr_logscale_a) %>%
    dyAxis("x", rangePad = 8) %>%
    dyLegend(width = 400, labelsDiv = "cntr_a_legend", show = "always", showZeroValues = FALSE) %>%
    dyCSS(css = "www/inverse.css") %>%
    dyEvent(as.Date("2016-09-13"), "Schema Switch", labelLoc = "bottom", color = "white") %>%
    dyEvent(as.Date("2017-01-01"), "R (reportupdater)", labelLoc = "bottom", color = "white")
})
output$traffic_pie_pl <- renderHighchart({
  if(input$traffic_select %in% c('events','ctr_all')){
    this_metric <- "Number of Events"
  } else if(input$traffic_select %in% c('visits','ctr_vst')){
    this_metric <- "Number of Visits"
  } else{
    this_metric <- "Number of Sessions"
  }
  data4pie <- all_country_data_dt() %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate_(region_total=paste0("sum(`",this_metric,"`)")) %>%
    dplyr::group_by(Region, Country) %>%
    dplyr::mutate_(country_total=paste0("sum(`",this_metric,"`)")) %>%
    dplyr::select(Region, Country, country_total, region_total) %>%
    unique() %>% dplyr::ungroup() %>% dplyr::arrange(Region, Country)
  data4pie_us <- us_data %>%
    dplyr::filter(us_data$Date >= input$date_all_country[1]
                  & us_data$Date <= input$date_all_country[2]) %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize_(us_total=paste0("sum(`",this_metric,"`)")) %>% dplyr::ungroup()
  hc <- highchart() %>%
    hc_chart(type = "pie", plotBackgroundColor=NULL, plotBorderWidth=NULL, plotShadow=F) %>%
    hc_title(text = paste0(this_metric, " by Country"),
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
output$all_country_tbl <- DT::renderDataTable(
  {if(input$prop_a){
    fnl_dt <- all_country_data_prop_dt()
    colnames(fnl_dt) <- c("Date", "Country", "Region", "Events Proportion",
                          "Overall Clickthrough Rate", "Visits Proportion", "Clickthrough Rate Per Visit",
                          "Sessions Proportion", "Clickthrough Rate Per Session")
  } else{
    fnl_dt <- all_country_data_dt()
  }
    ctr_mask <- colnames(fnl_dt) %in% c("Overall Clickthrough Rate", "Clickthrough Rate Per Visit", "Clickthrough Rate Per Session")
    fnl_dt[, ctr_mask] <- fnl_dt[, ctr_mask]*100
    colnames(fnl_dt)[ctr_mask] <- paste(colnames(fnl_dt)[ctr_mask], "(%)")
    fnl_dt},
  options = list(lengthMenu = list(c(15, 30, 60), c('15', '30', '60')), pageLength = 15)
)
# download the filtered data
output$all_country_dl = downloadHandler('all_country_filtered.csv', content = function(file) {
  s <- input$all_country_tbl_rows_all
  if(input$prop_a){
    fnl_dt <- all_country_data_prop_dt()
    colnames(fnl_dt) <- c("Date", "Country", "Region", "Events Proportion",
                          "Overall Clickthrough Rate", "Visits Proportion", "Clickthrough Rate Per Visit",
                          "Sessions Proportion", "Clickthrough Rate Per Session")
  } else{
    fnl_dt <- all_country_data_dt()
  }
  ctr_mask <- colnames(fnl_dt) %in% c("Overall Clickthrough Rate", "Clickthrough Rate Per Visit", "Clickthrough Rate Per Session")
  fnl_dt[, ctr_mask] <- fnl_dt[, ctr_mask]*100
  colnames(fnl_dt)[ctr_mask] <- paste(colnames(fnl_dt)[ctr_mask], "(%)")
  write.csv(fnl_dt[s, , drop = FALSE], file, row.names=FALSE)
})
