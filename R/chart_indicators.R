chart_indicators <- function(df, factor_name = NULL) {
  df %>%
    dplyr::filter(factor == factor_name, !str_detect(indicator, "discontinued")) %>%
    split(.$rowid) %>%
    purrr::map(~ {
      dat <- .x %>%
        tidyr::unnest(data)
      dat %>%
        highcharter::hchart("line", highcharter::hcaes(date, price), name = unique(.x$indicator)) %>%
        highcharter::hc_title(text = unique(.x$indicator)) %>%
        highcharter::hc_rangeSelector(
          enabled = TRUE,
          buttons = list(
            dataGrouping = list(forced = TRUE, units = list(
              list("week", 1),
              list("month",c(1,3,6,12))
            ))
          )
        ) %>%
        highcharter::hc_xAxis(title = list(text = "")) %>%
        highcharter::hc_yAxis(
          title = list(text = ""),
          plotLines = list(list(
            value = tail(dat, 1) %>% pull(price), color = "white",
            zIndex = 1000,
            dashStyle = "shortdash", width = 1, label = list(
              style = list(color = "white"),
              formatter = JS("function(){
                   return Highcharts.numberFormat(this.options.value, 2);
                 }")
            )
          ))
        ) %>%
        highcharter::hc_add_theme(highcharter::hc_theme_darkunica()) %>%
        hc_exporting(
          enabled = TRUE, filename = glue::glue("{unique(.x$id)}-fred-zg"),
          sourceWidth = 800, sourceHeight = 500, scale = 10
        ) %>%
        highcharter::hc_credits(enabled = TRUE, text = "Source: <a href='https://fred.stlouisfed.org/' target='_blank'>Federal Reserve Economic Data</a>", useHTML = TRUE)
    }) %>%
    highcharter::hw_grid(ncol = 2)
}
