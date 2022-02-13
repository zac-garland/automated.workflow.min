chart_indicators <- function(df, factor_name = NULL) {
  df %>%
    dplyr::filter(factor == factor_name,!str_detect(indicator,"discontinued")) %>%
    split(.$rowid) %>%
    purrr::map(~ {
      dat <- .x %>%
        tidyr::unnest(data)
      dat %>%
        highcharter::hchart("line", highcharter::hcaes(date, price)) %>%
        highcharter::hc_title(text = unique(.x$indicator)) %>%
        highcharter::hc_rangeSelector(enabled = TRUE) %>%
        highcharter::hc_xAxis(title = list(text = "")) %>%
        highcharter::hc_yAxis(
          title = list(text = ""),
          plotLines = list(list(
            value = tail(dat, 1) %>% pull(price), color = "white",
            dashStyle = "shortdash",width = 1, label = list(
              style = list(color = "white", zIndex = 100),
              formatter = JS("function(){
                   return Highcharts.numberFormat(this.options.value, 2);
                 }")
            )
          ))
        ) %>%
        highcharter::hc_add_theme(highcharter::hc_theme_darkunica()) %>%
        hc_exporting(enabled = TRUE, filename = glue::glue("{unique(.x$id)}-fred-zg")) %>%

        highcharter::hc_credits(enabled = TRUE, text = "Source: <a href='https://fred.stlouisfed.org/' target='_blank'>Federal Reserve Economic Data</a>",useHTML = TRUE)
    }) %>%
    highcharter::hw_grid(ncol = 2)
}
