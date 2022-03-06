sector_index_chart <- function(df,start_date = (Sys.Date() %m-% years(3))){
  df %>%
    unnest() %>%
    select(sector,ticker,date,adjusted) %>%
    filter(date >= as_date(start_date)) %>%
    mutate(key = glue::glue("{sector} | {ticker}")) %>%
    mutate(year_mon = paste(isoweek(date),year(date))) %>%
    group_by(key,year_mon) %>%
    filter(date == max(date,na.rm=TRUE)) %>%
    group_by(key) %>%
    arrange(key,date) %>%
    mutate(index = adjusted/head(adjusted) - 1) %>%
    group_by(sector,date) %>%
    summarize(index = mean(index,na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(sector,date) %>%
    hchart("line",hcaes(date,index,group = sector)) %>%
    hc_add_theme(hc_theme_darkunica()) %>%
    hc_tooltip(shared = TRUE,table = TRUE,valueDecimals = 2,sort = TRUE) %>%
    hc_rangeSelector(enabled = TRUE)

}


plot_index_splines <- function(df){
  seq.Date((Sys.Date() - years(6)),Sys.Date(),by = "year") %>%
    floor_date("year") %>%
    rev() %>%
    map(~{
      df %>%
        sector_index_chart(.x)

    }) %>%
    hw_grid(ncol = 2)

}

