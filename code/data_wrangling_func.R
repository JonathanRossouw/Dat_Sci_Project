#### Data Wrangling Function

data_wrangling_func <- function(data, type, final_date){
    # Filter dates and replace NAs with 0
    data <- data %>%
        select(country_name, cases, date) %>%
        mutate(date = as.Date(date)) %>%
        mutate(cases = ifelse(cases == 0, (dplyr::lag(cases) +
                                               dplyr::lead(cases, k = 1))/2,
                              cases)) %>%
        filter(country_name == "South Africa") %>%
        filter(date > as.Date("2020/06/13") & date < as.Date(final_date)) %>%
        replace_na(list(cases = 0))
    # Rescale data depending on whether ARIMA or LSTM
    if(type == "arima"){
        # Mean Sd rescaling
        cases_mean_sd <- c(mean(data$cases), sd(data$cases))
        data_arima <- data %>%
            mutate(cases = (cases - cases_mean_sd[1])/cases_mean_sd[2])
        return(list(data = data_arima, cases_mean_sd = cases_mean_sd))
    }
    if(type == "lstm"){
        # Rescale Min Max
        cases_min_max <- c(min(data$cases), max(data$cases))
        data_lstm <- data %>%
            mutate(cases = (cases - cases_min_max[1])/(cases_min_max[2] -
                                                           cases_min_max[1]))
        return(list(data = data_lstm, cases_min_max = cases_min_max))
    }
    else break
}
