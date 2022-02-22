###### ARIMA Fit Function

arima_fit_func <- function(data_arima, ARIMA, cases_mean_sd){
    # FIlter appropriate data
    arima_data_train <- data_arima %>%                     # Original Data
        head(nrow(.) - 14) %>%
        mutate(cases = coalesce(cases, 0))
    # Fit ARIMA model with correct order
    fitted <- forecast::Arima(arima_data_train$cases, order = ARIMA,
                              include.constant = FALSE, include.drift = FALSE)
    # Forecast with ARIMA model
    f_dat <- forecast::forecast(fitted, h=14)
    dats <- arima_data_train$cases
    dats <- c(dats, f_dat$mean)
    # Wrangle predictions and cases
    arima_data_fitted <- data_arima %>%
        select(date, cases) %>%
        mutate(predict = dats) %>%
        mutate(across(c(cases, predict),
                      ~.x * cases_mean_sd[2] + cases_mean_sd[1]))
    # Plot predictions and cases
    arima_plot <- arima_data_fitted %>%
        pivot_longer(c(cases, predict)) %>%
        tail(., n = 28) %>%
        ggplot() +
        theme_bw() +
        geom_point(aes(x = date, y = value, color = name)) +
        geom_line(aes(x = date, y = value, color = name)) +
        labs(title = glue("Actual vs Predicted Values ARIMA (",
                          ARIMA[1],",",ARIMA[2],",",ARIMA[3],")")) +
        theme(legend.title = element_blank())
    # Determine performance
    mse_arima <- mean((arima_data_fitted$cases %>%
                           tail(., n = 14) -
                           arima_data_fitted$predict %>%
                           tail(., n = 14))^2)
    # Store results
    return(list("Mean Squared Error Arima" = mse_arima,
                "ARIMA Plot" = arima_plot))
}
