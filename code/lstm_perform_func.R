##### LSTM Predict Plot and Performance Measure

lstm_perform_func <- function(data_train, lstm_model, cases_min_max = cases_min_max, title){
    # Rescale Predictions
    lstm_forecast <- data_train %>%
        tail(., n = 14) %>%
        mutate(predict = lstm_model$`Predicted Values`) %>%
        mutate(across(c(cases, predict), ~(.x * (cases_min_max[2] -
                                                     cases_min_max[1]))  +
                          cases_min_max[1]))
    # Create Plot of Predictions and Cases
    lstm_plot <- lstm_forecast %>%
        pivot_longer(c(cases, predict)) %>%
        ggplot() +
        geom_point(aes(x = date, y = value, color = name)) +
        geom_line(aes(x = date, y = value, color = name)) +
        labs(title = glue("Actual vs Predicted Values ", title), y = "") +
        theme_bw() +
        theme(legend.title = element_blank())
    # Determine performance
    mse_lstm <- lstm_forecast %>%
        summarise(mean((cases - predict)^2)) %>% .[[1]]
    # Store Results
    return(list("LSTM Plot" = lstm_plot, "Mean Squared Error" = mse_lstm))
}
