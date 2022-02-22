###### Simple LSTM Array Function

data_array_func <- function(data, initial, assess, skip){
    # Select Appropriate Data
    data_train <- data %>%
        dplyr::select(date, cases) %>% head((nrow(data) - 14))
    # Creating Rolling Data Splits
    data_rol <- rolling_origin(data_train,
                            initial = initial,
                            assess = assess,
                            skip = skip,
                            cumulative = FALSE)
    # Create Training Predictor Array
    x_train_array <- lapply(data_rol$splits, FUN = function(X){analysis(X)$cases})
    x_train_array <- do.call("rbind", x_train_array) %>%
        array(., dim = c(length(x_train_array), initial, 1))
    # Create Training Target Array
    y_train_array <- lapply(data_rol$splits, FUN = function(X){testing(X)$cases})
    y_train_array <- do.call("rbind", y_train_array) %>%
        array(., dim = c(length(y_train_array), assess, 1))
    # Create Training Validation Set
    x_val_array <- data %>% slice(., (n() - 27):(n() - 13)) %>% .$cases %>%
        array(., dim = c(1, assess, 1))
    # Create Target Validation Set
    y_val_array <- data$cases %>% tail(., n = 14) %>%
        array(., dim = c(1, assess, 1))

    return(list(x_train_array = x_train_array,
                y_train_array = y_train_array,
                x_val_array = x_val_array,
                y_val_array = y_val_array))
}
