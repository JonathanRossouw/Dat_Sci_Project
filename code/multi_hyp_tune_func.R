######### Multivariate Hyperparameter Tuning

multi_hyp_tune_func <- function(train_array, y_array, pred_array, data_actual,
                                hyperparams){
    # Training Function
    train_func <- function(hyperparams){
        # Set seed for reproducibility
        set_random_seed(seed = 2021)
        # Create Keras Sequential Model
        lstm_model <- keras_model_sequential()
        # Add Layers
        lstm_model %>%
            layer_lstm(units = 50, # size of the layer
                       batch_input_shape = c(1, 14, dim(train_array)[3]),
                       # batch size, timesteps, features
                       return_sequences = TRUE,
                       stateful = TRUE) %>%
            # fraction of the units to drop for the linear transformation of the inputs
            layer_dropout(rate = 0.1) %>%
            layer_lstm(units = 50, # size of the layer
                       return_sequences = TRUE,
                       stateful = TRUE) %>%
            # fraction of the units to drop for the linear transformation of the inputs
            layer_dropout(rate = 0.1) %>%
            time_distributed(keras::layer_dense(units = 1))
        # Set model parameters
        lstm_model %>%
            compile(loss = hyperparams$loss,
                    optimizer = hyperparams$optimizer,
                    metrics = "accuracy")
        # Fit Model
        lstm_model %>%
            fit(
                x = train_array,
                y = y_array,
                batch_size = 1,
                epochs = hyperparams$epochs,
                verbose = 0,
                shuffle = FALSE)
        # Predict cases
        lstm_forecast <- lstm_model %>%
            predict(pred_array, batch_size = 1) %>%
            .[, , 1]
        # Rescale and determine Performance
        lstm_forecast <- lstm_forecast %>%
            as_tibble() %>%
            mutate(cases = data_actual) %>%
            mutate(across(c(value, cases), ~(.x * (cases_min_max[2] -
                                                       cases_min_max[1])) +
                              cases_min_max[1])) %>%
            summarise(mean((cases - value)^2)) %>% .[[1]]
        # Store Results
        res <- hyperparams %>%
            as_tibble() %>%
            mutate(mse = lstm_forecast)
        return(res)
    }
    # Run iteratively through hyperparameter grid
    hyp_res <- map(hyperparams, train_func)
}
