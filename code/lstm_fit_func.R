##### LSTM Fit Function

lstm_fit_func <- function(data_train,
                          data_predict,
                          epochs,
                          optimizer,
                          loss){
    # Set seed for reproducibility
    set_random_seed(seed = 2021)
    # Create Keras Sequential Model
    lstm_model <- keras_model_sequential()
    # Add Layers
    lstm_model %>%
        layer_lstm(units = 50, # size of the layer
                   batch_input_shape = c(1, 14, 1),
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
        compile(loss = loss, optimizer = optimizer, metrics = "accuracy")
    # Fit Model
    lstm_model %>%
        fit(
        x = data_train$x_train_array,
        y = data_train$y_train_array,
        batch_size = 1,
        epochs = epochs,
        verbose = 0,
        shuffle = FALSE)
    # Predict cases
    lstm_forecast <- lstm_model %>%
        predict(data_predict, batch_size = 1) %>%
        .[, , 1]
    # Store Results
    return(list("Model Performance" = lstm_model,
                "Predicted Values" = lstm_forecast))
}
