### ARIMA Function

adf_func <- function(arima_data){
    # Wrangle data in levels
    arima_data <- arima_data %>%
        filter(date < as.Date("2021/05/27")) %>%
        mutate(cases = coalesce(cases, 0))
    # Plot data in levels
    levels_plot <- arima_data %>%
        ggplot() +
        theme_bw() +
        geom_line(aes(x = date, y = cases)) +
        labs(title = "Levels", y = "")
    # Wrangle data in first difference
    arima_data_diff_1 <- arima_data %>%
        filter(date < as.Date("2021/05/27")) %>%
        mutate(cases = coalesce(cases, 0)) %>%
        mutate(cases = (cases - lag(cases)))
    # Plot data in first differences
    dif_1_plot <- arima_data_diff_1 %>%
        ggplot() +
        theme_bw() +
        geom_line(aes(x = date, y = cases)) +
        labs(title = "First Differences", y = "")
    # Wrangle data in second differences
    arima_data_diff_2 <- arima_data %>%
        filter(date < as.Date("2021/05/27")) %>%
        mutate(cases = coalesce(cases, 0)) %>%
        mutate(cases = (cases - lag(cases))) %>%
        mutate(cases = (cases - lag(cases)))
    # Plot data in second differences
    dif_2_plot <- arima_data_diff_2 %>%
        ggplot() +
        theme_bw() +
        geom_line(aes(x = date, y = cases)) +
        labs(title = "Second Difference", y = "")
    # Create grid plot
    plot_grid_q <- quietly(plot_grid)
    diff_plots <- plot_grid_q(levels_plot, dif_1_plot, dif_2_plot, nrow = 3, ncol = 1)$result

    # Determine stationarity
    adf.test_q <- quietly(adf.test)
    lev_test <- adf.test_q(arima_data$cases)$result # Levels
    diff_1_test <- adf.test_q(arima_data_diff_1$cases[-1]) $result# First Difference
    diff_2_test <- adf.test_q(arima_data_diff_2$cases[-c(1,2)])$result # Second Difference
    # Store Results
    adf_test_res <- data.frame(test = c("Levels", "First Difference", "Second Difference"),
                               statistics = c(lev_test$statistic[1], diff_1_test$statistic[1], diff_2_test$statistic[1]),
                               p_values = c(lev_test$p.value, diff_1_test$p.value, diff_2_test$p.value))

    #### Plot ACFs and PACFs in Levels
    # Determine ACFs
    lev_acf <- acf(arima_data$cases, plot = FALSE)
    lev_acf_dat <- data.frame(lag = lev_acf$lag, ACF = lev_acf$acf)
    #Plot ACFs
    lev_acf_plot <- lev_acf_dat %>%
    ggplot() +
        theme_bw() +
        geom_segment(aes(x = lag, xend = lag, y = 0, yend = ACF), colour = "gray") +
        geom_point(aes(x = lag, y = ACF), stat = "identity") +
        geom_hline(yintercept = c(0.1, -0.1), colour = "blue", alpha = 0.5, linetype = "dashed") +
        labs(title = "Levels ACF Plot", y = "ACF")
    # Determine PACFs
    lev_pacf <- pacf(arima_data$cases, plot = FALSE)
    lev_pacf_dat <- data.frame(lag = lev_pacf$lag, PACF = lev_pacf$acf)
    #Plot PACFs
    lev_pacf_plot <- lev_pacf_dat %>%
        ggplot() +
        theme_bw() +
        geom_segment(aes(x = lag, xend = lag, y = 0, yend = PACF), colour = "gray") +
        geom_point(aes(x = lag, y = PACF), stat = "identity") +
        geom_hline(yintercept = c(0.1, -0.1), colour = "blue", alpha = 0.5, linetype = "dashed") +
        labs(title = "Levels PACF Plot", y = "PACF")

    #### Plot ACFs and PACFs in First Differences
    # Determine ACFs
    diff_1_acf <- acf(arima_data_diff_1$cases[-c(1)], plot = FALSE)
    diff_1_acf_dat <- data.frame(lag = diff_1_acf$lag, ACF = diff_1_acf$acf)
    #Plot ACFs
    diff_1_acf_plot <- diff_1_acf_dat %>%
        ggplot() +
        theme_bw() +
        geom_segment(aes(x = lag, xend = lag, y = 0, yend = ACF), colour = "gray") +
        geom_point(aes(x = lag, y = ACF), stat = "identity") +
        geom_hline(yintercept = c(0.1, -0.1), colour = "blue", alpha = 0.5, linetype = "dashed") +
        labs(title = "First Difference ACF Plot", y = "ACF")
    # Determine PACFs
    diff_1_pacf <- pacf(arima_data_diff_1$cases[-c(1)], plot = FALSE)
    diff_1_pacf_dat <- data.frame(lag = diff_1_pacf$lag, PACF = diff_1_pacf$acf)
    #Plot PACFs
    diff_1_pacf_plot <- diff_1_pacf_dat %>%
        ggplot() +
        theme_bw() +
        geom_segment(aes(x = lag, xend = lag, y = 0, yend = PACF), colour = "gray") +
        geom_point(aes(x = lag, y = PACF), stat = "identity") +
        geom_hline(yintercept = c(0.1, -0.1), colour = "blue", alpha = 0.5, linetype = "dashed") +
        labs(title = "First Difference PACF Plot", y = "PACF")
    #Determine Extended ACFs for first differences
    eacf_diff_1 <- eacf(arima_data_diff_1$cases[-c(1)], 5, 5)

    #### Plot ACFs and PACFs in Second Differences
    # Determine ACFs
    diff_2_acf <- acf(arima_data_diff_2$cases[-c(1, 2)], plot = FALSE)
    diff_2_acf_dat <- data.frame(lag = diff_2_acf$lag, ACF = diff_2_acf$acf)
    #Plot ACFs
    diff_2_acf_plot <- diff_2_acf_dat %>%
        ggplot() +
        theme_bw() +
        geom_segment(aes(x = lag, xend = lag, y = 0, yend = ACF), colour = "gray") +
        geom_point(aes(x = lag, y = ACF), stat = "identity") +
        geom_hline(yintercept = c(0.1, -0.1), colour = "blue", alpha = 0.5, linetype = "dashed") +
        labs(title = "Second Difference ACF Plot", y = "ACF")
    # Determine PACFs
    diff_2_pacf <- pacf(arima_data_diff_2$cases[-c(1, 2)], plot = FALSE)
    diff_2_pacf_dat <- data.frame(lag = diff_2_pacf$lag, PACF = diff_2_pacf$acf)
    #Plot PACFs
    diff_2_pacf_plot <- diff_2_pacf_dat %>%
        ggplot() +
        theme_bw() +
        geom_segment(aes(x = lag, xend = lag, y = 0, yend = PACF), colour = "gray") +
        geom_point(aes(x = lag, y = PACF), stat = "identity") +
        geom_hline(yintercept = c(0.1, -0.1), colour = "blue", alpha = 0.5, linetype = "dashed") +
        labs(title = "Second Difference PACF Plot", y = "PACF")
    #Determine Extended ACFs for second differences
    eacf_diff_2 <- eacf(arima_data_diff_2$cases[-c(1,2)], 5, 5)

    # Plot grid of ACFs and PACFs
    acf_pacf_plots <- plot_grid(lev_acf_plot, lev_pacf_plot, diff_1_acf_plot, diff_1_pacf_plot, diff_2_acf_plot, diff_2_pacf_plot, nrow = 3, ncol = 2)
    # Store results
    return(list("Augmented Dickey Fuller Test" = adf_test_res,
                "Levels, First and Second Difference Plots" = diff_plots,
                "ACF and PACF Plots" = acf_pacf_plots,
                "EACF Diff 1" = eacf_diff_1,
                "EACF Diff 2" = eacf_diff_2))
}

