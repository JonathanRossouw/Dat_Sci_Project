###### Cross Correlation Function

cross_cor_func <- function(data_target, data_var, title){
    # Plots Time Series in Levels
    data_plot <- data_var %>%
        ggplot() +
        theme_bw() +
        geom_line(aes(x = date, y = cases), colour = "deepskyblue4", alpha = 0.8) +
        labs(title = title, y = "cases") +
        scale_x_date(date_labels = "%m-%y")
    # Create CCF results
    ccf_dat <- ccf(data_target$cases, data_var$cases, plot = FALSE)
    # Store CCF data for plot
    ccf_dat <- data.frame(lag = ccf_dat$lag, ACF = ccf_dat$acf)
    # Create CCF Plot
    ccf_dat_plot <- ccf_dat %>%
        ggplot() +
        theme_bw() +
        geom_segment(aes(x = lag, xend = lag, y = 0, yend = ACF), colour = "gray") +
        geom_point(aes(x = lag, y = ACF), stat = "identity") +
        geom_hline(yintercept = c(0.1, -0.1), colour = "blue", alpha = 0.5, linetype = "dashed") +
        labs(title = glue(title, " CCF Plot"), y = "ACF")
    # Store results
    return(list(data_plot, ccf_dat_plot, "Maximum CCF" = ccf_dat %>% filter(ACF == max(ACF))))
}
