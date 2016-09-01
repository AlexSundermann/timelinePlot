
#' Timeline Plot
#' 
#' Takes data in with at least two columns- the measurement variable- and the time from baseline to that value.
#' 
#' @param time_data dataframe with at least two columns, what's being measured and the time of that measurement.
#' @param x_limits Limits of the x-axis. Defaults to ggplot's scaling but if you want to zoom in or out do it here.
#' @param variable_name The name of the column containing the  name of the measurement
#' @param interval_val_name The name of the column containing the time to measurement 
#' @param time_interval intervals you want for the x-axis. Options are "days", "weeks", "years". If an unknown value is seen it defaults to days.
#' @param title What you want displayed in the title section for the plot.  
#' @param alpha Opacity level of the curves. 
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' timelinePlot(my_time_data)
#' @export
timelinePlot <- function(time_data,
                         x_limits = NA,
                         variable_name = "variable", 
                         interval_val_name = "value", 
                         time_interval = "days", 
                         title = "", 
                         alpha = 0.3){
  
  interval_maker = function(column, type){
    #currently just takes, weeks, days, years
    ifelse(type == "weeks", column/7,
           ifelse(type == "years", column/365, column))
  }
  
  #get the data into the form that ggplot wants it
  plot_data <- time_data %>% 
    rename_(value = interval_val_name, variable = variable_name) %>% 
    mutate(interval = interval_maker(value, time_interval))
  
  #Do we have custom x-axis? If so, generate it, otherwise leave it at default
  
  # x_limits = c(0,150/3) #delete this, just for testing
  if(!is.na(x_limits)){
    x_axis_gen = scale_x_continuous( limits = x_limits, expand = c(0, 0))
  } else {
    x_axis_gen = scale_x_continuous(expand = c(0, 0))
  }
  
  
  ggplot(plot_data, aes(value, group = variable)) + 
    labs(x = time_interval, y="Count", title = title) + 
    theme_bw() + 
    x_axis_gen +
    scale_y_continuous( expand = c(0, 0) ) +
    geom_density(aes(fill = variable,colour =variable, y = ..count..), alpha = alpha) 
}