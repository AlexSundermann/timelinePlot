#' Sliding Histogram
#'
#' Takes one dimensional data and returns a data frame in the form of x value and sum which are the x and y coordinates of the sliding histogram respectively.
#'
#' @param vec vector of numeric values.
#' @param bin_width width of bin to slide, same as histogram bin width
#' @param range where to start and stop sliding of bin.
#' @examples
#' sliding_hist(rnorm(100), bin_width = 0.1)
#' @export
#'
sliding_hist <- function(vec, bin_width, range){

  bin_side   <- bin_width/2
  step_width <- bin_width/10

  bin_centers <- seq(from = range[1], to = range[2], by = step_width)
  sum_vals    <- c()

  for(point in bin_centers){
    points_in_bin <- vec[vec > (point - bin_side) & vec < (point + bin_side)]
    sum_vals <- c(sum_vals,length(points_in_bin))
  }

  data.frame(points = bin_centers, slide_sum = sum_vals)
}
#' Timeline Plot
#'
#' Takes in wide format
#'
#' @param d Data for events. Data should be formatted such that rows represent different participants and columns are different variables. There must be a column named "id" that contains a unique identifier for each study participant.
#' @param event_vars Title of columns containing event timing data to be represented in the figure inputted as a vector. Variables describing event timing data should be formatted as the number of days between study event and time zero on the graph.
#' @param event_labels Vector of the labels for the event data in legend. Must be in same order as event_vars. Defaults to event_vars.
#' @param time_interval Units for time as seen on the x-axis. Options are "days", "weeks", "years". Default is set to days.
#' @param custom_x_interval allows for the assignment of a custom unit for the X-axis that is different than the time_interval unit
#' @param max_time Maximum time plotted. If not specified defaults to maximum seen event time.
#' @param interval_width Width of rolling interval for sliding histogram in number of days (even though interval_width is specified in days, it will be reported in whatever units are used for time_interval).
#' @param greyscale Plot the figure in greyscale
#' @param custom_xbreaks Allows for the additional of customizable breaks on the x-axis. Should be in units specified in "time_interval" (default unit is days). Input as a vector.
#' @param custom_xlabs Allows for customizable labels for custom_xbreaks, must match custom_xbreaks in vector length length.
#' @param lineart- Allows distribustions to be graphed as lines
#' @examples
#' timelinePlot(data, event_vars=c("days_to_FU1", "days_to_FU2",
#'   "days_to_disease", "days_to_death"),
#'   event_labels = c("Follow-Up 1", "Follow-Up 2", "Disease   Diagnosis", "Death"),
#'   time_interval = "weeks", interval_width=28,
#'   custom_xbreaks = c(0, 52, 104, 156, 208, 260),
#'       custom_xlabs = c("Enrollment", "", "104","", "208", ""))
#' @export
#'
timelinePlot <- function(d,
                         event_vars,             #column titles of the variables to be plotted
                         event_labels = NULL,
                         time_interval = "days",
                         custom_x_interval = time_interval,
                         max_time = NULL,
                         interval_width = NULL,
                         greyscale = F,
                         custom_xbreaks = NULL,
                         custom_xlabs = NULL,
                         lineart=F){

  time_divider <-  ifelse(time_interval == "weeks", 7, ifelse(time_interval == "years", 365, 1))

  #move data into tidy/long format.
  tidy_data <- d[,c("id", event_vars)] %>%
    melt(id = "id") %>%
    na.omit()

  bin_width <- ifelse(!is.numeric(interval_width), #no specified interval width
                      (range(tidy_data$value)[2] - range(tidy_data$value)[1])/30,
                      interval_width)

  #Convert to the units we are using (e.g. weeks) from days
  if(time_divider == 1){
    bin_width <- round(bin_width/time_divider,1) #if days we can be a little liberal with rounding.
  } else if(time_divider == 7) {
    bin_width <- round(bin_width/time_divider,3) #Weeks, less so
  } else {
    bin_width <- round(bin_width/time_divider,5) #Years... not so much rounding.
  }

  tidy_data <- tidy_data %>% #convert the main dataset too.
    mutate(value = value/time_divider)

  #What our plot will range from
  data_range <- c(0, max(tidy_data$value))

  #if the user gave us a max time lets use that for upper limit instead.
  if(is.numeric(max_time)) data_range[2] = max_time

  #Now we find the unique events, roll through them and generate sliding hists for each.
  #We then will append these to a big dataframe

  #Initialize the big holder for all the histogrammed data.
  dist_data <- data.frame(time = numeric(), slide_sum = integer(), event = character(),stringsAsFactors=FALSE)

  for(event in event_vars){

    #Filter to the event we're looking at
    event_data <- tidy_data %>%
      filter(variable == event)

    #Pass this event's data to the slidy hist function
    histified <- sliding_hist(event_data$value, bin_width, range = data_range) %>%
      rename(time = points) %>%
      mutate(event = event)

    dist_data <- dist_data %>%
      bind_rows(histified)
  }

  #If the user supplied us with labels, let's assign them to the column as factor labels for simplicity.
  if(is.vector(event_labels)){
    dist_data <- dist_data %>%
      mutate( event = factor(event, levels = event_vars, labels = event_labels ))
  }


  # Plotting stuff goes below here.

  plot <- ggplot(dist_data, aes(x = time, color = event)) +
    theme_bw() +
    #eliminates background, gridlines, and chart border
    theme(
      plot.background   = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,panel.border     = element_blank()
    ) +
    #draws x and y axis line
    theme(axis.line.x = element_line(color="black", size = 0.3),
          axis.line.y = element_line(color="black", size = 0.3)) +
    scale_y_continuous( expand = c(0,0))

  if(lineart){
    plot <- plot +
      geom_line(aes(linetype = event, y = slide_sum)) +
      scale_linetype(name = "Study Event")
  } else {
    plot <- plot +
      geom_ribbon(aes( ymax = slide_sum, ymin=0, fill = event), alpha = 0.3)
  }

  if(greyscale){
    plot <- plot +
      scale_fill_grey( start = 0,  end = .9, name = "Study Event") +
      scale_color_grey(start = 0, end = 0,  name = "Study Event", guide = "none")
  } else {
    plot <- plot +
      scale_fill_discrete(name = "Study Event") +
      scale_color_discrete(name = "Study Event")
  }
  plot <- plot + labs(
    y = "Frequency"
  )

  #Do we have custom x-axis? If so, generate it, otherwise leave it at default
  if(is.vector(custom_xlabs) & !is.vector(custom_xbreaks)){
    stop("In order to have custom labels you need custom breaks too. ")
  }


  if(is.vector(custom_xbreaks) & !is.vector(custom_xlabs)){
    plot <- plot + scale_x_continuous( breaks = custom_xbreaks,
                                       expand = c(0, 0))
  } else if(is.vector(custom_xbreaks) & is.vector(custom_xlabs)){
    plot <- plot + scale_x_continuous( breaks = custom_xbreaks,
                                       labels = custom_xlabs,
                                       expand = c(0, 0))
  } else {
    plot <- plot + scale_x_continuous( expand = c(0,0))
  }

  #delivered unto the loving user.
  plot + labs(x = sprintf("Time (in %s) | Bin-Width: %s %s",custom_x_interval, bin_width, time_interval) )
}
