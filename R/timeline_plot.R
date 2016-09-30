
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

  bin_side   <- max( c(round(bin_width/2),  1))
  step_width <- max( c(round(bin_width/10), 1)) #arbitrary

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
#' @param d data for events
#' @param event_vars title of columns containing event data
#' @param event_labels Legend labels for the events
#' @param time_interval intervals you want for the x-axis. Options are "days", "weeks", "years". If an unknown value is seen it defaults to days.
#' @param max_time maximum time plotted
#' @param interval_width Size of rolling interval for sliding histogram
#' @param greyscale Want to output in greyscale?
#' @param custom_xbreaks If you want the have custom breaks on x axis to call out individual times
#' @param custom_xlabs labels for custom x breaks, must match in length.
#' @rarurn The sum of \code{x} and \code{y}.
#' @examples
#' timelinePlot(my_time_data)
#' @export
#'
timelinePlot <- function(d,
                               event_vars,            #column titles of the variables to be plotted
                               event_labels = NULL,
                               time_interval = "days",
                               max_time = NULL,
                               interval_width = NULL,
                               greyscale = F,
                               custom_xbreaks = NULL,
                               custom_xlabs = NULL){

  time_divider <-  ifelse(time_interval == "weeks", 7, ifelse(time_interval == "years", 365, 1))


  #move data into tidy/long format.
  tidy_data <- d[,c("id", event_vars)] %>%
    melt(id = "id") %>%
    na.omit() %>%
    mutate(value = value/time_divider)

  #What our plot will range from
  data_range <- c(0, max(tidy_data$value))

  #if the user gave us a max time lets use that for upper limit instead.
  if(is.numeric(max_time)) data_range[2] = max_time

  bin_width <- ifelse(!is.numeric(interval_width),
                      round((range(tidy_data$value)[2] - range(tidy_data$value)[1])/30),
                      interval_width)

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
    geom_ribbon(aes( ymax = slide_sum, ymin=0, fill = event), alpha = 0.3) +
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

    if(greyscale){
      plot <- plot +
        scale_fill_grey( start = 0,  end = .9, name = "Study Event") +
        scale_color_grey (start = 0, end = 0,  name = "Study Event")
    } else {
      plot <- plot +
        scale_fill_discrete(name = "Study Event") +
        scale_color_discrete(name = "Study Event")
    }

    #Do we have custom x-axis? If so, generate it, otherwise leave it at default
    if(is.vector(custom_xlabs) & !is.vector(custom_xbreaks)){
      stop("In order to have custom labels you need custom breaks too. ")
    }
    
    # if(length(custom_xlabs) != length(custom_xbreaks)){
    #   stop("Your labels must be the same length as your breaks.")
    # }

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

    #middle of the plot
    # size_pos <- 0.3 * (range(dist_data$slide_sum)[2] - range(dist_data$slide_sum)[1])
    plot + labs(x = sprintf("Time | Bin-Width: %s %s", bin_width, time_interval) )
}


timelinePlot(d, event_vars = c("gestage", "congestftlmp", "congest6wklmp", "SABlmp","lblmp"),time_interval = "day", 
             custom_xbreaks = c(100, 250, 420))

