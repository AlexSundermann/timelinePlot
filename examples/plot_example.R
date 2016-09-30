library(timelineViz)

#load data 
d <- read.csv("/Users/Nick/dev/timelineViz/data/GA_Data.csv", stringsAsFactors = F) 

timelinePlot <- function(data,
                         event_vars, #column titles of the variables to be plotted
                         event_labels = F, #Names as desired to appear in key, not neccesary. 
                         x_facet  = F, #Column title to facet on x axis
                         y_facet  = F, #Column title to facet on y axis. 
                         x_limits = F,
                         custom_xbreaks = F,
                         custom_xlabs = F,
                         time_interval = "days", 
                         title = "", 
                         alpha = 0.3){
  
  time_divider <-  ifelse(time_interval == "weeks", 7, ifelse(time_interval == "years", 365, 1))
  
  #move data into tidy/long format.
  tidy_data <- d[,c("id", event_vars)] %>% 
    melt(id = "id") %>% 
    na.omit() %>% 
    mutate(interval = value/time_divider)
  
  #Do we have custom x-axis? If so, generate it, otherwise leave it at default
  
  if(custom_xlabs & !custom_xbreaks){
    stop("In order to have custom labels you need custom breaks too. ")
  }
  
  if(length(custom_xlabs) != length(custom_xbreaks)){
    stop("Your labels must be the same length as your breaks.")
  }
  
  if(custom_xbreaks & !custom_xlabs){
    x_axis_gen <-  scale_x_continuous( breaks = custom_xbreaks, 
                        expand = c(0, 0))
  } else if(custom_xbreaks & custom_xlabs){
    x_axis_gen <- scale_x_continuous( breaks = custom_xbreaks, 
                        labels = custom_xlabs,
                        expand = c(0, 0))
  } else {
    x_axis_gen <- NULL
  }
  
  # x_limits = c(0,150/3) #delete this, just for testing
  if(x_limits){
    x_lims = xlim(x_limits)
  } else {
    x_lims = NULL
  }
  
  ggplot(tidy_data, aes(interval, group = variable)) + 
    labs(x = time_interval, y="Count", title = title) + 
    theme_bw() + 
    x_axis_gen +
    x_lims +
    scale_y_continuous( expand = c(0, 0) ) +
    # geom_freqpoly(aes(fill = variable, colour = variable, y = ..count..), bins = 100, alpha = alpha) 
    geom_density(kernel = "rectangular", aes(fill = variable, colour = variable, y = ..count..), alpha = alpha)
}

timelinePlot(d, event_vars = c("gestage", "congestftlmp", "congest6wklmp", "SABlmp","lblmp"),time_interval = "day")

vec <- rnorm(1000, 100, sd = 10)
#Width 
bin_width  <- 10
bin_side   <- bin_width/2
step_width <- 1


#Function takes a vector of data and a bin width and generates a sliding histogram from it. 
sliding_hist <- function(vec, bin_width, range = NA){
  # vec <- rnorm(1000, 100, sd = 10)
  # #Width 
  # bin_width  <- 10
  bin_side   <- max( c(round(bin_width/2), 1))
  step_width <- max(c(round(bin_width/10), 1)) #arbitrary
  
  #If the user didnt supply a range, make one from the data. 
  if(!is.na(range[1])) range <- c(min(vec), max(vec))
  
  bin_centers <- seq((range[1] - bin_side), (range[2] + bin_side), by = step_width)
  sum_vals <- c()

  #sapply this
  for(point in bin_centers){
    points_in_bin <- vec[vec > (point - bin_side) & vec < (point + bin_side)]
    sum_vals <- c(sum_vals,length(points_in_bin))
  }
  
  res <- data.frame(points = bin_centers, slide_sum = sum_vals)
  return(res)
}


#load data 
d <- read.csv("/Users/Nick/dev/timelineViz/data/GA_Data.csv", stringsAsFactors = F) 
library(dplyr)
library(tidyr)
library(reshape2)

timelinePlot_slide <- function(d,
                         event_vars,            #column titles of the variables to be plotted
                         time_interval = "days",
                         max_time = NA){
  
  time_divider <-  ifelse(time_interval == "weeks", 7, ifelse(time_interval == "years", 365, 1))
  
  #move data into tidy/long format.
  tidy_data <- d[,c("id", event_vars)] %>% 
    melt(id = "id") %>% 
    na.omit() %>% 
    mutate(value = value/time_divider)
  
  #What our plot will range from 
  data_range <- c(0, max(tidy_data$value))
  
  #if the user gave us a max time lets use that for upper limit instead. 
  if(!is.na(max_time[1])) data_range[2] = max_time
  
  #Now we find the unique events, roll through them and generate sliding hists for each. 
  #We then will append these to a big dataframe
  
  #Initialize the big holder for all the histogrammed data. 
  dist_data <- data.frame(time = numeric(), slide_sum = numeric(), event = character())
  
  for(event in unique(tidy_data$variable)){
    #Filter to the event we're looking at
    event_data <- tidy_data %>% 
      filter(variable == event)
    
    #Pass this event's data to the slidy hist function
    bin_width <- (range(event_data$value)[2] - range(event_data$value)[1])/20
   
    histified <- sliding_hist(event_data$value, bin_width, range = data_range) %>% 
      rename(time = points) %>% 
      mutate(event = as.character(event))
      
    dist_data <- dist_data %>% 
      bind_rows(histified)
  }
  
  return(dist_data)
}





tls <- timelinePlot_slide(d, event_vars = c("gestage", "congestftlmp", "congest6wklmp", "SABlmp","lblmp"),time_interval = "day")

ggplot(tls, aes(x = time, color = event)) +
  geom_ribbon(aes( ymax = slide_sum, ymin=0, fill = event), alpha = 0.3)

#Do we have custom x-axis? If so, generate it, otherwise leave it at default

if(custom_xlabs & !custom_xbreaks){
  stop("In order to have custom labels you need custom breaks too. ")
}

if(length(custom_xlabs) != length(custom_xbreaks)){
  stop("Your labels must be the same length as your breaks.")
}

if(custom_xbreaks & !custom_xlabs){
  x_axis_gen <-  scale_x_continuous( breaks = custom_xbreaks, 
                                     expand = c(0, 0))
} else if(custom_xbreaks & custom_xlabs){
  x_axis_gen <- scale_x_continuous( breaks = custom_xbreaks, 
                                    labels = custom_xlabs,
                                    expand = c(0, 0))
} else {
  x_axis_gen <- NULL
}

# x_limits = c(0,150/3) #delete this, just for testing
if(x_limits){
  x_lims = xlim(x_limits)
} else {
  x_lims = NULL
}

ggplot(tidy_data, aes(interval, group = variable)) + 
  labs(x = time_interval, y="Count", title = title) + 
  theme_bw() + 
  x_axis_gen +
  x_lims +
  scale_y_continuous( expand = c(0, 0) ) +
  # geom_freqpoly(aes(fill = variable, colour = variable, y = ..count..), bins = 100, alpha = alpha) 
  geom_density(kernel = "rectangular", aes(fill = variable, colour = variable, y = ..count..), alpha = alpha)


event_labels = F, #Names as desired to appear in key, not neccesary. 
x_facet  = F, #Column title to facet on x axis
y_facet  = F, #Column title to facet on y axis. 
x_limits = F,
custom_xbreaks = F,
custom_xlabs = F,

ggplot(res, aes(x = points, y = slide_sum)) + geom_line()


