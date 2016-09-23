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

range <- c(min(vec), max(vec))
bin_centers <- seq((range[1] + bin_side), (range[2] - bin_side), by = step_width)
sum_vals <- c()
#use the optimum bin width algorithm at default. 

#sapply this
for(point in bin_centers){
  points_in_bin <- vec[vec > (point - bin_side) & vec < (point + bin_side)]
  sum_vals <- c(sum_vals,length(points_in_bin))
}
res <- data.frame(points = bin_centers, slide_sum = sum_vals)

ggplot(res, aes(x = points, y = slide_sum)) + geom_line()
