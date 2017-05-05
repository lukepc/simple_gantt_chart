library(reshape2)
library(ggplot2)
library(lubridate)

## Imports a .csv file with the following columns: 
# task - Name of the task
# start_month - Starting month (Integer from 1-12)
# start_year - Starting year (YYYY)
# length - How many months (Integer)
# status - The status of the task (eg. Complete, In Progress)
dates <- read.csv("example_gantt.csv")

## Converts the date columns to R date objects
dates$r_sdate <- as.Date(paste(01, dates$start_month, dates$start_year, sep = "-"), "%d-%m-%Y")
dates$r_edate <- dates$r_sdate %m+% months(dates$length)

# Creates bimonthly tick markers
years <- append(unique(dates$start_year), unique(dates$start_year)[-1] + 1)
months <- seq(1,12, by = 2)
years <- (lapply(years, FUN =function(x) {rep(x, 6)}))
tickdates <- as.Date(paste(1, months, unlist(years), sep = "/"), "%d/%m/%Y")


# Reorders the tasks according to starting date and duration
dates<-dates[order(dates$r_sdate, dates$length),]

# Assigns an order index
dates$order <- nrow(dates):1

# Melts the data to long format for start/end dates
mdates <- melt(dates, measure.vars = c("r_sdate", "r_edate"))


# Plots them using the start and end date values to 
ggplot(mdates, aes(x=value, y = reorder(task, order), col = status))+
  scale_colour_grey(start = 0.8, end = 0)+
  geom_line(size = 10.5) + 
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()+
  theme(
    legend.position = c(0.8,0.8), 
    legend.margin = margin(3,3,3,1, "cm"),
    legend.text = (element_text(size = 32)),
    legend.key.size = unit(1.5, "cm"),
    legend.title = (element_blank()),
    axis.text.x = element_text(angle  = 45, hjust = 1, size = 15),
    axis.text.y = element_text(size = 15),
    panel.grid.major.x = element_line(size = 0.5, color = "grey90")
  ) +
  scale_x_date(breaks = tickdates,date_labels = "%b-%Y")
