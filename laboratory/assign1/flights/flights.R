library(nycflights13)
library(ggplot2)
library(tidyverse)

# 1.1)
# create a new column in "flights" with the date of departure
flights$date <- as.Date(with(flights, paste(day, month, year, sep="-")), "%d-%m-%Y")

# create a new data.frame with 3 columns: origin airport, date and number of flights
from_ny <- flights %>% count(origin, date)

# plot of the total number of flights departed from each of the three NYC airports as a function of time 
ggp_from_ny <- ggplot(from_ny, aes(date, n, col = origin)) + geom_point() + theme_bw() + theme(legend.position="top") + theme(legend.title = element_text(size=10, face="bold"))
ggp_from_ny


# 1.2)
# create a function that gives the number of the week from the date
count_week <- function(x){
    for (i in 1:length(x)) {
        if (x[i] == "2013-12-30" || x[i] == "2013-12-31"){
            n <- 53
        }
        else {
            n <- isoweek(x[i])
        }
    } 
    return(n)
}


# create 2 new columns in "flights" with the number of the day and the number of the week 
flights$weekday <- wday(flights$date, week_start = 1)
flights$week_num <- count_week(flights$date)

#divide the working days from the weekend
working_day <- filter(flights, weekday %in% c(1,2,3,4,5))
weekend_day <- filter(flights, weekday %in% c(6,7))

# calculate the mean number of flights for every week
working_day <- working_day %>% count(week_num, weekday) %>% group_by(week_num) %>% summarise(mean=mean(n), .groups = 'drop')
weekend_day <- weekend_day %>% count(week_num, weekday) %>% group_by(week_num) %>% summarise(mean=mean(n), .groups = 'drop')

# new data frames to create a single plot 
#average <- data.frame(week = working_day$week_num,                            
#                      mean = c(working_day$mean, weekend_day$mean),
#                      period = c(rep("working day", nrow(working_day)), rep("weekend", nrow(weekend_day))))

# create plot
# create plot
ggp_average_working <- ggplot(working_day, aes(week_num, mean)) + geom_line(color = "red") + theme_bw() + ggtitle("Average number of flights in working days") + labs(y = "n", x = "week")
ggp_average_weekend <- ggplot(weekend_day, aes(week_num, mean)) + geom_line(color = "blue") + theme_bw() + ggtitle("Average number of flights in the weekend") + labs(y = "n", x = "week")  
ggp_average_working
ggp_average_weekend


# 2.1)
# create a new tibble with 3 columns: date, origin airport and min/max/average delay
min_delay <- flights %>% group_by(date, origin) %>% summarise(min = dep_delay[which.min(dep_delay)], .groups = 'drop')
max_delay <- flights %>% group_by(date, origin) %>% summarise(max = dep_delay[which.max(dep_delay)], .groups = 'drop')
average_delay <- flights %>% group_by(date, origin) %>% summarise(average = mean(dep_delay, na.rm = TRUE), .groups = 'drop')

# create plot
ggp_min_delay <- ggplot(min_delay, aes(date, min, col = origin)) + geom_line() + theme_bw() + theme(legend.position="top") 
ggp_max_delay <- ggplot(max_delay, aes(date, max, col = origin)) + geom_line() + theme_bw() + theme(legend.position="top") 
ggp_average_delay <- ggplot(average_delay, aes(date, average, col = origin)) + geom_line() + theme_bw() + theme(legend.position="top") 
ggp_min_delay
ggp_max_delay
ggp_average_delay


# 3)
# add a new column with the average speed of every flight and calculate the mean for every day
flights$speed <- flights$distance / flights$air_time 
speed <- flights %>% group_by(date) %>% summarise(average = mean(speed, na.rm = TRUE), .groups = 'drop')

# create plot
ggp_speed <- ggplot(speed, aes(date, average)) + geom_line() + theme_bw()
ggp_speed


# 4.1)
# create two tibbles with date, company and number of flights of every company per day
company_day <- subset(flights, select = c(carrier, date)) %>% count(carrier, date)
company_day <- company_day[with(company_day, order(date, n)),]

company_week <- subset(flights, select = c(carrier, week_num)) %>% count(carrier, week_num) 
company_week <- company_week[with(company_week, order(week_num, n)),]

# create a new tibble with 3 columns: date/number of the week, origin airport and largest two numbers of flights
first_two_per_day <- company_day %>% group_by(date) %>% summarise(max_2 = n[length(n) - 1], company_2 = carrier[length(n) - 1], max = n[length(n) ], company= carrier[length(n)], .groups = 'drop')
first_two_per_week <- company_week %>% group_by(week_num) %>% summarise(max_2 = n[length(n) - 1], company_2 = carrier[length(n) - 1], max = n[length(n)], company= carrier[length(n)], .groups = 'drop')

# new data frames to create a single plot 
first_two_per_day <- data.frame(date = first_two_per_day$date,                            
                      max_two=c(first_two_per_day$max, first_two_per_day$max_2),
                      company = c(first_two_per_day$company, first_two_per_day$company_2))
first_two_per_week <- data.frame(week = first_two_per_week$week_num,                            
                      max_two = c(first_two_per_week$max, first_two_per_week$max_2),
                      company = c(first_two_per_week$company, first_two_per_week$company_2))

# create plot
ggp_first_two_per_day <- ggplot(first_two_per_day, aes(date, max_two, col = company)) + geom_point() + theme_bw() + theme(legend.position="top")
ggp_first_two_per_week <- ggplot(first_two_per_week, aes(week, max_two, col = company)) + geom_point() + theme_bw() + theme(legend.position="top")
ggp_first_two_per_day
ggp_first_two_per_week


# 4.2)
# create a new data.frame with 3 columns: origin airport, month and number of flights
company_month <- flights %>% count(month, carrier) 

# create a new tibble with 3 columns: month, origin airport and min/max/average delay
min_per_month <- company_month %>% group_by(month) %>% summarise(min = n[which.min(n)], company = carrier[which.min(n)], .groups = 'drop')

# create plot
ggp_min_per_month <- ggplot(min_per_month, aes(month, min, col = company)) + geom_point() + theme_bw() + theme(legend.position="top")
ggp_min_per_month


# 4.3)
distance_month <- subset(flights, select = c(carrier, month, distance))
distance_month<- distance_month %>% group_by(month) %>% summarise(max_distance = distance[which.max(distance)], company = carrier[which.max(distance)], .groups = 'drop')






        
