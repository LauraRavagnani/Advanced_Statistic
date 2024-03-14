library(tidyverse)
library(ggplot2)

# 1)
#read the data and import them in a data.frame
american <- read.table("american_airline_empl.txt", header=T)		
delta <- read.table("delta_airline_empl.txt", header=T)
federal <- read.table("federal_express_empl.txt", header=T)
united <- read.table("united_airline_empl.txt", header=T)

# remove the "," as a thousand separator
delta$Full.time<- as.numeric(gsub(",", "", delta$Full.time))
american$Full.time <- as.numeric(gsub(",", "", american$Full.time))		
federal$Full.time<- as.numeric(gsub(",", "", federal$Full.time))
united$Full.time<- as.numeric(gsub(",", "", united$Full.time))

american$Part.time<- as.numeric(gsub(",", "", american$Part.time))
delta$Part.time<- as.numeric(gsub(",", "", delta$Part.time))
federal$Part.time<- as.numeric(gsub(",", "", federal$Part.time))
united$Part.time<- as.numeric(gsub(",", "", united$Part.time))

american$Grand.Total<- as.numeric(gsub(",", "", american$Grand.Total))
delta$Grand.Total<- as.numeric(gsub(",", "", delta$Grand.Total))
federal$Grand.Total<- as.numeric(gsub(",", "", federal$Grand.Total))
united$Grand.Total<- as.numeric(gsub(",", "", united$Grand.Total))

head(american)


# 2)
# create a new column "company" in each dataframe
american$company <- c(rep("american", nrow(american)))
delta$company <- c(rep("delta", nrow(delta)))
federal$company <- c(rep("federal", nrow(federal)))
united$company <- c(rep("united", nrow(united)))

# merge the dataframes
df <- rbind(american, delta, federal, united)


# 3)
# create the date column		
df$time <- as.Date(with(df, paste(Year, Month, Month, sep="-")), "%Y-%m-%d")

# Create ggplot2 plot 
ggp_ft <- ggplot(df, aes(time, Full.time, col = company)) + geom_line() + theme_bw()          
ggp_pt <- ggplot(df, aes(time, Part.time, col = company)) + geom_line() + theme_bw()        
ggp_ft    
ggp_pt  


# 4)
# create two dataframes with the month and year of minumun/maximum employees for each company
min_empl <- df %>% group_by(company) %>% summarise(month = Month[which.min(Grand.Total)], year = Year[which.min(Grand.Total)], .groups = 'drop')
max_empl <- df %>% group_by(company) %>% summarise(month = Month[which.max(Grand.Total)], year = Year[which.max(Grand.Total)], .groups = 'drop')

print(paste("American airline reached the minimum number of employees on: ", min_empl$month[1], "-", min_empl$year[1]))
print(paste("American airline reached the maximum number of employees on: ", max_empl$month[1], "-", max_empl$year[1]))

print(paste("Delta airline reached the minimum number of employees on: ", min_empl$month[2], "-", min_empl$year[2]))
print(paste("Delta airline reached the maximum number of employees on: ", max_empl$month[2], "-", max_empl$year[2]))

print(paste("Federal airline reached the minimum number of employees on: ", min_empl$month[3], "-", min_empl$year[3]))
print(paste("Federal airline reached the maximum number of employees on: ", max_empl$month[3], "-", max_empl$year[3]))

print(paste("United airline reached the minimum number of employees on: ", min_empl$month[4], "-", min_empl$year[4]))
print(paste("United airline reached the maximum number of employees on: ", max_empl$month[4], "-", max_empl$year[4]))


# 5) 
# create a new column "fraction" in the dataframe
df$fraction <- df$Part.time / df$Grand.Total

# create ggplot2 plot 
ggp_frac <- ggplot(df, aes(time, fraction, col = company)) + geom_line() + theme_bw()        
ggp_frac
