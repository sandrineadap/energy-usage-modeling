# Create a table summarizing the average energy consumption in 2022 and 2023 (rounded to two decimal places) 
# by hour, day type (weekends/holidays vs weekday), and Time-of-Use season, 
# ordered by season, day type, then hour.

# install.packages("tidyverse")  # if not already installed
library(tidyverse)  # includes dplyr, tidyr, ggplot2, lubridate
                    # https://www.tidyverse.org/packages/

########################################################
# PREPROCESSING
########################################################

############## LOAD DATA

# load data from csv's in working directory
getwd()
usage <- read.csv('./data/EnergyUsage-2022-01-01-2022-12-31.csv')
usage_2023 <- read.csv('./data/EnergyUsage-2023-01-01-2023-12-31.csv')

# put all energy data into usage dataframe
usage <- rbind(usage, usage_2023)

# usage <- usage %>% distinct()

# remove unneeded dataframes
ls()
rm(usage_2023)

# load holiday schedule for 2022, 2023
holidays <- read.csv('./data/Holiday_Schedule_2022.csv')
holidays_2023 <- read.csv('./data/Holiday_Schedule_2023.csv')

########################################################
# contents of ./data/Holiday_Schedule_2022.csv
# Date,Holiday
# "Monday, January 3, 2022",New Year�s Day
# "Monday, February 21, 2022",Family Day
# "Friday, April 15, 2022",Good Friday
# "Monday, May 23, 2022",Victoria Day
# "Friday, July 1, 2022",Canada Day
# "Monday, August 1, 2022",Civic Holiday
# "Monday, September 5, 2022",Labour Day
# "Monday, October 10, 2022",Thanksgiving Day
# "Monday, December 26, 2022",Christmas Day
# "Tuesday, December 27, 2022",Boxing Day
########################################################
# contents of ./data/Holiday_Schedule_2023.csv
# Date,Holiday
# "Monday, January 2, 2023",New Year's Day
# "Monday, February 20, 2023",Family Day
# "Friday, April 7, 2023",Good Friday
# "Monday, May 22, 2023",Victoria Day
# "Monday, July 3, 2023",Canada Day
# "Monday, August 7, 2023",Civic Holiday
# "Monday, September 4, 2023",Labour Holiday
# "Monday, October 9, 2023",Thanksgiving Day
# "Monday, December 25, 2023",Christmas Day
# "Tuesday, December 26, 2023",Boxing Day
########################################################



############## CLEANING THE DATA

# HOLIDAYS: check for Date format
str(holidays)       # Date is character
str(holidays_2023)  # Date is character

# convert Date columns to R format Date
## separate Date column
# View(holidays)
holidays <- holidays %>%
  separate(Date, into=c('Day_of_Week', 'Date', 'Year'), sep=', ', remove=T, convert=T)
holidays <- holidays %>% 
  separate(Date, into=c('Month', 'Day'), sep=" ", remove=T, convert=T)

## convert month names to integers
month_nums <- match(holidays$Month, month.name)
class(month_nums)
holidays <- holidays %>% mutate(Month = month_nums)
class(holidays$Month)

## concatenate Year, Month, and Day Columns
holidays <- holidays %>% mutate(Date = make_datetime(Year, Month, Day), .before=2)
# View(holidays)

# remove Month, Day, Year columns
holidays <- holidays %>%
  select(-c(Month, Day, Year))

# convert Date columns in holidays_2023

## separate Date column
holidays_2023 <- holidays_2023 %>%
  separate(Date, into=c('Day_of_Week', 'Date', 'Year'), sep=', ', remove=T, convert=T)
holidays_2023 <- holidays_2023 %>%
  separate(Date, into=c('Month', 'Day'), sep=" ", remove=T, convert=T)

## convert month names to integers
month_nums <- match(holidays_2023$Month, month.name)
str(month_nums)
holidays_2023 <- holidays_2023 %>% mutate(Month = month_nums)
class(holidays_2023$Month)

## concatenate Year, Month, and Day Columns
holidays_2023 <- holidays_2023 %>% mutate(Date = make_datetime(Year, Month, Day), .before=2)
# View(holidays_2023)

# remove Month, Day, Year columns
holidays_2023 <- holidays_2023 %>%
  select(-c(Month, Day, Year))

# put all holiday data into holidays dataframe
holidays <- rbind(holidays, holidays_2023)

str(holidays)

# convert R date to lubridate
holidays <- holidays %>%
  mutate(Date = ymd(Date))
# View(holidays)

# remove unneeded dataframes
rm(holidays_2023)
rm(month_nums)

# USAGE: separate Energy.consumption.time.period into
# Date_start, Time_start, Date_end, Time_end
# View(usage)

## first separate the starting and ending dates from `Energy.consumption.time.period``
usage <- usage %>%
  separate(Energy.consumption.time.period, 
           into=c('Date_start', 'Date_end'),
           sep=' to ', remove=T, convert=T)

## Starting dates: separate starting date into its units of time
usage <- usage %>%
  separate(Date_start, 
           into=c('Year_start', 'Month_start', 'Day_start', 'Time_start'),
           sep=' ', remove=T, convert=T)

## combine them to datetime format
usage <- usage %>% 
  mutate(Date_start = make_datetime(Year_start, Month_start, Day_start), .before=1)
str(usage$Date_start)

## Ending dates: separate starting date into its units of time
usage <- usage %>%
  separate(Date_end, 
           into=c('Year_end', 'Month_end', 'Day_end', 'Time_end'),
           sep=' ', remove=T, convert=T)

## combine them into datetime format
usage <- usage %>% 
  mutate(Date_end = make_datetime(Year_end, Month_end, Day_end), .before=6)
str(usage$Date_end)

## Remove unnecessary columns
usage <- usage %>%
  select(-c(Day_start, Year_end, Month_end, Day_end))

# View(usage)
str(usage)

# Change start times from 12-hour to 24-hour
usage_dt24 <- usage   # dt24 = for DateTime 24
str(usage_dt24)

## create AM/PM column
suffixes <- rep(c('AM', 'PM'), each=12)
usage_dt24 <- usage_dt24 %>%
  mutate(suffix = rep_len(suffixes, length.out=nrow(usage_dt24)), .before=3)
# View(usage_dt24)

## Modify starting AM/PM for Daylight Savings time (March and November)
## find which rows Daylight Savings time starts & ends (the first occurence)

### 2022
dst_start_2022 <- 
  which(usage_dt24$Date_start == as.Date('2022-03-13') & 
      usage_dt24$Time_start == '03:00')[1] # row 1731

dst_end_2022 <- 
  which(usage_dt24$Date_start == as.Date('2022-11-06') & 
      usage_dt24$Time_start == '01:00')[1] # row 7442

### 2023
dst_start_2023 <-
  which(usage_dt24$Date_start == as.Date('2023-03-12') & 
      usage_dt24$Time_start == '03:00')[1] # row 10491

dst_end_2023 <-
  which(usage_dt24$Date_start == as.Date('2023-11-05') & 
      usage_dt24$Time_start == '01:00')[1] # row 16202

## modify AM/PM between those dates
suffixes_EDT <- c(rep('AM', each=11), rep('PM', each=12), rep('AM', each=1))

usage_dst <- usage_dt24 # dst = daylight savings time
dstRows <- c(dst_start_2022:dst_end_2022) # daylight savings time rows for 2022
# print(dstRows)

### 2022
### if the current row number is dstRows, replace suffix with suffixes_EDT.
### if not, keep current suffix
usage_dst <- usage_dst %>%
  mutate(suffix = ifelse(row_number() %in% dstRows, suffixes_EDT, suffix))
# View(usage_dst)

### 2023
dstRows <- c(dst_start_2023:dst_end_2023) # daylight savings time rows for 2023
usage_dst <- usage_dst %>%
  mutate(suffix = ifelse(row_number() %in% dstRows, suffixes_EDT, suffix))
# View(usage_dst)

## merge (paste) AM/PM column to Time_start column
usage_dst <- usage_dst %>%
  mutate(Time_start = paste(Time_start, suffix)) %>%
  mutate(suffix = NULL)
# View(usage_dst)

## convert to 24-hour
usage_dst <- usage_dst %>%
  mutate(Time_start = strftime(strptime(Time_start, format="%I:%M %p"), format="%H:%M"))
class(usage_dst$Time_start)

# combine Date_start and Time_start into DT_start 
# (so time can be used for lubridate manipulations)
usage_dst <- usage_dst %>%
  mutate(DT_start = paste(Date_start, Time_start), .before=1)
class(usage_dst$DT_start)

## convert DT_start to DateTime
usage_dst <- usage_dst %>% 
  mutate(DT_start = ymd_hm(DT_start))
class(usage_dst$DT_start)
# View(usage_dst)

# Change ending times from 12-hour to 24-hour
usage_ends <- usage_dst
suffixes <- c(rep('AM', each=11), rep('PM', each=12), 'AM')

## create suffix column for ending times
usage_ends <- usage_ends %>%
  mutate(suffix_end = rep_len(suffixes, length.out=nrow(usage_ends)), .before=8)
# View(usage_ends)

## Modify ending suffixes for Daylight Savings time (March and November)
suffixes_EDT <- c(rep('AM', each=10), rep('PM', each=12), rep('AM', each=2))

### 2022
dstRows <- c(dst_start_2022:dst_end_2022) # daylight savings time rows for 2022
usage_ends <- usage_ends %>%
  mutate(suffix_end = ifelse(row_number() %in% dstRows, suffixes_EDT, suffix_end))

### 2023
dstRows <- c(dst_start_2023:dst_end_2023) # daylight savings time rows for 2023
usage_ends <- usage_ends %>%
  mutate(suffix_end = ifelse(row_number() %in% dstRows, suffixes_EDT, suffix_end))

# View(usage_ends)

## merge (paste) AM/PM column to Time_start column
usage_ends <- usage_ends %>%
  mutate(Time_end = paste(Time_end, suffix_end)) %>%
  mutate(suffix_end = NULL)
# View(usage_ends)

## convert to 24-hour
usage_ends <- usage_ends %>%
  mutate(Time_end = strftime(strptime(Time_end, format="%I:%M %p"), format="%H:%M"))

# combine Date_end and Time_end into DT_end 
# (so time can be used for lubridate manipulations)
usage_ends <- usage_ends %>% 
  mutate(DT_end = paste(Date_end, Time_end), .before=2)

## convert DT_end to DateTime
usage_ends <- usage_ends %>% 
  mutate(DT_end = ymd_hm(DT_end))
class(usage_ends$DT_end)
# View(usage_ends)

# Rename Usage..kilowatt.hours. to Usage_kWh
# Also, remove unneeded/repetitive columns
usage_clean <- usage_ends %>%
  rename(Usage_kWh = Usage..kilowatt.hours.) %>%
  select(c(DT_start, DT_end, Usage_kWh))

usage_clean <- usage_clean %>% distinct()

# View(usage_clean)

############## PREPARE DATA FOR ANALYSIS

# Create day_type column (Weekend/Holiday vs Weekday) 

## Create Date column w/ Date class so we can join to `holidays` by Date
usage_daytype <- usage_clean %>%
  mutate(Date = date(DT_start), .before=2)

str(usage_daytype$Date)

# Create day_type column
## Left join `holidays` to `usage_daytype` on Date
usage_daytype <- left_join(usage_daytype, holidays[c('Date', 'Holiday')], by="Date")

usage_daytype <- usage_daytype %>%
  mutate(Holiday = case_when(
    is.na(Holiday) ~ FALSE,
    .default = TRUE
  )) %>%
  rename(is.Holiday = Holiday)

usage_daytype <- usage_daytype %>%
  mutate(day_type = case_when(
    # if Sunday or Saturday or Holiday, day_type = Weekend/Holiday
    wday(DT_start) == 1 | wday(DT_start) == 7 | is.Holiday ~ 'Weekend/Holiday',
    # else, day_type = Weekday
    .default = 'Weekday'
  ), .after=2)

# convert day_type to factor
usage_daytype$day_type <- as.factor(usage_daytype$day_type)

str(usage_daytype)

# Create season column (Winter vs Summer)
usage_season <- usage_daytype %>%
  mutate(season = case_when(
    # if month in Nov-Apr (inclusive), Winter; else, Summer
    month(DT_start) >= 11 | month(DT_start) <= 4 ~ 'Winter',
    .default = 'Summer'
  ), .after=2)
  
# convert season to factor
usage_season$season <- as.factor(usage_season$season)

str(usage_season)

# Create hour column
usage_hour <- usage_season %>%
  mutate(hour = hour(DT_start), .after=4)

# make sure time zone is Toronto time for proper time filtering
attr(usage_hour$DT_start, "tzone")
usage_hour <- usage_hour %>%
  mutate(DT_start = force_tz(DT_start, tz = "America/Toronto"))
attr(usage_hour$DT_start, "tzone")

# remove rows before 2022-01-01 and after 2023-12-31 (if any)
usage_filtered <- usage_hour %>%
  filter(DT_start >= as.POSIXct('2022-01-01 00:00:00', tz = 'America/Toronto') & 
           DT_start <= as.POSIXct('2023-12-31 23:59:59', tz = 'America/Toronto'))

# View(usage_filtered)

########################################################
# ANALYSIS
########################################################

# get average energy consumption by hour, day type, and season
usage_summary <- usage_filtered %>%
  group_by(season, day_type, hour) %>%
  summarize(avg_Usage_kWh = round(mean(Usage_kWh), 2)) %>%
  arrange(season, hour, day_type)
usage_summary

View(usage_summary)

# # write data to csv
write.csv(usage_summary,"./usage_summary.csv", row.names = FALSE) 
  
# print ('CSV created Successfully :)')

####### graph
library(ggplot2)

# Example: line graph of avg usage by hour, colored by day type, faceted by season
ggplot(usage_summary, aes(x = hour, y = avg_Usage_kWh, color = day_type)) +
  geom_line(size = 1.2) +
  facet_wrap(~season) +                     # separate panels for each season
  scale_x_continuous(breaks = 0:23) +      # show all hours on x-axis
  labs(
    title = "Average Hourly Electricity Usage 2022-2023",
    x = "Hour of Day",
    y = "Average kWh",
    color = "Day Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )

############## divide graph into coloured areas representing peak, mid-peak, off-peak hours
# Data frame defining the seasonal time-of-use periods
tou_periods <- data.frame(
  season = rep(c("Summer", "Winter"), each = 5),
  xmin = c(19, 0, 7, 11, 17,   # Summer times
           19, 0, 7, 11, 17),  # Winter times
  xmax = c(23, 7, 11, 17, 19,   # Summer times
           23, 7, 11, 17, 19),  # Winter times
  period = factor(
    c("Off-Peak", "Off-Peak", "Mid-Peak", "On-Peak", "Mid-Peak",  # Summer periods
      "Off-Peak", "Off-Peak", "On-Peak", "Mid-Peak", "On-Peak"), # Winter periods
    levels = c("On-Peak", "Mid-Peak", "Off-Peak")
  )
)

# Define colors for the periods
tou_colors <- c("On-Peak" = "salmon", "Mid-Peak" = "gold", "Off-Peak" = "lightgreen")


# The Plot
ggplot(usage_summary, aes(x = hour, y = avg_Usage_kWh, color = day_type)) +

  # --- MODIFICATION ---
  # Correctly specify the data source for the rectangles
  geom_rect(
    data = tou_periods, # FIX: Directly use the tou_periods data frame
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period),
    inherit.aes = FALSE,
    alpha = 0.4
  ) +
  # --- END MODIFICATION ---

  geom_line(size = 1.1) +
  facet_wrap(~season) +
  scale_x_continuous(breaks = seq(0, 23, by = 2), expand = c(0, 0)) +
  scale_fill_manual(values = tou_colors, name = "Time of Use (Weekdays)") +
  scale_color_brewer(palette = "Set1") +

  labs(
    title = "Average Hourly Electricity Usage 2022-2023 with TOU Periods",
    x = "Hour of Day",
    y = "Average kWh",
    color = "Day Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )
