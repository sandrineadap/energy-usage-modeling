# Using the hourly electricity consumption, determine what electricity plan should be used.

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
usage_2024 <- read.csv('./data/EnergyUsage-2024-01-01-2024-05-02.csv')

# put all energy data into usage dataframe
usage <- rbind(usage, usage_2023)
usage <- rbind(usage, usage_2024)

# usage <- usage %>% distinct()

# View(usage) # should have 20,496 rows from 2022-May 2024

# remove unneeded dataframes
ls()
rm(usage_2023)
rm(usage_2024)

# load pricing data for TOU, ULO, and Tiered
tou_prices <- read.csv('./data/Time_Of_Use_Pricing.csv')
ulo_prices <- read.csv('./data/Ultra-Low_Overnight_Pricing.csv')
tier_prices <- read.csv('./data/Tiered_Pricing.csv')

# load holiday schedule for 2022, 2023, and 2024
holidays <- read.csv('./data/Holiday_Schedule_2022.csv')
holidays_2023 <- read.csv('./data/Holiday_Schedule_2023.csv')
holidays_2024 <- read.csv('./data/Holiday_Schedule_2024.csv')


############## PREPARING & CLEANING THE DATA

# HOLIDAYS: check for Date format
str(holidays)       # Date is character
str(holidays_2023)  # Date is character
str(holidays_2024)  # Date is character

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

# convert Date columns in holidays_2023 & 2024
## combine 2023 & 2024
holidays_2023 <- rbind(holidays_2023, holidays_2024)

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

# holidays_test <- holidays

# holidays_test <- holidays_test %>%
#   mutate(Date = as.POSIXct(Date))
# str(holidays$Date)

# remove unneeded dataframes
rm(holidays_2023)
rm(holidays_2024)
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

### 2024
dst_start_2024 <-
  which(usage_dt24$Date_start == as.Date('2024-03-10') & 
      usage_dt24$Time_start == '03:00')[1] # row 19251

# november hasn't happened yet, so just go until the end of the dataframe
dst_end_2024 <- dim(usage_dt24)[1]

# which(usage_dt24$Date_start == as.Date('2023-11-03') & 
#       usage_dt24$Time_start == '01:00') 



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

### 2024
dstRows <- c(dst_start_2024:dst_end_2024) # daylight savings time rows for 2023
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

### 2024
dstRows <- c(dst_start_2024:dst_end_2024) # daylight savings time rows for 2024
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


# Create Day_Of_Week column from Date_start

# Rename Usage..kilowatt.hours. to Usage_kWh
# Also, remove unneeded/repetitive columns
usage_clean <- usage_ends %>%
  rename(Usage_kWh = Usage..kilowatt.hours.) %>%
  select(c(DT_start, DT_end, Usage_kWh))

usage_clean <- usage_clean %>% distinct()

View(usage_clean)

############## CALCULATIONS

# Create Monthly_Usage column to determine tier.
# Sum values from the first of the month until Date_start
usage_monthly <- usage_clean 

## get Month_start & Year_start (for Monthly_Usage grouping)
usage_monthly <- usage_monthly %>%
  mutate(Month_start = month(DT_start), .before=1) %>%
  mutate(Year_start = year(DT_start), .after=1)

## paste Month_start & Year_start together
usage_monthly<- usage_monthly %>%
  mutate(m.y_start = paste(Month_start, Year_start, sep='-'), .before=1) %>%
  select(-c(Month_start, Year_start))

View(usage_monthly)

## calculate cumulative sum by group (m.y_start)
## https://www.geeksforgeeks.org/how-to-calculate-cumulative-sum-by-group-in-r/
usage_monthly <- usage_monthly %>%
 mutate(Monthly_Usage = ave(Usage_kWh, by=m.y_start, FUN=cumsum))

# Create Tier column (based on Monthly_Usage column)
usage_options <- usage_monthly

# # non seasonal tiers as specified by Oshawa Power
# usage_options <- usage_options %>%
#   mutate(Tier = ifelse(Monthly_Usage > 600, "Tier 2", "Tier 1"))

# seasonal tiers as specified by Ontario Energy Board
usage_options <- usage_options %>%
  mutate(Tier = case_when(
    month(DT_start) %in% 5:10 & Monthly_Usage > 600  ~ "Tier 2",  # Summer
    month(DT_start) %in% c(11,12,1,2,3,4) & Monthly_Usage > 1000 ~ "Tier 2",  # Winter
    TRUE ~ "Tier 1"
  ))

# Create TOU Peak column. (if statements) 

## Create Date column w/ Date class so we can join to `holidays` by Date
usage_options <- usage_options %>%
  mutate(Date = date(DT_start), .before=2)

str(usage_options$Date)

## Left join `holidays` to `usage_options` on Date
usage_options <- left_join(usage_options, holidays[c('Date', 'Holiday')], by="Date")

usage_options <- usage_options %>%
  mutate(Holiday = case_when(
    is.na(Holiday) ~ FALSE,
    .default = TRUE
  )) %>%
  rename(is.Holiday = Holiday)

usage_options <- usage_options %>%
  mutate(TOU_Peak = case_when(
    # if Sunday or Saturday or Holiday, Off peak
    wday(DT_start) == 1 | wday(DT_start) == 7 | is.Holiday ~ 'Off',
    # if time between 7pm-7am, Off Peak
    hour(DT_start) >= 19 | hour(DT_start) < 7 ~ 'Off',
    # if time between 11am-5pm, check month
    hour(DT_start) >= 11 & hour(DT_start) < 17 ~ 
      # if month in Nov-Apr (inclusive), Mid; else, On
      ifelse(month(DT_start) >= 11 | month(DT_start) <= 4, 'Mid', 'On'),
    # if time between 7am-11am or 5pm-7pm, check month
    (hour(DT_start) >= 7 & hour(DT_start) < 11) | (
      # if month in Nov-Apr (inclusive), On; else, Mid
      hour(DT_start) >= 17 & hour(DT_start) < 19) ~
      ifelse(month(DT_start) >= 11 | month(DT_start) <= 4, 'On', 'Mid'),
    .default = NULL
  ), .after=4)

# Create ULO Peak column. (if statements)

usage_options <- usage_options %>%
  mutate(ULO_Peak = case_when(
    # if time between 11pm-7am, ULO
    hour(DT_start) >= 23 | hour(DT_start) < 7 ~ 'ULO',
    # if Sunday or Saturday or Holiday, Off
    wday(DT_start) == 1 | wday(DT_start) == 7 | is.Holiday ~ 'Wknd',
    # if time between 4pm-9pm, On
    hour(DT_start) >= 16 & hour(DT_start) < 21 ~ 'On',
    # if time between 7am-4pm or 9pm-11pm, Mid
    (hour(DT_start) >= 7 & hour(DT_start) < 16) | 
      (hour(DT_start) >= 21 & hour(DT_start) < 23) ~ 'Mid',
    .default = NULL
  ), .after=5)


# Determine Prices. Join columns for TOU Price, ULO Price, 
# and Tier Price for that hour of usage. 

## rename Peak.ID & Price....kWh.
tou_prices <- tou_prices %>%
  rename(TOU_Peak = Peak.ID) %>%
  rename(TOU_Price = Price....kWh.)

ulo_prices <- ulo_prices %>%
  rename(ULO_Peak = Peak.ID) %>%
  rename(ULO_Price = Price....kWh.)

tier_prices <- tier_prices %>%
  rename(Tier = Price.Tier) %>%
  rename(Tier_Price = Price....kWh.)

## Join tou_prices, ulo_prices, tier_prices
usage_prices <- usage_options %>%
  left_join(tou_prices[c('TOU_Peak', 'TOU_Price')], by="TOU_Peak") %>%
  left_join(ulo_prices[c('ULO_Peak', 'ULO_Price')], by="ULO_Peak") %>%
  left_join(tier_prices[c('Tier', 'Tier_Price')], by="Tier")

# multiply prices by usage_kWh
usage_prices <- usage_prices %>%
  mutate(TOU_Price = TOU_Price * Usage_kWh) %>%
  mutate(ULO_Price = ULO_Price * Usage_kWh) %>%
  mutate(Tier_Price = Tier_Price * Usage_kWh)


# just making sure no missing values
sum(is.na(usage_prices))

usage_final <- usage_prices %>%
  select(c(m.y_start, Date, DT_start, DT_end, Usage_kWh, Monthly_Usage, TOU_Price, ULO_Price, Tier_Price))

View(usage_final)

# write data to csv
write.csv(usage_final,"./data/usage_data_final.csv", row.names = FALSE) 
  
print ('CSV created Successfully :)')

########################################################
# ANALYSIS in EnergyUseAnalysis.R
########################################################
