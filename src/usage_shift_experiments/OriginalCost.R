# Using the hourly electricity consumption, determine what electricity plan should be used.

# install.packages("tidyverse")  # if not already installed
# install.packages("readxl") # if not already installed

library(tidyverse)  # includes dplyr, tidyr, ggplot2, lubridate
                    # https://www.tidyverse.org/packages/

library(readxl)  # for reading Excel files

########################################################
# PREPROCESSING
########################################################

############## LOAD DATA

# load data from csv's in working directory
getwd()

# load pricing data for TOU, ULO, and Tiered
tou_prices <- read.csv('./data/Time_Of_Use_Pricing.csv')
ulo_prices <- read.csv('./data/Ultra-Low_Overnight_Pricing.csv')
tier_prices <- read.csv('./data/Tiered_Pricing.csv')

# load holiday schedule for 2022, 2023, and 2024
holidays <- read.csv('./data/Holiday_Schedule_2025.csv')


############## PREPARING & CLEANING THE DATA

# HOLIDAYS: check for Date format
str(holidays)       # Date is character

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

str(holidays)

# convert R date to lubridate
holidays <- holidays %>%
  mutate(Date = ymd(Date))
View(holidays)

# remove unneeded dataframes
rm(month_nums)

################ USAGE: load usage data
usage <- read_excel('./data/EnergyUsage-2025ElectricUsage.xlsx', skip = 1)

View(usage)

# USAGE: separate Energy.consumption.time.period into
# Date_start, Time_start, Date_end, Time_end
# View(usage)

## first separate the starting and ending dates from `Energy.consumption.time.period``
usage <- usage %>%
  separate("Energy consumption time period", 
           into=c('Date_start', 'Date_end'),
           sep=' to ', remove=T, convert=T)

## Starting dates: separate starting date into its units of time
usage <- usage %>%
  separate(Date_start, 
           into=c('Year_start', 'Month_start', 'Day_start'),
           sep='/', remove=T, convert=T) # changed separator to '/' based on data format

usage <- usage %>%
  separate(Day_start, 
           into=c('Day_start', 'Time_start'),
           sep=' ', remove=T, convert=T)

## combine them to datetime format
usage <- usage %>% 
  mutate(Date_start = make_datetime(Year_start, Month_start, Day_start), .before=1)
str(usage$Date_start)

## Ending dates: separate starting date into its units of time
usage <- usage %>%
  separate(Date_end, 
           into=c('Year_end', 'Month_end', 'Day_end'),
           sep='/', remove=T, convert=T)

usage <- usage %>%
  separate(Day_end, 
           into=c('Day_end', 'Time_end'),
           sep=' ', remove=T, convert=T)

## combine them into datetime format
usage <- usage %>% 
  mutate(Date_end = make_datetime(Year_end, Month_end, Day_end), .before=6)
str(usage$Date_end)

## Remove unnecessary columns
usage <- usage %>%
  select(-c(Year_start, Month_start, Day_start, Year_end, Month_end, Day_end))

# View(usage)
str(usage)

# Times already in 24-hour format so skip that part

# combine Date_start and Time_start into DT_start 
# (so time can be used for lubridate manipulations)
usage <- usage %>%
  mutate(DT_start = paste(Date_start, Time_start), .before=1)
class(usage$DT_start)

## convert DT_start to DateTime
usage <- usage %>% 
  mutate(DT_start = ymd_hm(DT_start))
class(usage$DT_start)
View(usage)

# combine Date_end and Time_end into DT_end 
# (so time can be used for lubridate manipulations)
usage <- usage %>% 
  mutate(DT_end = paste(Date_end, Time_end), .before=2)

## convert DT_end to DateTime
usage <- usage %>% 
  mutate(DT_end = ymd_hm(DT_end))
class(usage$DT_end)
View(usage)


# Rename Usage..kilowatt.hours. to Usage_kWh
# Also, remove unneeded/repetitive columns
usage_clean <- usage %>%
  rename(Usage_kWh = "Usage (kilowatt-hours)") %>%
  select(c(DT_start, DT_end, Usage_kWh))

usage_clean <- usage_clean %>% distinct()

View(usage_clean)


# TODO: make sure the rest of this is correct

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
