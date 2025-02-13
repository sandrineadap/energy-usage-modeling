########################################################
# ANALYSIS from EnergyUse.R
########################################################

install.packages("hrbrthemes")
install.packages("dygraphs")

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyverse)
library(lubridate)

library(dygraphs)
library(xts)
library(htmlwidgets)

# usage_final <- read.csv('usage_data_final.csv')

# Group data by day & summarize by the 3 price columns
daily_data <- usage_final %>%
  group_by(Date) %>%
  summarize_at(c("TOU_Price", "ULO_Price", "Tier_Price"), sum) %>%
  arrange(Date)

# daily_data <- usage_final %>%
#   group_by(year(DT_start), month(DT_start), Date) %>%
#   summarize_at(vars(TOU_Price, ULO_Price, Tier_Price),
#                list(name = sum))

## Simple plot
daily_data %>%
  ggplot(aes(x=Date)) +
    geom_line(aes(y = TOU_Price), color="steelblue") +
    geom_line(aes(y = ULO_Price), color="goldenrod", linetype="twodash") +
    geom_line(aes(y = Tier_Price), color="darkred") +
    # geom_point(shape='o') +
    ggtitle("Pricing Plans for Electricity Usage")

# Group data by month
monthly_data <- usage_final %>%
  mutate(Year = year(DT_start), Month = month(DT_start)) %>%
  mutate(Year_Month = format(parse_date_time(paste(Year, Month, sep='-'), "ym"), "%Y-%m")) %>%
  group_by(Year, Year_Month) %>%
  summarize_at(c("TOU_Price", "ULO_Price", "Tier_Price"), sum)



monthly_data %>%
  ggplot(aes(x=Year_Month)) +
    geom_line(aes(y = TOU_Price, group = 1, color="TOU")) +
    geom_line(aes(y = ULO_Price, group = 1, color="ULO"), ) +
    geom_line(aes(y = Tier_Price, group = 1, color="Tier")) +
    labs(x="Month",
        y="Price ($)"
    ) +
    scale_color_manual(
      name = "",
      breaks = c("TOU", "ULO", "Tier"),
      values = c("TOU"="steelblue", 
                 "ULO"="goldenrod", 
                 "Tier"="darkred")
    ) +
    scale_linetype_manual(
      name = "",
      breaks = c("TOU", "ULO", "Tier"),
      values = c("TOU"="solid", 
                 "ULO"="solid", 
                 "Tier"="solid")
    ) +
    ggtitle("Pricing Plans for Electricity Usage") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# interactive daily graph
## group with datetime
daily <- xts(x=daily_data[,c(-1,-3)], order.by = daily_data$Date)

p <- dygraph(daily, main = "Prices for Daily Electricity Usage") %>%
  dyAxis("y", label = "Price for the Day ($)") %>%
  dyRangeSelector() %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE)
  # dyLimit(as.numeric(600*0.0103), color = "red")
  # dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  # dyCrosshair(direction = "vertical") %>%
  # dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  # dyRoller(rollPeriod = 1)
p

# save the widget
saveWidget(p, file=paste0( getwd(), "/daily_data_interactive.html"))


# interactive monthly graph
# monthly_data2 <- daily_data %>% 
#   group_by(Year, Year_Month) %>%
#   summarize_at(c("TOU_Price", "ULO_Price", "Tier_Price"), sum)

monthly <- xts(x=monthly_data[,c(-1,-2,-4)], order.by = ym(monthly_data$Year_Month))

q <- dygraph(monthly, main = "Prices for Monthly Electricity Usage") %>%
  dyAxis("y", label = "Price for the Month ($)") %>%
  dyRangeSelector() %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = TRUE) %>%
  dyLimit(as.numeric(600*0.103), color = "red") %>%
  dyLimit(as.numeric(1000*0.103), color = "gray")
  # dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  # dyCrosshair(direction = "vertical") %>%
  # dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  # dyRoller(rollPeriod = 1)
q

# save the widget
saveWidget(q, file=paste0( getwd(), "/monthly_data_interactive.html"))


# do we always go above 600 kWh? If so, by how much?
## group usage_final by month and summarize usage_kWH
monthly_data <- usage_final %>%
  mutate(Year = year(DT_start), Month = month(DT_start)) %>%
  mutate(Year_Month = format(parse_date_time(paste(Year, Month, sep='-'), "ym"), "%Y-%m")) %>%
  group_by(Year, Year_Month) %>%
  summarize_at(c("Usage_kWh","TOU_Price", "ULO_Price", "Tier_Price"), sum)


# How much energy are we using during each peak (specifically on-peak?)
## stacked bar graph for each month's energy use