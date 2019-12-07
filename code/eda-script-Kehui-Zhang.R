# ======================================================
# Title: eda-script
# Description:
#   This script includes exploratory data analysis
# Input(s): data file: ibtracs-2010-2015.csv
# Output(s): output file: github_document
# Author: Kehui Zhang
# Date: 10/15/2019
# ======================================================

#packages
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(naniar)
library(ggmap)

# read data file
ibt <- read_csv('/Users/zhangkehui/Desktop/UCB/workout1/data/ibtracs-2010-2015.csv',
                na = c("-999", "-1.0","0.0"), col_names=c("serial_num","season","num","basin","sub_basin","name",
                                                          "iso_time", "nature", "latitude","longitude","wind","press"),
                col_types = "cicfccccdddd", skip = 1)
ibt <- read_csv("/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout1/data/ibtracs-2010-2015.csv",col_names=c("serial_num","season","num","basin","sub_basin","name","iso_time", "nature", "latitude","longitude","wind","press"),col_types = "cicfccccdddd", skip = 1, na = c("-999.", "-1.0","0.0"))
ibt <- ibt %>% replace_with_na(replace = list( latitude = -999.00))
ibt <- ibt %>% replace_with_na(replace = list( longitude = -999.00))
ibt <- ibt %>% replace_with_na(replace = list( wind = -999))
ibt <- ibt %>% replace_with_na(replace = list( wind = -1))
ibt <- ibt %>% replace_with_na(replace = list( wind = 0))
ibt <- ibt %>% replace_with_na(replace = list( press = -999 ))
ibt <- ibt %>% replace_with_na(replace = list( press = -1 ))

sink(file = '/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout1/output/data-summary.txt')
summary(ibt)
sink()


# Data Visualization
# saving a single map in PDF format
pdf("/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout1/images/map-all-storms.pdf")
ibtbox <- make_bbox(lon = ibt$longitude, lat = ibt$latitude, f = .1)
ibt_map <- get_map(location = ibtbox, maptype = "satellite", source = "google")
ggmap(ibt_map) + geom_point(data = ibt, mapping = aes(x =longitude , y = latitude), color = "blue")
dev.off()

# saving a single map in Png format
png(filename = "/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout1/images/map-all-storms.png")
ibtbox <- make_bbox(lon = ibt$longitude, lat = ibt$latitude, f = .1)
ibt_map <- get_map(location = ibtbox, maptype = "satellite", source = "google")
ggmap(ibt_map) + geom_point(data = ibt, mapping = aes(x =longitude , y = latitude), color = "blue")
dev.off()

# viualize the storms in the EP and NA
ibt_EP_NA_year <-filter(ibt, basin=="EP" | basin=="NA")
# saving in PDF format
ibt_map <- get_map(location = ibtbox, maptype = "satellite", source = "google")
gg_EP_NA_year <- ggmap(ibt_map) + 
                   geom_point(data = ibt_EP_NA_year,mapping = aes(x =longitude , y = latitude),alpha = 0.7, color = "blue")+
                     facet_wrap(~season)
ggsave('/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout1/images/map-ep-na-storms-by-year.pdf',
       plot = gg_EP_NA_year, width = 6, height = 4, units = "in")

ggsave('/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout1/images/map-ep-na-storms-by-year.png',
       plot = gg_EP_NA_year, width = 6, height = 4, units = "in")
# by month

ibt_EP_NA_month <-mutate(ibt_EP_NA, month = month(ibt_EP_NA$iso_time))
                      
gg_EP_NA_month <- ggmap(ibt_map) + 
  geom_point(data = ibt_EP_NA_month, aes(x = latitude, y = longitude), alpha = 0.7, color = "blue")+
  facet_wrap(~month)

ggsave('/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout1/images/map-ep-na-storms-by-month.pdf',
       plot = gg_EP_NA_month, width = 6, height = 4, units = "in")

ggsave('/Users/zhangkehui/Desktop/UCB/Stat133/workouts/workout1/images/map-ep-na-storms-by-month.png',
       plot = gg_EP_NA_month, width = 6, height = 4, units = "in")


