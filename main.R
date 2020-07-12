library(tidyverse)
library(lubridate)
library(sweep)
library(feasts) # update.packages(oldPkgs = "ggplot2")
library(tsibbledata)
library(tsibble)
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
if(!dir.exists("output")) dir.create("output")

###################
#### FUNCTIONS ####
###################
import_measurement <- function(path = NA,Probe = NA,Position = NA,Trial = NA, Row = NA, Column = NA){
  if(anyNA(c(path,Probe,Position,Trial))) stop("Specify all meta information for path, Probe, Position and Trial")
  read_csv(path) %>% 
    mutate(Probe = Probe, Position = Position, Trial = Trial,Date = mdy(Date),date_time = paste(Date,Time) %>% as_datetime()) %>% 
    select(-Unit, -Date, -Time) %>% select(date_time,Probe,Position,Trial,Temp) %>% 
    filter(!duplicated(date_time)) %>%
    as_tsibble(index = date_time,key = c(Probe,Position,Trial)) %>% fill_gaps()
}

wrangle_aggregate_ts <- function(tsibble, bin_width){
  tsibble %>%
    group_by_key() %>%
    index_by(year_month = ~ lubridate::floor_date(., !!bin_width)) %>%
    summarise(Temp = mean(Temp, na.rm = TRUE)) %>%
    mutate(time_interval_number = row_number())
}

################
#### IMPORT #### 
################
# Notes: remove the header of the csv files and delete the hashtag before the header row (#Data ...)
pos7_green_run1 <- import_measurement("./data/pos7green.csv",Probe = "green",Position = 7,Trial = 1, Row = NA, Column = NA)
pos7_red_run1 <- import_measurement("./data/pos7red.csv",Probe = "red",Position = 7, Trial = 1, Row = NA, Column = NA)
# ... all studies in order
data_raw <- list(pos7_green_run1 = pos7_green_run1, pos7_red_run1 = pos7_red_run1) # ... list all studies here

############################
#### Manual time window #### 
############################
# Note: Inspect the plots and note down the time from when to when that is gap free
p_raw_interval <- map2(.x = data_raw,.y = names(data_raw),~.x %>% gg_season(y = Temp) + ggtitle(.y))
walk2(.x = p_raw_interval,.y = names(data_raw), ~ ggsave(plot = .x,filename = paste0("./output/raw_",.y,".pdf"),width = 10,height = 5))


#######################
#### Trim timeline #### 
#######################
# Note A: Check visually and computationally that there are no gaps for each measurement
# Note B: Always start and end at full hours so the data can be directly compared even though not measured at the same time
pos7_green_run1_filt <- import_measurement("./data/pos7green.csv",Probe = "green",Position = 7,Trial = 1) %>% filter_index("2020-03-23 15:00:00" ~ "2020-03-24 13:00:00")
pos7_red_run1_filt <- import_measurement("./data/pos7red.csv",Probe = "red",Position = 7, Trial = 1) %>% filter_index("2020-03-23 15:00:00" ~ "2020-03-24 12:00:00")
data_trim <- list(pos7_green_run1_filt = pos7_green_run1_filt, pos7_red_run1_filt = pos7_red_run1_filt)

trim_gaps <- data_trim %>% map(~ .x %>% has_gaps() %>% pull(.gaps))
titles <- map2(.x = names(data_trim),.y = trim_gaps,~ paste0(.x, ", has gaps: ",.y))
p_trim_interval <- map2(.x = data_trim,.y = titles,~.x %>% gg_season(y = Temp) + ggtitle(.y))
walk2(.x = p_trim_interval,.y = names(data_trim), ~ ggsave(plot = .x,filename = paste0("./output/trim_",.y,".pdf"),width = 10,height = 5))



####################
#### Raw signal #### 
####################
data_agg <- data_trim %>% map(~ .x %>% wrangle_aggregate_ts(bin_width = "30 seconds"))
plots <- map2(.x = data_agg,.y = names(data_agg), ~ggplot(data = .x,aes(x = time_interval_number,y = Temp)) + geom_path() + labs(x = "Interval number",y = "Temperature [°C]", title = .y))
walk2(.x = plots,.y = names(data_agg), ~ ggsave(plot = .x,filename = paste0("./output/agg_",.y,".pdf"),,width = 10,height = 5))

###########################
#### Decomposed signal #### 
###########################
# Decomposition: all components
data_components <- data_agg %>% map(~.x %>% model(STL(Temp ~ season(window = Inf))) %>% components()) 
walk2(.x = data_components,.y = names(data_components), ~ .x %>% autoplot() %>% ggsave(plot = .,filename = paste0("./output/components_",.y,".pdf"),,width = 10,height = 5))
data_components_df <- data_components %>% reduce(bind_rows)

# Hourly timecourse
p_hourly_component <- data_components_df %>% ggplot(aes(x = year_month, y = season_hour, color = Probe)) + geom_path(alpha = 0.5) + facet_grid(Trial ~ Position)
ggsave(plot = p_hourly_component,filename = paste0("./output/p_hourly_component.pdf"),width = 10,height = 5)

# Hourly variation
data_components_df %>% mutate(season_hour_scaled = scale(x = season_hour))  %>% ggplot(aes(x = Probe, y = season_hour)) + geom_boxplot() 
p_hourly_variability <- data_components_df %>% ggplot(aes(x = Probe, y = season_hour)) + stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  labs(x = "Flatbed position", y = "Temperature fluctuation", title = "Which position is the most variable?")
ggsave(plot = p_hourly_variability,filename = paste0("./output/p_hourly_variability.pdf"),width = 5,height = 5)






####################
#### Supplement #### 
####################


#create time series of 6 h from 102433 to 124033, so 20:44:16 to 02:44:15
#seconds.ts <-ts(pos8greenRun3,start=102433,end=116833)
#minutes.ts <-ts(pos8greenRun3,start=102433,end=124033)


# Check later:
# https://robjhyndman.com/hyndsight/feasts/


# join <- full_join(green,red)
# tsib <- as_tsibble(join,index = date_time,key = c(Probe,Position,Trial)) 
# # handle gaps
# tsib_gapna <- tsib %>% fill_gaps()
# tsib_gapmean <- tsib %>% group_by_key() %>% fill_gaps(Temp = mean(Temp), .full = TRUE) %>% ungroup()
# 
# # Inspct time gaps manually and filter the time period reliably covered by both sensors
# tsib_gapna %>% gg_season(y = Temp)
# range(tsib_gapna$date_time)
# tsib_filtered <- tsib_gapna %>% filter_index("2020-03-23 15:00:00" ~ "2020-03-24 13:00:00") #"2020-03-23 12:00:00" ~ "2020-03-24 12:00:00" or "2020-03-23 15:00:00" ~ .
# tsib_filtered %>% gg_season(y = Temp)
# has_gaps(tsib_filtered)
# # Aggregates
# agg <- tsib_filtered %>%
#   group_by_key() %>%
#   index_by(year_month = ~ lubridate::floor_date(., "30 seconds")) %>% # monthly aggregates
#   summarise(
#     Temp = mean(Temp, na.rm = TRUE)
#   )
# agg
# 
# # Time gaps
# ped_gaps <- tsib %>%
#   count_gaps(.full = TRUE)
# ggplot(ped_gaps, aes(x = Probe, colour = Probe)) +
#   geom_linerange(aes(ymin = .from, ymax = .to)) +
#   geom_point(aes(y = .from)) +
#   geom_point(aes(y = .to)) +
#   coord_flip() +
#   theme(legend.position = "bottom")
# 
# # Decomposition
# dcmp <- agg %>% filter(Probe == "red") %>% model(STL(Temp ~ season(window = Inf)))
# components(dcmp)
# components(dcmp) %>% autoplot()
