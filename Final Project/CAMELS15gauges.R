# CAMELS dataset, common gauges (15) with MOPEX
# Katharine Sink 10/14/20

# 15 gauges that appear in MOPEX and CAMELS datasets
# within the identified anomalous basins by Carmona et al (2014)
# txt files from Daymet forced model output, time series

# working directory "C:/Users/katha/Documents/R/Camels15/data/Daymet text files"

# Daymet primary meteorological dataset, derived from daily T and P observations
# Daymet(model_output_daymet/model_output/flow_timeseries/model_output
# used model run 5, chosen arbitrarily 

# 1 text file per gauge, file contains daily data from 1980/10 - 2014/12
# contains 12 columns 
# date information in first 4 columns YR, MNTH, DY, HR (all value 12)
# snow water equivalent (SWE) in mm, precipitation (PRCP) in mm/day, surface water input (RAIM), 
# mean daily air temp (TAIR) in celcius, potential evapotranspiration (PET) in mm/day, 
# evapotranspiration (ET) in mm/day from SAC model, model runoff (MOD_RUN) in mm/day, 
# observed runoff (OBS_RUN) in mm/day from USGS

# import libraries
library(tidyverse) # dplyr, ggplot2, forcats, tibble, readr, stringr, tidyr, purrr
library(lubridate) # date-time 
library(xts) # eXtensible time series, extends zoo 

# get filepath (folder location) for the .txt files
filelocation = getwd()

# return a list of the filenames as a named fs_path character vector
# use regular expression (regexp) to display only the .txt files 
allfiles = fs::dir_ls(filelocation, regexp = "\\.txt$")

# create a data frame that contains all text files and maintain
# the filepath as the id for each row of data
# magrittr (purrr) is for pipe ( %>% ) operators, ctrl+shift+m 
# read_table2 allows any amount of whitespace in between 
datatable = allfiles %>%  map_dfr(read_table2, .id = "source")

# remove the HR column since all values are "12"
datatable$HR = NULL 

# change class from character to numeric for month and day
datatable = datatable %>%  
  mutate(MNTH = as.numeric(MNTH), DY = as.numeric(DY))

# create new column as date object combining the year, month, and day columns
# class will be "date"
datatable = datatable %>% 
    mutate(DATE = make_date(year = datatable$YR, month = datatable$MNTH, 
                          day = datatable$DY))

# clean up the name (ID) for each row by removing the filepath characters
# except for the gauge number, rename the column header from source to GaugeID
# str_length(datatable$source[1]) indicated the string (row 1) consisted of 87 characters
datatable$source = str_sub(datatable$source, start = 60, end = 67)  
datatable = datatable %>% rename(GaugeID = source)

# note that 1980 contains only 3 months (October, November, December)
# remove data for that year since it is incomplete, even for water year
datatable2 = datatable[!(datatable$YR == "1980"),]

# save datatable as rds, r object, for easy use in shiny app
# saveRDS(datatable2, "Camels/shinydata.rds")

# change dataframe (tibble) to a time series (xts) object using xts function
# from xts package, class of the dataframe will be "xts" "zoo"
# requires date information to be classified as date class
# datatable_xts = xts(datatable2[1:12], datatable2$DATE) ## come back to this


#########################################################
# EXPLORATORY DATA ANALYSIS

# summary information for hydrometeorological variables
summary(datatable2[5:12])

###### PRECIPITATION ##########
# create dataframe (P_month) grouped by gauge and month (1-12) 
# mnth_total is total precipitation for that month over all years of record
# mnth_mean is mean precipitation for that month over all years of record
# 15 gauges x 12 months = 180 monthly totals and means
P_month = datatable2 %>% 
  group_by(GaugeID, MNTH) %>% 
  summarise(mnth_total = sum(PRCP), mnth_mean = mean(PRCP))

# dataframe (P_annual) grouped by gauge and year (1981 - 2014)
# yr_total is total precipitation for that year (12 months)
# yr_mean is mean precipitation for that year 
P_annual = datatable2 %>% 
  group_by(GaugeID, YR) %>% 
  summarise(yr_total = sum(PRCP), yr_mean = mean(PRCP))

# group data by gauge, year, and month for only precipitation
# mnth_total_per_ yr is total precipitation for each month in every year, 
# mnth_mean_per_yr is mean precipitation for each month in every year
# then arrange based on the year, then month
P_monthperyear = datatable2 %>% 
  group_by(GaugeID, YR, MNTH) %>% 
  select(PRCP) %>% 
  summarise(mnth_total_per_yr = sum(PRCP), mnth_mean_per_yr = mean(PRCP)) %>% 
  arrange(YR, MNTH)

###### RUNOFF ##########

# create dataframe (Q_month) grouped by gauge and month (1-12) 
# mnth_total is total runoff for that month over all years of record
# mnth_mean is mean runoff for that month over all years of record
# 15 gauges x 12 months = 180 monthly totals and means

Q_month = datatable2 %>% 
  group_by(GaugeID, MNTH) %>% 
  summarise(mnth_total = sum(OBS_RUN), mnth_mean = mean(OBS_RUN))

# dataframe (Q_annual) grouped by gauge and year (1981 - 2014)
# yr_total is total runoff for that year (12 months)
# yr_mean is mean runoff for that year 
Q_annual = datatable2 %>% 
  group_by(GaugeID, YR) %>% 
  summarise(yr_total = sum(OBS_RUN), yr_mean = mean(OBS_RUN))

# group data by gauge, year, and month for only runoff
# mnth_total_per_ yr is total runoff for each month in every year, 
# mnth_mean_per_yr is mean runoff for each month in every year
# then arrange based on the year, then month
Q_monthperyear = datatable2 %>% 
  group_by(GaugeID, YR, MNTH) %>% 
  select(OBS_RUN) %>% 
  summarise(mnth_total_per_yr = sum(OBS_RUN), mnth_mean_per_yr = mean(OBS_RUN)) %>% 
  arrange(YR, MNTH)



###############################################################
# PLOTS

library(ggplot2)
library(extrafont)
loadfonts(device = "win") # with each new session, just like library
# use "windowsFonts()" to list font families, specific name to use


# total annual precipitation plots per gauge
yrTotalP = ggplot(data = P_annual, mapping = aes(x = YR, y = yr_total)) +
  geom_bar(stat = "identity", width = 0.75, fill = "blue", color = "black") +
  labs(title = "Total Annual Precipitation per Gauge",
       caption = "Source: CAMELS (Addor et al 2017)") + 
  xlab(label = "Year") + ylab(label = "Precipitation (mm/day)") +
  facet_wrap(~GaugeID, nrow = 3) +
   theme_bw(base_family = "Palatino Linotype")  
   
plot(yrTotalP)

# export as png file 
png(file = "yrTotalP.png", res = 600, width = 4800, height = 2758, 
    pointsize = 10, type = "windows", antialias = "cleartype")
yrTotalP
dev.off()

####
# mean annual precipitation plots per gauge
yrMeanP = ggplot(data = P_annual, mapping = aes(x = YR, y = yr_mean)) +
  geom_bar(stat = "identity", width = 0.75, fill = "blue", color = "black") +
  labs(title = "Mean Annual Precipitation per Gauge",
       caption = "Source: CAMELS (Addor et al 2017)") + 
  xlab(label = "Year") + ylab(label = "Precipitation (mm/day)") +
  facet_wrap(~GaugeID, nrow = 3) +
  theme_bw(base_family = "Palatino Linotype")  

plot(yrMeanP)

# export as png file 
png(file = "yrMeanP.png", res = 600, width = 4800, height = 2758, 
    pointsize = 10, type = "windows", antialias = "cleartype")
yrMeanP
dev.off()

####
# total monthly precipitation plots per gauge
mnthTotalP = ggplot(data = P_month, mapping = aes(x = MNTH, y = mnth_total)) +
  geom_bar(stat = "identity", width = 0.75, fill = "blue", color = "black") +
  labs(title = "Total Monthly Precipitation per Gauge",
       subtitle = "Entire record (1981 - 2014)",
       caption = "Source: CAMELS (Addor et al 2017)") + 
  xlab(label = "Month") + ylab(label = "Precipitation (mm/day)") +
  scale_x_discrete(limits = month.abb) +
  facet_wrap(~GaugeID, nrow = 3) +
  theme_bw(base_family = "Palatino Linotype") + 
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))  

plot(mnthTotalP)

# export as png file 
png(file = "mnthTotalP.png", res = 600, width = 4800, height = 2758, 
    pointsize = 10, type = "windows", antialias = "cleartype")
mnthTotalP
dev.off()

####
# mean monthly precipitation plots per gauge
mnthMeanP = ggplot(data = P_month, mapping = aes(x = MNTH, y = mnth_mean)) +
  geom_bar(stat = "identity", width = 0.75, fill = "blue", color = "black") +
  labs(title = "Mean Monthly Precipitation per Gauge",
       subtitle = "Entire record (1981 - 2014)",
       caption = "Source: CAMELS (Addor et al 2017)") + 
  xlab(label = "Month") + ylab(label = "Precipitation (mm)") +
  scale_x_discrete(limits = month.abb) +
  facet_wrap(~GaugeID, nrow = 3) +
  theme_bw(base_family = "Palatino Linotype") + 
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))  

plot(mnthMeanP)

# export as png file 
png(file = "mnthMeanP.png", res = 600, width = 4800, height = 2758, 
    pointsize = 10, type = "windows", antialias = "cleartype")
mnthMeanP
dev.off()

####
# monthly totals in separate plots, per year for selected gauge
# repeat for each gauge (? more efficient way)
mnthPerYrP = ggplot(data = P_monthperyear, 
                    mapping = aes(x = YR, y = mnth_total_per_yr)) +
  geom_line(color = "blue", data = subset(P_monthperyear, GaugeID == "06814000")) +
  labs(title = "Total Monthly Precipitation for Gauge # 06814000",
       subtitle = "Entire record (1981 - 2014)",
       caption = "Source: CAMELS (Addor et al 2017)") + 
  facet_wrap(~MNTH, nrow = 3, scales = "free_y") +
  xlab("Year") + ylab("Total Precipitation(mm)") +
  theme_bw(base_family = "Palatino Linotype") +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

plot(mnthPerYrP)

# export as png file 
png(file = "Gauge_0681400_mnthPerYrP.png", res = 600, width = 4800, height = 2758, 
    pointsize = 10, type = "windows", antialias = "cleartype")
mnthPerYrP
dev.off()

############################################
# RUNOFF EFFICIENCY
# Ratio of runoff to precipitation (Q/P)


# take annual mean values for precipitation and runoff, create new datatable
# ratio calculated for each yr (1981 - 2014) at each gauge
runEff = data.frame(P_annual$GaugeID, P_annual$YR, P_annual$yr_mean, Q_annual$yr_mean) 
runEff = runEff %>% 
  mutate(Eff = Q_annual.yr_mean/P_annual.yr_mean) %>%  # divide Q/P, unitless
  rename(GaugeID = P_annual.GaugeID, 
         YR = P_annual.YR, 
         P_annual_mean = P_annual.yr_mean, 
         Q_annual_mean = Q_annual.yr_mean )

# create plot of runoff efficiency for each gauge
effPlot = ggplot(data = runEff, mapping = aes(x = YR, y = Eff)) +
  geom_bar(stat = "identity", width = 0.75, fill = "red", color = "black") +
  labs(title = "Annual Runoff Coefficient per Gauge",
       subtitle = "Annual Mean Values for Precipitation and Runoff",
       caption = "Source: CAMELS (Addor et al 2017)") + 
  xlab(label = "Year") + ylab(label = "Runoff Coefficient") +
    facet_wrap(~GaugeID, nrow = 3, scales = "free_y") +
  theme_bw(base_family = "Palatino Linotype") + 
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))  

plot(effPlot)

# export as png file 
png(file = "Annual_Runoff_Coeff.png", res = 600, width = 4800, height = 2758, 
    pointsize = 10, type = "windows", antialias = "cleartype")
effPlot
dev.off()
#########################################################
# CHLOROPLETH MAP

# load libraries 
library(maps) # draw geographical maps
library(ggmap) # plots a raster object from "get_map"
library(sf) # simple features, geometry for spatial vector data

# get gauge ids
gaugeIds = unique(datatable2$GaugeID)

# add state 
gaugeState = rep(c("Kansas", "Texas", "Arizona", "California"), 
                 times = c(6, 4, 4, 1))

# USGS gauge latitudes (CAMELS attributes file)
gaugeLat = as.numeric(c(39.94778, 39.23833, 38.56701, 38.71069, 37.12891,
                        38.19645, 33.55455, 30.0641, 29.97938, 28.29195, 
                        33.82227, 33.73644, 33.84311, 33.82783, 36.21468))

# USGS gauge longitudes
gaugeLong = as.numeric(c(-96.10862, -95.8886, -95.96163, -95.83603, -97.60144, 
                         -96.82458, -96.94723, -99.38699, -97.91, -97.27916, 
                         -109.81454, -110.16677, -110.55761, -110.85623, -120.47071))

# calculate average runoff efficiency value per gauge based on annual 
# values (1981 - 2014 averaged), resulting in 1 value per gauge
gaugeEff = runEff %>% 
  group_by(GaugeID) %>% 
  summarise(mean(Eff))

# combine USGS gauge IDs, latitude and longitude into one datatable
gaugeLatLong = tibble(GaugeID = gaugeIds, State = gaugeState, 
                      Lat = gaugeLat, Long = gaugeLong, runEff = gaugeEff$`mean(Eff)`)

# load geographic coordinates of each US date using ggplot2 package mapdata
usStates = map_data("state")

# ggplot map 
stateMap = ggplot(data = usStates) +
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), 
               color = "white") + coord_fixed(1.3) +    # coord = aspect ratio y/x
               guides(fill = FALSE)

# create a bounding box for the gauge locations, subset of the US
area = make_bbox(lon = gaugeLatLong$Long, lat = gaugeLatLong$Lat, f = 0.1) # f = range fraction

# try ggmap 
# add gauges as points and graduated colors showing runoff efficiency values 
stateMap = get_map(location = area, maptype = "satellite", source = "google")
gaugeMap = ggmap(stateMap, extent = "normal") +
  geom_point(aes(x = Long, y = Lat, color = runEff ), 
             data = gaugeLatLong) + 
            xlab("Longitude (°W)") + ylab("Latitude (°N)") +    # alt + 0176 degree symbol
            labs(title = "Gauge Locations and Runoff Efficiency (1981-2014)", 
                 subtitle = "Annual mean precipitation and runoff used", 
                 caption = "Source: CAMELS (Addor et al 2017)") +
                scale_fill_discrete(name = "Q/P") 
 
gaugeMap                                      

# export as png file 
png(file = "gaugeMapRunEff.png", res = 600, width = 4800, height = 2758, 
    pointsize = 10, type = "windows", antialias = "cleartype")
gaugeMap
dev.off()
