# SCRIPT 3 - Analysis of Precipitation
# daily rainfall station data and lows
# upper level & kona lows per stations

##################
##### SET UP #####
##################

# set path to root directory
# rootDir<-"/Volumes/Mac_Passport/KonaLows/"
rootDir<-"C:/Users/lkaiser/Dropbox/kl_update/"
# rootDir<-"C:/Users/Lauren/Dropbox/kl_update/"
# set working directory
setwd(rootDir)

# set data path
dataDir<-paste0(rootDir, "stn_data/")
# set processed stations database path
selStns<-paste0(dataDir, "db_rf/")
# list of 70 stations file names
stn_files<-list.files(selStns)

# set output path
outDir<-paste0(rootDir, "outputs/")
# water year output path
wyDir<-paste0(outDir, "stn_prcp_per_wy/")
# cool season water year output path
cwyDir<-paste0(outDir, "stn_cool_prcp_per_wy/")
# statistics output path
statsDir<-paste0(outDir, "prcp_stats/")

# write console outputs to text file to record processing 
sink(file(paste0(outDir, Sys.Date(), "_prcp_analysis.txt"), open = "wt"))

# time stamp to begin data formatting and preparation
cat('\n analysis of precipitation started', format(Sys.time(), "%a %b %d %Y %X"))

#####################
##### LOAD DATA #####
#####################

# load raw data of all low dates
all_lows<-read.csv("all_low_dates_1980-2014.csv")
names(all_lows)

# load water year reference date index ID data
wy_id<-read.csv("WaterYear_ID.csv")
# check data
wy_id

# function to calculate totals and percentages of precipitation (mm)
# apply to different temporal subsets of precipitation station data
prcp_analysis<-function(data_subset) {
  # calculate the sum of total rainfall (mm)
  prcp_sum<-sum(data_subset$PRCP, na.rm = TRUE)
  # calculate the total rainfall (mm) associated with lows
  ul_prcp<-sum(data_subset$PRCP[which(data_subset$UL == 1)], na.rm = TRUE)  # upper level
  kl_prcp<-sum(data_subset$PRCP[which(data_subset$KL == 1)], na.rm = TRUE)  # kona lows
  # calculate the percentage (%) of rainfall attributed to lows 
  ul_prcp_pct<-ul_prcp/prcp_sum * 100  # upper level
  kl_prcp_pct<-kl_prcp/prcp_sum * 100  # kona lows
  
  # combine station calculations 
  prcp_stats<-data.frame(prcp_sum, ul_prcp, kl_prcp, ul_prcp_pct, kl_prcp_pct)
  return(prcp_stats)
}

##################################
##### PRECIPITATION ANALYSIS #####
##################################

# loop through all 70 selected stations and analyze precipitation (in mm) trends
for (stn in 1:length(stn_files)) {  # set stn = 1 for debugging
  # open first station file 
  stn_data<-read.csv(paste0(selStns, stn_files[stn]), header = TRUE)
  
  ### BY STATION ###
  
  prcp_per_stn<-data.frame(stn_files[stn], prcp_analysis(stn_data))
  
  ### BY COOL SEASON PER STATION ###
  
  cool_season<-subset(stn_data, stn_data$MONTH < 5 | stn_data$MONTH > 9)
  cool_prcp_per_stn<-data.frame(stn_files[stn], prcp_analysis(cool_season))
  
  ### BY WATER YEAR ###
    
  # loop through all 34 water years (October 1 - September 30)
  for (wy in 1:length(wy_id$YEAR)) { # set wy = 2 for debugging
    # subset station data by water year index
    single_wy<-subset(stn_data, 
                      stn_data$INDEX >= wy_id[wy, 3] & stn_data$INDEX <= wy_id[wy, 4])
    # calculate precipitation statistics for each water year
    prcp_per_wy<-prcp_analysis(single_wy)
    # add additional water years
    if(wy == 1) {
      stn_prcp_per_wy<-prcp_per_wy
    } else {
      stn_prcp_per_wy<-rbind(stn_prcp_per_wy, prcp_per_wy)
    } # END adding additional water year data
  } # END loop through all 34 water years
  
  # add year column per water year to front of data frame
  stn_prcp_per_wy<-cbind(YEAR = 1981:2014, stn_prcp_per_wy)
  # save water year precipitation per station
  write.csv(stn_prcp_per_wy, file = paste0(wyDir, stn_files[stn]))
    
  # decontstruct precipitation statistics per water year
  total_prcp_per_wy<-data.frame(stn_prcp_per_wy$prcp_sum)  # station total
  ul_prcp_per_wy<-data.frame(stn_prcp_per_wy$ul_prcp)      # upper level (mm)
  kl_prcp_per_wy<-data.frame(stn_prcp_per_wy$kl_prcp)      # kona low (mm)
  ul_pct_per_wy<-data.frame(stn_prcp_per_wy$ul_prcp_pct)   # upper level (%)
  kl_pct_per_wy<-data.frame(stn_prcp_per_wy$kl_prcp_pct)   # kona low (%)
  # rename columns per station
  names(total_prcp_per_wy)<-stn_files[stn]
  names(ul_prcp_per_wy)<-stn_files[stn]
  names(kl_prcp_per_wy)<-stn_files[stn]
  names(ul_pct_per_wy)<-stn_files[stn]
  names(kl_pct_per_wy)<-stn_files[stn]
  
  ### BY COOL SEASON PER WATER YEAR ###
  
  # loop through all 34 water years (October 1 - September 30)
  for (cwy in 1:length(wy_id$YEAR)) { # set cwy = 2 for debugging
    # subset station data by water year index
    single_cwy<-subset(cool_season, 
                       cool_season$INDEX>=wy_id[cwy, 3] & cool_season$INDEX<=wy_id[cwy, 4])
    # calculate precipitation statistics for each water year
    cool_prcp_per_wy<-prcp_analysis(single_cwy)
    # add additional water years
    if(cwy == 1) {
      stn_cool_prcp_per_wy<-cool_prcp_per_wy
    } else {
      stn_cool_prcp_per_wy<-rbind(stn_cool_prcp_per_wy, cool_prcp_per_wy)
    } # END adding additional water year data
  } # END loop through all 34 water years
  
  # add year column per water year to front of data frame
  stn_cool_prcp_per_wy<-cbind(YEAR = 1981:2014, stn_cool_prcp_per_wy)
  # save water year precipitation per station
  write.csv(stn_cool_prcp_per_wy, file = paste0(cwyDir, stn_files[stn]))
  
  # decontstruct precipitation statistics per water year
  total_cool_prcp_per_wy<-data.frame(stn_cool_prcp_per_wy$prcp_sum)  # station total
  ul_cool_prcp_per_wy<-data.frame(stn_cool_prcp_per_wy$ul_prcp)      # upper level (mm)
  kl_cool_prcp_per_wy<-data.frame(stn_cool_prcp_per_wy$kl_prcp)      # kona low (mm)
  ul_cool_pct_per_wy<-data.frame(stn_cool_prcp_per_wy$ul_prcp_pct)   # upper level (%)
  kl_cool_pct_per_wy<-data.frame(stn_cool_prcp_per_wy$kl_prcp_pct)   # kona low (%)
  # rename columns per station
  names(total_cool_prcp_per_wy)<-stn_files[stn]
  names(ul_cool_prcp_per_wy)<-stn_files[stn]
  names(kl_cool_prcp_per_wy)<-stn_files[stn]
  names(ul_cool_pct_per_wy)<-stn_files[stn]
  names(kl_cool_pct_per_wy)<-stn_files[stn]
  
  ### CREATE COMBINED DATA SETS PER STATION ###
  
  # add additional station data to precipitation statistics data frames
  if(stn == 1) {
    # by station
    prcp_all_stns<-prcp_per_stn
    # by cool season per station
    cool_prcp_all_stns<-cool_prcp_per_stn
    # by water year
    stns_total_prcp_per_wy<-cbind(YEAR = 1981:2014, total_prcp_per_wy)
    stns_ul_prcp_per_wy<-cbind(YEAR = 1981:2014, ul_prcp_per_wy)
    stns_kl_prcp_per_wy<-cbind(YEAR = 1981:2014, kl_prcp_per_wy)
    stns_ul_pct_per_wy<-cbind(YEAR = 1981:2014, ul_pct_per_wy)
    stns_kl_pct_per_wy<-cbind(YEAR = 1981:2014, kl_pct_per_wy)
    # by cool season per water year
    stns_total_cool_prcp_per_wy<-cbind(YEAR = 1981:2014, total_cool_prcp_per_wy)
    stns_ul_cool_prcp_per_wy<-cbind(YEAR = 1981:2014, ul_cool_prcp_per_wy)
    stns_kl_cool_prcp_per_wy<-cbind(YEAR = 1981:2014, kl_cool_prcp_per_wy)
    stns_ul_cool_pct_per_wy<-cbind(YEAR = 1981:2014, ul_cool_pct_per_wy)
    stns_kl_cool_pct_per_wy<-cbind(YEAR = 1981:2014, kl_cool_pct_per_wy)
    
  } else {
    # by station
    prcp_all_stns<-rbind(prcp_all_stns, prcp_per_stn)
    # by cool season per station
    cool_prcp_all_stns<-rbind(cool_prcp_all_stns, cool_prcp_per_stn)
    # by water year
    stns_total_prcp_per_wy<-cbind(stns_total_prcp_per_wy, total_prcp_per_wy)
    stns_ul_prcp_per_wy<-cbind(stns_ul_prcp_per_wy, ul_prcp_per_wy)
    stns_kl_prcp_per_wy<-cbind(stns_kl_prcp_per_wy, kl_prcp_per_wy)
    stns_ul_pct_per_wy<-cbind(stns_ul_pct_per_wy, ul_pct_per_wy)
    stns_kl_pct_per_wy<-cbind(stns_kl_pct_per_wy, kl_pct_per_wy)
    # by cool season per water year
    stns_total_cool_prcp_per_wy<-cbind(stns_total_cool_prcp_per_wy, total_cool_prcp_per_wy)
    stns_ul_cool_prcp_per_wy<-cbind(stns_ul_cool_prcp_per_wy, ul_cool_prcp_per_wy)
    stns_kl_cool_prcp_per_wy<-cbind(stns_kl_cool_prcp_per_wy, kl_cool_prcp_per_wy)
    stns_ul_cool_pct_per_wy<-cbind(stns_ul_cool_pct_per_wy, ul_cool_pct_per_wy)
    stns_kl_cool_pct_per_wy<-cbind(stns_kl_cool_pct_per_wy, kl_cool_pct_per_wy)
  } # END adding additional station data
  
} # END loop through all 70 stations
  
# save complete station data precipitation statistics (FOR GRAPHS)
write.csv(prcp_all_stns, paste0(outDir, "prcp_stats_per_stn.csv"))
write.csv(cool_prcp_all_stns, paste0(outDir, "cool_prcp_stats_per_stn.csv"))
# single statistic by station per water year
write.csv(stns_total_prcp_per_wy, file = paste0(statsDir, "wy_prcp_per_stn.csv"))
write.csv(stns_ul_prcp_per_wy, file = paste0(statsDir, "wy_ul_prcp_per_stn.csv"))
write.csv(stns_kl_prcp_per_wy, file = paste0(statsDir, "wy_kl_prcp_per_stn.csv"))
write.csv(stns_ul_pct_per_wy, file = paste0(statsDir, "wy_ul_pct_per_stn.csv"))
write.csv(stns_kl_pct_per_wy, file = paste0(statsDir, "wy_kl_pct_per_stn.csv"))

write.csv(stns_total_cool_prcp_per_wy, file = paste0(statsDir, "wy_cool_prcp_per_stn.csv"))
write.csv(stns_ul_cool_prcp_per_wy, file = paste0(statsDir, "wy_ul_cool_prcp_per_stn.csv"))
write.csv(stns_kl_cool_prcp_per_wy, file = paste0(statsDir, "wy_kl_cool_prcp_per_stn.csv"))
write.csv(stns_ul_cool_pct_per_wy, file = paste0(statsDir, "wy_ul_cool_pct_per_stn.csv"))
write.csv(stns_kl_cool_pct_per_wy, file = paste0(statsDir, "wy_kl_cool_pct_per_stn.csv"))

### SUMMARY STATISTICS ###

# set names for statistics to be calculated
stat_names<-c("year", "total_prcp", "ul_prcp", "kl_prcp", "ul_pct", "kl_pct")

# means per water year
mm_mean<-data.frame(rowMeans(stns_total_prcp_per_wy[2:71], na.rm = TRUE))
ul_mm_mean<-data.frame(rowMeans(stns_ul_prcp_per_wy[2:71], na.rm = TRUE))
kl_mm_mean<-data.frame(rowMeans(stns_kl_prcp_per_wy[2:71], na.rm = TRUE))
ul_pct_mean<-data.frame(rowMeans(stns_ul_pct_per_wy[2:71], na.rm = TRUE))
kl_pct_mean<-data.frame(rowMeans(stns_kl_pct_per_wy[2:71], na.rm = TRUE))
# combined means per water year
all_means<-cbind(YEAR = 1981:2014, mm_mean, 
                 ul_mm_mean, kl_mm_mean, 
                 ul_pct_mean, kl_pct_mean)
names(all_means)<-stat_names

# cool season means per water year
cool_mm_mean<-data.frame(rowMeans(stns_total_cool_prcp_per_wy[2:71], na.rm = TRUE))
ul_cool_mm_mean<-data.frame(rowMeans(stns_ul_cool_prcp_per_wy[2:71], na.rm = TRUE))
kl_cool_mm_mean<-data.frame(rowMeans(stns_kl_cool_prcp_per_wy[2:71], na.rm = TRUE))
ul_cool_pct_mean<-data.frame(rowMeans(stns_ul_cool_pct_per_wy[2:71], na.rm = TRUE))
kl_cool_pct_mean<-data.frame(rowMeans(stns_kl_cool_pct_per_wy[2:71], na.rm = TRUE))
# combined cool season means per water year
all_cool_means<-cbind(YEAR = 1981:2014, cool_mm_mean, 
                 ul_cool_mm_mean, kl_cool_mm_mean, 
                 ul_cool_pct_mean, kl_cool_pct_mean)
names(all_cool_means)<-stat_names

# save summary statistics (FOR MAPS)
write.csv(all_means, file = paste0(outDir, "wy_mean_stats.csv"))
write.csv(all_cool_means, file = paste0(outDir, "wy_cool_mean_stats.csv"))


  # TO BE CONTINUED
  # MAX AND MIN PER STATION/YEAR?
  
  # select maximum kona low contribution per year
  stn_max_row<-which.max(stn_yrs$kl_pct)
  stn_kl_max<-data.frame(stns[n, ], stn_yrs[stn_max_row, ])
  
  
# time stamp to end data formatting and preparation 
cat('\n precip analyzed', format(Sys.time(), "%a %b %d %Y %X"))
# return output to console
sink(NULL)
  
#############################
##### END PRCP ANALYSIS #####
#############################