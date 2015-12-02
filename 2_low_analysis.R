# SCRIPT 2 - Analysis of Lows
# overview of low data record
# upper level & kona low data

##################
##### SET UP #####
##################

# install necessary packages
library("plyr")

# set path to root directory
# rootDir<-"/Volumes/Mac_Passport/KonaLows/"
rootDir<-"C:/Users/lkaiser/Dropbox/kl_update/"
# rootDir<-"C:/Users/Lauren/Dropbox/kl_update/"
# set working directory
setwd(rootDir)

# set data path
dataDir<-paste0(rootDir, "stn_data/")
# set raw station data path
rawStns<-paste0(dataDir, "raw_rf/")
# set processed stations database path
selStns<-paste0(dataDir, "db_rf/")

# set output path
outDir<-paste0(rootDir, "outputs/")

# write console outputs to text file to record processing 
sink(file(paste0(outDir, Sys.Date(), "_low_analysis.txt"), open = "wt"))

# time stamp to begin data formatting and preparation
cat('\n analysis of lows started', format(Sys.time(), "%a %b %d %Y %X"))

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

###!!!!!!!!!!!!!!!!!!!!!!!!!###
###-=-=- SELECT INPUTS -=-=-###
###!!!!!!!!!!!!!!!!!!!!!!!!!###

### CHANGE UL TO KL AND RUN FROM HERE ONCE FOR EACH ###

# set types of lows (upper level [1] or kona [2] lows)
types = c("upper_lows", "kona_lows")
# select low type to analyze
type = types[2]
cat('\n', type, 'being analyzed')

# set type of low to use (upper level [UL] or kona [KL] lows)
day_type = all_lows$KL
dur_type = all_lows$KL_DUR

#############################
##### OVERVIEW ANALYSIS #####
#############################

# total number of upper level lows
ul_events<-length(which(all_lows$UL_DUR > 0))
# upper level events per year
ul_per_yr<-ul_events/34
# total number of kona lows
kl_events<-length(which(all_lows$KL_DUR > 0))
# kona low events per year
kl_per_yr<-kl_events/34

cat('\n there are', ul_events, 'upper level and', kl_events, 'kona lows total')
cat('\n and on average ~', ul_per_yr, 'upper level and ~', kl_per_yr, 'kona lows per year')

### LOWS PER YEAR ###

# determine the number of days with lows
low_days<-all_lows[which(day_type == 1), ]
# determine the number of low events
low_events<-all_lows[which(dur_type != 0), ]

# loop through and analyze days and events per water year (October 1 - September 30)
for (wy in 1:length(wy_id$YEAR)) { # set wy = 2 for debugging
  # subset low data by water year index
  days_per_wy<-subset(low_days, 
                      low_days$INDEX >= wy_id[wy, 3] & low_days$INDEX <= wy_id[wy, 4])
  events_per_wy<-subset(low_events, 
                        low_events$INDEX >= wy_id[wy, 3] & low_events$INDEX <= wy_id[wy, 4])
  
  ### CHANGE UL TO KL HERE ###
  # aggregate data by water years
  daysXwy<-data.frame(wy_id[wy, 2], sum(days_per_wy$KL))
  eventsXwy<-data.frame(wy_id[wy, 2], length(events_per_wy$KL_DUR))
  
  # initialize loop to build dataset per water year
  if(wy == 1) {
    # create inital data frame for days and events
    wy_days<-daysXwy  
    wy_events<-eventsXwy
  } else {
    if(wy > 1) {
      # add each additional water year to data frames
      wy_days<-rbind(wy_days, daysXwy) 
      wy_events<-rbind(wy_events, eventsXwy)
    }
  }
}

# rename dataset column headers
names(wy_days)<-c("YEAR", "DAYS")
names(wy_events)<-c("YEAR", "EVENTS")

# merge datasets by year
low_occ<-merge(wy_days, wy_events, by = "YEAR")

# write output file for each low type of occurences per water year
write.csv(low_occ, file = paste0(outDir, type, "_occurrences.csv"))

# calculate fitted linear models for days and events
lm_days<-lm(low_occ$DAYS~low_occ$YEAR)
summary(lm_days)
lm_events<-lm(low_occ$EVENTS~low_occ$YEAR)
summary(lm_events)
# calculate fitted polynomial regressions
los_days<-loess(low_occ$DAYS~low_occ$YEAR)
summary(los_days)
los_events<-loess(low_occ$EVENTS~low_occ$YEAR)
summary(los_events)

### CHANGE UL TO KL IN MAIN AND SET YLIM() ###

# create blank tiff files to save image outputs
tiff(paste0(outDir, type, "_barplot.tif"), res = 300, units = "in", 
     pointsize = 12, width = 10, height = 10, compression = "lzw")

# create barlots of number of days and events of lows
day_bp<-barplot(low_occ$DAYS, ylim = c(0, 30), border = "red",
                names.arg = 1981:2014, las = 2, cex.names = 0.75,
                xlab = "Year", ylab = "Occurrences",
                main = "Number of Kona Low Days and Events")
lines(day_bp, predict(los_days), lwd = 2, lty = "dashed", col = "red")
lines(day_bp, predict(lm_days), lwd = 3, col = "red")
par(new = TRUE)
event_bp<-barplot(low_occ$EVENTS, ylim = c(0, 30), border = "blue",
                  las = 2, xlab = "", ylab = "", main = "")
lines(event_bp, predict(los_events), lwd = 2, lty = "dashed", col = "blue")
lines(event_bp, predict(lm_events), lwd = 3, col = "blue")
legend("topright", c("Days", "Events"), bty = "n",
       col = c("red", "blue"), pch = 15) 

# save plot 
dev.off()

# create blank tiff files to save image outputs
tiff(paste0(outDir, type, "_timeseries.tif"), res = 300, units = "in", 
     pointsize = 12, width = 10, height = 10, compression = "lzw")

# plot occurrences of low days and events over time
plot(low_occ$YEAR, low_occ$DAYS, ylim = c(0, 30), type = "o", pch = 19, col = "red",
     xlab = "Year", ylab = "Occurrences", 
     main = "Occurrence of Kona Low Days and Events")
abline(lm_days, lwd = 3, col = "red")
par(new = TRUE)
plot(low_occ$YEAR, low_occ$EVENTS, ylim = c(0, 30), type = "o", pch = 19, col = "blue",
     xlab = "", ylab = "", main = "")
abline(lm_events, lwd = 3, col = "blue")
legend("topright", c("Days", "Events"), bty = "n",
       col = c("red", "blue"), pch = 19) 

# save plot 
dev.off()

### LOWS PER MONTH ###

# number of low days each month
ul_days_per_month<-with(all_lows, aggregate(UL, list(MONTH), sum))
kl_days_per_month<-with(all_lows, aggregate(KL, list(MONTH), sum))
# rename column headers
names(ul_days_per_month)<-c("MONTH", "COUNT")
names(kl_days_per_month)<-c("MONTH", "COUNT")

# count number of low events
all_lows$ul_tf<-all_lows$UL_DUR != 0
all_lows$kl_tf<-all_lows$KL_DUR != 0
# number of low events each month
ul_events_per_month<-aggregate(all_lows$ul_tf, list(all_lows$MONTH), sum)
kl_events_per_month<-aggregate(all_lows$kl_tf, list(all_lows$MONTH), sum)
# rename column headers
names(ul_events_per_month)<-c("MONTH", "COUNT")
names(kl_events_per_month)<-c("MONTH", "COUNT")

# create blank tiff files to save image outputs
tiff(paste0(outDir, "days_per_month_barplot.tif"), res = 300, units = "in", 
     pointsize = 12, width = 10, height = 10, compression = "lzw")

# create barlots of number of days and events of lows
month_days_bp<-barplot(ul_days_per_month$COUNT, ylim = c(0, 200), border = "red",
                       names.arg = 1:12, las = 1, cex.names = 0.75,
                       xlab = "Month", ylab = "Number of Days with Low Occurrences",
                       main = "Low Days per Month")
par(new = TRUE)
event_bp<-barplot(kl_days_per_month$COUNT, ylim = c(0, 200), border = "blue",
                  las = 2, xlab = "", ylab = "", main = "")
legend("topleft", c("Upper Level", "Kona Lows"), bty = "n",
       col = c("red", "blue"), pch = 15) 

# save plot 
dev.off()

# create blank tiff files to save image outputs
tiff(paste0(outDir, "events_per_month_barplot.tif"), res = 300, units = "in", 
     pointsize = 12, width = 10, height = 10, compression = "lzw")

# create barlots of number of days and events of lows
month_days_bp<-barplot(ul_events_per_month$COUNT, ylim = c(0, 50), border = "red",
                       names.arg = 1:12, las = 1, cex.names = 0.75,
                       xlab = "Month", ylab = "Number of Low Occurrences",
                       main = "Low Events per Month")
par(new = TRUE)
event_bp<-barplot(kl_events_per_month$COUNT, ylim = c(0, 50), border = "blue",
                  las = 2, xlab = "", ylab = "", main = "")
legend("topleft", c("Upper Level", "Kona Lows"), bty = "n",
       col = c("red", "blue"), pch = 15) 

# save plot 
dev.off()

# time stamp to end data formatting and preparation 
cat('\n lows analyzed', format(Sys.time(), "%a %b %d %Y %X"))
# return output to console
sink(NULL)

############################
##### END LOW ANALYSIS #####
############################