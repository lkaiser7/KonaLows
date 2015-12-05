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

# enso data of strong el nino/la nina years
elnino<-data.frame(c(1983, 1988, 1998), c(5, 26, 8), rep(3, 3))
names(elnino)<-c("YEAR", "ul_ELNINO", "kl_ELNINO")
lanina<-data.frame(c(1989, 2000, 2011), c(31, 32, 44), rep(20, 3))
names(lanina)<-c("YEAR", "ul_LANINA", "kl_LANINA")

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
ul_days<-all_lows[which(all_lows$UL == 1), ]
kl_days<-all_lows[which(all_lows$KL == 1), ]
# determine the number of low events
ul_events<-all_lows[which(all_lows$UL_DUR != 0), ]
kl_events<-all_lows[which(all_lows$KL_DUR != 0), ]

###!!!!!!!!!!!!!!!!!!!!!!!!!###
###-=-=- SELECT INPUTS -=-=-###
###!!!!!!!!!!!!!!!!!!!!!!!!!###

### CHANGE UL TO KL 7 TIMES BELOW AND RUN FROM HERE to LINE 130 ONCE FOR EACH TYPE ###

# set types of lows (upper level [1] or kona [2] lows)
types = c("upper_lows", "kona_lows")
# select low type to analyze
type = types[2]
cat('\n', type, 'being analyzed')

# set type of low to use (upper level [UL] or kona [KL] lows)
low_days<-kl_days
low_events<-kl_events

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
names(wy_days)<-c("YEAR", "kl_DAYS")
names(wy_events)<-c("YEAR", "kl_EVENTS")

# merge datasets by year
low_occ<-merge(wy_days, wy_events, by = "YEAR")

# save for each upper level lows and kona lows
ul_low_occ<-low_occ
kl_low_occ<-low_occ
# merge ul and kl low occurrences once both are run
low_occ<-merge(ul_low_occ, kl_low_occ, by = "YEAR")

# write output file for each low type of occurences per water year
write.csv(low_occ, file = paste0(outDir, "all_low_occurrences.csv"))

# combine enso data with low occurrences
low_occ<-merge(low_occ, elnino, by = "YEAR", all = TRUE)
low_occ<-merge(low_occ, lanina, by = "YEAR", all = TRUE)

# function to create barplots for low occurrences
low_bp<-function(fig_name, l_days, l_events, main_name, ymax, nino, nina) {
  # calculate fitted linear models for days and events
  lm_days<-lm(l_days~low_occ$YEAR)
  lm_events<-lm(l_events~low_occ$YEAR)
  # calculate fitted polynomial regressions
  los_days<-loess(l_days~low_occ$YEAR)
  los_events<-loess(l_events~low_occ$YEAR)
  
  # summarize linear regressions [1:11] for days and events
  d_rsq<-round(as.numeric(summary(lm_days)[8]), 4)          #r-squared
  d_pval<-round(as.numeric(summary(lm_days)[[4]][8]), 4)    #p-value
  e_rsq<-round(as.numeric(summary(lm_events)[8]), 4)        #r-squared
  e_pval<-round(as.numeric(summary(lm_events)[[4]][8]), 4)  #p-value
  
  # create blank tiff files to save image outputs
  tiff(paste0(outDir, fig_name), res = 300, units = "in", 
       pointsize = 12, width = 10, height = 10, compression = "lzw")

  
  # create barlots of number of days and events of lows
  day_bp<-barplot(l_days, ylim = c(0, ymax), border = "red",
                  names.arg = 1981:2014, las = 2, cex.names = 0.75,
                  xlab = "Year", ylab = "Occurrences",
                  main = main_name, beside = TRUE)
  # lines(day_bp, predict(los_days), lwd = 2, lty = "dashed", col = "red")
  lines(day_bp, predict(lm_days), lwd = 1, col = "red", lty = "dashed")
  par(new = TRUE)  # add plot
  event_bp<-barplot(l_events, ylim = c(0, ymax), border = "blue",
                    las = 2, xlab = "", ylab = "", main = "")  
  # lines(event_bp, predict(los_events), lwd = 2, lty = "dashed", col = "blue")
  lines(event_bp, predict(lm_events), lwd = 1, col = "blue", lty = "dashed")
  
  # add text for upper level low and kona low counts
  text(day_bp, l_days, labels = l_days, pos = 3, cex = 0.75, col = "red")
  text(event_bp, l_events, labels = l_events, pos = 3, cex = 0.75, col = "blue")
  
  # add enso events
  text(event_bp, nino, labels = "*", cex = 2, col = "darkorange")
  text(event_bp, nina, labels = "*", cex = 2, col = "forestgreen")
  
  # add r-square and p-values
  mtext(paste0("r^2 = ", d_rsq, "   p-value = ", d_pval), 
        side = 3, padj = 0, col = "red", cex = 1)
  mtext(paste0("r^2 = ", e_rsq, "   p-value = ", e_pval), 
        side = 3, padj = 1, col = "blue", cex = 1)
  
  # add legend to plot
  legend("topleft", legend = c("Days", "Events", "Strong El Nino", "Strong La Nina"), 
         bty = "n", pch = c(15, 15, 8, 8),
         col = c("red", "blue", "darkorange", "forestgreen"))
  
  # save plot 
  dev.off()
  
}

ul_plots<-low_bp("upper_lows_barplot.tif", low_occ$UL_DAYS, low_occ$UL_EVENTS, 
                 "Number of Upper Level Low Days and Events",
                 60, low_occ$ul_ELNINO, low_occ$ul_LANINA)
kl_plots<-low_bp("kona_lows_barplot.tif", low_occ$KL_DAYS, low_occ$KL_EVENTS, 
                 "Number of Kona Low Days and Events", 
                 35, low_occ$kl_ELNINO, low_occ$kl_LANINA)

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
ul_days_bp<-barplot(ul_days_per_month$COUNT, ylim = c(0, 200), border = "red",
                    names.arg = 1:12, las = 1, cex.names = 0.75,
                    xlab = "Month", ylab = "Number of Days with Low Occurrences",
                    main = "Low Days per Month")
par(new = TRUE)
kl_days_bp<-barplot(kl_days_per_month$COUNT, ylim = c(0, 200), border = "blue",
                    las = 2, xlab = "", ylab = "", main = "")

# add text for upper level low and kona low counts
text(ul_days_bp, ul_days_per_month$COUNT, labels = ul_days_per_month$COUNT, 
     pos = 3, cex = 0.75, col = "red")
text(kl_days_bp, kl_days_per_month$COUNT, labels = kl_days_per_month$COUNT, 
     pos = 3, cex = 0.75, col = "blue")

# add legend to plot
legend("topleft", c("Upper Level", "Kona Lows"), bty = "n",
       col = c("red", "blue"), pch = 15) 

# save plot 
dev.off()

# create blank tiff files to save image outputs
tiff(paste0(outDir, "events_per_month_barplot.tif"), res = 300, units = "in", 
     pointsize = 12, width = 10, height = 10, compression = "lzw")

# create barlots of number of days and events of lows
ul_events_bp<-barplot(ul_events_per_month$COUNT, ylim = c(0, 50), border = "red",
                      names.arg = 1:12, las = 1, cex.names = 0.75,
                      xlab = "Month", ylab = "Number of Low Occurrences",
                      main = "Low Events per Month")
par(new = TRUE)
kl_events_bp<-barplot(kl_events_per_month$COUNT, ylim = c(0, 50), border = "blue",
                      las = 2, xlab = "", ylab = "", main = "")

# add text for upper level low and kona low counts
text(ul_events_bp, ul_events_per_month$COUNT, labels = ul_events_per_month$COUNT, 
     pos = 3, cex = 0.75, col = "red")
text(kl_events_bp, kl_events_per_month$COUNT, labels = kl_events_per_month$COUNT, 
     pos = 3, cex = 0.75, col = "blue")

# add legend to plot
legend("topleft", c("Upper Level", "Kona Lows"), bty = "n",
       col = c("red", "blue"), pch = 15) 

# save plot 
dev.off()

# calculate ratio of low events
low_occ[, 10:11]<-cbind(low_occ$KL_DAYS/low_occ$UL_DAYS, low_occ$KL_EVENTS/low_occ$UL_EVENTS)
# replace NaNs from 0/0 
low_occ[c(1,3),10:11]<-0
# rename columns
names(low_occ)[10:11]<-c("d_ratio", "e_ratio")

# create blank tiff files to save image outputs
tiff(paste0(outDir, "low_ratios.tif"), res = 300, units = "in", 
     pointsize = 12, width = 12, height = 8, compression = "lzw")

# ratio days and events of upper level lows and kona lows
plot(low_occ$YEAR, low_occ$d_ratio, ylim = c(-0.25, 1.25),
     type = 'o', pch = 15, col = "red", lwd = 1.75,
     main = "Ratio of Upper Level Lows and Kona Low",
     xlab = "Year", ylab = "Ratio (Kona Lows:Upper Level Lows)")
par(new = TRUE)
plot(low_occ$YEAR, low_occ$e_ratio, ylim = c(-0.25, 1.25),
     type = 'o', pch = 20, col = "blue",
     main = "", xlab = "", ylab = "")

# add legend to plot
legend("topright",  c("Number of Days", "Number of Events"), 
       bty = "n", col = c("red", "blue"), pch = c(15, 19))

# save plot 
dev.off()

# time stamp to end data formatting and preparation 
cat('\n lows analyzed', format(Sys.time(), "%a %b %d %Y %X"))
# return output to console
sink(NULL)

############################
##### END LOW ANALYSIS #####
############################