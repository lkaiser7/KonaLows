# SCRIPT 1 - Data Formatting
# combining precipitation and lows data
# prepare data for statistical analysis

##################
##### SET UP #####
##################

# set path to root directory
# rootDir<-"/Volumes/Mac_Passport/KonaLows/"
rootDir<-"C:/Users/lkaiser/Dropbox/kl_update/"
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
sink(file(paste0(outDir, Sys.Date(), "_data_prep.txt"), open = "wt"))

# time stamp to begin data formatting and preparation
cat('\n data prep initiated', format(Sys.time(), "%a %b %d %Y %X"))

#####################
##### LOAD DATA #####
#####################

# load raw data of all low dates
all_lows<-read.csv("all_low_dates_1980-2014.csv")
# check data
head(all_lows)
tail(all_lows)

# load database of 70 stations to use reference data
stn_db<-read.csv(paste0(dataDir, "db_70stn_info.csv"))
# check data
head(stn_db)
tail(stn_db)

#####################
##### DATA PREP #####
#####################

# list 70 selected station data files
stn_files<-stn_db$FILE

# loop to combine station precip data with low dates
for (stn in 1:length(stn_files)) {  # set stn = 1 for debugging
  # open first station file
  stn_fn<-read.csv(paste0(rawStns, stn_files[stn]))
  
  # format a DATES column to be used as a reference between datasets
  stn_fn$DATES<-factor(paste0(substr(stn_fn$DATE, 5, 6), "/",  # month
                              substr(stn_fn$DATE, 7, 8), "/",  # day
                              substr(stn_fn$DATE, 3, 4)))      # year
  
  # merge station data with all lows by DATES column
  all_data<-merge(all_lows, stn_fn, by = "DATES", 
                  all.x = T, sort = F, incomparables = NA)
  
  # sort merged data by index column to keep in sequential order
  all_data<-all_data[order(all_data$INDEX), ]
  
  # subset merged data and keep only necessary columns
  all_data<-all_data[, c(1:12, 14:18, 20)]
  
  # rename columns to remove '.x' from merged names
  names(all_data)[3:6]<-c("MONTH", "DAY", "YEAR", "DATE")
  
  # store station name to save as file name
  filename<-as.character(stn_files[stn])
  # save station files with merged low data to database folder
  write.csv(all_data, file = paste0(selStns, filename))
  
  # print station data to log file
  cat('\n station', filename)
  # check length of merged data to see if record is complete
  cat('\n start:', length(all_data$DATES))   # length = 12265
  # check station percentage of complete data record (> 75%)
  pct_comp<-length(all_data$PRCP[!is.na(all_data$PRCP)])/length(all_data$PRCP)*100
  cat('\n station data record is', pct_comp, '% complete')
  
  # loop through next station file
} # END database station prep loop

# time stamp to end data formatting and preparation 
cat('\n data prepped and ready', format(Sys.time(), "%a %b %d %Y %X"))
# return output to console
sink(NULL)

#########################
##### END DATA PREP #####
#########################