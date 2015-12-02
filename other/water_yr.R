# Created October 9, 2014 by Lauren Kaiser

# SCRIPT 1
# Determine index value for water years (October-September)
# Year labeled by YYYY of the January within the water year
# Start: October 1
# End: September 30

# set working directory and read in data file
setwd("/Volumes/Mac Passport/Final_Thesis")
lows<-read.csv("Data/lows_dat.csv", header = TRUE)
head(lows)

# function to identify index number for the start and end day of each water year
water_year<-function(yyyy) {
  # start year index
  s_year<-subset(lows$INDEX, lows$YEAR == yyyy & lows$MONTH == 10 & lows$DAY == 1)
  # end year index
  e_year<-subset(lows$INDEX, lows$YEAR == yyyy + 1 & lows$MONTH == 9 & lows$DAY == 30)
  # water year data
  w_year<-data.frame(yyyy + 1, s_year, e_year)
  names(w_year)<-c("YEAR", "OCT_1", "SEP_30")
  return(w_year)
}

# test function output returns water year data
water_year(1980)
# returns > 1980  28033 28397

# loop through all years to identify index number of each water year
# will not produce 1980 water year without previous 1979 data 
for(i in 1980:2013) {
  if(i == 1980) {
    wy_id<-water_year(i)
  } else {
    if(i > 1980) {
      wy_id<-rbind(wy_id, water_year(i))
    } 
  }
} 

# calculate length of year
wy_id$LEN<-wy_id$SEP_30 - wy_id$OCT_1

# save output as .csv file in data folder for future use
write.csv(wy_id, file = "Data/WaterYear_ID.csv")

### END SCRIPT ###