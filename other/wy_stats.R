# Created October 16, 2014 by Lauren Kaiser

# SCRIPT #
# Data analysis based on water years (October - September)
# Determines rainfall per year based on occurrences of lows

# set working directory and read in data file
setwd("/Volumes/Mac Passport/Final_Thesis")

# load data files for start and end dates of water years
wy_id<-read.csv("Data/WaterYear_ID.csv", header = TRUE)

# loop through database files to calculate sum of total precipitation
db_files<-list.files("Data/low_prcp")
for (i in 1:length(db_files)) {
  # open first station file to analyze
  rf_dat<-read.csv(paste("Data/low_prcp/", db_files[i], sep = ""), header = TRUE)
  
  # calculate sum per each water year and per event (Oct-Sep)
  for (k in 1:length(wy_id$YEAR)) {
    wy_dat<-subset(rf_dat, rf_dat$INDEX >= wy_id[k, 3] & rf_dat$INDEX <= wy_id[k, 4])
    wy_ul<-wy_dat[which(wy_dat$UL_LOW == 1), ]
    wy_kl<-wy_dat[which(wy_dat$KONA_LOW == 1), ]
    wy_sum<-sum(wy_dat$PRCP, na.rm = TRUE)
    wy_sum<-cbind(wy_dat$YEAR[k] + 1, wy_sum)
    ul_sum<-sum(wy_ul$PRCP, na.rm = TRUE)
    ul_sum<-cbind(wy_dat$YEAR[k] + 1, ul_sum)
    kl_sum<-sum(wy_kl$PRCP, na.rm = TRUE)
    kl_sum<-cbind(wy_dat$YEAR[k] + 1, kl_sum)
    
    if(k == 1) {
      wy_tot<-wy_sum
      ul_tot<-ul_sum
      kl_tot<-kl_sum
    } else {
      if(k > 1) {
        wy_tot<-rbind(wy_tot, wy_sum)
        ul_tot<-rbind(ul_tot, ul_sum)
        kl_tot<-rbind(kl_tot, kl_sum)
      }
    }
  }
  
  # add each annual sum per station to output file
  if(i == 1) {
    wy_ann<-wy_tot
    ul_ann<-ul_tot
    kl_ann<-kl_tot
  } else {
    if(i > 1) {
      wy_ann<-cbind(wy_ann, wy_tot[ , 2])
      ul_ann<-cbind(ul_ann, ul_tot[ , 2])
      kl_ann<-cbind(kl_ann, kl_tot[ , 2])
    }
  }
}

# match columns to station file names and save output
colnames(wy_ann)<-c("year", db_files)
colnames(ul_ann)<-c("year", db_files)
colnames(kl_ann)<-c("year", db_files)
write.csv(wy_ann, file = "D_Out/tot_prcpXwy.csv")
write.csv(ul_ann, file = "D_Out/ul_prcpXwy.csv")
write.csv(kl_ann, file = "D_Out/kl_prcpXwy.csv")

# total rainfall per year (34 years)
wy_sumXwy<-rowSums(wy_ann[ , -1])
ul_sumXwy<-rowSums(ul_ann[ , -1])
kl_sumXwy<-rowSums(kl_ann[ , -1])
# mean annual rainfall per year
wy_meanXwy<-rowMeans(wy_ann[ , -1])
ul_meanXwy<-rowMeans(ul_ann[ , -1])
kl_meanXwy<-rowMeans(kl_ann[ , -1])

# annual total rainfall per station (74 stations)
wy_sumXstn<-colSums(wy_ann[ , -1])
ul_sumXstn<-colSums(ul_ann[ , -1])
kl_sumXstn<-colSums(kl_ann[ , -1])
# mean total rainfall per station
wy_meanXstn<-colMeans(wy_ann[ , -1])
ul_meanXstn<-colMeans(ul_ann[ , -1])
kl_meanXstn<-colMeans(kl_ann[ , -1])

# create output files for annual and station data
all_yrs<-data.frame(1981:2014, wy_sumXwy, ul_sumXwy, kl_sumXwy,
                      wy_meanXwy, ul_meanXwy, kl_meanXwy)
names(all_yrs)<-c("YEAR", "YR_SUM", "UL_SUM", "KL_SUM",
                   "YR_MEAN", "UL_MEAN", "KL_MEAN")
all_stns<-data.frame(wy_sumXstn, ul_sumXstn, kl_sumXstn,
                     wy_meanXstn, ul_meanXstn, kl_meanXstn)
names(all_stns)<-c("YR_SUM", "UL_SUM", "KL_SUM",
                   "YR_MEAN", "UL_MEAN", "KL_MEAN")

# calculate percent rainfall contribution from mean values
all_yrs$UL_PCT<-with(all_yrs, {UL_MEAN/YR_MEAN*100})
all_yrs$KL_PCT<-with(all_yrs, {KL_MEAN/YR_MEAN*100})
all_stns$UL_PCT<-with(all_stns, {UL_MEAN/YR_MEAN*100})
all_stns$KL_PCT<-with(all_stns, {KL_MEAN/YR_MEAN*100})

# save output files
write.csv(all_yrs, file = "D_Out/pctXwy.csv")
write.csv(all_stns, file = "D_Out/pctXstn.csv")

# merge data to map
datXstn<-read.csv("D_Out/pctXstn.csv", header = TRUE)
prcp_db<-read.csv("D_Out/prcp_db.csv", header = TRUE)
map_dat<-merge(datXstn, prcp_db, by = "FILE")
write.csv(map_dat, file = "D_Out/map_data.csv")
