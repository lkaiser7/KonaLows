# SCRIPT 4 - Visualization of Analysis
# statistics of precipitation and lows
# graphs and maps of 3_prcp_analysis.R

##################
##### SET UP #####
##################

# load necessary packages 
library("rworldmap")
library("rworldxtra")
library("raster")
library("rgeos")
library("ggmap")

# set path to root directory
# rootDir<-"/Volumes/Mac_Passport/KonaLows/"
rootDir<-"C:/Users/lkaiser/Dropbox/kl_update/"
# rootDir<-"C:/Users/Lauren/Dropbox/kl_update/"
# set working directory
setwd(rootDir)

# set output path
outDir<-paste0(rootDir, "outputs/")
# set figures output path
figDir<-paste0(outDir, "figures/")

# write console outputs to text file to record processing 
sink(file(paste0(outDir, Sys.Date(), "_visualization.txt"), open = "wt"))

# time stamp to begin data formatting and preparation
cat('\n visualization of analysis started', format(Sys.time(), "%a %b %d %Y %X"))

#####################
##### LOAD DATA #####
#####################

# load calculated statistics for graphs (stats per year)
wy_mean_stats<-read.csv(paste0(outDir, "wy_mean_stats.csv"), header = TRUE)
wy_cool_mean_stats<-read.csv(paste0(outDir, "wy_cool_mean_stats.csv"), header = TRUE)

# load map information for station data
map_info<-read.csv("stns_map_info.csv", header = TRUE)

# load mapping data
world_map<-getMap(resolution = "high")
# load map of Hawaii
hi_map<-crop(world_map, extent(-160, -145, 15, 25))
# plot(hi_map, axes = TRUE)

# load Google Mmp of Hawaii
# map_hi<-get_map(location = c(-157.5, 20.5), zoom = 7)
# ggmap(map_hi)

# load calculated statistics for maps (stats per station)
prcp_stats_per_stn<-read.csv(paste0(outDir, "prcp_stats_per_stn.csv"), header = TRUE)
cool_prcp_stats_per_stn<-read.csv(paste0(outDir, "cool_prcp_stats_per_stn.csv"), header = T)

# rename column header to use to merge data
names(prcp_stats_per_stn)[2]<-"FILE"
names(cool_prcp_stats_per_stn)[2]<-"FILE"

# merge statistics and map data
stn_stats_map_info<-merge(map_info, prcp_stats_per_stn, by = "FILE")
cool_stn_stats_map_info<-merge(map_info, cool_prcp_stats_per_stn, by = "FILE")

#########################
##### GRAPH OUTPUTS #####
#########################

# function to graph time series of lows per water year
low_graphs<-function(fig_name, xdat, ul_dat, kl_dat, 
                     ymax, main_name, x_name, y_name) {
  # create blank tiff files to save image outputs
  tiff(paste0(figDir, fig_name, ".tif"), res = 300, units = "in", 
       pointsize = 12, width = 15, height = 10, compression = "lzw")
  
  # plot upper level low precipitation contribution
  plot(xdat, ul_dat, type = "o", col = "red", pch = 16,
       ylim = c(0, ymax), main = main_name, xlab = x_name, ylab = y_name)
  
  par(new = TRUE)
  
  # plot kona low precipitation contribution
  plot(xdat, kl_dat, type = "o", col = "blue", pch = 16,
       ylim = c(0, ymax), xlab = "", ylab = "", main = "")
  
  # add legend
  legend("topright", c("Upper Level", "Kona Lows"), bty = "n",
         col = c("red", "blue"), pch = 15) 
  
  # save plot 
  dev.off()
}
# annual contribution from lows per water year
low_prcp<-low_graphs("mean_prcp_per_wy", wy_mean_stats$year, 
                     wy_mean_stats$ul_prcp, wy_mean_stats$kl_prcp, 350,
                     "Mean Precipitation from Lows", "Year", "Precipitation (mm)")
low_pct<-low_graphs("mean_pct_per_wy", wy_mean_stats$year, 
                    wy_mean_stats$ul_pct, wy_mean_stats$kl_pct, 35,
                    "Cool Season Mean Percent Contribution from Lows", 
                    "Year", "Percentage (%)")
# cool season contribution from lows per water year 
cool_low_prcp<-low_graphs("mean_cool_prcp_per_wy", wy_cool_mean_stats$year, 
                          wy_cool_mean_stats$ul_prcp, wy_cool_mean_stats$kl_prcp, 350,
                          "Mean Precipitation from Lows", "Year", "Precipitation (mm)")

cool_low_pct<-low_graphs("mean_cool_pct_per_wy", wy_cool_mean_stats$year, 
                        wy_cool_mean_stats$ul_pct, wy_cool_mean_stats$kl_pct, 50,
                        "Cool Season Mean Percent Contribution from Lows", 
                        "Year", "Percentage (%)")

# time stamp to end graphs
cat('\n graphs plotted', format(Sys.time(), "%a %b %d %Y %X"))

#######################
##### MAP OUTPUTS #####
#######################


# plot map with different color of points relavtive to data
plot(hi_map, axes = TRUE, main = "Percent of Annual Rainfall from Kona Lows",
     xlab = "Longitude", ylab = "Latitude")
points(stn_stats_map_info$LON, stn_stats_map_info$LAT, 
       cex = cool_stn_stats_map_info$prcp_sum/20000, 
       bg = "steelblue1", pch = 21, lwd = 0.4)
legend("bottomleft", c("< 5%", "5-8%", "> 8%"), bty = "n",
       col = "steelblue1", pch = 19, 
       pt.cex = c(summary(stn_stats_map_info$ul_prcp_pct)[1]/10,
                  summary(stn_stats_map_info$ul_prcp_pct)[4]/10,
                  summary(stn_stats_map_info$ul_prcp_pct)[6]/10))



# google map
mapPoints<-ggmap(map_hi) +
  geom_point(aes(x = LON, y = LAT, size = kl_prcp_pct, 
                 colour = "green"), data = stn_stats_map_info, show_guide = F)
mapPoints
mapPtsKey<-mapPoints + 
  scale_size_area(breaks = c(2, 5, 7, 9), 
                  labels = c("< 2%", "2-5%", "5-9%", "> 9%" ),
                  name = "Percent (%)")
mapPtsKey


# function to map station data of amount of low precipitation lows per station
low_maps<-function(fig_name, main_name, lon_dat, lat_dat,
                   cex_size, bg_col, key_name, key_levels) {
  # create blank tiff files to save image outputs
  tiff(paste0(figDir, fig_name, ".tif"), res = 300, units = "in", 
       pointsize = 12, width = 15, height = 10, compression = "lzw")
  
  # plot base map of Hawaii 
  plot(hi_map, axes = TRUE, main = main_name,
       xlab = "Longitude", ylab = "Latitude")
  # add data points and map by symbol size and/or color
  points(lon_dat, lat_dat, pch = 21, lwd = 0.4,
         cex = cex_size, bg = bg_col)
  
  # add legend
  legend("bottomleft", key_levels, bty = "n",
         title = key_name, col = "steelblue1", pch = 19, 
         # change selected summary statistics [#] for mm vs. %
         pt.cex = c(summary(cex_size)[1],   #min
                    summary(cex_size)[3],   #mean
                    summary(cex_size)[6]))  #max
  # save plot 
  dev.off()
}
main_name<-"Total Annual Station Precipitation"
lon_dat<-stn_stats_map_info$LON
lat_dat<-stn_stats_map_info$LAT
cex_size<-stn_stats_map_info$prcp_sum/25000
bg_col<-"steelblue1"
key_name<-"Precipitation (mm)"
key_levels<- c("< 16500 mm", "16500-35000 mm", "> 50000 mm")


# rainfall totals per station
stn_prcp<-low_maps("annual_stn_prcp", "Total Annual Station Precipitation",
                   stn_stats_map_info$LON, stn_stats_map_info$LAT,
                   stn_stats_map_info$prcp_sum/25000, "steelblue1", 
                   "Precipitation (mm)", c("< 16500 mm", "16500-35000 mm", "> 35000 mm"))
cool_prcp<-low_maps("cool_stn_prcp", "Total Cool Season Station Precipitation",
                    cool_stn_stats_map_info$LON, cool_stn_stats_map_info$LAT,
                    cool_stn_stats_map_info$prcp_sum/20000, "steelblue1",
                    "Precipitation (mm)", c("< 13500 mm", "13500-35500 mm", "> 35500 mm"))
# annual contribution from lows per water year
ul_prcp_amt<-low_maps("ul_stn_prcp", "Upper Level Low Precipitation",
                      stn_stats_map_info$LON, stn_stats_map_info$LAT,
                      stn_stats_map_info$ul_prcp/1000, "steelblue1",
                      "Precipitation (mm)", c("< 2400 mm", "2400-3800 mm", "> 3800 mm"))
kl_prcp_amt<-low_maps("kl_stn_prcp", "Kona Low Precipitation",
                      stn_stats_map_info$LON, stn_stats_map_info$LAT,
                      stn_stats_map_info$kl_prcp/1000, "steelblue1",
                      "Precipitation (mm)", c("< 1600 mm", "1600-3000 mm", "> 3000 mm"))
ul_pct_amt<-low_maps("ul_stn_pct", "Percent of Precipitation from Upper Level Lows",
                     stn_stats_map_info$LON, stn_stats_map_info$LAT,
                     stn_stats_map_info$ul_prcp_pct/7, "steelblue1",
                     "Percent (%)", c("< 10%", "10-14%", "> 14%"))
kl_pct_amt<-low_maps("kl_stn_pct", "Percent of Precipitation from Kona Lows",
                     stn_stats_map_info$LON, stn_stats_map_info$LAT,
                     stn_stats_map_info$kl_prcp_pct/5, "steelblue1",
                     "Percent (%)", c("< 6%", "6-10%", "> 10%"))

# cool season contribution from lows per water year
cool_ul_prcp_amt<-low_maps("ul_cool_prcp", "Upper Level Low Cool Season Precipitation",
                           cool_stn_stats_map_info$LON, cool_stn_stats_map_info$LAT,
                           cool_stn_stats_map_info$ul_prcp/1000, "steelblue1",
                           "Precipitation (mm)", c("< 2400 mm", "2400-5200 mm", "> 5200 mm"))
cool_kl_prcp_amt<-low_maps("kl_cool_prcp", "Kona Low Cool Season Precipitation",
                           cool_stn_stats_map_info$LON, cool_stn_stats_map_info$LAT,
                           cool_stn_stats_map_info$kl_prcp/1000, "steelblue1",
                           "Precipitation (mm)", c("< 1600 mm", "1600-3000 mm", "> 3000 mm"))
cool_ul_pct_amt<-low_maps("ul_cool_pct", 
                          "Percent of Cool Season Precipitation from Upper Level Lows",
                          cool_stn_stats_map_info$LON, cool_stn_stats_map_info$LAT,
                          cool_stn_stats_map_info$ul_prcp_pct/7, "steelblue1",
                          "Percent (%)", c("< 14%", "14-20%", "> 20%"))
cool_kl_pct_amt<-low_maps("kl_cool_pct", 
                          "Percent of Cool Season Precipitation from Kona Lows",
                          cool_stn_stats_map_info$LON, stn_stats_map_info$LAT,
                          cool_stn_stats_map_info$kl_prcp_pct/5, "steelblue1",
                          "Percent (%)", c("< 8%", "8-13%", "> 13%"))

# time stamp to end mapping
cat('\n maps created', format(Sys.time(), "%a %b %d %Y %X"))
# return output to console
sink(NULL)

#############################
##### END VISUALIZATION #####
#############################