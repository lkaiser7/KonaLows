
# lows data
lows<-read.csv("data/lows_dates.csv", header = TRUE)
days<-lows[which(lows$UL == 1), ]
events<-lows[which(lows$UL_DUR != 0), ]
# water year indicies
wy_id<-read.csv("data/WaterYear_ID.csv", header = TRUE)

for (k in 1:length(wy_id$YEAR)) {
  # subset station data by water year indicies (October 1 - September 30)
  day_wy<-subset(days, days$INDEX >= wy_id[k, 3] & days$INDEX <= wy_id[k, 4])
  event_wy<-subset(events, events$INDEX >= wy_id[k, 3] & events$INDEX <= wy_id[k, 4])
  
  # aggregate data by water years
  dayXwy<-data.frame(wy_id[k, 2], sum(day_wy$UL))
  eventXwy<-data.frame(wy_id[k, 2], length(event_wy$UL_DUR))
  
  # initialize loop to build dataset of r95p per water year
  if(k == 1) {
    ul_days<-dayXwy # create inital data frame 
    ul_events<-eventXwy
  } else {
    if(k > 1) {
      ul_days<-rbind(ul_days, dayXwy) # add each additional year to data frame
      ul_events<-rbind(ul_events, eventXwy)
    }
  }
}

names(ul_days)<-c("YEAR", "DAYS")
names(ul_events)<-c("YEAR", "EVENTS")
ul_occ<-merge(ul_days, ul_events, by = "YEAR")
write.csv(ul_occ, file = "data/ul_occurrences.csv")

lm_day<-lm(ul_occ$DAYS~ul_occ$YEAR)
lm_event<-lm(ul_occ$EVENTS~ul_occ$YEAR)
los_day<-loess(ul_occ$DAYS~ul_occ$YEAR)
los_event<-loess(ul_occ$EVENTS~ul_occ$YEAR)

day_bp<-barplot(ul_occ$DAYS, ylim = c(0, 45), border = "red",
                names.arg = 1981:2014, las = 2, cex.names = 0.75,
                xlab = "Year", ylab = "Occurrences",
                main = "Number of Upper Level Low Days and Events")
lines(day_bp, predict(los_day), lwd = 2, lty = "dashed", col = "red")
lines(day_bp, predict(lm_day), lwd = 3, col = "red")
par(new=T)
event_bp<-barplot(ul_occ$EVENTS, ylim = c(0, 45), border = "blue",
                  las = 2, xlab = "", ylab = "", main = "")
lines(event_bp, predict(los_event), lwd = 2, lty = "dashed", col = "blue")
lines(event_bp, predict(lm_event), lwd = 3, col = "blue")
legend("topright", c("Days", "Events"), bty = "n",
       col = c("red", "blue"), pch = 15) 

plot(ul_occ$YEAR, ul_occ$DAYS, ylim = c(0, 45), type = "o", pch = 19, col = "red",
     xlab = "Year", ylab = "Occurrences", 
     main = "Occurrence of Upper Level Low Days and Events")
abline(lm_day, lwd = 3, col = "red")
par(new=T)
plot(ul_occ$YEAR, ul_occ$EVENTS, ylim = c(0, 45), type = "o", pch = 19, col = "blue",
     xlab = "", ylab = "", main = "")
abline(lm_event, lwd = 3, col = "blue")
legend("topright", c("Days", "Events"), bty = "n",
       col = c("red", "blue"), pch = 19) 