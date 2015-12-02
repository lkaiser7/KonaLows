
# low occurrences
low_occ<-read.csv("data/kl_occurrences.csv", header = TRUE)
names(low_occ)

low_occ<-merge(ul_occ, kl_occ, by = "YEAR")
names(low_occ)<-c("YEAR", "UL", "UL_DUR", "KL", "KL_DUR")

# ull lines
lm_uday<-lm(low_occ$UL~low_occ$YEAR)
lm_uevent<-lm(low_occ$UL_DUR~low_occ$YEAR)
los_uday<-loess(low_occ$UL~low_occ$YEAR)
los_uevent<-loess(low_occ$UL_DUR~low_occ$YEAR)
# kl lines
lm_kday<-lm(low_occ$KL~low_occ$YEAR)
lm_kevent<-lm(low_occ$KL_DUR~low_occ$YEAR)
los_kday<-loess(low_occ$KL~low_occ$YEAR)
los_kevent<-loess(low_occ$KL_DUR~low_occ$YEAR)

# day plots
uday_bp<-barplot(low_occ$UL, ylim = c(0, 50), border = "red",
                names.arg = 1981:2014, 
                las = 2, cex.names = 0.75,
                xlab = "Year", ylab = "Occurrences",
                main = "Number of Low Days")
par(new=T)
kday_bp<-barplot(low_occ$KL, ylim = c(0, 50), border = "blue",
                 las = 2, xlab = "", ylab = "", main = "")
lines(uday_bp, predict(los_uday), lwd = 2, lty = "dashed", col = "red")
lines(uday_bp, predict(lm_uday), lwd = 3, col = "red")
lines(kday_bp, predict(los_kday), lwd = 2, lty = "dashed", col = "blue")
lines(kday_bp, predict(lm_kday), lwd = 3, col = "blue")
legend("topleft", c("Upper Level Lows", "Kona Lows"), bty = "n",
       col = c("red", "blue"), pch = 15) 

# event plots
uevent_bp<-barplot(low_occ$UL_DUR, ylim = c(0, 15), border = "red",
                 names.arg = 1981:2014, las = 2, cex.names = 0.75,
                 xlab = "Year", ylab = "Occurrences",
                 main = "Number of Low Events")
par(new=T)
kevent_bp<-barplot(low_occ$KL_DUR, ylim = c(0, 15), border = "blue",
                 las = 2, xlab = "", ylab = "", main = "")
#lines(uevent_bp, predict(los_uevent), lwd = 2, lty = "dashed", col = "red")
lines(uevent_bp, predict(lm_uevent), lwd = 1, col = "red")
#lines(kevent_bp, predict(los_kevent), lwd = 2, lty = "dashed", col = "blue")
lines(kevent_bp, predict(lm_kevent), lwd = 1, col = "blue")
legend("topleft", c("Upper Level Lows", "Kona Lows"), bty = "n",
       col = c("red", "blue"), pch = 15) 
