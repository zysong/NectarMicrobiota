library(lattice)
ts1<-read.csv("timeseries_sd10early.csv", head=TRUE)
ts2<-read.csv("timeseries_sd10.csv", head=TRUE)
ts3<-read.csv("timeseries_sd10late.csv", head=TRUE)
ts4<-read.csv("timeseries_sd15early.csv", head=TRUE)
ts5<-read.csv("timeseries_sd15.csv", head=TRUE)
ts6<-read.csv("timeseries_sd15late.csv", head=TRUE)
ts7<-read.csv("timeseries_sd20early.csv", head=TRUE)
ts8<-read.csv("timeseries_sd20.csv", head=TRUE)
ts9<-read.csv("timeseries_sd20late.csv", head=TRUE)

data0<-rbind(ts1, ts2, ts3, ts4, ts5, ts6, ts7, ts8, ts9)

tsplot<-xyplot(NF + BF + YF + TF + Pol ~ Date | meanDay * sdDay,
       ylab="Floral (pollinator) density", xlab="Time (Day)", 
       data = data0, type = "l", lty = c(1,1,1,2,2), lwd=2, 
       col = c("red", "green", "blue", "black", "gray"),
       key = list(text=list(c("Flowers with no microbe", "Bacteria-dominated flowers", "Yeast-dominated flowers", "Total flowers", "Pollinators")), 
                  lines = list(lwd=2, col = c("red", "green", "blue", "black", "gray"), lty = c(1,1,1,2,2)), 
                  space = "top", columns = 2)
)

pdf("seasonality_new.pdf")
tsplot
dev.off()
