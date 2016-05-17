df.knockout<-read.csv("PolKnockout.csv", head=TRUE)
df.LateArrival<-read.csv("PolLateArrival.csv", head=TRUE)
attach(df.knockout)
lm.knockout<-lm(PolSum~Knockout)
summary(lm.knockout)
detach(df.knockout)

attach(df.LateArrival)
lm.arrival<-lm(PolSum~Arrival)
summary(lm.arrival)
lm01<-lm(PolSum[Arrival==0 | Arrival==10]~Arrival[Arrival==0 | Arrival==10])
PolDev<-PolSum-coef(lm01)[1]-Arrival*coef(lm01)[2]
detach(df.LateArrival)

pdf("Knock_Delay.pdf", height=3.5, width = 7)
par(mfrow=c(1,2), oma = c(2, 2, 0, 0), mar = c(3, 3, 3, 1),mgp = c(2, 1, 0),xpd=FALSE)
#graph 1
with(data=df.knockout, plot(Knockout, PolSum, cex=.5, cex.lab=.8,
                            xlab = "Window of pollinator knockout", ylab = "Pollination"))
with(data=df.knockout, points(Knockout, predict(lm.knockout), type="l"))
title(main="a", adj=0)
#graph 2
boxplot(PolDev~Arrival, data=df.LateArrival, cex=.5, cex.lab=.8,  xlab = "Delay of pollinator arrival", ylab = "Deviation from proportional pollination loss")
abline(h=0, lty=2)
title(main="b", adj=0)
dev.off()

