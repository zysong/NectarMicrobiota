#fixed pollinator curve, instead of window
library(msm)
mu<-.2 #flower closing rate
season<--75:75 #span of a flowering season
a.b0<-.3 #pol-independent transmission rate of bacteria
a.b1<-.4 #pol-dependent transmission rate of bacteria
a.y1<-1.5 #pol-dependent transmission rate of yeast
w.b<-.5 #dispersal limit of bacteria
w.y<-.5 #dispersal limit of yeast
e.b<-.02 #migration rate of bacteria
e.y1<-.005 #migration rate of yeast
mean.day<-0
sd.day<-15
mean.day.pol<-5
sd.day.pol<-15
R.total<-10000 #total new flowers
P.total<-500 #total pollinator*days
run<-100
r1<-.01 #pollinator-flower ratio
r0<-.01 #half-saturated pollinator-flower ratio
p.n<-1
p.y<-1
p.b<-.2
n.segment<-10 #number of knockout segments
quantile<-round(qtnorm((0:n.segment)/n.segment, mean=mean.day.pol, sd=sd.day.pol, lower=season[1], upper=tail(season, 1)))
df.knockout<-data.frame()

for (bin in 1:n.segment) {
  R<-round(R.total*dtnorm(season, mean=mean.day, sd=sd.day, lower=season[1], upper=tail(season, 1)))
  P<-P.total*dtnorm(season, mean=mean.day.pol, sd=sd.day.pol, lower=season[1], upper=tail(season, 1))
  P[quantile[bin]:quantile[bin+1]-season[1]+1]<-0 #knock out pollinators in the segment
  N.mat<-B.mat<-Y.mat<-F.mat<-fit.plant.mat<-matrix(rep(0,run*length(season)), ncol=run)
  for (seed in 1:run) {
    set.seed(seed)
    N<-B<-Y<-F<-rep(0,length(season))
    fit.plant<-rep(0,length(season))
    for(t in 1:(length(season)-1)) {
      if (P[t]!=0){
        S<-P[t]/(P[t]+r0*F[t]) #pollination saturation
      } else {
        S<-0
      }
      
      a.y<-a.y1*S
      a.b<-a.b0 + a.b1*S
      e.y<-e.y1*S
      fit.plant[t]<-(p.n*N[t] + p.y*Y[t]+p.b*B[t])*S
      
      mig.b<-rbinom(1, N[t], e.b)
      mig.y<-rbinom(1, N[t], e.y)
      death.n<-rbinom(1, N[t], mu)
      death.b<-rbinom(1, B[t], mu)
      death.y<-rbinom(1, Y[t], mu)
      if (F[t]!=0){
        f.b<-B[t]/F[t]
        f.y<-Y[t]/F[t]
      } else {
        f.b<-f.y<-0
      }
      trans.b<-rbinom(1, N[t], a.b * f.b/(1 + w.b * f.b))
      trans.y<-rbinom(1, N[t], a.y * f.y/(1 + w.y * f.y))
      
      N[t+1]<-max(0, N[t] + R[t] - death.n - trans.b - trans.y - mig.b - mig.y)
      B[t+1]<-max(0, B[t] - death.b + trans.b + mig.b)
      Y[t+1]<-max(0, Y[t] - death.y + trans.y + mig.y)
      F[t+1]<-N[t+1] + B[t+1] + Y[t+1]
    }
    N.mat[, seed]<-N
    B.mat[,seed]<-B
    Y.mat[,seed]<-Y
    F.mat[,seed]<-F
    fit.plant.mat[,seed]<-fit.plant
  }
  
  result<-data.frame(Knockout=bin,
                     NSum=colSums(N.mat)*mu/R.total,
                     BSum=colSums(B.mat)*mu/R.total,
                     YSum=colSums(Y.mat)*mu/R.total,
                     PolSum=colSums(fit.plant.mat)*2*mu/R.total)
  df.knockout<-rbind(df.knockout, result)
}

#save the results
write.csv(df.knockout, file="PolKnockout.csv", row.names = FALSE)

df.knockout<-read.csv("PolKnockout.csv", head=TRUE)
attach(df.knockout)
lm.knockout<-lm(PolSum~Knockout)
summary(lm.knockout)
pdf("PolKnockout.pdf")
plot(Knockout, PolSum, cex.lab=1.5, xlab = "Window of pollinator knockout", ylab = "Pollination")
points(Knockout, predict(lm.knockout), type="l")
dev.off()
detach(df.knockout)
