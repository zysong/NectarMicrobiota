library(reshape2)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggExtra)
source("summarySE.R")
#Changing the variation of flowering time

data1<-read.csv("BYsum.csv", head=TRUE)
data2<-read.csv("NoYsum.csv", head=TRUE)
data3<-read.csv("NoBsum.csv", head=TRUE)
data4<-read.csv("NoMsum.csv", head=TRUE)
tab0<-rbind(data1, data2, data3, data4)
Microbe<-c(rep(3, nrow(tab0)/4), rep(2, nrow(tab0)/4), rep(1, nrow(tab0)/4), rep(0, nrow(tab0)/4))
data<-cbind(tab0, Microbe)
data.long<-gather(data, condition, count, NSum:PlantFitSum)
data.long$count<-data.long$count/50000

summary0<-summarySE(data.long, measurevar="count", groupvars=c("condition", "meanDay", "sdDay", "Microbe"))
summary0$meanDay<-factor(summary0$meanDay)
summary0$sdDay<-factor(summary0$sdDay)
summary0$Microbe<-factor(summary0$Microbe)

summary1<-subset(summary0, sdDay==15)
summary2<-subset(summary0, meanDay==0)

pol1<-ggplot(subset(summary1, condition=="PlantFitSum"), 
             aes(x=meanDay, y=count*2, color=Microbe, group=Microbe, ymax=1, ymin=0.4)) + 
  geom_line(position=position_dodge(.3), size=1.5, linetype = 2)+
  geom_point(position=position_dodge(0), size=2, shape=21, fill="white")+
  geom_errorbar(aes(ymin=2*(count-sd), ymax=2*(count+sd)),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(0)) +
  xlab("")+
  ylab("Pollination") +
  ggtitle("a")+
  scale_color_manual(name="Microbial control", 
                     breaks=c("0", "1", "2", "3"),
                     labels=c("No microbe", "Yeast only", "Bacteria only", "Both included"),
                     values=c("red", "blue", "green", "black"), guide=FALSE)+
  theme_bw(base_size = 18)+
  theme(plot.title = element_text(hjust = 0))+
  removeGrid()

pol2<-ggplot(subset(summary2, condition=="PlantFitSum"), aes(x=sdDay, y=count*2, color=Microbe, group=Microbe, ymax=1, ymin=0.4)) + 
  geom_line(position=position_dodge(.3), size=1.5, linetype = 2)+
  geom_point(position=position_dodge(0), size=2, shape=21, fill="white")+
  geom_errorbar(aes(ymin=2*(count-sd), ymax=2*(count+sd)),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(0)) +
  xlab("") +
  ylab("") +
  ggtitle("b")+
  scale_color_manual(name="Microbial control", 
                     breaks=c("0", "1", "2", "3"),
                     labels=c("No microbe", "Yeast only", "Bacteria only", "Both included"),
                     values=c("red", "blue", "green", "black"), guide=FALSE)+
  theme_bw(base_size = 18)+
  theme(plot.title = element_text(hjust = 0))+
  removeGrid()

bf1<-ggplot(subset(summary1, condition=="BSum"), aes(x=meanDay, y=count, color=Microbe, group=Microbe, ymax=.5)) + 
  geom_line(position=position_dodge(.3), size=1.5, linetype = 2)+
  geom_point(position=position_dodge(0), size=2, shape=21, fill="white")+
  geom_errorbar(aes(ymin=count-sd, ymax=count+sd),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(0)) +
  ylim(c(0, 0.6))+
  xlab("") +
  ylab("Bacteria-dominated flowers") +
  ggtitle("c")+ 
  scale_color_manual(name="Microbial control", 
                     breaks=c("0", "1", "2", "3"),
                     labels=c("No microbe", "Yeast only", "Bacteria only", "Both included"),
                     values=c("red", "blue", "green", "black"), guide=FALSE)+
  theme_bw(base_size = 18)+
  theme(plot.title = element_text(hjust = 0))+
  removeGrid()

bf2<-ggplot(subset(summary2, condition=="BSum"), aes(x=sdDay, y=count, color=Microbe, group=Microbe, ymax=.5)) + 
  geom_line(position=position_dodge(.3), size=1.5, linetype = 2)+
  geom_point(position=position_dodge(0), size=2, shape=21, fill="white")+
  geom_errorbar(aes(ymin=count-sd, ymax=count+sd),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(0)) +
  ylim(c(0, 0.6))+
  xlab("") +
  ylab("") +
  ggtitle("d")+
  scale_color_manual(name="Microbial control", 
                     breaks=c("0", "1", "2", "3"),
                     labels=c("No microbe", "Yeast only", "Bacteria only", "Both included"),
                     values=c("red", "blue", "green", "black"), guide=FALSE)+
  theme_bw(base_size = 18)+
  theme(plot.title = element_text(hjust = 0))+
  removeGrid()

yf1<-ggplot(subset(summary1, condition=="YSum"), aes(x=meanDay, y=count, color=Microbe, group=Microbe, ymax=.5)) + 
  geom_line(position=position_dodge(.3), size=1.5, linetype = 2)+
  geom_point(position=position_dodge(0), size=2, shape=21, fill="white")+
  geom_errorbar(aes(ymin=count-sd, ymax=count+sd),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(0)) +
  xlab("Peak of flowering time (day)") +
  ylab("Yeast-dominated flowers") +
  ggtitle("e")+
  scale_color_manual(name="Microbial control", 
                     breaks=c("0", "1", "2", "3"),
                     labels=c("No microbe", "Yeast only", "Bacteria only", "Both included"),
                     values=c("red", "blue", "green", "black"), guide=FALSE)+
  theme_bw(base_size = 18)+
  theme(plot.title = element_text(hjust = 0))+
  removeGrid()

yf2<-ggplot(subset(summary2, condition=="YSum"), aes(x=sdDay, y=count, color=Microbe, group=Microbe, ymax=.5)) + 
  geom_line(position=position_dodge(.3), size=1.5, linetype = 2)+
  geom_point(position=position_dodge(0), size=2, shape=21, fill="white")+
  geom_errorbar(aes(ymin=count-sd, ymax=count+sd),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(0)) +
  xlab("SD of flowering time (day)") +
  ylab("") +
  ggtitle("f")+
  scale_color_manual(name="Scenarios", 
                     breaks=c("0", "1", "2", "3"),
                     labels=c("No microbe", "Yeast only", "Bacteria only", "Both included"),
                     values=c("red", "blue", "green", "black"))+
  theme_bw(base_size = 18)+
  theme(plot.title = element_text(hjust = 0))+
  removeGrid()

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend<-get_legend(yf2)

yf2 <- yf2 + theme(legend.position="none")

ggsave("figures/scenarios.pdf", grid.arrange(pol1, pol2, bf1, bf2, yf1, yf2, legend, ncol =3, nrow =3, 
             layout_matrix = cbind(c(1,3,5), c(2,4,6), c(7,7,7)), heights = c(2.5, 2.5, 2.5), widths=c(2.3, 2.3, 1)),
       height = 12.5, width = 12.5)
