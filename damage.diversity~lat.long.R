#Paleoherbivory reveiew paper
#Purpose: Damage diveristy and percentage by lat/long
#plant diversity by lat/long
#Date started: 02.03.2021
#R version:3.6.1
#Script author: LAS

#load packages
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(agricolae)

#load data file----
dam_data <- read.csv("LAS analyses/Paleoherb_paper/allflora.csv", header=T, sep=",", 
                     na.strings=  "NA", dec=".", strip.white=TRUE)
dam_data$Flora <- NULL

#dam_data <-dam_data[-c(64:982), ] 
#for some reason lots of blank rows are being added to the bottom of 
#the data frame. removes them
#orgnize data----
total.perc <- dam_data[,c(2,6)]#percent total damage
total.div <- drop_na(dam_data[,c(2,17)]) #total diversity

#following FFG same organization as above
spec.perc <- drop_na(dam_data[,c(2,7)]) 
spec.div <- drop_na(dam_data[,c(2,18)]) 
mine.perc <- drop_na(dam_data[,c(2,8)])
mine.div <- drop_na(dam_data[,c(2,19)])
gall.perc <- drop_na(dam_data[,c(2,9)])
gall.div <- drop_na(dam_data[,c(2,20)])
HF.perc <- drop_na(dam_data[,c(2,10)])
sk.perc <- drop_na(dam_data[,c(2,12)])
surf.perc <- drop_na(dam_data[,c(2,13)])
marg.perc <- drop_na(dam_data[,c(2,11)])
P.perc <- drop_na(dam_data[,c(2,14)])
plant.div <- drop_na(dam_data[,c(2,22)])
#############################################################################
#Cleaning up data and making color palette----
#Ordering legend properly

#Bar graph Total %Dam total----
lat.tot.perc<- compare_means(perc.damage~Latitude, data = total.perc)
write.csv(lat.tot.perc, "export/~Lat/lat.tot.perc.csv")
total.perc$Latitude <- factor(total.perc$Latitude,
                              levels = c("High-S","Mid-S","Low","Mid-N","High-N"))
total.perc <- arrange(total.perc, Latitude)

total.perc.lat <- ggplot(total.perc, aes(fill=Latitude, y=perc.damage, x=Latitude)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  stat_compare_means()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col5.bar)+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Total Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
total.perc.lat

#Bar graph Total Damage Diversity----
lat.tot.div <- compare_means(stand.dt~Latitude, data = total.div)
write.csv(lat.tot.div, "export/~Lat/lat.tot.div.csv")
total.div$Latitude <- factor(total.div$Latitude,
                             levels = c("High-S","Mid-S","Low","Mid-N","High-N"))
total.div <- arrange(total.div, Latitude)

total.div.lat <- ggplot(total.div, aes(fill=Latitude, y=stand.dt, x=Latitude)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  stat_compare_means()+
  scale_fill_manual(values=c("#F5DD90","#F76C5E","#177e89","#a6808c"))+ #color palette created above 
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Total Damage \n(standarized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
total.div.lat

#Bar graph % Gall Dam----
lat.perc.gall <- compare_means(perc.gall~Latitude, data = gall.perc)
write.csv(lat.perc.gall, "export/~Lat/lat.perc.gall.csv")
gall.perc$Latitude <- factor(gall.perc$Latitude,
                             levels = c("High-S","Mid-S","Low","Mid-N","High-N"))
gall.perc <- arrange(gall.perc, Latitude)

gall.perc.lat <- ggplot(gall.perc, aes(fill=Latitude, y=perc.gall, x=Latitude)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  stat_compare_means()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col5.bar)+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Gall Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
gall.perc.lat

#Gall Damage Diversity----
compare_means(stand.gall~Latitude, data = gall.div)
gall.div$Latitude <- factor(gall.div$Latitude,
                            levels = c("High-S","Mid-S","Low","Mid-N","High-N"))
gall.div <- arrange(gall.div, Latitude)

gall.div.lat <- ggplot(gall.div, aes(fill=Latitude, y=stand.gall, x=Latitude)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  stat_compare_means()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=c("#F5DD90","#F76C5E","#177e89","#a6808c"))+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Gall Damage \n(standarized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") 
gall.div.lat

#% HF dam----
lat.perc.HF <- compare_means(perc.hole~Latitude, data = HF.perc)
write.csv(lat.perc.HF, "export/~Lat/lat.perc.HF.csv")
HF.perc$Latitude <- factor(HF.perc$Latitude,
                           levels = c("High-S","Mid-S","Low","Mid-N","High-N"))
HF.perc <- arrange(HF.perc, Latitude)

hf.perc.lat <- ggplot(gall.div, aes(fill=Latitude, y=stand.gall, x=Latitude)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  stat_compare_means()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=c("#F5DD90","#F76C5E","#177e89","#a6808c"))+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Hole Feeding Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") 
hf.perc.lat

#% specialized dam ----
lat.per.spec <- compare_means(perc.spec~Latitude, data = spec.perc)
write.csv(lat.per.spec, "export/~Lat/lat.perc.spec.csv")
spec.perc$Latitude <- factor(spec.perc$Latitude,
                             levels = c("High-S","Mid-S","Low","Mid-N","High-N"))
spec.perc <- arrange(spec.perc, Latitude)

spec.perc.lat <- ggplot(spec.perc, aes(fill=Latitude, y=perc.spec, x=Latitude)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  stat_compare_means()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=c("#F5DD90","#F76C5E","#177e89","#a6808c"))+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Specialized Feeding")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") 
spec.perc.lat

#Specialized damage diveristy
lat.div.spec <-compare_means(stand.spec~Latitude, data = spec.div)
write.csv(lat.div.spec, "export/~Lat/lat.div.spec.csv")
spec.div$Latitude <- factor(spec.div$Latitude,
                            levels = c("High-S","Mid-S","Low","Mid-N","High-N"))
spec.div <- arrange(spec.div, Latitude)

spec.div.lat <- ggplot(spec.div, aes(fill=Latitude, y=stand.spec, x=Latitude)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  stat_compare_means()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=c("#F5DD90","#F76C5E","#177e89","#a6808c"))+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Specialized Feeding")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") 
spec.div.lat

#% mine dam----
compare_means(perc.mine~Latitude, data = mine.perc)
mine.perc$Latitude <- factor(mine.perc$Latitude,
                             levels = c("High-S","Mid-S","Low","Mid-N","High-N"))
mine.perc <- arrange(mine.perc, Latitude)

mine.perc.lat <- ggplot(mine.perc, aes(fill=Latitude, y=perc.mine, x=Latitude)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  stat_compare_means()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=c("#F5DD90","#F76C5E","#177e89","#a6808c"))+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Mine Feeding")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") 
mine.perc.lat

#mine damage diversity 
compare_means(stand.mine~Latitude, data = mine.div)
mine.div$Latitude <- factor(mine.div$Latitude,
                            levels = c("High-S","Mid-S","Low","Mid-N","High-N"))
mine.div <- arrange(mine.div, Latitude)

mine.div.lat <- ggplot(mine.div, aes(fill=Latitude, y=stand.mine, x=Latitude)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  stat_compare_means()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=c("#F5DD90","#F76C5E","#177e89","#a6808c"))+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Mine Feeding \n(standardized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") 
mine.div.lat

#% skel dam----
lat.skel.perc <- compare_means(perc.skel~Latitude, data = sk.perc)
write.csv(lat.skel.perc, "export/~Lat/lat.skel.perc.csv")
sk.perc$Latitude <- factor(sk.perc$Latitude,
                           levels = c("High-S","Mid-S","Low","Mid-N","High-N"))
sk.perc <- arrange(sk.perc, Latitude)

sk.perc.lat <- ggplot(sk.perc, aes(fill=Latitude, y=perc.skel, x=Latitude)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  stat_compare_means()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=c("#F5DD90","#F76C5E","#177e89","#a6808c"))+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Skeletonization Feeding")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") 
sk.perc.lat

#% piercing damage----
lat.perc.pierce <- compare_means(perc.pierce~Latitude, data = P.perc)
write.csv(lat.perc.pierce, "export/~Lat/lat.perc.pierce.csv")
P.perc$Latitude <- factor(P.perc$Latitude,
                          levels = c("High-S","Mid-S","Low","Mid-N","High-N"))
P.perc <- arrange(P.perc, Latitude)

P.perc.lat <- ggplot(P.perc, aes(fill=Latitude, y=perc.pierce, x=Latitude)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  stat_compare_means()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=c("#F5DD90","#F76C5E","#177e89","#a6808c"))+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Piercing and \nSucking Feeding")+ #axis labels
  xlab("")+
  theme_light()+ #light ackground makes bar plot easier to read
  theme(legend.position = "none") 
P.perc.lat

#% margin damage----
lat.perc.marg <-compare_means(perc.marg~Latitude, data = marg.perc)
write.csv(lat.perc.marg, "export/~Lat/lat.perc.marg.csv")
marg.perc$Latitude <- factor(marg.perc$Latitude,
                             levels = c("High-S","Mid-S","Low","Mid-N","High-N"))
marg.perc <- arrange(marg.perc, Latitude)

marg.perc.lat <- ggplot(marg.perc, aes(fill=Latitude, y=perc.marg, x=Latitude)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  stat_compare_means()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=c("#F5DD90","#F76C5E","#177e89","#a6808c"))+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Margine Feeding")+ #axis labels
  xlab("")+
  theme_light()+ #light ackground makes bar plot easier to read
  theme(legend.position = "none") 
marg.perc.lat

#% surface damage----
lat.perc.surf <- compare_means(perc.surface~Latitude, data = surf.perc)
write.csv(lat.perc.surf, "export/~Lat/lat.perc.surf.csv")
surf.perc$Latitude <- factor(surf.perc$Latitude,
                             levels = c("High-S","Mid-S","Low","Mid-N","High-N"))
surf.perc <- arrange(surf.perc, Latitude)

surf.perc.lat <- ggplot(surf.perc, aes(fill=Latitude, y=perc.surface, x=Latitude)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=c("#F5DD90","#F76C5E","#177e89","#a6808c"))+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Surface Feeding")+ #axis labels
  xlab("")+
  theme_light()+ #light ackground makes bar plot easier to read
  theme(legend.position = "none") 
surf.perc.lat

#Plant diveristy
compare_means(stand.plant.div~Latitude, data = plant.div)

plant.div$Latitude <- factor(plant.div$Latitude,
                             levels = c("High-S","Mid-S","Low","Mid-N","High-N"))
plant.div <- arrange(plant.div, Latitude)

plant.div.lat <- ggplot(plant.div, aes(fill=Latitude, y=stand.plant.div, x=Latitude)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=c("#F5DD90","#F76C5E","#177e89","#a6808c"))+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Plant Diversity \n(standarized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light ackground makes bar plot easier to read
  theme(legend.position = "none") 
plant.div.lat

#putting plots together into superplot----
super2.lat <- plot_grid(total.div.lat, total.perc.lat,
                        spec.div.lat, spec.perc.lat,
                        gall.perc.lat,surf.perc.lat,
                        sk.perc.lat,P.perc.lat,
                        marg.perc.lat,
                        nrow = 5,
                        ncol=2)
super2.lat

#saving figures----
ggsave("LAS analyses/Paleoherb_paper/figures/plant.div.lat.pdf", plant.div.lat)
ggsave("LAS analyses/Paleoherb_paper/figures/pierce.perc.lat.pdf",P.perc.lat)
ggsave("LAS analyses/Paleoherb_paper/figures/skel.perc.lat.pdf",sk.perc.lat)
ggsave("LAS analyses/Paleoherb_paper/figures/mine.div.lat.pdf",mine.div.lat)
ggsave("LAS analyses/Paleoherb_paper/figures/mine.perc.lat.pdf",mine.perc.lat)
ggsave("LAS analyses/Paleoherb_paper/figures/spec.divlat.pdf",spec.div.lat)
ggsave("LAS analyses/Paleoherb_paper/figures/spec.perc.lat.pdf",spec.perc.lat)
ggsave("LAS analyses/Paleoherb_paper/figures/hf.perc.lat.pdf",hf.perc.lat)
ggsave("LAS analyses/Paleoherb_paper/figures/gall.div.lat.pdf",gall.div.lat)
ggsave("LAS analyses/Paleoherb_paper/figures/gall.perc.lat.pdf",gall.perc.lat)
ggsave("LAS analyses/Paleoherb_paper/figures/total.perc.lat.pdf",total.perc.lat)
ggsave("LAS analyses/Paleoherb_paper/figures/total.div.lat.pdf",total.div.lat)
ggsave("LAS analyses/Paleoherb_paper/figures/totspecgallsuper2.lat.pdf",super2.lat)
