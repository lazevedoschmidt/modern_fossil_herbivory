#Paleoherbivory vs. modern paper
#Purpose: Damage diveristy and percentage by epoch
#plant diversity by epoch
#Date started: 02.12.2021
#R version:3.6.1
#Script author: LAS

#load packages
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(agricolae) #You need this for the Tukey test

#load data file----
dam_data <- read.csv("Finalized.Database.forests.csv", header=T, sep=",", 
                     na.strings=  "NA", dec=".", strip.white=TRUE)
#dam_data$Flora <- NULL
#dam_data <-dam_data[-c(76:96), ] 
#for some reason lots of blank rows are being added to the bottom of 
#the data frame. removes them
#orgnize data----
total.perc <- drop_na(dam_data[,c(2,13)])#percent total damage
total.div <- drop_na(dam_data[,c(2,24)]) #total diversity
#following FFG same organization as above
spec.perc <- drop_na(dam_data[,c(2,14)]) 
spec.div <- drop_na(dam_data[,c(2,25)]) 
mine.perc <- drop(dam_data[,c(2,15)])
mine.div <- drop(dam_data[,c(2,26)])
gall.perc <- drop_na(dam_data[,c(2,16)])
gall.div <- drop(dam_data[,c(2,27)])
plant.div <- drop_na(dam_data[,c(2,30)])

#means----
#Total percent damage
avgtotal.perc <- total.perc %>%
  drop_na()%>% #dropping NAs at bottom of .csv
  group_by(Epoch)%>% #grouping by epoch
  summarise(mean.all= mean(perc.dam), #average
            sd.all = sd(perc.dam), #standard deviation
            count=n())
#Total damage diversity 
avgtotal.div <- total.div %>%
  drop_na()%>% #dropping NAs at bottom of .csv
  group_by(Epoch)%>% #grouping by epoch
  summarise(mean.all= mean(div.dt), #average
            sd.all = sd(div.dt), #standard deviation
            count=n())

#percent specialized damage
avgspec.perc <- spec.perc %>%
  drop_na(perc.spec)%>%
  group_by(Epoch)%>%
  summarise(mean.spec = mean(perc.spec),
            sd.spec = sd(perc.spec),
            count=n()) #number of samples/sites per epoch)
#specialized damage diversity 
avgspec.div <- spec.div %>%
  drop_na(div.spec)%>%
  group_by(Epoch)%>%
  summarise(mean.spec = mean(div.spec),
            sd.spec = sd(div.spec),
            count=n()) #number of samples/sites per epoch)

#percent mine damage
avgmine.perc <- mine.perc %>%
  drop_na(perc.mine)%>%
  group_by(Epoch)%>%
  summarise(mean.mine = mean(perc.mine),
            sd.mine = sd(perc.mine),
            count=n()) #number of samples/sites per epoch)
#mine damage diversity 
avgmine.div <- mine.div %>%
  drop_na(div.mine)%>%
  group_by(Epoch)%>%
  summarise(mean.mine = mean(div.mine),
            sd.mine = sd(div.mine),
            count=n()) #number of samples/sites per epoch)

#percent gall damage
avggall.perc <-gall.perc %>%
  drop_na(perc.gall)%>%
  group_by(Epoch)%>%
  summarise(mean.gall = mean(perc.gall),
            sd.gall = sd(perc.gall),
            count=n()) #number of samples/sites per epoch)
#gall damage diversity 
avggall.div <-gall.div %>%
  drop_na(div.gall)%>%
  group_by(Epoch)%>%
  summarise(mean.gall = mean(div.gall),
            sd.gall = sd(div.gall),
            count=n()) #number of samples/sites per epoch)

#plant diveristy 
avgplant.div <- plant.div %>%
  drop_na()%>%
  group_by(Epoch)%>%
  summarise(mean.plant = mean(div.plant),
            sd.plant = sd(div.plant),
            count=n())
#----
#boxplots
col8 <- c("#c89f9c","#E07A5F","#8F5D5D", "#3D405B","#5F797B","#81B29A","#F2CC8F","#e76f51")

#Tukey tests----
#Tukey's test for significance across time
#Asking does Xdamage change across time? 
#total percent damage tukey test----
total.lm <- lm(perc.dam ~ Epoch, data = dam_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'Epoch')
total_test

#Total percent damage
tot.perc.data <-compare_means(perc.dam~Epoch, data = total.perc)
write.csv(tot.perc.data, "export/tot.perc.data.csv")
#write.csv(tot.perc.data, "export/tot.perc.data.csv")
total.perc$Epoch <- factor(total.perc$Epoch, 
                              levels = c("Cretaceous", "Paleocene", "Eocene",
                                         "Oligocene","Miocene","Pliocene",
                                         "Pleistocene","Anthropocene"))
total.perc <- arrange(total.perc, Epoch)

box.total.perc <- ggplot(total.perc, aes(fill=Epoch, y=perc.dam, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avgtotal.perc), 
            aes(label = c("b","a","a","a","a","a","a","a"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  ylab("% Total Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.total.perc

#total damage diversity tukey test----
totaldiv.lm <- lm(div.dt ~ Epoch, data = dam_data)
totaldiv.av <- aov(totaldiv.lm)
summary(totaldiv.av)
totaldiv_test <- HSD.test(totaldiv.av, 'Epoch')
totaldiv_test

#Total diversity box plot
tot.div.data <- compare_means(div.dt~Epoch, data = total.div)
write.csv(tot.div.data, "export/tot.div.data.csv")

total.div$Epoch <- factor(total.div$Epoch, 
                           levels = c("Cretaceous", "Paleocene", "Eocene",
                                      "Oligocene","Miocene","Pliocene",
                                      "Pleistocene", "Anthropocene"))
total.div <- arrange(total.div, Epoch)

box.total.div <- ggplot(total.div, aes(fill=Epoch, y=div.dt, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  #stat_compare_means(comparisons = my_comparisons)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above
  geom_text(data= data.frame(avgtotal.div), 
            aes(label = c("a","a","a","a","a","a","a","a"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Total Damage \n(standarized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.total.div

#gall diversity tukey test----
galldiv.lm <- lm(div.gall ~ Epoch, data = dam_data)
galldiv.av <- aov(galldiv.lm)
summary(galldiv.av)
galldiv_test <- HSD.test(galldiv.av, trt = 'Epoch')
galldiv_test

#Gall diversity box plot
gall.div.data <- compare_means(div.gall~Epoch, data = gall.div)
write.csv(gall.div.data, "export/gall.div.data.csv")

gall.div$Epoch <- factor(gall.div$Epoch, 
                          levels = c("Cretaceous", "Paleocene", "Eocene",
                                     "Oligocene","Miocene","Pliocene",
                                     "Pleistocene","Anthropocene"))
gall.div <- arrange(gall.div, Epoch)

box.gall.div <- ggplot(gall.div, aes(fill=Epoch, y=div.gall, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above 
  geom_text(data= data.frame(avggall.div), 
            aes(label = c("b","ab","ab","a","ab","b","ab","ab"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Total Gall Damage \n(standarized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.gall.div 

#percent gall damage tukey test----
gall.lm <- lm(perc.gall ~ Epoch, data = dam_data)
gall.av <- aov(gall.lm)
summary(gall.av)
gall_test <- HSD.test(gall.av, trt = 'Epoch')
gall_test

#%Gall damage box plot
gall.perc.data <- compare_means(perc.gall~Epoch, data = gall.perc)
write.csv(gall.perc.data, "export/gall.perc.data.csv")

gall.perc$Epoch <- factor(gall.perc$Epoch, 
                         levels = c("Cretaceous", "Paleocene", "Eocene",
                                    "Oligocene","Miocene","Pliocene",
                                    "Pleistocene","Anthropocene"))
gall.perc <- arrange(gall.perc, Epoch)

box.gall.perc <- ggplot(gall.perc, aes(fill=Epoch, y=perc.gall, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above
  geom_text(data= data.frame(avggall.perc), 
            aes(label = c("a","a","a","a","a","a","a","a"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Gall Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.gall.perc


#mine diversity tukey tes----
minediv.lm <- lm(div.mine ~ Epoch, data = dam_data)
minediv.av <- aov(minediv.lm)
summary(minediv.av)
minediv_test <- HSD.test(minediv.av, trt = 'Epoch')
minediv_test
#Mine diversity box plot
mine.div.data <- compare_means(div.mine~Epoch, data = mine.div)
write.csv(mine.div.data, "export/mine.div.data.csv")

mine.div$Epoch <- factor(mine.div$Epoch, 
                         levels = c("Cretaceous", "Paleocene", "Eocene",
                                    "Oligocene","Miocene","Pliocene",
                                    "Pleistocene","Anthropocene"))
mine.div <- arrange(mine.div, Epoch)

box.mine.div <- ggplot(mine.div, aes(fill=Epoch, y=div.mine, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above 
  geom_text(data= data.frame(avgmine.div), 
            aes(label = c("a","a","a","a","a","a","a","b"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Total Mine Damage \n(standarized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.mine.div


#percent mine damage tukey test----
mine.lm <- lm(perc.mine ~ Epoch, data = dam_data)
mine.av <- aov(mine.lm)
summary(mine.av)
mine_test <- HSD.test(mine.av, trt = 'Epoch')
mine_test

#Mine percent box plot
mine.perc.data <- compare_means(perc.mine~Epoch, data = mine.perc)
write.csv(mine.perc.data, "export/mine.perc.data.csv")

mine.perc$Epoch <- factor(mine.perc$Epoch, 
                         levels = c("Cretaceous", "Paleocene", "Eocene",
                                    "Oligocene","Miocene","Pliocene",
                                    "Pleistocene","Anthropocene"))
mine.perc <- arrange(mine.perc, Epoch)

box.mine.perc <- ggplot(mine.perc, aes(fill=Epoch, y=perc.mine, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above 
  geom_text(data= data.frame(avgmine.perc), 
            aes(label = c("a","ab","b","b","b","b","ab","b"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% of Mine Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.mine.perc

#percent specilized damage tukey test----
spec.lm <- lm(perc.spec ~ Epoch, data = dam_data)
spec.av <- aov(spec.lm)
summary(spec.av)
spec_test <- HSD.test(spec.av, trt = 'Epoch')
spec_test

#%spec damage box plot
perc.spec.data <- compare_means(perc.spec~Epoch, data = spec.perc)
write.csv(perc.spec.data, "export/perc.spec.data.csv")

spec.perc$Epoch <- factor(spec.perc$Epoch, 
                          levels = c("Cretaceous", "Paleocene", "Eocene",
                                     "Oligocene","Miocene","Pliocene",
                                     "Pleistocene","Anthropocene"))
spec.perc <- arrange(spec.perc, Epoch)

box.spec.perc <- ggplot(spec.perc, aes(fill=Epoch, y=perc.spec, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above 
  geom_text(data= data.frame(avgspec.perc), 
            aes(label = c("a","ab","b","b","b","b","ab","ab"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Specialized Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.spec.perc

#specialized damage diversity tukey test----
specdiv.lm <- lm(div.spec ~ Epoch, data = dam_data)
specdiv.av <- aov(specdiv.lm)
summary(specdiv.av)
specdiv_test <- HSD.test(specdiv.av, trt = 'Epoch')
specdiv_test

#Spec diversity box plot
spec.div.data <- compare_means(div.spec~Epoch, data = spec.div)
write.csv(spec.div.data, "export/spec.div.data.csv")

spec.div$Epoch <- factor(spec.div$Epoch, 
                         levels = c("Cretaceous", "Paleocene", "Eocene",
                                    "Oligocene","Miocene","Pliocene",
                                    "Pleistocene","Anthropocene"))
spec.div <- arrange(spec.div, Epoch)

box.spec.div <- ggplot(spec.div, aes(fill=Epoch, y=div.spec, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above
  geom_text(data= data.frame(avgspec.div), 
            aes(label = c("a","a","a","a","a","a","a","a"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Total Specialized Damage \n(standarized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.spec.div

#plant diversity tukey test----
plant.lm <- lm(div.plant ~ Epoch, data= dam_data)
plant.av <- aov(plant.lm)
summary(plant.av)
plant_test <- HSD.test(plant.av, trt="Epoch")
plant_test

#Plant diversity box plot
plant.div.data <- compare_means(div.plant~Epoch, data = plant.div)
write.csv(plant.div.data, "export/plant.div.data.csv")

plant.div$Epoch <- factor(plant.div$Epoch, 
                         levels = c("Cretaceous", "Paleocene", "Eocene",
                                    "Oligocene","Miocene","Pliocene",
                                    "Pleistocene", "Anthropocene"))
plant.div <- arrange(plant.div, Epoch)

box.plant.div <- ggplot(plant.div, aes(fill=Epoch, y=div.plant, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above
  geom_text(data= data.frame(avgplant.div), 
            aes(label = c("a","a","a","a","a","a","a","a"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Plant Diversity \n(standarized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.plant.div

#Super plot
#putting plots together into superplot----
total.super <- plot_grid(box.total.div,box.total.perc,
                    nrow = 1,
                    ncol=2)
total.super

gall.super <- plot_grid(box.gall.div,box.gall.perc,
                        nrow=1,
                        ncol=2)
gall.super

mine.super <- plot_grid(box.mine.div, box.mine.perc,
                        nrow=1,
                        ncol=2)
mine.super

spec.super <- plot_grid(box.spec.div, box.spec.perc,
                        nrow = 1,
                        ncol = 2)
spec.super

sig.super <- plot_grid(box.spec.perc,box.mine.perc,
                       box.total.perc,box.mine.div,
                       box.gall.div,
                        nrow = 3,
                        ncol = 2)
sig.super

#Saving figures----
ggsave("figures/box.plant.div.pdf", box.plant.div)
ggsave("figures/box.mine.div.pdf",box.mine.div)
ggsave("figures/box.mine.perc.pdf",box.mine.perc)
ggsave("figures/box.spec.div.pdf",box.spec.div)
ggsave("figures/box.spec.perc.pdf",box.spec.perc)
ggsave("figures/box.gall.div.pdf",box.gall.div)
ggsave("figures/box.gall.perc.pdf",box.gall.perc)
ggsave("figures/box.total.perc.pdf",box.total.perc)
ggsave("figures/box.total.div.pdf",box.total.div)
ggsave("figures/box.total.super.pdf", total.super)
ggsave("figures/box.gall.super.pdf", gall.super)
ggsave("figures/box.mine.super.pdf", mine.super)
ggsave("figures/box.spec.super.pdf", spec.super)
ggsave("figures/box.sig.super.pdf", sig.super)


