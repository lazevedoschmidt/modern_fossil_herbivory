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
library(EnvStats) #rosnerTest

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

hf.perc <- drop_na(dam_data[,c(2,17)])
mf.perc <- drop_na(dam_data[,c(2,18)])
skel.perc <- drop_na(dam_data[,c(2,19)])
sf.perc <- drop_na(dam_data[,c(2,20)])
pierce.perc <- drop_na(dam_data[,c(2,21)])
shannon <- drop_na(dam_data[,c(2,28)])
pj <- drop_na(dam_data[,c(2,29)])

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

#%hole feeding
avgphf.perc <- hf.perc %>%
  drop_na()%>%
  group_by(Epoch)%>%
  summarise(mean.plant = mean(perc.HF),
            sd.plant = sd(perc.HF),
            count=n())

#%margin feeding
avgpmf.perc <- mf.perc %>%
  drop_na()%>%
  group_by(Epoch)%>%
  summarise(mean.plant = mean(perc.mf),
            sd.plant = sd(perc.mf),
            count=n())
#%skeletonization feeding
avgpskel.perc <- skel.perc %>%
  drop_na()%>%
  group_by(Epoch)%>%
  summarise(mean.plant = mean(perc.skel),
            sd.plant = sd(perc.skel),
            count=n())
#%surface feeding
avgpsf.perc <- sf.perc %>%
  drop_na()%>%
  group_by(Epoch)%>%
  summarise(mean.plant = mean(perc.sf),
            sd.plant = sd(perc.sf),
            count=n())
#%piercing and sucking
avgppierce.perc <- pierce.perc %>%
  drop_na()%>%
  group_by(Epoch)%>%
  summarise(mean.plant = mean(perc.pierce),
            sd.plant = sd(perc.pierce),
            count=n())

#shannon diversity
avgshannon <- shannon %>%
  drop_na()%>%
  group_by(Epoch)%>%
  summarise(mean.plant = mean(Shannon),
            sd.plant = sd(Shannon),
            count=n())
avgpj <- pj %>%
  drop_na()%>%
  group_by(Epoch)%>%
  summarise(mean.plant = mean(Pj),
            sd.plant = sd(Pj),
            count=n())
#----
#boxplots
col8 <- c("#c89f9c","#E07A5F","#8F5D5D", "#3D405B","#5F797B","#81B29A","#e76f51",
          "#F2CC8F")
#Cretaceous, Paleocene, Eocene, Oligocene, Miocene, Pliocene,Pleist, Anthro
#Tukey tests----
#Tukey's test for significance across time
#Asking does Xdamage change across time? 

#total percent damage tukey test----
total.lm <- lm(perc.dam ~ Epoch, data = total.perc)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'Epoch')
total_test
TukeyHSD(total.av)
#checking outliers:
ros.totalperc <- rosnerTest(total.perc$perc.dam,
                           k = 10)
ros.totalperc

#Total percent damage
total.perc$Epoch <- factor(total.perc$Epoch, 
                              levels = c("Cretaceous", "Paleocene", "Eocene",
                                         "Oligocene","Miocene","Pliocene",
                                         "Pleistocene","Anthropocene"))
total.perc <- arrange(total.perc, Epoch)

avgtotal.perc$Epoch <- factor(avgtotal.perc$Epoch, 
                           levels = c("Cretaceous", "Paleocene", "Eocene",
                                      "Oligocene","Miocene","Pliocene",
                                      "Pleistocene","Anthropocene"))
avgtotal.perc <- arrange(avgtotal.perc, Epoch)

box.total.perc <- ggplot(total.perc, aes(fill=Epoch, y=perc.dam, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avgtotal.perc), #lumped modern
            aes(label = c("a","a","a","a","a","a","a","b"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  ylab("% Total Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.total.perc

#total damage diversity tukey test----
totaldiv.lm <- lm(div.dt ~ Epoch, data = total.div)
totaldiv.av <- aov(totaldiv.lm)
summary(totaldiv.av)
totaldiv_test <- HSD.test(totaldiv.av, 'Epoch')
totaldiv_test

ros.totaldiv <- rosnerTest(total.div$div.dt,
                            k = 10)
ros.totaldiv
total.div <- total.div[-4,]

#rerun tukey
totaldiv.lm <- lm(div.dt ~ Epoch, data = total.div)
totaldiv.av <- aov(totaldiv.lm)
summary(totaldiv.av)
totaldiv_test <- HSD.test(totaldiv.av, 'Epoch')
totaldiv_test
TukeyHSD(totaldiv.av) #super interesting out put

#Total diversity box plot
total.div$Epoch <- factor(total.div$Epoch, 
                           levels = c("Cretaceous", "Paleocene", "Eocene",
                                      "Oligocene","Miocene","Pliocene",
                                      "Pleistocene", "Anthropocene"))
total.div <- arrange(total.div, Epoch)

avgtotal.div$Epoch <- factor(avgtotal.div$Epoch, 
                          levels = c("Cretaceous", "Paleocene", "Eocene",
                                     "Oligocene","Miocene","Pliocene",
                                     "Pleistocene", "Anthropocene"))
avgtotal.div <- arrange(avgtotal.div, Epoch)

box.total.div <- ggplot(total.div, aes(fill=Epoch, y=div.dt, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  #stat_compare_means(comparisons = my_comparisons)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above
  geom_text(data= data.frame(avgtotal.div), 
            aes(label = c("ab","ab","ab","ab","ab","ab","a","a"),
                y = c(5,5,5,5,5,5,5,5), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Total Damage Diversity \n(standarized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.total.div

#gall diversity tukey test----
gall.div <- gall.div %>%
  drop_na()
galldiv.lm <- lm(div.gall ~ Epoch, data = gall.div)
galldiv.av <- aov(galldiv.lm)
summary(galldiv.av)
galldiv_test <- HSD.test(galldiv.av, trt = 'Epoch')
galldiv_test
TukeyHSD(galldiv.av)

#outliers:
ros.galldiv <- rosnerTest(gall.div$div.gall,
                           k = 10)
ros.galldiv
#remove outliers
gall.div<- gall.div[c(-4,-18),] #When outliers are removed no longer significant

#rerun tukey
galldiv.lm <- lm(div.gall ~ Epoch, data = gall.div)
galldiv.av <- aov(galldiv.lm)
summary(galldiv.av)
galldiv_test <- HSD.test(galldiv.av, trt = 'Epoch')
galldiv_test

gall.div$Epoch <- factor(gall.div$Epoch, 
                          levels = c("Cretaceous", "Paleocene", "Eocene",
                                     "Oligocene","Miocene","Pliocene",
                                     "Pleistocene","Anthropocene"))
gall.div <- arrange(gall.div, Epoch)

avggall.div$Epoch <- factor(avggall.div$Epoch, 
                         levels = c("Cretaceous", "Paleocene", "Eocene",
                                    "Oligocene","Miocene","Pliocene",
                                    "Pleistocene","Anthropocene"))
avggall.div <- arrange(avggall.div, Epoch)

box.gall.div <- ggplot(gall.div, aes(fill=Epoch, y=div.gall, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above 
  geom_text(data= data.frame(avggall.div), 
            aes(label = c("a","a","a","a","a","a","a","a"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Total Gall Damage Diversity \n(standarized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.gall.div 

#percent gall damage tukey test----
gall.lm <- lm(perc.gall ~ Epoch, data = gall.perc)
gall.av <- aov(gall.lm)
summary(gall.av)
gall_test <- HSD.test(gall.av, trt = 'Epoch')
gall_test

ros.gallperc <- rosnerTest(gall.perc$perc.gall,
                          k = 10)
ros.gallperc
#remove outliers
gall.perc <- gall.perc[c(-51,-14,-4,-65,-1),]
gall.lm <- lm(perc.gall ~ Epoch, data = gall.perc)
gall.av <- aov(gall.lm)
summary(gall.av)
gall_test <- HSD.test(gall.av, trt = 'Epoch')
gall_test

#%Gall damage box plot
gall.perc$Epoch <- factor(gall.perc$Epoch, 
                         levels = c("Cretaceous", "Paleocene", "Eocene",
                                    "Oligocene","Miocene","Pliocene",
                                    "Pleistocene","Anthropocene"))
gall.perc <- arrange(gall.perc, Epoch)

avggall.perc$Epoch <- factor(avggall.perc$Epoch, 
                          levels = c("Cretaceous", "Paleocene", "Eocene",
                                     "Oligocene","Miocene","Pliocene",
                                     "Pleistocene","Anthropocene"))
avggall.perc <- arrange(avggall.perc, Epoch)

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
mine.div <- mine.div %>%
  drop_na()
ros.minediv <- rosnerTest(mine.div$div.mine,
                          k = 10)
ros.minediv
mine.div <- mine.div[c(-4,-59),]

minediv.lm <- lm(div.mine ~ Epoch, data = mine.div)
minediv.av <- aov(minediv.lm)
summary(minediv.av)
minediv_test <- HSD.test(minediv.av, trt = 'Epoch')
minediv_test

#Mine diversity box plot
mine.div$Epoch <- factor(mine.div$Epoch, 
                         levels = c("Cretaceous", "Paleocene", "Eocene",
                                    "Oligocene","Miocene","Pliocene",
                                    "Pleistocene","Anthropocene"))
mine.div <- arrange(mine.div, Epoch)

avgmine.div$Epoch <- factor(avgmine.div$Epoch, 
                         levels = c("Cretaceous", "Paleocene", "Eocene",
                                    "Oligocene","Miocene","Pliocene",
                                    "Pleistocene","Anthropocene"))
avgmine.div <- arrange(avgmine.div, Epoch)

box.mine.div <- ggplot(mine.div, aes(fill=Epoch, y=div.mine, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above 
  geom_text(data= data.frame(avgmine.div), 
            aes(label = c("ab","b","ab","b","ab","ab","ab","a"),
                y = c(-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Total Mine Damage Diversity \n(standarized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.mine.div

#percent mine damage tukey test----
ros.mineperc <- rosnerTest(mine.perc$perc.mine,
                           k = 10)
ros.mineperc
mine.perc <- mine.perc[c(-73,-68,-60,-65,-67,-4),]

mine.lm <- lm(perc.mine ~ Epoch, data = mine.perc)
mine.av <- aov(mine.lm)
summary(mine.av)
mine_test <- HSD.test(mine.av, trt = 'Epoch')
mine_test

#Mine percent box plot
mine.perc$Epoch <- factor(mine.perc$Epoch, 
                         levels = c("Cretaceous", "Paleocene", "Eocene",
                                    "Oligocene","Miocene","Pliocene",
                                    "Pleistocene","Anthropocene"))
mine.perc <- arrange(mine.perc, Epoch)

avgmine.perc$Epoch <- factor(avgmine.perc$Epoch, 
                          levels = c("Cretaceous", "Paleocene", "Eocene",
                                     "Oligocene","Miocene","Pliocene",
                                     "Pleistocene","Anthropocene"))
avgmine.perc <- arrange(avgmine.perc, Epoch)

box.mine.perc <- ggplot(mine.perc, aes(fill=Epoch, y=perc.mine, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above 
  geom_text(data= data.frame(avgmine.perc), 
            aes(label = c("a","a","a","a","a","a","a","a"),
                y = c(-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% of Mine Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.mine.perc

#percent specilized damage tukey test----
ros.specperc <- rosnerTest(spec.perc$perc.spec,
                           k = 10)
ros.specperc

spec.lm <- lm(perc.spec ~ Epoch, data = spec.perc)
spec.av <- aov(spec.lm)
summary(spec.av)
spec_test <- HSD.test(spec.av, trt = 'Epoch')
spec_test

#%spec damage box plot
spec.perc$Epoch <- factor(spec.perc$Epoch, 
                          levels = c("Cretaceous", "Paleocene", "Eocene",
                                     "Oligocene","Miocene","Pliocene",
                                     "Pleistocene","Anthropocene"))
spec.perc <- arrange(spec.perc, Epoch)

avgspec.perc$Epoch <- factor(avgspec.perc$Epoch, 
                           levels = c("Cretaceous", "Paleocene", "Eocene",
                                      "Oligocene","Miocene","Pliocene",
                                      "Pleistocene","Anthropocene"))
avgspec.perc <- arrange(avgspec.perc, Epoch)

box.spec.perc <- ggplot(spec.perc, aes(fill=Epoch, y=perc.spec, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above 
  geom_text(data= data.frame(avgspec.perc), 
            aes(label = c("ab","b","b","b","b","ab","ab","a"),
                y = c(0,0,0,0,0,0,0,0), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Specialized Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.spec.perc

#specialized damage diversity tukey test----
ros.specdiv <- rosnerTest(spec.div$div.spec,
                           k = 10)
ros.specdiv
spec.div <- spec.div[-4,]

specdiv.lm <- lm(div.spec ~ Epoch, data = spec.div)
specdiv.av <- aov(specdiv.lm)
summary(specdiv.av)
specdiv_test <- HSD.test(specdiv.av, trt = 'Epoch')
specdiv_test
TukeyHSD(specdiv.av) #Anthropocene-Paleoecene and Anthro-Olig sig diff. 

#Spec diversity box plot
spec.div$Epoch <- factor(spec.div$Epoch, 
                         levels = c("Cretaceous", "Paleocene", "Eocene",
                                    "Oligocene","Miocene","Pliocene",
                                    "Pleistocene","Anthropocene"))
spec.div <- arrange(spec.div, Epoch)

avgspec.div$Epoch <- factor(avgspec.div$Epoch, 
                         levels = c("Cretaceous", "Paleocene", "Eocene",
                                    "Oligocene","Miocene","Pliocene",
                                    "Pleistocene","Anthropocene"))
avgspec.div <- arrange(avgspec.div, Epoch)

box.spec.div <- ggplot(spec.div, aes(fill=Epoch, y=div.spec, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above
  geom_text(data= data.frame(avgspec.div), 
            aes(label = c("ab","ab","ab","ab","ab","ab","a","a"),
                y = c(0,0,0,0,0,0,0,0), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Total Specialized Damage Diversity \n(standarized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.spec.div

#plant diversity tukey test----
ros.plantdiv <- rosnerTest(plant.div$div.plant,
                          k = 10)
ros.plantdiv
plant.div <- plant.div[c(-47,-2),]

plant.lm <- lm(div.plant ~ Epoch, data= plant.div)
plant.av <- aov(plant.lm)
summary(plant.av)
plant_test <- HSD.test(plant.av, trt="Epoch")
plant_test

#Plant diversity box plot
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

#Percent hole feeding----
ros.hfperc <- rosnerTest(hf.perc$perc.HF,
                           k = 10)
ros.hfperc

hf.lm <- lm(perc.HF ~ Epoch, data= hf.perc)
hf.av <- aov(hf.lm)
summary(hf.av)
hf_test <- HSD.test(hf.av, trt="Epoch")
hf_test
TukeyHSD(hf.av)

#% Hole feeding box plot
hf.perc$Epoch <- factor(hf.perc$Epoch, 
                          levels = c("Cretaceous", "Paleocene", "Eocene",
                                     "Oligocene","Miocene","Pliocene",
                                     "Pleistocene", "Anthropocene"))
hf.perc <- arrange(hf.perc, Epoch)

avgphf.perc$Epoch <- factor(avgphf.perc$Epoch, 
                        levels = c("Cretaceous", "Paleocene", "Eocene",
                                   "Oligocene","Miocene","Pliocene",
                                   "Pleistocene", "Anthropocene"))
avgphf.perc <- arrange(avgphf.perc, Epoch)

box.hf.perc <- ggplot(hf.perc, aes(fill=Epoch, y=perc.HF, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above
  geom_text(data= data.frame(avgphf.perc), 
            aes(label = c("a","a","a","a","a","a","a","b"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Hole Feeding Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.hf.perc

#Percent margin feeding----
ros.mfperc <- rosnerTest(mf.perc$perc.mf,
                         k = 10)
ros.mfperc
mf.perc <- mf.perc[c(-69,-67,-66,-70,-71,-73,-72,-68),] #all anthropocene

mf.lm <- lm(perc.mf ~ Epoch, data= mf.perc)
mf.av <- aov(mf.lm)
summary(mf.av)
mf_test <- HSD.test(mf.av, trt="Epoch")
mf_test

#% Margin feeding box plot
mf.perc$Epoch <- factor(mf.perc$Epoch, 
                        levels = c("Cretaceous", "Paleocene", "Eocene",
                                   "Oligocene","Miocene","Pliocene",
                                   "Pleistocene", "Anthropocene"))
mf.perc <- arrange(mf.perc, Epoch)

avgpmf.perc$Epoch <- factor(avgpmf.perc$Epoch, 
                            levels = c("Cretaceous", "Paleocene", "Eocene",
                                       "Oligocene","Miocene","Pliocene",
                                       "Pleistocene", "Anthropocene"))
avgpmf.perc <- arrange(avgpmf.perc, Epoch)

box.mf.perc <- ggplot(mf.perc, aes(fill=Epoch, y=perc.mf, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above
  geom_text(data= data.frame(avgpmf.perc), 
            aes(label = c("a","a","a","a","a","a","a","b"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Margin Feeding Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.mf.perc

#Percent Skeletonization feeding----
ros.skperc <- rosnerTest(skel.perc$perc.skel,
                         k = 10)
ros.skperc
skel.perc <- skel.perc[c(-69,-73,-33,-71,-31,-70,-48,-32,-72,-2),]
ros.skperc <- rosnerTest(skel.perc$perc.skel,
                         k = 10)
ros.skperc

skel.lm <- lm(perc.skel ~ Epoch, data= skel.perc)
skel.av <- aov(skel.lm)
summary(skel.av)
skel_test <- HSD.test(skel.av, trt="Epoch")
skel_test
TukeyHSD(skel.av) #Anthro-miocene sig diff. 

#% skeletonization feeding box plot
skel.perc$Epoch <- factor(skel.perc$Epoch, 
                        levels = c("Cretaceous", "Paleocene", "Eocene",
                                   "Oligocene","Miocene","Pliocene",
                                   "Pleistocene", "Anthropocene"))
skel.perc <- arrange(skel.perc, Epoch)

avgpskel.perc$Epoch <- factor(avgpskel.perc$Epoch, 
                            levels = c("Cretaceous", "Paleocene", "Eocene",
                                       "Oligocene","Miocene","Pliocene",
                                       "Pleistocene", "Anthropocene"))
avgpskel.perc <- arrange(avgpskel.perc, Epoch)

box.skel.perc <- ggplot(skel.perc, aes(fill=Epoch, y=perc.skel, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above
  geom_text(data= data.frame(avgpskel.perc), 
            aes(label = c("ab","ab","ab","ab","ab","ab","a","a"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Skeletonization Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.skel.perc

#Percent surface feeding----
ros.sfperc <- rosnerTest(sf.perc$perc.sf,
                         k = 10)
ros.sfperc
sf.perc <- sf.perc[c(-4,-68,-11,-62,-54,-53,-37,-70),]

sf.lm <- lm(perc.sf ~ Epoch, data= sf.perc)
sf.av <- aov(sf.lm)
summary(sf.av)
sf_test <- HSD.test(sf.av, trt="Epoch")
sf_test
TukeyHSD(sf.av) #paleo,olio,mio,eco and cret-Anthro sig diff. 

#% surface feeding box plot
sf.perc$Epoch <- factor(sf.perc$Epoch, 
                          levels = c("Cretaceous", "Paleocene", "Eocene",
                                     "Oligocene","Miocene","Pliocene",
                                     "Pleistocene", "Anthropocene"))
sf.perc <- arrange(sf.perc, Epoch)

avgpsf.perc$Epoch <- factor(avgpsf.perc$Epoch, 
                              levels = c("Cretaceous", "Paleocene", "Eocene",
                                         "Oligocene","Miocene","Pliocene",
                                         "Pleistocene", "Anthropocene"))
avgpsf.perc <- arrange(avgpsf.perc, Epoch)

box.sf.perc <- ggplot(sf.perc, aes(fill=Epoch, y=perc.sf, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above
  geom_text(data= data.frame(avgpsf.perc), 
            aes(label = c("b","b","b","b","b","ab","b","a"),
                y = c(-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Surface Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.sf.perc

#Percent piercing and sucking feeding----
ros.pperc <- rosnerTest(pierce.perc$perc.pierce,
                         k = 10)
ros.pperc
pierce.perc <- pierce.perc[c(-50,-54,-37,-62,-32,-57,-4,-69,-51,-35),]
ros.pperc <- rosnerTest(pierce.perc$perc.pierce,
                        k = 10)
ros.pperc
pierce.perc <- pierce.perc[-49,]

p.lm <- lm(perc.pierce ~ Epoch, data= pierce.perc)
p.av <- aov(p.lm)
summary(p.av)
p_test <- HSD.test(p.av, trt="Epoch")
p_test

#% piercing and sucking feeding box plot
pierce.perc$Epoch <- factor(pierce.perc$Epoch, 
                        levels = c("Cretaceous", "Paleocene", "Eocene",
                                   "Oligocene","Miocene","Pliocene",
                                   "Pleistocene", "Anthropocene"))
pierce.perc <- arrange(pierce.perc, Epoch)

avgppierce.perc$Epoch <- factor(avgppierce.perc$Epoch, 
                            levels = c("Cretaceous", "Paleocene", "Eocene",
                                       "Oligocene","Miocene","Pliocene",
                                       "Pleistocene", "Anthropocene"))
avgppierce.perc <- arrange(avgppierce.perc, Epoch)

box.p.perc <- ggplot(pierce.perc, aes(fill=Epoch, y=perc.pierce, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above
  geom_text(data= data.frame(avgpsf.perc), 
            aes(label = c("a","a","a","a","a","a","a","a"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("% Piercing and Sucking Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.p.perc

#Shannon diversity box plot----
ros.shan <- rosnerTest(shannon$Shannon,
                        k = 10)
ros.shan

shan.lm <- lm(Shannon ~ Epoch, data= dam_data)
shan.av <- aov(shan.lm)
summary(shan.av)
shan_test <- HSD.test(shan.av, trt="Epoch")
shan_test

shannon$Epoch <- factor(shannon$Epoch, 
                            levels = c("Cretaceous", "Paleocene", "Eocene",
                                       "Oligocene","Miocene","Pliocene",
                                       "Pleistocene", "Anthropocene"))
shannon <- arrange(shannon, Epoch)

avgshannon$Epoch <- factor(avgshannon$Epoch, 
                                levels = c("Cretaceous", "Paleocene", "Eocene",
                                           "Oligocene","Miocene","Pliocene",
                                           "Pleistocene", "Anthropocene"))
avgshannon <- arrange(avgshannon, Epoch)

box.shannon <- ggplot(shannon, aes(fill=Epoch, y=Shannon, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above
  geom_text(data= data.frame(avgshannon), 
            aes(label = c("a","a","a","a","a","a","a","a"),
                y = c(-1,-1,-1,-1,-1,-1,-1,-1), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Shannon Diversity")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.shannon

#PJ box plot----
ros.pj <- rosnerTest(pj$Pj,
                       k = 10)
ros.pj
pj <- pj[-29,]

pj.lm <- lm(Pj ~ Epoch, data= pj)
pj.av <- aov(pj.lm)
summary(pj.av)
pj_test <- HSD.test(pj.av, trt="Epoch")
pj_test

pj$Epoch <- factor(pj$Epoch, 
                        levels = c("Cretaceous", "Paleocene", "Eocene",
                                   "Oligocene","Miocene","Pliocene",
                                   "Pleistocene", "Anthropocene"))
pj <- arrange(pj, Epoch)

avgpj$Epoch <- factor(avgpj$Epoch, 
                           levels = c("Cretaceous", "Paleocene", "Eocene",
                                      "Oligocene","Miocene","Pliocene",
                                      "Pleistocene", "Anthropocene"))
avgpj <- arrange(avgpj, Epoch)

box.pj <- ggplot(pj, aes(fill=Epoch, y=Pj, x=Epoch)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  scale_fill_manual(values=col8)+ #color palette created above
  geom_text(data= data.frame(avgpj), 
            aes(label = c("a","a","a","a","a","a","a","a"),
                y = c(0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2), size=2))+
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Pielou's J")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
box.pj

#Super plot----
total.super <- plot_grid(box.total.div,box.total.perc,
                    nrow = 1,
                    ncol=2,
                    labels = c("A","B"))
total.super

# gall.super <- plot_grid(box.gall.div,box.gall.perc,
#                         nrow=1,
#                         ncol=2)
# gall.super

mine.super <- plot_grid(box.mine.div, box.mine.perc,
                        nrow=1,
                        ncol=2)
mine.super

spec.super <- plot_grid(box.spec.div, box.spec.perc,
                        nrow = 1,
                        ncol = 2,
                        labels = c("C","D"))
spec.super

#manuscript figure----
totalspec.super <- plot_grid(total.super,
                             spec.super,
                             nrow = 2,
                             ncol = 1)

totalspec.super

#Generalist feeding super
gensuper <- plot_grid(box.hf.perc,
                      box.mf.perc,
                      nrow = 1,
                      ncol = 2,
                      labels = c("A","B"))
gensuper

#Saving figures----
#nonsignificant
ggsave("figures/box.plant.div.pdf", box.plant.div)
ggsave("figures/box.mine.perc.pdf",box.mine.perc)
# ggsave("figures/box.spec.div.pdf",box.spec.div)
# ggsave("figures/box.spec.perc.pdf",box.spec.perc)
# ggsave("figures/box.gall.div.pdf",box.gall.div)
# ggsave("figures/box.gall.perc.pdf",box.gall.perc)
# ggsave("figures/box.total.perc.pdf",box.total.perc)
# ggsave("figures/box.total.div.pdf",box.total.div)
ggsave("figures/box.pierce.perc.pdf", box.p.perc)

#significant
ggsave("figures/box.hf.perc.pdf", box.hf.perc)
ggsave("figures/box.mf.perc.pdf", box.mf.perc)
ggsave("figures/box.skel.perc.pdf", box.skel.perc)
ggsave("figures/box.sf.perc.pdf", box.sf.perc)
ggsave("figures/box.mine.div.pdf",box.mine.div)

#super figures
ggsave("figures/box.total.super.pdf", total.super)
#ggsave("figures/box.gall.super.pdf", gall.super)
ggsave("figures/box.mine.super.pdf", mine.super)
ggsave("figures/box.spec.super.pdf", spec.super)
ggsave("figures/totalspec.super.pdf", totalspec.super, height = 14, units = "in")
ggsave("figures/gensuper.pdf", gensuper, width = 12, units = "in")

