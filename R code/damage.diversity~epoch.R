#Paleoherbivory Review Paper
#Purpose: Damage Diversity and Percent per Epoch
#Plant Diversity by Epoch
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
dam_data <- read.csv("Finalized.Database.forests.csv", header=T, sep=",", 
                     na.strings=  "NA", dec=".", strip.white=TRUE)
#dam_data$Flora <- NULL
#dam_data <-dam_data[-c(76:96), ] 
#for some reason lots of blank rows are being added to the bottom of 
#the data frame. removes them
#orgnize data----
total.perc <- dam_data[,c(2,13)]#percent total damage
total.div <- dam_data[,c(2,24)] #total diversity
#following FFG same organization as above
spec.perc <- dam_data[,c(2,14)] 
spec.div <- dam_data[,c(2,25)] 
mine.perc <- dam_data[,c(2,15)]
mine.div <- dam_data[,c(2,26)]
gall.perc <- dam_data[,c(2,16)]
gall.div <- dam_data[,c(2,27)]
plant.div <- dam_data[,c(2,30)]

#############################################################################
#Tukey's test for significance across time
#Asking does Xdamage change across time? 
#total percent damage tukey test----
total.lm <- lm(perc.dam ~ Epoch, data = dam_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'Epoch')
total_test
#total damage diversity tukey test----
totaldiv.lm <- lm(div.dt ~ Epoch, data = dam_data)
totaldiv.av <- aov(totaldiv.lm)
summary(totaldiv.av)
totaldiv_test <- HSD.test(totaldiv.av, 'Epoch')
totaldiv_test

#percent specilized damage tukey test----
spec.lm <- lm(perc.spec ~ epoch, data = dam_data)
spec.av <- aov(spec.lm)
summary(spec.av)
spec_test <- HSD.test(spec.av, trt = 'epoch')
spec_test
#specialized damage diversity tukey test----
specdiv.lm <- lm(stand.spec ~ epoch, data = dam_data)
specdiv.av <- aov(specdiv.lm)
summary(specdiv.av)
specdiv_test <- HSD.test(specdiv.av, trt = 'epoch')
specdiv_test


#percent mine damage tukey test----
mine.lm <- lm(perc.mine ~ epoch, data = dam_data)
mine.av <- aov(mine.lm)
summary(mine.av)
mine_test <- HSD.test(mine.av, trt = 'epoch')
mine_test
#mine diversity tukey tes----
minediv.lm <- lm(stand.mine ~ epoch, data = dam_data)
minediv.av <- aov(minediv.lm)
summary(minediv.av)
minediv_test <- HSD.test(minediv.av, trt = 'epoch')
minediv_test

#percent gall damage tukey test----
gall.lm <- lm(perc.gall ~ epoch, data = dam_data)
gall.av <- aov(gall.lm)
summary(gall.av)
gall_test <- HSD.test(gall.av, trt = 'epoch')
gall_test
#gall diversity tukey test----
galldiv.lm <- lm(stand.gall ~ epoch, data = dam_data)
galldiv.av <- aov(galldiv.lm)
summary(galldiv.av)
galldiv_test <- HSD.test(galldiv.av, trt = 'epoch')
galldiv_test

#percent hole damage tukey test----
hole.lm <- lm(perc.hole ~ epoch, data = dam_data)
hole.av <- aov(hole.lm)
summary(hole.av)
hole_test <- HSD.test(hole.av, trt = 'epoch')
hole_test

#skeletonization tukey test----
sk.lm <- lm(perc.skel ~ epoch, data = dam_data)
sk.av <- aov(sk.lm)
summary(sk.av)
sk_test <- HSD.test(sk.av, trt = 'epoch')
sk_test

#piercing/sucking tukey test----
p.lm <- lm(perc.pierce ~ epoch, data = dam_data)
p.av <- aov(p.lm)
summary(p.av)
p_test <- HSD.test(p.av, trt = 'epoch')
p_test

#perc. margin tukey test----
marg.lm <- lm(perc.marg ~ epoch, data = dam_data)
marg.av <- aov(marg.lm)
summary(marg.av)
p_test <- HSD.test(marg.av, trt = 'epoch')
p_test

#perc. surface tukey test----
surf.lm <- lm(perc.surface ~ epoch, data = dam_data)
surf.av <- aov(surf.lm)
summary(surf.av)
p_test <- HSD.test(surf.av, trt = 'epoch')
p_test

#plant diversity tukey test----
plant.lm <- lm(stand.plant.div ~ epoch, data= dam_data)
plant.av <- aov(plant.lm)
summary(plant.av)
plant_test <- HSD.test(plant.av, trt="epoch")
plant_test
###############################################################################
#creating average and sd for each ffg
#Total percent damage
avgtotal.perc <- total.perc %>%
  drop_na()%>% #dropping NAs at bottom of .csv
  group_by(epoch)%>% #grouping by epoch
  summarise(mean.all= mean(perc.damage), #average
            sd.all = sd(perc.damage), #standard deviation
            count=n())
#Total damage diversity 
avgtotal.div <- total.div %>%
  drop_na()%>% #dropping NAs at bottom of .csv
  group_by(epoch)%>% #grouping by epoch
  summarise(mean.all= mean(stand.dt), #average
            sd.all = sd(stand.dt), #standard deviation
            count=n())

#percent specialized damage
avgspec.perc <- spec.perc %>%
  drop_na(perc.spec)%>%
  group_by(epoch)%>%
  summarise(mean.spec = mean(perc.spec),
            sd.spec = sd(perc.spec),
            count=n()) #number of samples/sites per epoch)
#specialized damage diversity 
avgspec.div <- spec.div %>%
  drop_na(stand.spec)%>%
  group_by(epoch)%>%
  summarise(mean.spec = mean(stand.spec),
            sd.spec = sd(stand.spec),
            count=n()) #number of samples/sites per epoch)

#percent mine damage
avgmine.perc <- mine.perc %>%
  drop_na(perc.mine)%>%
  group_by(epoch)%>%
  summarise(mean.mine = mean(perc.mine),
            sd.mine = sd(perc.mine),
            count=n()) #number of samples/sites per epoch)
#mine damage diversity 
avgmine.div <- mine.div %>%
  drop_na(stand.mine)%>%
  group_by(epoch)%>%
  summarise(mean.mine = mean(stand.mine),
            sd.mine = sd(stand.mine),
            count=n()) #number of samples/sites per epoch)

#percent gall damage
avggall.perc <-gall.perc %>%
  drop_na(perc.gall)%>%
  group_by(epoch)%>%
  summarise(mean.gall = mean(perc.gall),
            sd.gall = sd(perc.gall),
            count=n()) #number of samples/sites per epoch)
#gall damage diversity 
avggall.div <-gall.div %>%
  drop_na(stand.gall)%>%
  group_by(epoch)%>%
  summarise(mean.gall = mean(stand.gall),
            sd.gall = sd(stand.gall),
            count=n()) #number of samples/sites per epoch)
#percent hole feeding damage
avgHF.perc <- HF.perc %>%
  drop_na(perc.hole)%>%
  group_by(epoch)%>%
  summarise(mean.HF = mean(perc.hole),
            sd.HF = sd(perc.hole),
            count=n()) #number of samples/sites per epoch)

#percent skeletonization damage
avgsk.perc <-sk.perc %>%
  drop_na(perc.skel)%>%
  group_by(epoch)%>%
  summarise(mean.sk = mean(perc.skel),
            sd.sk = sd(perc.skel),
            count=n()) #number of samples/sites per epoch)
#percent piercing/sucking damage
avgP.perc <- P.perc %>%
  drop_na(perc.pierce)%>%
  group_by(epoch)%>%
  summarise(mean.P = mean(perc.pierce),
            sd.P = sd(perc.pierce),
            count=n()) #number of samples/sites per epoch)

#percent margin damage
avgmarg.perc <- marg.perc %>%
  drop_na(perc.marg)%>%
  group_by(epoch)%>%
  summarise(mean.P = mean(perc.marg),
            sd.P = sd(perc.marg),
            count=n()) #number of samples/sites per epoch)

#percent surface damage
avgsurf.perc <- surf.perc %>%
  drop_na(perc.surface)%>%
  group_by(epoch)%>%
  summarise(mean.P = mean(perc.surface),
            sd.P = sd(perc.surface),
            count=n()) #number of samples/sites per epoch)

#plant diveristy 
avgplant.div <- plant.div %>%
  drop_na()%>%
  group_by(epoch)%>%
  summarise(mean.plant = mean(stand.plant.div),
            sd.plant = sd(stand.plant.div),
            count=n())

###############################################################################

#Cleaning up data and making color palette----
#Ordering legend properly
# dam_data$Epoch.Period <- factor(dam_data$Epoch.Period, 
#                          levels = c("Cretaceous", "Paleocene", "Eocene",
#                                     "Oligocene","Miocene","Pliocene",
#                                     "Pleistocene",))
#colors for barplot----
#col7 must be in THIS order for this plot!  
col7 <- c("#F2CC8F","#81B29A","#5F797B","#3D405B","#8F5D5D","#E07A5F","#c89f9c")
#col8 <- c("#F2CC8F","#81B29A","#5F797B","#3D405B","#8F5D5D","#E07A5F","#c89f9c","#EAB69F")
#col7 below is for the other plots.  
# col7 <- c("#c89f9c","#E07A5F","#8F5D5D", "#3D405B","#5F797B","#81B29A","#F2CC8F")

#Bar graph Total %Dam total----
avgtotal.perc$epoch <- factor(avgtotal.perc$epoch, 
                              levels = c("Cretaceous", "Paleocene", "Eocene",
                                         "Oligocene","Miocene","Pliocene",
                                         "Pleistocene"))
avgtotal.perc <- arrange(avgtotal.perc, epoch)
total.perc <- ggplot(avgtotal.perc, aes(fill=epoch, y=mean.all, x=epoch)) +
  #geom_errorbar must be first or it wont print them correctly
  geom_errorbar(aes(ymin = 0, ymax = mean.all + sd.all), 
                width=0.1,
                position = position_dodge(0.9),
                alpha=0.4, size=.5)+
  #geom_bar just needs these arguments becuase the aes are given in the first line
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(label = c("a","a","a","a","a","a","a"), 
            aes(y = c(-1,-1,-1,-1,-1,-1,-1), size = 2))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  #geom_text gives the asthetics for count information
  geom_text(aes(label=count), 
            position=position_dodge(width=0.9), #puts number on bar
            vjust=-0.5, #centers it
            size=4)+ #size of text
  scale_fill_manual(values=col7)+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Average % Total \nDamage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
total.perc

#Bar graph Total Damage Diversity----
avgtotal.div$epoch <- factor(avgtotal.div$epoch, 
                             levels = c("Cretaceous", "Paleocene", "Eocene",
                                        "Oligocene","Miocene","Pliocene",
                                        "Pleistocene"))

avgtotal.div <- arrange(avgtotal.div, epoch)
total.div <- ggplot(avgtotal.div, aes(fill=epoch, y=mean.all, x=epoch)) +
  #geom_errorbar must be first or it wont print them correctly
  geom_errorbar(aes(ymin = 0, ymax = mean.all + sd.all), 
                width=0.1,
                position = position_dodge(0.9),
                alpha=0.4, size=.5)+
  #geom_bar just needs these arguments becuase the aes are given in the first line
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(label = c("a","a","a","a","a","a","a"), 
            aes(y = c(-0.3,-0.3,-0.3,-0.3,-0.3,-0.3,-0.3), size = 2))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  #geom_text gives the asthetics for count information
  geom_text(aes(label=count), 
            position=position_dodge(width=0.9), #puts number on bar
            vjust=-0.5, #centers it
            size=4)+ #size of text
  scale_fill_manual(values=col7)+ #color palette created above 
  #ggtitle("Average Percent Damage by Epoch")+ #title
  ylab("Average Total Damage \nDiversity")+ #axis labels
  xlab("")+
  ylim(-0.5,35)+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
total.div

#Bar graph % Gall Dam----
avggall.perc$epoch <- factor(avggall.perc$epoch, 
                             levels = c("Cretaceous", "Paleocene", "Eocene",
                                        "Oligocene","Miocene","Pliocene",
                                        "Pleistocene"))
avggall.perc <- arrange(avggall.perc, epoch)

gall.perc <- ggplot(avggall.perc, aes(fill=epoch, y=mean.gall, x=epoch)) +
  #geom_errorbar must be first or it wont print them correctly
  geom_errorbar(aes(ymin = 0, ymax = mean.gall + sd.gall), 
                width=0.1,
                position = position_dodge(0.9),
                alpha=0.4, size=.5)+
  #geom_bar just needs these arguments becuase the aes are given in the first line
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(label = c("a","a","a","a","a","a","a"),
            aes(y = c(-0.2,-0.2,-0.2,-0.2,-0.2,-0.2,-0.2), size = 2))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  #geom_text gives the asthetics for count information
  geom_text(aes(label=count), 
            position=position_dodge(width=0.9), #puts number on bar
            vjust=-0.5, #centers it
            size=4)+ #size of text
  scale_fill_manual(values=col7)+ #color palette created above
  #ggtitle("Average Percent Gall Damage by Epoch")+ #title
  ylab("Average %Gall \nDamage")+ #axis labels
  xlab("")+
  ylim(-0.5,13)+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
gall.perc

#Gall Damage Diversity----
avggall.div$epoch <- factor(avggall.div$epoch, 
                            levels = c("Cretaceous", "Paleocene", "Eocene",
                                       "Oligocene","Miocene","Pliocene",
                                       "Pleistocene"))
avggall.div <- arrange(avggall.div, epoch)
gall.div <- ggplot(avggall.div, aes(fill=epoch, y=mean.gall, x=epoch)) +
  #geom_errorbar must be first or it wont print them correctly
  geom_errorbar(aes(ymin = 0, ymax = mean.gall + sd.gall), 
                width=0.1,
                position = position_dodge(0.9),
                alpha=0.4, size=.5)+
  #geom_bar just needs these arguments becuase the aes are given in the first line
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(label = c("ab","b","ab","ab","a","ab","ab"), 
            aes(y = c(-0.2,-0.2,-0.2,-0.2,-0.2,-0.2,-0.2), size = 2))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  #geom_text gives the asthetics for count information
  geom_text(aes(label=count), 
            position=position_dodge(width=0.9), #puts number on bar
            vjust=-0.5, #centers it
            size=4)+ #size of text
  scale_fill_manual(values=col7)+ #color palette created above
  ylim(-0.5,13)+
  #ggtitle("Average Percent Gall Damage by Epoch")+ #title
  ylab("Average Gall Damage \nDiversity")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
gall.div

#% HF dam----
avgHF.perc$epoch <- factor(avgHF.perc$epoch, 
                           levels = c("Cretaceous", "Paleocene", "Eocene",
                                      "Oligocene","Miocene","Pliocene",
                                      "Pleistocene"))
avgHF.perc <- arrange(avgHF.perc, epoch)
hf.perc <- ggplot(avgHF.perc, aes(fill=epoch, y=mean.HF, x=epoch)) +
  #geom_errorbar must be first or it wont print them correctly
  geom_errorbar(aes(ymin = 0, ymax = mean.HF + sd.HF), 
                width=0.1,
                position = position_dodge(0.9),
                alpha=0.4, size=.5)+
  #geom_bar just needs these arguments becuase the aes are given in the first line
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(label = c("a","a","a","a","a","a","a"), 
            aes(y = c(-0.3,-0.3,-0.3,-0.3,-0.3,-0.3,-0.3), size = 2))+
  # stat_compare_means(aes(group = epoch),label = "p.format")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  #geom_text gives the asthetics for count information
  geom_text(aes(label=count), 
            position=position_dodge(width=0.9), #puts number on bar
            vjust=-0.5, #centers it
            size=4)+ #size of text
  scale_fill_manual(values=col7)+ #color palette created above 
  #ggtitle("Average Percent Hole Feeding Damage by Epoch")+ #title
  ylab("Average % Hole Feeding \nDamage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
hf.perc

#% specialized dam ----
avgspec.perc$epoch <- factor(avgspec.perc$epoch, 
                             levels = c("Cretaceous", "Paleocene", "Eocene",
                                        "Oligocene","Miocene","Pliocene",
                                        "Pleistocene"))
avgspec.perc <- arrange(avgspec.perc, epoch)
spec.perc <- ggplot(avgspec.perc, aes(fill=epoch, y=mean.spec, x=epoch)) +
  #geom_errorbar must be first or it wont print them correctly
  geom_errorbar(aes(ymin = 0, ymax = mean.spec + sd.spec), 
                width=0.1,
                position = position_dodge(0.9),
                alpha=0.4, size=.5)+
  #geom_bar just needs these arguments becuase the aes are given in the first line
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(label = c("a","a","a","a","a","a","a"), 
            aes(y = c(-0.3,-0.3,-0.3,-0.3,-0.3,-0.3,-0.3), size = 2))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  #geom_text gives the asthetics for count information
  geom_text(aes(label=count), 
            position=position_dodge(width=0.9), #puts number on bar
            vjust=-0.5, #centers it
            size=4)+ #size of text
  scale_fill_manual(values=col7)+ #color palette created above 
  #ggtitle("Average Percent Specialized Damage by Epoch")+ #title
  ylab("Average % Specialized \nDamage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
spec.perc

#Specialized damage diveristy
avgspec.div$epoch <- factor(avgspec.div$epoch, 
                            levels = c("Cretaceous", "Paleocene", "Eocene",
                                       "Oligocene","Miocene","Pliocene",
                                       "Pleistocene"))
avgspec.div <- arrange(avgspec.div, epoch)
spec.div <- ggplot(avgspec.div, aes(fill=epoch, y=mean.spec, x=epoch)) +
  #geom_errorbar must be first or it wont print them correctly
  geom_errorbar(aes(ymin = 0, ymax = mean.spec + sd.spec), 
                width=0.1,
                position = position_dodge(0.9),
                alpha=0.4, size=.5)+
  #geom_bar just needs these arguments becuase the aes are given in the first line
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(label = c("a","a","a","a","a","a","a"), 
            aes(y = c(-0.3,-0.3,-0.3,-0.3,-0.3,-0.3,-0.3), size = 2))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  #geom_text gives the asthetics for count information
  geom_text(aes(label=count), 
            position=position_dodge(width=0.9), #puts number on bar
            vjust=-0.5, #centers it
            size=4)+ #size of text
  scale_fill_manual(values=col7)+ #color palette created above 
  #ggtitle("Average Percent Specialized Damage by Epoch")+ #title
  ylim(-0.5,20)+
  ylab("Average Specialized Damage \nDiversity")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
spec.div

#% mine dam----
avgmine.perc$epoch <- factor(avgmine.perc$epoch, 
                             levels = c("Cretaceous", "Paleocene", "Eocene",
                                        "Oligocene","Miocene","Pliocene",
                                        "Pleistocene"))
avgmine.perc <- arrange(avgmine.perc, epoch)
mine.perc <- ggplot(avgmine.perc, aes(fill=epoch, y=mean.mine, x=epoch)) +
  #geom_errorbar must be first or it wont print them correctly
  geom_errorbar(aes(ymin = 0, ymax = mean.mine + sd.mine), 
                width=0.1,
                position = position_dodge(0.9),
                alpha=0.4, size=.5)+
  #geom_bar just needs these arguments becuase the aes are given in the first line
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(label = c("a","a","a","a","a","a","a"), 
            aes(y = c(-0.3,-0.3,-0.3,-0.3,-0.3,-0.3,-0.3), size = 2))+ 
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  #geom_text gives the asthetics for count information
  geom_text(aes(label=count), 
            position=position_dodge(width=0.9), #puts number on bar
            vjust=-0.5, #centers it
            size=4)+ #size of text
  scale_fill_manual(values=col7)+ #color palette created above 
  #ggtitle("Average Percent Mine Damage by Epoch")+ #title
  ylab("Average % Mine \nDamage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mine.perc

#mine damage diversity 
avgmine.div$epoch <- factor(avgmine.div$epoch, 
                            levels = c("Cretaceous", "Paleocene", "Eocene",
                                       "Oligocene","Miocene","Pliocene",
                                       "Pleistocene"))
avggmine.div <- arrange(avgmine.div, epoch)
mine.div <- ggplot(avgmine.div, aes(fill=epoch, y=mean.mine, x=epoch)) +
  #geom_errorbar must be first or it wont print them correctly
  geom_errorbar(aes(ymin = 0, ymax = mean.mine + sd.mine), 
                width=0.1,
                position = position_dodge(0.9),
                alpha=0.4, size=.5)+
  #geom_bar just needs these arguments becuase the aes are given in the first line
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(label = c("a","a","a","a","a","a","a"), 
            aes(y = c(-0.3,-0.3,-0.3,-0.3,-0.3,-0.3,-0.3), size = 2))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  #geom_text gives the asthetics for count information
  geom_text(aes(label=count), 
            position=position_dodge(width=0.9), #puts number on bar
            vjust=-0.5, #centers it
            size=4)+ #size of text
  scale_fill_manual(values=col7)+ #color palette created above 
  #ggtitle("Average Percent Mine Damage by Epoch")+ #title
  ylab("Average Mine Damage \nDiversity")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mine.div

#% skel dam----
avgsk.perc$epoch <- factor(avgsk.perc$epoch, 
                           levels = c("Cretaceous", "Paleocene", "Eocene",
                                      "Oligocene","Miocene","Pliocene",
                                      "Pleistocene"))
avgsk.perc <- arrange(avgsk.perc, epoch)
skel.perc <- ggplot(avgsk.perc, aes(fill=epoch, y=mean.sk, x=epoch)) +
  #geom_errorbar must be first or it wont print them correctly
  geom_errorbar(aes(ymin = 0, ymax = mean.sk + sd.sk), 
                width=0.1,
                position = position_dodge(0.9),
                alpha=0.4, size=.5)+
  #geom_bar just needs these arguments becuase the aes are given in the first line
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(label = c("a","a","a","a","a","a","a"), 
            aes(y = c(-0.3,-0.3,-0.3,-0.3,-0.3,-0.3,-0.3), size = 2))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  #geom_text gives the asthetics for count information
  geom_text(aes(label=count), 
            position=position_dodge(width=0.9), #puts number on bar
            vjust=-0.5, #centers it
            size=4)+ #size of text
  scale_fill_manual(values=col7)+ #color palette created above 
  #ggtitle("Average Percent Skeleton Damage by Epoch")+ #title
  ylab("Average % Skeleton \nDamage")+ #axis labels
  xlab("Geologic Time (oldest - youngest)")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
skel.perc

#% piercing damage----
avgP.perc$epoch <- factor(avgP.perc$epoch, 
                          levels = c("Cretaceous", "Paleocene", "Eocene",
                                     "Oligocene","Miocene","Pliocene",
                                     "Pleistocene"))
avgP.perc <- arrange(avgP.perc, epoch)
pierce.perc <- ggplot(avgP.perc, aes(fill=epoch, y=mean.P, x=epoch)) +
  #geom_errorbar must be first or it wont print them correctly
  geom_errorbar(aes(ymin = 0, ymax = mean.P + sd.P), 
                width=0.1,
                position = position_dodge(0.9),
                alpha=0.4, size=.5)+
  #geom_bar just needs these arguments becuase the aes are given in the first line
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(label = c("a","a","a","a","a","a","a"), 
            aes(y = c(-0.3,-0.3,-0.3,-0.3,-0.3,-0.3,-0.3), size = 2))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  #geom_text gives the asthetics for count information
  geom_text(aes(label=count), 
            position=position_dodge(width=0.9), #puts number on bar
            vjust=-0.5, #centers it
            size=4)+ #size of text
  scale_fill_manual(values=col7)+ #color palette created above 
  #ggtitle("Average Percent Piercing and Sucking Damage by Epoch")+ #title
  ylab("Average % Piercing and \nSucking Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
pierce.perc

#% margin damage----
avgmarg.perc$epoch <- factor(avgmarg.perc$epoch, 
                             levels = c("Cretaceous", "Paleocene", "Eocene",
                                        "Oligocene","Miocene","Pliocene",
                                        "Pleistocene"))
avgmarg.perc <- arrange(avgmarg.perc, epoch)
marg.perc <- ggplot(avgmarg.perc, aes(fill=epoch, y=mean.P, x=epoch)) +
  #geom_errorbar must be first or it wont print them correctly
  geom_errorbar(aes(ymin = 0, ymax = mean.P + sd.P), 
                width=0.1,
                position = position_dodge(0.9),
                alpha=0.4, size=.5)+
  #geom_bar just needs these arguments becuase the aes are given in the first line
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(label = c("a","a","a","a","a","a","a"), 
            aes(y = c(-0.3,-0.3,-0.3,-0.3,-0.3,-0.3,-0.3), size = 2))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  #geom_text gives the asthetics for count information
  geom_text(aes(label=count), 
            position=position_dodge(width=0.9), #puts number on bar
            vjust=-0.5, #centers it
            size=4)+ #size of text
  scale_fill_manual(values=col7)+ #color palette created above 
  #ggtitle("Average Percent Piercing and Sucking Damage by Epoch")+ #title
  ylab("Average % Margin Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
marg.perc

#% surface damage----
avgsurf.perc$epoch <- factor(avgsurf.perc$epoch, 
                             levels = c("Cretaceous", "Paleocene", "Eocene",
                                        "Oligocene","Miocene","Pliocene",
                                        "Pleistocene"))
avgsurf.perc <- arrange(avgsurf.perc, epoch)
surf.perc <- ggplot(avgsurf.perc, aes(fill=epoch, y=mean.P, x=epoch)) +
  #geom_errorbar must be first or it wont print them correctly
  geom_errorbar(aes(ymin = 0, ymax = mean.P + sd.P), 
                width=0.1,
                position = position_dodge(0.9),
                alpha=0.4, size=.5)+
  #geom_bar just needs these arguments becuase the aes are given in the first line
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(label = c("a","a","a","a","a","a","a"), 
            aes(y = c(-0.3,-0.3,-0.3,-0.3,-0.3,-0.3,-0.3), size = 2))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  #geom_text gives the asthetics for count information
  geom_text(aes(label=count), 
            position=position_dodge(width=0.9), #puts number on bar
            vjust=-0.5, #centers it
            size=4)+ #size of text
  scale_fill_manual(values=col7)+ #color palette created above 
  #ggtitle("Average Percent Piercing and Sucking Damage by Epoch")+ #title
  ylab("Average % Surface Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
surf.perc


#Plant diveristy 
avgplant.div$epoch <- factor(avgplant.div$epoch, 
                             levels = c("Cretaceous", "Paleocene", "Eocene",
                                        "Oligocene","Miocene","Pliocene",
                                        "Pleistocene"))
avgplant.div <- arrange(avgplant.div, epoch)
plant.perc <- ggplot(avgplant.div, aes(fill=epoch, y=mean.plant, x=epoch)) +
  #geom_errorbar must be first or it wont print them correctly
  geom_errorbar(aes(ymin = 0, ymax = mean.plant + sd.plant), 
                width=0.1,
                position = position_dodge(0.9),
                alpha=0.4, size=.5)+
  #geom_bar just needs these arguments becuase the aes are given in the first line
  geom_bar(stat = "identity", position = position_dodge())+
  geom_text(label = c("a","a","a","a","a","a","a"), 
            aes(y = c(-0.3,-0.3,-0.3,-0.3,-0.3,-0.3,-0.3), size = 2))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl 
  #geom_text gives the asthetics for count information
  geom_text(aes(label=count), 
            position=position_dodge(width=0.9), #puts number on bar
            vjust=-0.5, #centers it
            size=4)+ #size of text
  scale_fill_manual(values=col7)+ #color palette created above 
  #ggtitle("Average Percent Piercing and Sucking Damage by Epoch")+ #title
  ylab("Average Plant \nDiversity")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
plant.perc

#putting plots together into superplot----
super1 <- plot_grid(total.div,total.perc,
                    spec.div, spec.perc,
                    gall.div, gall.perc,
                    nrow = 3,
                    ncol=2)
super1
#Saving figures----
ggsave("LAS analyses/Paleoherb_paper/figures/plant.div.pdf", plant.perc)
ggsave("LAS analyses/Paleoherb_paper/figures/pierce.perc.pdf",pierce.perc)
ggsave("LAS analyses/Paleoherb_paper/figures/skel.perc.pdf",skel.perc)
ggsave("LAS analyses/Paleoherb_paper/figures/mine.div.pdf",mine.div)
ggsave("LAS analyses/Paleoherb_paper/figures/mine.perc.pdf",mine.perc)
ggsave("LAS analyses/Paleoherb_paper/figures/spec.div.pdf",spec.div)
ggsave("LAS analyses/Paleoherb_paper/figures/spec.perc.pdf",spec.perc)
ggsave("LAS analyses/Paleoherb_paper/figures/hf.perc.pdf",hf.perc)
ggsave("LAS analyses/Paleoherb_paper/figures/gall.div.pdf",gall.div)
ggsave("LAS analyses/Paleoherb_paper/figures/gall.perc.pdf",gall.perc)
ggsave("LAS analyses/Paleoherb_paper/figures/total.perc.pdf",total.perc)
ggsave("LAS analyses/Paleoherb_paper/figures/total.div.pdf",total.div)
ggsave("LAS analyses/Paleoherb_paper/figures/marg.perc.pdf",marg.perc)
ggsave("LAS analyses/Paleoherb_paper/figures/surf.perc.pdf",surf.perc)
ggsave("LAS analyses/Paleoherb_paper/figures/totspecgallsuper.pdf", super1)
#ggsave("Figures/final_damage.pdf", final_dam)
