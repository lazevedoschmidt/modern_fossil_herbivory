#Damage percentage and diversity across only modern ecosystems
#Is there more damage in the tropics than in the temperate? 

#load packages
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(agricolae) #You need this for the Tukey test
library(EnvStats) #rosnerTest

#color palette for ecosystems
coleco <- c("#ff6b35","#004e89","#2c6e49")

#load data file----
dam_data <- read.csv("Finalized.Database.forests.csv", header=T, sep=",", 
                     na.strings=  "NA", dec=".", strip.white=TRUE)
#seperating out modern
anth_data <- filter(dam_data, Epoch == "Anthropocene")
anth_data <- anth_data[,c(-2:-11)]
anth_data <- drop_na(anth_data)
groups <- c("La Selva","La Selva","La Selva", "Harvard Forest", "Harvard Forest",
            "Harvard Forest", "SERC", "SERC","SERC")
anth_data <- cbind(anth_data, groups=groups)

#Total percent damage
avganth.all <- anth_data %>%
  drop_na()%>% #dropping NAs at bottom of .csv
  group_by(groups)%>% #grouping by epoch
  summarise(mean.all= mean(perc.dam), #average
            sd.all = sd(perc.dam), #standard deviation
            mean.spec=mean(perc.spec),
            sd.spec=sd(perc.spec),
            mean.spec=mean(perc.mine),
            sd.spec=sd(perc.mine),
            mean.gall=mean(perc.gall),
            sd.gall=sd(perc.gall),
            mean.HF=mean(perc.HF),
            sd.HF=sd(perc.HF),
            mean.mf=mean(perc.mf),
            sd.mf=sd(perc.mf),
            mean.skel=mean(perc.skel),
            sd.skel=sd(perc.skel),
            mean.sf=mean(perc.sf),
            sd.sf=sd(perc.sf),
            mean.pier=mean(perc.pierce),
            sd.pier=sd(perc.pierce),
            mean.rawdt=mean(raw.dt),
            sd.rawdt=sd(raw.dt),
            mean.rawffg=mean(raw.ffg),
            sd.rawffg=sd(raw.ffg),
            mean.divdt=mean(div.dt),
            sd.divdt=sd(div.dt),
            mean.divspec=mean(div.spec),
            sd.divspec=sd(div.spec),
            mean.divmine=mean(div.mine),
            sd.divmine=sd(div.mine),
            mean.divgall=mean(div.gall),
            sd.divgall=sd(div.gall),
            mean.shannon=mean(Shannon),
            sd.shannon=sd(Shannon),
            mean.pj=mean(Pj),
            sd.pj=sd(Pj),
            mean.divplant=mean(div.plant),
            sd.divplant=sd(div.plant),
            count=n())

#plotting damage percent
anth_data$groups <- factor(anth_data$groups, 
                           levels = c("La Selva", "SERC", "Harvard Forest"))
anth_data <- arrange(anth_data, groups)

avganth.all$groups <- factor(avganth.all$groups, 
                           levels = c("La Selva", "SERC", "Harvard Forest"))
avganth.all <- arrange(avganth.all, groups)

#Total percent damage----
total.lm <- lm(perc.dam ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av) #checking additional TUkey test 

#checking outliers:
ros.totalperc <- rosnerTest(anth_data$perc.dam,
                            k = 1)
ros.totalperc 

mod.total.perc <- ggplot(anth_data, aes(fill=groups, y=perc.dam, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","a","a"),
                y = c(68,68,68), size=2))+
  ylab("% Total Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.total.perc

#Total damage diversity----
total.lm <- lm(div.dt ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#checking outliers:
ros.totalperc <- rosnerTest(anth_data$div.dt,
                            k = 1)
ros.totalperc

#plotting damage diversity
mod.total.div <- ggplot(anth_data, aes(fill=groups, y=div.dt, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","a","a"),
                y = c(23,23,23), size=2))+
  ylab("Total Damage Diversity \n(Standardized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.total.div

#Specialization percent----
total.lm <- lm(perc.spec ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#checking outliers:
ros.totalperc <- rosnerTest(anth_data$perc.spec,
                            k = 1)
ros.totalperc

mod.perc.spec <- ggplot(anth_data, aes(fill=groups, y=perc.spec, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","a","a"),
                y = c(13,13,13), size=2))+
  ylab("% Specialized Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.perc.spec

#speialized diveristy----
total.lm <- lm(div.spec ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#checking outliers:
ros.totalperc <- rosnerTest(anth_data$div.spec,
                            k = 1)
ros.totalperc

#plotting damage percent
mod.spec.div <- ggplot(anth_data, aes(fill=groups, y=div.spec, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","a","a"),
                y = c(9,9,9), size=2))+
  ylab("Specialized Damage Diversity \n(Standardized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.spec.div

# #Minning percent----
#checking outliers:
ros.totalperc <- rosnerTest(anth_data$perc.mine,
                            k = 1)
ros.totalperc

total.lm <- lm(perc.mine ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#plotting damage percent
mod.perc.mine <- ggplot(anth_data, aes(fill=groups, y=perc.mine, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","a","a"),
                y = c(0,0,0), size=2))+
  ylab("% Mining Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.perc.mine

#Mining diversity----
#checking outliers:
ros.totalperc <- rosnerTest(anth_data$div.mine,
                            k = 1)
ros.totalperc

total.lm <- lm(div.mine ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#plotting damage percent
mod.div.mine <- ggplot(anth_data, aes(fill=groups, y=div.mine, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","ab","b"),
                y = c(0,0,0), size=2))+
  ylab("Mine Damage Diversity \n(Standardized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.div.mine

#Gall percent----
#checking outliers:
ros.totalperc <- rosnerTest(anth_data$perc.gall,
                            k = 1)
ros.totalperc

total.lm <- lm(perc.gall ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#plotting damage percent
mod.gall.perc <- ggplot(anth_data, aes(fill=groups, y=perc.gall, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","a","a"),
                y = c(0,0,0), size=2))+
  ylab("% Gall Damage")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.gall.perc
#Gall diveristy----
#checking outliers:
ros.totalperc <- rosnerTest(anth_data$div.gall,
                            k = 1)
ros.totalperc

total.lm <- lm(div.gall ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#plotting
mod.gall.div <- ggplot(anth_data, aes(fill=groups, y=div.gall, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","a","a"),
                y = c(0.5,0.5,0.5), size=2))+
  ylab("Gall Damage Diversity \n(Standardized to 300 leaves)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.gall.div

#Hole feeding----
#checking outliers:
ros.totalperc <- rosnerTest(anth_data$perc.HF,
                            k = 1)
ros.totalperc

total.lm <- lm(perc.HF ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#plotting 
mod.HF.perc <- ggplot(anth_data, aes(fill=groups, y=perc.HF, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","a","a"),
                y = c(35,35,35), size=2))+
  ylab("% Hole Feeding")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.HF.perc

#Margin feeding----
#checking outliers:
ros.totalperc <- rosnerTest(anth_data$perc.mf,
                            k = 1)
ros.totalperc

total.lm <- lm(perc.mf ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#plotting 
mod.mf.perc <- ggplot(anth_data, aes(fill=groups, y=perc.mf, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","a","a"),
                y = c(40,40,40), size=2))+
  ylab("% Margin Feeding")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.mf.perc

#Skeletonization----
ros.totalperc <- rosnerTest(anth_data$perc.skel,
                            k = 1)
ros.totalperc

total.lm <- lm(perc.skel ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#plotting 
mod.skel.perc <- ggplot(anth_data, aes(fill=groups, y=perc.skel, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","a","a"),
                y = c(40,40,40), size=2))+
  ylab("% Skeletonization Feeding")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.mf.perc

#Surface Feeding----
ros.totalperc <- rosnerTest(anth_data$perc.sf,
                            k = 1)
ros.totalperc
anth_data <- anth_data[-7,]

total.lm <- lm(perc.sf ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#plotting damage percent
mod.sf.perc <- ggplot(anth_data, aes(fill=groups, y=perc.sf, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","b","a"),
                y = c(0,0,0), size=2))+
  ylab("% Surface Feeding")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.sf.perc

#RERUN LINES 15-75 BEFORE MOVING ON!!!!!!

#Piercing----
ros.totalperc <- rosnerTest(anth_data$perc.pierce,
                            k = 1)
ros.totalperc
anth_data <- anth_data[-8,]

total.lm <- lm(perc.pierce ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#plotting damage percent
mod.p.perc <- ggplot(anth_data, aes(fill=groups, y=perc.pierce, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","a","a"),
                y = c(0,0,0), size=2))+
  ylab("% Piercing and Sucking \nFeeding")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.p.perc

#RERUN LINES 15-75 BEFORE MOVING ON!!!!!!

#Raw Damage Types----
ros.totalperc <- rosnerTest(anth_data$raw.dt,
                            k = 1)
ros.totalperc

total.lm <- lm(raw.dt ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#plotting
mod.raw.dt <- ggplot(anth_data, aes(fill=groups, y=raw.dt, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","a","a"),
                y = c(34,34,34), size=2))+
  ylab("Damage Type Diversity (Raw)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.raw.dt

#Shannon Diversity----
ros.totalperc <- rosnerTest(anth_data$Shannon,
                            k = 1)
ros.totalperc

total.lm <- lm(Shannon ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#plotting 
mod.shannon <- ggplot(anth_data, aes(fill=groups, y=Shannon, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","a","a"),
                y = c(1,1,1), size=2))+
  ylab("Shannon Diversity (mean)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.shannon

#PJ----
ros.totalperc <- rosnerTest(anth_data$Pj,
                            k = 1)
ros.totalperc

total.lm <- lm(Pj ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#plotting 
mod.PJ <- ggplot(anth_data, aes(fill=groups, y=Pj, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","a","a"),
                y = c(0.4,0.4,0.4), size=2))+
  ylab("Pielou's J (Evenness)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.PJ

#Plant Diversity----
ros.totalperc <- rosnerTest(anth_data$div.plant,
                            k = 1)
ros.totalperc

total.lm <- lm(div.plant ~ groups, data = anth_data)
total.av <- aov(total.lm)
summary(total.av)
total_test <- HSD.test(total.av, 'groups')
total_test
TukeyHSD(total.av)

#plotting damage percent
mod.plant.div <- ggplot(anth_data, aes(fill=groups, y=div.plant, x=groups)) +
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2),drop=FALSE)+ #offsets the xlabesl
  scale_fill_manual(values=coleco)+ #color palette created above
  #ggtitle("Average Percent Damage by Epoch")+ #title
  geom_text(data= data.frame(avganth.all), #lumped modern
            aes(label = c("a","ab","b"),
                y = c(5,5,5), size=2))+
  ylab("Plant Diversity \n(Standarized to 300 leaes)")+ #axis labels
  xlab("")+
  theme_light()+ #light background makes bar plot easier to read
  theme(legend.position = "none") #removes legend
mod.plant.div

#making total super plot to show no difference across all eosystems----
super.total <- plot_grid(mod.total.div, mod.total.perc,
                         nrow = 1,
                         ncol = 2, 
                         labels = c("B", "C"))
super.total

#significant figure
super.signif <- plot_grid(mod.div.mine, 
                          mod.sf.perc,
                          nrow=1,
                          ncol = 2,
                          labels = c("D", "E"))
super.signif

#making full figure for manuscript
top <- plot_grid(mod.plant.div,
                 labels = "A")
fullmodplot <- plot_grid(top,
                      super.total,
                      super.signif,
                      nrow = 2,
                      ncol = 1)

fullmodplot
#Saving figures----
ggsave("figures/surface.pdf", mod.sf.perc)
ggsave("figures/minediv.pdf", mod.div.mine)
ggsave("figures/plant.diversity.pdf", mod.plant.div)
ggsave("figures/totalsuper.pdf", super.total)
ggsave("figures/signifsuper.pdf", super.signif)
ggsave("figures/fullmodplot.pdf", fullmodplot, width = 9, height = 12, units = "in")
