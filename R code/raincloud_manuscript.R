#load packages
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(agricolae) #You need this for the Tukey test
library(EnvStats) #rosnerTest
library(ggdist)
library(gghalves)
library(ggridges)

#load data file----
dam_data <- read.csv("Finalized.Database.forests.csv", header=T, sep=",", 
                     na.strings=  "NA", dec=".", strip.white=TRUE)
newdata <- drop_na(dam_data[,c(2,13:21,24:30)])

longdata <- gather(data=newdata, key=ID, value=value, perc.dam:div.plant, 
                   factor_key = TRUE) %>%
  mutate(time = case_when(Epoch == "Anthropocene" ~ "Modern Leaf Packs",
                          TRUE ~ "Fossil")) %>%
  mutate(group = case_when(ID %in% c("div.plant", "Pj", "Shannon", "div.gall", "div.mine", "div.spec", "div.dt") ~ "diversity",
                           ID %in% grep("perc", colnames(newdata), value=TRUE) ~ "frequency"))

#load data + litter data
lit_data <- read.csv("Finalized.Database.forests_litter.added.csv", header=T, sep=",", 
                     na.strings=  "NA", dec=".", strip.white=TRUE)
newdata2 <- lit_data[,c(2,13:21,24:30)]
longdata2 <- gather(data=newdata2, key=ID, value=value, perc.dam:div.plant, 
                   factor_key = TRUE) %>%
  mutate(time = case_when(Epoch == "Anthropocene" ~ "Modern Leaf Packs",
                          Epoch == "Leaf Litter" ~ "Modern Litter",
                          TRUE ~ "Fossil")) %>%
  mutate(group = case_when(ID %in% c("div.plant", "Pj", "Shannon", "div.gall", "div.mine", "div.spec", "div.dt") ~ "diversity",
                           ID %in% grep("perc", colnames(newdata), value=TRUE) ~ "frequency"))
longdata2 <- longdata2[which(longdata2$ID=="perc.dam"),]

#combining longdata dataframes for new figure 06.16.2022
longdata3 <- longdata2[-c(1:64),] 
longdata3 <- full_join(longdata,longdata2)

#rain cloud plot fossil vs. modern
col2 <- c("#219ebc", "#fb8500")
col3 <- c("#219ebc", "#fb8500","#b5838d")
freqlabels <- c("Total Damage", "Specialized Damage", "Mining", "Galling", "Hole Feeding", "Margin Feeding", 
                "Skeletonization", "Surface Feeding", "Piercing & Sucking")

freq_rain <- longdata3 %>%
  filter(group == "frequency") %>%
  ggplot(aes(x = value, y = ID, fill= time,
             color = time))+
  ggdist::stat_halfeye(adjust = 1, # this changes the intervals used for calculating the density plot (bigger=smoother)
                       justification = -0.25,
                       .width = 0,
                       width = 0.8, 
                       alpha = 0.85,
                       point_colour = NA,
                       trim=FALSE, # whether to trim to data range-- looks nicer without trimming, but can be misleading
                       normalize="xy", # this normalizes each line, rather than having them all on the same scale
                       scale=0.6) + # change to adjust height (scale=1 means that they fill the whole line height)
  geom_point(
    size = 1.3,
    alpha = 0.3,
    position = position_jitter(
      seed = 1, width=0.001, height = 0.1
    )
  ) +
  geom_boxplot(width = 0.3,
               outlier.colour = NA, 
               alpha = 0.25,
               position = position_dodge(width=0)) + # this makes the boxplots overlap-- remove if you'd rather they be side-by-side :)
  # ggdist::stat_dots(side= "left",
  #                   justification = 1.15,
  #                   binwidth = 0.5)+
  
  #coord_flip()+
  scale_fill_manual(values=col3)+
  scale_color_manual(values=col3)+
  theme_light()+
  theme(axis.text = element_text(size=14),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=16)) +
  #legend.position = c(0.8,0.9), #add these back in if we want to see the legend 
  #legend.title = element_blank(),
  #legend.text = element_text(size=16),
  coord_cartesian(xlim=c(-10,110),clip="off") +
  scale_y_discrete(labels= freqlabels)+
  theme(legend.position = "none") +
  xlab("Damage Frequency \n(% of leaves)")
freq_rain 

rainleg <- get_legend(freq_rain)

# divlabels <- c("Total", "Specialized", "Mine","Gall", "Shannon", "Pielou's J", 
#                "Plant Species")
divlabels <- c("Total Damage", "Specialized Damage", "Mining DTs","Galling DTs","Plant Species")

longdata4<- longdata %>%
  filter(group == "diversity")%>%
  filter(ID=="div.plant"|ID=="div.gall"|ID=="div.mine"|ID=="div.dt"|ID=="div.spec")

div_rain<- ggplot(longdata4, aes(x = value, y = ID, fill= time, 
             color = time))+
  ggdist::stat_halfeye(adjust = 1, # this changes the intervals used for calculating the density plot (bigger=smoother)
                       justification = -0.25,
                       .width = 0,
                       width = 0.8, 
                       alpha = 0.85,
                       point_colour = NA,
                       trim=FALSE, # whether to trim to data range-- looks nicer without trimming, but can be misleading
                       normalize="xy", # this normalizes each line, rather than having them all on the same scale
                       scale=0.6) + # change to adjust height (scale=1 means that they fill the whole line height)
  geom_point(
    size = 1.3,
    alpha = 0.3,
    position = position_jitter(
      seed = 1, width=0.001, height = 0.1
    )
  ) +
  geom_boxplot(width = 0.3,
               outlier.colour = NA, 
               alpha = 0.25,
               position = position_dodge(width=0)) + # this makes the boxplots overlap-- remove if you'd rather they be side-by-side :)
  # ggdist::stat_dots(side= "left",
  #                   justification = 1.15,
  #                   binwidth = 0.5)+
  
  #coord_flip()+
  scale_fill_manual(values=col2)+
  scale_color_manual(values=col2)+
  theme_light()+
  theme(legend.position = c(0.7,0.3),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        axis.text = element_text(size=14),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=16)) +
  scale_y_discrete(labels= divlabels)+
  coord_cartesian(xlim=c(-10,100),clip="off") +
  theme(legend.position = "none") +
  xlab("Number of DTs or \nPlant Species")
div_rain

#composite plot
rain_super <- plot_grid(freq_rain,
                        div_rain,
                        #rainleg,
                        nrow = 1, 
                        ncol = 2,
                        rel_widths = c(1, 1),
                        labels = c("A", "B"))
rain_super

#statistical comparison of modern to fossil----
#total percent damage
pd.lm <- lm(longdata$value[ID == "perc.dam"]~ longdata$time[ID == "perc.dam"], data = longdata)
summary(pd.lm)
pd.av <- aov(pd.lm)
summary(pd.av)
x<-HSD.test(pd.av, trt = 'longdata$time[ID == "perc.dam"]')
x
#specialized
sp.lm <- lm(longdata$value[ID == "perc.spec"]~ longdata$time[ID == "perc.spec"], data = longdata)
summary(sp.lm)
sp.av <- aov(sp.lm)
summary(sp.av)
x<-HSD.test(sp.av, trt = 'longdata$time[ID == "perc.spec"]')
x
#mining
mine.lm <- lm(longdata$value[ID == "perc.mine"]~ longdata$time[ID == "perc.mine"], data = longdata)
summary(mine.lm)
mine.av <- aov(mine.lm)
summary(mine.av)
x<-HSD.test(mine.av, trt = 'longdata$time[ID == "perc.mine"]')
x
#gall
gall.lm <- lm(longdata$value[ID == "perc.gall"]~ longdata$time[ID == "perc.gall"], data = longdata)
summary(gall.lm)
gall.av <- aov(gall.lm)
summary(gall.av)
x<-HSD.test(gall.av, trt = 'longdata$time[ID == "perc.gall"]')
x
#hole feeding
hf.lm <- lm(longdata$value[ID == "perc.HF"]~ longdata$time[ID == "perc.HF"], data = longdata)
summary(hf.lm)
hf.av <- aov(hf.lm)
summary(hf.av)
x<-HSD.test(hf.av, trt = 'longdata$time[ID == "perc.HF"]')
x
#margin feeding
mf.lm <- lm(longdata$value[ID == "perc.mf"]~ longdata$time[ID == "perc.mf"], data = longdata)
summary(mf.lm)
mf.av <- aov(mf.lm)
summary(mf.av)
x<-HSD.test(mf.av, trt = 'longdata$time[ID == "perc.mf"]')
x
#skel
sk.lm <- lm(longdata$value[ID == "perc.skel"]~ longdata$time[ID == "perc.skel"], data = longdata)
summary(sk.lm)
sk.av <- aov(sk.lm)
summary(sk.av)
x<-HSD.test(sk.av, trt = 'longdata$time[ID == "perc.skel"]')
x
#surface
sf.lm <- lm(longdata$value[ID == "perc.sf"]~ longdata$time[ID == "perc.sf"], data = longdata)
summary(sf.lm)
sf.av <- aov(sf.lm)
summary(sf.av)
x<-HSD.test(sf.av, trt = 'longdata$time[ID == "perc.sf"]')
x
#Piercing
ps.lm <- lm(longdata$value[ID == "perc.pierce"]~ longdata$time[ID == "perc.pierce"], data = longdata)
summary(ps.lm)
ps.av <- aov(ps.lm)
summary(ps.av)
x<-HSD.test(ps.av, trt = 'longdata$time[ID == "perc.pierce"]')
x
########################### Diversity #######################################
#Total
div.lm <- lm(longdata$value[ID == "div.dt"]~ longdata$time[ID == "div.dt"], data = longdata)
summary(div.lm)
div.av <- aov(div.lm)
summary(div.av)
x<-HSD.test(div.av, trt = 'longdata$time[ID == "div.dt"]')
x
#specialized
divspec.lm <- lm(longdata$value[ID == "div.spec"]~ longdata$time[ID == "div.spec"], data = longdata)
summary(divspec.lm)
divspec.av <- aov(divspec.lm)
summary(divspec.av)
x<-HSD.test(divspec.av, trt = 'longdata$time[ID == "div.spec"]')
x
#mine
divmine.lm <- lm(longdata$value[ID == "div.mine"]~ longdata$time[ID == "div.mine"], data = longdata)
summary(divmine.lm)
divmine.av <- aov(divmine.lm)
summary(divmine.av)
x<-HSD.test(divmine.av, trt = 'longdata$time[ID == "div.mine"]')
x
#gall
divgall.lm <- lm(longdata$value[ID == "div.gall"]~ longdata$time[ID == "div.gall"], data = longdata)
summary(divgall.lm)
divgall.av <- aov(divgall.lm)
summary(divgall.av)
x<-HSD.test(divgall.av, trt = 'longdata$time[ID == "div.gall"]')
x
#Shannon
divshan.lm <- lm(longdata$value[ID == "Shannon"]~ longdata$time[ID == "Shannon"], data = longdata)
summary(divshan.lm)
divshan.av <- aov(divshan.lm)
summary(divshan.av)
x<-HSD.test(divshan.av, trt = 'longdata$time[ID == "Shannon"]')
x
#PJ
divpj.lm <- lm(longdata$value[ID == "Pj"]~ longdata$time[ID == "Pj"], data = longdata)
summary(divpj.lm)
divpj.av <- aov(divpj.lm)
summary(divpj.av)
x<-HSD.test(divpj.av, trt = 'longdata$time[ID == "Pj"]')
x
#Plant species
divplant.lm <- lm(longdata$value[ID == "div.plant"]~ longdata$time[ID == "div.plant"], data = longdata)
summary(divplant.lm)
divplant.av <- aov(divplant.lm)
summary(divplant.av)
x<-HSD.test(divplant.av, trt = 'longdata$time[ID == "div.plant"]')
x

#comparing total herbivory across fossil --> litter --> modern 
col3 <- c("#219ebc", "#b5838d", "#fb8500")
freqlabels2 <- c("Total Damage")

longdata2$time <- factor(longdata2$time, 
                        levels = c("Fossil", "Modern Litter", "Modern Leaf Packs"))
longdata2 <- arrange(longdata2, time)

freq_rain.lit <- longdata2 %>%
  filter(group == "frequency") %>%
  ggplot(aes(x = value, y = ID, fill= time,
             color = time))+
  ggdist::stat_halfeye(adjust = 1, # this changes the intervals used for calculating the density plot (bigger=smoother)
                       justification = -0.25,
                       .width = 0,
                       width = 0.8, 
                       point_colour = NA,
                       trim=FALSE, # whether to trim to data range-- looks nicer without trimming, but can be misleading
                       normalize="xy", # this normalizes each line, rather than having them all on the same scale
                       scale=0.6) + # change to adjust height (scale=1 means that they fill the whole line height)
  geom_point(
    size = 1.3,
    alpha = 0.3,
    position = position_jitter(
      seed = 1, width=0.001, height = 0.1
    )
  ) +
  geom_boxplot(width = 0.3,
               outlier.colour = NA, 
               alpha = 0.25,
               position = position_dodge(width=0)) + # this makes the boxplots overlap-- remove if you'd rather they be side-by-side :)
  # ggdist::stat_dots(side= "left",
  #                   justification = 1.15,
  #                   binwidth = 0.5)+
  
  #coord_flip()+
  scale_fill_manual(values=col3)+
  scale_color_manual(values=col3)+
  theme_light()+
  theme(axis.text = element_text(size=14),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=16),
        legend.position = c(0.1,0.85), #add these back in if we want to see the legend
        legend.title = element_blank(),
        legend.text = element_text(size=10))+
  coord_cartesian(xlim=c(-10,110),clip="off") +
  scale_y_discrete(labels= freqlabels2)+
  #theme(legend.position = "none") +
  xlab("Frequency of Damage")
freq_rain.lit 

#statistically testing fossil, modern, litt----
all.lm <- lm(longdata2$value[ID == "perc.dam"]~ longdata2$time[ID == "perc.dam"], data = longdata2)
summary(all.lm)
all.av <- aov(all.lm)
summary(all.av)
total_test <- HSD.test(all.av, 'longdata2$time[ID == "perc.dam"]')
total_test
TukeyHSD(all.av)

#manuscript figure ----
freq_rain.lit <- plot_grid(freq_rain.lit,
                           labels = "A")

full.rain.super <- plot_grid(freq_rain.lit,
                             rain_super,
                             nrow = 2, 
                             ncol = 1,
                             rel_widths = c(1, 1), 
                             rel_heights = c(1,2))
full.rain.super

#saving figures----
ggsave("figures/rain_super.alpha.pdf", rain_super, width = 18, height = 16, units = "in")
ggsave("figures/freq_rain.lit.pdf", freq_rain.lit, width = 7, units = "in")
ggsave("figures/full.rain.super.pdf", full.rain.super, height=20, width = 17, units = "in")









