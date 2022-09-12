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

dam_data <- read.csv("Finalized.Database.forests_litter.added2.csv", header=T, sep=",", 
                     na.strings=  "NA", dec=".", strip.white=TRUE)

#newdata <- drop_na(dam_data[,c(2,7,13:21,24:30)])
fdata <- dam_data %>%
  mutate(time = case_when(Epoch == "Recent" ~ "Recent",
                          TRUE ~ "Fossil")) %>%
  filter(time == "Fossil") %>%
  mutate(cstate = case_when(Flora == "Hubble Bubble" ~ "PETM",
                            Flora %in% c("Fifteenmile Creek","Wind River Interior",
                                         "Laguna del Hunco 6","Laguna del Hunco 13", 
                                         "Laguna del Hunco 2", "Laguna del Hunco 4") ~ "EECO",
                            TRUE ~ "NA"))
mdata <- dam_data %>%
  mutate(time = case_when(Epoch == "Recent" ~ "Recent",
                          TRUE ~ "Fossil")) %>%
  filter(time == "Recent") 

total.perc <- drop_na(dam_data[,c(2,7,13)])#percent total damage
total.div <- drop_na(dam_data[,c(2,7,24)]) #total diversity
#following FFG same organization as above
spec.perc <- drop_na(dam_data[,c(2,7,14)]) 
spec.div <- drop_na(dam_data[,c(2,7,25)]) 
mine.perc <- drop(dam_data[,c(2,7,15)])
mine.div <- drop(dam_data[,c(2,7,26)])
gall.perc <- drop_na(dam_data[,c(2,7,16)])
gall.div <- drop(dam_data[,c(2,7,27)])

HF.perc <- drop_na(dam_data[,c(2,7,17)])
mf.perc <- drop_na(dam_data[,c(2,7,18)])
skel.perc <- drop_na(dam_data[,c(2,7,19)])
sf.perc <- drop_na(dam_data[,c(2,7,20)])
p.perc <- drop_na(dam_data[,c(2,7,21)])

plant.div <- drop_na(dam_data[,c(2,7,30)])

#GLMs all data included----
lmod1<-glm(perc.dam ~ MAT, family=gaussian(link="identity"),dat=total.perc[total.perc$MAT < 80,])
summary(lmod1)

summary(glm(div.dt ~ MAT, family=gaussian(link="identity"),dat=total.div))

lmod2 <-glm(perc.mine ~ MAT, family=gaussian(link="identity"),dat=mine.perc)
summary(lmod2) 

lmod3 <-glm(div.mine ~ MAT, family=gaussian(link="identity"),dat=mine.div)
summary(lmod3) 

lmod4 <-glm(div.gall ~ MAT, family=gaussian(link="identity"),dat=gall.div)
summary(lmod4) 

summary(glm(div.spec ~ MAT, family=gaussian(link="identity"),dat=spec.div)) #specialized 

#GLMs of fossil data only----
fmod1 <- glm(perc.dam ~ MAT, family = gaussian(link = "identity"), data = fdata)
summary(fmod1)

fmod2 <- glm(div.dt ~ MAT, family = gaussian(link = "identity"), data = fdata)
summary(fmod2)

fmod3 <- glm(perc.spec ~ MAT, family = gaussian(link = "identity"), data = fdata)
summary(fmod3) #not significant

fmod4 <- glm(div.spec ~MAT, family = gaussian(link = "identity"), data = fdata)
summary(fmod4)

#Each individual epoch by MAT----
cretdata <- fdata %>%
  filter(Epoch == "Cretaceous")
cretmod <- glm(perc.spec ~ MAT, family = gaussian(link= "identity"), data = cretdata)
summary(cretmod) #no metrics significant

paleodata <- fdata %>%
  filter(Epoch == "Paleocene")
paleomod <- glm(perc.spec ~ MAT, family = gaussian(link= "identity"), data = paleodata)
summary(paleomod)#no metrics are sig

eodata <- fdata %>%
  filter(Epoch == "Eocene")
eomod <- glm(perc.spec ~ MAT, family = gaussian(link= "identity"), data = eodata)
summary(eomod) #only total and div. spec are significant

olidata <- fdata %>%
  filter(Epoch == "Oligocene")
olimod <- glm(perc.spec ~ MAT, family = gaussian(link= "identity"), data = olidata)
summary(olimod) #spec div and freq are almost signif (0.07)

miodata <- fdata %>%
  filter(Epoch == "Miocene")
miomod <- glm(perc.spec ~ MAT, family = gaussian(link= "identity"), data = miodata)
summary(miomod) #only spec freq is signif

#only one or two datapoins so can't be measured. 
# pliodata <- fdata %>%
#   filter(Epoch == "Pliocene")
# pliomod <- glm(perc.dam ~ MAT, family = gaussian(link= "identity"), data = pliodata)
# summary(pliomod) #not signif
# 
# pleidata <- fdata %>%
#   filter(Epoch == "Pleistocene")
# pleimod <- glm(perc.dam ~ MAT, family = gaussian(link= "identity"), data = pleidata)
# summary(pleimod) #not significant

# #modern data only ----
m.mod1 <- glm(perc.spec ~ MAT, family = gaussian(link = "identity"), data = mdata)
summary(m.mod1)

#plotting----
mdata[mdata$MAT < 80,] %>%
ggplot(aes(x=MAT, y=div.dt, color=Epoch)) +
  geom_point(size=6) +
  geom_smooth(method=glm , color="grey", fill="grey", se=TRUE) +
  scale_color_manual(values=col7)+
  #coord_cartesian(ylim=c(0,90)) +
  theme_bw()+
  theme(legend.position="none")

#plotting
col8 <- c("#7209b7","#81B29A","#5F797B","#3D405B","#8F5D5D","#E07A5F","#c89f9c", "#F2CC8F")
#"Cretaceous", "Paleocene", "Eocene","Oligocene","Miocene","Pliocene","Pleistocene", "Recent"
col7 <- c("#81B29A","#5F797B","#3D405B","#8F5D5D","#E07A5F","#c89f9c","#F2CC8F")
#"Cretaceous", "Paleocene", "Eocene","Oligocene","Miocene","Pliocene","Pleistocene"

total.perc$Epoch <- factor(total.perc$Epoch, 
                           levels = c("Recent", "Pleistocene", "Pliocene", "Miocene", 
                                      "Oligocene", "Eocene", "Paleocene", "Cretaceous"))
total.perc <- arrange(total.perc, Epoch)

#fossil data
fdata$Epoch <- factor(fdata$Epoch, 
                           levels = c("Pleistocene", "Pliocene", "Miocene", 
                                      "Oligocene", "Eocene", "Paleocene", "Cretaceous"))
fdata <- arrange(fdata, Epoch)
fdata$cstate <- factor(fdata$cstate, levels = c("NA", "PETM", "EECO"))
fdata <- arrange(fdata, cstate)

#fossil data only
plotfmod1 <- ggplot(fdata, aes(x=MAT, y=perc.dam, color=Epoch)) +
  geom_point(aes(shape = cstate), size=6) +
  geom_smooth(method=glm , color="grey", fill="grey", se=TRUE) +
  scale_color_manual(values=col7)+
  coord_cartesian(ylim=c(0,90)) +
  theme_bw()+
  theme(legend.position="none")
plotfmod1
flegend <- get_legend(plotfmod1)
#rerun above plot without legend

plotfmod2 <- ggplot(fdata, aes(x=MAT, y=div.dt, color=Epoch)) +
  geom_point(aes(shape = cstate), size=6) +
  geom_smooth(method=glm , color="grey", fill="grey", se=TRUE) +
  scale_color_manual(values=col7)+
  coord_cartesian(ylim=c(0,40)) +
  theme_bw()+
  theme(legend.position = "none") 
plotfmod2 

plotfmod4 <- ggplot(fdata, aes(x=MAT, y=div.spec, color=Epoch)) +
  geom_point(aes(shape = cstate), size=6) +
  geom_smooth(method=glm , color="grey", fill="grey", se=TRUE) +
  scale_color_manual(values=col7)+
  coord_cartesian(ylim=c(0,40)) +
  theme_bw()+
  theme(legend.position = "none") 
plotfmod4







