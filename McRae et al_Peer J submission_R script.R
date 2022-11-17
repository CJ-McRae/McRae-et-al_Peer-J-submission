#_________________________________________________________________________________________#
#Peer J manuscript submission

#"Baseline dynamics of Symbiodiniaceae genera and photochemical 
#efficiency in corals from reefs with different thermal histories"

#McRae et al. 
#submitted November 2022

#Contact information: 
#Dr. Crystal J. McRae 
#crystal.j.mcrae@gmail.com


#Species abbreviations:
#PA = Pocillopora acuta
#AN = Acropora nana
#PL = Porites lutea

#Site abbreviations:
#WT = Wanlitung reef
#OT = Outlet reef
#_________________________________________________________________________________________#


#clear workspace
rm(list = ls())


####___________####

####<TEMPERATURE>####

####1) Libraries####

library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)

####2) Data####

raw_temp <- read_csv("TEMP.csv")


#class check & organize the data

#Separate month, day, hour, and minute
TEMPDATA <-
  raw_temp %>%
  mutate(Date_Time = mdy_hm(raw_temp$DATETIME)) %>% 
  mutate(year= year(Date_Time), 
         month = month(Date_Time),
         day = day(Date_Time), 
         hour = hour(Date_Time), 
         minute = minute(Date_Time))

#Define seasons
DATA <-
  TEMPDATA%>%
  mutate(month_conversion = ifelse(year== 2018, -5, 7))%>%
  mutate(MONTHID = month + month_conversion)%>%
  mutate(
    season = case_when(
      MONTHID %in% 10:12 ~ "spring",
      MONTHID %in%  4:6  ~ "fall",
      MONTHID %in%  7:9  ~ "winter",
      MONTHID %in%  13:15  ~ "ssummer",
      TRUE ~ "summer"))


#create separate date column for plotting
DATA <- tidyr::separate(DATA, 'DATETIME',
                        into = c('date', 'time'),
                        sep= ' ')


DATA$date <- as.Date (DATA$date,"%m/%d/%Y")
DATA$SITE <- as.factor (DATA$SITE)
DATA$month <- as.integer (DATA$month)
DATA$MONTHID <- as.integer (DATA$MONTHID)
DATA$season <- as.factor (DATA$season)

str(DATA)
summary(DATA)
head(DATA)


#Monthly summaries

month_sum <-
  DATA%>%
  group_by(month, SITE, year)%>%
  summarise(TEMP_mean = mean(TEMP), TEMP_sd =sd(TEMP))

month_var <-
  DATA%>%
  group_by(month, SITE, year)%>%
  summarise(TEMP_max = max(TEMP), TEMP_min =min(TEMP))


#Daily summaries

day_sum <-
  DATA%>%
  group_by(date, SITE, month, year, season, MONTHID)%>%
  summarise(TEMP_mean = mean(TEMP), TEMP_sd =sd(TEMP), TEMP_max=max(TEMP), TEMP_min = min(TEMP))%>%
  mutate(day_range = TEMP_max - TEMP_min)

#Average of daily values
day_sum_short<-
  day_sum%>%
  group_by(month, year, SITE, season, MONTHID)%>%
  summarise(day_range = mean(day_range), meanday_max =mean (TEMP_max), meanday_min = mean(TEMP_min), meanday = mean(TEMP_mean), meanday_sd = mean(TEMP_sd))




####3) Plots####

#first make subsets for each site

OT_TEMP <-
  DATA%>%
  filter(SITE =="OT")

head(OT_TEMP)
tail(OT_TEMP)

WT_TEMP <-
  DATA%>%
  filter(SITE =="WT")

head(WT_TEMP)
tail(WT_TEMP)



####*Outlet_10min data####

OT_plot <- ggplot(data=OT_TEMP, 
                  aes(x=date, y=TEMP)) +
  geom_line(size=0.05, colour='darkgrey')+
  theme_classic()+
  theme(text=element_text(size=14,  family="serif"))+
  geom_hline(yintercept=30, linetype="dashed", 
             color = "black", size=0.5)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  scale_y_continuous(limits=c(22, 34), breaks=c(22, 24, 26, 28, 30, 32, 34))+
  labs(title="Outlet reef (warmed and thermally variable site)", y="Temperature (?C)", x="Month")

OT_plot 


####*Wanlitung_10min data####

WT_plot <- ggplot(data=WT_TEMP, 
                  aes(x=date, y=TEMP)) +
  geom_line(size=0.05, colour='steelblue')+
  theme_classic()+
  theme(text=element_text(size=14,  family="serif"))+
  geom_hline(yintercept=30, linetype="dashed", 
             color = "black", size=0.5)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  scale_y_continuous(limits=c(22, 34), breaks=c(22, 24, 26, 28, 30, 32, 34))+
  labs(title="Wanlitung reef (thermally stable site)", y="Temperature (?C)", x="Month")

WT_plot 



####*daily mean####
daymean_plot <- 
  ggplot(data=day_sum, aes(x=date, y=TEMP_mean, colour=SITE)) +
  geom_smooth()+
  theme_bw()+
  scale_color_manual(values=c("darkgrey", "steelblue")) + theme_classic()+
  theme(axis.text.x = element_text(angle=0, margin = margin(t=5, r=50)))+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  labs(title="Mean daily temperature", y="Temperature (?C)", x="")+
  theme(text=element_text(size=14,  family="serif"))

daymean_plot +   theme(legend.position = "none")


#####*daily range####

daymean_range_plot <- 
  ggplot(data=day_sum, aes(x=date, y=day_range, colour=SITE)) +
  geom_smooth()+
  theme_bw()+
  scale_color_manual(values=c("darkgrey", "steelblue")) + theme_classic()+
  theme(axis.text.x = element_text(angle=0, margin = margin(t=5, r=50)))+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  labs(title="Daily temperature range", y="Temperature (?C)", x="")+
  theme(text=element_text(size=14,  family="serif"))

daymean_range_plot  + theme(legend.position = "none")


#####*mean daily_max####
daymean_max_plot <- 
  ggplot(data=day_sum, aes(x=date, y=TEMP_max, colour=SITE)) +
  geom_smooth()+
  theme_bw()+
  scale_y_continuous(limits=c(21, 34), breaks=c(24, 28, 32))+
  scale_color_manual(values=c("darkgrey", "steelblue")) + theme_classic()+
  theme(axis.text.x = element_text(angle=0, margin = margin(t=5, r=50)))+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  labs(title="Daily temperature maxima", y="Temperature (?C)", x="")+
  theme(text=element_text(size=14,  family="serif"))

daymean_max_plot + theme(legend.position = "none")



####*mean daily_min####
daymean_min_plot <- 
  ggplot(data=day_sum, aes(x=date, y=TEMP_min, colour=SITE)) +
  geom_smooth()+
  theme_bw()+
  scale_y_continuous(limits=c(21, 34), breaks=c(24, 28, 32))+
  scale_color_manual(values=c("darkgrey", "steelblue")) + theme_classic()+
  theme(axis.text.x = element_text(angle=0, margin = margin(t=5, r=50)))+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  labs(title="Daily temperature minima", y="Temperature (?C)", x="")+
  theme(text=element_text(size=14,  family="serif"))

daymean_min_plot + theme(legend.position = "none")



####4) Analysis####

####*site: mean sd####

#OT
OT.meansd<-
  OT_TEMP%>%
  summarise(mean.ot = mean(TEMP), sd.ot = sd(TEMP))

OT.meansd

#WT
WT.meansd<-
  WT_TEMP%>%
  summarise(mean.wt = mean(TEMP), sd.wt = sd(TEMP))

WT.meansd


####*daily mean mixed model####

meanday.mm <- 
  lmer(TEMP_mean~SITE*season + (1|MONTHID), data=day_sum)

summary(meanday.mm)


#check assumptions
plot(fitted(meanday.mm),residuals(meanday.mm))
hist(residuals(meanday.mm ))
qqnorm(residuals(meanday.mm ))
vif(meanday.mm)


#ACF 
acf(residuals(meanday.mm))


#posthoc
emmeans(meanday.mm, pairwise~SITE*season)


####*daily mean linear model####
#just to take a look at ACF if month is not used as a random effect

my_lm <- 
  lm(TEMP_mean~SITE*season, data=day_sum)

summary(my_lm)


#check assumptions
plot(fitted(my_lm),residuals(my_lm))
hist(residuals(my_lm))
qqnorm(residuals(meanday.mm ))
vif(my_lm)

#ACF 
acf(residuals(my_lm))



####*daily range mixed model####

dayrange.mm <- 
  lmer(day_range~SITE*season + (1|MONTHID), data=day_sum)

summary(dayrange.mm)

#check assumptions
plot(fitted(dayrange.mm),residuals(dayrange.mm))
hist(residuals(dayrange.mm))
qqnorm(residuals(dayrange.mm))
vif(dayrange.mm)

#ACF 
acf(residuals(dayrange.mm))


#posthoc
emmeans(dayrange.mm, pairwise~SITE*season)


####*daily max mixed model####

daymax.mm <- 
  lmer(TEMP_max~SITE*season + (1|MONTHID), data=day_sum)

summary(daymax.mm)

#check assumptions
plot(fitted(daymax.mm),residuals(daymax.mm))
hist(residuals(daymax.mm))
qqnorm(residuals(daymax.mm))
vif(daymax.mm)

#ACF 
acf(residuals(daymax.mm))


#posthoc
emmeans(daymax.mm, pairwise~SITE*season)


####*daily min mixed model####

daymin <- 
  lmer(TEMP_min~SITE*season + (1|MONTHID), data=day_sum)

summary(daymin)


#check assumptions
plot(fitted(daymin),residuals(daymin))
hist(residuals(daymin))
qqnorm(residuals(daymin))
vif(daymin)

#ACF 
acf(residuals(daymin))


#posthoc
emmeans(daymin, pairwise~SITE*season)


####___________####

####<NUTRIENTS>#####

####1) Libraries####

library(tidyverse)
library(car)

####2) Data####

NUT_data <- read_csv("NUT.csv")

#class check
NUT_data$Date <- as.Date(NUT_data$Date)
NUT_data$Site <- as.factor(NUT_data$Site)

summary(NUT_data)
head(NUT_data)

#WT subset

NUT_WT <-
  NUT_data%>%
  filter(Site =='WT')


#OT subset

NUT_OT <-
  NUT_data%>%
  filter(Site =='OT')


####3) Plots####

boxplot(BOD5~Site, data=NUT_data)
boxplot(NO3N~Site, data=NUT_data)
boxplot(NO2N~Site, data=NUT_data)
boxplot(NH3N~Site, data=NUT_data)
boxplot(PO4P~Site, data=NUT_data)


####4) Analysis####

####*B0D5####

#test for normality
hist(NUT_OT$BOD5)
shapiro.test(NUT_OT$BOD5)


hist(NUT_WT$BOD5)
shapiro.test(NUT_WT$BOD5)


#test for equal variance
leveneTest(BOD5~Site, data=NUT_data)

#test for difference between sites
wilcox.test(BOD5~Site, data=NUT_data)



####*NO2####


#test for normality
hist(NUT_OT$NO2N)
shapiro.test(NUT_OT$NO2N)


hist(NUT_WT$NO2N)
shapiro.test(NUT_WT$NO2N)

#test for equal variance
leveneTest(NO2N~Site, data=NUT_data)

#test for difference between sites
wilcox.test(NO2N~Site, data=NUT_data)



####*NO3####


#test for normality
hist(NUT_OT$NO3N)
shapiro.test(NUT_OT$NO3N)


hist(NUT_WT$NO3N)
shapiro.test(NUT_WT$NO3N)

#test for equal variance
leveneTest(NO3N~Site, data=NUT_data)

#test for difference between sites
t.test(NO3N~Site, data=NUT_data, var.equal=TRUE)



####*NH3####


#test for normality
hist(NUT_OT$NH3N)
shapiro.test(NUT_OT$NH3N)


hist(NUT_WT$NH3N)
shapiro.test(NUT_WT$NH3N)

#test for equal variance
leveneTest(NH3N~Site, data=NUT_data)


#test for difference between sites
wilcox.test(NH3N~Site, data=NUT_data)


####*PO4####


#test for normality
hist(NUT_OT$PO4P)
shapiro.test(NUT_OT$PO4P)


hist(NUT_WT$PO4P)
shapiro.test(NUT_WT$PO4P)

#test for equal variance
leveneTest(PO4P~Site, data=NUT_data)

#test for difference between sites
wilcox.test(PO4P~Site, data=NUT_data)


#Mean and sd
NUT_summary <-
  NUT_data%>%
  group_by(Site)%>%
  summarise(BOD5.mean = mean(BOD5), BOD5.sd = sd(BOD5), 
            NO2N.mean = mean(NO2N), NO2N.sd = sd(NO2N), 
            NO3N.mean = mean(NO3N), NO3N.sd = sd(NO3N),
            NH3N.mean = mean(NH3N), NH3N.sd = sd(NH3N),
            PO4P.mean = mean(PO4P), PO4P.sd = sd(PO4P))

NUT_summary


####___________####

####<FV/FM>####

####1) Libraries####

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)


####2) Data####

#All species and sites
FVFM_ALL <- read_csv("FVFM.ALL.csv")

#PA subset
FVFM_PA <-
  FVFM_ALL%>%
  filter(SPECIES =='PA')

#AN subset
FVFM_AN <-
  FVFM_ALL%>%
  filter(SPECIES =='AN')

#PL subset
FVFM_PL <-
  FVFM_ALL%>%
  filter(SPECIES =='PL')


#all class check

#FVFM_ALL
FVFM_ALL$COL.ID <- as.factor (FVFM_ALL$COL.ID)
FVFM_ALL$SEASON <- as.factor (FVFM_ALL$SEASON)
FVFM_ALL$SITE <- as.factor (FVFM_ALL$SITE) 
FVFM_ALL$SUBSET <- as.factor (FVFM_ALL$SUBSET)
FVFM_ALL$FVFM<- as.numeric (FVFM_ALL$FVFM)
FVFM_ALL$SPECIES <-as.factor(FVFM_ALL$SPECIES)

summary(FVFM_ALL)
str(FVFM_ALL)
head(FVFM_ALL)


#subset class checks

#FVFM_PA
FVFM_PA$COL.ID <- as.factor (FVFM_PA$COL.ID)
FVFM_PA$SEASON <- as.factor (FVFM_PA$SEASON)
FVFM_PA$SITE <- as.factor (FVFM_PA$SITE) 
FVFM_PA$SUBSET <- as.factor (FVFM_PA$SUBSET)
FVFM_PA$FVFM<- as.numeric (FVFM_PA$FVFM)
FVFM_PA$SPECIES <-as.factor(FVFM_PA$SPECIES)

summary(FVFM_PA)
str(FVFM_PA)
head(FVFM_PA)


#FVFM_AN
FVFM_AN$COL.ID <- as.factor (FVFM_AN$COL.ID)
FVFM_AN$SEASON <- as.factor (FVFM_AN$SEASON)
FVFM_AN$SITE <- as.factor (FVFM_AN$SITE) 
FVFM_AN$SUBSET <- as.factor (FVFM_AN$SUBSET)
FVFM_AN$FVFM<- as.numeric (FVFM_AN$FVFM)
FVFM_AN$SPECIES <-as.factor(FVFM_AN$SPECIES)

summary(FVFM_AN)
str(FVFM_AN)
head(FVFM_AN)


#FVFM_PL
FVFM_PL$COL.ID <- as.factor (FVFM_PL$COL.ID)
FVFM_PL$SEASON <- as.factor (FVFM_PL$SEASON)
FVFM_PL$SITE <- as.factor (FVFM_PL$SITE) 
FVFM_PL$SUBSET <- as.factor (FVFM_PL$SUBSET)
FVFM_PL$FVFM<- as.numeric (FVFM_PL$FVFM)
FVFM_PL$SPECIES <-as.factor(FVFM_PL$SPECIES)

summary(FVFM_PL)
str(FVFM_PL)
head(FVFM_PL)



####3) Plots####

PA_plot<-
  ggplot(data=FVFM_PA, aes(x=SEASON, y=FVFM, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE, fill=SITE), 
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75), 
             size=1.5) +
  scale_shape_manual(values = c(1, 16)) +
  scale_y_continuous(limits=c(0.4, 0.85), breaks=c(.4, .45,.5,.55, .6,.65, .7,.75, .8, .85))+
  scale_fill_manual(breaks = c("Outlet", "Wanlitung"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Maximum quantum yeild (Fv/Fm)", x="Season", title="PA")+
  theme(text=element_text(size=14,  family="sans"))

PA_plot + theme(legend.position = "none")


AN_plot<-
  ggplot(data=FVFM_AN, aes(x=SEASON, y=FVFM, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE, fill=SITE), 
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75), 
             size=1.5) +
  scale_shape_manual(values = c(1, 16)) +
  scale_y_continuous(limits=c(0.4, 0.85), breaks=c(.4, .45,.5,.55, .6,.65, .7,.75, .8, .85))+
  scale_fill_manual(breaks = c("Outlet", "Wanlitung"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Maximum quantum yeild (Fv/Fm)", x="Season", title="AN")+
  theme(text=element_text(size=14,  family="sans"))

AN_plot + theme(legend.position = "none")


PL_plot<-
  ggplot(data=FVFM_PL, aes(x=SEASON, y=FVFM, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE, fill=SITE), 
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75), 
             size=1.5) +
  scale_shape_manual(values = c(1, 16)) +
  scale_y_continuous(limits=c(0.4, 0.85), breaks=c(.4, .45,.5,.55, .6,.65, .7,.75, .8, .85))+
  scale_fill_manual(breaks = c("Outlet", "Wanlitung"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Maximum quantum yeild (Fv/Fm)", x="Season", title="PL")+
  theme(text=element_text(size=14,  family="sans"))

PL_plot + theme(legend.position = "none")




####4) Analysis####


#mixed model
ALL.FVFMint <- lmer(FVFM ~ SITE * SEASON * SPECIES + (1|COL.ID), data=FVFM_ALL)
summary(ALL.FVFMint)

#check assumptions
plot(fitted(ALL.FVFMint),residuals(ALL.FVFMint))
hist(residuals(ALL.FVFMint))
qqnorm(residuals(ALL.FVFMint))
vif(ALL.FVFMint)


#post hoc

#comparing site differences for each species across seasons
FVFM_posthoc1 <- emmeans(ALL.FVFMint, pairwise~SITE | SPECIES | SEASON)
FVFM_posthoc1

#comparing species differences for each site across seasons
FVFM_posthoc2 <- emmeans(ALL.FVFMint, pairwise~SPECIES | SEASON | SITE)
FVFM_posthoc2

#comparing seasonal differences for each site across seasons
FVFM_posthoc3 <- emmeans(ALL.FVFMint, pairwise~ SEASON | SPECIES | SITE)
FVFM_posthoc3



####___________####

####<SYM_FV/FM>####


####1) Libraries####

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)


####2) Data####


sym_fvfm <- read_csv("sym_fvfm.csv")

#class check
summary(sym_fvfm)

sym_fvfm$GENUS <-as.factor(sym_fvfm$GENUS)
sym_fvfm$SEASON <-as.factor(sym_fvfm$SEASON)
sym_fvfm$SPECIES <-as.factor(sym_fvfm$SPECIES)
sym_fvfm$SITE <-as.factor(sym_fvfm$SITE)

summary(sym_fvfm)
head(sym_fvfm)

####3) Plots####

DATA_NO.NA <- na.omit(sym_fvfm) 

#set order for x axis
DATA_NO.NA$GENUS <- factor(DATA_NO.NA$GENUS, 
                                      levels=c("C", "CD", "DC", "D"))

sym_fvfm.plot <- ggplot(DATA_NO.NA,aes(x=GENUS,y=FVFM, fill=SPECIES))+
  geom_jitter(aes(colour=SITE, shape = SPECIES), size=3, width=0.2)+
  scale_colour_manual(breaks = c("Outlet", "Wanlitung"), 
                      values=c("darkgrey", "steelblue"))+
  theme(text=element_text(size=14,  family="serif"))+
  theme_classic()


sym_fvfm.plot

sym_fvfm.plot + theme(legend.position = "none")


####4) Analysis####

#mixed model
fvfm_genus <- lmer(FVFM ~ GENUS + (1|COL.ID) + (1|SEASON), data=DATA_NO.NA)
summary(fvfm_genus)

#check assumptions
plot(fitted(fvfm_genus),residuals(fvfm_genus))
hist(residuals(fvfm_genus))
qqnorm(residuals(fvfm_genus))


#posthoc
fvfm_genus_posthoc <- emmeans(fvfm_genus, pairwise~GENUS)
fvfm_genus_posthoc

#mean and sd

sym_genus_summary <-
  DATA_NO.NA %>%
  group_by(GENUS)%>%
  summarise(mean = mean(FVFM), sd= sd(FVFM))

sym_genus_summary


####___________####

####<SYM>####

####1) Libraries####

library(tidyverse)

####2) Data####

SYM_PA <- read_csv("PA_SYM.csv")
SYM_AN <- read_csv("AN_SYM.csv")
SYM_PL <- read_csv("PL_SYM.csv")


#Class checks

#PA
SYM_PA$SEASON <- as.factor(SYM_PA$SEASON)
SYM_PA$GENUS <- as.factor(SYM_PA$GENUS)

#AN
SYM_AN$SEASON <- as.factor(SYM_AN$SEASON)
SYM_AN$GENUS <- as.factor(SYM_AN$GENUS)


#PL
SYM_PL$SEASON <- as.factor(SYM_PL$SEASON)
SYM_PL$GENUS <- as.factor(SYM_PL$GENUS)



####3) Plots####

####*PA####

PA.sym.plot <-
  ggplot(SYM_PA, aes(y=PROP, x=SEASON, fill=GENUS)) + 
  geom_bar(position="fill", stat="identity")+
  facet_wrap(~SITE)+
  labs(y = "Proportion of colonies", x="", title ="PA")+
  theme(text=element_text(size=14,  family="serif"))+
  theme_bw()

col_list<-c("snow3", "snow4", "black") 

PA.sym.plot + 
  scale_colour_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  theme(legend.position = "none")



####*AN####

AN.sym.plot <-
  ggplot(SYM_AN, aes(y=PROP, x=SEASON, fill=GENUS)) + 
  geom_bar(position="fill", stat="identity")+
  facet_wrap(~SITE)+
  labs(y = "Proportion of colonies", x="", title ="AN")+
  theme(text=element_text(size=14,  family="serif"))+
  theme_bw()

col_list<-c("snow3", "snow4", "black") 

AN.sym.plot + 
  scale_colour_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  theme(legend.position = "none")


####*PL####

PL.sym.plot <-
  ggplot(SYM_PL, aes(y=PROP, x=SEASON, fill=GENUS)) + 
  geom_bar(position="fill", stat="identity")+
  facet_wrap(~SITE)+
  labs(y = "Proportion of colonies", x="", title ="PL")+
  theme(text=element_text(size=14,  family="serif"))+
  theme_bw()

col_list<-c("snow3", "snow4", "black") 

PL.sym.plot + 
  scale_colour_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  theme(legend.position = "none")



####_____END______####