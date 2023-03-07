#_________________________________________________________________________________________#
#Peer J manuscript submission

#"Baseline dynamics of Symbiodiniaceae genera and photochemical 
#efficiency in corals from reefs with different thermal histories"

#McRae et al. 
#submitted November 2022
#revised March 2022

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

PA_plot

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

####<SYM_qPCR>####


####1) Libraries####

library(tidyverse)
library(scales)
library(patchwork)

####2) Data####

#OT PA
OT_PA_long <- read_csv("OT_PA_long.csv")

summary(OT_PA_long)

OT_PA_long$season <- as.factor(OT_PA_long$season)
OT_PA_long$genus <- as.factor(OT_PA_long$genus)
OT_PA_long$colony_id <- as.factor(OT_PA_long$colony_id)
OT_PA_long$line_group <- as.factor(OT_PA_long$line_group)

summary(OT_PA_long)
head(OT_PA_long)

#WT PA
WT_PA_long <- read_csv("WT_PA_long.csv")

summary(WT_PA_long)

WT_PA_long$season <- as.factor(WT_PA_long$season)
WT_PA_long$genus <- as.factor(WT_PA_long$genus)
WT_PA_long$colony_id <- as.factor(WT_PA_long$colony_id)
WT_PA_long$line_group <- as.factor(WT_PA_long$line_group)

summary(WT_PA_long)
head(WT_PA_long)


#OT AN
OT_AN_long <- read_csv("OT_AN_long.csv")

summary(OT_AN_long)

OT_AN_long$season <- as.factor(OT_AN_long$season)
OT_AN_long$genus <- as.factor(OT_AN_long$genus)
OT_AN_long$colony_id <- as.factor(OT_AN_long$colony_id)
OT_AN_long$line_group <- as.factor(OT_AN_long$line_group)

summary(OT_AN_long)
head(OT_AN_long)


#WT AN
WT_AN_long <- read_csv("WT_AN_long.csv")

summary(WT_AN_long)

WT_AN_long$season <- as.factor(WT_AN_long$season)
WT_AN_long$genus <- as.factor(WT_AN_long$genus)
WT_AN_long$colony_id <- as.factor(WT_AN_long$colony_id)
WT_AN_long$line_group <- as.factor(WT_AN_long$line_group)

summary(WT_AN_long)
head(WT_AN_long)


#OT PL
OT_PL_long <- read_csv("OT_PL_long.csv")

summary(OT_PL_long)

OT_PL_long$season <- as.factor(OT_PL_long$season)
OT_PL_long$genus <- as.factor(OT_PL_long$genus)
OT_PL_long$colony_id <- as.factor(OT_PL_long$colony_id)
OT_PL_long$line_group <- as.factor(OT_PL_long$line_group)

summary(OT_PL_long)
head(OT_PL_long)


#WT PL
WT_PL_long <- read_csv("WT_PL_long.csv")

summary(WT_PL_long)

WT_PL_long$season <- as.factor(WT_PL_long$season)
WT_PL_long$genus <- as.factor(WT_PL_long$genus)
WT_PL_long$colony_id <- as.factor(WT_PL_long$colony_id)
WT_PL_long$line_group <- as.factor(WT_PL_long$line_group)

summary(WT_PL_long)
head(WT_PL_long)


####3) Plots####

#OT PA

#set orders
OT_PA_long$season <- factor(OT_PA_long$season , levels=c("Summer", "Fall", "Winter", "Spring"))
OT_PA_long$colony_id <- factor(OT_PA_long$colony_id, levels=c("OT_PA1", "OT_PA2", "OT_PA5", "OT_PA9", "OT_PA10"))

OT_PA.plot <-
  ggplot(data=OT_PA_long, aes(x=cycle, y=value, group=line_group, colour=genus)) +
  geom_line(linewidth = 0.6)+
  geom_point(aes(shape=colony_id, color=genus), size = 1.5)+
  scale_shape_manual(values=c(15, 16, 17, 18, 3))+
  scale_x_continuous(limits=c(0, 35), breaks=c(0, 5, 10, 15, 20, 25, 30, 35))+
  scale_y_continuous(limits=c(-12000, 820000), breaks=c(0, 200000, 400000, 600000, 800000), labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
  scale_color_manual(values=c("grey2", "orchid4")) + theme_bw()+
  labs(title="A.", y="Delta Rn", x="cycle")+
  theme(text=element_text(size=16,  family="serif"))

OT_PA.plot + facet_wrap(~season) 
OT_PA.plot + facet_wrap(~season) + theme(legend.position = "none")




#WT PA

#set orders
WT_PA_long$season <- factor(WT_PA_long$season , levels=c("Summer", "Fall", "Winter", "Spring"))
WT_PA_long$colony_id <- factor(WT_PA_long$colony_id, levels=c("WT_PA3", "WT_PA7"))

WT_PA.plot <-
  ggplot(data=WT_PA_long, aes(x=cycle, y=value, group=line_group, colour=genus)) +
  geom_line(size = 0.6)+
  geom_point(aes(shape=colony_id, color=genus), size = 1.5)+
  scale_shape_manual(values=c(15, 16))+
  scale_x_continuous(limits=c(0, 35), breaks=c(0, 5, 10, 15, 20, 25, 30, 35))+
  scale_y_continuous(limits=c(-12000, 820000), breaks=c(0, 200000, 400000, 600000, 800000), labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
  scale_color_manual(values=c("grey2", "orchid4")) + theme_bw()+
  labs(title="B.", y="Delta Rn", x="cycle")+
  theme(text=element_text(size=16,  family="serif"))

WT_PA.plot + facet_wrap(~season) 
WT_PA.plot + facet_wrap(~season) + theme(legend.position = "none")


#OT AN

#set orders
OT_AN_long$season <- factor(OT_AN_long$season , levels=c("Summer", "Fall", "Winter", "Spring"))
OT_AN_long$colony_id <- factor(OT_AN_long$colony_id, levels=c("OT_AN5", "OT_AN8", "OT_AN9", "OT_AN10", "OT_AN12", "OT_AN13"))

OT_AN.plot <-
  ggplot(data=OT_AN_long, aes(x=cycle, y=value, group=line_group, colour=genus)) +
  geom_line(size = 0.6)+
  geom_point(aes(shape=colony_id, color=genus), size = 1.5)+
  scale_shape_manual(values=c(15, 16, 17, 18, 3, 1))+
  scale_x_continuous(limits=c(0, 35), breaks=c(0, 5, 10, 15, 20, 25, 30, 35))+
  scale_y_continuous(limits=c(-12000, 820000), breaks=c(0, 200000, 400000, 600000, 800000), labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
  scale_color_manual(values=c("grey2", "orchid4")) + theme_bw()+
  labs(title="A.", y="Delta Rn", x="cycle")+
  theme(text=element_text(size=16,  family="serif"))

OT_AN.plot + facet_wrap(~season) 
OT_AN.plot + facet_wrap(~season) + theme(legend.position = "none")


#WT AN

#set orders
WT_AN_long$season <- factor(WT_AN_long$season , levels=c("Summer", "Fall", "Winter", "Spring"))
WT_AN_long$colony_id <- factor(WT_AN_long$colony_id, levels=c("WT_AN1", "WT_AN3", "WT_AN5", "WT_AN6", "WT_AN7", "WT_AN8"))

WT_AN.plot <-
  ggplot(data=WT_AN_long, aes(x=cycle, y=value, group=line_group, colour=genus)) +
  geom_line(size = 0.6)+
  geom_point(aes(shape=colony_id, color=genus), size = 1.5)+
  scale_shape_manual(values=c(15, 16, 17, 18, 3, 1))+
  scale_x_continuous(limits=c(0, 35), breaks=c(0, 5, 10, 15, 20, 25, 30, 35))+
  scale_y_continuous(limits=c(-12000, 820000), breaks=c(0, 200000, 400000, 600000, 800000), labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
  scale_color_manual(values=c("grey2", "orchid4")) + theme_bw()+
  labs(title="B.", y="Delta Rn", x="cycle")+
  theme(text=element_text(size=16,  family="serif"))

WT_AN.plot + facet_wrap(~season) 
WT_AN.plot + facet_wrap(~season) + theme(legend.position = "none")



#OT PL

#set orders
OT_PL_long$season <- factor(OT_PL_long$season , levels=c("Summer", "Fall", "Winter", "Spring"))
OT_PL_long$colony_id <- factor(OT_PL_long$colony_id, levels=c("OT_PL1", "OT_PL4", "OT_PL5", "OT_PL6", "OT_PL8"))

OT_PL.plot <-
  ggplot(data=OT_PL_long, aes(x=cycle, y=value, group=line_group, colour=genus)) +
  geom_line(size = 0.6)+
  geom_point(aes(shape=colony_id, color=genus), size = 1.5)+
  scale_shape_manual(values=c(15, 16, 17, 18, 3))+
  scale_x_continuous(limits=c(0, 35), breaks=c(0, 5, 10, 15, 20, 25, 30, 35))+
  scale_y_continuous(limits=c(-10000, 800000), breaks=c(0, 200000, 400000, 600000, 800000), labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
  scale_color_manual(values=c("grey2", "orchid4")) + theme_bw()+
  labs(title="A.", y="Delta Rn", x="cycle")+
  theme(text=element_text(size=16,  family="serif"))

OT_PL.plot + facet_wrap(~season) 
OT_PL.plot + facet_wrap(~season) + theme(legend.position = "none")



#WT PL

#set orders
WT_PL_long$season <- factor(WT_PL_long$season , levels=c("Summer", "Fall", "Winter", "Spring"))
WT_PL_long$colony_id <- factor(WT_PL_long$colony_id, levels=c("WT_PL1", "WT_PL2", "WT_PL3", "WT_PL4", "WT_PL6", "WT_PL10"))

WT_PL.plot <-
  ggplot(data=WT_PL_long, aes(x=cycle, y=value, group=line_group, colour=genus)) +
  geom_line(size = 0.6)+
  geom_point(aes(shape=colony_id, color=genus), size = 1.5)+
  scale_shape_manual(values=c(15, 16, 17, 18, 3, 1))+
  scale_x_continuous(limits=c(0, 35), breaks=c(0, 5, 10, 15, 20, 25, 30, 35))+
  scale_y_continuous(limits=c(-12000, 820000), breaks=c(0, 200000, 400000, 600000, 800000), labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
  scale_color_manual(values=c("grey2", "orchid4")) + theme_bw()+
  labs(title="B.", y="Delta Rn", x="cycle")+
  theme(text=element_text(size=16,  family="serif"))

WT_PL.plot + facet_wrap(~season) 
WT_PL.plot + facet_wrap(~season) + theme(legend.position = "none")


####_____END______####