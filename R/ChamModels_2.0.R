# load libraries
devtools::install_github("karthik/wesanderson")
pacman::p_load(lme4, lmerTest, ggplot2, lsmeans, emmeans, multcomp, dplyr, wesanderson)


### CONSPICUOUSNESS OF DISPLAY MODELS

# Load Dataset - Display_1
setwd("~/Dropbox/chameleon/chemeleon_signal_release")
Display_1 <- read.table("./data/Display_1.csv", 
                          header=TRUE, sep=",", dec=".", strip.white=TRUE)
names(Display_1) # header rows R is reading
Display_1 <- Display_1 %>% filter(!BodyRegion == "botflank") %>% as.data.frame() #exclude bottom flank.
Display_1$BodyRegion <- factor(Display_1$BodyRegion) #updates levels for the variable BodyRegion
str(Display_1) #how R is reading the stucture of the object, double checking the levels for body region after updating levels

## Display - Achromatic contrast (AC or dL)
## just this next section has Drew's code, which averages the backround (Leaf_B1, Leaf_B2, Leaf_T1, Leaf_T2, Stem1, Stem2) for each lizard body region measurement.
## This is the full model; however, we do not need background because the way the data is set up R is taking an average. See next model (background dropped)
ConspicDisplayAC <- lmer(dL ~ SVL + Population + BodyRegion + Background 
                         + Population:Background + Population:BodyRegion + (1|ID), data=Display_1)
Display_1$PopBRInt <- interaction(Display_1$Population,Display_1$BodyRegion)
ConspicDisplayAC2 <- lmer(dL ~ PopBRInt + (1|ID), data=Display_1)
summary(glht(ConspicDisplayAC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                          "Hawaii.tailbase - Kenya.tailbase = 0" ))))

## dropping background from model (it is still averaged)
## homogeneity of slopes test for SVl-dL by population. To meet test assumptions slopes for each population must be NS.
## assumptions are met
ConspicDisplayAC <- lmer(dL ~ SVL + Population + BodyRegion +  
                           + Population:BodyRegion + (1|ID), data=Display_1)
anova(ConspicDisplayAC)

## Averages over the body regions to get an overall estimate / average dl or ds per population
ConspicDisplayAC <- lmer(dL ~ SVL + Population + SVL:Population + (1|ID), data=Display_1)
ConspicDisplayAC <- lmer(dL ~ SVL + Population + (1|ID), data=Display_1)
anova(ConspicDisplayAC)

## because assumptions met (above) can fit a common slope model by dropping the SVL interaction with population
# SVL is a covariate in the model (it gets loaded as an integer)
# anova gives us significance of fixed effects
# ranova tests random effects (note: report just LRT and df)
# need the SD of residuals and random effects from summary (just report random effects)

ConspicDisplayAC <- lmer(dL ~ SVL + Population + BodyRegion +  
                           + Population:BodyRegion + (1|ID), data=Display_1)
anova(ConspicDisplayAC)
ranova(ConspicDisplayAC)

#for random effects, use ranova (is variance > 0?) report just LRT and df and need the SD of residuals and random effects from summary (just report random effects)

Display_1$PopBRInt <- interaction(Display_1$Population,Display_1$BodyRegion)
ConspicDisplayAC2 <- lmer(dL ~ PopBRInt + (1|ID), data=Display_1)
summary(glht(ConspicDisplayAC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                          "Hawaii.tailbase - Kenya.tailbase = 0" ))))

## this code not used for the moment. Not using lsmeans
summary(ConspicDisplayAC)
lsmeans(ConspicDisplayAC, ~Population*Background)
lsmeans(ConspicDisplayAC, ~Population*BodyRegion)
anova(ConspicDisplayAC)

#View plots of residuals
qqnorm(residuals(ConspicDisplayAC))
hist(residuals(ConspicDisplayAC))

## Display - Chromatic contrast (CC or dS)
## just this next section has Drew's code, which averages the backround (Leaf_B1, Leaf_B2, Leaf_T1, Leaf_T2, Stem1, Stem2) for each lizard body region measurement.
## This is the full model; however, we do not need background because the way the data is set up R is taking an average. See next model (background dropped)
ConspicDisplayCC <- lmer(dS ~ SVL + Population + BodyRegion + Background 
                         + Population:Background + Population:BodyRegion + (1|ID), data=Display_1)
Display_1$PopBRInt <- interaction(Display_1$Population,Display_1$BodyRegion)
ConspicDisplayCC2 <- lmer(dS ~ PopBRInt + (1|ID), data=Display_1)
summary(glht(ConspicDisplayCC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                          "Hawaii.tailbase - Kenya.tailbase = 0" ))))

## dropping background from model (it is still averaged)
## homogeneity of slopes test for SVl-dS by population. To meet test assumptions slopes for each population must be NS.
## assumptions are met
ConspicDisplayCC <- lmer(dS ~ SVL + Population + SVL:Population+ BodyRegion +  
                           + Population:BodyRegion + (1|ID), data=Display_1)
anova(ConspicDisplayCC)

## because assumptions met (above) can fit a common slope model by dropping the SVL interaction with population
# SVL is a covariate in the model (it gets loaded as an integer)
# anova gives us significance of fixed effects
# ranova tests random effects (note: report just LRT and df)
# need the SD of residuals and random effects from summary (just report random effects)

ConspicDisplayCC <- lmer(dS ~ SVL + Population + BodyRegion +  
                           + Population:BodyRegion + (1|ID), data=Display_1)
anova(ConspicDisplayCC)
ranova(ConspicDisplayCC)

#for random effects, use ranova (is variance > 0?) report just LRT and df and need the SD of residuals and random effects from summary (just report random effects)

Display_1$PopBRInt <- interaction(Display_1$Population,Display_1$BodyRegion)
ConspicDisplayCC2 <- lmer(dS ~ PopBRInt + (1|ID), data=Display_1)
summary(glht(ConspicDisplayCC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                          "Hawaii.tailbase - Kenya.tailbase = 0" ))))

#View plots of residuals
qqnorm(residuals(ConspicDisplayCC))
hist(residuals(ConspicDisplayCC))

#Making a point plot with error bars split by body size and population 
#Compute raw means and standard errors for body part and population for dS and assigned this data into Display_plot_dS_dat
Display_plot_dS_dat <- Display_1 %>% #data for dS Chromatic constrast
  group_by(Population, BodyRegion) %>% 
  summarise(Mean = mean(dS, na.rm = T), 
            SE = sd(dS)/sqrt(length(unique(ID))),
            Upper = Mean + 1.96*SE,
            Lower = Mean - 1.96*SE) 
            
Display_plot_dL_dat <- Display_1 %>% #data for dL Achromatic contrast
  group_by(Population, BodyRegion) %>% 
  summarise(Mean = mean(dL, na.rm = T), 
            SE = sd(dL)/sqrt(length(unique(ID))),
            Upper = Mean + 1.96*SE,
            Lower = Mean - 1.96*SE) 

#Plotting Chromatic contrast (dS)
p1 <- ggplot(Display_plot_dS_dat, aes(x = Population, y = Mean, group = BodyRegion, color = BodyRegion)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("Population") + 
  ylab(TeX("Chromatic contrast ($\\Delta s$)")) +
  theme_bw() +
  theme(#legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold"))


#Plotting Achromatic contrast (dL)
ggplot(Display_plot_dL_dat, aes(x = Population, y = Mean, group = BodyRegion, color = BodyRegion)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("Population") + 
  ylab("Achromatic contrast") +
  theme_bw() +
  theme(#legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold"))

#Produce plots to check for outliers for dS
Display_1 <- mutate(Display_1, obs = seq(1, nrow(Display_1))) #Create new variable called obs, a unique ID for each row of data
Display_1 %>% select(Population, dS, BodyRegion, obs) %>% #Selecting Population, dS, BodyRegion and obs to plot
  ggplot(aes(y = dS, x = obs, label = obs)) + #y axis = dS, x = obs, label with obs
  geom_point(colour = "white") + #set the point colour to white
  geom_text() + #replaces the point with text, it takes label = obs and plots this 
  facet_wrap(~Population + BodyRegion) #breaks up plots by population and body region

#Produce plots to check for outliers for dL
Display_1 <- mutate(Display_1, obs = seq(1, nrow(Display_1))) #Create new variable called obs, a unique ID for each row of data
Display_1 %>% select(Population, dL, BodyRegion, obs) %>% #Selecting Population, dL, BodyRegion and obs to plot
  ggplot(aes(y = dL, x = obs, label = obs)) + #y axis = dL, x = obs, label with obs
  geom_point(colour = "white") + #set the point colour to white
  geom_text() + #replaces the point with text, it takes label = obs and plots this 
  facet_wrap(~Population + BodyRegion) #breaks up plots by population and body region


### CONSPICUOUSNESS OF COURTSHIP MODELS
rm(list = ls()) #removes previous analyses, clears R's brain
getwd()
setwd("/Users/martinwhiting/Dropbox/chameleon/3_For Stats")
# Load Dataset - Courtship_1

## Notes on data: Courtship_1 does not have SVL data, but does have botflank, which we do not include in analysis
## Notes on data: Courtship_1.2withSVL has SVL data, but does not have botflank. This doesn't matter because we don't use botflank.
## Load both data files to extract and merge into a single data object that has SVL and body regions (excluding botflank) called Courtship_1.2
Courtship_1 <- read.table("~/Dropbox/chameleon/3_For Stats/Courtship_1.csv", 
                            header=TRUE, sep=",", dec=".", strip.white=TRUE)
Courtship_1.2withSVL <- read.table("~/Dropbox/chameleon/3_For Stats/Courtship_1.2.csv", 
                          header=TRUE, sep=",", dec=".", strip.white=TRUE)
names(Courtship_1) # header rows R is reading
names(Courtship_1.2withSVL)

Courtship_1 <- left_join(Courtship_1, select(Courtship_1.2withSVL, ID, SVL)) #merged SVL from Courtship_1.2withSVL with original dataset Courtship_1 and overwrites Courtship_1
Courtship_1

Courtship_1.2 <- Courtship_1 %>% filter(!BodyRegion == "botflank") %>% as.data.frame() #exclude bottom flank. new data object called Courtship_1.2
Courtship_1.2$BodyRegion <- factor(Courtship_1.2$BodyRegion) #updates levels for the variable BodyRegion

str(Courtship_1.2) # how R is reading the stucture of the object, double checking the levels for body region after updating levels.

## Courtship - Achromatic contrast (AC or dL)
## just this next section has Drew's code, which averages the backround (Leaf_B1, Leaf_B2, Leaf_T1, Leaf_T2, Stem1, Stem2) for each lizard body region measurement.
## This is the full model; however, we do not need background because the way the data is set up R is taking an average. See next model (background dropped)
ConspicCourtshipAC <- lmer(dL ~ SVL + Population + BodyRegion + Background 
                           + Population:Background + Population:BodyRegion + (1|ID), data=Courtship_1.2)
Courtship_1.2$PopBRInt <- interaction(Courtship_1.2$Population,Courtship_1.2$BodyRegion)
ConspicCourtshipAC2 <- lmer(dL ~ PopBRInt + (1|ID), data=Courtship_1.2)
summary(glht(ConspicCourtshipAC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                            "Hawaii.tailbase - Kenya.tailbase = 0" ))))

## dropping background from model (it is still averaged)
## homogeneity of slopes test for SVl-dL by population. To meet test assumptions slopes for each population must be NS.
## assumptions are met
ConspicCourtshipAC <- lmer(dL ~ SVL + Population + SVL:Population+ BodyRegion +  
                             + Population:BodyRegion + (1|ID), data=Courtship_1.2)
anova(ConspicCourtshipAC)

## because assumptions met (above) can fit a common slope model by dropping the SVL interaction with population
# SVL is a covariate in the model (it gets loaded as an integer)
# anova gives us significance of fixed effects
# ranova tests random effects (note: report just LRT and df)
# need the SD of residuals and random effects from summary (just report random effects)

ConspicCourtshipAC <- lmer(dL ~ SVL + Population + BodyRegion +  
                             + Population:BodyRegion + (1|ID), data=Courtship_1.2)
anova(ConspicCourtshipAC)
ranova(ConspicCourtshipAC)

#for random effects, use ranova (is variance > 0?) report just LRT and df and need the SD of residuals and random effects from summary (just report random effects)

Courtship_1.2$PopBRInt <- interaction(Courtship_1.2$Population,Courtship_1.2$BodyRegion)
ConspicCourtshipAC2 <- lmer(dL ~ PopBRInt + (1|ID), data=Courtship_1.2)
summary(glht(ConspicCourtshipAC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                            "Hawaii.tailbase - Kenya.tailbase = 0" ))))

## this code not used for the moment. Not using lsmeans
summary(ConspicCourtshipAC)
lsmeans(ConspicCourtshipAC, ~Population*Background)
lsmeans(ConspicCourtshipAC, ~Population*BodyRegion)
anova(ConspicCourtshipAC)

#View plots of residuals
qqnorm(residuals(ConspicCourtshipAC))
hist(residuals(ConspicCourtshipAC))

## Courtship - Chromatic contrast (CC or dS)
## just this next section has Drew's code, which averages the backround (Leaf_B1, Leaf_B2, Leaf_T1, Leaf_T2, Stem1, Stem2) for each lizard body region measurement.
## This is the full model; however, we do not need background because the way the data is set up R is taking an average. See next model (background dropped)
ConspicCourtshipCC <- lmer(dS ~ SVL + Population + BodyRegion + Background 
                           + Population:Background + Population:BodyRegion + (1|ID), data=Courtship_1.2)
Courtship_1.2$PopBRInt <- interaction(Courtship_1.2$Population,Courtship_1.2$BodyRegion)
ConspicCourtshipCC2 <- lmer(dS ~ PopBRInt + (1|ID), data=Courtship_1.2)
summary(glht(ConspicCourtshipCC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                            "Hawaii.tailbase - Kenya.tailbase = 0" ))))

## dropping background from model (it is still averaged)
## homogeneity of slopes test for SVl-dS by population. To meet test assumptions slopes for each population must be NS.
## assumptions are met
ConspicCourtshipCC <- lmer(dS ~ SVL + Population + SVL:Population+ BodyRegion +  
                             + Population:BodyRegion + (1|ID), data=Courtship_1.2)
anova(ConspicCourtshipCC)

## because assumptions met (above) can fit a common slope model by dropping the SVL interaction with population
# SVL is a covariate in the model (it gets loaded as an integer)
# anova gives us significance of fixed effects
# ranova tests random effects (note: report just LRT and df)
# need the SD of residuals and random effects from summary (just report random effects)

ConspicCourtship_1CC <- lmer(dS ~ SVL + Population + BodyRegion +  
                                 + Population:BodyRegion + (1|ID), data=Courtship_1.2)
anova(ConspicCourtshipCC)
ranova(ConspicCourtshipCC)


#for random effects, use ranova (is variance > 0?) report just LRT and df and need the SD of residuals and random effects from summary (just report random effects)

Courtship_1.2$PopBRInt <- interaction(Courtship_1.2$Population,Courtship_1.2$BodyRegion)
ConspicCourtshipCC2 <- lmer(dS ~ PopBRInt + (1|ID), data=Courtship_1.2)
summary(glht(ConspicCourtshipCC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                            "Hawaii.tailbase - Kenya.tailbase = 0" ))))

#View plots of residuals
qqnorm(residuals(ConspicCourtshipCC))
hist(residuals(ConspicCourtshipCC))

#Making a point plot with error bars split by body size and population 
#Compute raw means and standard errors for body part and population for dS and assigned this data into Display_plot_dS_dat
Courtship_plot_dS_dat <- Courtship_1.2 %>% #data for dS Chromatic constrast
  group_by(Population, BodyRegion) %>% 
  summarise(Mean = mean(dS, na.rm = T), 
            SE = sd(dS)/sqrt(length(unique(ID))),
            Upper = Mean + 1.96*SE,
            Lower = Mean - 1.96*SE) 

Courtship_plot_dL_dat <- Courtship_1.2 %>% #data for dL Achromatic contrast
  group_by(Population, BodyRegion) %>% 
  summarise(Mean = mean(dL, na.rm = T), 
            SE = sd(dL)/sqrt(length(unique(ID))),
            Upper = Mean + 1.96*SE,
            Lower = Mean - 1.96*SE) 

            
Courtship_plot_dL_dat
str(Courtship_plot_dL_dat)

#Plotting Chromatic contrast (dS)
ggplot(Courtship_plot_dS_dat, aes(x = Population, y = Mean, group = BodyRegion, color = BodyRegion)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("Population") + 
  ylab("Chromatic contrast") +
  theme_bw() +
  theme(#legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold"))

#Plotting Achromatic contrast (dL)
ggplot(Courtship_plot_dL_dat, aes(x = Population, y = Mean, group = BodyRegion, color = BodyRegion)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("Population") + 
  ylab("Achromatic contrast") +
  theme_bw() +
  theme(#legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold"))

#Checked for outliers for midflank because pattern was reversed for Hawaii vs Kenya
Courtship_1 %>% select(Population, dS, BodyRegion) %>%
  ggplot(aes(y = dS, x = BodyRegion)) + 
  geom_boxplot() +
  geom_point() +
  facet_wrap(~Population)

#Produce plots to check for outliers for dS
Courtship_1 <- mutate(Courtship_1, obs = seq(1, nrow(Courtship_1))) #Create new variable called obs, a unique ID for each row of data
Courtship_1 %>% select(Population, dS, BodyRegion, obs) %>% #Selecting Population, dS, BodyRegion and obs to plot
  ggplot(aes(y = dS, x = obs, label = obs)) + #y axis = dS, x = obs, label with obs
  geom_point(colour = "white") + #set the point colour to white
  geom_text() + #replaces the point with text, it takes label = obs and plots this 
  facet_wrap(~Population + BodyRegion) #breaks up plots by population and body region

#Produce plots to check for outliers for dL
Courtship_1 <- mutate(Courtship_1, obs = seq(1, nrow(Courtship_1))) #Create new variable called obs, a unique ID for each row of data
Courtship_1 %>% select(Population, dL, BodyRegion, obs) %>% #Selecting Population, dL, BodyRegion and obs to plot
  ggplot(aes(y = dL, x = obs, label = obs)) + #y axis = dL, x = obs, label with obs
  geom_point(colour = "white") + #set the point colour to white
  geom_text() + #replaces the point with text, it takes label = obs and plots this 
  facet_wrap(~Population + BodyRegion) #breaks up plots by population and body region


### PREDATION MODELS
rm(list = ls()) #removes previous analyses, clears R's brain
getwd()
setwd("/Users/martinwhiting/Dropbox/chameleon/3_For Stats")
# Load Dataset - PREDATOR_2+3
Predator <- read.table("~/Dropbox/chameleon/3_For Stats/Predator_2+3.csv", 
                       header=TRUE, sep=",", dec=".", strip.white=TRUE)
names(Predator) 
str(Predator) 

names(Predator) # header rows R is reading
Predator <- Predator %>% filter(!BodyRegion == "botflank") %>% as.data.frame() #exclude bottom flank.
Predator$BodyRegion <- factor(Predator$BodyRegion) #updates levels for the variable BodyRegion
str(Predator) #how R is reading the stucture of the object, double checking the levels for body region after updating levels

## chromatic contrast was right skewed, so this is a log transformation
summary(Predator)
Predator <- Predator %>% mutate(log_dS = log(dS),
                                log_dL = log(dL),
                                sqrt_dS = sqrt(dS),
                                sqrt_dL = sqrt(dL))
##checks distribution
hist(Predator$dL)
hist(Predator$sqrt_dS) ## use this
hist(Predator$log_dS) ## not as good as sq root

hist(Predator$sqrt_dL) ##sqrt transformation works really well--nice normal distribution. Model interpretation might be tricky though.

## Predator Model - Achromatic contrast (AC or dL)
## just this next section has Drew's code, which averages the backround (Leaf_B1, Leaf_B2, Leaf_T1, Leaf_T2, Stem1, Stem2) for each lizard body region measurement.
## This is the full model; however, we do not need background because the way the data is set up R is taking an average. See next model (background dropped)
PredatorAC <- lmer(sqrt_dL ~ Predator + Population + BodyRegion + Background 
                         + Predator:Background + Predator:BodyRegion + (1|ID), data=Predator)
Predator$PopBRInt <- interaction(Predator$Population,Predator$BodyRegion)
PredatorAC2 <- lmer(sqrt_dL ~ PopBRInt + (1|ID), data=Predator)
summary(glht(PredatorAC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                          "Hawaii.tailbase - Kenya.tailbase = 0" ))))

## dropping background from model (it is still averaged)
# anova gives us significance of fixed effects
# ranova tests random effects (note: report just LRT and df)
# need the SD of residuals and random effects from summary (just report random effects)

PredatorAC <- lmer(sqrt_dL ~ Predator + Population + BodyRegion +  
                           + Population:BodyRegion + (1|ID), data=Predator[Predator$BodyRegion!='gularbot',]) # we have gularbot for Kenya but not Hawaii, so exclude it
anova(PredatorAC)
ranova(PredatorAC)
# for the above model, predator is nonsignificant, so we can drop it from the model (below).

PredatorAC <- lmer(sqrt_dL ~ Population + BodyRegion +  
                     + Population:BodyRegion + (1|ID), data=Predator[Predator$BodyRegion!='gularbot',]) # we have gularbot for Kenya but not Hawaii, so exclude it
anova(PredatorAC)
ranova(PredatorAC)


#for random effects, use ranova (is variance > 0?) report just LRT and df and need the SD of residuals and random effects from summary (just report random effects)

Predator$PopBRInt <- interaction(Predator$Population,Predator$BodyRegion)
PredatorAC2 <- lmer(sqrt_dL ~ PopBRInt + (1|ID), data=Predator[Predator$BodyRegion!='gularbot',])
summary(glht(PredatorAC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                          "Hawaii.tailbase - Kenya.tailbase = 0" ))))

# View plots of residuals
qqnorm(residuals(PredatorAC))
hist(residuals(PredatorAC))


## Predator Model - Chromatic contrast (CC or dS)
## just this next section has Drew's code, which averages the backround (Leaf_B1, Leaf_B2, Leaf_T1, Leaf_T2, Stem1, Stem2) for each lizard body region measurement.
## This is the full model; however, we do not need background because the way the data is set up R is taking an average. See next model (background dropped)
PredatorCC <- lmer(sqrt_dS ~ Predator + Population + BodyRegion + Background 
                   + Predator:Background + Predator:BodyRegion + (1|ID), data=Predator)
Predator$PopBRInt <- interaction(Predator$Population,Predator$BodyRegion)
PredatorCC2 <- lmer(sqrt_dS ~ PopBRInt + (1|ID), data=Predator)
summary(glht(PredatorCC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                    "Hawaii.tailbase - Kenya.tailbase = 0" ))))

## dropping background from model (it is still averaged)
# anova gives us significance of fixed effects
# ranova tests random effects (note: report just LRT and df)
# need the SD of residuals and random effects from summary (just report random effects)

PredatorCC <- lmer(sqrt_dS ~ Predator + Population + BodyRegion +  
                     + Population:BodyRegion + (1|ID), data=Predator[Predator$BodyRegion!='gularbot',]) # we have gularbot for Kenya but not Hawaii, so exclude it
anova(PredatorCC)
ranova(PredatorCC)

### This is old code for when in the above model, predator was nonsignificant, so we can drop it from the model (below). But after sqrt it is significant, so don't use this code. Just keeping it in case.
PredatorCC <- lmer(sqrt_dS ~ Population + BodyRegion +  
                     + Population:BodyRegion + (1|ID), data=Predator[Predator$BodyRegion!='gularbot',]) # we have gularbot for Kenya but not Hawaii, so exclude it
anova(PredatorCC)
ranova(PredatorCC)
###

#for random effects, use ranova (is variance > 0?) report just LRT and df and need the SD of residuals and random effects from summary (just report random effects)

Predator$PopBRInt <- interaction(Predator$Population,Predator$BodyRegion)
PredatorCC2 <- lmer(sqrt_dS ~ PopBRInt + (1|ID), data=Predator[Predator$BodyRegion!='gularbot',])
summary(glht(PredatorCC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                    "Hawaii.tailbase - Kenya.tailbase = 0" ))))

# View plots of residuals
qqnorm(residuals(PredatorCC))
hist(residuals(PredatorCC))

#Making a point plot with error bars split by body size and population 
#Compute raw means and standard errors for body part and population for dS and assigned this data into Predator_plot_dS_dat
Predator_plot_dS_dat <- Predator %>% #data for dS Chromatic constrast
  group_by(Population, BodyRegion) %>% 
  summarise(Mean = mean(dS, na.rm = T), 
            SE = sd(dS)/sqrt(length(unique(ID))),
            Upper = Mean + SE,
            Lower = Mean - SE) 

## below is to exclude gularbot from figure because we only have it for Kenya and not hawaii
Pred_plot_dS_dat_nogularbot <- Predator_plot_dS_dat %>% filter(! BodyRegion == 'gularbot')


Predator_plot_dL_dat <- Predator %>% #data for dL Achromatic contrast
  group_by(Population, BodyRegion) %>% 
  summarise(Mean = mean(dL, na.rm = T), 
            SE = sd(dL)/sqrt(length(unique(ID))),
            Upper = Mean + SE,
            Lower = Mean - SE) 

## below is to exclude gularbot from figure because we only have it for Kenya and not hawaii
Pred_plot_dL_dat_nogularbot <- Predator_plot_dL_dat %>% filter(! BodyRegion == 'gularbot')

Predator_plot_dL_dat
str(Predator_plot_dL_dat)

#Plotting Chromatic contrast (dS)
ggplot(Pred_plot_dS_dat_nogularbot, aes(x = Population, y = Mean, group = BodyRegion, color = BodyRegion)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("Population") + 
  ylab("Chromatic contrast") +
  theme_bw() +
  theme(#legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold"))

#Plotting Achromatic contrast (dL)
ggplot(Pred_plot_dL_dat_nogularbot, aes(x = Population, y = Mean, group = BodyRegion, color = BodyRegion)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("Population") + 
  ylab("Achromatic contrast") +
  theme_bw() +
  theme(#legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold"))

### DISPLAY VS SUBORDINATE MODELS
## This is the contrast of display colour against subordinate colour (matched pairs, for each lizard).

rm(list = ls()) #removes previous analyses, clears R's brain
getwd()
setwd("/Users/martinwhiting/Dropbox/chameleon/3_For Stats")
# Load Dataset - Disp_Sub_4
DispSub <- read.table("~/Dropbox/chameleon/3_For Stats/Disp_Sub_4.csv", 
                       header=TRUE, sep=",", dec=".", strip.white=TRUE)
names(DispSub) 
str(DispSub) 

names(DispSub) # header rows R is reading
DispSub <- DispSub %>% filter(!BodyRegion == "botflank") %>% as.data.frame() #exclude bottom flank.
DispSub$BodyRegion <- factor(DispSub$BodyRegion) #updates levels for the variable BodyRegion
str(DispSub) #how R is reading the stucture of the object, double checking the levels for body region after updating levels

## Display Subordinate Model - Achromatic contrast (AC or dL)
## we do not need background for this analysis
DispSubAC <- lmer(dL ~ Population + BodyRegion +  
                    Population:BodyRegion + (1|ID), data=DispSub)
DispSub$PopBRInt <- interaction(DispSub$Population,DispSub$BodyRegion)
DispSubAC2 <- lmer(dL ~ PopBRInt + (1|ID), data=DispSub)
summary(glht(DispSubAC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                    "Hawaii.tailbase - Kenya.tailbase = 0" ))))

anova(DispSubAC)
ranova(DispSubAC)
summary(DispSubAC)

#for random effects, use ranova (is variance > 0?) report just LRT and df and need the SD of residuals and random effects from summary (just report random effects)

DispSub$PopBRInt <- interaction(DispSub$Population,DispSub$BodyRegion)
DispSubAC2 <- lmer(dL ~ PopBRInt + (1|ID), data=DispSub[DispSub$BodyRegion!='gularbot',])
summary(glht(DispSubAC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                    "Hawaii.tailbase - Kenya.tailbase = 0" ))))

# View plots of residuals
qqnorm(residuals(DispSubAC))
hist(residuals(DispSubAC))



## Display Subordinate Model - Chromatic contrast (CC or dS)
## we do not need background for this analysis
DispSubCC <- lmer(dS ~ Population + BodyRegion +  
                    Population:BodyRegion + (1|ID), data=DispSub)
DispSub$PopBRInt <- interaction(DispSub$Population,DispSub$BodyRegion)
DispSubCC2 <- lmer(dL ~ PopBRInt + (1|ID), data=DispSub)
summary(glht(DispSubCC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                   "Hawaii.tailbase - Kenya.tailbase = 0" ))))

anova(DispSubCC)
ranova(DispSubCC)
summary(DispSubCC)

#for random effects, use ranova (is variance > 0?) report just LRT and df and need the SD of residuals and random effects from summary (just report random effects)

DispSub$PopBRInt <- interaction(DispSub$Population,DispSub$BodyRegion)
DispSubCC2 <- lmer(dS ~ PopBRInt + (1|ID), data=DispSub[DispSub$BodyRegion!='gularbot',])
summary(glht(DispSubCC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", "Hawaii.topflank - Kenya.topflank = 0", 
                                                  "Hawaii.tailbase - Kenya.tailbase = 0" ))))

# View plots of residuals
qqnorm(residuals(DispSubCC))
hist(residuals(DispSubCC))


#Making a point plot with error bars split by body size and population 
#Compute raw means and standard errors for body part and population for dS and assigned this data into DispSub_plot_dS_dat
DispSub_plot_dS_dat <- DispSub %>% #data for dS Chromatic constrast
  group_by(Population, BodyRegion) %>% 
  summarise(Mean = mean(dS, na.rm = T), 
            SE = sd(dS)/sqrt(length(unique(ID))),
            Upper = Mean + SE,
            Lower = Mean - SE) 

DispSub_plot_dL_dat <- DispSub %>% #data for dL Achromatic contrast
  group_by(Population, BodyRegion) %>% 
  summarise(Mean = mean(dL, na.rm = T), 
            SE = sd(dL)/sqrt(length(unique(ID))),
            Upper = Mean + SE,
            Lower = Mean - SE) 

#Plotting Chromatic contrast (dS)
ggplot(DispSub_plot_dS_dat, aes(x = Population, y = Mean, group = BodyRegion, color = BodyRegion)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("Population") + 
  ylab("Chromatic contrast") +
  theme_bw() +
  theme(#legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold"))

#Plotting Achromatic contrast (dL)
ggplot(DispSub_plot_dL_dat, aes(x = Population, y = Mean, group = BodyRegion, color = BodyRegion)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("Population") + 
  ylab("Achromatic contrast") +
  theme_bw() +
  theme(#legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold"))


### LOCAL ADAPTATION - DISPLAY/ COURTSHIP AGAINST DIFF BACKGROUNDS

# Load Dataset - DISPLAY_5

## NEED TO DOUBLE CHECK IF WE NEED SVL. ITS NOT IN THE EXCEL FILE.

DisplayBgrd <- read.table("~/Dropbox/chameleon/3_For Stats/Display_5.csv", 
                        header=TRUE, sep=",", dec=".", strip.white=TRUE)
names(DisplayBgrd) # header rows R is reading
DisplayBgrd <- DisplayBgrd %>% filter(!BodyRegion == "botflank") %>% as.data.frame() #exclude bottom flank.
DisplayBgrd$BodyRegion <- factor(DisplayBgrd$BodyRegion) #updates levels for the variable BodyRegion
str(DisplayBgrd) #how R is reading the stucture of the object, double checking the levels for body region after updating levels

## Display - Achromatic contrast (AC or dL)
## just this next section has Drew's code, which averages the backround (Leaf_B1, Leaf_B2, Leaf_T1, Leaf_T2, Stem1, Stem2) for each lizard body region measurement.
## This is the full model; however, we do not need background because the way the data is set up R is taking an average. See next model (background dropped)
DispBgrdAC <- lmer(dL ~ Population + BodyRegion  + Background2
                          + Population:Background2 + Population:BodyRegion + (1|ID), data=DisplayBgrd)
# anova gives us significance of fixed effects
# ranova tests random effects (note: report just LRT and df)
# need the SD of residuals and random effects from summary (just report random effects)
#for random effects, use ranova (is variance > 0?) report just LRT and df and need the SD of residuals and random effects from summary (just report random effects)

anova(DispBgrdAC)
ranova(DispBgrdAC)
summary(DispBgrdAC)
### TEST CODE DAN NOBLE
##Background and it's interaction are dropped, used a robust variance estimator to reduce the effect of using many measurements from same individuals.
DisplayBgrd <- DisplayBgrd %>%
                mutate(backgroun_pop= ifelse(Background2 == "own" & Population == "Kenya", "Kenya", ifelse(Background2 == "own" & Population == "Hawaii", "Hawaii")))

DisplayBgrd2 <- DisplayBgrd %>%
        filter(Population == "Hawaii")
DispBgrdAC <-nlme::lme(dL ~ Background2,
                  random = ~1|ID, data=DisplayBgrd2)
# anova gives us significance of fixed effects
summary(DispBgrdAC)

install.packages("clubSandwich")
library(clubSandwich)

coef_test(DispBgrdAC, vcov="CR2", cluster=DisplayBgrd2$ID)
##END OF DAN'S CODE

## I want to compare population with background2 with body region, not sure how to amend 5 lines down starting with summary(glt)

## below are the multiple comparisons lines
DisplayBgrd$PopBRInt <- interaction(DisplayBgrd$Population,DisplayBgrd$BodyRegion, DisplayBgrd$Background2)
DisplayBgrdAC2 <- lmer(dL ~ PopBRInt + (1|ID), data=DisplayBgrd)
summary(glht(DisplayBgrdAC2, linfct = mcp(PopBRInt = c("Hawaii.gular - Kenya.gular = 0","Hawaii.midflank - Kenya.midflank = 0", 
            "Hawaii.topflank - Kenya.topflank = 0", "Hawaii.tailbase - Kenya.tailbase = 0" ))))

#View plots of residuals
qqnorm(residuals(DispBgrdAC))
hist(residuals(DispBgrdAC))





###Kat

## Display: own vs other background - Achromatic contrast (dL)
DisplayBgrdAC <- lmer(dL ~ Population + BodyRegion + Background + Background2 
                      + Population*Background + Population*Background2 + BodyRegion*Background + BodyRegion*Background2 +(1|ID), data=DisplayBgrd)
summary(DisplayBgrdAC)
anova(DisplayBgrdAC)
#View plots of residuals
qqnorm(residuals(DisplayBgrdAC))
hist(residuals(DisplayBgrdAC))


## Display: own vs other background - Chromatic contrast (dS)
DisplayBgrdCC <- lmer(dS ~ Population + BodyRegion + Background + Background2 
                      + Population*Background + Population*Background2 + BodyRegion*Background + BodyRegion*Background2 +(1|ID), data=DisplayBgrd)
summary(DisplayBgrdCC)
anova(DisplayBgrdCC)


#View plots of residuals
qqnorm(residuals(DisplayBgrdCC))
hist(residuals(DisplayBgrdCC))
