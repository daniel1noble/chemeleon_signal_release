## ----getlibrary, echo = FALSE, results = "hide"-----------------------------------------------------------
# load libraries
devtools::install_github("karthik/wesanderson")
pacman::p_load(lme4, lmerTest, ggplot2, lsmeans, emmeans, multcomp, dplyr, wesanderson, patchwork, latex2exp, png, cowplot, magick, jpeg, clubSandwich, flextable, readxl)
source("./R/func.R")



## ----Models, echo = FALSE, results = "hide"---------------------------------------------------------------
source("./R/func.R")
#########################################################
# Social Context: Male-male Competition Displays
#########################################################
#Load data and clean a bit
Display_1 <- read.table("./data/Display_1.csv", 
                          header=TRUE, sep=",", dec=".", strip.white=TRUE)

# Sample sizes
N_display <- data.frame(Display_1 %>% group_by(Population) %>% summarise(n = length(unique(ID))))

# Center SVL to make intercept interpretable
Display_1$sc_SVL <- with(Display_1, scale(SVL, scale=FALSE))

#exclude bottom flank.
Display_1 <- Display_1 %>% 
              filter(!BodyRegion == "botflank") %>% as.data.frame() 

# create as factor
    Display_1$BodyRegion <- factor(Display_1$BodyRegion)

##########
## Achromatic Contrast - dL
##########
      
    # Fit the model
    ConspicDisplayAC <- lmer(dL ~ sc_SVL + Population + (1|ID), data=Display_1)
      summary(ConspicDisplayAC)
     
    # Check robustness of results to non-independence using Robust Variance Estimator (RVE)
    Display_1_IDs <- Display_1 %>% filter(!is.na(sc_SVL), !is.na(Population)) # filter out missing data
       robust_ConspicDisplayAC <- coef_test(ConspicDisplayAC, 
                                            cluster = Display_1_IDs$ID, vcov = "CR2")  
    robust_ConspicDisplayAC_CI <- conf_int(ConspicDisplayAC, 
                                           cluster = Display_1_IDs$ID, vcov = "CR2")
    
     # Wald test
            Population_ConspicDisplayAC <- Wald_test(ConspicDisplayAC, 
                                             constraints = constrain_zero(3),
                                            cluster = Display_1_IDs$ID, vcov = "CR2")
    
##########
# Chromatic contrast - dS
##########
    #Fit the model
         ConspicDisplayCC <- lmer(dS ~ sc_SVL + Population + (1|ID), data=Display_1)
         summary(ConspicDisplayCC)
    
    # Check robustness of results to non-indepenedence using Robust Variance Estimator (RVE)
    Display_1_IDs <- Display_1 %>% filter(!is.na(sc_SVL), !is.na(Population)) # filter out misssing data
    robust_ConspicDisplayCC <- coef_test(ConspicDisplayCC, cluster = Display_1_IDs$ID, vcov = "CR2")
 robust_ConspicDisplayCC_CI <- conf_int(ConspicDisplayCC, cluster = Display_1_IDs$ID, vcov = "CR2")
    
  # Wald test
            Population_ConspicDisplayCC <- Wald_test(ConspicDisplayCC, 
                                             constraints = constrain_zero(3),
                                            cluster = Display_1_IDs$ID, vcov = "CR2")
#########################################################
# Social Context: Courtship Displays
#########################################################

# Load data
Courtship <- read.table("./data/Courtship_1.2.csv", 
                          header=TRUE, sep=",", dec=".", strip.white=TRUE)

# Sample sizes
N_Courtship <- data.frame(Courtship %>% group_by(Population) %>% summarise(n = length(unique(ID))))

# Center SVL do intercept is at the mean body size
Courtship$sc_SVL <- with(Courtship, scale(SVL, scale=FALSE))
    
##########
## Achromatic Contrast - dL
##########

  # Fit the model
    ConspicCourtshipAC <- lmer(dL ~ sc_SVL + Population  + (1|ID), data=Courtship)
    summary(ConspicCourtshipAC)
    
 # Check robustness of results to non-independence using Robust Variance Estimator (RVE)
    Courtship_IDs <- Courtship %>% filter(!is.na(sc_SVL), !is.na(Population)) # filter out missing data
    robust_ConspicCourtshipAC <- coef_test(ConspicCourtshipAC, 
                                           cluster = Courtship_IDs$ID, vcov = "CR2")
 robust_ConspicCourtshipAC_CI <- conf_int(ConspicCourtshipAC, 
                                          cluster = Courtship_IDs$ID, vcov = "CR2")
 
    # Wald test
            Population_ConspicCourtshipAC <- Wald_test(ConspicCourtshipAC, 
                                             constraints = constrain_zero(3),
                                            cluster = Courtship_IDs$ID, vcov = "CR2")

##########
# Chromatic contrast - dS
##########
    
  # Fit the model
  ConspicCourtshipCC <- lmer(dS ~ sc_SVL + Population  + (1|ID), data=Courtship)
    summary(ConspicCourtshipCC)
 
  # Check robustness of results to non-indepenedence using Robust Variance Estimator (RVE)
    Courtship_IDs <- Courtship %>% filter(!is.na(sc_SVL), !is.na(Population)) # filter out misssing data
       robust_ConspicCourtshipCC <- coef_test(ConspicCourtshipCC, cluster = Courtship_IDs$ID, vcov = "CR2")
    robust_ConspicCourtshipCC_CI <- conf_int(ConspicCourtshipCC, cluster = Courtship_IDs$ID, vcov = "CR2")
    
    # Wald test
            Population_ConspicCourtshipCC <- Wald_test(ConspicCourtshipCC, 
                                             constraints = constrain_zero(3),
                                            cluster = Courtship_IDs$ID, vcov = "CR2")
    
 #########################################################
# Predator Context: Birds and snakes
#########################################################
 Predator <- read.table("./data/Predator_2_3.csv", 
                       header=TRUE, sep=",", dec=".", strip.white=TRUE)
 
# Sample sizes
N_Pred <- data.frame(Predator %>% group_by(Population, Predator) %>% summarise(n = length(unique(ID)))) %>% arrange(Predator)
            
# Clean up
 Predator <- Predator %>% filter(!BodyRegion == "botflank" & !BodyRegion =='gularbot') %>% as.data.frame() #exclude bottom flank.
Predator$BodyRegion <- factor(Predator$BodyRegion) #updates levels for the variable BodyRegion

##########
## Achromatic Contrast - dL
##########

# First, if predator type varies across populations then we would predcit that whether snake and bird actually are different would depend on the population. For example, if, say, in Hawaii and Kenya birds were equally likely as predators then dL would not vary for birds across populations, but would so for snakes. This is an interaction.

  # Fit model testing this idea above
    PredatorAC_inter <- lmer(dL ~ Predator + Population + Predator:Population + (1|ID), 
                             data = Predator) 
    summary(PredatorAC_inter)
    
  #Refit main effects so we can test main effects on own.
         PredatorAC <- lmer(dL ~ Predator + Population + (1|ID), data = Predator) 
        summary(PredatorAC)
  
     # Wald tests
            back_pred_AC_Pred <- Wald_test(PredatorAC_inter, constraints = constrain_zero(4), 
                                   cluster = Predator$ID, vcov = "CR2")
           
             Predator_AC_Pred <- Wald_test(PredatorAC, constraints = constrain_zero(2), 
                                  cluster = Predator$ID, vcov = "CR2")
  
           Population_AC_Pred <- Wald_test(PredatorAC, constraints = constrain_zero(3), 
                                  cluster = Predator$ID, vcov = "CR2")
     
  # Check robustness of results to non-indepenedence using Robust Variance Estimator (RVE)
    Predator_IDs <- Predator %>% filter(!is.na(Predator), !is.na(Population)) # filter out misssing data
              robust_PredatorAC <- coef_test(PredatorAC, cluster = Predator_IDs$ID, vcov = "CR2")
           robust_PredatorAC_CI <-  conf_int(PredatorAC, cluster = Predator_IDs$ID, vcov = "CR2")
     robust_PredatorAC_CI_inter <-  conf_int(PredatorAC_inter, cluster = Predator_IDs$ID, vcov = "CR2")
     
  # Model allows us to get bird in Hwaii compared to bird in Kenya (contrast 3)
      Predator$pred_pop <- with(Predator, interaction(Predator, Population))
    
      PredatorAC_pred_pop1 <- lmer(dL ~  pred_pop + (1|ID), data = Predator) 
      summary(PredatorAC_pred_pop1)
      PredatorAC_pred_pop2 <- lmer(dL ~  relevel(pred_pop, ref = "snake.Hawaii") + (1|ID), data = Predator) 
      summary(PredatorAC_pred_pop2)
      
      # Fit the models so we get the estimate and corrected robust standard error for the contrasts we want to make
      robust_PredatorAC_pred_pop1 <- conf_int(PredatorAC_pred_pop1, cluster = Predator$ID, vcov="CR2")
      robust_PredatorAC_pred_pop2 <- conf_int(PredatorAC_pred_pop2, cluster = Predator$ID, vcov="CR2")
      
     # Contrasts hypothesis tests, but use RVE 
      
      PredatorAC_pred_pop <- lmer(dL ~  0+ pred_pop + (1|ID), data = Predator) 
    result_pred <- Wald_test(PredatorAC_pred_pop, constraints = constrain_pairwise(c(1:4)), 
                             vcov="CR2", tidy = TRUE)
    result_pred$p_val_bonfer <- sapply(p.adjust(result_pred$p_val, "bonferroni"), function(x) p_value(x))
    
    
    # Get the contrasts and Wald stats for comparions
     CI_hyp_testAC <- data.frame(
              comparison = c("Snake (Hawaii-Kenya)", "Birds (Hawaii-Kenya)"), 
                Estimate = c(robust_PredatorAC_pred_pop2[4,2], robust_PredatorAC_pred_pop1[3, 2]),
                lwr = c(robust_PredatorAC_pred_pop2[4,5], robust_PredatorAC_pred_pop1[3, 5]),
                upr = c(robust_PredatorAC_pred_pop2[4,6], robust_PredatorAC_pred_pop1[3, 6]),
              Fstat = c(result_pred[5,3], result_pred[2,3]),
              df = c(result_pred[5,5], result_pred[2,5]),
              p_val = c(result_pred[5,7], result_pred[2,7]),
              p_bonf = c(result_pred[5,8], result_pred[2,8]))

##########
# Chromatic contrast - dS
##########

    # First, if predator type varies across populations then we would predcit that whether snake and bird actually are different would depend on the population. For example, if, say, in Hawaii and Kenya birds were equally likely as predators then dL would not vary for birds across populations, but would so for snakes. This is an interaction.
    
    # Fit the interaction model.
    PredatorCC <- lmer(dS ~ Predator + Population + Predator:Population + (1|ID), 
                       data = Predator) 
    summary(PredatorCC)
  
    #Refit main effects so we can test main effects on own.
      PredatorCC_main <- lmer(dS ~ Predator + Population + (1|ID), data = Predator) 

     # Wald tests
            back_pred_CC_Pred <- Wald_test(PredatorCC, constraints = constrain_zero(4), 
                                   cluster = Predator$ID, vcov = "CR2")
           
             Predator_CC_Pred <- Wald_test(PredatorCC_main, constraints = constrain_zero(2), 
                                  cluster = Predator$ID, vcov = "CR2")
  
           Population_CC_Pred <- Wald_test(PredatorCC_main, constraints = constrain_zero(3), 
                                  cluster = Predator$ID, vcov = "CR2")
           
    # Support for an interaction between predator type and population in the chromatic realm. Planned comparisons. First re-fit model
      Predator$pred_pop <- with(Predator, interaction(Predator, Population))
    PredatorCC_pred_pop <- lmer(dS ~ -1 + pred_pop + (1|ID), data = Predator) 
    summary(PredatorCC_pred_pop)
    
    # Present full model
         robust_PredatorCC_CI_inter <-  conf_int(PredatorCC, cluster = Predator_IDs$ID, vcov = "CR2")
    
    # Fit RVE, we want contrast effects because annoyingly RVE doesn't provide them so will relevel to get them in the context of RVE
      
    # Model allows us to get bird in Hwaii compared to bird in Kenya (contrast 3)
      PredatorCC_pred_pop1 <- lmer(dS ~  pred_pop + (1|ID), data = Predator) 
      summary(PredatorCC_pred_pop1)
      PredatorCC_pred_pop2 <- lmer(dS ~  relevel(pred_pop, ref = "snake.Hawaii") + (1|ID), data = Predator) 
      summary(PredatorCC_pred_pop2)
      
      # Fit the models so we get the estimate and corrected robust standard error for the contrasts we want to make
      robust_PredatorCC_pred_pop1 <- conf_int(PredatorCC_pred_pop1, cluster = Predator$ID, vcov="CR2")
      robust_PredatorCC_pred_pop2 <- conf_int(PredatorCC_pred_pop2, cluster = Predator$ID, vcov="CR2")
      
     # Contrasts hypothesis tests, but use RVE 
    result_pred <- Wald_test(PredatorCC_pred_pop, constraints = constrain_pairwise(c(1:4)), vcov="CR2", tidy = TRUE)
    result_pred$p_val_bonfer <- sapply(p.adjust(result_pred$p_val, "bonferroni"), function(x) p_value(x))
    
    
    # Get the contrasts and Wald stats for comparions
     CI_hyp_test <- data.frame(
              comparison = c("Snake (Hawaii-Kenya)", "Birds (Hawaii-Kenya)"), 
                Estimate = c(robust_PredatorCC_pred_pop2[4,2], robust_PredatorCC_pred_pop1[3, 2]),
                lwr = c(robust_PredatorCC_pred_pop2[4,5], robust_PredatorCC_pred_pop1[3, 5]),
                upr = c(robust_PredatorCC_pred_pop2[4,6], robust_PredatorCC_pred_pop1[3, 6]),
              Fstat = c(result_pred[5,3], result_pred[2,3]),
              df = c(result_pred[5,5], result_pred[2,5]),
              p_val = c(result_pred[5,7], result_pred[2,7]),
              p_bonf = c(result_pred[5,8], result_pred[2,8]))

      
#########################################################
# Local Adaptation: Testing for local adaptation
#########################################################
##################
# Social Context: Male-male competition
##################
DisplayBgrd <- read.table("./data/Display_5.csv", 
                        header=TRUE, sep=",", dec=".", strip.white=TRUE)
svl_dat <- read.csv("./data/SVL2.csv")

DisplayBgrd <- DisplayBgrd %>% 
                filter(!BodyRegion == "botflank") %>% 
                mutate(Background_pop = ifelse(Background2 == "own", Population, if_else(Background2 == "other" & Population == "Hawaii", "Kenya", "Hawaii"))) %>% 
                as.data.frame() #exclude bottom flank.
DisplayBgrd$BodyRegion <- factor(DisplayBgrd$BodyRegion) #updates levels for the variable BodyRegion

# Filter out only Hawaiian and add in SVL for IDs
DisplayBgrd <- DisplayBgrd %>% 
               filter(Population == "Hawaii") %>% 
               left_join(select(svl_dat, ID, SVL), by = "ID", keep = FALSE) %>% 
               mutate(sc_svl = scale(SVL, scale = FALSE)) %>% 
                filter(!is.na(SVL))

##########
# Achromatic Contrast - dL
##########

  # Here, we want to test if the conspicuousness of lizard signals is locally adapted to the specific background of the Kenyan and Hawaiian population. The predcition is simply that, if we were to place a Hawaiian animal against the Kenyan background that it would be far more conspicuous compared to if we place a Kenyan animal in a Hawaiian background. 

  # Fit the main effect model
  DispBgrdAC <- lmer(dL ~ sc_svl  + Background_pop  + (1|ID), data=DisplayBgrd)
  summary(DispBgrdAC)
  

  # Check robustness of results to non-independence using Robust Variance Estimator (RVE)
  robust_localAdapt_DispBgrdAC <- clubSandwich::conf_int(DispBgrdAC, cluster = DisplayBgrd$ID, vcov =  "CR2")

  # Add Wald test to test signifiacnce of each factor, background and population using roust variance
  
  background_AC_Disp <- Wald_test(DispBgrdAC, constraints = constrain_zero(3), 
                                  cluster = DisplayBgrd$ID, vcov = "CR2")
  
  svl_AC_Disp <- Wald_test(DispBgrdAC, constraints = constrain_zero(2), 
                                  cluster = DisplayBgrd$ID, vcov = "CR2")
  
 ##########
# Chromatic Contrast - dS
##########

 # Here, we want to test if the conspicuousness of lizard signals is locally adapted to the specific background of the Kenyan and Hawaiian population. The predcition is simply that, if we were to place a Hawaiian animal against the Kenyan background that it would be far more conspicuous compared to if we place a Kenyan animal in a Hawaiian background. 
   
    # Fit the main effect model
          DispBgrdCC <- lmer(dS ~ sc_svl + Background_pop  + (1|ID), data=DisplayBgrd)
          summary(DispBgrdCC)
 
    # Check robustness of results to non-indepenedence using Robust Variance Estimator (RVE)
      robust_DispBgrdCC <- clubSandwich::conf_int(DispBgrdCC, cluster = DisplayBgrd$ID, vcov =  "CR2")
      
    # Check significance of each factor
        background_CC_Disp <- Wald_test(DispBgrdCC, constraints = constrain_zero(3), 
                                  cluster = DisplayBgrd$ID, vcov = "CR2")
  
        svl_CC_Disp <- Wald_test(DispBgrdCC, constraints = constrain_zero(2), 
                                  cluster = DisplayBgrd$ID, vcov = "CR2")

###################
# Social Context: Courtship
##################
CourtshipBgrd <- read.table("./data/Courtship_5.csv", 
                        header=TRUE, sep=",", dec=".", strip.white=TRUE)

CourtshipBgrd <- CourtshipBgrd %>% 
                filter(!BodyRegion == "botflank") %>% 
                mutate(Background_pop = ifelse(Background2 == "own", Population, if_else(Background2 == "other" & Population == "Hawaii", "Kenya", "Hawaii"))) %>% 
                as.data.frame() #exclude bottom flank.
CourtshipBgrd$BodyRegion <- factor(CourtshipBgrd$BodyRegion)

# Filter out only Hawaiian and add in SVL for IDs
CourtshipBgrd <- CourtshipBgrd %>% 
               filter(Population == "Hawaii") %>% 
               left_join(select(svl_dat, ID, SVL), by = "ID", keep = FALSE) %>% 
               mutate(sc_svl = scale(SVL, scale = FALSE)) %>% 
               filter(!is.na(SVL))

##########
## Achromatic Contrast - dL
##########

 # Here, we want to test if the conspicuousness of lizard signals is locally adapted to the specific background of the Kenyan and Hawaiian population. The predcition is simply that, if we were to place a Hawaiian animal against the Kenyan background that it would be far more conspicuous compared to if we place a Kenyan animal in a Hawaiian background. 

  # Fit main effects model
    CourtshipAC <- lmer(dL ~ sc_svl  + Background_pop  + (1|ID), data=CourtshipBgrd)
    summary(CourtshipAC)

   # Check robustness of results to non-indepenedence using Robust Variance Estimator (RVE)
   robust_CourtshipAC <- clubSandwich::conf_int(CourtshipAC, cluster = CourtshipBgrd$ID, vcov =  "CR2")
   
   # Wald tests
           background_AC_Court <- Wald_test(CourtshipAC, constraints = constrain_zero(3), 
                                  cluster = CourtshipBgrd$ID, vcov = "CR2")
  
           svl_AC_Court <- Wald_test(CourtshipAC, constraints = constrain_zero(2), 
                                  cluster = CourtshipBgrd$ID, vcov = "CR2")


##########
# Chromatic - dS
##########
 # Here, we want to test if the conspicuousness of lizard signals is locally adapted to the specific background of the Kenyan and Hawaiian population. The predcition is simply that, if we were to place a Hawaiian animal against the Kenyan background that it would be far more conspicuous compared to if we place a Kenyan animal in a Hawaiian background. 


  # Fit main effect model
      CourtshipCC <- lmer(dS ~ sc_svl  + Background_pop  + (1|ID), data=CourtshipBgrd)
      summary(CourtshipCC)

  # Robust variance esatimation to correct SE's for fixed effects given spectral curves are used multiple times for generating JNDs for each individuals data. 
  robust_CourtshipCC <- clubSandwich::conf_int(CourtshipCC, cluster = CourtshipBgrd$ID, vcov =  "CR2")
  
  
   # Wald tests
           background_CC_Court <- Wald_test(CourtshipCC, constraints = constrain_zero(3), 
                                  cluster = CourtshipBgrd$ID, vcov = "CR2")
  
           svl_CC_Court <- Wald_test(CourtshipCC, constraints = constrain_zero(2), 
                                  cluster = CourtshipBgrd$ID, vcov = "CR2")


#########################################################***** NEW
# Background Analysis - Founder           
#########################################################***** NEW
DisplayBgrd <- read.table("./data/Display_5.csv", 
                        header=TRUE, sep=",", dec=".", strip.white=TRUE)
svl_dat <- read.csv("./data/SVL2.csv")

DisplayBgrd <- DisplayBgrd %>% 
                filter(!BodyRegion == "botflank") %>% 
                mutate(Background_pop = ifelse(Background2 == "own", Population, 
                                               if_else(Background2 == "other" & Population == "Hawaii", "Kenya", "Hawaii"))) %>%
                 as.data.frame() #exclude bottom flank.

DisplayBgrd$BodyRegion <- factor(DisplayBgrd$BodyRegion) #updates levels for the variable BodyRegion
           
DisplayBgrd <- DisplayBgrd %>% 
                mutate(pop_background = interaction(Population, Background_pop)) %>%  
                left_join(select(svl_dat, ID, SVL), by = "ID", keep = FALSE) %>% 
                mutate(sc_svl = scale(SVL, scale = FALSE)) %>% 
                filter(!is.na(SVL))

#unique(DisplayBgrd$pop_background)
###################################
# Male-male display
###################################
##########
# Achromatic Contrast - dL
##########
  # Fit the interaction model
  DispBgrdAC_s6 <- lmer(dL ~ sc_svl  + Population*Background_pop + (1|ID), data=DisplayBgrd)
  summary(DispBgrdAC_s6)
  
  # Interaction test. Does the difference between Hawaii and Kenya depend on the background. It shouldn't. Wald F-test
  F_AC_Disp_pop_bgk <- Wald_test(DispBgrdAC_s6, constraints = constrain_zero(5), 
                                  cluster = DisplayBgrd$ID, vcov = "CR2")
  
  # Reparameterise the interaction model to generate the contrasts. It's not surprising that there is no interaction because we are literatlly just shifting the environment. 
  DispBgrdAC_s6 <- lmer(dL ~ sc_svl  + relevel(pop_background, ref = "Hawaii.Kenya")  + (1|ID), data=DisplayBgrd)
  summary(DispBgrdAC_s6)
  
  # Check robustness of results to non-independence using Robust Variance Estimator (RVE)
  robust_localAdapt_DispBgrdAC_s6 <- clubSandwich::conf_int(DispBgrdAC_s6, cluster = DisplayBgrd$ID, vcov =  "CR2")

##########
# Chromatic Contrast - dS
##########
    # Fit the interaction  model
      DispBgrdCC_s6 <- lmer(dS ~ sc_svl + Population*Background_pop  + (1|ID), data=DisplayBgrd)
      summary(DispBgrdCC_s6)
    clubSandwich::conf_int(DispBgrdCC_s6, cluster = DisplayBgrd$ID, vcov =  "CR2")
    # Interaction test. Does the difference between Hawaii and Kenya depend on the background. It shouldn't. Wald F-test. Although, somthing might be up with this one - effect size tiny but goes from non-sig to signficant. 
      F_CC_Disp_pop_bgk <- Wald_test(DispBgrdCC_s6, constraints = constrain_zero(5), 
                                  cluster = DisplayBgrd$ID, vcov = "CR2")
  
      # Reparameterise the interaction model to generate the contrasts. It's not surprising that there is no interaction because we are literatlly just shifting the environment. 
    DispBgrdCC_s6 <- lmer(dS ~ sc_svl + relevel(pop_background, ref = "Hawaii.Kenya")  + (1|ID), data=DisplayBgrd)
          summary(DispBgrdCC_s6)
          
    # Check robustness of results to non-indepenedence using Robust Variance Estimator (RVE)
      robust_DispBgrdCC_s6 <- clubSandwich::conf_int(DispBgrdCC_s6, cluster = DisplayBgrd$ID, vcov =  "CR2")
      

###################
# Social Context: Courtship
##################
CourtshipBgrd <- read.table("./data/Courtship_5.csv", 
                        header=TRUE, sep=",", dec=".", strip.white=TRUE)

CourtshipBgrd <- CourtshipBgrd %>% 
                filter(!BodyRegion == "botflank") %>% 
                mutate(Background_pop = ifelse(Background2 == "own", Population, if_else(Background2 == "other" & Population == "Hawaii", "Kenya", "Hawaii"))) %>% 
                as.data.frame() #exclude bottom flank.
CourtshipBgrd$BodyRegion <- factor(CourtshipBgrd$BodyRegion)

# Filter out only Hawaiian and add in SVL for IDs
CourtshipBgrd <- CourtshipBgrd %>% 
               mutate(pop_background = interaction(Population, Background_pop)) %>%  
                left_join(select(svl_dat, ID, SVL), by = "ID", keep = FALSE) %>% 
               mutate(sc_svl = scale(SVL, scale = FALSE)) %>% 
                filter(!is.na(SVL))

##########
## Achromatic Contrast - dL
##########

  # Fit interaction model
    CourtshipAC_s6 <- lmer(dL ~ sc_svl  + Population*Background_pop  + (1|ID), data=CourtshipBgrd)
    summary(CourtshipAC_s6)

  # Interaction test. Does the difference between Hawaii and Kenya depend on the background. It shouldn't. Wald F-test
      F_AC_Court_pop_bgk <- Wald_test(CourtshipAC_s6, constraints = constrain_zero(5), 
                                  cluster = CourtshipBgrd$ID, vcov = "CR2")
    
  # Reparameterise the interaction model to generate the contrasts. It's not surprising that there is no interaction because we are literatlly just shifting the environment. 
    CourtshipAC_s6 <- lmer(dL ~ sc_svl  + relevel(pop_background, ref = "Hawaii.Kenya")  + (1|ID), data=CourtshipBgrd)
    summary(CourtshipAC_s6)  
      
   # Check robustness of results to non-indepenedence using Robust Variance Estimator (RVE)
   robust_CourtshipAC_s6 <- clubSandwich::conf_int(CourtshipAC_s6, cluster = CourtshipBgrd$ID, vcov =  "CR2")


##########
# Chromatic - dS
##########

  # Fit main effect model
      CourtshipCC_s6 <- lmer(dS ~ sc_svl  + Population*Background_pop  + (1|ID), data=CourtshipBgrd)
      summary(CourtshipCC_s6)

  # Interaction test. Does the difference between Hawaii and Kenya depend on the background. It shouldn't. Wald F-test
      F_CC_Court_pop_bgk <- Wald_test(CourtshipCC_s6, constraints = constrain_zero(5), 
                                  cluster = CourtshipBgrd$ID, vcov = "CR2")
  
  # Reparameterise the interaction model to generate the contrasts. It's not surprising that there is no interaction because we are literatlly just shifting the environment. 
      CourtshipCC_s6 <- lmer(dS ~ sc_svl  + relevel(pop_background, ref = "Hawaii.Kenya")  + (1|ID), data=CourtshipBgrd)
      summary(CourtshipCC_s6)
  
  # Robust variance esatimation to correct SE's for fixed effects given spectral curves are used multiple times for generating JNDs for each individuals data. 
  robust_CourtshipCC_s6 <- clubSandwich::conf_int(CourtshipCC_s6, cluster = CourtshipBgrd$ID, vcov =  "CR2")
      
#########################################################
# Comparing displays
#########################################################
 # Load data from Devi
       Display_bird_diff <- read_excel("data/Display_bird_diff.xlsx")
       
       Diff <- Display_bird_diff 

      names(Diff) # header rows R is reading

    Diff$BodyRegion <- factor(Diff$BodyRegion) #updates levels for the variable BodyRegion
    str(diff.difftime) #how R is reading the stucture of the object, double checking the levels for body region after updating levels
    Diff$ID <- factor(Diff$ID) #updates levels for the variable ID
    str(diff.difftime) #how R is reading the stucture of the object, double checking the levels for body region after updating levels

# Center SVL to make intercept interpretable
    Diff$sc_SVL <- with(Diff, scale(SVL, scale=FALSE))

# Filter missing SVL data
  Diff <- Diff %>% filter(!is.na(SVL)) # filter out misssing SVL data

# create as factor
    Diff$BodyRegion <- factor(Diff$BodyRegion)

# Sample sizes
    N_diff <- data.frame(Diff %>% group_by(Population) %>% summarise(n = length(unique(ID))))
    
##################
# Achromatic contrast
##################
    
# Fit model testing this idea above. First just test for body region interaction. But, needs RVE
    Diff_dL_BR_int <- lmer(dL_diff_abs ~ sc_SVL + Population + BodyRegion + BodyRegion:Population + (1|ID), data=Diff)
    summary(Diff_dL_BR_int)
    
    # Refit mdoel using robust standard errors
  robust_Diff_dL_BR_int <- clubSandwich::conf_int(Diff_dL_BR_int, cluster = Diff$ID, vcov =  "CR2") 
  
    # Significance of interaction
      robust_Diff_dl_int <- Wald_test(Diff_dL_BR_int, constraints = constrain_zero(7:9), 
                                    cluster = Diff$ID, vcov = "CR2")
# Main effects BR
    Diff_dL_BR_me <- lmer(dL_diff_abs ~ sc_SVL + Population + BodyRegion  + (1|ID), data=Diff)
    summary(Diff_dL_BR_me)
    
    # Sig of Body Region
        robust_Diff_dL_BR_me <- Wald_test(Diff_dL_BR_me, constraints = constrain_zero(4:6), 
                                    cluster = Diff$ID, vcov = "CR2")
  
# Then fit marginalised model so that body regions are averaged
  Diff_dL <- lmer(dL_diff_abs ~ sc_SVL + Population + (1|ID), data=Diff)
  summary(Diff_dL)

# Check residuals of model
 # hist(residuals(Diff_dL))
 # plot(Diff_dL) # Some patterns in residuals, but likely driven by the absolute. Note that results are essentially the same if using absolute or raw, signed values. 
  
# Refit mdoel using robust standard errors
  robust_Diff_dl_BR <- clubSandwich::conf_int(Diff_dL_BR_me, cluster = Diff$ID, vcov =  "CR2") 

# Wald tests - Test significance of pop dofferences and SVL differences averaging over body regions
   
  robust_Diff_dl_pop <- Wald_test(Diff_dL, constraints = constrain_zero(3), 
                                  cluster = Diff$ID, vcov = "CR2")
   robust_Diff_dl_svl <- Wald_test(Diff_dL, constraints = constrain_zero(2), 
                                  cluster = Diff$ID, vcov = "CR2")

##################
# Chromatic contrast
##################  
   
  # Fit model testing this idea above. First just test for body region interaction. But, needs RVE
    Diff_dS_BR_int <- lmer(dS_diff_abs ~ sc_SVL + Population + BodyRegion + BodyRegion:Population + (1|ID), data=Diff)
    summary(Diff_dS_BR_int)
    
        # Refit mdoel using robust standard errors
     robust_Diff_dS_BR_int <- clubSandwich::conf_int(Diff_dS_BR_int, cluster = Diff$ID, vcov =  "CR2") 
    
     # Significance of interaction
      robust_Diff_dS_int <- Wald_test(Diff_dS_BR_int, constraints = constrain_zero(7:9), 
                                    cluster = Diff$ID, vcov = "CR2")
  # Main effects BR
    Diff_dS_BR_me <- lmer(dS_diff_abs ~ sc_SVL + Population + BodyRegion  + (1|ID), data=Diff)
    summary(Diff_dS_BR_me)
    
    # Sig of Body Region
      robust_Diff_dS_BR_me <- Wald_test(Diff_dS_BR_me, constraints = constrain_zero(4:6), 
                                    cluster = Diff$ID, vcov = "CR2")
   
# Fit model testing this idea above
  Diff_dS <- lmer(dS_diff_abs ~  SVL + Population + (1|ID), data=Diff)
  summary(Diff_dS)

# Check residuals of model
  #hist(residuals(Diff_dS))
  #plot(Diff_dS) # Some patterns in residuals, but likely driven by the absolute. Note that results are essentially the same if using absolute or raw, signed values. 
  
# Refit mdoel using robust standard errors
  robust_Diff_dS <- clubSandwich::conf_int(Diff_dS_BR_me, cluster = Diff$ID, vcov =  "CR2") 

# Wald tests - Test significance of pop dofferences and SVL differences averaging over body regions
   robust_Diff_dS_pop <- Wald_test(Diff_dS, constraints = constrain_zero(3), 
                                  cluster = Diff$ID, vcov = "CR2")
   robust_Diff_dS_svl <- Wald_test(Diff_dS, constraints = constrain_zero(2), 
                                  cluster = Diff$ID, vcov = "CR2")
   
   
#########################################################***** NEW
# Background Analysis - Founder           
#########################################################***** NEW
DisplayBgrd <- read.table("./data/Display_5.csv", 
                        header=TRUE, sep=",", dec=".", strip.white=TRUE)
svl_dat <- read.csv("./data/SVL2.csv")

DisplayBgrd <- DisplayBgrd %>% 
                filter(!BodyRegion == "botflank") %>% 
                mutate(Background_pop = ifelse(Background2 == "own", Population, 
                                               if_else(Background2 == "other" & Population == "Hawaii", "Kenya", "Hawaii"))) %>%
                 as.data.frame() #exclude bottom flank.

DisplayBgrd$BodyRegion <- factor(DisplayBgrd$BodyRegion) #updates levels for the variable BodyRegion
           
DisplayBgrd <- DisplayBgrd %>% 
                mutate(pop_background = interaction(Population, Background_pop)) %>%  
                left_join(select(svl_dat, ID, SVL), by = "ID", keep = FALSE) %>% 
                mutate(sc_svl = scale(SVL, scale = FALSE)) %>% 
                filter(!is.na(SVL))

###################################
# Male-male display
###################################
##########
# Achromatic Contrast - dL
##########
  # Fit the interaction model
  DispBgrdAC_s6 <- lmer(dL ~ sc_svl  + Population*Background_pop + (1|ID), data=DisplayBgrd)
  summary(DispBgrdAC_s6)
  
  # Interaction test. Does the difference between Hawaii and Kenya depend on the background. It shouldn't. Wald F-test
  F_AC_Disp_pop_bgk <- Wald_test(DispBgrdAC_s6, constraints = constrain_zero(5), 
                                  cluster = DisplayBgrd$ID, vcov = "CR2")
  
  # Reparameterise the interaction model to generate the contrasts. It's not surprising that there is no interaction because we are literatlly just shifting the environment. 
  DispBgrdAC_s6 <- lmer(dL ~ sc_svl  + relevel(pop_background, ref = "Hawaii.Kenya")  + (1|ID), data=DisplayBgrd)
  summary(DispBgrdAC_s6)
  
  # Check robustness of results to non-independence using Robust Variance Estimator (RVE)
  robust_localAdapt_DispBgrdAC_s6 <- clubSandwich::conf_int(DispBgrdAC_s6, cluster = DisplayBgrd$ID, vcov =  "CR2")

##########
# Chromatic Contrast - dS
##########
    # Fit the interaction  model
      DispBgrdCC_s6 <- lmer(dS ~ sc_svl + Population*Background_pop  + (1|ID), data=DisplayBgrd)
      summary(DispBgrdCC_s6)
    clubSandwich::conf_int(DispBgrdCC_s6, cluster = DisplayBgrd$ID, vcov =  "CR2")
    # Interaction test. Does the difference between Hawaii and Kenya depend on the background. It shouldn't. Wald F-test. Although, somthing might be up with this one - effect size tiny but goes from non-sig to signficant. 
      F_CC_Disp_pop_bgk <- Wald_test(DispBgrdCC_s6, constraints = constrain_zero(5), 
                                  cluster = DisplayBgrd$ID, vcov = "CR2")
  
      # Reparameterise the interaction model to generate the contrasts. It's not surprising that there is no interaction because we are literatlly just shifting the environment. 
    DispBgrdCC_s6 <- lmer(dS ~ sc_svl + relevel(pop_background, ref = "Hawaii.Kenya")  + (1|ID), data=DisplayBgrd)
          summary(DispBgrdCC_s6)
          
    # Check robustness of results to non-indepenedence using Robust Variance Estimator (RVE)
      robust_DispBgrdCC_s6 <- clubSandwich::conf_int(DispBgrdCC_s6, cluster = DisplayBgrd$ID, vcov =  "CR2")
      

###################
# Social Context: Courtship
##################
CourtshipBgrd <- read.table("./data/Courtship_5.csv", 
                        header=TRUE, sep=",", dec=".", strip.white=TRUE)

CourtshipBgrd <- CourtshipBgrd %>% 
                filter(!BodyRegion == "botflank") %>% 
                mutate(Background_pop = ifelse(Background2 == "own", Population, if_else(Background2 == "other" & Population == "Hawaii", "Kenya", "Hawaii"))) %>% 
                as.data.frame() #exclude bottom flank.
CourtshipBgrd$BodyRegion <- factor(CourtshipBgrd$BodyRegion)

# Filter out only Hawaiian and add in SVL for IDs
CourtshipBgrd <- CourtshipBgrd %>% 
               mutate(pop_background = interaction(Population, Background_pop)) %>%  
                left_join(select(svl_dat, ID, SVL), by = "ID", keep = FALSE) %>% 
               mutate(sc_svl = scale(SVL, scale = FALSE)) %>% 
                filter(!is.na(SVL))

##########
## Achromatic Contrast - dL
##########

  # Fit interaction model
    CourtshipAC_s6 <- lmer(dL ~ sc_svl  + Population*Background_pop  + (1|ID), data=CourtshipBgrd)
    summary(CourtshipAC_s6)

  # Interaction test. Does the difference between Hawaii and Kenya depend on the background. It shouldn't. Wald F-test
      F_AC_Court_pop_bgk <- Wald_test(CourtshipAC_s6, constraints = constrain_zero(5), 
                                  cluster = CourtshipBgrd$ID, vcov = "CR2")
    
  # Reparameterise the interaction model to generate the contrasts. It's not surprising that there is no interaction because we are literatlly just shifting the environment. 
    CourtshipAC_s6 <- lmer(dL ~ sc_svl  + relevel(pop_background, ref = "Hawaii.Kenya")  + (1|ID), data=CourtshipBgrd)
    summary(CourtshipAC_s6)  
      
   # Check robustness of results to non-indepenedence using Robust Variance Estimator (RVE)
   robust_CourtshipAC_s6 <- clubSandwich::conf_int(CourtshipAC_s6, cluster = CourtshipBgrd$ID, vcov =  "CR2")


##########
# Chromatic - dS
##########

  # Fit main effect model
      CourtshipCC_s6 <- lmer(dS ~ sc_svl  + Population*Background_pop  + (1|ID), data=CourtshipBgrd)
      summary(CourtshipCC_s6)

  # Interaction test. Does the difference between Hawaii and Kenya depend on the background. It shouldn't. Wald F-test
      F_CC_Court_pop_bgk <- Wald_test(CourtshipCC_s6, constraints = constrain_zero(5), 
                                  cluster = CourtshipBgrd$ID, vcov = "CR2")
  
  # Reparameterise the interaction model to generate the contrasts. It's not surprising that there is no interaction because we are literatlly just shifting the environment. 
      CourtshipCC_s6 <- lmer(dS ~ sc_svl  + relevel(pop_background, ref = "Hawaii.Kenya")  + (1|ID), data=CourtshipBgrd)
      summary(CourtshipCC_s6)
  
  # Robust variance esatimation to correct SE's for fixed effects given spectral curves are used multiple times for generating JNDs for each individuals data. 
  robust_CourtshipCC_s6 <- clubSandwich::conf_int(CourtshipCC_s6, cluster = CourtshipBgrd$ID, vcov =  "CR2")
      


## ----figure1, cache=FALSE, fig.cap = "**Chameleon color signal change in response to different social stimuli. Male chameleons experience intense sexual selection**. During the breeding season they change from dull green to a highly conspicuous bright yellow display signal. They also readily fight by locking horns and sometimes pierce their rival’s skin with their horns. **a**, A dominant male in display coloration. **b**, A subordinate male that lost a contest and turned from bright yellow to brown. **c**, Two males fighting, both are in display coloration and relatively even matched. **d**, A courting male in full display color while the female has turned to a contrasting color, rejecting the male. See Supplementary information for additional photos, including in response to a snake. Photo credit: Martin Whiting, Macquarie University."----
library(magick)

img65 <- image_read("./output/figures/fig1_ms_Nature.png")
img65
  


## ----figure2, fig.width = 14, fig.height= 14, fig.cap="**Mean spectral reflectance curves for male chameleons for representative body regions (gular, top flank) and background (leaves) for Hawaii and Kenya**. The context for measurement was **a**, male contest displays; **b**, courtship; **c**, bird predator; and **d**, snake predator. More details in online Supplementary Information and Supplementary Fig. 5. Photo credit: Martin Whiting, Macquarie University."----
library(magick)
img <- image_read("./output/figures/Figure3_Nature.png")
img



## ----figure3, cache = FALSE, fig.width = 14, fig.height= 14, fig.cap= "**Chromatic and luminance contrast in Just Noticeable Differences (JND), of Hawaiian and Kenyan chameleons against their respective backgrounds (i.e., average environment of stems and leaves)**. Luminance **a**, and chromatic **b**, contrast of male chameleons during male-male contests and female courtship. JNDs are calculated based on the chameleon visual system. Luminance **c**, and chromatic **d**, contrast of male chameleons during snake and bird predator encounters. JNDs are calculated based on the snake and bird visual systems. Contrasts between means, ‘Beta’, that are bold indicate significant effects. Photo credit: Martin Whiting, Macquarie University."----
rerun = FALSE
if(rerun){
## Make figures that average across body regions, but display Social Context

courtship_data <- Courtship %>% 
                  select(ID, Population, Colour, SVL, BodyRegion, Background, dS, dL)
  display_data <- Display_1 %>%
                  select(ID, Population, Colour, SVL, BodyRegion, Background, dS, dL)
combined_data <- rbind(courtship_data, display_data)

# Now calculate the averages for each population and Colour (i.e., context) / predation

summary_data_pred <- Predator %>%
                group_by(Predator, Population) %>%
                summarise( Mean_dS = mean(dS, na.rm = T), 
                             SE_dS = sd(dS)/sqrt(length(unique(ID))),
                             Upper_dS = Mean_dS + 1.96*SE_dS,
                             Lower_dS = Mean_dS - 1.96*SE_dS, 
                           Mean_dL = mean(dL, na.rm = T), 
                             SE_dL = sd(dL)/sqrt(length(unique(ID))),
                             Upper_dL = Mean_dL + 1.96*SE_dL,
                             Lower_dL = Mean_dL - 1.96*SE_dL) 


summary_data <- combined_data %>%
                group_by(Population, Colour) %>%
                summarise( Mean_dS = mean(dS, na.rm = T), 
                             SE_dS = sd(dS)/sqrt(length(unique(ID))),
                             Upper_dS = Mean_dS + 1.96*SE_dS,
                             Lower_dS = Mean_dS - 1.96*SE_dS, 
                           Mean_dL = mean(dL, na.rm = T), 
                             SE_dL = sd(dL)/sqrt(length(unique(ID))),
                             Upper_dL = Mean_dL + 1.96*SE_dL,
                             Lower_dL = Mean_dL - 1.96*SE_dL) %>%
                mutate(Colour = if_else(Colour == "Courtship", "Courtship", "Male-male contest"))


#Grab photos
  males <- readPNG("./photos/male_fight.png", native = TRUE)
females <- readPNG("./photos/courtship.png",  native = TRUE)
  snake <- readPNG("./photos/snake_cham.png", native = TRUE)
   bird <- readPNG("./photos/bird_cham.png",  native = TRUE)

#plots
p1 <- ggplot(summary_data, aes(x = Population, y = Mean_dS, group = Colour, colour = Colour)) +
    ylim(2.5, 6.5) + 
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = Lower_dS, ymax = Upper_dS), position = position_dodge(width=0.5), width = 0) + 
  geom_line(position = position_dodge(width=0.5)) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("") + 
  ylab("Chromatic contrast (JND)") +
  annotate("segment", x = 0.85, xend = 1.85, y = 6.1, yend = 6.1, colour = "black") +
  annotate("segment", x = 1.12, xend = 2.12, y = 5.7, yend = 5.7, colour = "black") + 
  annotate("text", x = 0.85+(1.85-0.85)/2,  y = 6.2, label = "NS",  size = 6) + 
  annotate("text", x = 1.12+(2.12-1.12)/2, y = 5.8, label = "NS", size = 6) + 
  theme_bw() +
  labs(colour = "Social Context") +
  theme(legend.position = c(0.18, 0.08),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold")) +
   inset_element(males,
                left = 0.45,
                right = 0.90,
                bottom = 0.58,
                top = 0.75, ignore_tag = TRUE) + 
    inset_element(females,
                left = 0.30,
                right = 0.75,
                bottom = 0.15,
                top = 0.28, ignore_tag = TRUE) 
      #annotation_raster(males, xmin = 1.2, xmax = 1.9, ymin = 4.8, ymax = 5.3) + 
      #annotation_raster(females, xmin = 1, xmax = 1.7, ymin = 3.5, ymax = 4)

p2 <- ggplot(summary_data, aes(x = Population, y = Mean_dL, group = Colour, color = Colour)) +
  ylim(15, 45) + 
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = Lower_dL, ymax = Upper_dL), position = position_dodge(width=0.5), width = 0) + 
  geom_line(position = position_dodge(width=0.5)) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("") + 
  ylab("Luminance contrast (JND)") +
   annotate("segment", x = 0.90, xend = 1.90, y = 40, yend = 40, colour = "black") +
  annotate("segment", x = 1.12, xend = 2.12, y = 42, yend = 42, colour = "black") + 
  annotate("text", x = 0.9+((1.90 - 0.90)/2), y = 41, label = "**",  size = 6) + 
  annotate("text", x = 1.12+((2.12 - 1.12)/ 2), y = 43, label = "**", size = 6) + 
  theme_bw() +
  theme(legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold")) +
    inset_element(males,
                left = 0.45,
                right = 0.90,
                bottom = 0.60,
                top = 0.75, ignore_tag = TRUE) + 
    inset_element(females,
                left = 0.05,
                right = 0.50,
                bottom = 0.25,
                top = 0.38, ignore_tag = TRUE)


p3 <- ggplot(summary_data_pred, aes(x = Population, y = Mean_dL, group = Predator, color = Predator)) +
  ylim(10, 30) + 
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = Lower_dL, ymax = Upper_dL), position = position_dodge(width=0.5), width = 0) + 
  geom_line(position = position_dodge(width=0.5)) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("") + 
  ylab("Luminance contrast (JND)") +
   annotate("segment", x = 0.90, xend = 1.90, y = 28, yend = 28, colour = "black") +
  annotate("segment", x = 1.12, xend = 2.12, y = 26, yend = 26, colour = "black") + 
  annotate("text", x = 0.9+((1.90 - 0.90)/2), y = 28.5, label = "**",  size = 6) + 
  annotate("text", x = 1.12+((2.12 - 1.12)/ 2), y = 26.5, label = "**", size = 6) + 
  theme_bw() +
  theme(legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold")) +
    inset_element(snake,
                left = 0.45,
                right = 0.75,
                bottom = 0.60,
                top = 0.70, ignore_tag = TRUE) + 
    inset_element(bird,
                left = 0.05,
                right = 0.50,
                bottom = 0.25,
                top = 0.38, ignore_tag = TRUE)

p4 <- ggplot(summary_data_pred, aes(x = Population, y = Mean_dS, group = Predator, color = Predator)) +
  ylim(1, 9) + 
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = Lower_dS, ymax = Upper_dS), position = position_dodge(width=0.5), width = 0) + 
  geom_line(position = position_dodge(width=0.5)) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("") + 
  ylab("Chromatic contrast (JND)") +
   annotate("segment", x = 0.88, xend = 1.88, y = 8.8, yend = 8.8, colour = "black") +
  annotate("segment", x = 1.12, xend = 2.12, y =2.5, yend = 2.5, colour = "black") + 
  annotate("text", x = 0.9+((1.90 - 0.90)/2), y = 8.9, label = "**",  size = 6) + 
  annotate("text", x = 1.12+((2.12 - 1.12)/ 2), y = 2.7, label = "NS", size = 6) + 
  theme_bw() +
  theme(legend.position = c(0.10, 0.08),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold")) +
    inset_element(bird,
                left = 0.30,
                right = 0.60,
                bottom = 0.45,
                top = 0.60, ignore_tag = TRUE) + 
    inset_element(snake,
                left = 0.02,
                right = 0.32,
                bottom = 0.20,
                top = 0.35, ignore_tag = TRUE)

#pdf(file = "./output/figures/Figure1.1.pdf", width = 14, height = 14)
plot_grid(p1, p2, p4, p3, labels = c("A", "B", "C", "D"), label_size = 16,
  label_fontface = "bold", rel_widths=c(1,1))
#dev.off()
} else {
  
  img2 <- image_read("./output/figures/Figure1_ms_Nature.png")
  img2
}


## ----figure4, cache = FALSE, fig.width = 14, fig.height= 9, fig.cap= "**Evidence for local adaptation of social color signals in both a, chromatic- and b, luminance contrast for Hawaiian chameleons**. For local adaptation, signals are predicted to be more conspicuous against their own background. In this case, Hawaiian chameleons were both significantly more chromatic and brighter (luminance contrast) against their own background compared to the Kenyan background indicating increased local signal conspicuousness. Photo credit: Martin Whiting, Macquarie University."----
rerun=FALSE

if(rerun){
##########
# Data
##########
courtship_summary <- CourtshipBgrd %>% 
                    group_by(Population, Background_pop) %>%
                summarise( Mean_dS = mean(dS, na.rm = T), 
                             SE_dS = sd(dS)/sqrt(length(unique(ID))),
                             Upper_dS = Mean_dS + 1.96*SE_dS,
                             Lower_dS = Mean_dS - 1.96*SE_dS, 
                           Mean_dL = mean(dL, na.rm = T), 
                             SE_dL = sd(dL)/sqrt(length(unique(ID))),
                             Upper_dL = Mean_dL + 1.96*SE_dL,
                             Lower_dL = Mean_dL - 1.96*SE_dL) %>% filter(Population == "Hawaii") %>% as.data.frame() %>% mutate(Social_Context = "Courtship")

display_summary <- DisplayBgrd %>%
                group_by(Population, Background_pop) %>%
                summarise( Mean_dS = mean(dS, na.rm = T), 
                             SE_dS = sd(dS)/sqrt(length(unique(ID))),
                             Upper_dS = Mean_dS + 1.96*SE_dS,
                             Lower_dS = Mean_dS - 1.96*SE_dS, 
                           Mean_dL = mean(dL, na.rm = T), 
                             SE_dL = sd(dL)/sqrt(length(unique(ID))),
                             Upper_dL = Mean_dL + 1.96*SE_dL,
                             Lower_dL = Mean_dL - 1.96*SE_dL) %>% filter(Population == "Hawaii") %>% as.data.frame() %>% mutate(Social_Context = "Male-male contest")

summary_data_localAdap <- rbind(courtship_summary, display_summary)

##########
# Plots
##########
#Grab photos
  males <- readPNG("./photos/male_fight.png", native = TRUE)
females <- readPNG("./photos/courtship.png",  native = TRUE)

#plots
p1.2 <- ggplot(summary_data_localAdap, aes(x = Background_pop, y = Mean_dS, group = Social_Context, color = Social_Context)) +
    ylim(2.5, 6.5) + 
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = Lower_dS, ymax = Upper_dS), position = position_dodge(width=0.5), width = 0) + 
  geom_line(position = position_dodge(width=0.5)) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("Background Environment") + 
  ylab("Chromatic contrast (JND)") +
  annotate("segment", x = 0.85, xend = 1.85, y = 6.1, yend = 6.1, colour = "black") +
  annotate("segment", x = 1.12, xend = 2.12, y = 5.7, yend = 5.7, colour = "black") + 
  annotate("text", x = 0.85+(1.85-0.85)/2,  y = 6.2, label = "NS",  size = 6) + 
  annotate("text", x = 1.12+(2.12-1.12)/2, y = 5.8, label = "NS", size = 6) + 
  theme_bw() +
  labs(colour = "Social Context") +
  theme(legend.position = c(0.15, 0.08),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold")) +
   inset_element(males,
                left = 0.45,
                right = 0.90,
                bottom = 0.58,
                top = 0.75, ignore_tag = TRUE) + 
    inset_element(females,
                left = 0.30,
                right = 0.75,
                bottom = 0.15,
                top = 0.28, ignore_tag = TRUE) 

p2.2 <- ggplot(summary_data_localAdap, aes(x = Background_pop, y = Mean_dL, group = Social_Context, color = Social_Context)) +
  ylim(15, 45) + 
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = Lower_dL, ymax = Upper_dL), position = position_dodge(width=0.5), width = 0) + 
  geom_line(position = position_dodge(width=0.5)) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("Background Environment") + 
  ylab("Luminance contrast (JND)") +
   annotate("segment", x = 0.90, xend = 1.90, y = 40, yend = 40, colour = "black") +
  annotate("segment", x = 1.12, xend = 2.12, y = 42, yend = 42, colour = "black") + 
  annotate("text", x = 0.9+((1.90 - 0.90)/2), y = 41, label = "**",  size = 6) + 
  annotate("text", x = 1.12+((2.12 - 1.12)/ 2), y = 43, label = "**", size = 6) + 
  theme_bw() +
  theme(legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold")) +
    inset_element(males,
                left = 0.45,
                right = 0.90,
                bottom = 0.60,
                top = 0.75, ignore_tag = TRUE) + 
    inset_element(females,
                left = 0.05,
                right = 0.50,
                bottom = 0.25,
                top = 0.38, ignore_tag = TRUE)
#pdf(file="./output/figures/Figure2.pdf", height = 10, width = 18)
plot_grid(p1.2, p2.2, labels = c("A", "B"), label_size = 16,
  label_fontface = "bold", rel_widths=c(1,1))
#dev.off()
} else {
  img3 <- image_read("./output/figures/Figure2_ms_Nature.png")
  img3
}




## ----table4, results='hide'-------------------------------------------------------------------------------
#Section will create and write Tables for Supplement. It is not spit out in the main MS. Exported then imported for the supp

# Table S1
    table1 <- as.data.frame(rbind(robust_ConspicDisplayAC_CI,
                    robust_ConspicDisplayCC_CI,
                    robust_ConspicCourtshipAC_CI,
                    robust_ConspicCourtshipCC_CI))
    table1$Parameter <- rownames(table1)
    rownames(table1) <- NULL
    # Clean
     final_T1 <- table1 %>% 
                 mutate(SocialContext = c("Male-Male Display", rep("", 5), "Courtship Display", rep("", 5)),
                        VisualRealm = c("Luminance  Contrast", rep("", 2), "Chromatic Contrast", rep("", 2), 
                                        "Luminance Contrast", rep("", 2), "Chromatic Contrast", rep("", 2))) %>%
                  select(SocialContext, VisualRealm, Parameter, beta, SE, df, CI_L, CI_U)
     write.csv(final_T1, file = "./output/tables/Table1_MS.csv", row.names = FALSE)
 
# Table S2
    table2 <- as.data.frame(rbind(robust_PredatorAC_CI_inter,
                                  robust_PredatorCC_CI_inter))
    table2$Parameter <- rownames(table2)
    rownames(table2) <- NULL
    
    # Clean
     final_T2 <- table2 %>% 
                 mutate(JND = c("Luminance Contrast", rep("", 3), "Chromatic Contrast", rep("", 3))) %>%
                  select(JND, Parameter, beta, SE, df, CI_L, CI_U)
     write.csv(final_T2, file = "./output/tables/Table2_MS.csv", row.names = FALSE)

 #Table S3
    contrast <- rbind(CI_hyp_test,
                     CI_hyp_testAC)
    
    contrast <- contrast %>% mutate(VisualSpec = c("Chromatic Contrast", "", "Luminance Contrast", ""),
                                    p_val = sapply(p_val, function(x) p_value(x))) %>% 
                select(VisualSpec, comparison, Estimate, lwr, upr, Fstat, df, p_val, p_bonf)
    
    write.csv(contrast, file = "./output/tables/Table3_MS.csv", row.names = FALSE)


#TableS4
    tb4 <- data.frame(rbind(robust_localAdapt_DispBgrdAC,
                 robust_DispBgrdCC,
                 robust_CourtshipAC,
                 robust_CourtshipCC))
    tb4$parameter <- rownames(tb4)
    rownames(tb4) <- NULL
    
    tb4 <- tb4 %>% mutate(SocialContext = c("Male-Male Display", rep("", 5), 
                                            "Courtship Display", rep("", 5)),
                        VisualRealm = c("Luminance Contrast", rep("", 2), "Chromatic Contrast", 
                                        rep("", 2), "Luminance Contrast", rep("", 2), 
                                        "Chromatic Contrast", rep("", 2))) %>%
                  select(SocialContext, VisualRealm, parameter, beta, SE, df, CI_L, CI_U)
    
    write.csv(tb4, file = "./output/tables/Table4_MS.csv", row.names = FALSE)


# Table S5
    tb5 <- data.frame(rbind(robust_Diff_dL_BR_int,
                            robust_Diff_dS_BR_int))
    tb5$parameter <- rownames(tb5)
    rownames(tb5) <- NULL
    
  tb5 <- tb5 %>% mutate(VisualRealm = c("Luminance Contrast", rep("", 8), "Chromatic Contrast", 
                                        rep("", 8))) %>%
                  select(VisualRealm, parameter, beta, SE, df, CI_L, CI_U)
     write.csv(tb5, file = "./output/tables/Table5_MS.csv", row.names = FALSE)
  
# Table S6
tableS6 <- as.data.frame(rbind(robust_localAdapt_DispBgrdAC_s6,
                               robust_DispBgrdCC_s6,
                               robust_CourtshipAC_s6,
                              robust_CourtshipCC_s6))
tableS6$Parameter <- rownames(tableS6)
rownames(tableS6) <- NULL

# Clean
final_S6 <- tableS6 %>% 
  mutate(SocialContext = c("Male-Male Display", rep("", 9), "Courtship Display", rep("", 9)),
         VisualRealm = c("Luminance  Contrast", rep("", 4), "Chromatic Contrast", rep("", 4), 
                         "Luminance Contrast", rep("", 4), "Chromatic Contrast", rep("", 4))) %>%
  dplyr::select(SocialContext, VisualRealm, Parameter, beta, SE, df, CI_L, CI_U)
write.csv(final_S6, file = "./output/tables/tableS6.csv", row.names = FALSE)
     
     

