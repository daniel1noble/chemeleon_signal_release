## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, cache = TRUE, message = FALSE, warning = FALSE, tidy = TRUE, fig.width = 10)
## numbers >= 10^5 will be denoted in scientific notation,      ## numbers >= 10^5 will be denoted in scientific notation,
  ## and rounded to 2 digits      ## and rounded to 2 digits
  options(digits = 2)


## ----getlibrary, echo = FALSE, results = "hide"---------------------------------------------------------------------------------------
# load libraries
devtools::install_github("karthik/wesanderson")
pacman::p_load(lme4, lmerTest, ggplot2, lsmeans, emmeans, multcomp, dplyr, wesanderson, patchwork, latex2exp, png, cowplot, magick, jpeg, clubSandwich, flextable)
source("./R/func.R")


## ----display, results = "hide"--------------------------------------------------------------------------------------------------------
source("./R/func.R")
#########################################################
# Male-male Competition Displays
#########################################################
  # Load data and clean a bit
  Display_1 <- read.table("./data/Display_1.csv", 
                            header=TRUE, sep=",", dec=".", strip.white=TRUE)
  
  # Exclude bottom flank.
  Display_1 <- Display_1 %>% 
                filter(!BodyRegion == "botflank") %>% as.data.frame() 
  
  # Create as factor
      Display_1$BodyRegion <- factor(Display_1$BodyRegion)
      
  # Center SVL to make intercept interpretable
  Display_1$sc_SVL <- with(Display_1, scale(SVL, scale=FALSE))


##########
## Achromatic Contrast - dL
##########

    # Fit the model that estimates changes across body regions
    ConspicDisplayAC <- lmer(dL ~ sc_SVL + Population + BodyRegion + 
                               Population:BodyRegion + (1|ID), data=Display_1)
    summary(ConspicDisplayAC)
    ConspicDisplayAC_main <- lmer(dL ~ sc_SVL + Population + BodyRegion +  (1|ID), data=Display_1)
    
    # Wald Table - Test for interaction, plus refit model and test sig of each factor. Uses RVEs
    wald_table_ConspicDisplayAC <-  wald_table(model_main =  ConspicDisplayAC_main , model_inter = ConspicDisplayAC)

    # Robust variance estimators (RVE). Mostly all consistent
    Display_subset <- Display_1 %>% filter(!is.na(sc_SVL), !is.na(Population), !is.na(BodyRegion))
    robust_ConspicDisplayAC <- coef_test(ConspicDisplayAC, cluster = Display_subset$ID, vcov="CR2")
    
    # Fit model for linear hypothesis tests
           Display_1$PopBRInt <- interaction(Display_1$Population,Display_1$BodyRegion)
            ConspicDisplayAC2 <- lmer(dL ~ -1 + sc_SVL + PopBRInt + (1|ID), data=Display_1)
            summary(ConspicDisplayAC2)
    
     # Fit RVE
      robust_ConspicDisplayAC2 <- coef_test(ConspicDisplayAC2, cluster = Display_subset$ID, vcov="CR2")
      
     # Contrasts hypothesis tests, but use RVE 
    result1 <- Wald_test(ConspicDisplayAC2, constraints = constrain_pairwise(c(2:9)), vcov="CR2", tidy = TRUE)
    result1$p_val_bonfer <- sapply(p.adjust(result1$p_val, "bonferroni"), function(x) p_value(x))

##########
## Chromatic Contrast - dL
##########

 # Fit the model that esatimtes changes across body regions
    ConspicDisplayCC <- lmer(dS ~ sc_SVL + Population + BodyRegion + Population:BodyRegion + (1|ID), data=Display_1)
    ConspicDisplayCC_main <- lmer(dS ~ sc_SVL + Population + BodyRegion + (1|ID), data=Display_1)
    
    
# Wald Table - Test for interaction, plus refit model and test sig of each factor. Uses RVEs
    wald_table_ConspicDisplayCC <-  wald_table(model_main =  ConspicDisplayCC_main, model_inter = ConspicDisplayCC)
    
 # Robust variance estimators (RVE). Mostly all consistent
             Display_subset <- Display_1 %>% filter(!is.na(sc_SVL), !is.na(Population), !is.na(BodyRegion))
    robust_ConspicDisplayCC <- coef_test(ConspicDisplayCC, cluster = Display_subset$ID, vcov="CR2")    

  # Fit model for linear hypothesis tests
    Display_1$PopBRInt <- interaction(Display_1$Population,Display_1$BodyRegion)
     ConspicDisplayCC2 <- lmer(dS ~ -1 + sc_SVL + PopBRInt + (1|ID), data=Display_1)
     summary(ConspicDisplayCC2)
 
 # Contrasts hypothesis tests, but use RVE 
                 result2 <- Wald_test(ConspicDisplayCC2, constraints = constrain_pairwise(c(2:9)), vcov="CR2", tidy = TRUE)
    result2$p_val_bonfer <- sapply(p.adjust(result2$p_val, "bonferroni"), function(x) p_value(x))

#########################################################
# Courtship Displays
#########################################################

Courtship <- read.table("./data/Courtship_1.2.csv", 
                          header=TRUE, sep=",", dec=".", strip.white=TRUE)

# Center SVL to make intercept interpretable
Courtship$sc_SVL <- with(Courtship, scale(SVL, scale=FALSE))


##########
## Achromatic Contrast - dL
##########
  # Fit model
    ConspicCourtshipAC <- lmer(dL ~ sc_SVL + Population + BodyRegion + 
                                 Population:BodyRegion + (1|ID), data=Courtship)
    summary(ConspicCourtshipAC)
    
  # Main effects Model
    ConspicCourtshipAC_main <- lmer(dL ~ sc_SVL + Population + BodyRegion + (1|ID), data=Courtship)

   # Wald Table - Test for interaction, plus refit model and test sig of each factor. Uses RVEs
    wald_table_ConspicCourtshipAC <-  wald_table(model_main =  ConspicCourtshipAC_main, model_inter = ConspicCourtshipAC)  
  # Robust variance estimators (RVE). Mostly all consistent
             Courtship_subset <- Courtship %>% filter(!is.na(sc_SVL), !is.na(Population), !is.na(BodyRegion))
    robust_ConspicCourtshipAC <- coef_test(ConspicCourtshipAC, cluster = Courtship_subset$ID, vcov="CR2")    
      
  # Refit model for contrasts
     Courtship$PopBRInt <- interaction(Courtship$Population,Courtship$BodyRegion)
    ConspicCourtshipAC2 <- lmer(dL ~ -1 + sc_SVL + PopBRInt + (1|ID), data=Courtship)

  # Contrasts hypothesis tests, but use RVE 
                 result3 <- Wald_test(ConspicCourtshipAC2, constraints = constrain_pairwise(c(2:9)), vcov="CR2", tidy = TRUE)
    result3$p_val_bonfer <- sapply(p.adjust(result3$p_val, "bonferroni"), function(x) p_value(x))

##########
## Chromatic Contrast - dL
#########
    
    # Fit model, interaction
        ConspicCourtshipCC <- lmer(dS ~ sc_SVL + Population + BodyRegion + Population:BodyRegion + (1|ID), data=Courtship)
    
    # Fit main effects
    ConspicCourtshipCC_main <- lmer(dS ~ sc_SVL + Population + BodyRegion + (1|ID), data=Courtship)
    
    # Wald Table - Test for interaction, plus refit model and test sig of each factor. Uses RVEs
    wald_table_ConspicCourtshipCC <-  wald_table(model_main =  ConspicCourtshipCC_main, model_inter = ConspicCourtshipCC)

    # Robust variance estimators (RVE). Mostly all consistent
             Courtship_subset <- Courtship %>% filter(!is.na(sc_SVL), !is.na(Population), !is.na(BodyRegion))
    robust_ConspicCourtshipCC <- coef_test(ConspicCourtshipCC, cluster = Courtship_subset$ID, vcov="CR2")    
    
    # Refit model to get the contrasts between coefficienst we want
     Courtship$PopBRInt <- interaction(Courtship$Population,Courtship$BodyRegion)
    ConspicCourtshipCC2 <- lmer(dS ~ -1 + sc_SVL + PopBRInt + (1|ID), data=Courtship)
    
    # Contrasts hypothesis tests, but use RVE 
            result4 <- Wald_test(ConspicCourtshipCC2, 
                                 constraints = constrain_pairwise(c(2:9)), 
                                 vcov="CR2", tidy = TRUE)
      string <- c("PopBRIntKenya.gular - PopBRIntHawaii.gular|PopBRIntKenya.midflank - PopBRIntHawaii.midflank|PopBRIntKenya.tailbase - PopBRIntHawaii.tailbase|PopBRIntKenya.topflank - PopBRIntHawaii.topflank")
      result4 <- result4[grep(string, result4$hypothesis),]
    result4$p_val_bonfer <- sapply(p.adjust(result4$p_val, "bonferroni"), function(x) p_value(x))
    

#########################################################
# Displays to Predators, birds and snake
#########################################################
   Predator <- read.table("./data/Predator_2_3.csv", 
                         header=TRUE, sep=",", dec=".", strip.white=TRUE)
   
  # Clean up
             Predator <- Predator %>% filter(!BodyRegion == "botflank" & !BodyRegion =='gularbot') %>% as.data.frame() #exclude bottom flank.
  Predator$BodyRegion <- factor(Predator$BodyRegion) #updates levels for the variable BodyRegion

##########
## Achromatic Contrast - dL
##########

   # Fit model
      PredatorAC <- lmer(dL ~ Predator + Population + BodyRegion + Predator:BodyRegion + Predator:Population + 
                           (1|ID), data=Predator)
      summary(PredatorAC)
  
  PredatorAC_main <- lmer(dL ~ Predator + Population + BodyRegion  + (1|ID), data=Predator)
      
  # Wald table RVE
    
    Wald_table_PredatorAC <- wald_table_p(PredatorAC_main, PredatorAC)
  

   # Robust variance estimators (RVE). Mostly all consistent
             Predator_subset <- Predator %>% filter(!is.na(Population), !is.na(BodyRegion))
           robust_PredatorAC <- coef_test(PredatorAC, cluster = Predator_subset$ID, vcov="CR2")    

  # Fit model for linear hypothesis tests
    Predator$PopBRInt <- interaction(Predator$Population,Predator$Predator, Predator$BodyRegion)
     PredatorAC2 <- lmer(dL ~ -1 + PopBRInt + (1|ID), data=Predator)
     summary(PredatorAC2)
 
 # Contrasts hypothesis tests, but use RVE 
                 result5 <- Wald_test(PredatorAC2, constraints = constrain_pairwise(c(1:16)), vcov="CR2", tidy = TRUE)
 string <- c("PopBRIntKenya.snake.midflank - PopBRIntHawaii.snake.midflank|PopBRIntKenya.snake.gular - PopBRIntHawaii.snake.gular|PopBRIntKenya.snake.tailbase - PopBRIntHawaii.snake.tailbase|PopBRIntKenya.snake.topflank - PopBRIntHawaii.snake.topflank|PopBRIntKenya.bird.midflank - PopBRIntHawaii.bird.midflank|PopBRIntKenya.bird.gular - PopBRIntHawaii.bird.gular|PopBRIntKenya.bird.tailbase - PopBRIntHawaii.bird.tailbase|PopBRIntKenya.bird.topflank - PopBRIntHawaii.bird.topflank")
    result5 <- result5[grep(string, result5$hypothesis),]
    result5$p_val_bonfer <- sapply(p.adjust(result5$p_val, "bonferroni"), function(x) p_value(x))
    

##########
## Chromatic Contrast - dS
########

  # Fit Model
        PredatorCC <- lmer(dS ~ Predator + Population + BodyRegion + Predator:BodyRegion + Predator:Population + (1|ID), data=Predator)
        summary(PredatorCC)
          anova(PredatorAC)
          
    PredatorCC_main <- lmer(dS ~ Predator + Population + BodyRegion  + (1|ID), data=Predator)            
  
  # Wald table RVE
    
    Wald_table_PredatorCC <- wald_table_p(PredatorCC_main, PredatorCC)
  
  # Robust variance estimators (RVE). Mostly all consistent
             Predator_subset <- Predator %>% filter(!is.na(Population), !is.na(BodyRegion))
           robust_PredatorCC <- coef_test(PredatorCC, cluster = Predator_subset$ID, vcov="CR2")    
  
  # Fit model for linear hypothesis tests
    Predator$PopBRInt <- interaction(Predator$Population,Predator$Predator, Predator$BodyRegion)
          PredatorCC2 <- lmer(dS ~ -1 + PopBRInt + (1|ID), data=Predator)
          summary(PredatorCC2)
 
 # Contrasts hypothesis tests, but use RVE 
                 result6 <- Wald_test(PredatorCC2, constraints = constrain_pairwise(c(1:16), with_zero = TRUE), vcov="CR2", tidy = TRUE)
                   string <- c("PopBRIntKenya.snake.midflank - PopBRIntHawaii.snake.midflank|PopBRIntKenya.snake.gular - PopBRIntHawaii.snake.gular|PopBRIntKenya.snake.tailbase - PopBRIntHawaii.snake.tailbase|PopBRIntKenya.snake.topflank - PopBRIntHawaii.snake.topflank|PopBRIntKenya.bird.midflank - PopBRIntHawaii.bird.midflank|PopBRIntKenya.bird.gular - PopBRIntHawaii.bird.gular|PopBRIntKenya.bird.tailbase - PopBRIntHawaii.bird.tailbase|PopBRIntKenya.bird.topflank - PopBRIntHawaii.bird.topflank")
    result6 <- result6[grep(string, result6$hypothesis),]
    result6$p_val_bonfer <- sapply(p.adjust(result6$p_val, "bonferroni"), function(x) p_value(x))
    
  
  
#########################################################
# Testing local adaptation contrasting backgrounds between hawaii and Kenya
#########################################################
##################
# Social Context: Male-male competition
##################
DisplayBgrd <- read.table("./data/Display_5.csv", 
                        header=TRUE, sep=",", dec=".", strip.white=TRUE)

DisplayBgrd <- DisplayBgrd %>% 
                filter(!BodyRegion == "botflank") %>% 
                mutate(Background_pop = ifelse(Background2 == "own", Population, if_else(Background2 == "other" & Population == "Hawaii", "Kenya", "Hawaii"))) %>% 
                as.data.frame() #exclude bottom flank.
DisplayBgrd$BodyRegion <- factor(DisplayBgrd$BodyRegion) #updates levels for the variable BodyRegion

##########
# Achromatic Contrast - dL
##########

  # Here, we want to test if the conspicuousness of lizard signals is locally adapted to the specific background of the Kenyan and Hawaiian population. The predcition is simply that, if we were to place a Hawaiian animal against the Kenyan background that it would be far more conspicuous compared to if we place a Kenyan animal in a Hawaiian background. 

  # Fit the interaction model
  DispBgrdAC_inter <- lmer(dL ~ Population  + Background_pop + Population:Background_pop + (1|ID), data=DisplayBgrd)
  summary(DispBgrdAC_inter)
  
  # Fit the main effect model
  DispBgrdAC <- lmer(dL ~ Population  + Background_pop  + (1|ID), data=DisplayBgrd)
  summary(DispBgrdAC)
  
  # Doesn't look like an interaction, so test.
  anova(DispBgrdAC_inter, DispBgrdAC)

  # Check robustness of results to non-indepenedence using Robust Variance Estimator (RVE)
  robust_localAdapt <- clubSandwich::conf_int(DispBgrdAC, cluster = DisplayBgrd$ID, vcov =  "CR2")

  # Add Wald test to test signifiacnce of each factor, background and population using roust variance
  background_AC_Disp <- Wald_test(DispBgrdAC, constraints = constrain_zero(3), 
                                  cluster = DisplayBgrd$ID, vcov = "CR2")
  
  Population_AC_Disp <- Wald_test(DispBgrdAC, constraints = constrain_zero(2), 
                                  cluster = DisplayBgrd$ID, vcov = "CR2")
  
  # Planned contrast. Fit the model in a slightly different way
  DisplayBgrd$pop_back <- with(DisplayBgrd, interaction(Population, Background_pop))
  DisplayBgrd_pop_back <- lmer(dL ~ pop_back + (1|ID), data = DisplayBgrd) 
  summary(DisplayBgrd_pop_back)
  
  robust_DisplayBgrd_pop_back <- clubSandwich::conf_int(DisplayBgrd_pop_back, cluster = DisplayBgrd$ID, vcov =  "CR2")
   
  # Contrasts hypothesis tests, but use RVE 
    result_DisplayBgrd_pop_back <- Wald_test(DisplayBgrd_pop_back, constraints = constrain_pairwise(c(1:4)), vcov="CR2", tidy = TRUE)
    result_DisplayBgrd_pop_back$p_val_bonfer <- sapply(p.adjust(result_DisplayBgrd_pop_back$p_val, "bonferroni"), function(x) p_value(x))
  
  # Grab contrasts that we need from the robust esatimtes
  p_displ_AC <- result_DisplayBgrd_pop_back[2,8]
   CI_dis_AC <- data.frame(Estimate=robust_DisplayBgrd_pop_back$beta[3],
                           lwr = robust_DisplayBgrd_pop_back$CI_L[3],
                           upr = robust_DisplayBgrd_pop_back$CI_U[3])
     
##########
# Chromatic Contrast - dS
##########

 # Here, we want to test if the conspicuousness of lizard signals is locally adapted to the specific background of the Kenyan and Hawaiian population. The predcition is simply that, if we were to place a Hawaiian animal against the Kenyan background that it would be far more conspicuous compared to if we place a Kenyan animal in a Hawaiian background. 

   # Fit the interaction model
    DispBgrdCC_inter <- lmer(dS ~ Population  + Background_pop + Population:Background_pop + (1|ID), data=DisplayBgrd)
    summary(DispBgrdCC_inter)
   
    # Fit the main effect model
          DispBgrdCC <- lmer(dS ~ Population  + Background_pop  + (1|ID), data=DisplayBgrd)
          summary(DispBgrdCC)
          
    # Looks like weak evidence for interaction    
        anova(DispBgrdCC_inter, DispBgrdCC)

    # Check robustness of results to non-indepenedence using Robust Variance Estimator (RVE)
      robust_DispBgrdCC <- clubSandwich::coef_test(DispBgrdCC, cluster = DisplayBgrd$ID, vcov =  "CR2")

    # Check significance of each factor
        background_CC_Disp <- Wald_test(DispBgrdCC, constraints = constrain_zero(3), 
                                  cluster = DisplayBgrd$ID, vcov = "CR2")
  
  Population_CC_Disp <- Wald_test(DispBgrdCC, constraints = constrain_zero(2), 
                                  cluster = DisplayBgrd$ID, vcov = "CR2")
      
    # Planned contrasts. Refit model in slight different form
      DisplayBgrd$pop_back <- with(DisplayBgrd, interaction(Population, Background_pop))
    DisplayBgrd_pop_backCC <- lmer(dS ~ pop_back + (1|ID), data = DisplayBgrd) 

    # Fit Robust model
    robust_DisplayBgrd_pop_backCC <- clubSandwich::conf_int(DisplayBgrd_pop_backCC, cluster = DisplayBgrd$ID, vcov =  "CR2")
   
    # Contrasts hypothesis tests, but use RVE 
      result_DisplayBgrd_pop_backCC <- Wald_test(DisplayBgrd_pop_backCC, constraints = constrain_pairwise(c(1:4)), vcov="CR2", tidy = TRUE)
      result_DisplayBgrd_pop_backCC$p_val_bonfer <- sapply(p.adjust(result_DisplayBgrd_pop_back$p_val, "bonferroni"), function(x) p_value(x))
    
    # Grab contrasts that we need from the robust esatimtes
      p_displ_CC <- result_DisplayBgrd_pop_backCC[2,8]
       CI_Disp_CC <- data.frame(Estimate=robust_DisplayBgrd_pop_backCC$beta[3],
                               lwr = robust_DisplayBgrd_pop_backCC$CI_L[3],
                               upr = robust_DisplayBgrd_pop_backCC$CI_U[3])

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

##########
## Achromatic Contrast - dL
##########

 # Here, we want to test if the conspicuousness of lizard signals is locally adapted to the specific background of the Kenyan and Hawaiian population. The predcition is simply that, if we were to place a Hawaiian animal against the Kenyan background that it would be far more conspicuous compared to if we place a Kenyan animal in a Hawaiian background. 

  # Fit the interaction model
    CourtshipAC_inter <- lmer(dL ~ Population  + Background_pop + Population:Background_pop + (1|ID), data=CourtshipBgrd)

  # Fit main effects model
    CourtshipAC <- lmer(dL ~ Population  + Background_pop  + (1|ID), data=CourtshipBgrd)
    summary(CourtshipAC)

  # Interaction needed? No
    anova(CourtshipAC_inter, CourtshipAC)

   # Check robustness of results to non-indepenedence using Robust Variance Estimator (RVE)
   robust_CourtshipAC <- clubSandwich::coef_test(CourtshipAC, cluster = CourtshipBgrd$ID, vcov =  "CR2")
   
   # Wald tests
           background_AC_Court <- Wald_test(CourtshipAC, constraints = constrain_zero(3), 
                                  cluster = CourtshipBgrd$ID, vcov = "CR2")
  
           Population_AC_Court <- Wald_test(CourtshipAC, constraints = constrain_zero(2), 
                                  cluster = CourtshipBgrd$ID, vcov = "CR2")

  # Planned contrasts / hypothesis tests. Need to fit model a little differently
    CourtshipBgrd$pop_back <- with(CourtshipBgrd, interaction(Population, Background_pop))
    CourtshipBgrd_pop_back <- lmer(dL ~ pop_back + (1|ID), data = CourtshipBgrd) 
    
   # Fit Robust model
    robust_CourtshipBgrd_pop_back <- clubSandwich::conf_int(CourtshipBgrd_pop_back, cluster = CourtshipBgrd$ID, vcov =  "CR2")
   
    # Contrasts hypothesis tests, but use RVE 
      result_CourtshipBgrd_pop_back <- Wald_test(CourtshipBgrd_pop_back, constraints = constrain_pairwise(c(1:4)), vcov="CR2", tidy = TRUE)
      result_CourtshipBgrd_pop_back$p_val_bonfer <- sapply(p.adjust(result_CourtshipBgrd_pop_back$p_val, "bonferroni"), function(x) p_value(x))
    
    # Grab contrasts that we need from the robust esatimtes
      p_courtship_AC <- result_CourtshipBgrd_pop_back[2,8]
       CI_AC_Courtship <- data.frame(Estimate=robust_CourtshipBgrd_pop_back$beta[3],
                               lwr = robust_CourtshipBgrd_pop_back$CI_L[3],
                               upr = robust_CourtshipBgrd_pop_back$CI_U[3])
    
##########
# Chromatic - dS
##########
 # Here, we want to test if the conspicuousness of lizard signals is locally adapted to the specific background of the Kenyan and Hawaiian population. The predcition is simply that, if we were to place a Hawaiian animal against the Kenyan background that it would be far more conspicuous compared to if we place a Kenyan animal in a Hawaiian background. 

  # Fit interaction model
    CourtshipCC_inter <- lmer(dS ~ Population  + Background_pop + Population:Background_pop + (1|ID), data=CourtshipBgrd)

  # Fit main effect model
      CourtshipCC <- lmer(dS ~ Population  + Background_pop  + (1|ID), data=CourtshipBgrd)
      summary(CourtshipCC)
      anova(CourtshipCC)

  # Test whether interaction is supported
    anova(CourtshipCC_inter, CourtshipCC)

  # Robust variance esatimation to correct SE's for fixed effects given spectral curves are used multiple times for generating JNDs for each individuals data. 
  robust_CourtshipCC <- clubSandwich::coef_test(CourtshipCC, cluster = CourtshipBgrd$ID, vcov =  "CR2")
  
  
   # Wald tests
           background_CC_Court <- Wald_test(CourtshipCC, constraints = constrain_zero(3), 
                                  cluster = CourtshipBgrd$ID, vcov = "CR2")
  
           Population_CC_Court <- Wald_test(CourtshipCC, constraints = constrain_zero(2), 
                                  cluster = CourtshipBgrd$ID, vcov = "CR2")

  # Planned comparions
    CourtshipBgrd$pop_back <- with(CourtshipBgrd, interaction(Population, Background_pop))
  CourtshipBgrd_pop_backCC <- lmer(dS ~ pop_back + (1|ID), data = CourtshipBgrd) 

  
     # Fit Robust model
    robust_CourtshipBgrd_pop_backCC <- clubSandwich::conf_int(CourtshipBgrd_pop_backCC, cluster = CourtshipBgrd$ID, vcov =  "CR2")
   
    # Contrasts hypothesis tests, but use RVE 
      result_CourtshipBgrd_pop_backCC <- Wald_test(CourtshipBgrd_pop_backCC, constraints = constrain_pairwise(c(1:4)), vcov="CR2", tidy = TRUE)
      result_CourtshipBgrd_pop_backCC$p_val_bonfer <- sapply(p.adjust(result_CourtshipBgrd_pop_backCC$p_val, "bonferroni"), function(x) p_value(x))
    
    # Grab contrasts that we need from the robust esatimtes
      p_courtship_CC <- result_CourtshipBgrd_pop_backCC[2,8]
       CI_CC_Courtship <- data.frame(Estimate=robust_CourtshipBgrd_pop_backCC$beta[3],
                               lwr = robust_CourtshipBgrd_pop_backCC$CI_L[3],
                               upr = robust_CourtshipBgrd_pop_backCC$CI_U[3])
  


## ----FigS1, fig.cap = "Examples of chameleon colour signal change in response to different social and predatory contexts"-------------

files <- list.files("./photos/ESM/")
img1 <- readJPEG(paste0("./photos/ESM/", files[1]))
img2 <- readJPEG(paste0("./photos/ESM/", files[2]))
img3 <- readJPEG(paste0("./photos/ESM/", files[3]))
img4 <- readJPEG(paste0("./photos/ESM/", files[4]))
img5 <- readJPEG(paste0("./photos/ESM/", files[5]))
img6 <- readJPEG(paste0("./photos/ESM/", files[6]))

p1 <- cowplot::ggdraw()+
        cowplot::draw_image(img1)
p2 <- cowplot::ggdraw()+
        cowplot::draw_image(img2)
p3 <- cowplot::ggdraw()+
        cowplot::draw_image(img3)
p4 <- cowplot::ggdraw()+
        cowplot::draw_image(img4)
p5 <- cowplot::ggdraw()+
        cowplot::draw_image(img5)
p6 <- cowplot::ggdraw()+
        cowplot::draw_image(img6)

(p1 + p3 + p6) / (p2 + p5 + p4) + plot_annotation(tag_levels = c("A", "B", "C", "D", "E", "F"))
  


## ----FigS2, fig.width = 13, fig.height = 12, fig.cap = "Chormatic (A, C) and Achromatic (B, D) contrast in just noticible differences (JND) for Hawaiian and Kenyan lizards relative to their environments. All four body regions (gular, mid-flank, tailbase and topflank) are shown averaged across stems and leaves found in their habitat. Mean JNDs are provided along with 95% confidence intervals. A & B represent male-male contest displays whereas C & D represent courtship displays"----

#Making a point plot with error bars split by body size and population 
#Compute raw means and standard errors for body part and population for dS and assigned this data into Display_plot_dS_dat
Display_plot_dS_dat <- Display_1 %>% #data for dS Chromatic constrast
  group_by(Population, BodyRegion) %>% 
  summarise( Mean = mean(dS, na.rm = T), 
               SE = sd(dS)/sqrt(length(unique(ID))-1),
            Upper = Mean + 1.96*SE,
            Lower = Mean - 1.96*SE) 
            
Display_plot_dL_dat <- Display_1 %>% #data for dL Achromatic contrast
  group_by(Population, BodyRegion) %>% 
  summarise(Mean = mean(dL, na.rm = T), 
            SE = sd(dL)/sqrt(length(unique(ID))-1),
            Upper = Mean + 1.96*SE,
            Lower = Mean - 1.96*SE) 

#Plotting Chromatic contrast (dS)
p1 <- ggplot(Display_plot_dS_dat, aes(x = Population, y = Mean, group = BodyRegion, color = BodyRegion)) +
  geom_point(size = 3, position = position_dodge(width=0.2)) +
  geom_line(position = position_dodge(width=0.2)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(width=0.2), width = 0) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("") + 
  ylab("Chromatic contrast (JND)") +
  theme_bw() +
  theme(legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold"))


#Plotting Achromatic contrast (dL)
p2 <- ggplot(Display_plot_dL_dat, aes(x = Population, y = Mean, group = BodyRegion, color = BodyRegion)) +
  geom_point(size = 3, position = position_dodge(width=0.2)) +
  geom_line(position = position_dodge(width=0.2)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(width=0.2), width = 0) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("") + 
  ylab("Achromatic contrast (JND)") +
  theme_bw() +
  labs(color = "Body Region") + 
  theme(legend.position = c(0.85, 0.85),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold"))

#Making a point plot with error bars split by body size and population 
#Compute raw means and standard errors for body part and population for dS and assigned this data into Display_plot_dS_dat
Courtship_plot_dS_dat <- Courtship %>% #data for dS Chromatic constrast
  group_by(Population, BodyRegion) %>% 
  summarise(Mean = mean(dS, na.rm = T), 
            SE = sd(dS)/sqrt(length(unique(ID))),
            Upper = Mean + 1.96*SE,
            Lower = Mean - 1.96*SE) 

Courtship_plot_dL_dat <- Courtship %>% #data for dL Achromatic contrast
  group_by(Population, BodyRegion) %>% 
  summarise(Mean = mean(dL, na.rm = T), 
            SE = sd(dL)/sqrt(length(unique(ID))),
            Upper = Mean + 1.96*SE,
            Lower = Mean - 1.96*SE) 


#Plotting Chromatic contrast (dS)
p3 <- ggplot(Courtship_plot_dS_dat, aes(x = Population, y = Mean, group = BodyRegion, color = BodyRegion)) +
  geom_point(size = 3, position = position_dodge(width=0.2)) +
  geom_line(position = position_dodge(width=0.2)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(width=0.2), width = 0) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("") + 
  ylab("Chromatic contrast (JND)") +
  theme_bw() +
  theme(legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold"))

#Plotting Achromatic contrast (dL)
p4 <- ggplot(Courtship_plot_dL_dat, aes(x = Population, y = Mean, group = BodyRegion, color = BodyRegion)) +
  geom_point(size = 3, position = position_dodge(width=0.2)) +
  geom_line(position = position_dodge(width=0.2)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(width=0.2), width = 0) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("") + 
  ylab("Achromatic contrast (JND)") +
  theme_bw() +
  theme(legend.position = c(0.85, 0.85),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold"))




p1 + p2 + p3 + p4 + plot_annotation(tag_levels = c('A', 'B', 'C', 'D')) & theme(plot.tag = element_text(face = 'bold', size=30))



## ----tableS1, tab.cap = "Wald Tests comparing significance of model parameters. Wald tests computed using robust variance estimators with a Saitterwaite correction to the degrees of freedom. Interactions were first tested and models refit to test the signifiacnce of main effects"----
fullTable <- rbind(wald_table_ConspicDisplayAC, 
                   wald_table_ConspicDisplayCC, 
                   wald_table_ConspicCourtshipAC,
                   wald_table_ConspicCourtshipCC)

fullTable <- fullTable %>% 
              mutate(Context = c("Male-Male Display", rep("", 7), "Courtship Display", rep("", 7)),
                     JND = c("Achromatic Contrast", rep("", 3), "Chromatic Contrast", rep("", 3), "Achromatic Contrast", rep("", 3), "Chromatic Contrast", rep("", 3)),
                     p_val = sapply(p_val, function(x) p_value(x))) %>% select(Context, JND, Variable, Fstat, df_num, df_denom, p_val)

S3 <- flextable(fullTable) %>%
        table_style2(.)
S3


## ----tableS2, tab.cap = "Linear mixed effects model results using sandwich estimators and Saitterwaite degrees of freedom correction.Social Context"----
robust_sc_table <- rbind(robust_ConspicDisplayAC,
                          robust_ConspicDisplayCC,
                          robust_ConspicCourtshipAC,
                          robust_ConspicCourtshipCC) %>% 
                    mutate(p_Satt = sapply(p_Satt, function(x) p_value(x)),
                           SocialContext = c("Male-Male Display", rep("", 17), 
                                             "Courtship Display", rep("", 17)),
                           JND = c("Achromatic Contrast", rep("", 8), 
                                   "Chromatic Contrast", rep("", 8), 
                                   "Achromatic Contrast", rep("", 8), 
                                   "Chromatic Contrast", rep("", 8)))

robust_sc_table$parameter <- rownames(robust_sc_table)

robust_sc_table <- robust_sc_table %>% select(SocialContext, JND, parameter, beta, SE, tstat, df, p_Satt)


tableS5 <- flextable(robust_sc_table) %>%  table_style4()
tableS5


## ----FigS3, fig.width = 12, fig.height = 12, fig.cap = "Predators All four body regions (gular, mid-flank, tailbase and topflank) are shown averaged across stems and leaves found in their habitat. Mean JNDs are provided along with 95% confidence intervals. A & B represent male-male contest displays whereas C & D represent courtship displays"----

# Load the data
Predator <- read.table("./data/Predator_2_3.csv", 
                       header=TRUE, sep=",", dec=".", strip.white=TRUE)
 
# Clean up
 Predator <- Predator %>% filter(!BodyRegion == "botflank" & !BodyRegion =='gularbot') %>% as.data.frame() #exclude bottom flank.
Predator$BodyRegion <- factor(Predator$BodyRegion) #updates levels for the variable BodyRegion


# Calculate summary data
summary_data_pred <- Predator %>%
                group_by(Predator, Population, BodyRegion) %>%
                summarise( Mean_dS = mean(dS, na.rm = T), 
                             SE_dS = sd(dS)/sqrt(length(unique(ID))),
                             Upper_dS = Mean_dS + 1.96*SE_dS,
                             Lower_dS = Mean_dS - 1.96*SE_dS, 
                           Mean_dL = mean(dL, na.rm = T), 
                             SE_dL = sd(dL)/sqrt(length(unique(ID))),
                             Upper_dL = Mean_dL + 1.96*SE_dL,
                             Lower_dL = Mean_dL - 1.96*SE_dL) %>%
                mutate(Predator = if_else(Predator == "bird", "Birds", "Snakes"))

# PLot 
p3 <- ggplot(summary_data_pred, aes(x = Population, y = Mean_dL, group = BodyRegion, color = BodyRegion)) + facet_wrap(~Predator) +
  ylim(8, 30) + 
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = Lower_dL, ymax = Upper_dL), position = position_dodge(width=0.5), width = 0) + 
  geom_line(position = position_dodge(width=0.5)) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("") + 
  ylab("Achromatic contrast (JND)") +
  theme_bw() +
  labs(color = "Body Region") + 
  theme(legend.position = c(0.90, 0.85), 
        strip.background = element_blank(),
    strip.text.x = element_text(size = 24, face = "bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold")) 

p4 <- ggplot(summary_data_pred, aes(x = Population, y = Mean_dS, group = BodyRegion, color = BodyRegion)) + facet_wrap(~Predator) +
  ylim(1, 9) + 
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = Lower_dS, ymax = Upper_dS), position = position_dodge(width=0.5), width = 0) + 
  geom_line(position = position_dodge(width=0.5)) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("") + 
  ylab("Chromatic contrast (JND)") +
  labs(colour = "Body Region") + 
  theme_bw() +
  theme(legend.position = c(0.90, 0.85),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 24, face = "bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold"))

plot_grid(p4, p3, nrow = 2, ncol = 1, labels = c("A", "B"), label_size = 30,
  label_fontface = "bold", rel_widths=c(1,1))



## ----tableS3, tab.cap = "Wald Tests comparing significance of model parameters for predators. Wald tests computed using robust variance estimators with a Saitterwaite correction to the degrees of freedom. Interactions were first tested and models refit to test the signifiacnce of main effects"----

fulltableP <- rbind(Wald_table_PredatorAC,
                    Wald_table_PredatorCC)

fulltableP <- fulltableP %>% mutate(JND = c("Achromatic Contrast", rep("",4), "Chromatc Contrast", rep("",4)),
                                            p_val = sapply(p_val, function(x) p_value(x))) %>% 
                                      select(JND, Variable, Fstat, df_num, df_denom, p_val)

S4 <- flextable(fulltableP) %>%
        table_style3(.)
S4


## ----tableS4, tab.cap = "Linear mixed effects model results using sandwich estimators and Saitterwaite degrees of freedom correction.Predators"----
robust_sc_pred_table <- rbind(robust_PredatorAC,
                         robust_PredatorCC) %>% 
                    mutate(p_Satt = sapply(p_Satt, function(x) p_value(x)),
                           JND = c("Achromatic Contrast", rep("", 9), 
                                   "Chromatic Contrast", rep("", 9)))

robust_sc_pred_table$parameter <- rownames(robust_sc_pred_table)

robust_sc_pred_table <- robust_sc_pred_table %>% select(JND, parameter, beta, SE, tstat, df, p_Satt)


tableS6 <- flextable(robust_sc_pred_table) %>%  table_style5()
tableS6


## ----tableS5, tab.cap = "Pairwise comparisons of achormatic contrast between Kenyan and Hawaiian chameleon displays to a snake and bird visual system. Four different body regions ('gular', 'midflank', 'tailbase', topflank') are provided along with pairwise Wald test for contrast significance. Wald test uses a robust variance correction on the standard errors."----

# Split snakes and birds
result5_snake <-  result5[grep("snake", result5$hypothesis), ] %>% mutate(Species = c("Snake", rep("",3)))
 result5_bird <- result5[grep("bird", result5$hypothesis), ] %>% mutate(Species = c("Bird", rep("",3)))

 # Bind together in order
  final_r5 <- rbind(result5_snake,result5_bird)
  
  
  # Clean
  final_r5 <- final_r5 %>% 
              select(-test, -delta, -df_num) %>%
              mutate(hypothesis = gsub("PopBRInt", "", hypothesis),
                     p_val = sapply(p_val, function(x) p_value(x))) %>%
              select(Species, hypothesis, Fstat, df_denom, p_val, p_val_bonfer)


ft5 <- flextable(final_r5) %>%
        table_style(.) %>% 
        add_header_row(values = "Achromatic Contrast", colwidths = 6)  
  
 ft5

 


## ----tableS6, tab.cap = "Pairwise comparisons of achormatic contrast between Kenyan and Hawaiian chameleon displays to a snake and bird visual system. Four different body regions ('gular', 'midflank', 'tailbase', topflank') are provided along with pairwise Wald test for contrast significance. Wald test uses a robust variance correction on the standard errors."----

# Split snakes and birds

result6_snake <- result6[grep("snake", result6$hypothesis), ] %>% mutate(Species = c("Snake", rep("",3)))
 result6_bird <- result6[grep("bird", result6$hypothesis), ] %>% mutate(Species = c("Bird", rep("",3)))

# Bind together in order
 final_r6 <- rbind(result6_snake,result6_bird)

 
# Clean
 final_r6 <- final_r6 %>% 
              select(-test, -delta, -df_num) %>%
              mutate(hypothesis = gsub("PopBRInt", "", hypothesis),
                     p_val = sapply(p_val, function(x) p_value(x))) %>%
              select(Species, hypothesis, Fstat, df_denom, p_val, p_val_bonfer)
 
 # Table
 ft6 <- flextable(final_r6) %>%
        table_style(.) %>% 
        add_header_row(values = "Chromatic Contrast", colwidths = 6) 
 
 ft6


## ----FigS4, fig.width = 13, fig.height = 7.5, fig.cap = "Local adaptation. All four body regions (gular, mid-flank, tailbase and topflank) are shown averaged across stems and leaves found in their habitat. Mean JNDs are provided along with 95% confidence intervals. A & B represent male-male contest displays whereas C & D represent courtship displays"----
# Courship data

CourtshipBgrd <- read.table("./data/Courtship_5.csv", 
                        header=TRUE, sep=",", dec=".", strip.white=TRUE)

CourtshipBgrd <- CourtshipBgrd %>% 
                filter(!BodyRegion == "botflank") %>% 
                mutate(Background_pop = ifelse(Background2 == "own", Population, if_else(Background2 == "other" & Population == "Hawaii", "Kenya", "Hawaii"))) %>% 
                as.data.frame() #exclude bottom flank.
CourtshipBgrd$BodyRegion <- factor(CourtshipBgrd$BodyRegion)


# Display data
DisplayBgrd <- read.table("./data/Display_5.csv", 
                        header=TRUE, sep=",", dec=".", strip.white=TRUE)

DisplayBgrd <- DisplayBgrd %>% 
                filter(!BodyRegion == "botflank") %>% 
                mutate(Background_pop = ifelse(Background2 == "own", Population, if_else(Background2 == "other" & Population == "Hawaii", "Kenya", "Hawaii"))) %>% 
                as.data.frame() #exclude bottom flank.
DisplayBgrd$BodyRegion <- factor(DisplayBgrd$BodyRegion) #updates levels for the variable BodyRegion

##########
# Data
##########
courtship_summary <- CourtshipBgrd %>% 
                    group_by(Population, Background_pop, BodyRegion) %>%
                summarise( Mean_dS = mean(dS, na.rm = T), 
                             SE_dS = sd(dS)/sqrt(length(unique(ID))),
                             Upper_dS = Mean_dS + 1.96*SE_dS,
                             Lower_dS = Mean_dS - 1.96*SE_dS, 
                           Mean_dL = mean(dL, na.rm = T), 
                             SE_dL = sd(dL)/sqrt(length(unique(ID))),
                             Upper_dL = Mean_dL + 1.96*SE_dL,
                             Lower_dL = Mean_dL - 1.96*SE_dL) %>% filter(Population == "Hawaii") %>% as.data.frame() %>% mutate(Social_Context = "Courtship")

display_summary <- DisplayBgrd %>%
                group_by(Population, Background_pop, BodyRegion) %>%
                summarise( Mean_dS = mean(dS, na.rm = T), 
                             SE_dS = sd(dS)/sqrt(length(unique(ID))),
                             Upper_dS = Mean_dS + 1.96*SE_dS,
                             Lower_dS = Mean_dS - 1.96*SE_dS, 
                           Mean_dL = mean(dL, na.rm = T), 
                             SE_dL = sd(dL)/sqrt(length(unique(ID))),
                             Upper_dL = Mean_dL + 1.96*SE_dL,
                             Lower_dL = Mean_dL - 1.96*SE_dL) %>% filter(Population == "Hawaii") %>% as.data.frame() %>% mutate(Social_Context = "Male-male contest")


##########
# Plots
##########
#Grab photos

#plots
p1.2 <- ggplot(courtship_summary, aes(x = Background_pop, y = Mean_dS, color = BodyRegion, group = BodyRegion)) +
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = Lower_dS, ymax = Upper_dS), position = position_dodge(width=0.5), width = 0) + 
  geom_line(position = position_dodge(width=0.5)) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("Background Environment") + 
  ylab("Chromatic contrast (JND)") +
  # annotate("segment", x = 0.85, xend = 1.85, y = 6.1, yend = 6.1, colour = "black") +
  # annotate("segment", x = 1.12, xend = 2.12, y = 5.7, yend = 5.7, colour = "black") + 
  # annotate("text", x = 0.85+(1.85-0.85)/2,  y = 6.2, label = "NS",  size = 6) + 
  # annotate("text", x = 1.12+(2.12-1.12)/2, y = 5.8, label = "NS", size = 6) + 
  theme_bw() +
  labs(colour = "Body Region") +
  theme(legend.position = "none",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold")) 

p2.2 <- ggplot(display_summary, aes(x = Background_pop, y = Mean_dL, group = BodyRegion, color = BodyRegion)) +
  geom_point(size = 3, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = Lower_dL, ymax = Upper_dL), position = position_dodge(width=0.5), width = 0) + 
  geom_line(position = position_dodge(width=0.5)) + 
  scale_color_manual(values = wes_palette("Cavalcanti1")) + #Change color palette here see: https://github.com/karthik/wesanderson
  xlab("Background Environment") + 
  ylab("Achromatic contrast (JND)") +
  theme_bw() +
  theme(legend.position = c(0.80, 0.85),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18),
    axis.title=element_text(size=20,face="bold"),
    legend.text =element_text(size = 12),
    legend.title =element_text(size = 12, face = "bold")) 

#pdf(file="./output/figures/Figure2.pdf", height = 10, width = 18)
plot_grid(p1.2, p2.2, labels = c("A", "B"), label_size = 30,
  label_fontface = "bold", rel_widths=c(1,1))


