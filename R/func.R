
# Cleans up look of p_values
p_value <- function(x){
  if(x <= 0.0001) {tmp = "< 0.0001"}
  if(x <= 0.001 & x >= 0.0001) {tmp ="< 0.001"}
  if(x <= 0.01 & x >= 0.001) {tmp ="< 0.01"}
  if(x >= 0.01) {tmp = round(x, digits =2)}
  return(tmp)
}

# Create wald tables. Functions using RVE's to look at overall significance of interactions and main effects in models
wald_table   <- function(model_main, model_inter){
  
       inter <- Wald_test(model_inter, 
                                        constraints = constrain_zero(7:9),
                                        vcov="CR2", tidy = TRUE)
  BodyRegion <- Wald_test(model_main, 
                                             constraints = constrain_zero(4:6),
                                             vcov="CR2", tidy = TRUE)
  SVL <- Wald_test(model_main, 
                                      constraints = constrain_zero(2),
                                      vcov="CR2", tidy = TRUE)
  Pop <- Wald_test(model_main, 
                                      constraints = constrain_zero(3),
                                      vcov="CR2", tidy = TRUE)
  
  WaldTab <- rbind(SVL,
                                      Pop, 
                                      BodyRegion, 
                                      inter)
  WaldTab <- WaldTab %>% 
    mutate(Variable = c("SVL", 
                        "Population", 
                        "Body Region", 
                        "Population:Body Region")) %>% 
    select(Variable, Fstat, df_num, df_denom, p_val) %>% as.data.frame()
  
  return(WaldTab)
  }
  
wald_table_p <- function(model_main, model_inter){
    
    inter_BR_Pred <- Wald_test(model_inter, 
                       constraints = constrain_zero(7:9),
                       vcov="CR2", tidy = TRUE)
    
    inter_Pop_Pred <- Wald_test(model_inter, 
                               constraints = constrain_zero(10),
                               vcov="CR2", tidy = TRUE)
    
    BodyRegion <- Wald_test(model_main, 
                            constraints = constrain_zero(4:6),
                            vcov="CR2", tidy = TRUE)
    Pred <- Wald_test(model_main, 
                     constraints = constrain_zero(2),
                     vcov="CR2", tidy = TRUE)
    Pop <- Wald_test(model_main, 
                     constraints = constrain_zero(3),
                     vcov="CR2", tidy = TRUE)
    
    WaldTab <- rbind(BodyRegion,
                     Pop, 
                     Pred, 
                     inter_BR_Pred,
                     inter_Pop_Pred)
    WaldTab <- WaldTab %>% 
      mutate(Variable = c("Body Region", 
                          "Population", 
                          "Predator", 
                          "Predator:Body Region",
                          "Predator:Population")) %>% 
      select(Variable, Fstat, df_num, df_denom, p_val) %>% as.data.frame()
    
    return(WaldTab)
  }

# Table styling functions

table_style  <- function(table){
  table <- table %>% width(j = 1, width = 1) %>% width(j = 2, width = 2.5) %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    compose(part = "header", j = 1, value = as_paragraph(as_b("Predator"))) %>% 
    compose( part = "header", j = 2, value = as_paragraph(as_b("Contrast"))) %>% 
    compose(part = "header", j = 3, value = as_paragraph(as_b("F"))) %>%
    compose(part = "header", j = 4, value = as_paragraph(as_b("DF"))) %>% 
    compose(part = "header", j = 5, value = as_paragraph(as_b("p"))) %>% 
    compose(part = "header", j = 6, value = as_paragraph(as_b("p"), as_sub(as_b("Bonferroni")))) %>%
    compose(part = "body", i = 1, j = 2, 
            value = as_paragraph("Kenya", as_sub("gular"), " - ", "Hawaii", as_sub("gular")))  %>% 
    compose(part = "body", i = 2, j = 2, 
            value = as_paragraph("Kenya", as_sub("midflank"), " - ", "Hawaii", as_sub("midflank"))) %>% 
    compose(part = "body", i = 3, j = 2, 
            value = as_paragraph("Kenya", as_sub("tailbase"), " - ", "Hawaii", as_sub("tailbase"))) %>% 
    compose(part = "body", i = 4, j = 2, 
            value = as_paragraph("Kenya", as_sub("topflank"), " - ", "Hawaii", as_sub("topflank"))) %>% 
    compose(part = "body", i = 5, j = 2, 
            value = as_paragraph("Kenya", as_sub("gular"), " - ", "Hawaii", as_sub("gular")))  %>% 
    compose(part = "body", i = 6, j = 2, 
            value = as_paragraph("Kenya", as_sub("midflank"), " - ", "Hawaii", as_sub("midflank"))) %>% 
    compose(part = "body", i = 7, j = 2, 
            value = as_paragraph("Kenya", as_sub("tailbase"), " - ", "Hawaii", as_sub("tailbase"))) %>% 
    compose(part = "body", i = 8, j = 2, 
            value = as_paragraph("Kenya", as_sub("topflank"), " - ", "Hawaii", as_sub("topflank")))
  return(table)
}

table_style2 <- function(table){
  table <- table %>%  width(j = 3, width = 3) %>% 
    align(align = "center", part = "header") %>% 
    align( align = "center", part = "body") %>% 
    compose(part = "header", j = 1, value = as_paragraph(as_b("Social Context"))) %>% 
    compose(part = "header", j = 2, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    compose(part = "header", j = 3, value = as_paragraph(as_b("Variable"))) %>% 
    compose( part = "header", j = 4, value = as_paragraph(as_b("F"))) %>% 
    compose(part = "header", j = 5, value = as_paragraph(as_b("df"), as_sub(as_b("num")))) %>%
    compose(part = "header", j = 6, value = as_paragraph(as_b("df"), as_sub(as_b("denom")))) %>% 
    compose(part = "header", j = 7, value = as_paragraph(as_b("p"))) 
  return(table)
}

table_style3 <- function(table){
  table <- table %>% width(j = 1, width = 1) %>% width(j = 3, width = 3.5) %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    compose(part = "header", j = 1, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    compose(part = "header", j = 2, value = as_paragraph(as_b("Variable"))) %>% 
    compose( part = "header", j = 3, value = as_paragraph(as_b("F"))) %>% 
    compose(part = "header", j = 4, value = as_paragraph(as_b("df"), as_sub(as_b("num")))) %>%
    compose(part = "header", j = 5, value = as_paragraph(as_b("df"), as_sub(as_b("denom")))) %>% 
    compose(part = "header", j = 6, value = as_paragraph(as_b("p"))) 
  return(table)
}

table_style4 <- function(table){
  table <- table %>%
    width(j = 3, width = 6) %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    compose(part = "header", j = 1, value = as_paragraph(as_b("Social Context"))) %>% 
    compose(part = "header", j = 2, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    compose(part = "header", j = 3, value = as_paragraph(as_b("Parameter"))) %>% 
    compose(part = "header", j = 4, value = as_paragraph(as_b("Estimate"))) %>% 
    compose(part = "header", j = 5, value = as_paragraph(as_b("SE"))) %>%
    compose(part = "header", j = 6, value = as_paragraph(as_b("Tstat"))) %>% 
    compose(part = "header", j = 7, value = as_paragraph(as_b("df"))) %>% 
    compose(part = "header", j = 8, value = as_paragraph(as_b("p"))) %>% 
    compose(part = "body", j = 3, i = c(1, 10, 19, 28), value = as_paragraph("Intercept")) %>%
    compose(part = "body", j = 3, i = c(1, 10, 19, 28)+1, value = as_paragraph("SVL")) %>%
    compose(part = "body", j = 3, i = c(1, 10, 19, 28)+2, value = as_paragraph("Population", as_sub("Kenya"))) %>%
    compose(part = "body", j = 3, i = c(1, 10, 19, 28)+3, value = as_paragraph("Body Region", as_sub("midflank"))) %>%
    compose(part = "body", j = 3, i = c(1, 10, 19, 28)+4, value = as_paragraph("Body Region", as_sub("tailbase"))) %>%
    compose(part = "body", j = 3, i = c(1, 10, 19, 28)+5, value = as_paragraph("Body Region", as_sub("topflank"))) %>%
    compose(part = "body", j = 3, i = c(1, 10, 19, 28)+6, value = as_paragraph("Population", as_sub("Kenya"), "*BodyRegion", as_sub("midflank"))) %>%
    compose(part = "body", j = 3, i = c(1, 10, 19, 28)+7, value = as_paragraph("Population", as_sub("Kenya"), "*BodyRegion", as_sub("tailbase"))) %>%
    compose(part = "body", j = 3, i = c(1, 10, 19, 28)+8, value = as_paragraph("Population", as_sub("Kenya"), "*BodyRegion", as_sub("topflank"))) %>% 
    font(i = c(1:36), j = c(1:7), fontname="Times", part = "body") %>% 
    font(j = c(1:7), fontname="Times", part = "header")
  
  return(table)
}

table_style5 <- function(table){
  table <- table %>% width(j = 1, width = 1) %>% 
    width(j = 2, width = 3) %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    compose(part = "header", j = 1, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    compose(part = "header", j = 2, value = as_paragraph(as_b("Parameter"))) %>% 
    compose(part = "header", j = 3, value = as_paragraph(as_b("Estimate"))) %>% 
    compose(part = "header", j = 4, value = as_paragraph(as_b("SE"))) %>%
    compose(part = "header", j = 5, value = as_paragraph(as_b("T"), as_sub(as_b("statistic")))) %>% 
    compose(part = "header", j = 6, value = as_paragraph(as_b("df"))) %>% 
    compose(part = "header", j = 7, value = as_paragraph(as_b("p"))) %>% 
    compose(part = "body", j = 2, i = c(1, 11), value = as_paragraph("Intercept")) %>%
    compose(part = "body", j = 2, i = c(1, 11)+1, value = as_paragraph("Predator", as_sub("Snake"))) %>%
    compose(part = "body", j = 2, i = c(1, 11)+2, value = as_paragraph("Population", as_sub("Kenya"))) %>%
    compose(part = "body", j = 2, i = c(1, 11)+3, value = as_paragraph("Body Region", as_sub("midflank"))) %>%
    compose(part = "body", j = 2, i = c(1, 11)+4, value = as_paragraph("Body Region", as_sub("tailbase"))) %>%
    compose(part = "body", j = 2, i = c(1, 11)+5, value = as_paragraph("Body Region", as_sub("topflank"))) %>%
    compose(part = "body", j = 2, i = c(1, 11)+6, value = as_paragraph("Predator", as_sub("Snake"), "* BodyRegion", as_sub("midflank"))) %>%
    compose(part = "body", j = 2, i = c(1, 11)+7, value = as_paragraph("Predator", as_sub("Snake"), "* BodyRegion", as_sub("tailbase"))) %>%
    compose(part = "body", j = 2, i = c(1, 11)+8, value = as_paragraph("Predator", as_sub("Snake"), "* BodyRegion", as_sub("topflank"))) %>% 
    compose(part = "body", j = 2, i = c(1, 11)+9, value = as_paragraph("Predator", as_sub("Snake"), "* Population", as_sub("Kenya"))) %>% 
    font(i = c(1:20), j = c(1:7), fontname="Times New Roman", part = "body") %>% 
    font(j = c(1:7), fontname="Times New Roman", part = "header")
  
  return(table)
}

table_style6 <- function(table){
  table <- table  %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    compose(part = "header", j = 1, value = as_paragraph(as_b("Social Context"))) %>% 
    compose(part = "header", j = 2, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    compose(part = "header", j = 3, value = as_paragraph(as_b("Parameter"))) %>% 
    compose( part = "header", j = 4, value = as_paragraph(as_b("Estimate"))) %>% 
    compose(part = "header", j = 5, value = as_paragraph(as_b("SE"))) %>%
    compose(part = "header", j = 6, value = as_paragraph(as_b("df"))) %>% 
    compose(part = "header", j = 7, value = as_paragraph(as_b("95% CI"), as_sub(as_b("lower")))) %>% 
    compose(part = "header", j = 8, value = as_paragraph(as_b("95% CI"), as_sub(as_b("upper")))) %>% 
    compose(part = "body", j = 3, i = c(1,4,7,10), value = as_paragraph("Intercept")) %>%
    compose(part = "body", j = 3, i = c(1,4,7,10)+1, value = as_paragraph("SVL", as_sub("z-transformed"))) %>%
    compose(part = "body", j = 3, i = c(1,4,7,10)+2, value = as_paragraph("Population", as_sub("Kenya")))
  return(table)
}

table_style7 <- function(table){
  table <- table %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    compose(part = "header", j = 1, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    compose(part = "header", j = 2, value = as_paragraph(as_b("Parameter"))) %>% 
    compose( part = "header", j = 3, value = as_paragraph(as_b("Estimate"))) %>% 
    compose(part = "header", j = 4, value = as_paragraph(as_b("SE"))) %>%
    compose(part = "header", j = 5, value = as_paragraph(as_b("df"))) %>% 
    compose(part = "header", j = 6, value = as_paragraph(as_b("95% CI"), as_sub(as_b("lower")))) %>% 
    compose(part = "header", j = 7, value = as_paragraph(as_b("95% CI"), as_sub(as_b("upper")))) %>% 
    compose(part = "body", j = 2, i = c(1,5), value = as_paragraph("Intercept")) %>%
    compose(part = "body", j = 2, i = c(1,5)+1, value = as_paragraph("Predator", as_sub("Snake"))) %>%
    compose(part = "body", j = 2, i = c(1,5)+2, value = as_paragraph("Population", as_sub("Kenya"))) %>%
    compose(part = "body", j = 2, i = c(1,5)+3, value = as_paragraph("Predator", as_sub("Snake"), "* Population", as_sub("Kenya")))
    return(table)
}

table_style8 <- function(table){
  table <- table %>%
    align(align = "center", part = "header") %>% 
    align( align = "center", part = "body") %>% 
    compose(part = "header", j = 1, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    compose(part = "header", j = 2, value = as_paragraph(as_b("Contrast"))) %>% 
    compose(part = "header", j = 3, value = as_paragraph(as_b("Estimate"))) %>% 
    compose( part = "header", j = 4, value = as_paragraph(as_b("95% CI"), as_sub(as_b("lower")))) %>% 
    compose(part = "header", j = 5, value = as_paragraph(as_b("95% CI"), as_sub(as_b("upper")))) %>%
    compose(part = "header", j = 6, value = as_paragraph(as_b("F"), as_sub(as_b("stat")))) %>% 
    compose(part = "header", j = 7, value = as_paragraph(as_b("df"))) %>% 
    compose(part = "header", j = 8, value = as_paragraph(as_b("p"))) %>% 
    compose(part = "header", j = 9, value = as_paragraph(as_b("p"), as_sub(as_b("Bonferroni")))) %>% 
  return(table)
}

table_style9 <- function(table){
  table <- table  %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    compose(part = "header", j = 1, value = as_paragraph(as_b("Social Context"))) %>% 
    compose(part = "header", j = 2, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    compose(part = "header", j = 3, value = as_paragraph(as_b("Parameter"))) %>% 
    compose( part = "header", j = 4, value = as_paragraph(as_b("Estimate"))) %>% 
    compose(part = "header", j = 5, value = as_paragraph(as_b("SE"))) %>%
    compose(part = "header", j = 6, value = as_paragraph(as_b("df"))) %>% 
    compose(part = "header", j = 7, value = as_paragraph(as_b("95% CI"), as_sub(as_b("lower")))) %>% 
    compose(part = "header", j = 8, value = as_paragraph(as_b("95% CI"), as_sub(as_b("upper")))) %>% 
    compose(part = "body", j = 3, i = c(1,4,7,10), value = as_paragraph("Intercept")) %>%
    compose(part = "body", j = 3, i = c(1,4,7,10)+1, value = as_paragraph("Population", as_sub("Kenya"))) %>%
    compose(part = "body", j = 3, i = c(1,4,7,10)+2, value = as_paragraph("Background", as_sub("Kenya")))
  return(table)
}
