
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
  table <- table  %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    flextable::compose(part = "header", j = 1, value = as_paragraph(as_b("Predator"))) %>% 
    flextable::compose(part = "header", j = 2, value = as_paragraph(as_b("Contrast"))) %>% 
    flextable::compose(part = "header", j = 3, value = as_paragraph(as_b("F"))) %>%
    flextable::compose(part = "header", j = 4, value = as_paragraph(as_b("DF"))) %>% 
    flextable::compose(part = "header", j = 5, value = as_paragraph(as_b("p"))) %>% 
    flextable::compose(part = "header", j = 6, value = as_paragraph(as_b("p"), as_sub(as_b(" Bonferroni")))) %>%
    flextable::compose(part = "body", i = 1, j = 2, 
            value = as_paragraph("Kenya", as_sub(" gular"), " - ", "Hawaii", as_sub(" gular")))  %>% 
    flextable::compose(part = "body", i = 2, j = 2, 
            value = as_paragraph("Kenya", as_sub(" midflank"), " - ", "Hawaii", as_sub(" midflank"))) %>% 
    flextable::compose(part = "body", i = 3, j = 2, 
            value = as_paragraph("Kenya", as_sub(" tailbase"), " - ", "Hawaii", as_sub(" tailbase"))) %>% 
    flextable::compose(part = "body", i = 4, j = 2, 
            value = as_paragraph("Kenya", as_sub(" topflank"), " - ", "Hawaii", as_sub(" topflank"))) %>% 
    flextable::compose(part = "body", i = 5, j = 2, 
            value = as_paragraph("Kenya", as_sub(" gular"), " - ", "Hawaii", as_sub(" gular")))  %>% 
    flextable::compose(part = "body", i = 6, j = 2, 
            value = as_paragraph("Kenya", as_sub(" midflank"), " - ", "Hawaii", as_sub(" midflank"))) %>% 
    flextable::compose(part = "body", i = 7, j = 2, 
            value = as_paragraph("Kenya", as_sub(" tailbase"), " - ", "Hawaii", as_sub(" tailbase"))) %>% 
    flextable::compose(part = "body", i = 8, j = 2, 
            value = as_paragraph("Kenya", as_sub(" topflank"), " - ", "Hawaii", as_sub(" topflank"))) %>% 
    font(fontname="Times", part = "body") %>% 
    font(fontname="Times", part = "header") 
  return(table)
}

table_style2 <- function(table){
  table <- table %>%  width(j = 2, width = 1.5) %>% 
    align(align = "center", part = "header") %>% 
    align( align = "center", part = "body") %>% 
    flextable::compose(part = "header", j = 1, value = as_paragraph(as_b("Social Context"))) %>% 
    flextable::compose(part = "header", j = 2, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    flextable::compose(part = "header", j = 3, value = as_paragraph(as_b("Variable"))) %>% 
    flextable::compose( part = "header", j = 4, value = as_paragraph(as_b("F"))) %>% 
    flextable::compose(part = "header", j = 5, value = as_paragraph(as_b("df"), as_sub(as_b(" num")))) %>%
    flextable::compose(part = "header", j = 6, value = as_paragraph(as_b("df"), as_sub(as_b(" denom")))) %>% 
    flextable::compose(part = "header", j = 7, value = as_paragraph(as_b("p"))) %>% 
    font(fontname="Times", part = "body") %>% 
    font(fontname="Times", part = "header")
  return(table)
}

table_style3 <- function(table){
  table <- table %>% width(j = 1, width = 1) %>% width(j = 3, width = 3.5) %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    flextable::compose(part = "header", j = 1, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    flextable::compose(part = "header", j = 2, value = as_paragraph(as_b("Variable"))) %>% 
    flextable::compose( part = "header", j = 3, value = as_paragraph(as_b("F"))) %>% 
    flextable::compose(part = "header", j = 4, value = as_paragraph(as_b("df"), as_sub(as_b(" num")))) %>%
    flextable::compose(part = "header", j = 5, value = as_paragraph(as_b("df"), as_sub(as_b(" denom")))) %>% 
    flextable::compose(part = "header", j = 6, value = as_paragraph(as_b("p"))) %>% 
    font(fontname="Times", part = "body") %>% 
    font(fontname="Times", part = "header")
  return(table)
}

table_style4 <- function(table){
  table <- table %>%
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    flextable::compose(part = "header", j = 1, value = as_paragraph(as_b("Social Context"))) %>% 
    flextable::compose(part = "header", j = 2, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    flextable::compose(part = "header", j = 3, value = as_paragraph(as_b("Parameter"))) %>% 
    flextable::compose(part = "header", j = 4, value = as_paragraph(as_b("Estimate"))) %>% 
    flextable::compose(part = "header", j = 5, value = as_paragraph(as_b("SE"))) %>%
    flextable::compose(part = "header", j = 6, value = as_paragraph(as_b("Tstat"))) %>% 
    flextable::compose(part = "header", j = 7, value = as_paragraph(as_b("df"))) %>% 
    flextable::compose(part = "header", j = 8, value = as_paragraph(as_b("p"))) %>% 
    flextable::compose(part = "body", j = 3, i = c(1, 10, 19, 28), value = as_paragraph("Intercept")) %>%
    flextable::compose(part = "body", j = 3, i = c(1, 10, 19, 28)+1, value = as_paragraph("SVL")) %>%
    flextable::compose(part = "body", j = 3, i = c(1, 10, 19, 28)+2, value = as_paragraph("Population", as_sub(" Kenya"))) %>%
    flextable::compose(part = "body", j = 3, i = c(1, 10, 19, 28)+3, value = as_paragraph("Body Region", as_sub(" midflank"))) %>%
    flextable::compose(part = "body", j = 3, i = c(1, 10, 19, 28)+4, value = as_paragraph("Body Region", as_sub(" tailbase"))) %>%
    flextable::compose(part = "body", j = 3, i = c(1, 10, 19, 28)+5, value = as_paragraph("Body Region", as_sub(" topflank"))) %>%
    flextable::compose(part = "body", j = 3, i = c(1, 10, 19, 28)+6, value = as_paragraph("Population", as_sub(" Kenya"), "* BodyRegion", as_sub(" midflank"))) %>%
    flextable::compose(part = "body", j = 3, i = c(1, 10, 19, 28)+7, value = as_paragraph("Population", as_sub(" Kenya"), "* BodyRegion", as_sub(" tailbase"))) %>%
    flextable::compose(part = "body", j = 3, i = c(1, 10, 19, 28)+8, value = as_paragraph("Population", as_sub(" Kenya"), "* BodyRegion", as_sub(" topflank"))) %>% 
    width(j = 3, width = 2) %>% 
    font(fontname="Times", part = "body") %>% 
    font(fontname="Times", part = "header")
  
  return(table)
}

table_style5 <- function(table){
  table <- table %>% width(j = 1, width = 1) %>% 
    width(j = 2, width = 3) %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    flextable::compose(part = "header", j = 1, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    flextable::compose(part = "header", j = 2, value = as_paragraph(as_b("Parameter"))) %>% 
    flextable::compose(part = "header", j = 3, value = as_paragraph(as_b("Estimate"))) %>% 
    flextable::compose(part = "header", j = 4, value = as_paragraph(as_b("SE"))) %>%
    flextable::compose(part = "header", j = 5, value = as_paragraph(as_b("T"), as_sub(as_b("statistic")))) %>% 
    flextable::compose(part = "header", j = 6, value = as_paragraph(as_b("df"))) %>% 
    flextable::compose(part = "header", j = 7, value = as_paragraph(as_b("p"))) %>% 
    flextable::compose(part = "body", j = 2, i = c(1, 11), value = as_paragraph("Intercept")) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 11)+1, value = as_paragraph("Predator", as_sub(" Snake"))) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 11)+2, value = as_paragraph("Population", as_sub(" Kenya"))) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 11)+3, value = as_paragraph("Body Region", as_sub(" midflank"))) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 11)+4, value = as_paragraph("Body Region", as_sub(" tailbase"))) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 11)+5, value = as_paragraph("Body Region", as_sub(" topflank"))) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 11)+6, value = as_paragraph("Predator", as_sub(" Snake"), "* BodyRegion", as_sub(" midflank"))) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 11)+7, value = as_paragraph("Predator", as_sub(" Snake"), "* BodyRegion", as_sub(" tailbase"))) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 11)+8, value = as_paragraph("Predator", as_sub(" Snake"), "* BodyRegion", as_sub(" topflank"))) %>% 
    flextable::compose(part = "body", j = 2, i = c(1, 11)+9, value = as_paragraph("Predator", as_sub(" Snake"), "* Population", as_sub(" Kenya"))) %>% 
    font(fontname="Times", part = "body") %>% 
    font(fontname="Times", part = "header")
  
  return(table)
}

table_style6 <- function(table){
  table <- table  %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    flextable::compose(part = "header", j = 1, value = as_paragraph(as_b("Social Context"))) %>% 
    flextable::compose(part = "header", j = 2, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    flextable::compose(part = "header", j = 3, value = as_paragraph(as_b("Parameter"))) %>% 
    flextable::compose( part = "header", j = 4, value = as_paragraph(as_b("Estimate"))) %>% 
    flextable::compose(part = "header", j = 5, value = as_paragraph(as_b("SE"))) %>%
    flextable::compose(part = "header", j = 6, value = as_paragraph(as_b("df"))) %>% 
    flextable::compose(part = "header", j = 7, value = as_paragraph(as_b("95% CI"), as_sub(as_b(" lower")))) %>% 
    flextable::compose(part = "header", j = 8, value = as_paragraph(as_b("95% CI"), as_sub(as_b(" upper")))) %>% 
    flextable::compose(part = "body", j = 3, i = c(1,4,7,10), value = as_paragraph("Intercept")) %>%
    flextable::compose(part = "body", j = 3, i = c(1,4,7,10)+1, value = as_paragraph("SVL", as_sub(" z-transformed"))) %>%
    flextable::compose(part = "body", j = 3, i = c(1,4,7,10)+2, value = as_paragraph("Population", as_sub(" Kenya"))) %>% 
    font(fontname="Times", part = "body") %>% 
    font(fontname="Times", part = "header")
  return(table)
}

table_style7 <- function(table){
  table <- table %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    flextable::compose(part = "header", j = 1, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    flextable::compose(part = "header", j = 2, value = as_paragraph(as_b("Parameter"))) %>% 
    flextable::compose( part = "header", j = 3, value = as_paragraph(as_b("Estimate"))) %>% 
    flextable::compose(part = "header", j = 4, value = as_paragraph(as_b("SE"))) %>%
    flextable::compose(part = "header", j = 5, value = as_paragraph(as_b("df"))) %>% 
    flextable::compose(part = "header", j = 6, value = as_paragraph(as_b("95% CI"), as_sub(as_b(" lower")))) %>% 
    flextable::compose(part = "header", j = 7, value = as_paragraph(as_b("95% CI"), as_sub(as_b(" upper")))) %>% 
    flextable::compose(part = "body", j = 2, i = c(1,5), value = as_paragraph("Intercept")) %>%
    flextable::compose(part = "body", j = 2, i = c(1,5)+1, value = as_paragraph("Predator", as_sub(" Snake"))) %>%
    flextable::compose(part = "body", j = 2, i = c(1,5)+2, value = as_paragraph("Population", as_sub(" Kenya"))) %>%
    flextable::compose(part = "body", j = 2, i = c(1,5)+3, value = as_paragraph("Predator", as_sub(" Snake"), "* Population", as_sub(" Kenya"))) %>% 
    font(fontname="Times", part = "body") %>% 
    font(fontname="Times", part = "header") %>% 
    width(j = 2, width = 4)
    return(table)
}

table_style8 <- function(table){
  table <- table %>%
    align(align = "center", part = "header") %>% 
    align( align = "center", part = "body") %>% 
    flextable::compose(part = "header", j = 1, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    flextable::compose(part = "header", j = 2, value = as_paragraph(as_b("Contrast"))) %>% 
    flextable::compose(part = "header", j = 3, value = as_paragraph(as_b("Estimate"))) %>% 
    flextable::compose( part = "header", j = 4, value = as_paragraph(as_b("95% CI"), as_sub(as_b(" lower")))) %>% 
    flextable::compose(part = "header", j = 5, value = as_paragraph(as_b("95% CI"), as_sub(as_b(" upper")))) %>%
    flextable::compose(part = "header", j = 6, value = as_paragraph(as_b("F"), as_sub(as_b(" stat")))) %>% 
    flextable::compose(part = "header", j = 7, value = as_paragraph(as_b("df"))) %>% 
    flextable::compose(part = "header", j = 8, value = as_paragraph(as_b("p"))) %>% 
    flextable::compose(part = "header", j = 9, value = as_paragraph(as_b("p"), as_sub(as_b(" Bonferroni")))) %>% 
    font(fontname="Times", part = "body") %>% 
    font(fontname="Times", part = "header") 
  return(table)
}

table_style9 <- function(table){
  table <- table  %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    flextable::compose(part = "header", j = 1, value = as_paragraph(as_b("Social Context"))) %>% 
    flextable::compose(part = "header", j = 2, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    flextable::compose(part = "header", j = 3, value = as_paragraph(as_b("Parameter"))) %>% 
    flextable::compose( part = "header", j = 4, value = as_paragraph(as_b("Estimate"))) %>% 
    flextable::compose(part = "header", j = 5, value = as_paragraph(as_b("SE"))) %>%
    flextable::compose(part = "header", j = 6, value = as_paragraph(as_b("df"))) %>% 
    flextable::compose(part = "header", j = 7, value = as_paragraph(as_b("95% CI"), as_sub(as_b(" lower")))) %>% 
    flextable::compose(part = "header", j = 8, value = as_paragraph(as_b("95% CI"), as_sub(as_b(" upper")))) %>% 
    flextable::compose(part = "body", j = 3, i = c(1,4,7,10), value = as_paragraph("Intercept")) %>%
    flextable::compose(part = "body", j = 3, i = c(1,4,7,10)+1, value = as_paragraph("Population", as_sub(" Kenya"))) %>%
    flextable::compose(part = "body", j = 3, i = c(1,4,7,10)+2, value = as_paragraph("Background", as_sub(" Kenya"))) %>% 
    font(fontname="Times", part = "body") %>% 
    font(fontname="Times", part = "header")
  return(table)
}

table_style10 <- function(table){
  table <- table %>%
    width(j = 3, width = 6) %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    flextable::compose(part = "header", j = 1, value = as_paragraph(as_b("Social Context"))) %>% 
    flextable::compose(part = "header", j = 2, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    flextable::compose(part = "header", j = 3, value = as_paragraph(as_b("Parameter"))) %>% 
    flextable::compose(part = "header", j = 4, value = as_paragraph(as_b("Estimate"))) %>% 
    flextable::compose(part = "header", j = 5, value = as_paragraph(as_b("SE"))) %>%
    flextable::compose(part = "header", j = 6, value = as_paragraph(as_b("df"))) %>% 
    flextable::compose(part = "header", j = 7, value = as_paragraph(as_b("95% CI"), as_sub(as_b("lower")))) %>% 
    flextable::compose(part = "header", j = 8, value = as_paragraph(as_b("95% CI"), as_sub(as_b("upper")))) %>% 
    flextable::compose(part = "body", j = 3, i = c(1, 6, 14, 19), value = as_paragraph("Intercept")) %>%
    flextable::compose(part = "body", j = 3, i = c(1, 6, 14, 19)+1, value = as_paragraph("Background Population", as_sub(" Kenya"))) %>%
    flextable::compose(part = "body", j = 3, i = c(1, 6, 14, 19)+2, value = as_paragraph("Body Region", as_sub(" midflank"))) %>%
    flextable::compose(part = "body", j = 3, i = c(1, 6, 14, 19)+3, value = as_paragraph("Body Region", as_sub(" tailbase"))) %>%
    flextable::compose(part = "body", j = 3, i = c(1, 6, 14, 19)+4, value = as_paragraph("Body Region", as_sub(" topflank"))) %>%
    flextable::compose(part = "body", j = 3, i = c(11,24), value = as_paragraph("Background Population", as_sub(" Kenya"), "* BodyRegion", as_sub(" midflank"))) %>%
    flextable::compose(part = "body", j = 3, i = c(11,24)+1, value = as_paragraph("Background Population", as_sub(" Kenya"), "* BodyRegion", as_sub(" tailbase"))) %>%
    flextable::compose(part = "body", j = 3, i = c(11,24)+2, value = as_paragraph("Background Population", as_sub(" Kenya"), "* BodyRegion", as_sub(" topflank"))) %>% 
    font(fontname="Times", part = "body") %>% 
    font(fontname="Times", part = "header")
  
  return(table)
}

table_style11  <- function(table){
  table <- table %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    flextable::compose(part = "header", j = 1, value = as_paragraph(as_b("Context"))) %>% 
    flextable::compose( part = "header", j = 2, value = as_paragraph(as_b("Contrast"))) %>% 
    flextable::compose(part = "header", j = 3, value = as_paragraph(as_b("F"))) %>%
    flextable::compose(part = "header", j = 4, value = as_paragraph(as_b("DF"))) %>% 
    flextable::compose(part = "header", j = 5, value = as_paragraph(as_b("p"))) %>% 
    flextable::compose(part = "header", j = 6, value = as_paragraph(as_b("p"), as_sub(as_b(" Bonferroni")))) %>%
    flextable::compose(part = "body", i = c(1,5), j = 2, 
            value = as_paragraph("Kenyan Background", as_sub(" gular"), " - ", "Hawaiian Background", as_sub(" gular")))  %>% 
    flextable::compose(part = "body", i = c(2,6), j = 2, 
            value = as_paragraph("Kenyan Background", as_sub(" midflank"), " - ", "Hawaiian Background", as_sub(" midflank"))) %>% 
    flextable::compose(part = "body", i = c(3,7), j = 2, 
            value = as_paragraph("Kenyan Background", as_sub(" tailbase"), " - ", "Hawaiian Background", as_sub(" tailbase"))) %>% 
    flextable::compose(part = "body", i = c(4,8), j = 2, 
            value = as_paragraph("Kenyan Background", as_sub(" topflank"), " - ", "Hawaiian Background", as_sub(" topflank"))) %>% 
    font(fontname="Times", part = "body") %>% 
    font(fontname="Times", part = "header") %>% 
    width(j = 1, width = 1) %>% width(j = 2, width = 8)
  
  return(table)
}


table_style12 <- function(table){
  table <- table %>%  width(j = 1, width = 1.5) %>% width(j = 2, width = 3) %>% 
    align(align = "center", part = "header") %>% 
    flextable::compose(part = "header", j = 1, value = as_paragraph(as_b("Hypothesis"))) %>% 
    flextable::compose(part = "header", j = 2, value = as_paragraph(as_b("Prediction"))) %>% 
    flextable::compose(part = "header", j = 3, value = as_paragraph(as_b("Population"))) %>% 
    flextable::compose( part = "header", j = 4, value = as_paragraph(as_b("Visual System"))) %>% 
    flextable::compose( part = "header", j = 5, value = as_paragraph(as_b("Background"))) %>% 
    font(fontname="Times", part = "body") %>% 
    font(fontname="Times", part = "header")
  return(table)
}


table_style13 <- function(table){
  table <- table %>%
    width(j = 3, width = 6) %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    flextable::compose(part = "header", j = 1, value = as_paragraph(as_b("Visual Spectrum"))) %>% 
    flextable::compose(part = "header", j = 2, value = as_paragraph(as_b("Parameter"))) %>% 
    flextable::compose(part = "header", j = 3, value = as_paragraph(as_b("Estimate"))) %>% 
    flextable::compose(part = "header", j = 4, value = as_paragraph(as_b("SE"))) %>%
    flextable::compose(part = "header", j = 5, value = as_paragraph(as_b("df"))) %>% 
    flextable::compose(part = "header", j = 6, value = as_paragraph(as_b("95% CI"), as_sub(as_b("lower")))) %>% 
    flextable::compose(part = "header", j = 7, value = as_paragraph(as_b("95% CI"), as_sub(as_b("upper")))) %>% 
    flextable::compose(part = "body", j = 2, i = c(1, 10), value = as_paragraph("Intercept")) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 10)+1, value = as_paragraph("Snout-vent Length")) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 10)+2, value = as_paragraph("Population", as_sub(" Kenya"))) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 10)+3, value = as_paragraph("Body Region", as_sub(" midflank"))) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 10)+4, value = as_paragraph("Body Region", as_sub(" tailbase"))) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 10)+5, value = as_paragraph("Body Region", as_sub(" topflank"))) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 10)+6, value = as_paragraph("Population", as_sub(" Kenya"), "* BodyRegion", as_sub(" midflank"))) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 10)+7, value = as_paragraph("Population", as_sub(" Kenya"), "* BodyRegion", as_sub(" tailbase"))) %>%
    flextable::compose(part = "body", j = 2, i = c(1, 10)+8, value = as_paragraph("Population", as_sub(" Kenya"), "* BodyRegion", as_sub(" topflank"))) %>% 
    font(fontname="Times", part = "body") %>% 
    font(fontname="Times", part = "header")
  
  return(table)
}