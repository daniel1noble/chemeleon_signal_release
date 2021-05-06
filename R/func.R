p_value <- function(x){
  if(x <= 0.0001) {tmp = "< 0.0001"}
  if(x <= 0.001 & x >= 0.0001) {tmp ="< 0.001"}
  if(x <= 0.01 & x >= 0.001) {tmp ="< 0.01"}
  if(x >= 0.01) {tmp = round(x, digits =2)}
  return(tmp)
}


wald_table <- function(model_main, model_inter){
  
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