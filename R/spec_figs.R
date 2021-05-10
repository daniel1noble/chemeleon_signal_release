# Create spectral reflectance curves
  rm(list=ls())
# Libraries
  pacman::p_load(pavo, tidyverse)
  
  # Function to extract, clean and aggregate background
  get_background <- function(dir, background = "leaf", label = NULL){
    ## Import spec data, name wl
    bkgrd <- read.csv(file = dir)
    bkgrd <- bkgrd %>% rename("wl" = "wavelength")
    
    ## Get leaves
    bkgrd <- bkgrd[, c("wl",grep(background, colnames(bkgrd), value = TRUE))]
    
    ## Smooth specs
    bkgrd <- procspec(bkgrd, opt = "smooth" , span = 0.2)
    
    # Aggregate specs
    avg_bkgrd <- aggspec(bkgrd)
    colnames(avg_bkgrd)[2] <- paste0(background, ".",  label)
   
     return(avg_bkgrd)
    
  }
  
  
  leaves_hawaii <- get_background(dir = "./data/specs/processed/hawaii_background.csv", label = "hawaii")
   leaves_kenya <- get_background(dir = "./data/specs/processed/kenya_background.csv", label = "kenya")
  
  # Function to extract, clean and aggregate body regions of interst. More than one body region at once. Out put is a column for each body region, the boy regions are aggregated across all individuals
   get_body_regions <- function(dir, body_region = "gular|topflank", label = NULL){
     ## Import spec data, name wl
     bkgrd <- read.csv(file = dir)
     bkgrd <- bkgrd %>% rename("wl" = "wavelength")
     
     ## Get body regions of interest
     bkgrd <- bkgrd[, c("wl",grep(body_region, colnames(bkgrd), value = TRUE))]
     
     ## Smooth specs
     bkgrd <- procspec(bkgrd, opt = "smooth" , span = 0.2)
     
     # Aggregate specs
     body_region_split <- unlist((strsplit(body_region, "|", fixed = TRUE)))
     avg_bkgrd <- data.frame(wl = bkgrd[,1])
     
     for(i in 1:length(body_region_split)){
            avg_bkgrd[,i+1] <- aggspec(bkgrd[, c("wl", grep(body_region_split[i], colnames(bkgrd), value = TRUE))])[,2]
            colnames(avg_bkgrd)[i+1] <- paste0(body_region_split[i],".",label)
     }
     
     return(avg_bkgrd)
     
   }
   
   bodyregions_hawaii_bird <- get_body_regions(dir = "./data/specs/processed/bird.dat_hawaii.csv", label = "hawaii")
    bodyregions_kenya_bird <- get_body_regions(dir = "./data/specs/processed/bird.dat_kenya.csv", label = "kenya")
   
  
# Merge all together, add some grouping variables
    bird_spec_data <- cbind(leaves_hawaii, leaves_kenya, bodyregions_hawaii_bird, bodyregions_kenya_bird)[,-c(3,5,8)]
    bird_spec_data <- pivot_longer(bird_spec_data, cols = c(2:7)) %>% arrange(name) %>% 
                          mutate(bodyreg = gsub(".hawaii|.kenya", "", name),
                                 pop = gsub("gular.|topflank.|leaf.", "", name))
# Plot!
  pdf(file = "./output/figures/spec_birds.pdf", width = 8.588235,  height = 6.955882)  
      ggplot(bird_spec_data, aes(x = wl, y = value, group = name)) + 
      geom_line(aes(colour = bodyreg, linetype = pop), size = 1.2) + 
      theme_bw() +
      labs(x = "Wavelength (nm)", y = "Reflectance (%)", linetype = "Population", colour = "Body Region / Environment") + 
      scale_color_discrete(labels = c("Gular Region", "Leaf Background", "Top-flank Region")) +
      scale_linetype_discrete (labels = c("Hawaii", "Kenya")) +
      theme(legend.position  = c(0.25, 0.75),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 24),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 20))
  dev.off()  
  
  