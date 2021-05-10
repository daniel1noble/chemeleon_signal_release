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

data_merge <- function(table1_background, table2_background, table3, table4){
    spec_data <- cbind(table1_background, table2_background, table3, table4)[,-c(3,5,8)]
    spec_data <- pivot_longer(spec_data, cols = c(2:7)) %>% 
                 arrange(name) %>% 
                 mutate(bodyreg = gsub(".hawaii|.kenya", "", name),
                            pop = gsub("gular.|topflank.|leaf.", "", name))
    return(spec_data)
}
bird_spec_data <- data_merge(leaves_hawaii, leaves_kenya, bodyregions_hawaii_bird, bodyregions_kenya_bird)

# Plot!
  pdf(file = "./output/figures/spec_birds.pdf", width = 8.588235,  height = 6.955882)  
plot_spec <- function(data){
        ggplot(data, aes(x = wl, y = value, group = name)) + 
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
            legend.title = element_text(size = 20))}
      plot_spec(bird_spec_data)

  dev.off()  
  

### Now, run through more simplified processing using all the functions, we already have the background, so now it's just getting the contexts

  # Get snakes
   bodyregions_hawaii_snake <- get_body_regions(dir = "./data/specs/processed/snake.dat_hawaii.csv", label = "hawaii")
    bodyregions_kenya_snake <- get_body_regions(dir = "./data/specs/processed/snake.dat_kenya.csv", label = "kenya")

      # Create composite data
    snake_spec_data <- data_merge(leaves_hawaii, leaves_kenya, bodyregions_hawaii_snake, bodyregions_kenya_snake)

    # Plot snakes
    pdf(file = "./output/figures/spec_snake.pdf", width = 8.588235,  height = 6.955882)  
      plot_spec(snake_spec_data)
    
    dev.off()

  # Get Male-Male Displays

     bodyregions_hawaii_maleDisp <- get_body_regions(dir = "./data/specs/processed/display.dat_hawaii.csv", label = "hawaii")
    bodyregions_kenya_maleDisp <- get_body_regions(dir = "./data/specs/processed/display.dat_kenya.csv", label = "kenya")

      # Create composite data
    maleDisp_spec_data <- data_merge(leaves_hawaii, leaves_kenya, bodyregions_hawaii_maleDisp, bodyregions_kenya_maleDisp)

    # Plot snakes
    pdf(file = "./output/figures/spec_maleDisp.pdf", width = 8.588235,  height = 6.955882)  
      
      plot_spec(maleDisp_spec_data)
    
    dev.off()


     # Get Courtship Displays

     bodyregions_hawaii_CourtDisp <- get_body_regions(dir = "./data/specs/processed/court.dat_hawaii.csv", label = "hawaii")
    bodyregions_kenya_CourtDisp <- get_body_regions(dir = "./data/specs/processed/court.dat_kenya.csv", label = "kenya")

      # Create composite data
    CourtDisp_spec_data <- data_merge(leaves_hawaii, leaves_kenya, bodyregions_hawaii_CourtDisp, bodyregions_kenya_CourtDisp)

    # Plot snakes
    pdf(file = "./output/figures/spec_CourtDisp.pdf", width = 8.588235,  height = 6.955882)  
      
      plot_spec(CourtDisp_spec_data)
    
    dev.off()