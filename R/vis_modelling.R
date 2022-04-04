#setup - working directory and loading library
setwd("~/Dropbox/DSF_Katrina/chameleon/1_Data")
library(pavo)

#import reflectance, irradiance files
reflectances=read.csv(file.choose(),header=TRUE)
reflectances=as.rspec(reflectances) #where "rspec" means it reads first column as wavelength
specsenschameleon=read.csv(file.choose(),header=TRUE)
specsenschameleon=as.rspec(specsenschameleon)
specsensbirdUV=read.csv(file.choose(),header=TRUE)
specsensbirdUV=as.rspec(specsensbirdUV)
specsenssnake=read.csv(file.choose(),header=TRUE)
specsenssnake=as.rspec(specsenssnake)

#run visual model to get quantum catches
sunchameleon=vismodel(reflectances, visual = specsenschameleon[, 1:5], achromatic=specsenschameleon[,6], illum = 'D65', qcatch = 'fi', relative=FALSE, vonkries=TRUE)
sunbirdUV =vismodel(reflectances, visual = specsensbirdUV[, 1:5], achromatic=specsensbirdUV[,6], illum = 'D65', qcatch = 'fi', relative=FALSE, vonkries=TRUE)
sunsnake= vismodel(reflectances, visual = specsenssnake[, 1:4], achromatic=specsenssnake[,4], illum = 'D65', qcatch = 'fi', relative=FALSE, vonkries=TRUE)

#calculate contrasts (dS = chromatic contrasts, dL = achromatic contrasts)
Csunchameleon = coldist(sunchameleon, noise="neural", achro=TRUE, n = c(1,1.6,3.3,3.2), weber = 0.1, weber.ref = 4, weber.achro = 0.05)
Csunbirduv = coldist(sunbirdUV, noise="neural", achro=TRUE, n = c(1,2,2,4), weber = 0.06, weber.ref = 4, weber.achro = 0.05)
Csunsnake = coldist(sunsnake, noise="neural", achro=TRUE, n = c(1,1.6,7.3), weber = 0.1, weber.ref = 3, weber.achro = 0.05)

#Export back to Excel
write.table(Csunchameleon, "~/Dropbox/DSF_Katrina/chameleon/DisplayHawaiiCham_5nm.csv", sep="\t") #comma-delimited text file
write.table(Csunbirduv, "~/Dropbox/DSF_Katrina/chameleon/Csunbirduv_3b.csv", sep="\t") #comma-delimited text file
write.table(Csunsnake, "~/Dropbox/DSF_Katrina/chameleon/Csunsnake_2b.csv", sep="\t") #comma-delimited text file
