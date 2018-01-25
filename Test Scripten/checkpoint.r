# To create a checkpoint project, you do:
#     
#     Create a new folder and change your working directory to this folder. If you use an IDE like RStudio, this is identical to creating a new RStudio project.
# Add your R script files to this folder.
# Add a checkpoint to the top of the script:
# #install.packages("checkpoint")



library(checkpoint)
checkpoint("2015-07-01")
getOption("repos")
normalizePath(.libPaths(), winslash = "/")
#grep("checkpoint", .libPaths())   
#installed.packages()
install.packages("C:/Users/wolfgang/Dropbox/3_Forschung/R-Project/stp5_1.14.tar.gz", 
                 repos = NULL, 
                 type = "source")

library(installr) # fuer require2(installr)
#-- check.for.updates.R()

##### @importFrom caret confusionMatrix
#library("confusionMatrix" )
library("Hmisc")
library("car")
library("effects")

library("stringr")


require("lattice")
require("latticeExtra")
#require2("reshape")
library("reshape2")
library("dplyr")
library("psych") 
library("arm")
library("texreg")

library("R2HTML")
library("Gmisc") # for ?htmlTable
library("caret")  # f?r xtabs caret:::print.confusionMatrix spezifit?t usw

library("vcd") # f?r xtabs Chi-Test

library("diagram")  # Flussdiagramme


library(htmlTable)
library(lmtest)


# cleanup
#unlink(getwd(), recursive = TRUE)
 
