#--

library(stp25)
setwd("C:/Users/wpete/Dropbox/3_Forschung/1 Statistik/BspDaten/423 Alois Koegl")
#-- Load Data ---------------------------------------------
#Steko<-NULL
if (!exists("Steko")){
tmdr<- tempdir()
data <- unzip("Steko.zip", exdir= tmdr )
fn<-paste0(tmdr, "\\Steko.sav")




Steko<- GetData(fn) %>% Drop_NA(key)

file.remove(fn)
}
# some(DF <- GetData("Raw data/File.R"))    # summary(~., DF)
#APA2(~geb_land, Steko)

Projekt("html")
library(dplyr)

levels(Steko$geb_ort) <- tolower(levels(Steko$geb_ort))

Steko<- Steko %>%
         mutate(jahr = factor(jahr),
                 WHtR_1 =round(WHtR_1, 2)) %>%
         Label(BMI= "Body-Mass-Index",
               WHtR=  "Waist-Height-Ratio",
               WHtR_1="Waist-Height-Ratio",
               bildprof="Bildungsprofil",
               jahr = "Jahr"#Rauchverhalten Sichtigkeit #hw.bld=Bundesland  gemstat = Gemeinde
           )


x<-   aggregate(cbind(BMI, WHtR_1)~ jahr+Bldngspr,
                Steko,
                function(x) c(m=mean(x), sd=sd(x) ))

x
#Recast2(BMI+ WHtR_1 ~ jahr+Bldngspr, Steko, mean)


# library(dplyr)
#
# summary(Steko)
# levels(Steko$Rchvrhlt)
# sapply(Steko, class)
#vars<- c("Bundslnd",  )
#Steko2<- as.data.frame(Steko)
# str(Steko[["Rchvrhlt"]])
# str(DF[["Smoker"]])

APA2(~BMI+Rchvrhlt, Steko)
Steko %>% Table(BMI)
Steko %>% Table(WHtR_1="mean", BMI="median",Rchvrhlt, Bundslnd)

 #%>% Output()
End()
