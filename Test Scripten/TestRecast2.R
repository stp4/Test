#546  Gottfried SALCHNER
#gottfried.salchner@marshallcenter.org
#+49 (0) 157 3804 8443
##-- Beschreinunh der Statistik

              #--21.06.2015 20:27:01     21.06.2015 22:57:55

graphics.off()
library(RColorBrewer)
 require(plyr)
library(stp5)
library(haven)
 
 Start("html")
# save(x, DF , file= "C:/Users/wolfgang/Dropbox/1_Projekte/DummyRecast.Rdata")
 
# load(  "C:/Users/wolfgang/Dropbox/1_Projekte/DummyRecast.Rdata" )
 data<- data.frame(sex= gl(2, 8, labels = c("male", "female")),
                   x1=c(1,2,3,1,1,2,3,4, 1,1,1,1,1,1,1,1),
                   x2=c(1,2,3,3,1,2,3,4, 2,2,3,3,1,2,3,4),
                   x3=c(1,2,3,1,1,2,3,1, 2,2,3,3,1,2,3,4),
                   x4=c(1,3,3,4,1,2,3,1, 2,3,3,3,1,2,3,4),
                   x5=c(4,4,4,4,4,4,4,4, 2,2,3,3,1,3,3,4),
                   x6=c(2,2,3,4,1,2,3,4, 4,4,3,3,3,3,3,4)
 )

 
 
 
 prz <- function(x){
     round((mean(x, na.rm=T)-1)/3*100,0)
     }
 
     
res1 <- Recast2(.~sex, data, fun= prz)   
 
res2<- Recast2(.~sex, data, fun= prz, margins=T)   
 res1
 res2[1:nrow(res1), ]
 
 data$gr <-  cut(data$x2 * data$x3, 2, c("A", "B"))
 
 res <-  Recast2(. ~ sex + gr, data , fun = prz)
 
 res <- Recast2(. ~ sex + gr, data , fun = prz, margins = T)
 
 res <- Recast2(. ~ sex + gr, data , fun = prz, margins = "sex")
 
 res <- Recast2( . ~ sex + gr, data , fun = prz, margins = "sex" , formula = variable + gr ~ sex)
 res <- Recast2( . ~ sex + gr, data , fun = prz, margins = "gr" ,  formula = variable + gr ~ sex)
 
 res[1,3]
 Start()
 set.seed(1)
 data<-data.frame(x=1:30, 
                  y=gl(3, 10, labels = c("sehr gut", "mittel", "schlecht")),
                  z=gl(2, 15, labels = c("Control", "Treat"))[sample.int(30)]
 )
 res<-APA2(x~y, data, print=FALSE)
 
 
 ?htmlTable
 End()
 