library(memisc )   #   install.packages("memisc" )
library(stp4)
Start("")
#  ?Sys.setlocale(locale="de_DE.ISO8859-15")

#Sys.setlocale("LC_TIME", "German") # Windows
#Sys.getlocale("locale")
#Sys.getlocale("LC_TIME")

TransformToHmisc<- function(Data){
  DF<-  as.data.frame(Data)  
   my_labs<-  gsub("'","", c(description(Data) ))
   upData(DF , labels= my_labs) 
 # DF
}

DF3<-GetData("Arbeitsdaten.sav")
N=1640
cols<-1357


#?spss.system.file




data2 <- as.data.set(spss.system.file('Arbeitsdaten2.sav'))

ncol(   as.data.frame(data2)   )
DF<- upData(as.data.frame(data2))  

ncol(data4 <- TransformToHmisc(data2))

 
ncol(data3 <- GetData('Arbeitsdaten2.sav'))


head(data3[,c(29:35)])
End()