#row.names(uniquedf) <- NULL 
library(stp5)
Start("",OutDec=".")
 
 
load("TestData.Rdata")
 


APA(~ ., DF, caption="caption Test1")
APA(~ ., DF, caption="caption Test2, center=FALSE", center=FALSE)
APA(aov( BMI ~Edu  , DF ) )

APA(aov( SOC ~Edu +PSD *Sex+IQ + +PSD +BMI +PSD, DF ), stars = c(0.001, 0.01, 0.05,0.50) )

APA( BMI + PSD~Edu +Sex , DF, fun=mean2  )

APA(Group + BMI+ PSD + SOC + Sex~Edu , DF, test=T) 
 
APA(~BMI+ PSD + SOC + IQ , DF, test=T, caption="Tabelle 2")
APA(lm(BMI~ PSD + SOC + IQ , DF))


APA3<- function(o,data, fun){
  print(substitute(fun)=="xtabs")
  x <-  strsplit(gsub(" ", "",deparse(o[[2L]])), split = "\\+")
  x <- if( x ==".")  names(data) else  x
  var_vektor <- unlist(x)
  mydata <- if(length(var_vektor)==1){data[var_vektor]} else{data[, var_vektor]}                          
  mylabels <- Hmisc::label(mydata)
  colnames(mydata) <- ifelse( mylabels == "", names(mylabels), mylabels )
 
  molten_data <- melt(cbind(my_id=1: nrow(mydata), mydata), id.vars=1)
  print(head(molten_data))
  cast( molten_data, variable ~. ,  fun.aggregate=fun )  
  
}

APA3(~ Edu +Sex , DF, fun=Prozent  )

APA(~ Edu  , DF, fun=Prozent  )


End()

