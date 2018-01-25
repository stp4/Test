#row.names(uniquedf) <- NULL 
library(stp5)


APA<-function(x,...){
  
  print(x)
}
Start("html",OutDec=".")
n<- 100
set.seed(12)
DF<- data.frame(Group= gl(2, 1, n, labels=c("Control", "Treat"))
               , Edu= gl(3, 3, n, labels = c("A-Level", "B-Level", "C-Level"))
                ,Sex=gl(2,7,n, labels=Cs(male,female)), Age=cut(rnorm(n),5, labels=c( 20,30,40,50,60))
                ,PSQ=round(rnorm(n)*50)
                ,IQ=round(rnorm(n)*100)
                )


DF <- upData(DF
             ,PSD=  round(PSQ  + rnorm(n),1)
             ,BMI= as.numeric(as.character(cut( (PSD+IQ/100+PSQ+rnorm(n) ), 10, labels=c(18,19,20,22,23,25,27,29,34,38))))
             ,SOC=round(BMI^2+ rnorm(n) )
             ,labels=c(Sex="Gender"))
#DF$BMI[3:7]<-replace(DF$BMI, DF$BMI<20, NA)
DF$PSD[5:10]<-NA
DF$PSQ[7:20]<-NA
DF$SOC[7:10]<-NA  
DF$IQ[10:30]<-NA 
DF$Group[1:5]<-NA 
DF$Edu[1:2]<-NA 
#save(DF,file="TestData.Rdata")


APA2(~Sex, DF ) 
errate_statistik2(~Sex  , DF ) 
errate_statistik2(~IQ, DF, caption="caption Test1", type="median")

DF$PSQ

APA2(~ ., DF, caption="caption Test1", type="median")
#recast(DF, Group ~ ., id.var = 1, measure.var= Cs(PSQ ,  IQ ,  PSD))
head(DF)

(x<- dcast(DF, Group + Edu ~  Sex  ,Mean2, value.var="PSQ" ))
#APA(x, caption="dcast")




#head(DF)
#?ddply

#?recast(DF, BMI+PSD~.)
#qq(Sex~BMI, DF)

#APA(DF)
                
#Linear mixed-effects models


attach(warpbreaks)
#ave(breaks, wool)
#ave(breaks, tension)


library(nlme)

model.1 <- lme(distance ~ age, data = Orthodont, random = ~ 1)
model.2 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1)
summary(model.3 <- lm(SOC ~Edu +PSD *Sex+IQ + +PSD +BMI +PSD, data = DF))




APA2(~ ., DF, caption="caption Test1" )
#Print2.formula(~ .,   DF  , level.position = 1)
#Print2(~ ., DF)

APA2(~ ., DF, caption="caption Test2, center=FALSE", center=FALSE)


APA2(aov( BMI ~Edu  , DF ) )

APA2(aov( SOC ~Edu +PSD *Sex+IQ + +PSD +BMI +PSD, DF ), stars = c(0.001, 0.01, 0.05,0.50) )
anova(model.3 )
#class( anova(model.3 ) )
#str(aov( BMI ~Edu +Sex , DF ) )

APA2( BMI + PSD~Edu +Sex , DF, fun=mean2  )


#?dcast


#   APA(BMI+ PSD + SOC~ Sex, DF)
#HTML(Print2(BMI+ PSD + SOC~ Sex, DF, test=T))
APA2(Group + BMI+ PSD + SOC + Sex~Edu , DF, test=T) 
 
#APA( ~., as.data.frame(Orthodont), caption="Tabelle 1")
APA(~BMI+ PSD + SOC + IQ , DF, test=T, caption="Tabelle 2")
APA(lm(BMI~ PSD + SOC + IQ , DF))

#texreg:::stars.string
# stars.string<-function (pval, stars, star.char, star.prefix, star.suffix, symbol){
# paste(" p=", stp4:::ffpvalue(pval)  )  }

#fixInNamespace("stars.string", "texreg")
#assignInNamespace( "stars.string",stars.string, "texreg")    
#texreg:::stars.string


 #screenreg( model.1 ,  include.pvalues=TRUE  )
#texreg:::get.data(model.1)
APA(lm(BMI~ PSD + SOC + IQ , DF) , pvalues=TRUE )

names(model.1)#formula

summary(model.3 )


#getFormulaName(list(model.1, model.2,model.3 ), c("Model A","B","C"))

#Guetekriterien(list(model.1, model.2,model.3 ) )
#Guetekriterien( model.3  )



APA(list(model.1, model.2,model.3 ),caption="Abbildung 4", custom.model.names=c("Model A","B","C")
    , single.row=F
    ,  stars = c(0.001, 0.01, 0.05,0.10) 
 
    )






 
#APA(Edu~Sex  , DF, fun=Prozent)
#APA(BMI~Sex  , DF, fun=Prozent,  breaks = fivenum(DF$BMI, na.rm = TRUE)[-3],labels = Cs(low, med, hig))





APA(BMI+ PSD~Sex  , DF, fun=mean2 )
APA(BMI+ PSD~Sex  , DF, fun=Mean2)

APA(BMI+ PSD~Sex  , DF,  fun=Median2)



APA(~BMI+ PSD + SOC + IQ , DF )

Hmisc::describe(DF) 
datadensity(DF)

#End()
n<-100
lvs<-Cs(a,b,c,d,e,f,h)
DF2<- data.frame(
  Magazines=gl(length(lvs),1,n,lvs)
  ,Comic.books=gl(length(lvs),2,n,lvs)
  ,Fiction=gl(length(lvs),3,n,lvs)
  ,Newspapers=gl(length(lvs),5,n,lvs))

#D#F<-sample(DF)[1:37,]

l29 <- likert2(DF2 )

class(l29)
APA(l29)

l29$results

#mean2(Reliability(DF[, Cs(BMI, PSD, SOC, IQ)]))

Scala2<- data.frame(lapply(DF[, Cs(BMI, PSD, SOC, IQ)], scale))
#  mean2(Reliability( Scala2 ))
#  mean2(Reliability( Scala2,2 ))
# Reliability( Scala2, apa=TRUE) 
# Reliability( Scala2, 2, apa=TRUE) 


APA(Scala2,2)
#library(memisc )   #   install.packages("memisc" )


#TransformToHmisc<- function(Data){
 # DF<- cleanup.import(as.data.frame(Data))  
 # my_labs<-  gsub("'","", c(description(Data) ))
 # upData(DF , labels= my_labs) 
 # DF
#}

DF3<-GetData("Arbeitsdaten.sav")
 






#data2 <- as.data.set(spss.system.file('Arbeitsdaten2.sav'))
#ncol(data4 <- TransformToHmisc(data2))

#head(data2.1)
##ncol(data3 <- GetData('Arbeitsdaten2.sav'))
 


#DF3 <- data.frame(lapply( DF3[, 11:23], scale ))

#APA(~. , DF3  ,test=T)

# Varimax Rotated Principal Components
# retaining 5 components 
# library(psych)
#fit <- principal(DF3, nfactors=5, rotate="varimax")
#(fit) # print results
##library(mva)  #install.packages("mva")
##prcomp(na.omit(DF3), scale = TRUE)
#? principal 

#Start("html")
APA(x<-xtabs( ~Edu+Sex  , DF ), margin=2)



 
 
 
 #  stat<- assocstats(x)
 # ff(stat$chisq_tests[,3])
#APA(xtabs( ~Edu+Sex  , DF ) )  
#  

#library(xtable)
 


#tab<-xtabs( ~Edu+Sex  , DF )
#HTML(print(xtable(tab),floating=FALSE, type = "html"))

#Print2(tab)
#str( ans<-APA(xtabs( ~Edu+Sex +Group , DF ))   )
#as.data.frame.matrix(ans$Test$Chisq)
#?xtable



#  
 
End()

