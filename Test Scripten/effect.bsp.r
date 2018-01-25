graphics.off()
library(effects)
library(stp5)
library(sjPlot)

data(Cowles)

head(Cowles)
 
Cowles$extraversion[1]<-NA
mod.cowles <- glm(volunteer ~ sex*extraversion + neuroticism*extraversion,
                  data=Cowles, family=binomial)



eff.cowles <- allEffects(mod.cowles )
windows(8,8)
plot(eff.cowles, multiline=T)
str(mod.cowles)

fit<- mod.cowles
UVs<-Formula_Names( fit$formula ) 

 
 for(i in UVs$xname){
  cat("\n----\n", i, "\n")
   vars<-unlist( strsplit(i, "[:*]"))
 
   
   if(length(vars)>1){
 
     windows(3,3)
     if( is.factor(fit$data[vars[2]])) {
       
   print( 
     plot(
       effect(i, fit,
       x.var= vars[1]) 
       ,  main="", multiline=T, rug=F
       )
   )}else {
    # print("Hallo")
    # print(vars)
     xlvls <- fivenum(fit$data[,vars[2]]) 
     
     print(xlvls)
     names(xlvls)<-vars[2]
     print( 
     plot(
       effect(i, fit,
              x.var= vars[1]) 
       ,  main="", multiline=T, rug=F
       ,xlevels=list(xlvls)
     )
   )}
     
     
   }
 }


#x<-unlist(strsplit("sex:extraversion", ":"))
#x

# 
# windows(3,3)
# plot(effect("sex:extraversion", mod.cowles, 
#             xlevels=list(extraversion=seq(0, 24, 12)))
#  # ,x.var="sex"
#   )

