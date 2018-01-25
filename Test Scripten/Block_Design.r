#load libraries
#install.packages("merTools")
library(merTools)
library(effects)
library(stp25vers)
Projekt("html")
#library(lme4)
library(arm) #for the sim function

#simulate a dataset
set.seed(10)
# we measured plant biomass at 20 different sites with varying temperature
# and different nutrient levels
#we have 5 sample per sites

n<-100*2
data<-data.frame(Temp=runif(n,-2,2),
                 N=runif(n,-2,2),
                 Site=
                     gl(n=20,k=n/20,labels=paste0("Site",1:20))
                )
#some simulated parameters
params<-c(10,0.3,2.2)
rnd_int<-rnorm(n/5,0,1.2)
rnd_slp<-rnorm(n/5,0,0.5)
#the expected plant biomass
mus<-params[1]+rnd_int[data$Site]+(params[2]+rnd_slp[data$Site])*data$Temp+params[3]*data$N
#one simulation
data$Biomass<-rnorm(n,mus,1)

windows(8,8)
library(lattice)
histogram( ~ log(Biomass)+1, data)
head(data)
#data %>% Tabelle2(Biomass, Temp, N,Site)

data$Biomass2 <- data$Biomass<median(data$Biomass)
data$Biomass2 <-
log(data$Biomass )

#fitting the models
fit0<- lm(Biomass~1,data)
fit1<-lm(Biomass~Temp+N,data)
fit2<- glm( Biomass2~Temp+N,data, family = binomial)
fit3<-lmer(Biomass~Temp+N+(1+Temp|Site),data)
#APA_Table(fit, m1)

#APA_Table( m1, type="tex")
#APA_Table( m1, type="long")
#APA_Table(fit, m1, type="long"),  include.pseudo=TRUE
   APA_Table(fit0, fit1, fit2, fit3, type="long")
R2(fit2)

#plot(allEffects(fit))
End()
