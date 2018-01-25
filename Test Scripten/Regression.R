options(contrasts = c("contr.Treatment", "contr.poly"))
library(car)
library(broom)
library(lattice)
library(stp25)
Projekt("html")
#library(dplyr)

fit1<-lm(y1~x1, anscombe)
fit2<-lm(y2~x2, anscombe)
#library(effects)
#allEffects(fit1) %>% Ordnen() %>% fix_format() %>% Output()
Describe(fit1)  %>% Format2(digits=c(0, 0,2,2))


anov1<-aov(y1~x1, anscombe)
Head("ANOVA")
APA_Table(fit1, type="anova")
APA_Table(Anova(fit1))
APA_Table(anov1)
etaSquared2(anov1)


APA_Table(fit1)
 APA_Table(fit1, fit2, type="anova")
 APA_Table(fit1, fit2, type=c("broom", "anova"))
 APA_Table(fit1, fit2, type="tex")
 APA_Table(fit1, fit2, type="st")
RMSE(fit1) #RMSE
 RMSE(fit2) #RMSE






# Get Pseudo R²
R2(fit1)
R2(fit2)


## Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
Describe(d.AD <- data.frame(treatment, outcome, counts))
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())

Describe(glm.D93)

APA_Table(glm.D93)
anova(glm.D93)
summary(glm.D93)



Fr <- c(68,42,42,30, 37,52,24,43,
     	66,50,33,23, 47,55,23,47,
    	63,53,29,27, 57,49,19,29)

  Temp <- gl(2, 2, 24, labels = c("Low", "High"))
  Soft <- gl(3, 8, 24, labels = c("Hard","Medium","Soft"))
  M.user <- gl(2, 4, 24, labels = c("N", "Y"))
  Brand <- gl(2, 1, 24, labels = c("X", "M"))
  detg <- data.frame(Fr,Temp, Soft,M.user, Brand)

  detg.m0 <- glm(Fr ~ M.user*Temp*Soft + Brand, family = poisson, data = detg)
  APA_Table(detg.m0)


  p <- read.csv("http://www.ats.ucla.edu/stat/data/poisson_sim.csv")
  p <- within(p, {
    prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
    id <- factor(id)
  })
 # Describe(m1)
  m1 <- glm(num_awards ~ prog + math, family="poisson", data=p)
 APA_Table(m1, type=c("broom", "anova"))
  R2(m1)


## an example with offsets from Venables & Ripley (2002, p.189)
utils::data(anorexia, package = "MASS")

anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                family = gaussian, data = anorexia)
summary(anorex.1)



library(lmerTest)
## linear mixed models - reference values from older code
(fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy))
APA_Table(fm1)# (with its own print method; see class?merMod % ./merMod-class.Rd


## generalized linear mixed model
library(lattice)
windows(8,8)
xyplot(incidence/size ~ period|herd, cbpp, type=c('g','p','l'),
       layout=c(3,5),
    index.cond = function(x,y)max(y)

       )




(gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
              data = cbpp, family = binomial))
## using nAGQ=0 only gets close to the optimum
(gm1a <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
               cbpp, binomial, nAGQ = 0))
## using  nAGQ = 9  provides a better evaluation of the deviance
## Currently the internal calculations use the sum of deviance residuals,
## which is not directly comparable with the nAGQ=0 or nAGQ=1 result.
(gm1a <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
               cbpp, binomial, nAGQ = 9))

APA_Table(gm1)
model_info(gm1)
 #cor.test(~y1+x1, anscombe) %>% tidy %>%fix_format()  %>% Output()
#

head(gm1@frame)

library(effects)

windows(8,8)
plot(allEffects(gm1))
# for(i in Output_info){
#   Text(i )
#   HTML_BR()
#   }

End()

str(Anova(fit1))
## Not run:


## End(Not run)

