## ----setup, include=FALSE------------------------------------------------
#setwd("C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25APA2/vignettes")
knitr::opts_chunk$set(echo = TRUE)
#owd = setwd('vignettes')

## ----setup2, include=FALSE-----------------------------------------------
 #getwd()
library(stp25vers)
Projekt("", "Introduction" )
hkarz$Lai<- factor(hkarz$lai, 0:1, c("negativ", "positiv"))
hkarz<- upData2(hkarz, c(gruppe="Gruppe"
                      ,tzell="T-Zelltypisierung"
                      ,Lai="LAI-Test"))

## ---- include=FALSE------------------------------------------------------
smokers  <- c( 83, 90, 129, 70 )
patients <- c( 86, 93, 136, 82 )

n<-999
g =gl(3, n/3, labels = c("Control", "Treat A", "Treat B"))
g2<- g[sample.int(n)]
levels(g2)<- c("male", "female", "female")
data<- data.frame(g=g, g2=g2,
                  x=rnorm(n) )[sample.int(n)[1:78],]


## ---- results='asis', warning=FALSE--------------------------------------
Prop_Test2(data$g)  
#  library(BayesianFirstAid)
#  x<-as.data.frame(table(data$g))$Freq
#  bayes.prop.test(x, rep(nrow(data), nlevels(data$g)), p=1/3)
# APA2(~g+g2, data, type="freq.ci")
# APA2(g~g2, data, type="freq.ci")

## ---- results='asis', warning=FALSE--------------------------------------
hkarz %>% Tabelle2(tzell="median", Lai, gruppe) 

set_my_options(median=list(style="IQR"))

APA2(tzell[1]+Lai~gruppe, hkarz, 
     type=c("auto", "median"),
     caption="Einfache Auswertung",
     test=TRUE, include.n = TRUE)

## ---- results='asis', warning=FALSE--------------------------------------
 APA2(tzell~gruppe,hkarz, type="cohen.d")  # APA_Effsize ist das gleiche

## ----include=FALSE-------------------------------------------------------
 n<- 2*8
 e<- rnorm(n)*10
 DF<- data.frame(a=rnorm(n) + e,
                    b=rnorm(n), c=rnorm(n), d=rnorm(n) + e,
                    g=gl(2, 8, labels = c("Control", "Treat")))
  

## ---- results='asis', warning=FALSE--------------------------------------
 APA_Correlation(~a+b+c, DF, caption="~a+b+c")

## ----include=FALSE-------------------------------------------------------
 set.seed(1)
n<-100
lvs<-c("--","-","o","+","++")
DF2<- data.frame(
  Magazines=gl(length(lvs),1,n,lvs),
  Comic.books=gl(length(lvs),2,n,lvs),
  Fiction=gl(length(lvs),3,n,lvs),
  Newspapers=gl(length(lvs),5,n,lvs))
DF2$Comic.books[sample.int(n/2)]<- lvs[length(lvs)]
DF2$Newspapers[sample.int(n/2)]<- lvs[1]
DF2$Magazines[sample.int(n/2)]<- lvs[2]

DF2<- transform(DF2, Geschlecht= cut( rnorm(n), 2, Cs(m, f)))
  

## ---- results='asis', warning=FALSE--------------------------------------
Res1 <- Likert(~., DF2 )
APA2(Res1)

## ---- results='asis', warning=FALSE--------------------------------------
APA2(Res2 <- Likert(.~ Geschlecht, DF2 ))
APA2(Res2, ReferenceZero=3, na.exclude=TRUE, type="freq")

  

## ---- results='asis', warning=FALSE--------------------------------------
#require(HH)
# ?likertplot
#
#windows(7,3)
#likertplot( Item   ~ .| Geschlecht , data=Res2$results,
#            main='',ylab="", sub="" ,
#            # col=brewer_pal_likert(5, "RdBu", "Geschlechtay80") ,
#            positive.order=TRUE,
#            as.percent = TRUE,
#            auto.key=list(space="right", columns=1)
#)
#data<- Res2$names
#data$mean<- Res2$m
#  barchart( Item~ mean |Geschlecht, Mymean2, xlim=c(1,5))
#windows(3,6)
#dotplot(Item ~ mean, data,
#        groups=Geschlecht, xlim=c(.085, 5.15),
#        type=c("p", "l"))  

## ----include=FALSE-------------------------------------------------------
DF <-structure(list(
        Geschlecht = structure(c(1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L),
                               .Label = c("Maennlich", "Weiblich"), class = "factor"),
        Alter = structure(c(2L, 4L, 2L, 4L, 2L, 2L, 2L, 3L, 3L, 2L, 1L, 1L, 3L, 4L, 4L, 4L, 2L, 1L, 2L, 1L, 4L, 4L, 3L, 4L, 2L, 2L, 1L, 4L, 4L, 3L, 3L, 3L, 3L, 2L, 3L, 4L, 3L, 3L, 1L, 3L, 1L, 1L, 2L, 1L, 1L, 4L, 3L, 1L, 4L, 2L, 2L, 1L, 3L, 3L, 2L, 3L, 4L, 4L, 1L, 2L, 3L, 2L, 1L, 2L, 1L, 2L, 3L),
                          .Label = c("20 - 29", "30 - 39", "40 - 49", "50 - 59"), class = "factor"),
        Konsum = structure(c(1L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 3L, 1L, 1L, 1L, 2L, 3L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 3L, 1L, 2L, 2L, 3L, 2L),
                                .Label = c("weniger als 3 T.", "3 bis 6 T.", "mehr als 6 T."), class = "factor"),
        Kaffeeform = structure(c(3L, 1L, 3L, 2L, 3L, 3L, 3L, 1L, 3L, 1L, 3L, 3L, 1L, 2L, 3L, 3L, 3L, 3L, 1L, 3L, 3L, 2L, 2L, 3L, 3L, 3L, 3L, 2L, 3L, 1L, 2L, 2L, 2L, 1L, 3L, 3L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 3L, 3L, 3L, 2L, 3L, 3L, 2L, 3L, 3L, 3L, 3L, 3L, 2L, 3L, 3L, 2L, 1L, 3L, 3L, 3L, 2L, 2L),
                               .Label = c("Espresso", "Filterkaffee", "Milchkaffee"), class = "factor"),
        FavA = structure(c(3L, 1L, 2L, 1L, 3L, 3L, 4L, 1L, 2L, 2L, 1L, 1L, 1L, 4L, 3L, 4L, 3L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 3L, 4L, 1L, 1L, 4L, 4L, 1L, 1L, 1L, 2L, 1L, 2L, 4L, 3L, 2L, 4L, 1L, 1L, 2L, 2L, 2L, 4L, 2L, 2L, 2L, 1L, 3L, 2L, 4L, 2L, 4L, 1L, 4L, 4L, 2L, 1L, 1L, 4L, 2L, 1L, 3L, 2L, 3L),
                         .Label = c("Cubanischer Arabica Filter", "Cubanischer Arabica Kaltextrakt", "Dallmayr Prodomo Kaltextrakt", "Dallmayr Prodomo Filter"), class = "factor"),
        FavB = structure(c(4L, 2L, 1L, 3L, 2L, 1L, 3L, 2L, 1L, 1L, 4L, 4L, 2L, 2L, 2L, 2L, 4L, 3L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 4L, 2L, 2L, 3L, 4L, 4L, 1L, 3L, 1L, 2L, 4L, 4L, 1L, 3L, 3L, 1L, 3L, 1L, 1L, 1L, 3L, 1L, 2L, 2L, 1L, 3L, 3L, 3L, 2L, 2L, 3L, 3L, 2L, 4L, 1L, 1L, 2L, 2L, 1L, 2L),
                         .Label = c("Cubanischer Arabica Filter", "Cubanischer Arabica Kaltextrakt", "Dallmayr Prodomo Kaltextrakt", "Dallmayr Prodomo Filter"), class = "factor"),
        FavC = structure(c(2L, 3L, 3L, 4L, 1L, 2L, 1L, 4L, 4L, 3L, 2L, 3L, 3L, 3L, 4L, 3L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 3L, 3L, 3L, 3L, 1L, 4L, 3L, 2L, 3L, 2L, 3L, 1L, 2L, 3L, 2L, 4L, 4L, 4L, 4L, 3L, 2L, 3L, 1L, 3L, 4L, 4L, 3L, 2L, 1L, 1L, 3L, 3L, 2L, 1L, 4L, 2L, 3L, 3L, 4L, 1L, 3L, 1L),
                         .Label = c("Cubanischer Arabica Filter", "Cubanischer Arabica Kaltextrakt", "Dallmayr Prodomo Kaltextrakt", "Dallmayr Prodomo Filter"), class = "factor"),
        FavD = structure(c(1L, 4L, 4L, 2L, 4L, 4L, 2L, 3L, 3L, 4L, 3L, 2L, 4L, 1L, 1L, 1L, 1L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 1L, 1L, 4L, 2L, 1L, 3L, 2L, 2L, 3L, 4L, 4L, 4L, 3L, 1L, 1L, 3L, 2L, 2L, 3L, 1L, 4L, 3L, 4L, 4L, 4L, 3L, 1L, 4L, 1L, 4L, 2L, 4L, 1L, 1L, 4L, 3L, 3L, 2L, 4L, 3L, 4L, 4L, 4L),
                         .Label = c("Cubanischer Arabica Filter", "Cubanischer Arabica Kaltextrakt", "Dallmayr Prodomo Kaltextrakt", "Dallmayr Prodomo Filter"), class = "factor")),
        .Names = c("Geschlecht", "Alter", "Konsum", "Kaffeeform", "FavA", "FavB", "FavC", "FavD"), row.names = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L, 66L, 67L, 68L), class = "data.frame")

 

## ---- results='asis', warning=FALSE--------------------------------------
ans <- Rangreihe(~FavA+FavB+FavC+FavD, DF )
APA2(ans, caption="Alle") 

## ---- results='asis', warning=FALSE--------------------------------------

fit1<- lm(tzell~gruppe, hkarz)
fit2<- lm(tzell~gruppe+Lai, hkarz) 
fit3<- lm(tzell~gruppe*Lai, hkarz)
 
APA_Table(fit1,fit2,fit3, type="long", caption="Regression")

## ---- results='asis', warning=FALSE--------------------------------------
fit1 <- glm(gruppe~tzell, hkarz, family = binomial)
APA_Table(fit1, type="long")

## ---- results='asis', warning=FALSE--------------------------------------
res <- broom::tidy(fit1) 
cbind(res[1:2], 
      confint(fit1),
      res[5]) %>% 
  fix_format %>% Output("Odds Ratios")


## ---- results='asis', warning=FALSE--------------------------------------
#--Omnibus Test
 lmtest::lrtest(fit1) %>% fix_format() %>% Output("LR-Test")
# Fuer F-Test :  lmtest::waldtest(fit1)

## ---- results='asis', warning=FALSE--------------------------------------
Goodness <- function(x) {
  cbind(round(broom::glance(x)[, c(6,  3, 4, 5)], 1),
  round(R2(x), 2),
  round(RMSE(x)[2], 2))
}
fit1 %>% Goodness %>% Output()

## ---- results='asis', warning=FALSE--------------------------------------
Klassifikation2(fit1)

## ---- warning=FALSE------------------------------------------------------
#-- SPSS kodiert die Gruppe 3 als Referenz
poisson_sim$prog <-
  factor(poisson_sim$prog, c("vocation", "general",  "academic"))
  fit5 <- glm(num_awards ~ prog + math, 
              poisson_sim, family =  poisson())

## ---- results='asis', warning=FALSE--------------------------------------
poisson_sim %>% Tabelle2(num_awards, prog, math)

APA_Table(fit5)

## ---- results='asis', warning=FALSE--------------------------------------
APA2(xtabs(~gruppe+lai, hkarz), test=TRUE, type="fischer")
fit1<- glm(gruppe~lai, hkarz, family = binomial)
thkarz <- as.data.frame(xtabs(~gruppe+lai, hkarz))
fit2<- glm(Freq ~ gruppe*lai, thkarz, family = poisson())

APA_Table(fit1, include.odds = TRUE)
APA_Table(fit1, fit2, include.odds = TRUE, 
          include.b=FALSE,
          include.se = FALSE,
          type="long")

## ---- results='asis', warning=FALSE--------------------------------------
lmer_fit1<-lmerTest::lmer(score ~ agegrp+trial + (1|id), MMvideo)

lmer_fit2<-lmerTest::lmer(score ~ agegrp*trial + (1|id), MMvideo)
APA_Table(lmer_fit1, lmer_fit2, type="long")


# MySet()
# library(gridExtra)
#   windows(8,4)
#   p1 <- plot(effect("trial",lmer_fit1), multiline=TRUE, main="")
#   p2 <- plot(effect("agegrp*trial",lmer_fit2), multiline=TRUE, main="")
# 
#   grid.arrange(p1,p2,ncol=2)
# 
#  library(coefplot)
#  windows(4,3)
#   coefplot(lmer_fit2, intercept=F, xlab="b (SE)")
# 
#    windows(4,3)
#   multiplot(lmer_fit1, lmer_fit2, intercept=F, xlab="b (SE)")

## ---- results='asis', warning=FALSE--------------------------------------
library(survival)
mkarz <- GetData("C:/Users/wpete/Dropbox/3_Forschung/1 Statistik/BspDaten/SPSS/_B�hl/MKARZ.SAV")
mkarz$status<- ifelse(mkarz$status=="tot", 1, 0)
mkarz %>% Tabelle2(survive="median", status, lkb)


## ---- results='asis', warning=FALSE--------------------------------------
# Kaplan-Meier estimator without grouping

m0 <- Surv(survive, status) ~ 1
res0<- survfit(m0, mkarz)
APA2(res0)

APA2(summary(res0, times= c(5, 10, 20, 60)),
     percent=TRUE,
     #Statistik Anfordern und ander Schreibweise
     include=c( time ="time", n.risk ="n.risk", 
                n.event ="n.event", surv = "survival",
                lower = "lower 95% CI",upper ="upper 95% CI"),
     caption="Kaplan-Meier" )

## ---- results='asis', warning=FALSE--------------------------------------
m1 <- Surv(survive, status) ~ lkb
res1<- survfit(m1, mkarz)
fit1<- coxph(m1, mkarz)
logrank1<- survdiff(m1, mkarz)
APA2(res1, caption="Kaplan-Meier")
APA2(logrank1)
APA2(coxph(m1,mkarz))

## ---- include=FALSE------------------------------------------------------
m_data<-GetData("C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25data/extdata/manova.sav")
m_data$GROUP<- factor(m_data$GROUP, 1:3, c("website", "nurse ", "video tape" ))

z<- as.matrix(m_data[,-1])


## ---- results='asis', warning=FALSE--------------------------------------
 
fit1<- manova(z ~ m_data$GROUP)
APA2(fit1)
#summary(fit1)$Eigenvalues
#library(MASS)
#fit2 <- MASS::lda(GROUP ~ ., data=m_data)
#APA2(fit2)
#plot(fit2)

## ---- results='asis', warning=FALSE--------------------------------------
# Kopie der \link{car::vif} funktion
library(car)
 fit<-lm(prestige ~ income + education, data=Duncan)
 car::vif(fit)
 VIF2(fit)

## ---- results='asis', warning=FALSE--------------------------------------
Verarbeitung <- Reliability(~ F5+F16+F22+F9+F26+F6+F35+F33+F12+F34+F4, fkv, check.keys =TRUE)
Coping <- Reliability(~ F7+F8+F17+F14+F15+F18+F19+F1+F13+F20, fkv, check.keys =TRUE)
Vertrauen <- Reliability(~ F28+F27+F31+F29, fkv, check.keys =TRUE)
Religion <- Reliability(~F21+F25+F30+F23+F24, fkv, check.keys =TRUE)
Distanz <- Reliability(~F3+F2+F10+F11, fkv, check.keys =TRUE)
Verarbeitung %>% Output

Alpha(Verarbeitung, Coping, Vertrauen, Religion, Distanz) %>% Output()

## ---- include=FALSE------------------------------------------------------
 sf <- GetData("
               J1 J2 J3 J4 J5 J6
               1  1  6  2  3  6
               2  2  7  4  1  2
               3  3  8  6  5 10
               4  4  9  8  2  4
               5  5 10 10  6 12
               6  6 11 12  4  8")

## ---- results='asis', warning=FALSE--------------------------------------
# #Quelle  http://www.personality-project.org/r/book/Chapter7.pdf
ICC2(sf)

## ---- results='asis', warning=FALSE--------------------------------------
  # APA2( ~., fkv, test=TRUE)
  # library(arm)
  # windows(5,5)
  # corrplot(fkv, abs=TRUE, n.col.legend=7)

 fit1<- Principal(fkv, 5, cut=.35)
 names(fit1$Loadings ) <- c("Item", "nr", 
                           "PC_1", "PC_2", "PC_3", "PC_4", "PC_5", "h2"  )
 fit1$Loadings %>% Output()
 
 fit1$Eigenvalue %>% Output()
 fit1$Test %>% Output()

## ---- results='asis', warning=FALSE--------------------------------------
 fit1$KMO %>% Output()
 

## ------------------------------------------------------------------------
x <- c(123, 23, 456,3)
separate_multiple_choice(x,
                          label = c("Allgemein", "Paro", 
                          "Endo", "Prothetik",
                           "Oral chirurgie",
                          "Kieferorthopedie"))



## ---- fig.width=8, fig.height=4------------------------------------------


#Beispiel: Sachs Seite 337
#Scheuerfestigkeit eines Garns
garn  <- c( 550,  760,  830,  890, 1100, 
           1150, 1200, 1350, 1400, 1600, 
           1700, 1750, 1800, 1850, 1850, 
           2200, 2400, 2850, 3200)

Weibull<- function(x){# empirische Verteilungsfunktion
x <- sort(x); n <- length(x); i <- rank(x)
 if(n < 50) F_t <- (i - 0.3) / (n+0.4)  
 else       F_t <- i/(n+1)

 data.frame(data=x, x=log(x),
      y =log(log(1/(1-F_t))))
}

data<-Weibull(garn) 
z <- lm(y ~ x, data)
  res<-round(cbind(shape=coef(z)[2],             
                   scale=exp(-(coef(z)[1]/coef(z)[2]))), 2) 
res

 
m<-(-(coef(z)[1]/coef(z)[2]))
par(mfrow=c(1,2), lwd=2, font.axis=2, bty="n", ps=11,
    bg="white", las=1, cex=1.1)

plot(data$x, data$y, main="Weibull-Diagram",
     xlab="x=log(Garn)", ylab="y=log(log(1/(1-F)))", 
     xlim=c(5.9,8.5), ylim=c(-4, 2), axes = FALSE, pch=16, cex=1.0)
abline(z)
my.at<- signif(c(500, 1000, exp(m), 5000),4)
axis(1, at = log(my.at), labels = my.at)
axis(2)
i <- rank(data$x)
n <- length(data$x)
v.unten <- 1 / (((n-i+1)/i)*(qf(0.025, 2*(n-i+1), 2*i))+1)
v.oben  <- 1 - 1 / (1 + (i / (n-i+1)*qf(0.025, 2*i, 2*(n-i+1))))
lines(data$x, log(log(1/(1-v.oben))), lty=2)
lines(data$x, log(log(1/(1-v.unten))), lty=2)
abline(h=0, lty=3)
abline(v=m, lty=3)
points(m, 0, cex=5, col="red")


library(MASS)
(fit <- fitdistr(garn, densfun="weibull", lower = 0))
 
library(car)
qqPlot(garn, distribution="weibull", main="qqPlot",
       scale=coef(fit)[1], shape=coef(fit)[2], las=1, pch=19)

