

library("nlme")
library(stp25vers)## R version 3.1.2 (2014-10-31)

Projekt("html")
set.seed(2)

ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
edu <- cut(c(ctl, trt), 3)
ctl2 <- ctl + rnorm(10, 0, .5)
trt2 <- trt + rnorm(10, 1.2, .5)
group <- gl(2, 10, 40, labels = c("Ctl_gt", "Trt"))
serum <- round(rnorm(length(group)), 2)
time <- factor(rep(1:2, each = 20))
DF <- data.frame(
  id = factor(c(1:20, 1:20)),
  time,
  y = c(ctl, trt, ctl2, trt2) + serum + as.numeric(group),
  y1 = c(ctl, trt, ctl2, trt2) * 10,
  y2 = c(ctl, trt, ctl2, trt2) + 2,
  group,
  edu = factor(c(edu, edu), labels = Cs(low, med, high)),
  serum
)
DF <- rbind(DF, DF, DF)
#DF[1:4,1:4]<-NA
DF <-
  upData2(
    DF,
    labels = c(
      time = "Time",
      serum = "H?moglobin",
      y2 = "Cholesterin",
      y1 = "Triglyzeride",
      # y="Transferrin",
      group = "Group"
    )
  )

Recast2(y + y2 + y1 ~ group + edu, DF, Mean2, formula = variable +
               edu ~ group)

APA2( ~ ., DF)


#library(stargazer)
#HTML( stargazer(DF, type = "html"))



library(nlme)
f0 <- lm(y ~ group * serum, data = DF, na.action = na.omit)
#f1 <- lme(y ~ group + serum, data = DF, random= ~1, na.action=na.omit)
fit_lme <-
  lme(
    y ~ group + serum,
    data = DF,
    random = ~ 1 | id,
    na.action = na.omit
  )


APA2(aov(f0))



attr(fit_lme$terms , "dataClasses")


str(fit_lme$model)

APA2(f0)
APA2(fit_lme)
APA2(list(f0, fit_lme), custom.model.names = c("Lm", "Lme + Id"))




aov1 <- aov(y ~ group, DF)
m1 <- lm(aov1)
m2 <- lm(y ~ group + serum, DF)
m3 <- glm(I(y < 6) ~ group + serum, DF, family = binomial)
m4 <- glm(I(round(y)) ~ group + serum, DF, family = poisson)

attr(m1$residuals , "label")
attr(fit_lme$residuals , "label")
attr(m4$terms , "dataClasses")
## gls(follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time), Ovary,
## correlation = corAR1(form = ~ 1 | Mare))

## S3 method for class 'list'

APA2(list(m2, m3, m4),
     custom.model.names = c("(1) lm", "(2) binomial", "(3) poisson"))
APA2(list(m1, m2, m3), pvalues = T)
APA2(list(m1, m2, m3), digits = 3)
APA2(
  list(m2, m3, m4),
  custom.model.names = c("(1) lm", "(2) binomial", "(3) poisson"),
  type = "wide"
) # alles Ausgeben
## plotreg(m1)

## S3 method for class 'lm

APA2(aov1)
APA2(m2)
eff_m2 <- allEffects(lm(y ~ group, DF))
APA2(eff_m2)

#APA2(y ~ group, DF , type = "ci")

## S3 method for class 'efflist'

eff_m11 <- allEffects(lm(y ~ group, DF))
APA2(eff_m11, digits = 2)

names(eff_m11)
windows(6, 6)
plot(eff_m11 , factor.names = F)


eff_m2 <- effect("group:serum", m2, ylevels = list(serum = c(-2, 1)))

APA2(eff_m2)



#End()
windows(6, 6)

plot(eff_m2, multiline = T, key.args = list(border = 0))



## S3 method for class 'lavaan'
if (0) {
  library(lavaan)
  library(semPlot)


  Model <- 'y ~ group + serum'
  fit.Lavaan <- sem(Model, data = DF)
  APA2(fit.Lavaan)

  parameterEstimates(fit.Lavaan)
  Est <-
    parameterEstimates(fit.Lavaan, ci = FALSE, standardized = TRUE)

}
#fitMeasures(fit.Lavaan, c("chisq", "df", "pvalue", "cfi", "rmsea"))
#round( inspect(fit.Lavaan,"r2") ,2)
#parTable(fit.Lavaan)
#show(fit.Lavaan)
#anova(fit.Lavaan)



#semPaths(fit.Lavaan, "std", rotation=2, title = FALSE)
#title("Std", line = 3)



## S3 method for class 'TukeyHSD'
##APA2(TukeyHSD(aov1))
##plot(TukeyHSD(aov1))


## S3 method for class 'lme'

End()
