library(nlme)
library(stp25vers)

Projekt("html")
set.seed(2)

ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
serum <- round(rnorm(length(group)), 2)
DF <-
 data.frame(y = c(ctl, trt) + serum + as.numeric(group) , group, serum)


APA2(t.test(y ~ group, DF))
APA2(wilcox.test(y ~ group, DF))
Tabelle2(y ~ group, DF, test = TRUE, APA=TRUE)
APA2(y ~ group, DF, test = TRUE)
End()
stop()
airquality$Month <-
 factor(airquality$Month, labels = month.abb[5:9])

# x<- pairwise.t.test(airquality$Ozone, airquality$Month )
# APA2(x)

aov1 <- aov(y ~ group, DF)
APA2(aov1) # class: "aov" "lm"

require(car)
m1 <- lm(aov1)
APA2(anova(m1))
APA2(m1)


m2 <- lm(y ~ group + serum, DF)
m3 <- glm(I(y < 6) ~ group + serum, DF, family = binomial)
m4 <- glm(I(round(y)) ~ group + serum, DF, family = poisson)

APA2(m2)
APA2(m3, caption = "bimomial")

summary(m3)
Anova(m3)

APA2(m4)


## gls(follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time), Ovary,
## correlation = corAR1(form = ~ 1 | Mare))

## S3 method for class 'list'
APA2(list(m1, m2, m3, m4))
APA2(list(m2, m3, m4),
 custom.model.names = c("(1) lm", "(2) binomial", "(3) poisson"))
APA2(list(m1, m2, m3), pvalues = TRUE)
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


## S3 method for class 'efflist'
require(effects)
APA2(allEffects(m2), digits = 2)



## S3 method for class 'lavaan'

library(lavaan)
library(semPlot)
require(lmerTest)

Model <- 'y ~ group + serum'
fit.Lavaan <- sem(Model, data = DF)
APA2(fit.Lavaan)
parameterEstimates(fit.Lavaan)
Est <-
 parameterEstimates(fit.Lavaan, ci = FALSE, standardized = TRUE)
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


data(sleepstudy)
head(sleepstudy)
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
fm2 <-
 lmer(Reaction ~ Days + (1 | Subject) + (0 + Days |
 Subject), sleepstudy)


#APA2(list(fm1, fm2), single.row =F, pvalues=T)
APA2(list(fm1, fm2), single.row = F)
APA2(list(fm1, fm2),
 single.row = F ,
 type = "stargazer",
 style = "ajs") # geht nicht
#HTML(htmlreg(list(fm1, fm2)))
sleepstudy$Reaction.1000 <- sleepstudy$Reaction / 1000
fm11 <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
fm12 <-
 lme4::lmer(Reaction ~ Days + (1 |
 Subject) + (0 + Days | Subject), sleepstudy)
fm13 <-
 lme4::lmer(Reaction.1000 ~ Days + (1 |
 Subject) + (0 + Days | Subject), sleepstudy)
APA2(list(fm11, fm12, fm13) , type = "stargazer", digits = 1)

End()
