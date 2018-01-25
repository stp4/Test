
library(tibble)


require(graphics)

had.splines <- "package:splines" %in% search()
if(!had.splines) rs <- require(splines)
x <- 1:100
z <- factor(rep(LETTERS[1:4], 25))
y <- rnorm(100, sin(x/10)+as.numeric(z))
model <- glm(y ~ ns(x, 6) + z)
windows(8,8)
par(mfrow = c(2,2)) ## 2 x 2 plots for same model :
termplot(model, main = paste("termplot( ", deparse(model$call)," ...)"))
termplot(model, rug = TRUE)
termplot(model, partial.resid = TRUE, se = TRUE, main = TRUE)
termplot(model, partial.resid = TRUE, smooth = panel.smooth, span.smth = 1/4)



dat<- data_frame(x=rnorm(20), y= x+.1+rnorm(20,.2,.3), 
                 x1=x+.1+rnorm(20,.2,.3),
                 x2=x+.1+rnorm(20,.2,.3),
                 x3=x+.1+rnorm(20,.2,.3)
                 )
reg <-lm(y ~ x, data = dat)
plot(y ~ x, data = dat)
# simplest way to add a straight line
abline(reg)
#If you have more predictors, then you need to think on what you intend to plot.
reg <- lm(y ~ x1 + x2 + x3, data = dat)
termplot(reg, se = TRUE, resid = TRUE)
#Will cycle through the predictors. There's another nice looking approach I suggest in the rockchalk package
library(rockchalk)
plotSlopes(reg, plotx = "x2", interval = "confidence")
#The plotx argument has to be a numeric predictor. That can also draw interaction plots for either numeric or factor moderator variables.
library(rockchalk)
plotSlopes(reg, plotx = "x2", modx = "x3", interval = "confidence")






## Manufacture some predictors
set.seed(12345)

dat <- genCorrelatedData2 (N = 100, means = rep(0,4), sds = 1, rho = 0.2,
                           beta = c(0.3, 0.5, -0.45, 0.5, -0.1, 0, 0.6),
                           stde = 2)

dat$xcat1 <- gl(2, 50, labels = c("M", "F"))
dat$xcat2 <- cut(rnorm(100), breaks = c(-Inf, 0, 0.4, 0.9, 1, Inf),
                 labels = c("R", "M", "D", "P", "G"))
## incorporate effect of categorical predictors
dat$y <- dat$y + 1.9 * dat$x1 * contrasts(dat$xcat1)[dat$xcat1] +
    contrasts(dat$xcat2)[dat$xcat2 , ] %*% c(0.1, -0.1, 0, 0.2)

m1 <- lm(y ~ x1 * x2 + x3 + x4 + xcat1 + xcat2, data = dat)
summary(m1)

## New in rockchalk 1.7.x. No modx required:
plotSlopes(m1, plotx = "x1")

## Confidence interval, anybody?
plotSlopes(m1, plotx = "x1", interval = "conf")

## Prediction interval.
plotSlopes(m1, plotx = "x1", interval = "pred")

## Now experiment with a moderator variable
## let default quantile algorithm do its job
plotSlopes(m1, plotx = "x1", modx = "x2")
## previous uses default equivalent to
## plotSlopes(m1, plotx = "x1", modx = "x2", modxVals = "quantile")
## Want more focal values?
plotSlopes(m1, plotx = "x1", modx = "x2", n = 5)
## Pick focal values yourself?
plotSlopes(m1, plotx = "x1", modx = "x2", modxVals = c(-2, 0, 0.5))
## Alternative algorithm?
plotSlopes(m1, plotx = "x1", modx = "x2", modxVals = "std.dev.",
           main = "Uses \"std.dev.\" Divider for the Moderator",
           xlab = "My Predictor", ylab = "Write Anything You Want for ylab")

## Will catch output object from this one
m1ps <- plotSlopes(m1, plotx = "x1", modx = "x2", modxVals = "std.dev.", n = 5,
                   main = "Setting n = 5 Selects More Focal Values for Plotting")

m1ts <- testSlopes(m1ps)

plot(m1ts)

### Examples with categorical Moderator variable

m3 <- lm (y ~ x1 + xcat1, data = dat)
summary(m3)
plotSlopes(m3, modx = "xcat1", plotx = "x1")
plotSlopes(m3, modx = "xcat1", plotx = "x1", interval = "predict")


m4 <- lm (y ~ x1 * xcat1, data = dat)
summary(m4)
plotSlopes(m4, modx = "xcat1", plotx = "x1")
plotSlopes(m4, modx = "xcat1", plotx = "x1", interval = "conf")


m5 <- lm (y ~ x1 + x2 + x1 * xcat2, data = dat)
summary(m5)
plotSlopes(m5, modx = "xcat2", plotx = "x1")
m5ps <- plotSlopes(m5, modx = "xcat2", plotx = "x1", interval = "conf")

testSlopes(m5ps)



## Now examples with real data. How about Chilean voters?
library(car)
m6 <- lm(statusquo ~ income * sex, data = Chile)
summary(m6)
plotSlopes(m6, modx = "sex", plotx = "income")
plotSlopes(m6, modx = "sex", plotx = "income", col = c("yellow", "blue"))


m7 <- lm(statusquo ~ region * income, data= Chile)
summary(m7)
plotSlopes(m7, plotx = "income", modx = "region")

plotSlopes(m7, plotx = "income", modx = "region", plotPoints = FALSE)
plotSlopes(m7, plotx = "income", modx = "region", plotPoints = FALSE,
           interval = "conf")
plotSlopes(m7, plotx = "income", modx = "region", modxVals = c("SA","S", "C"),
           plotPoints = FALSE, interval = "conf")
## Same, choosing 3 most frequent values
plotSlopes(m7, plotx = "income", modx = "region", n = 3, plotPoints = FALSE,
           interval = "conf")


m8 <- lm(statusquo ~ region * income + sex + age, data= Chile)
summary(m8)
plotSlopes(m8, modx = "region", plotx = "income")


m9 <- lm(statusquo ~ income * age + education + sex + age, data = Chile)
summary(m9)
plotSlopes(m9, modx = "income", plotx = "age")

plotSlopes(m9, modx = "income", plotx = "age", plotPoints = FALSE)

windows(8,8)
## Convert education to numeric, for fun
Chile$educationn <- as.numeric(Chile$education)
m10 <- lm(statusquo ~ income * educationn + sex + age, data = Chile)
summary(m10)
plotSlopes(m10, plotx = "educationn",  modx = "income")

## Now, the occupational prestige data. Please note careful attention
## to consistency of colors selected
data(Prestige)
m11 <- lm(prestige ~ education * type, data = Prestige)

plotSlopes(m11, plotx = "education", modx = "type", interval = "conf")
dev.new()
plotSlopes(m11, plotx = "education", modx = "type",
           modxVals = c("prof"), interval = "conf")
dev.new()
plotSlopes(m11, plotx = "education", modx = "type",
           modxVals = c("bc"), interval = "conf")
dev.new()
plotSlopes(m11, plotx = "education", modx = "type",
           modxVals = c("bc", "wc"), interval = "conf")

windows(8,8)
data(mtcars)
m_cyl <- lm(mpg ~ wt * cyl, data = mtcars)
library(interplot)

# Plot interactions with a continous conditioning variable
interplot(m = m_cyl, var1 = 'cyl', var2 = 'wt') +
    xlab('Automobile Weight (thousands lbs)') +
    ylab('Estimated Coefficient for Number of Cylinders') +
    ggtitle('Estimated Coefficient of Engine Cylinders\non Mileage by Automobile Weight') +
    theme(plot.title = element_text(face='bold'))

# 
# library(stp5)
# Start("html")
# 
# APA2(list(m11))
# 
# #stp5:::Output.texregT
# ?outreg2HTML(m11)
# 
# End()