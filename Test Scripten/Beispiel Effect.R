# Note: Some of these examples are marked as "don't test"
#       to reduce the execution times of the examples
#       when the package is checked.

library(stp25vers)
graphics.off()
Projekt(" "  )
library(effects)


#-- Modifiziert wegen xlab

plot.efflist<-
  function (x, selection, rows, cols, ask = FALSE, graphics = TRUE, xlab,
            ...)
  {
    if (!missing(selection)) {
      if (is.character(selection))
        selection <- gsub(" ", "", selection)
      return(
        plot(x[[selection]], ... )
      )
    }


    effects <- gsub(":", "*", names(x))


    if (ask) {
      repeat {
        selection <- menu(effects, graphics = graphics, title = "Select Term to Plot")
        if (selection == 0)
          break
        else print(plot(x[[selection]], ... ))
      }
    }
    else {
      neffects <- length(x)
      mfrow <- effects:::mfrow(neffects)
      if (missing(rows) || missing(cols)) {
        rows <- mfrow[1]
        cols <- mfrow[2]
      }
      for (i in 1:rows) {
        for (j in 1:cols) {
          if ((i - 1) * cols + j > neffects)
            break
          more <- !((i - 1) * cols + j == neffects)

          # print(paste("i=",i, " j=", j, (i - 1) * cols + j))

          if (missing(xlab))
            print(plot(x[[(i - 1) * cols + j]],
                       row = i,
                       col = j,
                       nrow = rows,
                       ncol = cols,
                       more = more,
                       ... ))
          else {
            k<-(i - 1) * cols + j
            print(names(x[k] ))

             x_lab <- if ( is.null(names(xlab))) xlab[k] else xlab[names(x[k])]
              print(x_lab)
            print(plot(x[[k]],
                          row = i,
                          col = j,
                          nrow = rows,
                          ncol = cols,
                          more = more,
                          xlab = x_lab,
                          ... ))}


        }
      }
    }
  }






data(Cowles)


mod.cowles <- glm(volunteer ~ sex + neuroticism*extraversion,
                  data=Cowles, family=binomial)

mod.cowles
eff.cowles <- allEffects(mod.cowles, xlevels=list(extraversion=seq(0, 24, 6)),
                         given.values=c(sexmale=0.5))




data(Cowles)

head(Cowles)
APA2(  sex + neuroticism+extraversion~ volunteer, data=Cowles)



mod.cowles2 <- glm(volunteer ~ sex*neuroticism*extraversion,
                   data=Cowles, family=binomial)


 Text('


plot(allEffects(mod.cowles), main="")

plot(allEffects(mod.cowles), main="",  x.var="extraversion")

plot(allEffects(mod.cowles2), x.var="sex",  multiline=T )

      ')

windows(8,4)
plot(allEffects(mod.cowles), main="")
SaveData("default", caption="allEffects(mod.cowles)")


windows(8,4)
plot(allEffects(mod.cowles), main="",
         xlab=c(  sex= "Gender",
                neuroticism = "Neurozenstismus", extraversion="Extraversion"
                ))
SaveData("default", caption="allEffects(mod.cowles)")
End()
Ens()



windows(5,4)
plot(allEffects(mod.cowles), main="",  x.var="extraversion")

windows(5,4)
plot(allEffects(mod.cowles2), x.var="sex",  multiline=T )



Text("
allEffects(mod.cowles)
     ")


Text("
allEffects(mod.cowles,
        xlevels=list(extraversion=seq(0, 24, 6)),
        given.values=c(sexmale=0.5))

     ")




# the following are equivalent:
#eff.ne <- effect("neuroticism*extraversion", mod.cowles)
#Eff.ne <- Effect(c("neuroticism", "extraversion"), mod.cowles)
#all.equal(eff.ne$fit, Eff.ne$fit)

windows(8,4)
plot(eff.cowles, main="")
SaveData("default", caption="plot(eff.cowles)  ")

windows(4,4)
plot(eff.cowles, 'sex', ylab="Prob(Volunteer)", main="")
SaveData(caption="plot(eff.cowles, 'sex', ylab= ...")


windows(4,4)
plot(eff.cowles, 'neuroticism:extraversion', ylab="Prob(Volunteer)", main="",
     ticks=list(at=c(.1,.25,.5,.75,.9)))
SaveData(caption="plot(eff.cowles, 'neuroticism:extraversion',
     ticks=list(at=c(.1,.25,.5,.75,.9)), ... ")
windows(4,4)
plot(eff.cowles, 'neuroticism:extraversion', multiline=TRUE, main="",
     ylab="Prob(Volunteer)")
SaveData(caption="plot(eff.cowles, 'neuroticism:extraversion', multiline=TRUE, ...")
windows(4,4)
plot(effect('sex:neuroticism:extraversion', mod.cowles, main="",
            xlevels=list(extraversion=seq(0, 24, 6))), multiline=TRUE)
SaveData(caption="plot(effect('sex:neuroticism:extraversion', mod.cowles,
            xlevels=list(extraversion=seq(0, 24, 6))), multiline=TRUE")

# a nested model:
APA2(~log(prestige)+income+type + education, data=Prestige)
mod <- lm(log(prestige) ~ income:type + education, data=Prestige)

# does not work: effect("income:type", mod, transformation=list(link=log, inverse=exp))
Text(" lm(log(prestige) ~ income:type + education, data=Prestige)

plot(Effect(.... , mod,
  transformation=list(link=log, inverse=exp)

     " )
windows(7,6)
plot(Effect(c("income", "type"), mod,
            transformation=list(link=log, inverse=exp), main="",
            ylab="prestige")) # works
SaveData(caption='plot(Effect(c("income", "type"), mod,
            transformation=list(link=log, inverse=exp),...')

if (require(nnet)){

    Text("
 require(nnet)
    multinom(vote ~ age + gender + economic.cond.national +
                             economic.cond.household + Blair + Hague + Kennedy +
                             Europe*political.knowledge, data=BEPS)
         ")
    head(BEPS)


    Text('
     mod.beps <- multinom(vote ~ age + gender + economic.cond.national +
                             economic.cond.household + Blair + Hague + Kennedy +
                             Europe*political.knowledge, data=BEPS)

    plot(effect("Europe*political.knowledge", mod.beps, main="",
                xlevels=list(political.knowledge=0:3)))


    plot(Effect(c("Europe", "political.knowledge"), mod.beps,  main="",
                xlevels=list(Europe=1:11, political.knowledge=0:3),
                given.values=c(gendermale=0.5)),
         style="stacked", colors=c("blue", "red", "orange"), rug=FALSE)



    plot(effect("Europe*political.knowledge", mod.beps, # equivalent
                xlevels=list(political.knowledge=0:3),
                given.values=c(gendermale=0.5)),
         style="stacked", colors=c("blue", "red", "orange"), rug=FALSE)



         ')
    mod.beps <- multinom(vote ~ age + gender + economic.cond.national +
                             economic.cond.household + Blair + Hague + Kennedy +
                             Europe*political.knowledge, data=BEPS)
    windows(8,8)
   print( plot(effect("Europe*political.knowledge", mod.beps, main="",
                xlevels=list(political.knowledge=0:3)))
    )
    SaveData()

    windows(4,3)
    print(   plot(Effect(c("Europe", "political.knowledge"), mod.beps,  main="",
                xlevels=list(Europe=1:11, political.knowledge=0:3),
                given.values=c(gendermale=0.5)),
         style="stacked", colors=c("blue", "red", "orange"), rug=FALSE))
    SaveData()
    windows(4,3)
   print(plot(effect("Europe*political.knowledge", mod.beps, # equivalent
                xlevels=list(political.knowledge=0:3),
                given.values=c(gendermale=0.5)),
         style="stacked", colors=c("blue", "red", "orange"), rug=FALSE))
    SaveData()
}

 require(MASS)
    mod.wvs <- polr(poverty ~ gender + religion + degree + country*poly(age,3),
                    data=WVS)


    some(WVS)
 Text('
 mod.wvs <- polr(poverty ~ gender + religion + degree + country*poly(age,3),
                    data=WVS)
         plot(effect("country*poly(age, 3)", mod.wvs))
     plot(effect("country*poly(age, 3)", latent=TRUE, mod.wvs))')


     windows(8,6)
    plot(effect("country*poly(age, 3)", mod.wvs))
    SaveData()
    windows(8,5)
     plot(effect("country*poly(age, 3)", latent=TRUE, mod.wvs))
     SaveData()
   # plot(Effect(c("country", "age"), mod.wvs), style="stacked")
     windows(8,5)

    plot(effect("country*poly(age, 3)", mod.wvs), style="stacked") # equivalent
    SaveData()





mod.pres <- lm(prestige ~ log(income, 10) + poly(education, 3) + poly(women, 2),
               data=Prestige)
eff.pres <- allEffects(mod.pres, xlevels=50)
plot(eff.pres)
plot(eff.pres[1],
     transform.x=list(income=list(trans=log10, inverse=function(x) 10^x)),
     ticks.x=list(income=list(at=c(1000, 2000, 5000, 10000, 20000))))

# linear model with log-response and log-predictor
# to illustrate transforming axes and setting tick labels
mod.pres1 <- lm(log(prestige) ~ log(income) + poly(education, 3) + poly(women, 2),
                data=Prestige)
# effect of the log-predictor
eff.log <- Effect("income", mod.pres1)
# effect of the log-predictor transformed to the arithmetic scale
eff.trans <- Effect("income", mod.pres1, transformation=list(link=log, inverse=exp))
#variations:
# y-axis:  scale is log, tick labels are log
# x-axis:  scale is arithmetic, tick labels are arithmetic
plot(eff.log)

# y-axis:  scale is log, tick labels are log
# x-axis:  scale is log, tick labels are arithmetic
plot(eff.log, transform.x=list(income=c(trans=log, inverse=exp)),
     ticks.x=list(income=list(at=c(1000, 2000, 5000, 10000, 20000))),
     xlab="income, log-scale")

# y-axis:  scale is log, tick labels are arithmetic
# x-axis:  scale is arithmetic, tick labels are arithmetic
plot(eff.trans, ylab="prestige")

# y-axis:  scale is arithmetic, tick labels are arithmetic
# x-axis:  scale is arithmetic, tick labels are arithmetic
plot(eff.trans, type="response", ylab="prestige")

# y-axis:  scale is log, tick labels are arithmetic
# x-axis:  scale is log, tick labels are arithmetic
plot(eff.trans, transform.x=list(income=c(trans=log, inverse=exp)),
     ticks.x=list(income=list(at=c(1000, 2000, 5000, 10000, 20000))),
     xlab="income, log-scale", ylab="prestige, log-scale",
     main="Both effect and X in log-scale")

# y-axis:  scale is arithmetic, tick labels are airthmetic
# x-axis:  scale is log, tick labels are arithmetic
plot(eff.trans, transform.x=list(income=c(trans=log, inverse=exp)),
     ticks.x=list(income=list(at=c(1000, 2000, 5000, 10000, 20000))),
     type="link",
     xlab="income, log-scale", ylab="prestige")








# mlm example
if (require(heplots)) {
    data(NLSY, package="heplots")

    head(NLSY)

    mod <- lm(cbind(read, math) ~ income+educ, data=NLSY)
    Text(" lm(cbind(read, math) ~ income+educ, data=NLSY) ")

    eff.inc <- Effect("income", mod)
      windows(5,3.2)
      print(plot(eff.inc))
    SaveData()
     # eff.edu <- Effect("educ", mod)
    Text('plot(Effect("educ", mod, response="read"))') # plot(eff.edu, rug=FALSE, grid=TRUE)

    windows(4,3.4)
   print(plot(Effect("educ", mod, response="read")), main="") #plot(Effect("educ", mod, response="read"))
    SaveData()
    detach(package:heplots)
}

# component + residual plot examples


Prestige$type <- factor(Prestige$type, levels=c("bc", "wc", "prof"))

mod.prestige.1 <- lm(prestige ~ income + education, data=Prestige)
plot(allEffects(mod.prestige.1, partial.residuals=TRUE
                )
     ) # standard C+R plots

mod.prestige.2 <- lm(prestige ~ type*(income + education), data=Prestige)
plot(allEffects(mod.prestige.2, partial.residuals=TRUE))

Text('mod.prestige.3 <- lm(prestige ~ type + income*education, data=Prestige)
plot(Effect(c("income", "education"), mod.prestige.3, partial.residuals=TRUE),
     span=1)
')

windows(6,4)
mod.prestige.3 <- lm(prestige ~ type + income*education, data=Prestige)
plot(Effect(c("income", "education"), mod.prestige.3, partial.residuals=TRUE),main="",
     span=1)
SaveData()
#  artificial data

set.seed(12345)
x1 <- runif(500, -75, 100)
x2 <- runif(500, -75, 100)
y <- 10 + 5*x1 + 5*x2 + x1^2 + x2^2 + x1*x2 + rnorm(500, 0, 1e3)
Data <- data.frame(y, x1, x2)
mod.1 <- lm(y ~ poly(x1, x2, degree=2, raw=TRUE), data=Data)
# raw=TRUE necessary for safe prediction
mod.2 <- lm(y ~ x1*x2, data=Data)
mod.3 <- lm(y ~ x1 + x2, data=Data)
APA2(list(mod.1, mod.2, mod.3))
.save.strip <- setStrip() # change color of lattice strips

Text( '
 plot(Effect(c("x1", "x2"), mod.1, partial.residuals=TRUE)) # correct model
')


windows(6,4)
plot(Effect(c("x1", "x2"), mod.1, partial.residuals=TRUE),main="") # correct model
SaveData()
Text( '
 plot(Effect(c("x1", "x2"), mod.2, partial.residuals=TRUE)) # wrong model
      ')
windows(6,4)
plot(Effect(c("x1", "x2"), mod.2, partial.residuals=TRUE),main="") # wrong model
SaveData()

Text( '
plot(Effect(c("x1", "x2"), mod.3, partial.residuals=TRUE)) # wrong model

')
windows(6,4)
plot(Effect(c("x1", "x2"), mod.3, partial.residuals=TRUE),main="") # wrong model
SaveData()





restoreStrip(.save.strip)
remove(.save.strip)

End()
