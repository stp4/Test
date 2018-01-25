

library(stp25vers)
require(graphics)
Projekt("html")

warpbreaks %>% Tabelle2(breaks, by= ~ wool + tension)
summary(fm1 <- aov(breaks ~ wool + tension, data = warpbreaks))
 TukeyHSD(fm1, "tension", ordered = TRUE) %>%
APA_Table(caption="TukeyHSD" )

plot(TukeyHSD(fm1, "tension"))
levels(warpbreaks$tension)

summary(fm2 <- lm(breaks ~ wool+tension, data = warpbreaks))
anova(fm2)
summary(fm1,
        split=list(tension=list( M=1,  H=3, L=2)),
        expand.split=FALSE)

library(multcomp)
glht(fm1,
     linfct=mcp(tension="Tukey"),
     alternative = "less"
     ) %>% APA_Table(caption="multcomp mcp Tukey")
### mix of one- and two-sided alternatives
 contrasts(warpbreaks$tension)

### contrasts for `tension'
K <- rbind("L - M" = c( 1, -1,  0),
           "M - L" = c(-1,  1,  0),
           "L - H" = c( 1,  0, -1),
           "M - H" = c( 0,  1, -1))

warpbreaks.mc <- glht(fm1,
                      linfct = mcp(tension = K),
                      alternative = "less")

### correlation of first two tests is -1
cov2cor(vcov(fm1))

### use smallest of the two one-sided
### p-value as two-sided p-value -> 0.0232
summary(fm1)



summary(fm1 <- aov(breaks ~ wool * tension, data = warpbreaks))
APA_Table(fm1)
TukeyHSD(fm1, "tension", ordered = TRUE) %>%
  APA_Table(caption="TukeyHSD" )


warpbreaks$WW<-interaction(warpbreaks$wool,warpbreaks$tension )
mod2<-aov(breaks~WW, warpbreaks)


APA2(x)
End()
