

 library(stp25vers)
 Projekt("html")
 summary(schools)
 Head("Lineare-Regression lm")
 lm1<-lm(score ~ grade + treatment + stdTest, schools)
 #   type = c("default", "broom", "c", "c", "sjPlot","anova")
 APA_Table(lm1, caption="default/broom")

 APA_Table(lm1, type="texreg", caption="texreg")
 APA_Table(lm1, type="stargazer", caption="stargazer")
 APA_Table(lm1, type="sjPlot", caption="sjPlot")
 APA_Table(lm1, type="anova", caption="anova")


 Head("Lineare-Mixed-Effect-Regression")
 Text("
# lme4
lmer(y ~ 1 + (1 | subjects), data=data)

# nlme
lme(y ~ 1, random = ~ 1 | subjects, data=data)

      ")

 Head("nlme::lme", style=3)
 lme1<-nlme::lme(score ~  grade +treatment  + stdTest , schools, random =~ 1|classroom)


 APA_Table(lme1, caption="default/broom")
 APA_Table(lme1, type="texreg", caption="texreg")
 APA_Table(lme1, type="stargazer", caption="stargazer")
 APA_Table(lme1, type="sjPlot", caption="sjPlot")
 APA_Table(lme1, type="anova", caption="anova")



Head("lmerTest::lmer", style=3)
 lmer11<-lmerTest::lmer(score ~  grade +treatment  + stdTest + (1|classroom), schools)

 APA_Table(lmer11, caption="default/broom")
 APA_Table(lmer11, type="texreg", caption="texreg")
 APA_Table(lmer11, type="stargazer", caption="stargazer")
 APA_Table(lmer11, type="sjPlot", caption="sjPlot")
 APA_Table(lmer11, type="anova", caption="anova")

 Head("lme4::lmer", style=3)
 lmer12<-lme4::lmer(score ~  grade +treatment  + stdTest + (1|classroom), schools)
 APA_Table(lmer12, caption="default/broom")
 APA_Table(lmer12, type="texreg", caption="texreg")
 APA_Table(lmer12, type="stargazer", caption="stargazer")
 APA_Table(lmer12, type="sjPlot", caption="sjPlot")
 APA_Table(lmer12, type="anova", caption="anova")

 nms<-  c("lm", "lme", "lmerTest", "lme4")
# APA_Table(lm1, lmer11, lmer12, caption="default/broom")
 APA_Table(lm1, lme1, lmer11, lmer12, type="texreg", caption="texreg", names=nms)
 APA_Table(lm1, lme1,  lmer12, type="stargazer", caption="stargazer", names=nms[-3])
 APA_Table(lm1, lme1, lmer11, lmer12, type="sjPlot", caption="sjPlot", names=nms)
 APA_Table(lm1, lme1, lmer11, lmer12, type="anova", caption="anova", names=nms)

 R2(lm1)
R2(lme1)
R2(lmer11)
R2(lmer12)

# texreg::extract(lm1)
# texreg::extract(lme1)
# texreg::extract(lmer11, include.pseudors = F, include.loglik = TRUE  )
# str(texreg::extract(lmer12))
#car::vif(fit)
#VIF(fit)

Head("Block Lineare-Mixed-Effect-Regression")

sleepstudy<- lme4::sleepstudy
fm1 <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
fm2 <- lmerTest::lmer(Reaction ~ Days + (Days || Subject), sleepstudy)
APA_Table(fm1, fm2)
APA_Table(fm1, fm2, type="tex")


End()
