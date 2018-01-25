



library(haven)
hyper<-read_sav("C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25/extdata/HYPER.SAV")
hyper<-hyper %>% cleanup_factor()
save(hyper,file="C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25/data/hyper.rda")



kirche<-read_sav("C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25/extdata/KIRCHE.SAV")
save(kirche,file="C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25/data/kirche.rda")

hkarz<-read_sav("C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25/extdata/HKARZ.SAV")
names(hkarz)<- tolower(names(hkarz))
#hkarz$gruppe
hkarz<-hkarz %>% cleanup_factor()
save(hkarz,file="C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25/data/hkarz.rda")


#awards
poisson_sim <-read_sav("C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25/extdata/poisson_sim.sav")

poisson_sim<-poisson_sim %>% cleanup_factor()
save(poisson_sim ,file="C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25/data/poisson_sim.rda")


library(haven)
varana<-read_sav("C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25/extdata/VARANA.SAV")
varana<-varana %>% cleanup_factor()
save(varana,file="C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25/data/varana.rda")





library(stp25)

##
Describe(~alter+kirche+gast, kirche, test=TRUE)
#kirche<- dapply2(kirche, scale)
fit<-summary(lm(kirche~alter+gast, kirche) )
p<- coefficients( fit)[3,4]

ki_al<- residuals(lm(kirche~alter, kirche))
ga_al<- residuals(lm(gast~alter, kirche))
round(c(r=cor(ki_al, ga_al),  df=fit$df[2],  p.value= p),3)


# cor(kirche)
#
# x<-kirche$kirche
#   y<-kirche$gast
#
#   z<- kirche$alter
# cor(x,y)-(cor(x,y)*cor(y,z))/
#   sqrt((1-cor(x,z)^2) * (1-cor(y,z)^2))
