#--  03.10.2014 16:09:37   16:36
#--03.10.2014 21:02:27
require(HH)
library(stp5)
Start("html",  HtmlCSS="Pastel", language="de")

# #HtmlCSS
#?HTMLChangeCSS
DF <-structure(list(
        Geschlecht = structure(c(1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L), 
                               .Label = c("Männlich", "Weiblich"), class = "factor"), 
        Alter = structure(c(2L, 4L, 2L, 4L, 2L, 2L, 2L, 3L, 3L, 2L, 1L, 1L, 3L, 4L, 4L, 4L, 2L, 1L, 2L, 1L, 4L, 4L, 3L, 4L, 2L, 2L, 1L, 4L, 4L, 3L, 3L, 3L, 3L, 2L, 3L, 4L, 3L, 3L, 1L, 3L, 1L, 1L, 2L, 1L, 1L, 4L, 3L, 1L, 4L, 2L, 2L, 1L, 3L, 3L, 2L, 3L, 4L, 4L, 1L, 2L, 3L, 2L, 1L, 2L, 1L, 2L, 3L), 
                          .Label = c("20 - 29", "30 - 39", "40 - 49", "50 - 59"), class = "factor"), 
        Tägl.Konsum = structure(c(1L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 3L, 1L, 1L, 1L, 2L, 3L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 3L, 1L, 2L, 2L, 3L, 2L), 
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
        .Names = c("Geschlecht", "Alter", "Tägl.Konsum", "Kaffeeform", "FavA", "FavB", "FavC", "FavD"), row.names = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L, 66L, 67L, 68L), class = "data.frame")

head(DF)
APA2(~., DF)
ans <- Rangreihe(~FavA+FavB+FavC+FavD, DF )
APA2(ans, caption="Alle") 

ans <- Rangreihe(~FavA+FavB+FavC+FavD~ Geschlecht + Kaffeeform, DF )
APA2(ans, caption="Geschlecht") 
ans <- Rangreihe(~FavA+FavB+FavC+FavD~ Geschlecht, DF )
APA2(ans, caption="Geschlecht") 
DF[2:3,1] <- NA
ans <- Rangreihe(~FavA+FavB+FavC+FavD~ Geschlecht, DF )
APA2(ans, caption="Geschlecht") 

class(ans)


APA2(ans <- Rangreihe(FavA+FavB+FavC+FavD~Kaffeeform, DF), caption="Kaffeeform")

trellis.par.set(superpose.symbol = list(pch = c(15, 16, 17), cex=1))
windows(6,3)
dotplot( reorder(Items, Skalenwert)~ Skalenwert|"Kaffeeform", ans$result, groups=Kaffeeform , xlab="",
         xlim=range( ans$result$Skalenwert)*1.10 , auto.key=list(), cex=1)
SaveData("Kaffeeform")

DF1<-  data.frame(A=c(1,1,1,2,3,1), B=c(2,2,2,3,2,3), C=c(3,3,3,1,1,NA), D=c(NA,NA,NA,NA,NA,2))
DF2<-   data.frame(R1=factor(Cs(A,A,A,C,C,A)),R2=factor(Cs(B,B,B,A,B,D)),R3= factor(Cs(C,C,C,B,A,B) )) 

DF1<- upData(DF1, labels= c(A="Milchkaffer", B="Filter", C="Esspresso"))
 (Rangreihe(DF1)   )
 (Rangreihe(~R1+R2+R3, DF2)  )






End()
