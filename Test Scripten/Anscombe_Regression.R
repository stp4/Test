
library(stp5)
graphics.off()
set.seed(1)
Start("html",OutDec=".")

# Start("HTML", "572 Example (3.1d)",
#       html_folder = "Example",  fig_folder="Fig1_ex",  
#       , sep_element = ";")

#Anscombe's quartet
#I	II	III	IV
data<- GetData("x.1  y.1 x.2   y.2 x.3   y.3 x.4  y.4
10.0	8.04	10.0	9.14	10.0	7.46	8.0	6.58
8.0	6.95	8.0	8.14	8.0	6.77	8.0	5.76
13.0	7.58	13.0	8.74	13.0	12.74	8.0	7.71
9.0	8.81	9.0	8.77	9.0	7.11	8.0	8.84
11.0	8.33	11.0	9.26	11.0	7.81	8.0	8.47
14.0	9.96	14.0	8.10	14.0	8.84	8.0	7.04
6.0	7.24	6.0	6.13	6.0	6.08	8.0	5.25
4.0	4.26	4.0	3.10	4.0	5.39	19.0	12.50
12.0	10.84	12.0	9.13	12.0	8.15	8.0	5.56
7.0	4.82	7.0	7.26	7.0	6.42	8.0	7.91
5.0	5.68	5.0	4.74	5.0	5.73	8.0	6.89")

data_long<- with( data, {
    data.frame(x=c(x.1,x.2,x.3,x.4), 
     y=c(y.1,y.2,y.3,y.4),
     z=rep(c("a", "b", "c", "d"), each=nrow(data))
           )
     })

APA2(x~z, data_long, test=T)
# Median2(x~z, data_long)
Mean2(x~z, data_long)
fit1<- lm( y.1~x.1, data)
fit2<- lm( y.2~x.2, data)
fit3<- lm( y.3~x.3, data)
fit4<- lm( y.4~x.4, data)

APA2(fit1)
APA2(aov( y.1~x.1, data))
# # +type="describe"
# 




x<-GetData('Daten Variablen   Statistik  "shapiro test" "KS-Test "Normalverteilung"
  "a"   y.1 "7.50 (2.03)" "W=0.98 p=.947"  "Z=0.300 p=1.000" "ja"
   " "   x.1 "9.00 (3.32)" "W=0.97 p=.870"  "Z=0.399 p=.997"  "ja"
   "b"   y.2 "7.50 (2.03)" "W=0.83 p=.022"  "Z=0.300 p=1.000"  "ja"
   " "   x.2 "9.00 (3.32)" "W=0.97 p=.870"  "Z=0.837 p=.486"  "ja"
  "c"    y.3 "7.50 (2.03)" "W=0.83 p=.026"  "Z=0.300 p=1.000"  "ja"
  " "    x.3 "9.00 (3.32)" "W=0.97 p=.870" "Z=0.639 p=.809"  "ja"
  "d"    y.4 "7.50 (2.03)" "W=0.88 p=.091"  "Z=1.750 p=.004"  "nein"
  " "    x.4 "9.00 (3.32)" "W=0.34 p=.000"    "Z=0.544 p=.929"  "ja"  ') 
		
 

Head("Normalverteilung und Regressionsanalyse")
# 
# 
# Text( "
# 
# Die Voraussetzung der Normalverteilung der Variablen bei der Regressionsanalyse ist ein Luxus-Problem das eher auf einem Missverst?ndnis beruht und keine Voraussetzung f?r die Regressionsanalyse darstellt. Zur?ckzuf?hren ist das Missverst?ndnis wahrscheinlich darauf, dass die Residuen normalverteilt sein sollten und, dass die Auswahl des richtigen Regressionsverfahrens von der Verteilungseigenschaften der Zielvariable (Abh?ngige Variable) abh?ngt. 
#       Nach Gellman [2] Seite 45 gelten folgende Voraussetzungen f?r die (lineare) Regressionsanalyse
#       In absteigender Wichtigkeit
#       1.  G?ltigkeit des Models (Validity). 
#       2. Additivit?t und linearit?t. 
#       3.  Unabh?ngigkeit der Fehler (Independence of errors)
#       4.  Gleiche Fehlervarianz (Equal variance of errors)
#       5.  Normalverteilung des Fehlers (Normality of errors)
# Die Pr?fung dieser Voraussetzungen erfolgt am zweckm?ssigsten durch Streudiagramme der Daten und einer Residualanalyse.
# Zur Veranschaulichung habe ich hier die Anscombe-Daten [1] untersucht. Der Datensatz beinhaltet 4 Beispieldaten (a, b, c und d) mit den gleichen statistischen Eigenschaften (Mittelwert, Varianz, Korrelation, usw.) aber mit ganz unterschiedlichen Voraussetzungen.
# In der Tabelle 1 sind die Mittelwerte mit Standardabweichung sowie der Normalverteilungs-Test abgebildet. Bei Anwendung des Statistik-Programm SPSS wir zur Pr?fung auf Normalverteilung oft der KS-Test empfohlen. Im vorliegenden Beispiel zeigt sich das mit Ausnahme des letzten Datensatzes (d) alle Daten ann?hernd Normalverteilt sind also die -Voraussetzung-Normalverteilung- erf?llt ist. In Tabelle 2 sind die Ergebnisse der Regressionsanalyse abgebildet. Es zeigt sich das alle Koeffizienten, sowie die Modelg?te identisch ist.
# Zusammenfassend w?rde man aus den vorliegenden Ergebnisse zu den 4 Datens?tzen schlie?en dass bei allen 4 Beispielen ein statistisch signifikanter linearer Zusammenhang besteht und nur beim Datensatz (d) hinsichtlich der Normalverteilung Unsicherheiten bestehen. 
# Betrachtet man aber zus?tzlich die Streudiagramme der Daten (Abbildung 1) wir  schnell deutlich dass nur bei Datensatz (a) die Voraussetzung der Linearit?t gegeben ist und es nur hier gerechtfertigt ist mittels linearer Regression zu rechnen. Bei den Daten (b) ist ein Kurve (nicht linearer Zusammenhang) gegeben, bei (c) liegt ein Ausreiser vor der das Ergebnis verzerrt und bei (d) ist gar kein Zusammenhang vorhanden sondern das Ergebnis ist nur ein Artefakt eines extremen Ausreissers. Die Residual-Analyse in Abbildung 2 zeigt deutlich dass nur Datensatz (a) die Voraussetzungen erf?llt. Wenn man sich also nur auf die klassischen Parameter wie Signifikanz (p-Wert) und R? und die falschen Voraussetzungen verl?sst kann es leicht passieren dass man zu falschen Interpretationen der Ergebnisse kommt.
#       
#       ")
# 
# 
# Output(x, 
#        caption="Shapiro-Wilk Normality Test und Kolmogorov-Smirnov-Z",
#        note="Bei einem p<0.05 (signifikantes Ergebnis) ist keine Normalverteilung gegeben."
# )	

APA2(list(fit1, fit2, fit3, fit4),
     digits=3,ci.force=T,
     
      #p.value=T,
     names=Cs(a,b,c,d) )
#End()


windows(6,6)
xyplot(y~x|z, data_long, type=c("p", "r"))
SaveData("Streudiagramm")

data_res<- data.frame(a=rstandard(fit1),
                      b=rstandard(fit2),
                      c=rstandard(fit3),
                      d=rstandard(fit4))
data_res<- melt(data_res)
windows(6,6)
# histogram(~value|variable, data_res, 
#           ylab="Density", xlab="studRes", breaks=NULL
#           , scales= list(y=list(relation="free"),
#                          x=list(relation="free") ))
par(mfrow=c(2,2))
plot(fit1, 1, main="a")
plot(fit2, 1, main="b")
plot(fit3, 1, main="c")
plot(fit4, 1, main="d")
SaveData("Residuen", caption="standartisierte Residuen")

Text("
Quellen
[1] Anscombe, Francis J. (1973) Graphs in statistical analysis. American Statistician, 27, 17-21.
[2] Data Analysis Using Regression and Multilevel/Hierarchical Models; Cambridge;2009; Gelman, Hill
[3] Sachs, Lothar; und J?rgen Hedderich; Angewandte Statistik : Methodensammlung mit R; Berlin : Springer Berlin, 2009 Angewandte Statistik: Methodensammlung mit R Seite 109
   


")

require(stats); require(graphics)
summary(anscombe)



library(haven)
##-- now some "magic" to do the 4 regressions in a loop:
write_sav(anscombe, "Anscombe.sav")
End(T)



#A procedure to generate similar data sets with identical statistics and dissimilar graphics has since
