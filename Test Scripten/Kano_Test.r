library(stp5)
Start("")
head(DF<-GetData("   Geschlecht f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18
                 1           w  1  1  1  2  1  3  1  4  1   5   5   1   4   1   5   2   5   1   
                 2           w  2  1  2  2  2  3  2  4  2   5   2   5   1   3   2   5   2   5   
                 3           m  3  1  3  2  3  3  3  4  3   5   5   1   4   1   5   2   5   1
                 4           m  4  1  4  2  4  3  4  4  4   5   5   1   4   1   5   2   5   1
                 5           w  5  1  5  2  5  3  5  4  5   5   5   1   4   1   5   2   5   1
                 6           w  1 4  NA NA NA NA NA NA NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  
                 7           m  2  5  1  5  2  5  1  5  1   5   2   5   1   5   1   5   2   5
                 8           w  2  4  2  5  1  3  1  3  2   5   3   3   1   3   1   4   1   3
                 9           m  2  4  2  5  2  3  1  3  2   5   1   3   1   3   2   4   3   3
                 10          m  2  5  1  5  1  4  1  5  2   5   1   4   1   5   2   5   1   3
                 11          w  1  5  2  5  1  4  1  4  1   5   1   4   2   5   2   5   1   4
                 12          m  2  5  2  5  2  5  2  4  1   5   1   3   1   4   2   5   3   3
                 13          w  2  5  2  5  3  3  1  5  2   5   1   5   1   5   3   3   3   3
                 14          m  2  5  1  5  1  5  2  5  2  NA   1   5   1   5   2   5   1   5
                 15          w  1  4  2  5  1  3  2  5  1   5   1   3   1   4   2   5   1   3
                 16          w  1  4  2  5  2  5  2  5  1   5   1   4   2   5   2   5   1   3
                 17          w  1  5  2  5  1  5  1  5  2   5   1   4   1   5   2   5   1   4
                 18          w  1  5  2  5  2  5  2  5  1   5   1   2   1   5   1   5   3   2
                 19          m  1  5  2  5  2  5  2  5  2   5   1   4   2   5   2   5   1   3
                 20          w  2  5  2  5  2  5  2  5  2   5   1   3   2   5   2   5   1   3
                 21          w  2  5  2  5  1  5  2  5  2   5   1   5   2   5   2   5   1   3
                 22          m  1  3  2  5  1  3  2  5  2   5   1   3   2   5   1   3   1   3
                 23          w  1  5  2  5  3  3  2  5  2   5   1   4   2   5   2   5   1   4
                 24          w  2  4  2  5  2  3  2  4  2   5   3   3   2   4   2   4   1   3
                 25          m  2  4  1  5  1  4  2  4  1   5   1   3   3   3   2   4   1   3
                 26          w  1  5  1  5  1  3  1  5  1   5   1   3   1   5   1   5   3   3
                 27          w  1  5  2  5  3  3  1  4  2   4   1   3   1   3   3   3   5   1
                 28          w  2  5  2  5  1  4  2  5  1   5   1   3   2   4   2   5   4   1
                 29          w  2  5  2  5  2  4  2  4  2   5   1   4   2   4   1   5   1   4
                 30          m  1  5  2  5  1  3  1  3  1   4   1   3   1   3   1   3   1   3
                 "))
 DF<- upData(DF,  labels=c(f1="1.motiviert"
                           ,f3="2.anpasst"
                           ,f5="3.programm"
                           ,f7="4.info"
                           ,f9="5.text"
                           ,f11="6.reagieren"
                           ,f13="7.text"
                           ,f15="8.Arzt"
                           ,f17="9.Telefonanruf"
 ))



intersect(1:10, 7:20)

DF$f1
  match(DF$f1,1 ) &  match(DF$f2,5) 
  KANO2(DF[,-1], grouping=DF[1] ,rm_I=Inf  )

kano_res <-  KANO2(DF[,-1], grouping=DF[1] ,rm_I=1  )
kano_res$value
str(kano_res)
APA2(value~variable, kano_res, caption="Ueber formula")
Kano_plot( value~variable+Geschlecht, kano_res, main="kano-analysis", ylab="Prozent", type=2, col=2:7, prop.table=T)
Kano_plot( value~variable+Geschlecht, kano_res,   prop.table=T)















 

APA2( KANO2(DF[,-1]  ), caption="Alles dierkt")


APA2( KANO2(  ~ . ,  DF[,-1]  )) 
APA2(KANO2( .~ Geschlecht,  DF  ))


APA2("hallo Test", caption="Alles dierkt")


?APA2.Kano



End()