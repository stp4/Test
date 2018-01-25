

library(stp5)
set.seed(0815)
Start("html")
#library("metafor")  
library(meta)#     install.packages("meta")
DF2 <- GetData("id   Sub   Author Year tn   tm   ts cn      cm     cs          s          d
13   P Holwell.Dawn 1992 10  33.700  11.970  9  20.700  10.71  11.394428  1.1409085
14   P Holwell.Dawn 1992 10  12.600  18.700  9   6.670   6.60  14.339764  0.4135354
30   P  Balzarini.A 2000 29   7.448   3.439 32   9.062   6.47   5.254274 -0.3071785
45   A        Eid.P 1993 20 227.650 100.160 34 314.120 163.55 143.668446 -0.6018719
55   A Gaulthier.JE 1983  8   5.750   4.060  7   2.640   0.63   3.009812  1.0332871
56   P Gaulthier.JE 1983  8   5.750   4.060 13   7.350   4.11   4.091650 -0.3910403
66   P  Thompson.EA 2005 28   3.000   1.200 25   3.400   1.10   1.154021 -0.3466141
               ")



DF2$Author <- gsub("\\.", " ", DF2$Author)
 

#?metacont
cont<- metacont( tn, tm, ts, cn,cm, cs, data=DF2, sm="SMD",
                 studlab=paste(Author, Year)
) 
#windows(10,4)
#forest(cont, comb.fixed=F)    #   , comb.random=T
 

#windows(4,4)
#funnel(cont)



APA2.metacont<- function(x, ...){

  
  summary(x)

}

#library(xtable)
APA2(cont)

#meta:::print.meta
#meta:::print.summary.meta 
 #forest.meta
End()
