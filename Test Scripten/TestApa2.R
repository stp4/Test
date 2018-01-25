library(stp5)##  R version 3.1.2 (2014-10-31)

Start("html",  language="de",  digits_type ="signif")
set.seed(2)
#HTMLhr()
ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
edu <- cut(c(ctl, trt),3) 
ctl2<- ctl + rnorm(10,0,.5)
trt2 <-trt + rnorm(10,1.2,.5)
group <- gl(2, 10, 40, labels = c("Ctl_gt", "Trt"))
serum <-round(rnorm(length(group)),2) 
time <- factor(rep(1:2, each=20))
DF<- data.frame(id= factor(c(1:20, 1:20)), 
                time, 
                y = c(ctl, trt, ctl2, trt2) + serum +  as.numeric(group), 
                y1 = c(ctl, trt, ctl2, trt2)*10,
                y2 = c(ctl, trt, ctl2, trt2)+2,
                group, 
                edu= factor(c(edu,edu), labels=Cs(low, med, high)),
                serum  )
DF<- rbind(DF,DF,DF)
DF <- upData(DF, labels=c(time="Time", y="Hämoglobin", y1="Triglyzeride", group="Group" ))


ds <- tbl_df(DF)
x1<-ds[,1]
x2<-DF[,1]

#c(mean(x1, na.rm=T), mean2(x2, na.rm=T))
#DF[1]
#View(ds)


### ?tbl_df
DF$id <- as.character(DF$id)
APA2(~ ., DF)
APA2(~ ., ds, caption="dplyr")
APA2(. ~ group, DF)
APA2(y ~ group, DF, fun=mean2, caption="Mittelwert" )
#APA2(y ~ group, DF, fun=smean.sd)

smean.sdl(DF$y)






 

APA2(~ y + y1 + y2, DF)
APA2(y + y1 + y2 ~ group, DF, debug=T)
 



DF[c(3,10,32,33) ,3]<-NA
 formula <- y+y1 ~group + time +edu


(ANS<- Melt2(formula, DF, fun=Mean2) )
(ANS<- Recast2(formula, DF, fun=Mean2) )
as.character(formula[-2] )
levels(ANS[,1])





APA2(y ~group + time, DF )
#APA2(y ~group + time, DF, fun=Mean2, print.n=F)
#APA2(formula, DF, fun=Median2)
Head("Recast")
APA2(y ~group , DF, caption= "wide", fun=Mean2 )
APA2(y ~group , DF, caption= "wide", fun=Mean2, direction="wide")


APA2(y ~group + time, DF, caption= "wide", fun=Mean2, direction="wide")


APA2(y + y1 ~group + time, DF, caption= "long", fun=Mean2 )

APA2(y +y1 ~group + time, DF, caption= "wide", fun=Mean2, direction="wide")
    APA2(formula, DF, fun=Mean2, direction="wide")

 






vars <- c("Discomfort","Vomitus","Reflux","Diarrhea","Obstipation", "Flatulence" ,
          "Irregular bowel movement","Bloating" ,"Good feeling of satiety","No feeling of satiety"
          ,"Dumping syndrome","Difficulties in swallowing", "Pain"  )
names(vars) <- paste0("x", 1:length(vars))
length(vars)
n<-100
set.seed(1)
DF1<- data.frame( x1= rbinom(n, 1, .9994),  x2= rbinom(n, 1, .0045),
                 x3= rbinom(n, 1, .1414),   x4= rbinom(n, 1, .451),   x5= rbinom(n, 1, .41),
                 x6= rbinom(n, 1, .44),  x7= rbinom(n, 1, .04),
                 x8= rbinom(n, 1, .34), x9= rbinom(n, 1, .324),
                 x10= rbinom(n, 1, .24),   x11= rbinom(n, 1, .674),
                 x12= rbinom(n, 1, .524),  x13= rbinom(n, 1, .754),
                 group= gl(2, n/2, labels = c("Control", "Treat")) )

DF1<- upData(DF, labels=vars)
DF1<- dapply2(DF, function(x) {
        if(is.numeric(x)) factor(x,1:0 ,Cs(yes, no))
        else x
})

#describe

x<-Melt2(.~group, DF1, 
         fun=function(x) mean2(ifelse(x=="yes", 1,0))
         )

dcast(x, reorder(variable, value) + group ~ "v" )
Recast2(.~group, DF1, 
       fun= function(x) mean2(ifelse(x=="yes", 1,0)),
       formula=reorder(variable, value) + group ~ .)
        

        
        

#APA2(.~group, DF)
#APA2(.~group, DF, fun=Prozent)


APA2(.~group, DF)

Recast2(edu+time~group, DF, fun=mean2 ) 

APA2(edu+time~group, DF, fun=mean2, direction="wide") 


APA2(.~group, DF ,test=T) 

fun2<- function(x,...){
    
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action", 
                 "offset"), names(mf), 0L)
    
    
}

APA2(.~group, DF ,test=T) 





#windows(5.2,5)
#barchart(reorder2(variable, value, ref = "Discomfort")~value, x, groups=group,
#         origin=0, xlab="Percent (yes)", main="female", auto.key=list(space="right"))
Output_info
End()

