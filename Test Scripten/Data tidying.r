# - 
library(tidyr); library(dplyr); library(reshape2); library(stp5)
Start()
#- functions gather(), separate() and spread(), from tidyr, with the functions melt(), colsplit() and dcast(), from reshape2.

set.seed(10)
messy <- data.frame(id = 1:40,
                    trt = sample(rep(c('control', 'treatment'), each = 20)),
                    work.T1 = round(runif(40),2),
                    home.T1 = round(runif(40),2),
                    work.T2 = round(runif(40),2),
                    home.T2 = round(runif(40),2))
messy
gathered.messy <- gather(messy, key, value, -id, -trt)


head(gathered.messy)
molten.messy <- melt(messy, 
                     variable.name = "key",
                     value.names = "value",
                     id.vars = c("id", "trt"))
head(molten.messy)
## Split a column: separate() vs colsplit()


tidy <- separate(gathered.messy,
                 key, into = c("location", "time"), sep = "\\.") 
head(tidy)
res.tidy <- cbind(molten.messy[1:2], 
                  colsplit(molten.messy[, 3], "\\.", c("location", "time")),
                  molten.messy[4])

head(res.tidy)

tbl_df(messy) %>%  
  gather( key, value, -id, -trt) %>%
  separate(key, into = c("location", "time"), sep = "\\.") 
    
 


#-- From the long to the wide format: spread() vs dcast()
spread.tidy <- spread(tidy, location, value)
head(spread.tidy)

cast.tidy <- dcast(res.tidy, formula = id + trt + time~ location, value.var = "value")
head(cast.tidy)
 

dcast(res.tidy, formula = trt ~ location + time, value.var = "value",
      fun.aggregate =function(x){
        paste0(
             round(mean(x),2)," (",  round(sd(x),2), ")")
      })
#- tips data frame from reshape2

head(tips)
names(tips)[1]<-"total.bill" 
# total_bill  tip    sex smoker day   time size
#      16.99 1.01 Female     No Sun Dinner    2
#      10.34 1.66   Male     No Sun Dinner    3
#      21.01 3.50   Male     No Sun Dinner    3
mean3<-function(x)round(mean(x),1)
sd3<-function(x)round(sd(x),1)
m.tips <- melt(tips)
result_dcast <- dcast(m.tips, day + time ~ variable, mean3)
Recast2(total.bill+tip+size~day+time, tips, mean3, formula= day + time ~ variable)


tbl_df(m.tips) %>% 
  group_by(day, time, variable) %>% 
  summarise (mean = mean3(value)) %>%
  spread(variable, mean)


result<-tbl_df(m.tips) %>% 
  group_by(day, time, variable) %>% 
  summarise (mean = mean3(value), sd= sd3(value)) %>% 
  melt(variable.name = "key")  %>%
  dcast(  day + time ~ variable+key )


graphics.off()
Start("html")
#APA2(result_dcast, type="data.frame")
#APA2(result_tbl_df, type="data.frame")
#Output(result_tbl_df)

#class(result_tbl_df)
result_tbl_df<- NULL

APA2(total.bill+tip+size~day, tips )
APA2(total.bill+tip+size~day+time, tips )
APA2(lm(total.bill ~day+time, tips ))
APA2(list(lm(total.bill ~day+time, tips )))
APA2(xtabs( ~day+time, tips ))
APA2(total.bill+tip+size~day+time, tips, fun=Mean2)
mean2<-function(x) round(mean(x),2)
sd2<-function(x) round(sd(x),2)
Output(
    tbl_df(m.tips) %>% 
    group_by(day, time, variable) %>% 
    summarise (n = length(value), mean=mean2(value), sd=sd2(value)
               ) %>% 
    melt(variable.name = "key")  %>%
    dcast(  day + time ~ variable+key )
    )


APA2(result, type="data.frame", caption="APA2")
Output(result, caption="Output")
Text(
sapply(str_split(Output_info$table, ":"), "[",1))
End()