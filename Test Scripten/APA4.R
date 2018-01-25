library(compareGroups)
library(stp5) 
Start("html")
# load REGICOR data
data(regicor)

# compute a time-to-cardiovascular event variable
regicor$tcv <- with(regicor,Surv(tocv, as.integer(cv=='Yes')))
label(regicor$tcv)<-"Cardiovascular incidence"

# remove variables used to create time-to variables
regicor<-remove.vars(regicor,c("todeath","death","tocv","cv"))

# descriptives by time-to-cardiovascular event, taking 'no' category as 
# the reference in computing HRs.
res <- compareGroups(tcv ~ age + sex + smoker + sbp + histhtn +  chol + txchol + bmi + phyact + pcs  , 
                     regicor, ref.no='no')
 

APA2(createTable(res, hide.no = 'n') , caption="hide.no")
APA2(createTable(res, show.all=TRUE) , caption="show.all")
 

some(regicor)




res <- compareGroups(sex ~ age + smoker + sbp + histhtn +  chol + txchol + bmi + phyact + pcs   , 
                     regicor, ref.no='no', method=2)


APA2(createTable(res, hide.no = 'n') , caption="hide.no")
 
APA2(age  + smoker + sbp + histhtn +  chol + txchol + bmi + phyact + pcs  ~ sex , 
     regicor, test=T , print.n=F , type=c("auto", "median"))
End()

