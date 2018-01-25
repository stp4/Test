Diagnostik <- function (fit, type="plot") 
{
    if (type=="plot") {
        windows(8,8)
        print(plot(fit))
        
        
        
    }
    
}
require(datasets);data(swiss);require(car)
require(GGally);require(ggplot2)# install.packages("GGally")

#swiss<-transform(swiss, Education2=cut(Education,3) )
g<- ggpairs(swiss, lower=list(continuous="smooth"), params=c(method="loess"))
g
head(swiss)
lm(income ~ education + prestige + type, data=Duncan)

scatterplotMatrix(~ income + education + prestige | type, data=Duncan)
scatterplotMatrix(~ income + education + prestige, 
                  transform=TRUE, data=Duncan, smoother=loessLine)
scatterplotMatrix(~ income + education + prestige | type, smoother=FALSE, 
                  by.group=TRUE, transform=TRUE, data=Duncan)




