library(stp5)
#library(R2HTML)
library(lmerTest)
Start("html" ) 
 
set.seed(1)
data <- data.frame(
    x = 1:30,
    y = gl(3, 10, labels = c("sehr gut", "mittel", "schlecht")),
    z = gl(2, 15, labels = c("Control", "Treat"))[sample.int(30)]
)
  APA2(x ~ y, data )

n<-100
df<- data.frame(
    group=gl(2, n/2, labels = c("Control", "Treat")),
    rnorm = rnorm(n,50,23),
    rbinom = rbinom(n,1,prob=0.72),
    rpois = rpois(n, 2) ,
    runif = runif(n,40,100) ,
    rbeta = rbeta(n,2,2) 
  
)
trellis.par.get()
APA2(~rnorm, df)
# opar <- trellis.par.get()
# 
# col <- if(is.numeric(col)) col[1] else 4 #  Altlast weil vorher nur 4 Farben eingestellt waren
# trellis.par.set(ggplot2like(n = col, h.start = h))
# trellis.par.set(axis.text = list(cex = 0.8, lineheight = 0.9, col = "grey20"))
# #-- Boxplots
# trellis.par.set(box.dot = list(pch=19, cex=1.2), plot.symbol=list(pch=1))
# #oopt <- lattice.options(ggplot2like.opts())
# 
# set_default_params(list(opar=opar, 
#                         oopt=lattice.options(ggplot2like.opts()) ))
# 
# 


#MySet(brewer.pal(9,"Set1")[c(3,5)], col.bar=3 )
# windows(8,8)
# show.settings()
#
trellis.par.set()
#trellis.device()
 windows(7,4)
 bwplot(rnorm ~group, df )
 SaveData()
Text("Hallo")
End()
