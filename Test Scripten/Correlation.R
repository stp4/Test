
panel.cor <- function(x, y, digits, prefix="", cex.cor, method, stars, resize,..., cex_resize=.75)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1) )

   # box(   col ="white")
    test <- cor.test(x, y ,na.action=na.omit, method=method)
    r <- test$estimate

    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    
    txt.cex <- format(c(abs(r), 0.123456789), digits=digits)[1]
    txt.cex <- paste(prefix, txt.cex, sep="")

    if(is.null(cex.cor)) cex <- cex_resize/strwidth(txt.cex)
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))

    if(resize) text(0.5, 0.5, txt, cex = round(cex * abs(r),2))
    else text(0.5, 0.5, txt, cex = cex )
    if(stars) text(.8, .8, Signif, cex=cex/2, col=2)
}



panel.hist <- function(x,...)
{  # print(summary(x))
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )


    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)


    y <- h$counts; y <- y/max(y)

       if(nlevels(factor(x)) < 5 ) {
       print(breaks[-nB])
        print(y)
      }
    box(lty = 1, col = 'white')
    rect(breaks[-nB], 0, breaks[-1], y, col="RoyalBlue", border="lightblue")



}

#-------------------------------------------------------------------------------
#--  Anpassungslieneie mit Itter
#-- 7/1/2011 9:59:41 AM
#-------------------------------------------------------------------------------

panel.lines2 <-function (x, y, col = par("col"), bg = NA, pch = par("pch"),
    cex = 1, col.smooth = "blue", span = 2/3, iter = 3,lines, smooth, ...)
{

    if(nlevels(factor(x)) < 5 ) x <- jitter(x)
    if(nlevels(factor(y)) < 5 ) y <- jitter(y)
     points(x, y, pch = pch, col = col, bg = bg, cex = cex)
     axis(2, labels = FALSE)
     axis(1, labels = FALSE)

    if(lines)  abline(lm(y ~ x, data=na.omit( data.frame(x,y)) ), col = col.smooth)
    if( smooth){
     ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = col.smooth)
    }

}


Corr.Matrix<-function(data,
                      jitter=F,
                      #type="smooth",   #lines
                      smooth=TRUE,
                      lines=TRUE,
                      pch=20,
                      digits=2,
                      cex.cor=NULL,
                      method="pearson"
                      , stars=FALSE
                      , resize=FALSE
                      ,...){    #c("pearson", "kendall", "spearman")
par(pch = pch, bty = 'n' )
data<-  dapply2(data)

print( GetLabelOrName(data))



        pairs(data,
               lower.panel=panel.lines2,
               upper.panel=panel.cor,
               diag.panel=panel.hist,
              # cex.labels=2,

               method=method,
               stars= stars,
               resize=resize ,
               cex.cor=cex.cor,
               digits=digits ,
               smooth=smooth,
               lines=lines,
               ...
               )



cat(method,"\n")
#print(round(cor(na.omit(data), method=method),2))
}

library(stp5)
Start()
require(stats); require(graphics)
#windows(7,7)
#pairs(attitude, main = "attitude data")



windows(7,7)
Corr.Matrix(attitude, digits=3 )
 summary(attitude)

attitude <- transform(attitude, learning2 = cut( learning, 3, c("low", "med", "hig")) )
 fit<- lm(  rating ~complaints+privileges+learning2 +raises,  attitude)
 #data<- dapply2(fit$model , function(x) if(is.numeric(x)) x else jitter(as.numeric(x)))
 Corr.Matrix(fit$model, digits=2, method="pearson", stars=FALSE, resize=FALSE )





