#' Guetekriterien
#' 
#' Berechnet Guetekriterien (Zb r2)
#' \itemize{
#'    \item \code{l} -   Regressions-Modellen   (lm, glm...)
#' }
#' @export
#' @examples
#' library(stp4)
#'  Start("html")
#' #N  <- 100               # generate some data
#' #set.seed(1)
#' #X1 <- round(rnorm(N, 175, 7))
#' #X2 <- round(rnorm(N,  30, 8))
#' # X3 <- round(abs(rnorm(N, 60, 30)))
#' #Y  <- 0.5*X1 - 0.3*X2 - 0.4*X3 + 10 + rnorm(N, 0, 12)
#' # dichotomize Y and do logistic regression
#' #Yfac   <- cut(Y, breaks=c(-Inf, median(Y), Inf), labels=c("0", "1"))
#' #save.csv(data.frame(Yfac, X1, X2, X3), "logisticRegression.csv")
#' bwplot(value ~ factor(Yfac)|variable, melt(DF, id.vars=1))
#' some(DF<- GetData( "logisticRegression.sav"))
#' glmFit <- glm(Yfac ~ X1 + X2 + X3, DF, family="binomial")
#' lrmFit <- lrm(Yfac ~ X1 + X2 + X3, DF)
#' lmFit <- lm(Yfac ~ X1 + X2 + X3, DF )
#' glmFit2 <- glm(Yfac ~ X1 + X2 + X3, DF, family="poisson")
#' APA(list(lmFit, glmFit, lrmFit,glmFit2 ))
#' Guetekriterien(  lmFit )
#' anova(glmFit, test="Chisq")
#' Guetekriterien(glmFit)
#' Guetekriterien(glmFit2)
#' End()


Guetekriterien  <- function(model){  
 ff<-function(x,dig=0){ format(round(x ,dig), nsmall = dig)}
 if( any(class(model)=="glm")){  
     cat( "
          LogLikelihood-Funktion
          Devianz: Abweichung vom Idealfall (-2LL-Wert) [-2LL nahe 0 sig. nahe 1]    
          Likelihood Ratio-Test: Signifikanz des Models [ Chi2 hoch, sig. <0.05]    
          
          R-Quadrat 
          Cox und Snell R??: [ 0.2 = akzeptabel, 0.4 = gut ]
          Nagelkerke R??: [ 0.2 = akzeptabel, 0.4 = gut, 0.5 = sehr gut]
          McFaddens R??: [ 0.2 = akzeptabel, 0.4 = gut ]
          ")  
     n    <- dim(model$model)[1]
     Chi2 <- model$null - model$dev
     Df   <- model$df.null - model$df.res
     p    <- 1-pchisq(Chi2,Df)
     
     RL2  <- Chi2/model$null            # also called McFaddens R??
     Cox  <- 1-exp(-Chi2/n)             # Cox & Snell Index
     Nag  <- Cox/(1-exp(-model$null/n)) # Nagelkerke Index
     
     nulldeviance<- model$null.deviance 
     deviance <- model$deviance
     LL  <- logLik(model) 
     AIC <- AIC(model)
     BIC <- BIC(model)
     
     Model <- c(   Obs=ff(n)#, LL=ff(LL,1)               
                 ,'Likelihood Ratio-Test'= paste0('Chi2(Df=',ff(Df),')=', ff(Chi2,2),', p=', ifelse(p<0.001,"<0.001",ff(p,3)) )       
                 ,'RL2'=ff(RL2,2),'Cox R2'=ff(Cox,2),'Nagelkerke R2'=ff(Nag,2)
                 ,'null deviance (-2LL)' = ff(nulldeviance,1)
                 ,'deviance (-2LL)'=ff(deviance,1) 
                 , AIC=ff(AIC,2), BIC=ff(BIC,2) )
     p   <-anova(glmFit, test="Chisq")#["Pr(>Chi)"]
     odds<- round(cbind(OR=exp(model$coef),exp(confint.default(model))),2)
     odds<-cbind(odds,  ff(p,3) )
     Print(odds) 
     Print( data.frame(Model)) 
   #  list(Stat=odds, Model=Model)   
   
 }else {
  summary_model<- summary(model)
  dfSSE <- summary_model$fstatistic[3]
  P <- summary_model$fstatistic[2]
  Fval <- summary_model$fstatistic[1]            ## F-Wert
  pVal <- 1-pf(Fval, P, dfSSE) 
  n    <- dim(model$model)[1]
  Model<- c( Obs=n
          ,R  = ff(sqrt(summary_model$r.squared),2)
          ,R2 = ff(summary_model$r.squared,2)
          ,adj.R2 =ff(summary_model$adj.r.squared ,2)
          ,ANOVA = paste0("F", "(df=", P ,";",dfSSE,")=", ff(Fval,2),", p"
                          , ifelse( pVal<0.001 ,"<0.001", paste0("=",ff( pVal,3))  )))
  Print(data.frame(Model))
  #  list(Stat=summary_model, Model=Model) 
  }
}







