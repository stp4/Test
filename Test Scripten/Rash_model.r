require(installr, quietly = TRUE) # fuer require2(installr)
library(stp5)
Start("html")
#-- check.for.updates.R()
require2("Hmisc", ask = FALSE)
require2("car", ask = FALSE)
require2("ltm")
require2("eRm")
require2("mokken")
require2("KernSmoothIRT")
require2("mirt")
  data(WIRS)
  
  some(WIRS)
  summary(WIRS)
  rm1 <- RM(WIRS)
  summary( rm1 )
  class(rm1)
 APA2.eRm <-
  function (object, caption ="",  ...) 
  {
    cat("\n")
    cat("Results of", object$model, "estimation: \n")
    cat("\n")
    cat("Call: ", deparse(object$call), "\n")
    cat("\n")
    cat("Conditional log-likelihood:", object$loglik, "\n")
    cat("Number of iterations:", object$iter, "\n")
    cat("Number of parameters:", object$npar, "\n")
    cat("\n")
    X <- object$X
    X01 <- object$X01
    mt_vek <- apply(X, 2, max, na.rm = TRUE)
    ci <- confint(object, "eta")
    capt_1 <-""
    if (object$model %in% c("RM", "RSM", "PCM")) 
      if (is.null(object$call$W)) {
        capt_1 <-"Item (Category) Difficulty Parameters (eta)" 
      }
    else {
      capt_1 <-paste("Item (Category) Parameters (eta):\nBased on design matrix W =", 
          deparse(object$call$W))
    }
    else capt_1 <-"Basic Parameters eta"
    capt_1 <- paste(capt_1, " with 0.95 CI ")
    coeftable <- as.data.frame(cbind(round(object$etapar, 3), 
                                     round(object$se.eta, 3), round(ci, 3)))
    colnames(coeftable) <- c("Estimate", "Std. Error", "lower CI",  "upper CI")
    rownames(coeftable) <- names(object$etapar)
    stp5:::APA2.grouped_df(cbind(Item=names(object$etapar), coeftable),
                           caption=paste(caption, capt_1) )
    ci <- confint(object, "beta")
 #   cat("\nItem Easiness Parameters (beta) with 0.95 CI:\n")
    coeftable <- cbind(round(object$betapar, 3), round(object$se.beta, 
                                                       3), round(ci, 3))
    colnames(coeftable) <- c("Estimate", "Std. Error", "lower CI", "upper CI")
    rownames(coeftable) <- names(object$betapar)
    stp5:::APA2.grouped_df( cbind(Item=names(object$betapar), coeftable), 
caption=paste(caption,"Item Easiness Parameters (beta) with 0.95 CI"))     
  #  cat("\n")
  }
 
 APA2(  rm1, caption="Estimation of Rasch Models")
 
 
 
 
 
 
 
 # Rasch model with beta.1 restricted to 0
 
 some(raschdat1)
 res <- RM(raschdat1, sum0 = FALSE)
 res
 APA2(res)
 res$W                                       #generated design matrix
 
 # Rasch model with sum-0 beta restriction; no standard errors computed
 res <- RM(raschdat1, se = FALSE, sum0 = TRUE)
 res
 APA2(res)
 res$W                                       #generated design matrix
 
 #Rasch model with missing values
 res <- RM(raschdat2)
 res
 APA2(res)
 
 
 plotjointICC(res, main="Item Characteristics (1PL)",ylab="Probability")
 ?plotjointICC
 
 End(F)