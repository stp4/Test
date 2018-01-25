get_xy <- function(formula, y=NULL, x=NULL, names_data=names(data)){
  if (length(formula) == 2) {
    y <- strsplit(gsub(" ", "", deparse(formula[[2L]])), split = "\\+")
    y <- if (y == ".") names_data  else unlist(y)
  }else {
    x <- unlist(strsplit(gsub(" ", "", deparse(formula[[3L]])), split = "\\+")) 
    y <- strsplit(gsub(" ", "", deparse(formula[[2L]])), split = "\\+")
    y <- if (y == ".") names_data[ !names_data %in% x ]  else unlist(y) 
  }
  list(x=x,y=y)
}

library(stp4)
Start("",OutDec=".")

Print2_formula<-
  function (o, data, fun = NULL, type="mean", 
            test = FALSE,  
            exclude = NA,  
            sig.star = TRUE, 
            pvalues=FALSE,
            print.n=TRUE,
            corr_test = "pearson",
            Corr2 = function(x, y = NULL, ...) {
              names_x <- names(x)
              x <- apply(x, 2, as.numeric)
              y <- as.matrix(y)
              ans <- rcorr(x, y, ...)
              k <- ncol(ans$r)
              p <- ans$P[-k, k]
              r <- stp4:::ffreta(ans$r[-k, k])
              data.frame(Merkmal = NA, 
                         N = ans$n[-k, k], 
                         Wert1 = ans$r[-k, k], 
                         Wert2 = round(p, 3), 
                         summary = paste0("r=", r), 
                         summary2 = paste0("r=", r, " ", 
                                           ifelse(p < 0.001,"p<.001", paste0("p=", stp4:::ffpvalue(p)))), stringsAsFactors = F)
            }, 
            conTest = function(fml, data) {
              st <- spearman2(fml, data)
              c(testname = if (st[3] == 1) "Wilcoxon" else "Kruskal-Wallis", 
                stat = stp4:::fftest(st[2]), 
                df = paste(st[3:4], collapse = ";"), 
                P = stp4:::ffpvalue(st[5])
              )
            }, 
            catTest = function(fml, data) {
              tab <- xtabs(fml, data)
              stchi <- chisq.test(tab, correct = FALSE)
              c(testname = "Chi-squared", stat = stp4:::fftest(as.numeric(stchi$statistic)), 
                df = stchi$parameter, P = stp4:::ffpvalue(stchi$p.value))
            }, 
            ordTest = function(group, x) {
              require(rms)
              f <- lrm(x ~ group)$stats
              list(P = f["P"], stat = f["Model L.R."], df = f["d.f."], 
                   testname = "Proportional odds likelihood ratio", 
                   statname = "Chi-square", plotmathstat = "chi[df]^2")
            },  
            ...) 
{
    #---------------------------------------------------------------   
    Stat_Mean_Freq <- function(x){
      x_NA <- x
      N <- length(x)
      x <- na.omit(x)
      n <- length(x)
      if (n == 0) {ANS <- data.frame(Merkmal =  "", N ="", statistic= "") }
      else {
        if (is.factor(x)) {
          if (is.null(exclude)) x <- x_NA
          ans <- table(x, exclude = exclude)
          Freq <- as.data.frame(ans)
          Precent <- as.data.frame(round(prop.table(ans) * 100, 3))
          ANS <-    data.frame(Merkmal = names(ans), 
                               N = c(n, rep("", length(ans) -  1)), 
                               statistic=  stp4:::ffprozent(Precent[, 2],Freq[, 2]  )#,
                               # x1 = Freq[, 2], 
                               # x2 = Precent[, 2],
                               # x3 =NA
          )         
        }else{
          x<- as.numeric(x)
          m <- mean(x)
          sd <- ifelse(n > 2, sd(x), NA)
          qant <- quantile(x, na.rm = TRUE)
          if (type == "mean") ANS <-  data.frame(Merkmal = "", 
                                                 N =as.character(n),
                                                 statistic= stp4:::ffmean(m,sd)#,
                                                 # x1 = m, 
                                                 # x2 = sd,
                                                 # x3=NA
          ) 
          else ANS <- data.frame(Merkmal = "", 
                                 N =as.character(n),
                                 statistic= paste(qant[2], qant[3], qant[4], sep = "|")#,
                                 # x1 = qant[3] 
                                 # x2 = qant[2],
                                 # x3=qant[4]
          )   
        }}
      if( print.n) ANS else  ANS[ ,-2]
    }   
    #---------------------------------------------------------------       
    return_data_frame <- function(ans, var_vektor = "") {
      ANS <- NULL
      if (class(ans) == "list") {
        for (var in names(ans)) {
          var_name <- ifelse(is.null(attr(data[, var], "label")), var, attr(data[, var], "label"))
          ans[[var]]$Merkmal <- paste(var_name, ans[[var]]$Merkmal)
          if (is.null(ANS)) {ANS <- ans[[var]]
          }else {ANS <- rbind(ANS, ans[[var]])}
        }
      }
      else {
        ans$Merkmal <- paste(var_vektor, ans$Merkmal)
        ANS <- ans
      }
      ANS
    }
    #---------------------------------------------------------------    
    
    vars <-  get_xy(o)     
    if (is.null(vars$x)){
      mydata <- if (length(vars$y) == 1) data[vars$y] else data[, vars$y]
      if (length(vars$y) == 1){
        ANS <- return_data_frame(
          Stat_Mean_Freq(mydata[, 1]), var_vektor = vars$y)   
      }else{
        ANS <- return_data_frame(lapply(mydata, Stat_Mean_Freq))
        if (test) { # Sig Test angefordert also Correlationsmatrix
          if (any(sapply(mydata, class) == "factor")) {mycorrtable <- rep("Gemischte Skalenniveaus use as.numeric", nrow(ANS))}
          if (nrow(na.omit(mydata)) < 4) {
            mycorrtable <- rep("must have >4 observations", nrow(ANS))
          }else{
            mycor <- Hmisc:::rcorr(as.matrix(mydata), type = corr_test)
            mycorrtable <- round(mycor$r, 4)
            if(pvalues){
              p <- mycor$P
              pval <- apply(p, 2, stp4:::ffpvalue)
              pval <- paste0(" (p=",pval,")")
              mycorrtable <- matrix(paste0(mycorrtable, pval), ncol = ncol(mycorrtable))
            }
            if (sig.star) {
              p <- mycor$P
              pval <- apply(p, 2, stp4:::ffsigstars)
              mycorrtable <- matrix(paste0(mycorrtable, pval), ncol = ncol(mycorrtable))
            }
            diag(mycorrtable) <- "1"
            mycorrtable[lower.tri(mycorrtable)] <- ""
          }
          ANS <- cbind(ANS, mycorrtable)
        }}
      return(ANS)
    }else{
      y<- vars$x
      x<- vars$y 
      variable_list <- c(y, x)
      for (iy in y){
        Y <- data[, iy]
        y_name <- ifelse(is.null(attr(Y, "label")), iy, attr(Y, "label"))
        x_name <- sapply(unlist(x), 
                         function(x) ifelse(is.null(attr(data[, x], "label")), x, attr(data[, x], "label")))
        ANS2 <- list()
        ANS <- NULL
        my_levels <- levels(Y)
        
        if (is.null(my_levels)){
          cat("\n\nAchtung Gruppe ist kein Factor!\n\n")
          if (corr_test %in% c("pearson", "spearman")) {
            ANS <- Corr2(data[, unlist(x)], Y, corr_test)
            ANS$Merkmal <- x_name
            colnames(ANS)[1] <- y_name
            ANS <- if (test) ANS[, c(1, 2, 6)] else ANS[, c(1, 2, 5)]
          }
        }else{
          tabel_header <-   if(print.n)  paste0(names(table(Y)), " (N=", table(Y), ")") else names(table(Y))
          my_levels <- levels(factor(Y))
          for (lev in 1:length(my_levels)) {
            my_subset <- which(Y == my_levels[lev])        
            ans <-  return_data_frame(lapply(data[my_subset,  ][unlist(x)], Stat_Mean_Freq))
            colnames(ans)[  print.n+2    ] <- tabel_header[lev]          
            if (is.null(ANS)) {ANS <- ans}else {ANS <-  if(print.n) cbind(ANS, ans[, -1 ]) else cbind(ANS, ans[2])}
          }
          if (test){
            st <- NULL
            for (x in unlist(x)) {
              if (any(class(data[, x]) == "factor")) {
                cctest <- catTest(formula(paste("~", iy, "+", x)), data)
                cctest <- c(cctest, sig = paste0("X(df=", cctest[3], ")=", cctest[2], "; p=", cctest[4]))
                for (i in 1:(length(levels(data[, x])) - 1)) {
                  cctest <- rbind(cctest, c(testname = "", stat = "", P = "", df = "", sig = ""))
                }
                st <- rbind(st, cctest)
              }else{
                #print(paste(iy, "~", x))
                cctest <- conTest(formula(paste(x, "~", iy)), data)
                cctest <- c(cctest, sig = paste0("W(df=", cctest[3], ")=", cctest[2], "; p=", cctest[4]))
                st <- rbind(st, cctest)
              }
            }# end inner for
            ANS <- cbind(ANS, sig.Test=st[ ,5])
          }}}
      ANS}
  }




n<- 100
set.seed(12)
DF<- data.frame(Group= gl(2, 1, n, labels=c("Control", "Treat"))
                , Edu= gl(3, 3, n, labels = c("A-Level", "B-Level", "C-Level"))
                ,Sex=gl(2,7,n, labels=Cs(male,female)), Age=cut(rnorm(n),5, labels=c( 20,30,40,50,60))
                ,PSQ=round(rnorm(n)*50)
                ,IQ=round(rnorm(n)*100)
)


DF <- upData(DF
             ,PSD=  round(PSQ  + rnorm(n),1)
             ,BMI= as.numeric(as.character(cut( (PSD+IQ/100+PSQ+rnorm(n) ), 10, labels=c(18,19,20,22,23,25,27,29,34,38))))
             ,SOC=round(BMI^2+ rnorm(n) )
             ,labels=c(Sex="Gender"))
#DF$BMI[3:7]<-replace(DF$BMI, DF$BMI<20, NA)
DF$PSD[5:10]<-NA
DF$PSQ[7:20]<-NA
DF$SOC[7:10]<-NA  
DF$IQ[10:30]<-NA 
DF$Group[1:5]<-NA 
DF$Edu[1:2]<-NA 




#APA(~ ., DF )
Print2_formula(  ~BMI  , DF )  
Print2_formula(  ~BMI+Edu  , DF , print.n=F) 


Print2_formula(  ~BMI+PSD+ PSQ  , DF , test=T, sig.star =F) 

Print2_formula( BMI+PSD ~ Edu  , DF ) 

Print2_formula( BMI+PSD ~ Edu  , DF, print.n=F ) 

Print2_formula( BMI+PSD +Sex ~ Edu  , DF, print.n=F, test=TRUE ) 

Print2_formula( BMI+PSD ~  PSQ+SOC   , DF,test=T ) 
