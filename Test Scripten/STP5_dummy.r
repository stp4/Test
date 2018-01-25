#-- stp5_dummy
#-- stp5 ist eine sammlung von Skripten die die Ergebnisse als Tabellen ausgibt sowie einstellungen 
#-- wie zB contrasts 
loaded <- paste("package", "stp6", sep = ":") %in% search()
if(loaded){

Start <<- function (myformat = "", 
                    Projektname = "Demo", 
                    datum = date(), 
                    contrasts =  c("contr.Treatment", "contr.poly"), ## SPSS"contr.Sum"  ,
                    ...) 
{
oldc<<-getOption("contrasts")
        options(contrasts = contrasts)
cat("Kontraste von " ,paste(oldc, collapse=", "), "auf ",  
                         paste(contrasts, collapse=", "), " umgestellt!")

    
    
    
   
    require(installr, quietly = TRUE) # fuer require2(installr)
    
    require2(Hmisc)
    #require2(caret)
    require2(diagram)
    require2(vcd)
    require2(car)
    require2(effects)
    require2(stringr)
    require2(lattice)
    require2(latticeExtra)
    require2(dplyr)
    require2(reshape2)
    require2(haven)
    require2(lmerTest)
    require2(texreg)
}

End<- function(...){ date() }


APA2.default <<- function(x, ...) {
    warning(paste("No method for APA2 an S3 object of class",
                  class(x), ", using broom"))
    require2(broom)
    #install_github("broom", "dgrtwo") 
    # install.packages("broom")
    print(tidy(x))
    print(APA2.grouped_df(glance(x))
}


APA2.formula<- function(...) Hmisc::summary(...)

APA2.list<- function(x, ...){
    for(i in x){
        print(summary(i))
        #print(anova(i))
        #print(car::Anova(i))
        
    }
    
}
    

APA2.lme <- function(fit,   
                     caption = "" , note = "", digits = 2, digits.beta=2, type="III", # exclude = NA,
                     return.string = TRUE, single.row = TRUE, caption.above = TRUE,  inline.css = TRUE, inline.border = FALSE, 
                     doctype = FALSE, html.tag = FALSE, head.tag = FALSE, body.tag = FALSE,  
                   #  center = options()$stp4$apa.style$center,   
                     stars = c(0.001, 0.01, 0.05), stars.symbols = c("***", "**", "*", "B7")[1:length(stars)], 
                     sig.stars = FALSE, pvalues=TRUE, 
                     eta=FALSE, #nicht optional
                     anova=TRUE,
                     anova_type="F",   #  F-werte (wie SPSS) oder Chi (car::Anova)
                     ...) {
  #  APA_regression <<- TRUE
    fit_sum <- summary(fit)
    xtTab <- as.data.frame(fit_sum$tTable)
 
     if(anova_type=="F"){
        Anov <- anova(fit)
    
        Anov <- as.data.frame(Anov)
        goodnes <- cbind( Obs = fit_sum$dims[["N"]], 
                          round(r.squared(fit)[,4:6],2) ,  
                          BIC = round(fit_sum$BIC,2), 
                          logLik = round(c(fit_sum$logLik),2))
        
    }else{
        Anov <- Anova(fit, type=type)
       
        goodnes <- cbind( Obs = fit_sum$dims[["N"]], 
                          round(r.squared(fit)[,4:6],2) ,  
                          BIC = round(fit_sum$BIC,2), 
                          logLik = round(c(fit_sum$logLik),2))
    }
    
    
    Output(xtTab, 
           caption = paste("Regression Model", caption), 
           caption.above = TRUE, inline.border = FALSE, 
           note = note, center = center) 
    
    
    if(anova) Output(data.frame(Source = rownames(Anov), Anov), 
                     caption = paste("ANOVA:", caption), 
                     caption.above = TRUE, inline.border = FALSE, 
                     note = note, center = center)
    
    Output(goodnes,
           caption = paste("Goodness-of-fit", caption), 
           caption.above = TRUE, inline.border = FALSE, 
           note = "R-Quadrat entspricht Marginal und Conditional", 
           center = center)                               
}


Output<- function(x,...) print(x)



APA2.lm <- function(fit, 
                    caption = "" , note = "", digits = 2, digits.beta=2, 
                    type="III", # exclude = NA,
                    return.string = TRUE, single.row = TRUE, caption.above = TRUE,  inline.css = TRUE, inline.border = FALSE, 
                    doctype = FALSE, html.tag = FALSE, head.tag = FALSE, body.tag = FALSE, 
                    # language = "en",  
                  #  center = options()$stp4$apa.style$center,   
                    stars = c(0.001, 0.01, 0.05), stars.symbols = c("***", "**", "*", "B7")[1:length(stars)], 
                    sig.stars = FALSE, 
                    pvalues=TRUE,  eta=TRUE, anova=TRUE,
                    txt_regr= "Regression Model:",
                    txt_ods="Exp(B):",
                    txt_anov1="Analysis of Deviance Table (Type II tests):",
                    txt_ll="Likelihood ratio test:",
                    txt_god="Goodness-of-fit:",
                    print_odds=FALSE, ###das ist für glm mit poisson mit xtable
                    
                    ...){ 
      #https://statistics.laerd.com/spss-tutorials/poisson-regression-using-spss-statistics.php
    
           
    ans<-NULL
    if(any(class(fit) %in% "aov")){   
        #  cat("class aov")
        # ans<- format_aov( summary(fit)[[1]], sig.stars=sig.stars, pvalues=pvalues,
        #stars=stars,stars.symbols=stars.symbols) 
        #Text("ANOVA ",type)
        ans <- aov_Statistik(fit, type, eta)
        # if (options()$prompt[1] == "HTML> ") {   
        Output(data.frame(Source = rownames(ans$ANOVA), ans$ANOVA), 
               caption = paste("ANOVA:", caption), 
               caption.above = TRUE, inline.border = FALSE, 
               note = note, 
               center = center)
        # }else{ans}
    } else if(any(class(fit) %in% "glm")){  
        #cat("class glm")
        
        myfam <- fit$family$family
        
        cat("\n", myfam, "\n")
        require2(lmtest)
        #lrtst <- lrtest(fit)
        
        lrtst <- try(lrtest(fit))
        if (class(lrtst) ==  "try-error") {
            print_odds <- TRUE
            lrtst <- data.frame(
                "#Df" = NA,
                "LogLik" = NA,
                "Df" = NA,
                "Chisq" = NA,
                "Pr(>Chisq))" = NA
            )
        }
        
        Stat <- glm_Statistik(fit)
        
        
        Output(
            Stat$res_param,
            caption = paste(txt_regr, caption),
            caption.above = TRUE, inline.border = FALSE,
            note = note, center = center
        )
        
        # print(str(Stat))
        
        if (   fit$family$family == "binomial" | fit$family$family == "poisson" | print_odds  )
            Output(
                data.frame(Source = rownames(Stat$odds), Stat$odds),
                caption = paste(txt_ods, caption),
                caption.above = TRUE, inline.border = FALSE,
                note = note, center = center
            )
        
        Output(
            data.frame(
                param = row.names(Stat$Anova),
                LR.Chisq = ff(Stat$Anova[,1], 2),
                Df = Stat$Anova[,2],
                p.value = ffpvalue(Stat$Anova[,3])
            ),
            caption = paste(txt_anov1, caption),
            caption.above = TRUE, inline.border = FALSE,
            note = note, center = center
        )
        
        if (fit$family$family == "binomial")
            Output(
                data.frame(
                    DF = lrtst[,1],
                    LogLik = ff(lrtst[,2],2),
                    Chisq = c("", ff(lrtst[2,4],2)),
                    p.value = c("", ffpvalue(lrtst[2,5]))
                ),
                caption = paste(txt_ll, caption),
                caption.above = TRUE, inline.border = FALSE,
                note = note, center = center
            )
        
        Output(
            data.frame(Source = rownames(Stat$Guetekriterien),
                       Stat$Guetekriterien),
            caption = paste(txt_god, caption),
            caption.above = TRUE, inline.border = FALSE,
            note = note, center = center
        )
        
    }
    else{#--  lineare Regression 
        cat("Achtung: Beta-Werte werden nur ausgegeben wenn alle Einflussvariablen Numerisch sind!")
        #  ZielVar<-if( attr(fit$residuals, "label") != "")  attr(fit$residuals, "label") else names(attr(fit$terms, "dataClasses"))[1]
        
        ans <- lm_Statistik(fit, type=type)#  , type="III"
        #print(ans)
        Output(data.frame(Source = rownames(ans$res_param), ans$res_param[,-1]), 
               caption = paste(txt_regr, caption), 
               caption.above = TRUE, inline.border = FALSE, 
               note = note, center = center)  
        
        if(anova) {
            Text("ANOVA ",type)
            Output(cbind(Source = rownames(ans$ANOVA), ans$ANOVA), 
                   caption = paste("ANOVA (",type,"):", caption), 
                   caption.above = TRUE, inline.border = FALSE, 
                   note = note, center = center)  
        }  
        
        Output(data.frame(t(ans$Guetekriterien)), 
               caption = paste(txt_god, caption), 
               caption.above = TRUE, inline.border = FALSE, 
               note = note, center = center)
    } 
}


#APA2.TukeyHSD <- function(fit, ...){
#  if (options()$prompt[1] == "HTML> ")  HTML(fit) 
#  else print(fit)
#
#}








}



