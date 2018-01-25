library(stp5)
n<-50
lvs<-Cs(a,b,c,d,e,f,h)
lvs<-c( "sehr gut", "2" ,"3", "neutral", "5", "6", "sehr schlecht")
DF2<- data.frame(id=1:n)
DF2<- upData(DF2,
             Group= gl(2, n/2, labels = c("Control", "Treat")),
             sex= sample(gl(2, n/2, labels = c("m", "f"))),
             IQ=rnorm(n),
             Magazines=gl(length(lvs),1,n,lvs),
             Comic.books=gl(length(lvs),2,n,lvs),
             Fiction=gl(length(lvs),3,n,lvs),
             Newspapers=gl(length(lvs),5,n,lvs)
             , labels= c(sex="Gender")
)
head(DF2)
DF2$Newspapers[1:5]<-NA 
l29 <- likert2(DF2[,5:7])



likert3<- function(Formula, 
                   data,
                   subset, 
                   na.action = na.retain,
                   nlevels=NA,
                   ...){
 
 
      X<-Formula_Data(Formula, data, subset, na.action)
      items<- X$Y_data
      
      
      grouping_vars<- X$xname 
      print(grouping_vars)
      #  
      # grouping
      #   data <- if(is.null(X$X_data))  X$Y_data  else cbind(X$X_data, X$Y_data)
      #  }
      
    #  print()
      first_item<- if( any(class(items[,1])=="factor"))    items[,1] else as.factor(items[,1]) 
      #-- wenn alle Numeric dann 
      if (!all(sapply(items, function(x) "factor" %in% class(x)))) {
       # warning("Achtung: items parameter contains non-factors. Will convert to factors")
        items<- dapply2(items, as.factor)
      }
     
     if(is.na(nlevels)) nlevels<-nlevels(first_item) 

    if(!all(unlist(sapply(items, function(x) {levels(x)== levels(first_item)}) ))) {
       warning("All items (columns) must have the same number of levels","\n Verwende levels des ersten Items als Labels!!!") 
        items<- dapply2(items, function(x) factor(as.numeric(x), labels=levels(first_item)) )
       
   
       
       
      }  
      
   # lowrange  <- 1 : ceiling(nlevels / 2 - nlevels %% 2)
   #   highrange <- ceiling(nlevels / 2 + 1 ) : nlevels  
    
  #  print(  lowrange )
  #  print(highrange )
  
  # neue FunkrtonalitC$t 
  ###items <- names_from_Hmisc_
  
  result<- if(is.null(grouping_vars))   dcast(Melt2(items), variable~value, length)
  else  {
    
    #print(  str(first_item) )
   # print( str(items[,1]))
    dcast(
      Melt2(cbind( X$X_data, items), id.vars=1:ncol(X$X_data) ) , 
      paste(paste(grouping_vars, collapse="+"), "+ variable~value") , 
      length)
  #  head(Melt2(cbind( X$X_data, items), id.vars=1:ncol(X$X_data) ) )
    
  } 
  

  
  
  
  
  
  
 # results<- Melt2(items)
#  result <- list(results= results
#                 ,items=items
#                 ,grouping=NULL
#                 ,nlevels=nlevels)
  
  class(result) = c('likert2' ,  class(result) ) 
  
  
 return(result) 
}
#DF2$Magazines <- as.numeric(DF2$Magazines)
#levels(DF2$Magazines )<- c( "sehr gut", "b" ,"c", "d", "e", "f", "sehr schlecht")
l21 <- likert3( ~Magazines+ Comic.books+ Fiction+ Newspapers, DF2)
 l21
#l21 <- likert3(Magazines+ Comic.books+ Fiction+ Newspapers~ Group+sex, DF2)
l21


#l21$result




APA2(l21)
#APA2(l29)
