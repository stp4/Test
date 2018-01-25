

Beispieldaten.Borz <- 
  matrix(c(
    2,8,10,13,17,
    5,10,15,18,2,
    10,12,20,5,3,
    15,20,10,3,2,
    22,18,7,2,1)
         , nrow = 5, ncol=5, byrow=TRUE,dimnames = list(c("A", "B", "C", "D", "E"),1:5))


Rangreihe<- function(items, grouping=NULL, transpose=FALSE)  {
  
  if(!is.null(grouping)){
    
    data_by_group <- split(items, grouping )
    cat("\n\n")
    print(grouping)
    print(data_by_group)
    cat("\n\n")
    grouping <- lapply(data_by_group
                  ,Rangreihe
                  ) 
    
    #print(counts)
  }

  
 # cat("\nLaw of Categorial Judgement\nBorz & D??ring Seite 158\n\n")
  require(reshape2)
  
  My_table<-function(items,my_ranks){ 
    sapply(items,
           function(x, ...){ 
             x<-factor(x, ...)
             table(x)
           }
           ,levels=my_ranks, simplify = TRUE) 
  }  
  my_ranks <-levels(factor(as.character(melt(data.frame(id=1:nrow(items),items),id.vars=1)$value),exclude="NA")) 
  faktotrstyle<-any(grepl("[[:alpha:]]+$", my_ranks))        
  my_table <- My_table(items, my_ranks ) 

  if(!faktotrstyle) my_table<-t(my_table)  
  rel_feq <-prop.table(my_table, 1)                                           
  if(ncol(my_table) < nrow(my_table)   ) {
    if(!faktotrstyle) my_ranks <- c(my_ranks, "n.a.")    else  items$n.a. <-  NA 
    my_table<- My_table(items, my_ranks )  
    
    if(!faktotrstyle) my_table<-t(my_table)  
    my_table[ ,ncol(my_table)  ]<-  nrow(items) - rowSums( my_table ) 
    rel_feq <- prop.table(my_table, 1) 
  }
  kum_feq <- t(apply(rel_feq,1,cumsum))
  
  z.wert <- qnorm(kum_feq[,-ncol(kum_feq)])
  z.wert[which(is.infinite(z.wert))]<-NA
  zeilen.mittel <- rowMeans(z.wert,na.rm=T)
  spalten.mittel <- colMeans(z.wert,na.rm=T)

  ANS <- as.data.frame(my_table) 
  anz <- formatC(my_table, format="f", digits=0)
  prz <- formatC(rel_feq*100, format="f", digits=0)
  ANS <- data.frame( matrix( paste0(prz,"% (", anz,")" ), 
                          nrow=nrow(my_table), dimnames=dimnames(my_table) )  )
  ANS$mittlerer.Rang<-  round(rowSums(rel_feq*1:ncol(rel_feq)),1)
  ANS$Skalenwert<- -round(mean(zeilen.mittel,na.rm=T)-zeilen.mittel,3)
  if(!is.null(grouping))   {grouping[["all"]] <- ANS
                              ANS <-grouping
  
  
  }
 # class(ANS) = 'rangordnung'
  return(ANS)
}
DF<-  data.frame(A=c(1,1,1,2,3,1), B=c(2,2,2,3,2,3), C=c(3,3,3,1,1,NA), D=c(NA,NA,NA,NA,NA,2))
DF2<-   data.frame(R1=Cs(a,a,b,b,b,a,a,b,a),R2=Cs(b,b,a,a,c,c,b,c,c),R3=Cs(c,c,c,c,f,d,d,d,d) )


Rangreihe(DF) 


(ans<-Rangreihe(DF2))


gr<- factor(rep(1:2, length.out=nrow(DF)))
Rangreihe(DF, gr) 


#Rangreihe( split(DF, gr)[[1]] )


