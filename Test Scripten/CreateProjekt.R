CreateProjekt<-
function(project = "000 Dummy", 
         datum = date(), 
         comment = "Test Dummy", 
         checkpoint ="2015-07-01",
         path = "C:/Users/wpete/Dropbox/1_Projekte", 
         lib="stp5"){
  # library(reports)
   #-- die library scheint nicht mehr gewartet zu werden daher habe ich die Funktionen kopiert
  clean<- function (text.var) { gsub("\\s+", " ", gsub("\r|\n|\t", " ", text.var))}
  reducer<-  function (x) gsub("\\s+", " ", x)
   Trim <-function (x) gsub("^\\s+|\\s+$", "", x)
  unblanker<-  function (x) subset(x, nchar(x) > 0)
  scrubber<-  function (text.var, rm.quote = TRUE, fix.comma = TRUE, ...) {
    x <- reducer(Trim(clean(text.var)))
    if (rm.quote) {
      x <- gsub("\"", "", x)
    }
    if (fix.comma) {
      x <- gsub(" ,", ",", x)
    }
    ncx <- nchar(x)
    x <- paste0(Trim(substring(x, 1, ncx - 1)), substring(x, 
                                                          ncx))
    x[is.na(text.var)] <- NA
    x
  }
  
  
  
  folder<-
  function (..., folder.name = NULL) 
  {
    if (!is.null(folder.name)) {
      x <- strsplit(folder.name, split = ", ")
    }
    else {
      x <- substitute(...())
    }
    if (!is.null(x)) {
      x <- unblanker(scrubber(unlist(lapply(x, function(y) {
        as.character(y)
      }))))
    }
    hfolder <- function(folder.name = NULL) {
      if (is.null(folder.name)) {
        FN <- mgsub(c(":", " "), c(".", "_"), substr(Sys.time(), 1, 19))
      }
      else {
        FN <- folder.name
      }
      if (length(unlist(strsplit(FN, "/"))) == 1) {
        x <- paste(getwd(), "/", FN, sep = "")
      }
      else {
        x <- FN
      }
      dir.create(x)
      return(x)
    }
    if (is.null(x)) {
      hfolder()
    }
    else {
      if (length(x) == 1) {
        hfolder(x)
      }
      else {
        lapply(x, function(z) {
          hfolder(z)
        })
      }
    }
  }
  
  
  
  
  
  WD <- getwd()
  on.exit(setwd(WD))
  # print(  paste0(path, "/", project))
  # print ( file.exists(paste0(path, "/", project)) )
  
  if(file.exists(paste0(path, "/", project))){
    cat(paste0("\"", paste0(path, "/", project), "\" already exists:\nDo you want to overwrite?\n\n"))
    ans <- menu(c("Yes", "No"))
    if (ans == "2") {stop("new_project aborted")
    }else {delete(paste0(path, "/", project))}
  }
  x <- suppressWarnings(invisible(folder(folder.name = paste0(path,"/", project))))
  setwd(x)
  "Processed data" <- "Raw scripts" <- "Raw data"  <- R<- Results <- Docs <- Fig <- NULL
  invisible(folder("Processed data", "Raw data", "Raw scripts", Results, R, Docs, Fig))
  myswd<- paste0("setwd(\"", x,"\")")
  #---------------------------------------------------------------------------------
  
  if(lib=="stp4"){
    cat( "#-- Eigene Functionen", file = "R/miscFun.r")
    cat( paste( project, datum, path, comment, sep="\n"  ) , file = paste0("A1 ",project, ".csv"))
    cat(  
      paste0(
        
        '
        library(stp4)
        graphics.off()
        ' 
        ,myswd,
        '
        Start("", "'
        ,project, 
        '", "'      ,datum,
        '")   
        source( "R/miscFun.r",  echo=F) 
        # data <-unzip( "Raw data/auswertung.zip" , exdir= Folder[1] )   some(DF <- GetData( data[1] ) ) 
        # some(DF <- GetData( "Raw data/File.R" )) 
        
        
        
        
        
        
        
        End()
        
        
        ')
      , file = "Auswertung.r")
  }else   if(lib=="stp5"){
    cat( "#-- Eigene Functionen", file = "R/miscFun.r")
    cat( paste( project, datum, path, comment, sep="\n"  ) , file = paste0("A1 ",project, ".csv"))
    
    cat(  
      paste0(
        '#- library(checkpoint)
        #- checkpoint("', checkpoint, '")
        #- normalizePath(.libPaths(), winslash = "/")
        
        ',
        
        
        '
        library(stp5)
        # library(formatR)
        graphics.off()
        ' 
        ,myswd,
        '
        Start("", "'
        ,project, 
        '", "'      ,datum,
        '")   
        # source("R/miscFun.r", echo=F) 
        #-- Load Data ---------------------------------------------
        # data <- unzip("Raw data/auswertung.zip", exdir= Folder[1] ) some(DF <- GetData(data[1])) 
        # some(DF <- GetData("Raw data/File.R"))    # summary(~., DF)
        data_dictionary<- GetData(',"'",
        'name	plot_name	group	description
        id	      identifier	 demographic	identifier
        sex	      Sex	         demographic	"Male (M) or Female (F)"
        diet_days "Days on diet" clinical       "Number of days on high-fat diet"',"')",
        '
        
        #-- Tidy Data ---------------------------------------------
        
        # save(DF, file="Processed data/Tidy.Rdata)
        
        #-- Analyse Data ------------------------------------------
        
        End()
        
        
        ')
      , file = "(1) Get Data.R")
    
    
    cat(  
      paste0(
        '#- library(checkpoint)
        #- checkpoint("', checkpoint, '")
        #- normalizePath(.libPaths(), winslash = "/")
        
        ',
        
        
        '
        library(stp5)
        # library(formatR)
        graphics.off()
        ' 
        ,myswd,
        '
        Start("", "'
        ,project, 
        '", "' ,datum,
        '")   
        # source("R/miscFun.r", echo=F) 
        #-- Load Data ---------------------------------------------
        str(load("Processed data/Tidy.Rdata) )
        #-- Analyse Data ------------------------------------------
        
        End()
        
        ')
      , file = "(2) Analyse.R")    
    
  }
  
  
  
  else{
    cat( '#-- Eigene Functionen
         source("C:/Users/wpete/Dropbox/Student/[Musterordner/stp2misc.r", echo=F)
         source("C:/Users/wpete/Dropbox/Student/[Musterordner/Print2.r", echo=F)
         '
         , file = "Raw scripts/miscFun.r")
    cat( '#-- Projekt:
         graphics.off()
         '
         ,myswd,
         '
         
         Folder<- c("Raw data","Processed data","Raw scripts") 
         Datum  <- "12.11.2012 13:09:01"
         File  <-  "Example/Arbeitsdaten.sav"
         
         source( "Raw scripts/miscFun.r",  echo=F) 
         Start("MasterFile", "")   # data <-unzip( "Raw data/auswertungdiplomarbeitmedizin.zip" , exdir= Folder[1] )   some(DF <- GetData( data[1] ) )  
         
         some(DF <- GetData( paste0(Folder[1], "/",File) )) ##- summary(~., DF)
         
         
         End()'
         
         , file = "Auswertung.r")
    
  }
  
  cat("\nOk\n\n")
}



 