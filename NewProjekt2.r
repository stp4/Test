#================================
Name      = "Lara Kay (P)"  # M Medizin Z Zahnmedizin
# Dr Artzt #v Veterinär
# B BWL  P Psychologie PH Pädagogische Hochschule OÖ
# U Umit/FH-gesund  X Alles ander
# F Firma
Email     = "<lara.kay08@gmail.com>"
Tel       = "Tel 0650 8550 525"
Adresse	  = " "
Aufwand	  = "2-5 Stunden"
Thema	    = "Vergleich bzgl Entwicklungswissens"

Kommentar = "Bereits gerechnet und geschrieben aber als negativ beurteilt."
#================================
#  75  Student
#  80  Doktorand berufsbegleitend
#  90  Klinik
# 125  Firmen
Stundensatz = 75
Datum =  format(Sys.time(), "%d.%m.%Y")
Zeit =format(Sys.time(), "%H:%M")
Folder  = "C:/Users/wpete/Dropbox/1_Projekte"
KNr       = NA






Name<- gsub("[^A-Za-z0-9 ()]", "", Name)



FunktionsTest<-FALSE



##devtools::install_github("jennybc/googlesheets")
##install.packages("XML")
##devtools::install_github("hadley/xml2")
library(stp25vers)
library(googlesheets)
library(dplyr)

user_session_info <- gs_user()

Projekt <- gs_title("ProjektVorlageKunde") %>% gs_gs()
Kunde <-   Projekt %>% gs_read(ws = "Stammdaten", range = cell_cols("A:K"))

n<-nrow(Kunde)
last_KNr <-Kunde[n, 1]
n_row<- n+2
KNr<- as.numeric(as.character(last_KNr))+1
Kunden_Daten<-c(KNr,
                Datum,	Zeit,	Name,
                Email,	Tel,	Adresse,	Aufwand	,
                Thema,	Kommentar,	Stundensatz)
neuer_Kunde <- paste(KNr, Name)

#-- neuen Kunde anlegen ---------------------------------
myCopy <-  Projekt %>%
           gs_copy(to = neuer_Kunde)

cat("\n Erstelle neues Googel-Dokument:",  myCopy$sheet_title, "\n\n")
#?gs_edit_cells
#




try(Projekt %>% gs_edit_cells( ws = "Stammdaten",
                           input = Kunden_Daten,
                           anchor = paste0("A", n_row),
                           trim = TRUE,
                           byrow = TRUE
                           ))
  cat("\nKopiere neuen Kunden in die Projektliste\n" )

  try(myCopy %>%  gs_edit_cells( ws = "Stammdaten",
                           input = Kunden_Daten,
                           anchor = paste0("A", 2),
                           trim = TRUE,
                           byrow = TRUE
                           )
)

if(!FunktionsTest){
#-- Create Project ---------------------------------------------
#install.packages("reports")
#
setwd(Folder)

CreateProjekt( project = neuer_Kunde,
               datum = Datum,
               comment = Kommentar,
               path = Folder

)
}
