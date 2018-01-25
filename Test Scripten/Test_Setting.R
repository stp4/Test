
library(stp25vers)
library(tidyverse)
Projekt("html")
# library(formatR)
graphics.off()


set_my_options()
get_my_options()$apa.style$prozent

get_my_options()$fig_folder
set_my_options(fig_folder="Fig2")
get_my_options()$fig_folder

get_my_options()$apa.style$mittelwert




APA2(~. , hkarz)
set_my_options(prozent=list(digits=c(1,0), style=2))
get_my_options()$apa.style$prozent

set_my_options(mittelwert=list(digits=c(1,4), plusmin_sign=TRUE))
get_my_options()$apa.style$mittelwert

set_my_options(bez=NULL)
get_my_options()$bez


APA2(~. , hkarz)


APA2(~. , hkarz, type = c("auto", "median"))

set_my_options(median=list(style=2))

APA2(~. , hkarz, type = c("auto", "median"))
median(hkarz$tzell)
End()

median(hkarz$tzell)
IQR(hkarz$tzell)
fivenum(hkarz$tzell)
?settings


