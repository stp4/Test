Start <-
function (myformat = "", Projektname = "", datum = NA, dir = NA, 
    grafik = "ggplot2like", html_folder = "TMP", ...) 
{
    invisible(library(Hmisc, T))
    invisible(require(effects))
    invisible(require(car))
    invisible(library(lattice))
    invisible(require(latticeExtra))
    invisible(library(reshape2))
    invisible(require(psych))
    invisible(require(arm))
    invisible(require(xtable))
    if (myformat == "knit") {
        myformat <- ""
        stp2_exist <- FALSE
    }
    else {
        invisible(stp2_exist <- require(stp2))
    }
    if (grafik == 2 | grafik == "ggplot2like") {
        opar <<- trellis.par.get()
        trellis.par.set(ggplot2like(n = 4, h.start = 180))
        trellis.par.set(axis.text = list(cex = 0.8, lineheight = 0.9, 
            col = "grey20"))
        oopt <<- lattice.options(ggplot2like.opts())
    }
    if (!stp2_exist) {
        options(continue = "  ")
        .Init <<- function(...) "Nicht Implementiert!"
        End <<- function(...) {
            cat("Good by!\n\n")
        }
        Print <<- function(...) print(...)
        Print2 <<- function(...) print(...)
        Tabelle <<- function(formula, data, ...) summary(formula, 
            data)
        Sig.Test <<- function(formula, data, ...) summary(formula, 
            data, method = "reverse", test = TRUE)
        Reliability <<- function(x, revcoded = F, fun = "mean", 
            na.rm = T, digits = 2, check.keys = F, ...) {
            if (!is.logical(revcoded)) {
                mytempdata <- x[, revcoded]
                if (is.numeric(mytempdata)) 
                  x[, revcoded] <- max(x, na.rm = T) + 1 - mytempdata
                else x[, revcoded] <- apply(mytempdata, 2, function(x) max(x, 
                  na.rm = T) + 1 - x)
            }
            print(alpha(x, check.keys = check.keys))
            switch(fun, mean = rowMeans(x, na.rm = na.rm), sum = rowSums(x, 
                na.rm = na.rm), x)
        }
        reset <<- par(no.readonly = T)
        if (!exists("HTML")) 
            HTML <<- function(...) print(...)
        Cs2 <<- function(...) {
            if (.SV4. || .R.) 
                x <- as.character(sys.call())[-1]
            else {
                y <- ((sys.frame())[["..."]])[[1]][-1]
                x <- unlist(lapply(y, deparse))
            }
            if (length(x) == 1) 
                x <- .trim.ws(x)
            if (all.is.numeric(levels(factor(x)))) 
                x <- as.numeric(as.character(x))
            x
        }
    }
    if (!is.na(dir)) 
        setwd(dir)
    .Init(html_folder, myformat)
    cat(paste("\n\nDatei: ", getwd(), "\n\n"))
    cat("\n**********************************\n*  DI Wolfgang PETER             *\n*  Data Engineering & Statistics *\n*  w.peter@statistik-peter.at    * \n*  Tel +43 699 81530117          *\n**********************************\n")
    Print(c(Projektname, "\nDatum: ", datum, "\n\n", R.version.string))
}
