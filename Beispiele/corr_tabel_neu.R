corr_tabel_X <-
  function (...,
            type = c("pearson", "spearman"),
            exclude = NA, subset, na.action = na.pass,
            cor_diagonale_up = TRUE,
            sig.star = TRUE, p.value = FALSE,
            include.mean = FALSE,
            include.n = TRUE)
  {
    type <-  match.arg(type)
    #-- Vorbereiten der Daten (na.omit, subset)
    ANS <- NULL
    X <- prepare_data2(..., na.action = na.action)
   # return(X )
    N <- nrow(X$data)
    measure.vars<- X$measure.vars
    group.vars<- X$group.vars
    row_name <- X$row_name
    Y_data<-X$data[measure.vars]
    X_data <-X$data[group.vars]

    condition.vars<- X$condition.vars


    ## wenn nur ein element dann ergibt sich ein Fehler
    if (ncol(X$data[measure.vars]) == 1)
      X$data[measure.vars]  <- dplyr::as_data_frame(X$data[measure.vars])


    if (!is.null(condition.vars)) {
      cat(" in !is.null(condition.vars)")
      #groups <- all.vars(groups)
      if (length(condition.vars) == 1) {


        condition <- X$data[[condition.vars]]
        if (X$condition.class  != "factor") {
          warning("Achtung nur eine Faktor kann Gruppen bilden!")
          return(head(X$data))
        }

        condition <- droplevels(condition)
       # return(condition)
        lvls <- levels(condition)
        g1 <- which(condition == lvls[1])

        # vor corr_2"


        ans <- stp25APA2:::corr_2( Y_data[g1,], X_data[g1, 1], type)
        names(ans)[2:4] <- paste0(lvls[1], "_", names(ans)[2:4])

        for (i in 2:(length(lvls))) {
          g2 <- which(condition == lvls[i])
          ans2 <-  stp25APA2:::corr_2(Y_data[g2,], X_data[g2, 1], type)
          names(ans2)[2:4] <-
            paste0(lvls[i], "_", names(ans2)[2:4])
          ans <- cbind(ans, ans2[-1])
        }

      }
      else{
        return("Achtung nur eine Gruppe kann berechnet werden!")
      }
      ans[, 1] <- factor(ans[, 1], names(row_name), row_name)
      attr(ans, "note") <- type
      attr(ans, "N") <- N
    }
    else if (!is.null(group.vars)) {
      # Beginn der Funktion -----------------------------------------------------
      ans <- stp25APA2:::corr_2(Y_data, X_data[1], type)

      if (length(X$xname) > 1) {
        names(ans)[2:4] <- paste0(group.vars[1], "_", names(ans)[2:4])
        for (i in  2:(length(group.vars))) {
          ans2 <-  stp25APA2:::corr_2(Y_data, X_data[i], type)
          names(ans2)[2:4] <-
            paste0(group.vars[i], "_", names(ans2)[2:4])
          ans <- cbind(ans, ans2[-1])
        }
        attr(ans, "note") <- type
        attr(ans, "N") <- N
      }
      ans[, 1] <- factor(ans[, 1], names(row_name), row_name)
    }
    else{


      ans <- stp25APA2:::corr_1( Y_data, type = type) ## liste
      print(ans)
      ans$mean <-
        t(stp25APA2:::berechne.default(Y_data , measure.vars, measure = "mean", type = 1))

      ans$row_name <- row_name
    }
    ordne_corr_tabel(ans)
  }

ordne_corr_tabel <- function(ans,
                        cor_diagonale_up = TRUE,
                        sig.star = TRUE,
                        p.value = FALSE,
                        # mean = FALSE, # Veraltet
                        include.mean = FALSE,
                        include.n = TRUE)
{

  #cat("\ninclude.mean =", include.mean, "\n")
  res <- data.frame()
  if (class(ans) == "rcorr") {
    #-- Hmisc also wie aus APA2
    format_diagonale <- function(mycorrtable,
                                 d = 1,
                                 l = "") {
      diag(mycorrtable) <- d
      if (cor_diagonale_up)
        mycorrtable[lower.tri(mycorrtable)] <- l
      else
        mycorrtable[upper.tri(mycorrtable)] <- l
      mycorrtable
    }
    n <- Format2(ans$n, 0)
    # colnames(n)<- paste0(colnames(n), "_", "n" )
    r <- format_diagonale(Format2(ans$r, 2))

    if (sig.star) {
      p <- apply(ans$P, 2, function(x)
        cut(
          x,
          c(-Inf, options()$stp25$apa.style$p$stars.value, Inf),
          c(options()$stp25$apa.style$p$stars.symbols, "")
        ))

      r <- format_diagonale(matrix(
        paste0(Format2(ans$r, 2), p),
        nrow = nrow(ans$r),
        dimnames = dimnames(ans$r)
      ))
    }
    if (p.value) {
      p <- paste0(" (p=", ffpvalue(ans$P), ")")
      r <- format_diagonale(matrix(
        paste0(Format2(ans$r, 2), p),
        nrow = nrow(ans$r),
        dimnames = dimnames(ans$r)
      ))
    }

    res <- data.frame(Source = rownames(ans$r), r)
    attr(res, "note") <-  attr(ans, "note")
    attr(res, "N") <-  attr(ans, "N")
    my_num <- paste0("(", 1:length(ans$row_name), ")")

    res[, 1] <-
      factor(res[, 1], names(ans$row_name), paste(my_num, ans$row_name)) # Labels
    colnames(res)[2:ncol(res)] <- my_num

    if(!include.n){

      cat("\n", include.n, "\n\n-----\n")
      # grep("_N", c("jdhdz", "hdgdt_N", "sfsg_"))
      #ans<- ans[- grep("_N", names(ans))]
    }

    if (include.mean)
      res <- cbind(res[1], "M (SD)" = ans$mean, res[2:ncol(res)])
    return(res)
  }
  else{
    return(ans)
  }
}
prepare_data2(m1+m2~m3 , varana)
corr_tabel_X(~m1+m2, varana)
corr_tabel_X(m1+m2~m3, varana)
corr_tabel_X(m1+m2~m3|geschl, varana)

