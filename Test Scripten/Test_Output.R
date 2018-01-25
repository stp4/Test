 graphics.off()
 
library(stp5)
 
 Start("html")
 set.seed(1)
 data<-data.frame(x=1:30, 
                  y=gl(3, 10, labels = c("sehr gut", "mittel", "schlecht")),
                  z=gl(2, 15, labels = c("Control", "Treat"))[sample.int(30)]
 )
 res<-APA2(x~y, data )
 library(htmlTable)
 # A simple output
 output <- matrix(1:4,
                  ncol=2,
                  dimnames = list(list("Row 1", "Row 2"),
                                  list("Column 1", "Column 2")))
 htmlTable(output)
 
 # An advanced output
 output <-
     matrix(ncol=6, nrow=8)
 
 for (nr in 1:nrow(output)){
     for (nc in 1:ncol(output)){
         output[nr, nc] <-
             paste0(nr, ":", nc)
     }
 }
 
 x<-htmlTable(output, align="c",
           header =  c( "1st", "2nd","3rd", "4th","5th", "6th"),
                           
           rnames = paste(c("1st", "2nd", "3rd",paste0(4:8, "th")),"row"),
           rgroup = c("Group A", "Group B", "Group C"),
             n.rgroup = c(2, 4, 2),
           # cgroup = rbind(c("", "Column spanners", NA),
           #                c("A", "B group 1", "C group 2&dagger;")),
           #   n.cgroup = rbind(c(1,2,NA),
           #                    c(2,2,2)),
           
           cgroup = rbind(c("", "A2", NA),
                          c("", "B group 1", "C group 2&dagger;")),
             n.cgroup = rbind(c(1,5,NA),
                              c(1,2,3)),
           
           caption="Basic table with both column spanners (groups) and row groups",
           tfoot="&dagger; A table footer commment",
        #   cspan.rgroup = 2,
          # col.columns = c(rep("none", 2),
          #                 rep("#F5FBFF", 4)),
          # col.rgroup = c("none", "#F7F7F7"),
           css.cell = "padding-left: .5em; padding-right: .2em;")

 
 class(x)
 Output("x")
  Output(x)
  
 End()
 