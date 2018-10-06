# devtools::install_github("jumpingrivers/prettyB")
 
## Warning: base function masking
library("prettyB")

op = par(mfrow = c(1, 2))
 plot.default(iris$Sepal.Length, iris$Sepal.Width)
 plot(iris$Sepal.Length, iris$Sepal.Width)

theme_set("minimal")
 plot.default(iris$Sepal.Length, iris$Sepal.Width, 
                       main="Classic Iris Dataset", xlab="Length", ylab="Width")
 plot(iris$Sepal.Length, iris$Sepal.Width, 
     main="Classic Iris Dataset", xlab="Length", ylab="Width")
