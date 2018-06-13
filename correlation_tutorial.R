#Computing Correlation Matrix
M <- cor(mtcars)
head(M)
library(corrplot)
#Correlogram:Visualizing hte correlation matrix
corrplot(M, method = "circle")
# pie
corrplot(M, method = "pie")
#color (Recommended)
corrplot(M, method = "color")
#Number (Best)
corrplot(M, method = "number")

#Types of Correlogram
#1. Upper
corrplot(M, type = "upper")
#2. Lower
corrplot(M, type = "lower")

#Reordering the correlation matrix

#correlogram with hclust reordering
corrplot(M, type = "upper", order = "hclust")

#Using different color spectrum

col <- colorRampPalette(c("red","white","blue"))(20)
corrplot(M, type = "upper", order = "hclust",col =col)

#Change background color to lightblue

corrplot(M, type = "upper", order = "hclust", col = c("black","white"), bg = "lightblue")

library(RColorBrewer)
corrplot(M, type = "upper", order = "hclust",
         col = brewer.pal(n= 8, name = "RdBu"))
#Red,Blue and Yellow
corrplot(M, type ="upper", order = "hclust",
         col = brewer.pal(n=8, name = "RdYlBu"))
#Purple and Orange
corrplot(M, type = "upper", order = "hclust",
         col = brewer.pal(n=8, name = "PuOr"))

#Changing the color and rotation of text labels

corrplot(M, type = "upper", order = "hclust", tl.col="black", tl.srt = 45)

#Combining correlogram with the significance test

cor.mtest <- function(mat, ...){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA,n,n)
  diag(p.mat) <- 0
  for (i in 1:(n-1)) {
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], ...)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(mtcars)
head(p.mat[,1:5])


# Add significance level to the correlogram

# Specialized the insignificant value according to the significant level

corrplot(M, type = "upper", order = "hclust", p.mat = p.mat, sig.level = 0.01)

#Leave Blank on no significant coefficent

corrplot(M, type = "upper", order = "hclust", p.mat= p.mat, sig.level = 0.01, insig = "blank")

#Customize the correlogram

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method = "color", col = col(200), type = "upper", order = "hclust",
         addCoef.col = "black", #Add Coefficent of Correlation
         tl.col = "black", tl.srt = 45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank",
         #hide correlation coefficient on the principal diagonal
         diag = FALSE)
