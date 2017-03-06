winequality_white <- read_delim("~/Downloads/Education/Spring 17/R for Data Scientists/R_CSV/winequality-white.csv", ";", escape_double = FALSE, trim_ws = TRUE)
#remove the last item
winequality_white$quality[winequality_white$quality>=9] <- "Excellent"
winequality_white$quality[winequality_white$quality==8] <- "Good"
winequality_white$quality[winequality_white$quality==7] <- "Good"
winequality_white$quality[winequality_white$quality==6] <- "Good"
winequality_white$quality[winequality_white$quality==5] <- "Not Good"
winequality_white$quality[winequality_white$quality==4] <- "Not Good"
winequality_white$quality[winequality_white$quality==3] <- "Not Good"
winequality_white$quality[winequality_white$quality==2] <- "Bad"
winequality_white$quality[winequality_white$quality==1] <- "Bad"
winequality_white$quality[winequality_white$quality==0] <- "Bad"

#change the scale and remove the last item
winequality_white <- read_delim("~/Downloads/Education/Spring 17/R for Data Scientists/R_CSV/winequality-white.csv", ";", escape_double = FALSE, trim_ws = TRUE)
new_wine <- winequality_white
new_wine$quality = NULL
new_wine <- scale(new_wine)

#now we do the Hierarchical clustering
new_wine.complete =  hclust(dist(new_wine),method="complete")

#plot for comparison to know where to cut
par(mfrow=c(1,1))
plot(new_wine.complete,main="Complete Linkage", xlab="", sub="", cex=.9)

table(cutree(new_wine.complete,3),winequality_white$quality)

#now use the same PCA obtained for the previous method and see if it improves the results
pca_wine.complete =  hclust(dist(wine_pca$x[,1:1]),method="complete")
par(mfrow=c(1,1))
plot(pca_wine.complete,main="Complete Linkage", xlab="", sub="", cex=.9)

table(cutree(pca_wine.complete,3),winequality_white$quality)

