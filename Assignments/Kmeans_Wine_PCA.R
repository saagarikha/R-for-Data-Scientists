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
new_wine <- winequality_white
new_wine$quality = NULL
new_wine <- scale(new_wine)

#run the normal kmeans - since there was no bad wine, i used only three samples.
wine_kmeans <- kmeans(new_wine,3)
table(winequality_white$quality,wine_kmeans$cluster)
wine_kmeans$betweenss/wine_kmeans$totss
#now apply PCA to the components
wine_pca <- prcomp(new_wine)
biplot(wine_pca,cex=c(1/3,1/2), scale=0)
wine_pca.var =wine_pca$sdev ^2
pve=wine_pca.var/sum(wine_pca.var)
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="
     Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
     type='b')

#after principal component and variance - 4-6 anything should do good - i am taking 5
km_wine_pca = kmeans(wine_pca$x[,1:2],3)
table(winequality_white$quality,km_wine_pca$cluster)
km_wine_pca$betweenss/km_wine_pca$totss