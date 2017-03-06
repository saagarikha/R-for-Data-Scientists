library(arules)
library(arulesViz)
library(tm)
library(ggplot2)

#path to the Assignment corpus <- get from user - but I embeded it here
cname <- file.path("/Users","sasrinivasan","Downloads","Education","Spring 17","R for Data Scientists","Assignments","AssignmentCorpus")
#dir(cname) #just making sure that right file is present

docs <- VCorpus(DirSource(cname))
#summary(docs)

#pre-process the documents - punctuation, numbers, special characters, convert to lower, remove sto
docs <- tm_map(docs,content_transformer(PlainTextDocument))
docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs,removeNumbers)
docs = tm_map(docs, content_transformer(tolower) )
docs <- tm_map(docs,removeWords,stopwords("english"))
docs <- tm_map(docs,stripWhitespace)
docs <- tm_map(docs,stemDocument)
docs <- tm_map(docs,content_transformer(PlainTextDocument))

#create a document term matrix out of it
dtm <- DocumentTermMatrix(docs,control = list(weighting = weightTfIdf))
dtm
inspect(dtm[1:5,1:20])

#create using TFWeight
dtm.tf <- DocumentTermMatrix(docs,control = list(weighting = weightTf))
dtm.tf
inspect(dtm.tf[1:5,1:20])

#create using WeightBin
dtm.bin <- DocumentTermMatrix(docs,control = list(weighting = weightBin))
dtm.bin
inspect(dtm.bin[1:5,1:20])

#create weightSmart
dtm.smart <- DocumentTermMatrix(docs,control = list(weighting = weightSMART))
dtm.smart
inspect(dtm.smart[1:5,1:20])

#organize the terms by frequency
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq)


#create a dense matrix by removing the sparse words
dtms <- removeSparseTerms(dtm,0.1)
inspect(dtms)

#least frequent terms
freq[head(ord)]

#most frequent terms
freq[tail(ord)]

#table of frequencies
head(table(freq),20)

tail(table(freq),20)

#less frequent dtm, dtms

freq <- colSums(as.matrix(dtms))
freq

findFreqTerms(dtm,lowfreq = 20)

#another approach for frequent terms
wf <- data.frame(word = names(freq), freq= freq)
head(wf)

#plot words appear atleast 20 times
p <- ggplot(subset(wf,freq>20),aes(word,freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#how often terms appear together
#findAssocs(dtm, c("piano" , "cartoon"), corlimit=0.98)

#h cluster on the package - on terms
d <- dist(t(dtms),method = "euclidian")
fit <- hclust(d=d,method="ward.D2")
fit

plot(fit)

#cluster on the docs
d <- dist(dtms,method="euclidian")
fit <- hclust(d=d, method = "ward.D2")
fit
plot(fit)


#kmeans
d <- dist(dtm,method="manhattan")

kc <- kmeans(d,4)
kc$betweenss/kc$totss


d <- dist(dtms,method="manhattan")

kc <- kmeans(d,4)
kc$betweenss/kc$totss

d <- dist(dtm,method="euclidian")

kc <- kmeans(d,4)
kc$betweenss/kc$totss

d <- dist(dtms,method="euclidian")

kc <- kmeans(d,4)
kc$betweenss/kc$totss
