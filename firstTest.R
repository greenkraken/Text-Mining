# Bereinigung der Arbeitsbereiches

graphics.off();
rm(list = ls());
cat("\014");

#Istallation benötigter "packages"
## ipak function: install and load multiple R packages.
## (from https://gist.github.com/stevenworthington/3178163)

## Überprüfung ob die geforderten "packages" installiert sind. Installation noch benötigter
## "packages" und einspielen dieser in die "R"-Programmstruktur.

## Functionsdefinition

ipak <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) 
    
    install.packages(new.pkg, dependencies = TRUE);
  
  sapply(pkg, require, character.only = TRUE)
  
}



## Anwendung

packages <- c("ggplot2", "plyr", "reshape2", "RColorBrewer","scales", "grid", "extrafont", "NLP", "tm", "SnowballC", "wordcloud", "biclust", "cluster", "igraph", "fpc", "quanteda", "corrplot", "igraph", "devtools")

ipak(packages)

# Definition benötigter Variablen

## Im Programm vordefiniert
### Minimale Anzahl der Wörter ind er Häufigkeitsauflistung
minFreq <- 100;

## Vom Nutzer abgefragt

# minFreq <- readline("Geben sie die gewünschte minimale Häufigkeit für die Darstellung an:")

# Laden der Textbibliothek (interaktive Gestaltung möglich)

# folderpath <- readline("Insert the path/URL of your sourcefolder:")
# folderpath <- file.path("T:/Documents/Studium/BA/RStudio/textKorea")
# folderpath <- file.path("T:/Documents/Studium/BA/RStudio/textsADAABA/split")
folderpath <- file.path("T:/Documents/Studium/BA/RStudio/textsADAABA/example")

dir(folderpath)
setwd(folderpath)
write(strwrap(DirSource(folderpath)),file = "T:/Documents/Studium/BA/RStudio/Output/01 Blank.txt")

#create Corpus
myCorpus <- Corpus(DirSource(folderpath))

look <- strwrap(myCorpus[[1]])
write(look, file = "T:/Documents/Studium/BA/RStudio/Output/02 Corpus.txt")


# Preprocessing Text (from https://cran.r-project.org/doc/contrib/Zhao_R_and_data_mining.pdf)

# convert to lower case 
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

look <- strwrap(myCorpus[[1]])
write(look, file = "T:/Documents/Studium/BA/RStudio/Output/03 ToLower.txt")

# remove punctuation 
myCorpus <- tm_map(myCorpus, removePunctuation)

look <- strwrap(myCorpus[[1]])
write(look, file = "T:/Documents/Studium/BA/RStudio/Output/04 RemovePunctuation.txt")

# remove numbers 
myCorpus <- tm_map(myCorpus, removeNumbers)

look <- strwrap(myCorpus[[1]])
write(look, file = "T:/Documents/Studium/BA/RStudio/Output/05 RemoveNumbers.txt")

#remove stop words
# add two extra stop words: "available" and "via" 
myStopwords <- c(stopwords("english"),"available", "via", "rar") 
# remove "r" and "big" from stopwords 
myStopwords <- setdiff(myStopwords, c("r", "big")) 
# remove defined stopwords from corpus 
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

look <- strwrap(myCorpus[[1]])
write(look, file = "T:/Documents/Studium/BA/RStudio/Output/06 RemoveStopwords.txt")

# remove URLs 
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) 
myCorpus <- tm_map(myCorpus, removeURL) 

look <- strwrap(myCorpus[[1]])
write(look, file = "T:/Documents/Studium/BA/RStudio/Output/07 RemoveURL.txt")

# Stemm Text (from https://cran.r-project.org/doc/contrib/Zhao_R_and_data_mining.pdf)

# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)

look <- strwrap(myCorpus[[1]])
write(look, file = "T:/Documents/Studium/BA/RStudio/Output/08 Stemming.txt")

# Error in x$content[[i]] : subscript out of bounds
# # # inspect documents (tweets) numbered 11 to 15
# # # inspect(myCorpus[11:15])
# # # The code below is used for to make text fit for paper width
# for (i in 11:15) {
#   cat(paste("[[", i, "]] ", sep=""))
#   writeLines(strwrap(myCorpus[[i]], width=73))
#   }
#   
# # # stem completion (Error output: Error in UseMethod("meta", x) : 
# no applicable method for 'meta' applied to an object of class "character")
# myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=myCorpusCopy)

# # count frequency of "building"
# buildingCases <- tm_map(myCorpusCopy, grep, pattern="\\<building") 
# sum(unlist(buildingCases))

# Convert to TextDocument

myCorpus <- tm_map(myCorpus, PlainTextDocument)

# Build up TDM and DTM
TDM <- TermDocumentMatrix(myCorpus)
DTM <- DocumentTermMatrix(myCorpus)
tdm <- as.matrix(TDM)
dtm <- as.matrix(DTM)

# inspect frequent words

findFreqTerms(TDM, lowfreq = 100)

###Show terms frequencies with histogram
# can see the Zipf's law !
term.freq <- rowSums(tdm)
term.freq <- subset(term.freq, term.freq>=minFreq)
word_freqs = sort(term.freq, decreasing=FALSE) 
vocab <- names(word_freqs)
# create a data frame with words and their frequencies
df = data.frame(terms=vocab, freq=word_freqs)

df$terms <- factor( df$terms, levels=unique(as.character(df$terms)) )
ggplot(df, aes(terms,freq))+ geom_bar(stat= "identity") + scale_x_discrete(name="Terms", labels=df$terms)+ xlab("Terms") + ylab("Freq") + coord_flip()



######### TERMS * TERMS Matrix (Graph) #######
head(tdm)
tdm_short <- tdm[names(tail(sort(rowSums(tdm)), 7)), ]
# transform into a term-term adjacency matrix
tdm_short <- tdm_short %*% t(tdm_short)
#### Create a graph from it
# library(igraph)
#build a graph from the above matrix
g <- graph.adjacency(tdm_short, weighted=T, mode="undirected")
# remove loops
g <- simplify(g)
### Visualize it
#plot.igraph(g, layout=layout_with_fr(g, niter=5000), edge.label=round(E(g)$weight, 2))
# mtext("Terms Co-occurrences", side=1)
plot.igraph(g, layout=layout_with_fr(g, niter=5000))
            
# ######### TERMS * TERMS Matrix (Graph) #######
# # transform into a term-term adjacency matrix
# tdm <- tdm %*% t(tdm)
# #### Create a graph from it
# library(igraph)
# #build a graph from the above matrix
# g <- graph.adjacency(tdm, weighted=T, mode="undirected")
# # remove loops
# g <- simplify(g)
# ### Visualize it
# plot.igraph(g, layout=layout.fruchterman.reingold(g, niter=1000, area=100*vcount(g)^2),
#             vertex.label.family = "AppleMyungjo")
# mtext("Terms Co-occurrences", side=1)


# termFrequency <- rowSums(tdm)
# termFrequency <- subset(termFrequency, termFrequency>=100)
# termFrequency
# #barplot(termFrequency, las=2) + coord_flip()
# qplot(names(termFrequency), termFrequency, geom="bar", xlab="Terms") + coord_flip()

#wordcloud
m <- as.matrix(TDM)
# calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(rowSums(m), decreasing=TRUE)
# word cloud
set.seed(500)
# to make it reproducible
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=30, random.order=F, colors=grayLevels)

# ##get the names of the 10 words that correlate the highest with reach
# words <- rownames(findAssocs(DTM, 'reach', .005))[1:20]
# find <- colnames(dtm) %in% words
# corr <- cor(dtm[,find])
# #plot heatmap of correlations
# corrplot(corr, type = "upper")
# 
# ##get the names of the 10 words that correlate the highest with reach
# words <- rownames(findAssocs(DTM, 'reach', .005))[1:20]
# find <- colnames(dtm) %in% words
# corr <- cor(dtm[,find])
# #plot heatmap of correlations
# corrplot(corr, type = "upper")
# 
# ##get the names of the 10 words that correlate the highest with house
# words <- row.names(findAssocs(DTM, house, .005))
# find <- colnames(dtm) %in% words
# corr <- cor(dtm[,find])
# #plot heatmap of correlations
# owncorrplot(corr, type = "upper")

# remove sparse terms 
TDM2 <- removeSparseTerms(TDM, sparse=0.95)
tdm2 <- as.matrix(TDM2)
head(tdm2)
tdm_short2 <- tdm[names(tail(sort(rowSums(tdm2)), 20)), ]
# cluster terms
distMatrix <- dist(scale(tdm_short2))
fit <- hclust(distMatrix, method="ward.D")
plot(fit)
# cut tree into 10 clusters
rect.hclust(fit, k=10) > (groups <- cutree(fit, k=10))

# Word2VecAPI <- folderpath  
# build <- paste("q=", "build", sep="")
# url <- paste(Word2VecAPI, build, sep="")
# # loading data into R 
# mydata <- read.table(url, header=T, sep=",", row.names=1)
# 
# 
# # K-Means Clustering with 7 clusters
# fit <- kmeans(mydata, 7)
# # Cluster Plot against 1st 2 principal components
# clusplot(mydata, fit$cluster, color=TRUE, shade=FALSE, labels=2, lines=0, main='K-Means on Word2Vec')

# transpose the matrix to cluster documents (tweets)
tdm3 <- t(tdm2)
# set a fixed random seed
set.seed(122)
# k-means clustering of tweets
k <- 8
kmeansResult <- kmeans(tdm3, k)
# cluster centers
round(kmeansResult$centers, digits=3)
  


# partitioning around medoids with estimation of number of clusters
tdm3 <- t(tdm2)
pamResult <- pamk(tdm3, metric="manhattan")
# number of clusters identified
(k <- pamResult$nc)
pamResult <- pamResult$pamobject
# print cluster medoids
for (i in 1:k) { 
  + cat(paste("cluster", i, ": ")) 
  + cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n") 
  # + print tweets in cluster i 
  # + print(rdmTweets[pamResult$clustering==i]) 
}

# plot clustering result 
layout(matrix(c(1,2),2,1))
# set to two graphs per page
plot(pamResult, color=F, labels=4, lines=0, cex=.8, col.clus=1)
layout(matrix(1)) # change back to one graph per page