# --- extract data from Twitter -----------------
library(twitteR)


consumer_key <- "Y9hftAUYKVJLMs21LleW1rZXq"
consumer_secret <- "xpZEQkW3jOVCF7stu7NoL2VQGIK7sOMGpxfHcBxK1h4DwKZ8JZ"
access_token <- "120467600-cDAAr6LrpStRN3IsBMmSsGZhoWpiSiZy18q1GYeN"
access_secret <- "UwVkFbQcUrcGizV8euPZ1sa8O77YoqM23U4hbEhwyt3JM"



setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)
NASA=userTimeline("NASA",n=200)
NASA[1:3]
tweets.df=twListToDF(NASA)
NASA_text=sapply(NASA,function(x) x$getText())


# --- clean extracted data and build a document-term matrix -----------------
library(tm)

# build a corpus, and specify the source to be character vectors
NCorpus <- Corpus(VectorSource(NASA_text))
inspect(NCorpus[1])

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
NCorpus <- tm_map(NCorpus, content_transformer(removeURL))

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
NCorpus <- tm_map(NCorpus, content_transformer(removeNumPunct))

# remove punctuation
NCorpus <- tm_map(NCorpus, removePunctuation)

# convert to lower case
NCorpus <- tm_map(NCorpus, content_transformer(tolower))# apply transformation functions (also denoted as mappings) to corpora

# remove stopwords from corpus
NCorpus <- tm_map(NCorpus, removeWords, stopwords("English"))

# remove numbers
NCorpus <- tm_map(NCorpus, removeNumbers)

# remove extra whitespace
NCorpus <- tm_map(NCorpus, stripWhitespace)

# remove searched words
NCorpus <- tm_map(NCorpus, removeWords, "nasa")

library(wordcloud)
wordcloud(NCorpus,random.order=F,scale=c(3,0.5),col=rainbow(50))

# Constructs a term-document matrix or a document-term matrix
tdm <- DocumentTermMatrix(NCorpus)
mat=as.matrix(tdm)

x=NULL
row.name=rownames(mat)
col.name=colnames(mat)
for(i in 1:length(row.name)){
  for(j in 1:length(col.name)){
    if(mat[i,j] != 0){
      x=rbind(x,c(col.name[j],row.name[i]))
    }   
  }
}

data <- data.frame(x)
colnames(data)=c("Word","Occurrence")
library(widyr)

a=pairwise_count(data, Word, Occurrence, sort = TRUE)
a
tdma=as.DocumentTermMatrix(tdm)
library(topicmodels)
lda <- LDA(tdma, k = 8) # create 8 topic-topic LDA model.It returns an object containing the full details of the model fit, such as how words are associated with topics and how topics are associated with documents.
term <- terms(lda, 6) # first 6 terms of every topic
term <- apply(term, MARGIN = 2, paste, collapse = ", ")
# first topic identified for every document (tweet)
topic <- topics(lda, 1)
topics <- data.frame(date=as.Date(tweets.df$created), topic)
qplot(date, ..count.., data=topics, geom="density",
      fill=term[topic], position="stack")

tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)

# 1. hierarchical cluster analysis
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward")
plot(fit)
rect.hclust(fit, k = 6) # cut tree into 6 clusters

# 2. K-means
m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k <- 6 # number of clusters

kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers
for (i in 1:k){
  cat(paste("cluster ", i, ": ", sep = ""))
  s <- sort(kmeansResult$centers[i, ], decreasing = T)
  cat(names(s)[1:5], "\n")
  # print the tweets of every cluster
  # print(tweets[which(kmeansResult$cluster==i)])
}

# choose K 

cost_df <- data.frame()#accumulator for cost results
#run kmeans for all clusters up to 50
for(i in 1:50){
  #Run kmeans for each level of i, allowing up to 50 iterations for convergence
  kmeans<- kmeans(x=tdm, centers=i, iter.max=50)
  #Combine cluster number and cost together, write to df
  cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
  
}

names(cost_df) <- c("cluster", "cost")

cost_df

