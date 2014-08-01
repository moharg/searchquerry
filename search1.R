search1<-read.csv("100Ksample_site_searches.csv")
search<-search1
search<-search[,which(names(search)%in%c('full_string','ip','timestamp'))]


a<-sub(".*?search_string=(.*?)&.*", "\\1", search$full_string)
# b<-gsub("%2520", " " ,a)
# b<-gsub("%2522", " " ,b)
# b<-gsub("%252", " " ,b)
# b<-gsub("%253", " " ,b)
# b<-gsub("%2526", " " ,b)
# b<-gsub("%20", " " ,b)
# b<-gsub("%40", " " ,b)
# b<-gsub("%92", " " ,b)
# b<-gsub("%22", " " ,b)
# b<-gsub("%27", " " ,b)
# b<-gsub("%2", " " ,b)
# b<-gsub("%3", " " ,b)
# b<-gsub("%5", " " ,b)
# b<-gsub("%D1%85%D1%86%D0%B3", " " ,b)
c<- gsub("%2520|%2522|%252|%253|%2526|%20|%40|%92|%22|%27|%2|%3|%5|%D1%85%D1%86%D0%B3"," ",a)
#csub<-c[grep("%",c)] #check if the string contains any %
search$full_string<-c

#Convert to epoch time and create time intervals every "nmin" minutes
nmin<-4
search$timestamp<- unlist(strsplit(gsub("T"," ",search$timestamp),".",fixed=TRUE))[2*(1:length(search$timestamp))-1]

search$timestamp<-as.POSIXct(strptime(search$timestamp, "%Y-%m-%d %H:%M:%S"))
sum(is.na(search$timestamp))
search$timeint<-unclass(search$timestamp)/(nmin*60)
search$identifier<-apply(search,1,function(x) paste(x[2],x[4],sep=" ; "))
combine<-aggregate(full_string~identifier,search,function(x) paste(x, collapse = " ; ")) # combine by time stamp and ip
combine$fs <- sapply(combine$full_string,function(x) paste(unique(strsplit(x," ")[[1]]),collapse=" ") )
combine$fs <- gsub("pregnant","pregnancy",combine$fs)

k<- which(names(combine$fs)=="pregnant")
names(v)[k]<- "pregnancy"



######## Frequency Chart ##########
library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)



a<-Corpus(DataframeSource(data.frame(combine$fs)))

a <- tm_map(a, content_transformer(tolower))
#a <- tm_map(a,replaceExpressions,useMeta=FALSE)
a <- tm_map(a, removePunctuation)
mystopwords<- c('know','feel','high','normal','and','with','will','what','lower','test','day','weeks','time','right','left','periods','now','months','low','causes','bleeding','every','really','use','one','rid','old','today','side','red','make','inside','way','without','days','just','pain','will','can','like','get','taking','take','mean','long','cause',";")

a <- tm_map(a, function(x) removeWords(x, c(stopwords("english"),mystopwords)))

# # Stemming
# adict<-a
# a<-tm_map(adict,stemDocument,language="english")
# corpus <- tm_map(a,stemCompletion,dictionary=adict)

corpus<- a
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
# k<- which(names(v)=="pregnant")
# names(v)[k]<- "pregnancy"
d <- data.frame(word = names(v),freq=v)
d<-subset(d,d$freq>220)
n<- 25
bard<-d[1:n,]
##### Plot sample size frequency #####
png("topfreq.png", width=1000,height=800)
ylim<- c(0,1.1*max(bard$freq))
xx<-barplot(bard$freq,xaxt='n',xlab='',width=0.85,ylim=ylim,main="Top 25 Frequently Appearing Terms \n in HT Queries between Jun 12-Jul 26",ylab="Frequency")
text(x=xx,y=bard$freq,label=bard$freq,pos=3,cex=0.6,col="red")
axis(1,at=xx,labels=bard$word,tick=FALSE,las=2,line=-0.5,cex.axis=0.9)
dev.off()
######

png("freqword.png", width=1000,height=800)
qplot(x=d$word,y=as.numeric(d$freq),geom="bar",stat="identity")+coord_flip()+xlab("Words")+ylab("Frequency")+ggtitle("Frequency Distribution of Words (freq >220)")
dev.off()

#####Wordcloud
png("wordcloudq.png", width=1280,height=800)
wordcloud(d$word,d$freq, scale=crequir(8,.3),min.freq=2,max.words=100, random.order=FALSE, rot.per=.15, colors=brewer.pal(9, "Dark2"), vfont=c("sans serif","plain"))
dev.off()

#### Clustering
findFreqTerms(tdm, lowfreq=30)
findAssocs(tdm,'pregnancy',0.1)
head(inspect(removeSparseTerms(tdm,0.2)))
##hclust ###
distMatrix <- dist(scale(m))
fit<- hclust(distMatrix,method="ward")



