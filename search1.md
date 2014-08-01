---
title: "Query Analysis"
author: "Mohar"
date: "July 29, 2014"
output: html_document
---

```r
library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
```


This  is a report on text analysis of queries on Healthtap over the past two months. We will look at the attributes *full_string* , *ip* and *timestamp* for this analysis.



```r
search1<-read.csv("100Ksample_site_searches.csv")
search<-search1
search<-search[,which(names(search)%in%c('full_string','ip','timestamp'))]
```

The attribute "full_string" is the full search querry which should be processed.


```r
a<-sub(".*?search_string=(.*?)&.*", "\\1", search$full_string)
c<- gsub("%2520|%2522|%252|%253|%2526|%20|%40|%92|%22|%27|%2|%3|%5|%D1%85%D1%86%D0%B3"," ",a)
search$full_string<-c
```

Convert timestamp to to epoch time and create time intervals every "n" minutes


```r
nmin<-4
search$timestamp<- unlist(strsplit(gsub("T"," ",search$timestamp),".",fixed=TRUE))[2*(1:length(search$timestamp))-1]
search$timestamp<-as.POSIXct(strptime(search$timestamp, "%Y-%m-%d %H:%M:%S"))
#sum(is.na(search$timestamp))
search$timeint<-unclass(search$timestamp)/(nmin*60)
```

We combine the records from same time interval and same ip address.


```r
search$identifier<-apply(search,1,function(x) paste(x[2],x[4],sep=" ; "))
combine<-aggregate(full_string~identifier,search,function(x) paste(x, collapse = " ; ")) # combine by time stamp and ip
combine$fs <- sapply(combine$full_string,function(x) paste(unique(strsplit(x," ")[[1]]),collapse=" ") )
combine$fs <- gsub("pregnant","pregnancy",combine$fs)
```

The number of records reduces from 100000 to 37455. Now we preprocess each querry and find the most frequent words.


```r
a<-Corpus(DataframeSource(data.frame(combine$fs)))
a <- tm_map(a, content_transformer(tolower))
a <- tm_map(a, removePunctuation)
mystopwords<- c('high','normal','lower','test','day','weeks','time','right','left','periods','now','months','low','causes','bleeding','every','really','use','one','rid','old','today','side','red','make','inside','way','without','days','just','pain','will','can','like','get','taking','take','mean','long','cause',";")
a <- tm_map(a, function(x) removeWords(x, c(stopwords("english"),mystopwords)))
corpus<- a
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d1<-d
d<-subset(d,d$freq>220)
n<- 25
bard<-d[1:n,]
```
The following figure shows the word distribution with frequency greater than 200,

```r
qplot(x=d$word,y=as.numeric(d$freq),geom="bar",stat="identity")+coord_flip()+xlab("Words")+ylab("Frequency")+ggtitle("Frequency Distribution of Words (freq >220)")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

and the top 15 frequently appearing terms are as follows:

```r
ylim<- c(0,1.1*max(bard$freq))
xx<-barplot(bard$freq,xaxt='n',xlab='',width=0.85,ylim=ylim,main="Top 25 Frequently Appearing Terms \n in HT Queries between Jun 12-Jul 26",ylab="Frequency")
text(x=xx,y=bard$freq,label=bard$freq,pos=3,cex=0.6,col="red")
axis(1,at=xx,labels=bard$word,tick=FALSE,las=2,line=-0.5,cex.axis=0.9)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


