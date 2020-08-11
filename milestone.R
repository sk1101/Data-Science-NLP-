##################################################################
## task0
library(tm)
library(slam)
engblog<-file("C:\\Users\\Shrishti\\Downloads\\Coursera-SwiftKey\\final\\en_US\\en_US.blogs.txt","r")
enb<-readLines(engblog,encoding="UTF-8")
close.connection(engblog)
engnews<-file("C:\\Users\\Shrishti\\Downloads\\Coursera-SwiftKey\\final\\en_US\\en_US.news.txt","r")
enn<-readLines(engnews,encoding="UTF-8")
close.connection(engnews)
engtwitter<-file("C:\\Users\\Shrishti\\Downloads\\Coursera-SwiftKey\\final\\en_US\\en_US.twitter.txt","r")
ent<-readLines(engtwitter,encoding="UTF-8")
close.connection(engtwitter)

df<-data.frame(files=c("Blogs","News","Twitter"),length=c(length(enb),length(enn),length(ent)))
df

#     files  length
# 1   Blogs  899288
# 2    News   77259
# 3 Twitter 2360148

## we'll take a total of 90,000 lines and since twitter has the most number of elements we'll take 45,000 
##from there  and 30,000 from blogs and 15,000 from news.
set.seed(123)
sam_data<-sample(ent,45000)
sam_data<-c(sam_data,sample(enb,30000,replace = FALSE))
sam_data<-c(sam_data,sample(enn,15000,replace=FALSE))
sam_data_cor<-Corpus(VectorSource(sam_data))


# convert to lower case
mydata <- tm_map(sam_data_cor, content_transformer(tolower))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
mydata <- tm_map(mydata, content_transformer(removeNumPunct))
# remove stopwords
mydata <- tm_map(mydata, removeWords, stopwords("english"))
# remove extra whitespace
mydata <- tm_map(mydata, stripWhitespace)
# Remove numbers
mydata <- tm_map(mydata, removeNumbers)
# Remove punctuations
mydata <- tm_map(mydata, removePunctuation)

#sam_data_tdm<-TermDocumentMatrix(mydata)
#sam_data_tdm2<-DocumentTermMatrix(mydata)

##################################################################

##################################################################
## task1

### Tokenization
library(tokenizers)


tokenwords<-function(x){
  y<-tokenize_words(x)
  yu<-unlist(y,recursive = FALSE)
  yu
}

tokesentence<-function(x){
  y<-tokenize_sentences(x)
  yu<-unlist(y,recursive = FALSE)
  yu
}

tokenchar<-function(x){
  y<-tokenize_characters(x)
  yu<-unlist(y,recursive = FALSE)
  yu
}

df2<-data.frame(Source=c("Blogs","Twitter","News"),Characters=c(length(tokenchar(sample(enb,30000))),
                                                                length(tokenchar(sample(ent,45000))),
                                                                length(tokenchar(sample(enn,15000)))),
                Words=c(length(tokenwords(sample(enb,30000))),
                        length(tokenwords(sample(ent,45000))),
                        length(tokenwords(sample(enn,15000)))),
                Sentences=c(length(tokesentence(sample(enb,30000))),
                            length(tokesentence(sample(ent,45000))),
                            length(tokesentence(sample(enn,15000)))))
df2
#   Source Characters Words Sentences
#1   Blogs        457 54991     78122
#2 Twitter        338 37168     68495
#3    News         85 38062     29931
## removing the bad words
## link: "https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/blob/master/en"

badwords<-file("C:\\Users\\Shrishti\\Downloads\\Coursera-SwiftKey\\final\\badwords.txt","r")
profan<-readLines(badwords)
close.connection(badwords)
sd2<-tm_map(mydata,removeWords,profan)
sam_data_tdm22<-DocumentTermMatrix(sd2)

## sd2 is my sample corpus object
## sam_data_tsm22 is my sample dtm 
##################################################################

##################################################################
## task2
library(plotly)
csd2<-col_sums(sam_data_tdm22) ## slam package 
max(csd2)
## [1] 7207
csd_sort<-sort(csd2,decreasing=TRUE)
Words=names(csd_sort[1:20])
Frequency=csd_sort[1:20]
data1 <- data.frame(Words, Frequency, stringsAsFactors = FALSE)
data1$Words <- factor(data1$Words, levels = unique(data1$Words)[order(data1$Frequency, decreasing = FALSE)])
plot_ly(data1,x=~Frequency,y=~Words,type = "bar")

csd_sort[1:10]
#  will just  one like  can said  get time  new  now 
#  7207 6996 6927 6212 5811 5277 5100 5082 4301 4103


t=get("content",mydata)
t2gram<-tokenize_ngrams(t,n=2,n_min=2)
t2gramfin<-unlist(t2gram)
#t2gramuniq<-unique(t2gramfin)
t2ngramtable<-table(t2gramfin)
t2ngram_sort<-sort(t2ngramtable,decreasing = TRUE)
Tokens_2word<-names(t2ngram_sort[1:10])
freq_2token<-t2ngram_sort[1:10]
t2ngram_df<-data.frame(Phrases=Tokens_2word,Frequency=freq_2token)
plot_ly(t2ngram_df,x=~Frequency.Freq, y=~Phrases, type="bar")


t3gram<-tokenize_ngrams(t,n=3,n_min=3)
t3gramfin<-unlist(t3gram)
#t3gramuniq<-unique(t3gramfin)
t3ngramtable<-table(t3gramfin)
t3ngram_sort<-sort(t3ngramtable,decreasing = TRUE)
Tokens_3word<-names(t3ngram_sort[1:10])
freq_3token<-t3ngram_sort[1:10]
t3ngram_df<-data.frame(Phrases=Tokens_3word,Frequency=freq_3token)
plot_ly(t3ngram_df,x=~Frequency.Freq, y=~Phrases, type="bar")

ans<-cumsum(csd_sort)/sum(csd_sort)
ans_fin<-c(ans, use.names=FALSE)
which(ans_fin>=0.5 & ans_fin<=0.5001)
# 963
which(ans_fin>=0.9 & ans_fin<=0.900005)
# 15996
##################################################################

##################################################################
## task3

# modelData <- data.frame(rawtext = sapply(mydata, as.character), stringsAsFactors=FALSE)
# modelData$textLines <- iconv(modelData$rawtext, 'UTF-8', 'ASCII')
# gram3model<-tokenize_ngrams(modelData$textLines,n=3,n_min = 3)
# gram3modeltable<-table(unlist(gram3model))
# final_3gram<- data.frame(gram3modeltable)
# head(final_3gram)

word1fun<-function(input){
  word1<-strsplit(input," ")[[1]][1]
  word1
}
word2fun<-function(input){
  word2<-strsplit(input," ")[[1]][2]
  word2
}
word3fun<-function(input){
  word3<-strsplit(input," ")[[1]][3]
  word3
}

gram3_model<-data.frame(t3ngramtable)
gram3_model$t3gramfin<-as.character(gram3_model$t3gramfin)
gram3_model$word1<-sapply(gram3_model$t3gramfin,word1fun)
gram3_model$word2<-sapply(gram3_model$t3gramfin,word2fun)
gram3_model$word3<-sapply(gram3_model$t3gramfin,word3fun)
write.csv(gram3_model,"C:\\Users\\Shrishti\\Downloads\\Coursera-SwiftKey\\final\\gram3_model.csv", row.names = FALSE)

gram2_model<-data.frame(t2ngramtable)
gram2_model$t2gramfin<-as.character(gram2_model$t2gramfin)
gram2_model$word1<-sapply(gram2_model$t2gramfin,word1fun)
gram2_model$word2<-sapply(gram2_model$t2gramfin,word2fun)
write.csv(gram2_model,"C:\\Users\\Shrishti\\Downloads\\Coursera-SwiftKey\\final\\gram2_model.csv", row.names = FALSE)

gram1_model<-data.frame(words=names(csd_sort),freq=csd_sort,stringsAsFactors = FALSE)
write.csv(gram1_model,"C:\\Users\\Shrishti\\Downloads\\Coursera-SwiftKey\\final\\gram1_model.csv", row.names = FALSE)

# unigram_prediction<-function(inputword){
#   inputword=as.character(inputword)
#   c2<-gram2_model$Freq[which(gram2_model$word1==inputword)]
#   c2names<-gram2_model$word2[which(gram2_model$word1==inputword)]
#   probdf<-data.frame(Prediction=c2names, freq2=c2, stringsAsFactors = FALSE)
#   sumc2<-sum(c2)
#   probdf$prob<-probdf$freq/sumc2
#   finalans<-probdf$Prediction[which.max(probdf$prob)]
#   return(finalans)
# }
# 
# bigram_prediction<-function(inputword1, inputword2){
#   inputword1=as.character(inputword1)
#   inputword2=as.character(inputword2)
#   feqs<-gram3_model$Freq[which(gram3_model$word1==inputword1 & gram3_model$word2==inputword2)]
#   feqnames<-gram3_model$word3[which(gram3_model$word1==inputword1 & gram3_model$word2==inputword2)]
#   bprobdf<-data.frame(Predword=feqnames,freqq=feqs, stringsAsFactors = FALSE)
#   sumfeq<-sum(feqs)
#   bprobdf$probab<-bprobdf$freqq/sumfeq
#   finalpred<-bprobdf$Predword[which.max(bprobdf$probab)]
#   return(finalpred)
# }
# 
# Predictnextword<-function(Inputline){
#   Text <- tolower(Inputline)
#   textInput <- removePunctuation(Text)
#   textInput <- removeNumbers(textInput)
#   textInput <- str_replace_all(textInput, "[^[:alnum:]]", " ")
#   textInput <- stripWhitespace(textInput)
#   textInput <- txt.to.words.ext(textInput, preserve.case = TRUE)
#   lengthofinput<-length(textInput)
#   suggestion=c()
#   if(lengthofinput>1)
#   {
#     wordInput <- textInput[(lengthofinput-1):lengthofinput]
#     suggestion <- bigram_prediction(wordInput[1],wordInput[2])
#     if(length(suggestion)==0)
#     {
#       suggestion<-unigram_prediction(wordInput[2])
#       if(length(suggestion)==0)
#       {
#         bvgh<-sample(1:100,1)
#         suggestion=names(csd_sort[bvgh])
#       }
#     }
#   }
#   if(lengthofinput==1)
#   {
#     suggestion<-unigram_prediction(textInput)
#     if(length(suggestion)==0)
#     {
#       bvgh<-sample(1:100,1)
#       suggestion=names(csd_sort[bvgh])
#     }
#   }
#   if(lengthofinput==0)
#   {
#     suggestion<-"Enter Text!"
#   }
#   return(suggestion)
# }