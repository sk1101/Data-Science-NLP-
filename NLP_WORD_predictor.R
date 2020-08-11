library(tm)
library(slam)
library(stringr)
library(stylo)
gram1_model<-read.csv("gram1_model.csv",stringsAsFactors = FALSE)
gram2_model<-read.csv("gram2_model.csv",stringsAsFactors = FALSE)
gram3_model<-read.csv("gram3_model.csv",stringsAsFactors = FALSE)

unigram_prediction<-function(inputword){
  inputword=as.character(inputword)
  c2<-gram2_model$Freq[which(gram2_model$word1==inputword)]
  c2names<-gram2_model$word2[which(gram2_model$word1==inputword)]
  probdf<-data.frame(Prediction=c2names, freq2=c2, stringsAsFactors = FALSE)
  sumc2<-sum(c2)
  probdf$prob<-probdf$freq/sumc2
  finalans<-probdf$Prediction[which.max(probdf$prob)]
  return(finalans)
}

bigram_prediction<-function(inputword1, inputword2){
  inputword1=as.character(inputword1)
  inputword2=as.character(inputword2)
  feqs<-gram3_model$Freq[which(gram3_model$word1==inputword1 & gram3_model$word2==inputword2)]
  feqnames<-gram3_model$word3[which(gram3_model$word1==inputword1 & gram3_model$word2==inputword2)]
  bprobdf<-data.frame(Predword=feqnames,freqq=feqs, stringsAsFactors = FALSE)
  sumfeq<-sum(feqs)
  bprobdf$probab<-bprobdf$freqq/sumfeq
  finalpred<-bprobdf$Predword[which.max(bprobdf$probab)]
  return(finalpred)
}

Predictnextword<-function(Inputline){
  Text <- tolower(Inputline)
  textInput <- removePunctuation(Text)
  textInput <- removeNumbers(textInput)
  textInput <- str_replace_all(textInput, "[^[:alnum:]]", " ")
  textInput <- stripWhitespace(textInput)
  textInput <- txt.to.words.ext(textInput, preserve.case = TRUE)
  lengthofinput<-length(textInput)
  suggestion=c()
  if(lengthofinput>1)
  {
    wordInput <- textInput[(lengthofinput-1):lengthofinput]
    suggestion <- bigram_prediction(wordInput[1],wordInput[2])
    if(length(suggestion)==0)
    {
      suggestion<-unigram_prediction(wordInput[2])
      if(length(suggestion)==0)
      {
        bvgh<-sample(1:100,1)
        suggestion=gram1_model$words[bvgh]
      }
    }
  }
  if(lengthofinput==1)
  {
    suggestion<-unigram_prediction(textInput)
    if(length(suggestion)==0)
    {
      bvgh<-sample(1:100,1)
      suggestion=gram1_model$words[bvgh]
    }
  }
  if(lengthofinput==0)
  {
    suggestion<-"Enter Text!"
  }
  return(suggestion)
}






