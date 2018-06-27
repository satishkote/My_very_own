# Required Libraries to be installed 
install.packages("tm")
library(tm)
library(ggplot2)
 library(RColorBrewer)
library(RCurl)
library(RJSONIO)
library(coreNLP)
library(stringr)
library(ROAuth)
library(plyr)
library(reshape2)
library(wordcloud)
library(RDRPOSTagger)
library(tokenizers)

#--------Step 1 Cleaing Data [Removing numbers, special characters, white spaces, symbols etc]--------


#loading Data
data_comments<- read.csv("D:/Sentimate Analysis/Reviews.csv", stringsAsFactors = FALSE)
head(Stopwords)

#Text cleaning using Gsub Function
comments.df <- gsub("http.*","",data_comments$text)  
comments.df <- tolower(comments.df) 
comments.df <- gsub("https.*","",comments.df)
comments.df<- gsub("[[:digit:]]", "",comments.df) 
comments.df<- gsub("[[:punct:]]", "",comments.df)

#to remove english stopwords
#stop_pattern <- paste0("\\b(", paste0(merged_Negation_Negative$combined, collapse="|"), ")\\b")
#stop_pattern <- paste0("\\b",Stopwords, collapse="|","\\b")

#converting dataframe to vectore
comments.vc<- as.vector(comments.df)
comments.vc <- cbind(data_comments$brand, comments.vc)
colnames(comments.vc)[1] <- "brand"
colnames(comments.vc)[2] <- "text"
View(comments.vc)

#write.csv(stop,"D:/Sentimate Analysis/stop.csv")
#creating csv from vectore
#write.csv(comments.vc,"D:/Sentimate Analysis/Cleaned_Data.csv")

#--------Step 2 Getting list bigrams to be replaced with single word ------------ 
# we can also consider tribram and fourgram for more possibilities.
# Loading cleaned data and dictionary into data frame
data <- read.csv("D:/Final Sentiment Analysis/Cleaned_Data.csv", stringsAsFactors = FALSE)
Dictionary <- read.csv("D:/Final Sentiment Analysis/Dictionary.csv", stringsAsFactors = FALSE)


#changing class to data frame
class(Dictionary)
nrc <- as.data.frame(nrc)
head(nrc)
class(data)
data <- as.data.frame(data)
class(data)


#Using ngrams tokenization into bigrams
bigram_data<- data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

#changing coloumn names for standardization
colnames(bigram_data)[colnames(bigram_data)=="bigram"] <- "word"
View(bigram_data)
View(bigram_with_not)

count_data <- bigram_data %>%
  count(bigram_data, sort = TRUE)

#Analysis Logic
View(nrc)

#Inner join to to extract each sentiment words from each text with sentiment
sentiment_Words<-inner_join(bigram_data, Dictionary) 

#merge function to match the bigram with dictionary
merged_data <- merge(bigram_data,Dictionary, by = "word")
View(merged_data)

View(sentiment_Words)

bigrams_separated <- bigram_data %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#pairs with negation stopword + sentiment words[both positive and negative]
bigram_data <- bigrams_separated %>%
  filter(word1 %in% c("very","too","significantly","really","extremely","substantially","hasnt","not","wasnt","arent","cant","couldnt","didnt","doesnt","dont","havent","isnt","non","no","shouldnt","wasnt","wouldnt","wont","without")) %>%
  filter(word2 %in% Dictionary$word ) %>%
  count(word1, word2, sort = TRUE)
bigram_data$bigram <- paste(bigram_data$word1,"",bigram_data$word2)
View(bigram_data)

colnames(bigram_with_negation)[colnames(bigram_with_negation)=="word2"] <- "word"
bigram_with_negation <- merge(bigram_with_negation,Dictionary, by = "word")
View(bigram_with_negation)
bigram_with_negation <- bigram_with_negation[c(2,1,4,3)]
colnames(bigram_with_negation)[colnames(bigram_with_negation)=="word"] <- "word2"

#Separating bigrams with negation according to the senitment[positive or negative]
Negation_Positive <- filter(bigram_with_negation, sentiment=="positive")
Negation_Positive$combined <- paste(Negation_Positive$word1," ", Negation_Positive$word2)

#Final list of bigrams with negation stopword + positive senitment word
Negation_Positive$sentiment <- NULL
Negation_Positive <- Negation_Positive[c(2,1)]
colnames(Negation_Positive)[colnames(Negation_Positive)=="test"] <- "to_replace"
test <- Negation_Positive
test <- replace(test$replace, grepl("positive", test,perl = TRUE ),"negative")
Negation_Positive <- cbind(Negation_Positive, test)
Final_Negation_Positive <- Negation_Positive
View(Final_Negation_Positive)

#Getting finel list of bigrams with negatio stopword + negative senitment word
Negation_Negative <- filter(bigram_with_negation, sentiment=="negative")
Negation_Negative$combined <- paste(Negation_Negative$word1," ", Negation_Negative$word2) 
Negation_Negative$com_pos <- paste(Negation_Negative$word1," ", Negation_Negative$word2)
Negation_Negative$sentiment <- NULL
Negation_Negative <- Negation_Negative[c(2,1)]
colnames(Negation_Negative)[colnames(Negation_Negative)=="test"] <- "to_replace"
to_replace <- Negation_Negative$sentiment
to_replace<- replace(to_replace, grepl("negative", test,perl = TRUE ),"positive")
Negation_Negative <- cbind(Negation_Negative, to_replace)
Final_Negation_Negative <- Negation_Negative
View(Final_Negation_Negative)

#pairs with positive stopword + sentiment words[both positive and negative]
bigram_with_positive_Stopword <- bigrams_separated %>%
  filter(word1 %in% c("very","too","significantly","really","extremely","substantially")) %>%
  filter(word2 %in% Dictionary$word ) %>%
  count(word1, word2, sort = TRUE)
View(bigram_with_positive_Stopword)


colnames(bigram_with_positive_Stopword)[colnames(bigram_with_positive_Stopword)=="word2"] <- "word"
bigram_with_positive_Stopword <- merge(bigram_with_positive_Stopword,Dictionary, by = "word")
bigram_with_positive_Stopword <- bigram_with_positive_Stopword[c(2,1,4,3)]
colnames(bigram_with_positive_Stopword)[colnames(bigram_with_positive_Stopword)=="word"] <- "word2"
View(bigram_with_positive_Stopword)

#Separating bigrams with negation according to the senitment[positive or negative]
pos_stop_positive <- filter(bigram_with_positive_Stopword, sentiment=="positive")
pos_stop_positive$com_pos <- paste(pos_stop_positive$word1," ", pos_stop_positive$word2)

#Final list of bigrams with negation stopword + positive senitment word
pos_stop_positive$sentiment<- NULL
pos_stop_positive <- pos_stop_positive[c(2,1)]
colnames(pos_stop_positive)[colnames(pos_stop_positive)=="com_pos"] <- "combined"
to_replace <- pos_stop_positive
to_replace <- replace(to_replace$replace, grepl("positive", to_replace,perl = TRUE ),"strong_positive")
pos_stop_positive <- cbind(pos_stop_positive, to_replace)
View(pos_stop_positive)

#Getting finel list of bigrams with negatio stopword + negative senitment word
pos_stop_negative <- filter(bigram_with_positive_Stopword, sentiment=="negative")
pos_stop_negative$combined <- paste(pos_stop_negative$word1," ", pos_stop_negative$word2) 
pos_stop_negative$sentiment <- NULL
pos_stop_negative <- pos_stop_negative[c(2,1)]
colnames(pos_stop_negative)[colnames(pos_stop_negative)=="to_replace"] <- "to_replace"
to_replace <- pos_stop_negative$sentiment
to_replace<- replace(to_replace, grepl("negative", to_replace,perl = TRUE ),"strong_negative")
pos_stop_negative <- cbind(pos_stop_negative, to_replace)
View(pos_stop_negative)


#Final Bigram list with new word to be replaced with
final_bigram_list <- rbind(Final_Negation_Negative, Final_Negation_Positive, pos_stop_negative, pos_stop_positive )
View(final_bigram_list)
rm(final_bigram_list)
final_bigram_list <- as.data.frame(final_bigram_list)
#write.csv(final_bigram_list, "D:/Final Sentiment Analysis/final_bigram_list.csv")


#------- Step 3 Replace bigrams with single words ------------
#Loading cleaned data and bigram list to be replaced
data1<-read.csv("D:/Final Sentiment Analysis/Cleaned_Data.csv",header = T)
data2<- read.csv("D:/Final Sentiment Analysis/final_bigram_list.csv",header = T)

colnames(data1)
unique(data1$text)
ab<-c("I am a Boy")
gsub("Boy", "Girl", ab)

# To create a coloumn named Ntext with replaced bigrams[]
data1$Ntext<-gsub("not bad","positive",data1$text)

# Creating two objects one for bigram and another for single word to be replaced with
nVal1<-data2$combined
nval2<-data2$to_replace
ncol<-length(nVal1)

# Looping until each bigram gets replaced
for(i in 1:ncol) {
  print(nVal1[i])
  data1$Ntext<-gsub(nVal1[i],nval2[i],data1$Ntext)
}

# Saving final data with replaced bigrams
#write.csv(data1,"D:/Final Sentiment Analysis/Final_Dataset.csv")


#----------- Step 4 Visualizatoin using compariosn cloud and Bar plot --------

# Loading data
text <- read.csv("D:/Final Sentiment Analysis/Olay.csv", stringsAsFactors = FALSE)
bing <- read.csv("D:/Final Sentiment Analysis/Dictionary.csv", stringsAsFactors = FALSE)

#Tokenizing sentence into words

tidy_data <- text  %>%
  unnest_tokens(word, word)


#comparision cloud with reshape

library(reshape2)
tidy_data %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 150)

#Bar Graph for sentiment words frequency
sentiment_data<-inner_join(tidy_data, Dictionary)   %>% 
  count(word, sentiment,sort = TRUE)   %>%
  ungroup()

sentiment_data %>%
  filter(n > 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")


#-------- step 5 Calculating sentiment score ------------
#Step 1 : Scan the cleaned data with replaced bigrams into single word into R
Review <- read.csv("D:/Final Sentiment Analysis/Final_dataset.csv", stringsAsFactors = FALSE)
#Dictionary <- read.csv("D:/Final Sentiment Analysis/Dictionary.csv", stringsAsFactors = FALSE)

#Step 2 : Scan the positive and negative words into R
neg.word<-scan("D:/Sentimate Analysis/Negative.csv",what = 'character')
pos.words<-scan("D:/Sentimate Analysis/Positive.csv",what = 'character')
reviews<- Review$word
#noof_reviews = length(reviews)

#Step 3 to create funtion for sentiment analysis
#Function to get Score
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of positive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  # create a simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

#Now, we can start processing the reviews to calculate the sentiment score.

scores =score.sentiment(reviews, pos.words,neg.word , .progress='text')
View(scores)

#Step 4 - Calculate positive, negative and neutral sentiments.
scores$positive <- as.numeric(scores$score >0)
scores$negative <- as.numeric(scores$score >0)
scores$neutral <- as.numeric(scores$score==0)

#Step 5 Visualization
summary(scores)
hist(scores$score,col ="yellow", main ="Score of Reviews", ylab = " Count of Reviews")
count(scores$negative)

#plots for Polarity
scores$polarity <- ifelse(scores$score >1,"positive",ifelse(scores$score < 1,"negative",ifelse(scores$score==1,"Neutral",0)))
qplot(factor(polarity), data=scores, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Customer Sentiments")

write.csv(olay_score, "D:/Sentimate Analysis/olay_scores.csv")


#--------- Step 6 parts of speech to get driving factors and visualization ---------
# Load cleaned data [original data without replacing bigrams into single word]
Review <- read.csv("D:/Final Sentiment Analysis/Olay_positive.csv", stringsAsFactors = FALSE)
Review <- as.character(Review$text)

# Loading English POS using unipostagger library
unipostagger <- rdr_model(language = "English", annotation = "UniversalPOS")
unipostgas <- rdr_pos(unipostagger, Review)


pairs_pos <- as.data.frame(pairs_pos)
class(pairs_pos)
class(unipostgas)

pairs_pos <- unipostgas %>%
  filter(pos %in% c("ADJ", "NOUN"))

View(adj_NN)

#comaprision cloud for POS
pairs_pos %>%
  count(token, pos) %>%
  acast(token ~ pos, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#F8766D"), max.words = 100)


#---------- Step 7 Getting driving factors by matching logic with direct and indirect attribute -------

# Comments separeted by their polarity using sentiment score
olay_total_p <- read.csv("D:/Final Sentiment Analysis/Olay_total_effects_nositive.csv", stringsAsFactors = FALSE)
olay_total_n <- read.csv("D:/Final Sentiment Analysis/Olay_total_effects_negative.csv", stringsAsFactors = FALSE)
 
# Downlaoding English POS from unipostagger library
unipostagger <- rdr_model(language = "English", annotation = "UniversalPOS")

#creating bigrams of reviews
bigram_n <- olay_total_n %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
#seperating them into two coloums
bigram_n <- bigram_n %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_n_2 <- bigram_n$word2
class(bigram_n_2)
# tagging POS for each coulomn
tags_w1 <- rdr_pos(unipostagger, bigram_n_1)

# Filtering the words with specific POS
bigram_data <- tags_w1 %>%
  filter(token1 %in% tags_w1$pos = c("ADV","NOUN","VERB" )) %>%
  filter(token2 %in% tags_w1$pos = c("ADV","NOUN","VERB" ) ) %>%
  count(token1, token2, sort = TRUE)

#creating file with different combinations and concatinanting them into one single pair.
#write.csv(tags_w1,"D:/Final Sentiment Analysis/tags_n_1.csv")


