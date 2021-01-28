library(rtweet)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(textdata)
library(purrr)
library(SentimentAnalysis)
library(e1071) 
library(caret)


## API Keys
api_key <- "Ud90i4wTL8ZRsBMjpJ7o4TtH9"
api_secret_key <- "zkMMNq3KBlUw4mD9sbnMrgMCsST2tBTKs7iZ9pU16My07lh2sb"

## Authenticate via web browser
token <- create_token(
  app = "SentimenPSBB_UPNYK",
  consumer_key = api_key,
  consumer_secret = api_secret_key)

# Cari tweet tentang topik pilihan, 
# Mempersempit jumlah tweet yang diinginkan dan putuskan untuk memasukkan retweet atau tidak.
dataCuitan <- search_tweets("America Lockdown", n=1200,lang = "en", include_rts = FALSE)
dataCuitan

# Proses setiap set tweet menjadi teks Tidy.
Cuitan = dataCuitan %>% select(screen_name, text)
Cuitan
head(Cuitan$text)
# menghapus element html yang masih ada
Cuitan$After <- gsub("http\\S+","",Cuitan$text)

# Menggunakan fungsi unnest_tokens() untuk konversi menjadi huruf kecil
# hapus tanda baca, dan id untuk setiap tweet
kata <- Cuitan %>%
  select(After) %>%
  unnest_tokens(word, After)
head(kata)

#hapus kata-kata stopwords dari daftar kata-kata
cleanedKata <- kata %>%
  anti_join(stop_words)
head(cleanedKata)


# classify emotion 
class_emo =  classify_emotion(cleanedKata, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"
# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]
View(polarity)

# data frame with results
sent_df = data.frame(text=cleanedKata, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
write.csv(sent_df, file = 'dataSentimen.csv')
View(sent_df)
head(sent_df,20)
table(sent_df$emotion)
# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  labs(title = "Sentiment Analysis",
       plot.title = element_text(size=12))
plotSentiments1 <- function(sentiment_dataframe, title) 
{
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=emotion)) + 
    geom_bar(aes(y=..count.., fill=emotion)) + 
    scale_fill_brewer(palette="Dark2") + 
    ggtitle(title) + 
    theme(legend.position="right") + 
    ylab("Number of Tweets") + 
    xlab("Emotion Categories")
}

#20 kata teratas di tweet #PSBB
visualisasiKata <- cleanedKata %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y = n)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  labs(x="Count",
       y="Unique word",
       tittle="Unique word counts")

# Untuk melakukan analisis sentimen menggunakan Bing di tweet PSBB, 
# perintah berikut ini mengembalikan sebuah tibble.
bingKata = cleanedKata %>% inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% ungroup()

# Visualisasi jumlah kata Positif Negatif, 
visualisasiSentimen <- bingKata %>% group_by(sentiment) %>% top_n(5) %>% ungroup() %>% 
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment))+ 
  geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + 
  labs(title = "Tweets containing '#PSBB'", y = "Contribution to sentiment", 
       x = NULL) + coord_flip() + theme_bw()



# World Cloud 
library(tm)
library(wordcloud)
library(RColorBrewer)

# Generate word-cloud.
wordcloud(bingKata$word, bingKata$n, random.order=FALSE, rot.per=0.25, colors=brewer.pal(8, "Dark2"))


write.csv(Cuitan, paste('E:\\Mata Kuliah\\Semester 5\\Data Science\\2. UAS\\Project\\Tugas Akhir DS Website\\','cuitan.csv',sep = ''))
write.csv(cleanedKata, paste('E:\\Mata Kuliah\\Semester 5\\Data Science\\2. UAS\\Project\\Tugas Akhir DS Website\\','cleanedData.csv',sep = ''))
write.csv(bingKata, paste('E:\\Mata Kuliah\\Semester 5\\Data Science\\2. UAS\\Project\\Tugas Akhir DS Website\\','bingKata.csv',sep = ''))
