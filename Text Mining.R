#lexicon based text analysis for 30 hotels/restaurants
#set working directory
setwd("C:/Users/faco9/Downloads")

#intalling packages and calling libraries needed.
library(readr)
library(tidyverse)
library(tidytext)

#read in and explore data
tourist<-read_csv("tourist_accommodation_reviews.csv")
names(tourist)
str(tourist)
View(tourist)

# use this to select 30 hotels/restaurant for analysis
tourist%>%
    select(`Hotel/Restaurant name`, `Review`)%>%
    count(`Hotel/Restaurant name`, `Review`)%>%
    arrange(desc(n))%>%
    View()

#create a new subset consisting of selected 30 hotels/restaurants  
tourist2<-subset(tourist,
                 `Hotel/Restaurant name` %in% c("Wok",
                                                "Baoli",
                                                "Golbasi",
                                                "Heaven",
                                                "Fatty's",
                                                "DeDos",
                                                "Cucina",
                                                "Flame",
                                                "Pinto",
                                                "Crust",
                                                "Ohlala",
                                                "Nami",
                                                "Veranda",
                                                "Bellini",
                                                "Chaba",
                                                "Doo Dee",
                                                "Mint",
                                                "La Casa",
                                                "Shakers",
                                                "Papaya",
                                                "Jeffer",
                                                "Le Siam",
                                                "Modena",
                                                "OSOT",
                                                "Sawasdee",
                                                "Vista",
                                                "Amalfi",
                                                "Coyote",
                                                "Tatonka",
                                                "Tantra"),
                 select =c(`Hotel/Restaurant name`,`ID`,`Review`))

               	
View(tourist2)
head(tourist2)
tail(tourist2)
  
#Export to Workbook for SAS analysis
library(writexl)
write_csv(tourist2, "c:/Users/faco9/Downloads/Tourist_Reviews.csv")

#restructure the data using the models 
T30<-unnest_tokens(tbl=tourist2, input = Review, output = word)
head(T30)
tail(T30)

#create dataframe with stop words
stp_wrds<-get_stopwords(source = "snowball")
head(stp_wrds)
tail(stp_wrds)

#remove stop words from our database of interest
T30<-anti_join(T30, stp_wrds, by="word")
head(T30)
tail(T30)

#sentiment analysis using the lexicon/dictionary 'bing'
bing<-get_sentiments(lexicon="bing")
head(bing)
tail(bing)

#join our data frames
T30_bing<-inner_join(T30, bing, by="word")
head(T30_bing)
tail(T30_bing)

#create a word cloud
mydata<-T30_bing
head(mydata)
wordcloud(mydata$word, max.words = 50, scale = c(3.0, 0.5), rot.per = 0,
          colors = brewer.pal(8, "Dark2"), random.order = FALSE, random.color = FALSE,
          fixed.asp = FALSE)

#compute sentiment count by Hotel/Restaurant name by ID
T30_bing<-count(T30_bing, `Hotel/Restaurant name`, `ID`, sentiment)
head(T30_bing)
tail(T30_bing)

#manipulate data frame from long to wide format
T30_bing<-spread(key=sentiment, value=n, fill=0, data=T30_bing)
head(T30_bing)
tail(T30_bing)

#create new variable by calculating overall sentiment
T30_bing<-mutate(sentiment=positive-negative, .data=T30_bing)
head(T30_bing)
tail(T30_bing)
View(T30_bing)

#for further analysis we calculate mean sentiment of the 30
mean(T30_bing$sentiment, na.rm=TRUE)
mean(T30_bing$negative, na.rm = TRUE)
mean(T30_bing$positive, na.rm=TRUE)


#visualization of analysis
ggplot(aes(x =`Hotel/Restaurant name`  , y = sentiment,), data = T30_bing) +
geom_col(show.legend = FALSE, fill = "steel blue")+
  labs(x="Hotel/Restaurant", y="Sentiment") +
  theme_classic()


#A more robust approach using pipe function Option B
#segment by Hotel/Restaurant name
df30<-T30
df30<-T30 %>%
  inner_join(bing, by="word")%>%
  count(`Hotel/Restaurant name`, sentiment)%>%
  spread(key=sentiment, value=n, fill=0) %>%
  mutate(sentiment=positive-negative)
head(df30)
tail(df30)

#visuals
ggplot(aes(x = `Hotel/Restaurant name`, y = sentiment ), data = df30) +
  geom_col(show.legend = FALSE, fill = "steel blue") + 
  labs(x="Hotel/Restaurant", y="Sentiment") +
  theme_classic()

