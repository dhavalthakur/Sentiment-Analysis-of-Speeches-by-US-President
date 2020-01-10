library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
files <- list.files("C:/Waterloo Notes/R Scripts/Sentiment Analysis using R/sotu")

# stick together the path to the file & 1st file name
fileName <- glue("C:/Waterloo Notes/R Scripts/Sentiment Analysis using R/sotu/", files[1], sep = "")
fileName <- trimws(fileName)
fileText <- glue(read_file(fileName))
# remove any dollar signs (they're special characters in R)
fileText <- gsub("\\$", "", fileText) 

# tokenize
tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)

tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words, here we using Bing list
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative words

#Now Function to calculate the sentiments of all the speeches
GetSentiment <- function(file){
  # get the file
  fileName <- glue("C:/Waterloo Notes/R Scripts/Sentiment Analysis using R/sotu/", file, sep = "")
  # get rid of any sneaky trailing spaces
  fileName <- trimws(fileName)
  
  # read in the new file
  fileText <- glue(read_file(fileName))
  # remove any special characters in R
  fileText <- gsub("\\$", "", fileText) 
  tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
  # get the sentiment from the first text: 
  sentiment <- tokens %>%
    inner_join(get_sentiments("bing")) %>% # pull out only sentimen words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
    mutate(sentiment = positive - negative) %>% # number of positive words - number of negative owrds
    mutate(file = file) %>% # add the name of our file
    mutate(year = as.numeric(str_match(file, "\\d{4}"))) %>% # add the year
    mutate(president = str_match(file, "(.*?)_")[2]) # add  the president
  
  # return our sentiment dataframe
  return(sentiment)
}
GetSentiment(files[1])

#Now putting all presidents speech sentiments into one
sentiments <- data_frame()
for(i in files){
  sentiments <- rbind(sentiments, GetSentiment(i))
}

bushSr <- sentiments %>% 
  filter(president == "Bush") %>% # get rows where the president is named "Bush"...
  filter(year < 2000) %>% # ...and the year is before 200
  mutate(president = "Bush Sr.") # and change "Bush" to "Bush Sr."

# remove incorrect rows
sentiments <- anti_join(sentiments, sentiments[sentiments$president == "Bush" & sentiments$year < 2000, ])

# add corrected rows to data_frame 
sentiments <- full_join(sentiments, bushSr)

summary(sentiments)


#Now plotting a graph to check if during the course of time pattern of sentiments
#same or not
ggplot(sentiments, aes(x = as.numeric(year), y = sentiment)) + 
  geom_point(aes(color = president))+ # add points to our plot, color-coded by president
  geom_smooth(method = "auto") # pick a method & fit a model

# plotting of sentiment by president
ggplot(sentiments, aes(x = president, y = sentiment, color = president)) + 
  geom_boxplot() # draw a boxplot for each president
