
### Getting access to Twitter
library(tidyverse)
library(rtweet)
library(lubridate)
library(tidytext)
library(jsonlite)
library(hrbrthemes)

####### TWITTER - API #######

## Some demonstration of what you can do:
tweet_df2 <- rtweet::search_tweets("#Ukraine",
                                  n = 1000,
                                  include_rts = FALSE)

# whatever name you assigned to your created app
appname <- "your-app-name"

## api key (example below is not a real key)
key <- "yourLongApiKeyHere"

## api secret (example below is not a real key)
secret <- "yourSecretKeyHere"

## authentication via web browser
token <- create_token(
  app = appname,
  consumer_key = key, 
  consumer_secret = secret
)

token

## Interesting requests
## Doing some visualizations:

## Who is twitting the most about the topic?
tweet_df %>% 
  count(screen_name, sort = T) %>%
  head(15) %>% 
  mutate(screen_name = reorder(screen_name, n)) %>% 
  ggplot(aes(screen_name, n)) +
  geom_col() +
  coord_flip()

## Whose tweets are getting more retwitted?
tweet_df %>% 
  group_by(screen_name) %>% 
  summarize(tweets = n(),
            retweets = sum(retweet_count)) %>%
  arrange(desc(retweets))

## Analyzing the text - most common words
tweet_words <- tweet_df %>% 
  select(screen_name, text, retweet_count, favorite_count, created_at) %>% 
  unnest_tokens(word, text, token = "tweets") %>% 
  anti_join(stop_words, by = "word")

tweet_words %>% 
  count(word, sort = TRUE) %>% 
  head(30) %>% 
  mutate(word= reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_col() + 
  coord_flip() +
  labs(title = "Most common words in #Ukraine tweets",
       y = "Frequency of Words")

## Amount of tweets - hours
tweet_df %>% 
  count(date = floor_date(created_at, unit = "hours")) %>% 
  ggplot(aes(date, n)) + 
  geom_line() +
  expand_limits(y = 0)


####### CHICAGO DATA PORTAL - API #######

camerasViolations_url <- "https://data.cityofchicago.org/resource/hhkd-xvj4.json?$limit=286884"

df_violations <- read_json(camerasViolations_url, simplifyVector = TRUE) %>% 
  as_tibble()

## Transforming the data before graphing
df_speed_tickets <- df_violations %>%
  select(address, camera_id, violation_date, violations, longitude, latitude) %>% 
  mutate(division = str_split(violation_date, pattern = "T")) %>% 
  unnest(cols = division) %>% 
  filter(division != "00:00:00.000") %>% 
  mutate(DATE = lubridate::ymd(division)) %>% 
  group_by(year(DATE), month(DATE)) %>% 
  summarize(total_violations = sum(as.numeric(violations))) %>% 
  setNames(c("year", "month", "total_violations")) %>% 
  unite("year_month", year:month, sep = "/") %>% 
  mutate(date = lubridate::ym(year_month)) %>% 
  select(-year_month)


df_speed_tickets %>%
  ungroup() %>% 
  filter(date < ymd("2022-02-1")) %>%
  mutate(total_violations = total_violations) %>% 
  group_by(year(date)) %>% 
  summarize(total_violations = sum(total_violations))

df_speed_tickets %>%
  ungroup() %>% 
  filter(date < ymd("2022-02-1")) %>%
  mutate(total_violations = total_violations/1000) %>% 
  ggplot(aes(x = date, y = total_violations)) +
  geom_line(size = 0.65) +
  theme_ipsum() +
  xlab("") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%y") +
  labs(title = "Speed Camera Violations",
       subtitle = "Monthly volume of traffic violations in Children's Safety Zones - Chicago",
       x = "",
       y = "Total of violations (in thousands)",
       caption = "Source: Chicago Data Portal")
  #geom_smooth()
