library(fpp3)
library(plotly)
library(lubridate)
library(tidyr)
library(zoo)

df <- read.csv("Group Project (Reviews)/total_reviews.csv")

df <- df %>% 
  mutate(year_month=yearmonth(year_month)) %>% 
  tsibble(index=year_month) %>% 
  select(year_month, total) 

df %>% 
  autoplot(total) %>% 
  ggplotly()

# make change here- clean the df by removing the drop due to covid and 
# replace it with linear interpolation- probably.
cleaned_df = df

# Convert year_month to Date format to remove covid drop
cleaned_df$year_month <- ym(cleaned_df$year_month)
cleaned_df$total[(cleaned_df$year_month >= ym("2020-02")) & (cleaned_df$year_month <= ym("2021-05"))] <- NA

cleaned_df <- cleaned_df %>% 
  tsibble(index=year_month) %>% 
  mutate(year_month = yearmonth(year_month))

cleaned_df %>% 
  autoplot(total)


cleaned_df <- cleaned_df %>% 
  mutate(total =  na.approx(total))

cleaned_df %>% 
  autoplot(total)
  

# train and test data filtering
train_df <- cleaned_df %>% 
  filter_index(. ~ "Nov 2021")

test_df <- cleaned_df %>% 
  filter_index("Dec 2021" ~ .)


# Model 1- seasonal naive
train_df %>% 
  model(SNAIVE(total)) %>% 
  forecast(h = 10) %>% 
  autoplot(cleaned_df)


