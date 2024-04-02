library(fpp3)
library(plotly)
library(lubridate)
library(tidyr)
library(zoo)
library(dplyr)
library(forecast)


df <- read.csv("Group Project (Reviews)/total_reviews.csv")

df <- df %>% 
  mutate(year_month=yearmonth(year_month)) %>% 
  tsibble(index=year_month) %>% 
  select(year_month, total) 

# df %>% 
#   autoplot(total) %>% 
#   ggplotly()

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

# Fill NA's
# Method-1 Linear interpolation
cleaned_df <- cleaned_df %>% 
  mutate(total =  na.approx(total))

cleaned_df %>% 
  autoplot(total)

# Method-2 exponential smoothing -get same result as linear interpolation!?
# Remove non-numeric values
# cleaned_df <- cleaned_df %>%
#   filter(!is.na(as.numeric(total)))
# 
# # Perform interpolation
# cleaned_df_1 <- cleaned_df %>%
#   mutate(total = forecast::na.interp(total, "spline"))
# 
# cleaned_df_1 %>% 
#   autoplot(total)
# 


# transformation- trend and seasonality are now Additive- apply directly to model
lambda <- guerrero(cleaned_df$total, .period = 12)
 
cleaned_df %>%
  autoplot(box_cox(total, lambda))

# cleaned_df <- cleaned_df %>%
#   mutate(total = box_cox(total, lambda))


# train and test data filtering
train_df <- cleaned_df %>% 
  filter_index(. ~ "Nov 2021")

test_df <- cleaned_df %>% 
  filter_index("Dec 2021" ~ .)


# Model 1- seasonal naive
train_df %>% 
  model(SNAIVE(box_cox(total, lambda))) %>% 
  forecast(h = "1 year", level = NULL) %>% 
  autoplot(cleaned_df)


# Model 2- Seasonal naive with drift
fit <- train_df %>%
  model(SNAIVE(box_cox(total, lambda) ~ drift()))

fit %>%
  forecast(h = "1 year") %>%
  autoplot(cleaned_df, level = NULL)



# Model 3- Applying all simple models
fit <- train_df %>% 
  model(
    Mean = MEAN(box_cox(total, lambda)),
    `Naïve` = NAIVE(box_cox(total, lambda)),
    `Seasonal naive` = SNAIVE(box_cox(total, lambda)),
    Drift = RW(box_cox(total, lambda) ~ drift()), 
    Drift_with_Snaive = SNAIVE(box_cox(total, lambda) ~ drift())
  )


fit %>%
  forecast(h = "1 year") %>%
  autoplot(cleaned_df, level = NULL)

fit_fc <- fit %>% 
  forecast(h = "1 year")

fit_fc %>% 
  autoplot( train_df,level = NULL) +
  labs(
    y = "Total Reviews",
    title = "Forecasts for Total Reviews"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

# find the best model
accuracy(fit_fc, cleaned_df)


# Model 4- ETS and TSLM
fit <- train_df %>% 
  model(
    ets = ETS(box_cox(total, lambda)), 
    tslm = TSLM(box_cox(total, lambda)~ trend() + season())
  )

fit_fc <- fit %>% 
  forecast(h = "1 year") 

fit_fc %>% 
  autoplot( train_df,level = NULL) +
  labs(
    y = "Total Reviews",
    title = "Forecasts for Total Reviews"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

# Plot to check forecast line along with original
# fit %>%
#   forecast(h = "1 year") %>%
#   autoplot(cleaned_df, level = NULL)

# find the best model
accuracy(fit_fc, cleaned_df)








