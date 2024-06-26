library(fpp3)
library(plotly)
library(lubridate)
library(tidyr)
library(zoo)
library(dplyr)
library(forecast)
library(fable.prophet)
library(Rcpp)

# reading the data
df <- read.csv("Group Project (Reviews)/total_reviews.csv")

df <- df %>% 
  mutate(year_month=yearmonth(year_month)) %>% 
  tsibble(index=year_month) %>% 
  select(year_month, total) 

# df %>% 
#   autoplot(total) %>% 
#   ggplotly()

# make change here- clean the df by removing the drop due to covid and 
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
# Linear interpolation to fill covid gap
cleaned_df <- cleaned_df %>% 
  mutate(total =  na.approx(total))

cleaned_df %>% 
  autoplot(total)


# all models worse with tslm gap filling 
# cleaned_df <- cleaned_df %>%
#   # Fit ARIMA model to the data containing missing values
#   model(TSLM(total~ trend() + season())) %>%
#   # Estimate Trips for all periods
#   interpolate(cleaned_df)


cleaned_df %>% 
  autoplot(total)


# transformation- trend and seasonality are now Additive- apply directly to model
lambda <- guerrero(cleaned_df$total, .period = 12)
 
cleaned_df %>%
  autoplot(box_cox(total, lambda))


# train and test data filtering
train_df <- cleaned_df %>% 
  filter_index(. ~ "Nov 2021")

train_df %>% 
  autoplot() %>% 
  ggplotly()


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


# Models Using Cross Validation

# Drift, ets, tslm, snaive, arima with CV- Final model selection!
df_cv <- train_df %>% 
  stretch_tsibble(.init = 10*12, .step = 12) 


fit <- df_cv %>%
  model(drift_bc = RW(box_cox(total, lambda) ~ drift()),
        ets_bc = ETS(box_cox(total, lambda)),
        tslm_bc = TSLM(box_cox(total, lambda)~ trend() + season()),
        snaive_bc = SNAIVE(box_cox(total, lambda)),
        arima_bc = ARIMA(box_cox(total, lambda)),
        Drift_with_Snaive_bc = SNAIVE(box_cox(total, lambda) ~ drift()),
        prophet_bc = prophet(box_cox(total, lambda)),
        drift = RW(total ~ drift()),
        ets = ETS(total),
        tslm = TSLM(total~ trend() + season()),
        snaive = SNAIVE(total),
        arima = ARIMA(total),
        Drift_with_Snaive = SNAIVE(total ~ drift()),
        prophet = prophet(total))



# Using filter because it's predicting two extra values which are not in train dataset.
fit_fc <- fit %>% 
  forecast(h = "1 year") %>% 
  filter(year_month <= yearmonth("2021 Nov"))


fit_fc %>% 
  accuracy(train_df) %>% 
  arrange(RMSE)


fit_fc %>% 
  accuracy(train_df) %>% 
  arrange(RMSE) %>% 
  select(.model, RMSE)

# Prophet is the best model!

# fitting model to train data
fit <- train_df %>% 
  model(prophet = prophet(total))

gg_tsresiduals(fit)

fit_fc <- fit %>% 
  forecast(h = "1 year", times=0)

fit_fc %>% 
  accuracy(test_df) 


# Final prophet model trained on all data and create final predictions (CSV)
fit_fc <- cleaned_df %>% 
  model(prophet(total)) %>%  #model(ARIMA(box_cox(total, lambda))) %>% 
  forecast(h = "12 months") 

fit_fc <- cleaned_df %>% 
  model(prophet(total)) %>% 
  forecast(h = "12 months") 

#fit_fc

colnames(fit_fc)

final_csv <- data_frame(month = fit_fc$year_month, n_reviews = fit_fc$.mean)

# final_csv %>% 
#   autoplot(cleaned_df)

# Plot
plot <- ggplot(final_csv, aes(x = month, y = n_reviews)) +
  geom_line() +
  labs(x = "Month", y = "Number of Reviews", title = "Monthly Reviews Trend") 

ggplotly(plot)

# plot
fit_fc %>% 
  autoplot(cleaned_df)

write.csv(final_csv, "Group_Predictions.csv", row.names = FALSE)

