library(tidyverse)
library(tidymodels)
library(vroom)

train <- vroom("train.csv")
test <- vroom("test.csv")

#get rid of casual and registered
train <- train %>%
  select(-casual, -registered)

my_linear_model <- linear_reg() %>% #Type of model
  set_engine("lm") %>% # Engine = What R function to use
  set_mode("regression") %>% # Regression just means quantitative response
  fit(formula=count~.-datetime, data=train)

## Generate Predictions Using Linear Model
bike_predictions <- predict(my_linear_model,new_data=test)
bike_predictions ## Look at the output

submission <- tibble(
  datetime = test$datetime, 
  count = bike_predictions$.pred
) %>%
  mutate(count = if_else(count < 0, 0, count))

submission_formatted <- submission %>%
  mutate(
    # Convert the datetime object to a character string in YYYY-MM-DD HH:MM:SS format
    datetime = format(datetime, "%Y-%m-%d %H:%M:%S")
  )

write_csv(submission_formatted, "bike_linear_reg.csv")

head(submission)

