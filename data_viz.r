# My first R program

print("Hello world")
print("oppenheimer is very good movie")

library(dplyr)
mtcars %>%
  select(1:5) %>%
  filter(mpg > 30)

mtcars %>%
  select(mpg) %>%
  filter(mpg < 30)