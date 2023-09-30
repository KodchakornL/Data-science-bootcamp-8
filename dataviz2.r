# load GGplot library
# update this file, just another comment

library(ggplot2)

# first plot
ggplot(diamonds, aes(caret, price)) +
  geom_point()

# second plot
ggplot(diamonds, aes(caret, price)) +
  geom_point() +
  geom_smooth()
