## G 324
## Chiayu Tu
## 1/6/2022

## Question one (1)
ggplot(data = faithful) + 
  geom_point(mapping = aes(x = waiting, y = eruptions))

## Question one (2)

## Question one (3)
ggplot(data = faithful, mapping = aes(x = waiting, y = eruptions)) +
  geom_point() +
  geom_smooth()
  
## Question two (1)
ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  geom_smooth()

## Question two (2)


