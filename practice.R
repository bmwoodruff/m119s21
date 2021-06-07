pacman::p_load_gh("drsimonj/ourworldindata")
pacman::p_load(tidyverse)

?financing_healthcare
financing_healthcare

financing_healthcare %>% glimpse()

financing_healthcare %>% View()

financing_healthcare %>% 
  ggplot() +
  aes(x = life_expectancy, y = child_mort, color = continent) +
  geom_point()



b11 <- 3
b12 <- 2
b21 <- 2
b22 <- 5
c1 <- 5
c2 <- 12

x <- (c1*b22 - b12*c2)/(b11*b22 - b12*b21)
y <- (b11*c2 - c1*b21)/(b11*b22 - b12*b21)

#Check
b11*x+b12*y == c1
b21*x+b22*y == c2
