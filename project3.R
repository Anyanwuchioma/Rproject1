library(tidyverse)
data
glimpse(msleep)

#rename variable
msleep %>%
  rename("consevr" = "conservation")

#change the variable type
class(msleep$vore)
msleep$vore <- as.factor(msleep$vore)
####or
  msleep %>%
    mutate(vore = as.factor(vore))
  
#### Select variable to work with
names(msleep)
msleep %>%
  select (2:4,awake, starts_with("sleep"), contains("wt")) %>%
  names()


## reordering variable
msleep %>%
  select(vore, name, everything())

## select missing column
msleep %>%
  select(name,sleep_rem, sleep_total, brainwt) %>%
  filter(!complete.cases(.))

##filter and arrange
unique(msleep$order)

msleep %>%
  filter((order=="carnivora" | order=="primate") & sleep_total >8) %>%
  select(name, order, sleep_total) %>%
  arrange(-sleep_total ) %>%
  view()

msleep %>%
  filter(order %in% c("carnivora","primate" ) & sleep_total > 8 )%>%
           select(name, order, sleep_total) %>%
           arrange(order ) %>%
           View()



#change observation with Mutate
msleep %>%
  mutate(brainwt =brainwt*1000) %>%
  view()


#to create  new variable
msleep %>%
  mutate(brainwt_in_gram =brainwt*1000) %>%
  view()

#Condition change (if else)
msleep$brainwt
msleep$brainwt >0.01

size_of_brain <- msleep %>%
  select(name, brainwt)%>%
  drop_na(brainwt) %>%
  mutate(brain_size = if_else(brainwt>0.01,"Large","Small")) %>%
  view()


#reshape the date from wide to long or long to wide
install.packages("gapminder")
library(gapminder)
view(gapminder)

data <- select(gapminder, country, year, lifeExp)
view(data)

wide_data <- data %>%
  pivot_wider(names_from = year, values_from = lifeExp) %>%
view()

long_data <- wide_data %>%
  pivot_longer( 2:13, names_to = "year", values_to = "lifeExp" )
view(long_data)

  