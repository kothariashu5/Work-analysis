library(tidyverse)
library(dplyr)
project<- read.csv("https://query.data.world/s/etfy7iungh6qdtrindshkskeyevrkx")
colnames(project)
for (c in colnames(project)) {
  typeof(project[[c]])
}
for(c in colnames(project)){
  print("unique values in the column:")
  print(c)
  print(unique(project[[c]]))
  print("")
}
new_project = project %>%
  filter(!is.na(review))
dim(new_project)
View(new_project)
new_project<- new_project %>%
  mutate(
    state = case_when(
      new_project$state == "California" ~ "CA",
      new_project$state == "New York" ~ "NY",
      new_project$state == "Texas" ~ "TX",
      new_project$state == "Florida" ~ "FL",
       TRUE ~ state
      )
  )
# Transforming the Review Data

```{r}
new_project <- new_project %>% 
  mutate(
    review_num = case_when(
      review == "Poor" ~ 1,
      review == "Fair" ~ 2,
      review == "Good" ~ 3,
      review == "Great" ~ 4,
      review == "Excellent" ~ 5
    ),
    is_high_review = if_else(review_num >= 4, TRUE, FALSE)
  )
```



#analyzing the data

```{r}
new_project %>% 
group_by(book) %>% 
summarize(
purchased = n()
) %>% 
arrange(-purchased)
```
