library(tidyverse)
library(dplyr)
library(lubridate)
sales<- read_csv("https://query.data.world/s/e4wjf6yz5xwztxdfebrdcb2bpdpuia")

#Data Exploration
View(sales)
colnames(sales)
for (c in colnames(sales)){
  typeof(sales[[c]])
}
date<- mdy(date)

#Handling missing data

# Remove the rows with no user_submitted_review
new_sales<- sales %>%
  filter(!is.na(user_submitted_review))

# Calculate the mean of the total_purchased column, without the missing values
View(new_sales)
purchased_mean<- new_sales%>%
  filter(!is.na(total_purchased))%>%
  pull(total_purchased)%>%
  mean


new_sales<- new_sales%>%
  mutate(
   new_total_purchased= if_else(is.na(total_purchased),purchased_mean,total_purchased)
  )

# Processing Review Data
is_positive <- function (review){
  review_positive= case_when(
    str_detect(review,"Awesome")~TRUE,
    str_detect(review,"Good")~TRUE,
    str_detect(review,"Never")~TRUE,
    TRUE~FALSE
  )
}
new_sales<- new_sales%>%
  mutate(
    is_positive= unlist(map(user_submitted_review,is_positive)))

# Comparing Book Sales Between Pre- and Post-Program Sales
new_sales<- new_sales%>%
  mutate(
    data_status = if_else(mdy(date)< ydm("2019-01-07"),"Pre","Post")
  )

new_sales %>%
  group_by(data_status)%>%
  summarise(
    books_purchased = sum(new_total_purchased)
  )

new_sales %>%
  group_by(data_status,customer_type)%>%
  summarise(
    books_purchased = sum(new_total_purchased)
  )

#comparing positive reviews pre and post sentiment program


new_sales %>%
  group_by(data_status)%>%
  summarise(
    pos_reviews = sum(is_positive)
  )

