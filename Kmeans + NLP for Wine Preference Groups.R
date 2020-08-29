library(tm)
library(wordcloud)

#Data Tech for Wine - Sample Data Analysis

customer_data <- data.frame(seq(1:1000))
names(customer_data) <- 'ID'

customer_data$registration_date <- sort(sample(seq(as.Date('2019/01/01'), 
                                      as.Date('2020/01/01'), by="day"), 
                                  size = 1000, replace = TRUE), 
                               decreasing = FALSE)

customer_data$name <- sample(x = NA, size = 1000, replace = TRUE)

customer_data$registration_methods <- sample(x = c('Facebook', 'Google', 
                                'Manual', 'Unregistered'), 
                          size = 1000, replace = TRUE)

customer_data$cookies <- sample(x = c('yes', 'no'), size = 1000, replace = TRUE)

table(customer_data$cookies)

customer_data$time_spent[customer_data$cookies == 'yes'] <- sample(x = c(1:60), 
                                                 size = 489,
                                                 replace = TRUE)

customer_data$gender <- sample(x = c('F', 'M', NA), size = 1000, replace = TRUE)

customer_data$age <- sample(x = c(18:85), size = 1000, replace = TRUE)

customer_data$wine_knowledge <- sample(x = c('newbie', 'somewhat', 'expert'),
                              size = 1000, replace = TRUE)

customer_data$country <- sample(x = 'Australia',
                              size = 1000, replace = TRUE)

customer_data$region <- sample(x = c('South Australia', 'Victoria', 'New South Wales',
                                     'Queensland', 'Tasmania', 'Western Australia',
                                     'Australia Capital Territory', 'North Territories'),
                              size = 1000, replace = TRUE)


customer_data$wine <- sample(x = c('fruity', 'light', 'medium', 'full body',
                                        'organic', 'vegan', 'no sugar', 'soft',
                                        'sweet', 'other'), 
                                  size = 1000, replace = TRUE)

customer_data$food <- sample(x = c('Meat', 'Fish', 'Pasta', 
                                        'Sushi', 'Vegetarian', 'Vegan',
                                        'Other'), 
                                  size = 1000, replace = TRUE)

customer_data$music <- sample(x = c('Rock', 'Rap', 'Pop', 
                                        'Classical', 'Country', 
                                        'Reggae', 'Jazz', 'Techno', 
                                        'Other'), 
                                  size = 1000, replace = TRUE)

customer_data$cities <- sample(x = c('Buenos Aires', 'Sydney', 'Rio', 
                                          'London', 'New York', 'Cape Town',
                                          'Tokyo', 'Shanghai', 'Wellington',
                                          'Paris', 'Tel Aviv', 'Moscow',
                                          'Other'), 
                                    size = 1000, replace = TRUE)

customer_data$movie <- sample(x = c('Action', 'Comedy', 'Adventure', 
                                         'Scary', 'Drama', 'Reality', 'Suspence', 
                                         'Mystery', 'Other'), 
                                   size = 1000, replace = TRUE)



winery_data <- data.frame(seq(1:50))
names(winery_data) <- 'ID'

winery_data$registration_date <- sort(sample(seq(as.Date('2019/01/01'), 
                                                   as.Date('2020/01/01'), by="day"), 
                                               size = 50, replace = TRUE), 
                                        decreasing = FALSE)

winery_data$name <- sample(x = NA, size = 50, replace = TRUE)

winery_data$country <- sample(x = 'Australia', size = 50, replace = TRUE)

winery_data$region <- sample(x = c('South Australia', 'Victoria', 'New South Wales',
                                                 'Queensland', 'Tasmania', 'Western Australia',
                                                 'Australia Capital Territory', 'North Territories'),
                                           size = 50, replace = TRUE)

winery_data$wine_maker <- sample(x = NA, size = 50, replace = TRUE)

winery_data$n_products <- sample(x = 5:100, size = 50, replace = TRUE)

winery_data$n_sales <- sample(x = 1:500, size = 50, replace = TRUE)

winery_data$earnings <- sample(x = 20:2000, size = 50, replace = TRUE)

winery_data$customer_rating_avg <- sample(x = c(1:5, NA), size = 50, replace = TRUE)

winery_data$products_rating_avg <- sample(x = c(1:5, NA), size = 50, replace = TRUE)

winery_data$total_consumers <- sample(x = c(1:100, NA), size = 50, replace = TRUE)

winery_data$direct_consumers <- sample(x = c(1:100, NA), size = 50, replace = TRUE)

winery_data$guided_consumers <- sample(x = c(1:100, NA), size = 50, replace = TRUE)

winery_data$page_viewers <- sample(x = c(1:1000, NA), size = 50, replace = TRUE)

winery_data$product_viewers <- sample(x = c(1:1000, NA), size = 50, replace = TRUE)

winery_data$wine <- sample(x = c('fruity', 'light', 'medium', 'full body',
                                        'organic', 'vegan', 'no sugar', 'soft',
                                        'sweet', 'other'), 
                                  size = 50, replace = TRUE)

winery_data$food <- sample(x = c('Meat', 'Fish', 'Pasta', 
                                        'Sushi', 'Vegetarian', 'Vegan',
                                        'Other'), 
                                  size = 50, replace = TRUE)

winery_data$music <- sample(x = c('Rock', 'Rap', 'Pop', 
                                         'Classical', 'Country', 
                                         'Reggae', 'Jazz', 'Techno', 
                                         'Other'), 
                                   size = 50, replace = TRUE)

winery_data$cities <- sample(x = c('Buenos Aires', 'Sydney', 'Rio', 
                                          'London', 'New York', 'Cape Town',
                                          'Tokyo', 'Shanghai', 'Wellington',
                                          'Paris', 'Tel Aviv', 'Moscow',
                                          'Other'), 
                                    size = 50, replace = TRUE)

winery_data$movie <- sample(x = c('Action', 'Comedy', 'Adventure', 
                                         'Scary', 'Drama', 'Reality', 'Suspence', 
                                         'Mystery', 'Other'), 
                                   size = 50, replace = TRUE)




customer_interests <- customer_data[,12:16]
interests_corpus <- Corpus(VectorSource(customer_interests))
interests_corpus
inspect((interests_corpus[1:5]))

corpus_clean <- tm_map(interests_corpus, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
corpus_clean <- tm_map(corpus_clean, tolower)

inspect(corpus_clean[1:5])

interests_dtm <- DocumentTermMatrix(corpus_clean)

kmeans4 <- kmeans(interests_dtm, 4)

interests_with_cluster <- as.data.frame(cbind(customer_interests, kmeans4$cluster))

cluster1 <- subset(interests_with_cluster,
                   subset = interests_with_cluster$`kmeans4$cluster` == 1)

sort(table(cluster1$wine))
sort(table(cluster1$food))
sort(table(cluster1$music))
sort(table(cluster1$cities))
sort(table(cluster1$movie))

cluster3 <- subset(interests_with_cluster,
                   subset = interests_with_cluster$`kmeans4$cluster` == 3)

sort(table(cluster3$wine))
sort(table(cluster3$food))
sort(table(cluster3$music))
sort(table(cluster3$cities))
sort(table(cluster3$movie))