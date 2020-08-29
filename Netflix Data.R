setwd('R practice')

Netflix <- read.csv("~/Downloads/netflix.csv")

#has netflix produced more movies or tv shows?
table(Netflix$type)

#were more movies or tv shows released recently?
class(Netflix$release_year)
Netflix$release_year <- as.numeric(Netflix$release_year)
table(Netflix$type[Netflix$release_year > 2017])

#which country had more movies realsed
table(Netflix$country)
sort(table(Netflix$country), decreasing = TRUE)

#which movie and tv Show had the longest duration and the mean
class(Netflix$duration)
Netflix$duration <- as.numeric(Netflix$duration)
max(Netflix$duration[Netflix$type == "Movie"])
max(Netflix$duration[Netflix$type == "TV Show"])
mean(Netflix$duration[Netflix$type == "Movie"])
mean(Netflix$duration[Netflix$type == "TV Show"])


#what is the movie with the longest duration? (name)
class(Netflix$title)
Netflix$title <- as.character(Netflix$title)
table(Netflix$title[Netflix$duration])

Netflix2 <- data.frame(Netflix$title[Netflix$type == "Movie"],
                       Netflix$type[Netflix$type == "Movie"],
                       Netflix$duration[Netflix$type == "Movie"]) 

names(Netflix2) <- c('title', 'type', 'duration')