setwd('R practice')

#Spotify data analysis
spotify <- read.csv("~/Downloads/spotify.csv")

#which is the most common genre
sort(table(spotify$Genre), decreasing = TRUE)
#what is the mean of minutes of all these songs
mean(spotify$Length.)
#which artist has the most songs on the dataframe
sort(table(spotify$Artist.Name))
#find "the best" song according to parameters
#energy, danceability, valence, popularity
spotify2 <- data.frame(spotify$Track.Name[(spotify$Energy > 70) &
                                          (spotify$Danceability > 75) &
                                          (spotify$Valence. > 70) &
                                          (spotify$Popularity > 85)])