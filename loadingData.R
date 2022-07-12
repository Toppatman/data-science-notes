#### Load in csv file ####
spotify_data <- read.csv("data/Spotify-2000.csv")
View(spotify_data)

## Practice plotting with your dataset
ggplot(data = spotify_data, aes(x = Title, y= Popularity)) +
  geom_bar()

ggplot(data = spotify_data, aes(x = , y= )) +
  geom_histogram()