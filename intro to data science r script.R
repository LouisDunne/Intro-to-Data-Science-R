# This section installs and loads the tidyverse package.
# It also reads the data set chosen into R for analysis.

install.packages("tidyverse")
library(tidyverse)
music<-read_csv(file.choose())

# This section performs initial exploration and inspection of the data.
# This helps understand the structure, variables, summary statistics,
# missing values, and example observations in the data set.

glimpse(music)
str(music)
summary(music)
colSums(is.na(music))
head(music)

# This section selects relevant variables from the original data set.
# This is done to create a cleaned data set focused on features used in analysis.

music_clean<-music %>%
  select(ranking, year, song, band_singer,
         danceability, energy, valence, tempo,
         loudness, acousticness, speechiness,
         instrumentalness, liveness, duration_ms)

# This section converts selected variables into a numerical format to ensure
# their suitability for quantitative analysis and modelling.

music_clean<-music_clean %>%
  mutate(ranking=as.numeric(ranking),
         year=as.numeric(year),
         danceability=as.numeric(danceability),
         energy=as.numeric(energy),
         valence=as.numeric(valence),
         tempo=as.numeric(tempo),
         loudness=as.numeric(loudness),
         acousticness=as.numeric(acousticness),
         speechiness=as.numeric(speechiness),
         instrumentalness=as.numeric(instrumentalness),
         liveness=as.numeric(liveness),
         duration_ms=as.numeric(duration_ms))

# This section checks for missing values, removes incomplete and duplicate
# observations, verifies the cleaned data set, and exports it for use in
# subsequent analysis.

colSums(is.na(music_clean))
music_clean<-music_clean %>%
  drop_na(ranking, year, danceability, energy, valence)
music_clean<-music_clean %>%
  distinct()
glimpse(music_clean)
summary(music_clean)
write_csv(music_clean, "music_clean.csv")
nrow(music_clean)

# This section explores the distribution and range of key variables, including
# the temporal coverage of the data set and the spread of song rankings.

range(music_clean$year, na.rm=TRUE)
table(music_clean$year)
range(music_clean$ranking, na.rm=TRUE)
summary(music_clean$ranking)

# This section calculates yearly average values for selected audio features
# to examine how musical characteristics vary over time.

yearly<-music_clean %>%
  group_by(year) %>%
  summarise(
    mean_danceability=mean(danceability, na.rm=TRUE),
    mean_energy=mean(energy, na.rm=TRUE),
    mean_valence=mean(valence, na.rm=TRUE),
    .groups="drop")

# This section visualises the yearly trends in average danceability, energy, and
# valence to identify how key musical characteristics of Billboard Hot 100
# songs change over time.

ggplot(yearly, aes(x=year, y=mean_danceability)) +
  geom_line() +
  labs(title="Average Danceability of Billboard Hot 100 Songs (2000-2021)",
    x="Year",
    y="Mean Danceability")
ggplot(yearly, aes(x=year, y=mean_energy)) +
  geom_line() +
  labs(
    title="Average Energy of Billboard Hot 100 Songs (2000-2021)",
    x="year",
    y="Mean Energy")
ggplot(yearly, aes(x=year, y=mean_valence)) +
  geom_line() +
  labs(
    title="Average Valence of Billboard Hot 100 Songs (2000-2021)",
    x="Year",
    y="Mean Valence")

# This section calculates correlation coefficients between song ranking and
# selected audio features to assess the strength and direction of their
# relationships.

cor(music_clean$ranking, music_clean$danceability, use="complete.obs")
cor(music_clean$ranking, music_clean$energy, use="complete.obs")
cor(music_clean$ranking, music_clean$valence, use="complete.obs")

# This section visualises the relationship between danceability and chart
# ranking using a scatter plot with a linear trend line to assess potential
# associations.

ggplot(music_clean, aes(x=danceability, y=ranking)) +
  geom_point(alpha=0.4, colour="steelblue") +
  geom_smooth(method="lm", colour="darkred", se=TRUE) +
  scale_y_reverse() +
  labs(
    title="Danceability and Billboard Hot 100 Chart Position",
    subtitle="Relationship between song danceability and chart ranking",
    x="Danceability",
    y="Chart Ranking (1=highest)"
  ) +
  theme_minimal()

# This section fits a multiple linear regression model in order to examine
# how selected audio features jointly relate to chart ranking, and summarises
# the model results.

model<-lm(ranking~danceability + energy + valence + tempo,
          data=music_clean)
summary(model)

# This section creates a categorical variable to distinguish top 10 songs from
# other charting songs for comparative analysis.

music_clean<-music_clean %>%
  mutate(top10=ifelse(ranking<=10, "Top 10", "Other"))

# This section compares the average audio features between top 10 songs and
# other charting songs to identify differences in musical characteristics.

music_clean %>%
  group_by(top10) %>%
  summarise(
    mean_danceability=mean(danceability),
    mean_energy=mean(energy),
    mean_valence=mean(valence))

# This section visualises and compares the distribution of danceability between
# top 10 songs and other charting songs using a box plot.

ggplot(music_clean, aes(x=top10, y=danceability, fill=top10)) +
  geom_boxplot(alpha=0.7) +
  labs(
    title="Danceability of Billboard Hot 100 Songs: Top 10 vs Other",
    x="Chart Category",
    y="Danceability"
  ) +
  scale_fill_manual(values=c("Top 10" = "steelblue", "Other" = "lightblue")) +
  theme_minimal() +
  theme(legend.position="none")

# This section creates a correlation matrix for selected numerical variables
# to examine relationships between audio features and chart ranking.

cor_features<-music_clean %>%
  select(ranking, danceability, energy, valence, tempo, loudness) %>%
  cor(use="complete.obs")
round(cor_features, 2)

# This section visualises the relationships between energy, valence, and chart
# ranking using scatter plots with linear trend lines to assess potential
# associations.

ggplot(music_clean, aes(x=energy, y=ranking)) +
  geom_point(alpha=0.4, colour="steelblue") +
  geom_smooth(method="lm", colour="darkred", se=TRUE) +
  scale_y_reverse() +
  labs(
    title="Energy and Billboard Hot 100 Chart Position",
    subtitle="Relationship between song energy and chart ranking",
    x="Energy",
    y="Chart Ranking (1=highest)"
  ) +
  theme_minimal()
ggplot(music_clean, aes(x=valence, y=ranking)) +
  geom_point(alpha=0.4, colour="steelblue") +
  geom_smooth(method="lm", colour="darkred", se=TRUE) +
  scale_y_reverse() +
  labs(
    title="Valence and Billboard Hot 100 Chart Position",
    subtitle="Relationship between emotional positivity and chart ranking",
    x="Valence",
    y="Chart Ranking (1=highest)"
  ) +
  theme_minimal()

# This section fits a multiple linear regression model to evaluate how
# danceability, energy, valence, and tempo jointly relate to chart ranking.

model<-lm(
  ranking~danceability + energy + valence + tempo,
  data=music_clean)
summary(model)

# This section converted the regression results into a formatted table using
# the stargaze package.

install.packages("stargazer")
library(stargazer)
stargazer(
  model,
  type="text",
  title="Multiple Linear Regression Results",
  dep.var.labels="Chart Ranking",
  covariate.labels = c("Danceability", "Energy", "Valence", "Tempo"),
  digits=3
)

