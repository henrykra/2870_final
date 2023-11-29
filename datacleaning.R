library(tidyverse)
pitches <- read.csv("data/pitches.csv")
abs <- read.csv("data/atbats.csv")
names <- read.csv("data/player_names.csv")
games <- read.csv("data/games.csv")

p_2018 <- 
  pitches |> 
  left_join(
    y = abs,
    by = "ab_id"
  ) |> 
  filter(
    g_id >= 201800000
  )

p_2018_names <-
  p_2018 |> 
  left_join(
    y = names,
    by = c("batter_id" = "id"),
    keep = F
  )

p_2018_names_2 <-
  p_2018_names |> 
  mutate(batter_name = paste(first_name, last_name))

p_2018_names_3 <-
  p_2018_names_2 |> 
  left_join(
    y = names,
    by = c("pitcher_id" = "id"),
    keep = F
  )

pitches_2018_full <-
  p_2018_names_3 |> 
  mutate(pitcher_name = paste(first_name.y, last_name.y)) |> 
  select(-first_name.x, -first_name.y, -last_name.x, -last_name.y)

write.csv(pitches_2018_full, "data/2018_pitches_full.csv", row.names = F, quote = F)


