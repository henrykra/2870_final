library(tidyverse)
library(sf)
pitches <- read.csv("data/2018_pitches_full.csv")

sz_width <- 17/12
sz_height <- mean(pitches$sz_top, na.rm = T) - mean(pitches$sz_bot, na.rm = T)
sz_offset <- mean(pitches$sz_bot, na.rm = T)



# creating spacial objects for 9 sections of the average strike zone
sz_points <-
  data.frame(
    x = rep(c(-sz_width/2, sz_width/3 - sz_width/2, sz_width*2/3 - sz_width/2, sz_width/2), times=4),
    y = rep(c(sz_offset, sz_offset + sz_height / 3, sz_offset + sz_height * 2/3, sz_offset + sz_height), each=4)
  )


sz_origins <- data.frame(x = rep(-1, 9), z = rep(-1, 9))
counter <- 0
for (i in 1:(nrow(sz_points)-4)){
  if (i %% 4 != 0){
    counter <- counter + 1
    sz_origins[counter,] <- sz_points[i,] 
  }
}
# want to have a dataframe with 9 groups of 5 points

# initialize
sz_map <- data.frame(x = rep(-1,45), z = rep(-1, 45), section = rep(1:9, each = 5))
counter <- 0
# for each strike zone area, list the corner points, looping back around to the starting point
for (i in 1:nrow(sz_origins)){
  sz_map[1 + counter,1:2] <- sz_origins[i,]
  sz_map[2 + counter,1:2] <- c(sz_origins[i,1] + sz_width/3, sz_origins[i,2])
  sz_map[3 + counter,1:2] <- c(sz_origins[i,1] + sz_width/3, sz_origins[i,2] + sz_height/3)
  sz_map[4 + counter,1:2] <- c(sz_origins[i,1], sz_origins[i,2] + sz_height/3)
  sz_map[5 + counter,1:2] <- sz_origins[i,]
  
  counter <- counter + 5
}
# for each section, combine the points into a polygon, and then a simple geometry
polys_temp_sz <- rep(-1,9)
for (i in 1:9){
  polys_temp_sz[i] <- 
    sz_map |> 
    filter(section == i) |> 
    select(1,2) |> 
    as.matrix() |> 
    list() |> 
    st_polygon() |> 
    st_geometry()
}


# Creating spatial objects for the areas outside of the strike zone
# Going to be doing this one more manually, because we aren't dealing with rectangles anymore

# Lets only consider pitches that are within 2 feet either side of the middle, and pitches that are above the ground,
# but below 5 feet

section_10 <- rbind(
  c(-2,0),
  c(-sz_width/2, sz_offset),
  c(-sz_width/2, sz_offset + sz_height),
  c(-2,5),
  c(-2,0)
) |> as.matrix() |> list() |> st_polygon() |> st_geometry()
section_11 <- rbind(
  c(-2,5),
  c(-sz_width/2, sz_offset + sz_height),
  c(sz_width/2, sz_offset + sz_height),
  c(2,5),
  c(-2,5)
) |> as.matrix() |> list() |> st_polygon() |> st_geometry()
section_12 <- rbind(
  c(2,5),
  c(sz_width/2, sz_offset + sz_height),
  c(sz_width/2, sz_offset),
  c(2,0),
  c(2,5)
) |> as.matrix() |> list() |> st_polygon() |> st_geometry()
section_13 <- rbind(
  c(-2,0),
  c(-sz_width/2, sz_offset),
  c(sz_width/2, sz_offset),
  c(2,0),
  c(-2,0)
) |> as.matrix() |> list() |> st_polygon() |> st_geometry() 





# final dataframe with each section and its geometry
sz_polys <- 
  st_sf(
    section = 1:13,
    poly = c(polys_temp_sz, section_10, section_11, section_12, section_13)
  ) 
# we can take a look at it like this
ggplot(
  data = sz_polys,
  mapping = aes(label = section)
) + 
  geom_sf(show.legend = F,
          fill = "darkslateblue",
          color = "white") + 
  geom_sf_text(color = "white")


# now to create the map for 2018s pitches

# we must create a column of simple geometries instead of the px, pz pairs

pitches |> 
  filter(!is.na(px),
         !is.na(pz)) |> 
  st_as_sf(coords = c("px", "pz")) |> 
  select(code, stand, geometry) -> p_geom

# now for every pitch, figure out what geometry it is in, if any:
# use the function st_join, works like a normal join, but matches based on where the point falls

st_join(
  x = p_geom,
  y = sz_polys
) |> 
  filter(!is.na(section)) |> 
  tibble() |> 
  group_by(section) |> 
  summarize(num_pitches = n()) |> 
  ungroup() |> 
  left_join(
    y = sz_polys |> 
        mutate(area = st_area(poly)),
    by = "section") -> sections_df


# graph the strike zone map for all batters
ggplot(
  data = st_sf(sections_df),
  mapping = aes(fill = num_pitches/area)
) + 
  geom_sf(
    color = "white"
  ) 

# was this a good idea? idk


pitches_labels <- c("4-Seam Fastball", "Slider", "2-Seam Fastball", "Changeup", "Curveball")

pitches |> 
  select(stand, p_throws, pitch_type, code, px, pz) |> 
  filter(px >= -3 & px <= 3,
         pz >= 0 & pz <= 6,
         pitch_type %in% c("FF", "SL", "FT", "CH", "CU", "SI")) |> 
  ggplot(
    mapping = aes(
      x = px,
      y = pz
    )
  ) + 
  geom_density_2d_filled(
    mapping = aes(fill = after_stat(level)),
    contour_var = "ndensity",
    adjust = .5,
    alpha = .8,
    color = "black",
    linewidth = .1,
    show.legend = F
  ) +
  geom_polygon(
    data = data.frame(
      x = rep(c(-sz_width/2, sz_width/2), each=2),
      y = c(sz_offset + sz_height, sz_offset, sz_offset, sz_offset + sz_height)
    ),
    mapping = aes(x = x,
                  y = y),
    fill = NA,
    color = "black"
  ) + 
  facet_wrap(
    facets = ~pitch_type,
    labeller = labeller(pitch_type = c(FF = "4-Seam Fastball", SL = "Slider", FT = "2-Seam Fastball", CH = "Changeup", CU = "Curveball", SI = "Sinker"))
  ) + coord_equal() + 
  labs(title = "Heat Map By Pitch",
       caption = "View is from behind home plate",
       x = NULL,
       y = NULL) +
  theme(title = element_text(hjust =.5))





pitches |> 
  select(stand, p_throws, pitch_type, code, px, pz) |> 
  filter(px >= -3 & px <= 3,
         pz >= 0 & pz <= 6) |> 
  rename("Pitcher" = p_throws,
         "Batter" = stand) |> 
  ggplot(
    mapping = aes(
      x = px,
      y = pz
    )
  ) + 
  geom_density_2d_filled(
    mapping = aes(fill = after_stat(level)),
    contour_var = "ndensity",
    adjust = .5,
    alpha = .8,
    color = "black",
    linewidth = .1,
    show.legend = F
  ) +
  geom_polygon(
    data = data.frame(
      x = rep(c(-sz_width/2, sz_width/2), each=2),
      y = c(sz_offset + sz_height, sz_offset, sz_offset, sz_offset + sz_height)
    ),
    mapping = aes(x = x,
                  y = y),
    fill = NA,
    color = "black"
  ) + 
  facet_grid(
    rows = vars(Batter),
    cols = vars(Pitcher),
    labeller = label_both
  ) + coord_equal() + 
  labs(
    x = NULL,
    y = NULL,
    title = "Pitch Heatmap by Handedness",
    caption = "View is from behind home plate"
  ) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = .5))

