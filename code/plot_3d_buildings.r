library(sf)
library(tidyverse)
library(mapgl)

snnu1 = read_sf('./data/snnu.gpkg',layer = 'yanta')
snnu2 = read_sf('./data/snnu.gpkg',layer = 'changan')
b1 = read_sf('./data/snnu.gpkg',layer = 'yanta_building')
b2 = read_sf('./data/snnu.gpkg',layer = 'changan_building')

maplibre(
  style = maptiler_style("basic"),
  center = c(unname(st_coordinates(st_centroid(snnu1))[1,1]), 
             unname(st_coordinates(st_centroid(snnu1))[1,2])),
  zoom = 16,
  pitch = 90,
  bearing = -45,
  bounds = snnu1
) |>
  add_vector_source(
    id = "openmaptiles",
    url = paste0("https://api.maptiler.com/tiles/v3/tiles.json?key=",
                 getOption("maptiler.key"))
  ) |>
  add_fill_extrusion_layer(
    id = "3d-buildings",
    source = b1 |> 
      select(-name) |> 
      mutate(bh = if_else(is.na(buildingheright),
                          min(b1$buildingheright,na.rm = TRUE),
                          buildingheright)),
    # source_layer = 'building',
    fill_extrusion_color = interpolate(
      column = 'buildingheright',
      values = c(5, 15, 30),
      stops = c('lightgray', 'royalblue', 'lightblue')
    ),
    fill_extrusion_height = list(
      'interpolate',
      list('linear'),
      list('zoom'),
      15,
      0,
      16,
      list('get', 'buildingheright')
    )
  )