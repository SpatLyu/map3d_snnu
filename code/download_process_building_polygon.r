library(sf)

# st_layers('./data/snnu.gpkg')

snnu1 = read_sf('./data/snnu.gpkg',layer = 'yanta')
snnu2 = read_sf('./data/snnu.gpkg',layer = 'changan')

snnubb = purrr::map(list(snnu1,snnu2),
                    \(sfj) as.double(sf::st_bbox(sfj)))

.getbuilding = \(sfj,bb){
  osmdata::opq(bbox = bb) %>% 
    osmdata::add_osm_feature(key = 'building') %>% 
    osmdata::osmdata_sf() %>% 
    .$osm_polygons %>% 
    dplyr::select(name) %>% 
    sf::st_make_valid() %>% 
    sf::st_intersection(sfj)
}

b1 = .getbuilding(snnu1,snnubb[[1]])
b2 = .getbuilding(snnu2,snnubb[[2]])

mapview::mapview(b1)
mapview::mapview(b2)