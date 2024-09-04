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
    sf::st_intersection(sfj) %>% 
    tibble::as_tibble() %>% 
    sf::st_as_sf()
}

b1 = .getbuilding(snnu1,snnubb[[1]]) %>% 
  st_collection_extract()
b2 = .getbuilding(snnu2,snnubb[[2]]) %>% 
  st_collection_extract()

mapview::mapview(b1)
mapview::mapview(b2)

url_cnbh = paste0('https://zenodo.org/records/7923866/files/CNBH10m_X',
                  109, 'Y', 35, '.tif?download=1')
download.file(url = url_cnbh, mode = 'wb',
              destfile = paste0('./data/',
                                stringr::str_sub(url_cnbh,-22,-16),
                                '.tif'))

library(terra)
library(magrittr)
bh = rast('./data/X109Y35.tif')

.getbuildingheight = \(.sfj,.bh){
  .sfj = project(vect(.sfj),crs(.bh))
  bh_raster = crop(.bh,.sfj,mask = TRUE)
  bh_vector = zonal(bh_raster,.sfj,fun = 'mean',na.rm = TRUE) |> 
    dplyr::pull(1)
  return(bh_vector)
}

b1 = b1 %>% 
  dplyr::mutate(buildingheright = .getbuildingheight(.,bh)) %>% 
  tibble::as_tibble() %>% 
  sf::st_as_sf() %>% 
  dplyr::select(name,buildingheright)

b2 = b2 %>% 
  dplyr::mutate(buildingheright = .getbuildingheight(.,bh)) %>% 
  tibble::as_tibble() %>% 
  sf::st_as_sf() %>% 
  dplyr::select(name,buildingheright)

write_sf(b1,'./data/snnu.gpkg',layer = 'yanta_building')
write_sf(b2,'./data/snnu.gpkg',layer = 'changan_building')