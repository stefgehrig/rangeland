library(sf)
library(ggsn)
library(stars)
library(raster)
library(patchwork)
library(tidyverse)
library(furrr)
library(future)

#########################
#### area estimation ####
#########################
shp_projvill <- st_read("data/tnc/Assessment_Villages/assessment_villages.shp") 
shp_projvill <- shp_projvill %>% #choose correct Losirwa: Esilalei ward
  filter(!(Village == "Losirwa" & Ward_Name != "Esilalei"))

###########################################
##### cropland epoch 2015: "2012–2015" ####
###########################################
# own downloaded data from https://glad.umd.edu/dataset/croplands/
c2015 <- raster("data/landsat/Global_cropland_SE_2015.tif")
# crop for area of interest in northern tanzania
e <- as(extent(35.7, 37.3, -5.5, -2.3), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
r1 <- crop(c2015, e)
rm(c2015)

# create simple features object with coordinate system
c2015 <- as_tibble(rasterToPoints(r1))
c2015 <- st_as_sf(c2015, coords = c("x", "y"))
st_crs(c2015) <- st_crs(shp_projvill)
rm(r1)

# find intersections
# tictoc::tic()
# int <- st_intersects(c2015, shp_projvill) # about 20 minutes
# tictoc::toc()
# saveRDS(int, file = "intc_2015.rds")
int <- readRDS("intc_2015.rds")

plan(multisession(workers = 12))
int_dbl <- future_map_dbl(int, function(x){
  if(length(x) == 0){
    NA_real_
  } else{
    x
  }
})

c2015 <- c2015 %>% 
  mutate(village = shp_projvill$Village[int_dbl]) %>% 
  filter(!is.na(village))

# summarise and aggregate
df_crop_pre <- c2015 %>% 
  group_by(village) %>% 
  summarise(share_cropland_pre = mean(Global_cropland_SE_2015))
df_crop_pre <- df_crop_pre %>% 
  left_join(tibble(area = as.numeric(st_area(shp_projvill)) * 0.0001,  # 1 m2 = 0.0001 ha
       village = shp_projvill$Village)
  ) %>% 
  mutate(area_cropland_pre = area * share_cropland_pre) %>% 
  mutate(village = case_when(
    village == "Olchoroonyokie" ~ "Olchoro Onyokie",
    village == "Ngoswak" ~ "Ngoswaki",
    village == "Engaruka chini" ~ "Engaruka Chini",
    TRUE ~ village
  ))
saveRDS(df_crop_pre, file = "intc_2015_calculated_df.rds")

# CHECK: compare to Nathan's provided data
df_nathan <- read_csv("data_processed/df_landuse_area.csv")
df_nathan <- df_nathan %>%
  filter(type == "crop", period == "pre") %>%
  left_join(df_crop_pre)

ggplot(df_nathan) +
  geom_point(aes(x = total_area,
                 y = area)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_text(aes(x = total_area,
                y = area,
                label = village))

# nearly perfect fit, but 2 villages divert implausibly in the (total) areas estimates.. (when looking at my map)
# -> how to proceed?
