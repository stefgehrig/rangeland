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
shp_projvill <- st_read("data/tnc/Assessment_Villages_2/Assessment_villages_excl_NP.shp") 
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

# # find intersections
# tictoc::tic()
# int <- st_intersects(c2015, shp_projvill) # about 20 minutes
# tictoc::toc()
# saveRDS(int, file = "data/landsat/intc_2015.rds")
# int <- readRDS("data/landsat/intc_2015.rds")
# # ------
# plan(multisession(workers = 12))
# int_dbl <- future_map_dbl(int, function(x){
#   if(length(x) == 0){
#     NA_real_
#   } else{
#     x
#   }
# })
# 
# c2015 <- c2015 %>% 
#   mutate(village = shp_projvill$Village[int_dbl]) %>% 
#   filter(!is.na(village))
# 
# # summarise and aggregate
# df_crop_pre <- c2015 %>% 
#   group_by(village) %>% 
#   summarise(share_cropland_pre = mean(Global_cropland_SE_2015))
# df_crop_pre <- df_crop_pre %>% 
#   left_join(tibble(area = as.numeric(st_area(shp_projvill)) * 0.0001,  # 1 m2 = 0.0001 ha
#        village = shp_projvill$Village)
#   ) %>% 
#   mutate(area_cropland_pre = area * share_cropland_pre) %>% 
#   mutate(village = case_when(
#     village == "Olchoroonyokie" ~ "Olchoro Onyokie",
#     village == "Ngoswak" ~ "Ngoswaki",
#     village == "Engaruka chini" ~ "Engaruka Chini",
#     TRUE ~ village
#   ))
# saveRDS(df_crop_pre, file = "data/landsat/intc_2015_calculated_df.rds")
df_crop_pre <- readRDS(file = "data/landsat/intc_2015_calculated_df.rds")

# CHECK: compare cropland to Nathan's provided data
df_nathan <- read_csv("data_processed/df_landuse_area.csv")
df_nathan <- df_nathan %>%
  filter(type == "crop", period == "pre") %>%
  left_join(df_crop_pre)

ggplot(df_nathan) +
  geom_point(aes(x = area_cropland_pre,
                 y = area_ha)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_text(aes(x = area_cropland_pre,
                y = area_ha,
                label = village)) + 
  scale_x_continuous(trans = "log") + 
  scale_y_continuous(trans = "log")
summary(lm(df_nathan$area_cropland_pre ~ df_nathan$area_ha))  # OK!

# CHECK: compare total area from shape files to Nathan's provided data
ggplot(df_nathan) +
  geom_point(aes(x = total_area,
                 y = area)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_text(aes(x = total_area,
                y = area,
                label = village))+ 
  scale_x_continuous(trans = "log") + 
  scale_y_continuous(trans = "log")
summary(lm(df_nathan$area~ df_nathan$total_area)) # OK!

#######################
##### bare ground #####
#######################
#### PRE
bpre <- raster("data/tnc/geoTiffs/bare_pre.tif")

# crop for area of interest in northern tanzania
e <- as(extent(35.7, 37.3, -5.5, -2.3), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
r1 <- crop(bpre, e)
rm(bpre)

# create simple features object with coordinate system
bpre <- as_tibble(rasterToPoints(r1))
bpre <- st_as_sf(bpre, coords = c("x", "y"))
st_crs(bpre) <- st_crs(shp_projvill)
rm(r1)

tictoc::tic()
intb <- st_intersects(bpre, shp_projvill) # about 20 minutes
tictoc::toc()

intb_dbl <- future_map_dbl(intb, function(x){
  if(length(x) == 0){
    NA_real_
  } else{
    x
  }
})

bpre <- bpre %>%
  mutate(village = shp_projvill$Village[intb_dbl]) %>%
  filter(!is.na(village))

# summarise and aggregate
df_bare_pre <- bpre %>%
  group_by(village) %>%
  summarise(share_bare_pre = mean(Percent_NonVegetated / 100))

df_bare_pre <- df_bare_pre %>%
  left_join(tibble(area = as.numeric(st_area(shp_projvill)) * 0.0001,  # 1 m2 = 0.0001 ha
       village = shp_projvill$Village)
  ) %>%
  mutate(area_bare_pre = area * share_bare_pre) %>%
  mutate(village = case_when(
    village == "Olchoroonyokie" ~ "Olchoro Onyokie",
    village == "Ngoswak" ~ "Ngoswaki",
    village == "Engaruka chini" ~ "Engaruka Chini",
    TRUE ~ village
  ))

df_nathan <- read_csv("data_processed/df_landuse_area.csv")
df_nathan <- df_nathan %>%
  filter(type == "bare", period == "pre") %>%
  left_join(df_bare_pre)

ggplot(df_nathan) +
  geom_point(aes(x = area_bare_pre,
                 y = area_ha)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_text(aes(x = area_bare_pre,
                y = area_ha,
                label = village)) + 
  scale_x_continuous(trans = "log") + 
  scale_y_continuous(trans = "log") # OK!

#### POST
bpre <- raster("data/tnc/geoTiffs/bare_post.tif")

# crop for area of interest in northern tanzania
e <- as(extent(35.7, 37.3, -5.5, -2.3), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
r1 <- crop(bpost, e)
rm(bpost)

# create simple features object with coordinate system
bpost <- as_tibble(rasterToPoints(r1))
bpost <- st_as_sf(bpost, coords = c("x", "y"))
st_crs(bpost) <- st_crs(shp_projvill)
rm(r1)

tictoc::tic()
intb <- st_intersects(bpost, shp_projvill) # about 20 minutes
tictoc::toc()

intb_dbl <- future_map_dbl(intb, function(x){
  if(length(x) == 0){
    NA_real_
  } else{
    x
  }
})

bpost <- bpost %>%
  mutate(village = shp_projvill$Village[intb_dbl]) %>%
  filter(!is.na(village))

# summarise and aggregate
df_bare_post <- bpost %>%
  group_by(village) %>%
  summarise(share_bare_post = mean(Percent_NonVegetated / 100))

df_bare_post <- df_bare_post %>%
  left_join(tibble(area = as.numeric(st_area(shp_projvill)) * 0.0001,  # 1 m2 = 0.0001 ha
                   village = shp_projvill$Village)
  ) %>%
  mutate(area_bare_post = area * share_bare_post) %>%
  mutate(village = case_when(
    village == "Olchoroonyokie" ~ "Olchoro Onyokie",
    village == "Ngoswak" ~ "Ngoswaki",
    village == "Engaruka chini" ~ "Engaruka Chini",
    TRUE ~ village
  ))

df_nathan <- read_csv("data_processed/df_landuse_area.csv")
df_nathan <- df_nathan %>%
  filter(type == "bare", period == "post") %>%
  left_join(df_bare_post)

ggplot(df_nathan) +
  geom_point(aes(x = area_bare_post,
                 y = area_ha)) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_text(aes(x = area_bare_post,
                y = area_ha,
                label = village)) + 
  scale_x_continuous(trans = "log") + 
  scale_y_continuous(trans = "log") # OK!

#####
##### RELATIONSHIP OF THE TWO DATA SOURCES?
#####
bpre <- raster("data/tnc/geoTiffs/bare_pre.tif")
cpre <- raster("data/landsat/Global_cropland_SE_2015.tif")

e <- as(extent(35.5, 36.5, -5.25, -4.5), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"

r1 <- crop(bpre, e)
r2 <- crop(cpre, e)

r1 # resolution : 0.002245788, 0.002245788  (x, y)
r2 # resolution : 0.0002694946, 0.0002694946  (x, y)

r2resampled <- projectRaster(r2, r1, method = 'ngb')

plot(r1)
plot(r2)
plot(r2resampled)

br <- corLocal(r1, r2resampled, ngb = 99, test = TRUE)
plot(br) # strongly positive correlation in most areas!
br # strongly positive correlation in most areas!

# global pixel-wise comparison
left_join(
  as_tibble(rasterToPoints(r1)),
  as_tibble(rasterToPoints(r2resampled))
) %>% 
  ggplot() + 
  geom_boxplot(aes(x = factor(Global_cropland_SE_2015), y = Percent_NonVegetated))

with(
  left_join(
    as_tibble(rasterToPoints(r1)),
    as_tibble(rasterToPoints(r2resampled))
  ),
  cor(Global_cropland_SE_2015, Percent_NonVegetated)
)
cor()

# create graph for explaining to collaborators

