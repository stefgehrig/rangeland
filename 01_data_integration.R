library(readstata13)
library(openxlsx)
library(sf)
library(foreign)
library(ggrepel)
library(ggsn)
library(stars)
library(raster)
library(patchwork)
library(tidyverse)
library(rgeos)
library(furrr)
library(sp)

######################################
#### 1: village household surveys ####
######################################
df1 <- as_tibble(read.dta13("data/tnc_cleandataset.dta"))
df1$village <- str_to_lower(as.character(df1$village))

##################################
#### 2: villages coded binary ####
##################################
df2 <- as_tibble(read.xlsx("data/TNC_RangelandMGT_codesbyvillage.xlsx"))
names(df2) <- str_replace(names(df2), "\\.Gr=.*","")
names(df2) <- str_replace_all(names(df2), "[^[:alnum:]]", " ")
names(df2) <- str_replace_all(names(df2), "    ", " ")
names(df2) <- str_replace_all(names(df2), "   ", " ")
names(df2) <- str_replace_all(names(df2), "  ", " ")
names(df2) <- trimws(names(df2))
names(df2)[1] <- "village"

df2$village <- trimws(str_replace_all(df2$village, "[^[:alnum:]]", " "))
df2$village <- str_replace(df2$village," Village.*","")
df2 <- df2 %>% filter(village != "Totals") %>% select(-Totals)
df2 <- df2 %>% 
  pivot_longer(cols = 2:ncol(.), names_to = "code", values_to = "val_binary")


#### code manager
df_cods <- as_tibble(read.xlsx("data/TNC_Rangeland MGT_Code Manager.xlsx"))

names(df_cods) <- str_to_lower(names(df_cods))
df_cods$code <- trimws(str_replace_all(df_cods$code, "[^[:alnum:]]", " "))
df_cods$code <- str_replace_all(df_cods$code, "   ", " ")
df_cods$code <- str_replace_all(df_cods$code, "  ", " ")
df_cods$comment <- str_remove_all(df_cods$comment, "\u2029")
df_cods <- df_cods %>% select(-x1)

#### merge them ####
df2 <- df2 %>% 
  left_join(df_cods, by = "code")  %>% 
  mutate(across(where(is.character), ~str_to_lower(.x)))

##################################
#### 3: villages coded scores ####
##################################
df3 <- as_tibble(read.xlsx("data/APPENDIX A4.xlsx"))
names(df3)[names(df3)=="02.1.Invasives"] <- "O2.1.Invasives"
names(df3)[names(df3)=="A4.2.-.Leadership.authority"] <- "A4.2.Leadership.authority"
names(df3)[1] <- "village"

# add those that are not yet coded, so that them missing is obvious
df3 <- df3 %>% 
  mutate(I2.2.particip.social.monitor = NA)

df3 <- df3 %>% 
  pivot_longer(cols = 2:ncol(.), names_to = "dimension", values_to = "score") %>% 
  mutate(across(where(is.character), ~str_to_lower(.x)))

df3 %>% #... reproduces appendix A4 table
  group_by(village) %>% 
  summarise(mean(score, na.rm = TRUE))

####################################
#### 4: village land cover perc ####
####################################
df4 <- read_csv2("data/geodata.csv") %>% 
  rbind(
    tibble(
      Village = "Katikati",
      `% bare 2010-15` = NA,
      `% bare 2015-20` = NA,
      `% agriculture 2015` = NA,
      `% agriculture 2020` = NA,
      Notes = NA,
      `SFTZ 2021 info` = NA
    )
  )

df4 <- df4 %>% dplyr::select(village = Village, contains("%")) %>% 
  pivot_longer(cols = contains("%"), names_to = "var", values_to = "cover_percent") %>% 
  mutate(var = str_sub(var, 3,100)) %>% 
  separate(var, " ", into = c("land_type", "period"))

#############################################################
#### 5: village land use plan (VLUP) indicator variables ####
#############################################################
# from e-mail Philipo Lukumay <philipo.lukumay@TNC.ORG> (2022-08-12, 21:31)
df5 <- tribble(
  
  ~village,    ~vlup,
  
  "Katikati",         "RZMP for WMA",
  "Olchoro Onyokie",  "No",
  "Eleng'ata Dapash", "No",
  "Losirwa",          "Yes", # Monduli district, Esilalei ward
  "Ngoswak",          "Yes",
  "Kakoi",            "RZMP for WMA",
  "Sangaiwe",         "RZMP for WMA",
  "Matale A",         "Yes",
  "Kimana",           "Yes", # less than 150 Ha
  "Kitwai A",         "Yes",
  "Nadonjukin",       "Yes",
  "Engaruka Chini",   "Yes"
  
)

################################################
#### 6: village land use areas pre and post ####
################################################
df6a <- read_csv("data/tnc/village_bare_ground_v2.csv") %>%
  #choose correct Losirwa: Esilalei ward
  filter(!(Village == "Losirwa" & Ward_Name != "Esilalei"))
df6b <- read_csv("data/tnc/village_crop_v2.csv") %>%
  #choose correct Losirwa: Esilalei ward
  filter(!(Village == "Losirwa" & Ward_Name != "Esilalei"))

df6 <- map_dfr(list(df6a, df6b), function(x){
  
  names(x) <- str_to_lower(names(x))
  x %>% rename(total_area = area) %>% 
    pivot_longer(contains("pre") | contains("post"),
                     names_to = "val",
                     values_to = "area_ha")
  
}) %>% 
  mutate(val = str_replace(val, "p", "//.p")) %>% 
  separate(val, "//.", into = c("type", "period")) %>% 
  mutate(type = ifelse(grepl("cro", type), "crop", "bare"),
         period = ifelse(grepl("pre", period), "pre", "post")) %>% 
  select(-district, -region_nam, -ward_name)

###################################
#### 7: rainfall data yearwise ####
###################################
# shp_distr <- st_read("data/shapes_distr/tza_admbnda_adm2_20181019.shp")
# shp_distr <- shp_distr  %>% 
#   filter(ADM1_EN %in% c("Manyara", "Arusha")) # reduce roughly to necessary areas
# write_sf(shp_distr, "data_processed/shp_proj_distr.geojson") # export wards only to get rainfall on ward-level from PERSIANN-CDR

# to extract from netcdf: mm per annum for each year 2010 to 2020 for each ward
# from PERSIANN-CCS (https://chrsdata.eng.uci.edu/)
precipdata <- metR::ReadNetCDF("data/rainfall/CCS_shp_proj_distr_2022-08-13053147am.nc")

precipdata <- precipdata %>% 
  as_tibble() %>% 
  mutate(datetime = lubridate::year(datetime)) %>% 
  filter(precip != -99) %>% 
  select(lon, lat, datetime, precip) 
precipdata <- st_as_sf(precipdata, coords = c("lon", "lat"))

shp_projvill <- st_read("data/tnc/Assessment_Villages_2/assessment_villages.shp") # need to match polygons from district rainfall to villages
shp_projvill <- shp_projvill %>% #choose correct Losirwa: Esilalei ward
  filter(!(Village == "Losirwa" & Ward_Name != "Esilalei"))

st_crs(precipdata) <- st_crs(shp_projvill)
plan(multisession, workers = 14)
precipdata$vill_rownumber <- future_map_dbl(1:nrow(precipdata), function(x){
  sf_use_s2(FALSE)
  as.numeric(st_intersects(precipdata[x,], shp_projvill))
}, .progress = TRUE)

precipdata <- precipdata %>% 
  mutate(village = shp_projvill$Village[vill_rownumber]) %>% 
  filter(!is.na(village))

# ... just for checking the overlap: plot the precipitation from that netcdf file
named_vills <- ggplot(precipdata) +
  geom_sf(aes(col = factor(village))) + 
  coord_sf(xlim = c(34.5, 38.25), ylim = c(-6, -1))

# ... and compare
stars_object <- raster("data/rainfall/CCS_shp_proj_distr_2022-08-13053147am.nc")%>% st_as_stars()
sf_object    <- sf::st_read("data/tnc/Assessment_Villages/assessment_villages.shp")
stars_object$precip[stars_object$precip==-99] <- NA
sf::st_crs(stars_object) <- sf::st_crs(stars_object)
rain_vills <- ggplot() +
  geom_sf(data = sf_object) + 
  geom_stars(data = stars_object, alpha = 0.75)

named_vills + rain_vills # CHECK!

# summarise and aggregate
precipdata_agr <- precipdata %>% 
  group_by(village, datetime) %>% 
  summarise(mean_annual_precip = mean(precip),
            .groups = "drop")

df7 <- precipdata_agr %>%
  as_tibble %>% 
  select(village,
         year=datetime,
         mean_annual_precip)

##################################
#### 8: administrative levels ####
##################################
df8 <- shp_projvill %>% 
  as_tibble %>% 
  select(Village, 
         ward = Ward_Name, 
         district = District_N, 
         region = Region_Nam)

############################
#### 9: population data ####
############################
df9 <- tribble(
  # from "Populations at Village level 2012 PHC", accessed 2022-08-14 (csv-download defunct)
  # https://africaopendata.org/dataset/idadi-ya-watu-kwa-ngazi-ya-vijiji-mtaa-kwa-sensa-ya-mwaka-2012/resource/5713f007-4a4d-4b6e-8eba-60d8984216e1?view_id=65c0bace-2cbd-46f7-9278-c82dce5cd8ee
  ~village,  ~pop_cens2012, ~pop_leader_postperiod,
  
 "Katikati",         2923, NA,
 "Olchoroonyokie",   2319, NA,
 "Eleng'ata Dapash", 3175, NA,
 "Losirwa",          9360, NA,
 "Ngoswaki",         3692, NA,
 "Kakoi",            3691, NA,
 "Sangaiwe",         3065, NA,
 "Matale A",         3000, NA,
 "Kimana",           7358, NA,
 "Kitwai A",         2037, NA,
 "Nadonjukin",       2598, NA,
 "Engaruka chini",   5336, NA
  
  
)

# fehlt noch eine spalte! ! ! 

#################################################
#### harmonize village names and export data ####
#################################################
dflist <- map(list(df1, df2, df3, df4, df5, df6, df7, df8, df9), function(x){
  
  names(x) <- str_to_lower(names(x))
  
  dat <- x %>% 
    mutate(village = str_to_lower(village),
           village = ifelse(grepl("olchor", village), "olchoro onyokie", village),
           village = ifelse(grepl("eleng", village), "eleng'ata dapash", village),
           village = ifelse(grepl("ngaruka", village), "engaruka chini", village),
           village = ifelse(grepl("ngoswa", village), "ngoswaki", village)) %>% 
    
    mutate(village = str_to_title(village))
  
  stopifnot(

    # check for consistency in desired village name spellings in column "village"
    "Matale A"          %in% dat$village &
    "Losirwa"           %in% dat$village &
    "Ngoswaki"          %in% dat$village &
    "Olchoro Onyokie"   %in% dat$village &
    "Eleng'ata Dapash"  %in% dat$village &
    "Nadonjukin"        %in% dat$village &
    "Kitwai A"          %in% dat$village &
    "Kimana"            %in% dat$village &
    "Katikati"          %in% dat$village &
    "Sangaiwe"          %in% dat$village &
    "Kakoi"             %in% dat$village &
    "Engaruka Chini"    %in% dat$village
    
  )
  
  return(dat)
})

df1 <- dflist[[1]]
df2 <- dflist[[2]]
df3 <- dflist[[3]]
df4 <- dflist[[4]]
df5 <- dflist[[5]]
df6 <- dflist[[6]]
df7 <- dflist[[7]]
df8 <- dflist[[8]]
df9 <- dflist[[9]]

write_csv(df1, "data_processed/df_hhsurv.csv")
write_csv(df2, "data_processed/df_codelists.csv")
write_csv(df3, "data_processed/df_codescores.csv")
write_csv(df4, "data_processed/df_landuse_perc.csv")
write_csv(df5, "data_processed/df_landuse_vlup.csv")
write_csv(df6, "data_processed/df_landuse_area.csv")
write_csv(df7, "data_processed/df_rainfall.csv")
write_csv(df8, "data_processed/df_admlevels.csv")
write_csv(df9, "data_processed/df_popul.csv")

################################################################
#### afrobarometer data Round 5 [2012] + 6 [2015] +7 [2018] ####
################################################################
afro5 <- as_tibble(read.spss("data/afro/tan_r5_data_july_2015.sav", to.data.frame = TRUE))
afro6 <- as_tibble(read.spss("data/afro/tan_r6_data_eng.sav", to.data.frame = TRUE))
afro7 <- as_tibble(read.spss("data/afro/tan_r7_data.sav", to.data.frame = TRUE))

names(afro5) <- attributes(afro5)$variable.labels
names(afro6) <- attributes(afro6)$variable.labels
names(afro7) <- attributes(afro7)$variable.labels

ourregio <- unique(df8$region)

afro5 %>%
  group_by(`Province or region`) %>%
  summarise(n=n()) %>%
  filter(`Province or region` %in% ourregio) # n = 88, 72 in our regions (6.7% of total)

afro6 %>%
  group_by(`Province or Region`) %>%
  summarise(n=n()) %>%
  filter(`Province or Region` %in% ourregio) # n = 88, 64 in our regions (6.4% of total)

afro7 %>%
  group_by(`Province or region`) %>%
  summarise(n=n()) %>%
  filter(`Province or region` %in% ourregio) # n = 88, 64 in our regions (6.3% of total)

##############################
#### Control area results ####
##############################
### calculate bareground and cropaldn pre and post cover for any other villages/areas in the landscape via using the
### raster files!
# crop_post_landscape <- raster(x = "data/tnc/geoTiffs/crop_post.tif")
# plot(crop_post_landscape)
# sources:
#https://glad.umd.edu/dataset/croplands (crop)
#https://modis.gsfc.nasa.gov/data/dataprod/mod44.php (bare)