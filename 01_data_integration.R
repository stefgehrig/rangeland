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
library(psych)
library(DescTools)

############################
#### 9: population data ####
############################
# df9 <- tribble(
#   # all 2012 pop census data from "Populations at Village level 2012 PHC", accessed 2022-08-14 (csv-download defunct)
#   # https://africaopendata.org/dataset/idadi-ya-watu-kwa-ngazi-ya-vijiji-mtaa-kwa-sensa-ya-mwaka-2012/resource/5713f007-4a4d-4b6e-8eba-60d8984216e1?view_id=65c0bace-2cbd-46f7-9278-c82dce5cd8ee
#   
#   # best estimates based on data from file "TNC_Data_Analysis_file_rv1_pop trends.xlsx", overtaking what Majory tells to be the best estimates
#   ~village,   ~pop_census2012,   ~pop_best_estim2012,   ~pop_best_estim2020,
#   
#   "Katikati",         2923,    2923,  3415,
#   "Olchoroonyokie",   2319,    1170,  1661,
#   "Eleng'ata Dapash", 3175,    2040,  5020,
#   "Losirwa",          9360,    NA, NA, # unclear in excel sheet? -> ask majory
#   "Ngoswaki",         3692,    3410,  4635,
#   "Kakoi",            3691,    2553,  2953,
#   "Sangaiwe",         3065,    2886,  3740,
#   "Matale A",         3000,    7600,  9682,
#   "Kimana",           7358,    15000, 17000,
#   "Kitwai A",         2037,    2300,  3018,
#   "Nadonjukin",       2598,    1992,  4005,
#   "Engaruka chini",   5336,    2080,  2800
#   
# )

df9 <- read_csv2("data/populations.csv") # retrieved from Majory's updated file sent in Devember 2022 ("TNC_Data_Analysis_file_rv11.29.22.xlsx")

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
plan(multisession, workers = 15)
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
sf_object    <- shp_projvill
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

df6 <- df6 %>% # specify the period when pre and post was measured according to mail from Nathaniel (2022-12-05)
  mutate(measurement_time = case_when(
    type == "bare" & period == "pre"  ~ "2010-2015",
    type == "bare" & period == "post" ~ "2016-2021",
    type == "crop" & period == "pre"  ~ "2015",
    type == "crop" & period == "post" ~ "2019"
  ))

######################################
#### 1: village household surveys ####
######################################
df1 <- as_tibble(read.dta13("data/tnc_cleandataset.dta"))
df1$village <- str_to_lower(as.character(df1$village))
attributes(df1)
df1$village[df1$village=="engaruka juu"] <- "engaruka chini"

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
df3 <- as_tibble(read.xlsx("data/APPENDIX A4_rv30.11.2022.xlsx"))
names(df3)[names(df3)=="02.1.Invasives"] <- "O2.3.Invasives"
names(df3)[1] <- "village"
df3 <- df3 %>% 
  mutate(village = case_when(
    village == "Engaruka Juu" ~ "engaruka chini",
    village == "Ngoswak" ~ "ngoswaki",
    village == "Eleng'ata" ~ "eleng'ata dapash",
    TRUE ~ str_to_lower(village)
  ))

# do data cleaning in line with Majory's comments (mail from 2022-12-05)
# drop variable with missings where reasonable
df3 <- df3 %>% 
  select(-`A4.2.-.Leadership.authority`) %>%  # The variable is dropped due to its similarity to "A4.1 Leadership accountability." The question was also not well addressed.
  select(-`A1.1.Actor.group.size.(#.of.livestock.keepers)`) %>%  # you could probably just use the alternative variable, "A1.1.1 Actor group size (# of cattle)," instead (r = 0.9)
  select(-`GS6.1.Actor.group.boundary.clarity`)# This variable was not well addressed and not all the intended groups were asked the question. You don't need to worry about it cos it is correlated with other variables - i.e., you can leave it out

# replace variables (with and without missings) where better measures are available
df3$A2.1.Economic.heterogeneity
df3$ECO1.01.Rainfall.patterns
df3$`RS3.1.Commons.spatial.extent.(Ha)`
df3$`S1.1.Human.population.size.change.(Annual.increase)`

# a) economic heterogeneity as gini from household data,
# then gini classified in {1,2,3} scale via code manager instruction: 
# Low: Analogous to a Gini coefficient less than 0.3 
# Medium: Analogous to a Gini coefficient between 0.3 and 0.5 
# High: Analogous to a Gini coefficient greater than 0.5
df1_econ <- df1 %>% 
  mutate(num_cattle_ordered = as.numeric(cattleowned)) %>% 
  select(village,
         # variables where higher values is more wealth
         numhouses,
         improvedtoilet,
         housewalltype,
         housefloortype,
         houserooftype,
         electric_connect,
         num_cattle_ordered,
         num_mobile_phone,
         num_radio,
         num_bicycle, 
         num_motorcycle,
         num_solartorch,
         num_solarpanel,
         num_invertersolar,
         num_solarbattery,
         num_ploughs,
         num_modernbed,
         num_spraypump,
         num_watertank,
         num_land) %>%
  mutate(across(.cols=everything(), ~ifelse(is.na(.x), 0, .x)),
         across(where(is.numeric), ~scale(.x)))

pca_econ <- principal(df1_econ %>% select(-village))
pca_econ$loadings # high correlations consistently with PC1

df1_econ_smry <- df1 %>% 
  bind_cols(wealth = pca_econ$scores) %>%
  mutate(wealth = ((wealth)-min(wealth)) / (max(wealth)-min(wealth))) %>%  # scale individual wealth between 0 and 1
  group_by(village) %>% 
  summarise(gini = Gini(wealth)) %>% 
  arrange(desc(gini)) %>% 
  # mutate(`A2.1.Economic.heterogeneity` = case_when(
  #   gini < 0.3 ~ 1,
  #   gini < 0.5 ~ 2,
  #   gini >= 0.5 ~ 3
  # ))
  rename(`A2.1.Economic.heterogeneity` = gini)

df3 <- df3 %>% 
  select(-`A2.1.Economic.heterogeneity`) %>% 
  left_join(df1_econ_smry)
  
# b) rainfall patterns
df7_rain_smry <- df7 %>% 
  mutate(village = case_when(
    village == "Ngoswak" ~ "ngoswaki",
    village == "Olchoroonyokie" ~ "olchoro onyokie",
    village == "Eleng'ata" ~ "eleng'ata dapash",
    TRUE ~ str_to_lower(village)
  )) %>% 
  filter(year >= 2016) %>% # post period only
  group_by(village) %>% 
  summarise(`ECO1.01.Rainfall.patterns` = mean(mean_annual_precip))

df3 <- df3 %>% 
  select(-`ECO1.01.Rainfall.patterns`) %>% 
  left_join(df7_rain_smry)

# c) commons spatial extent
df3$`RS3.1.Commons.spatial.extent.(Ha)`

df6 <- df6 %>% 
  mutate(village = case_when(
    village == "Ngoswak" ~ "ngoswaki",
    village == "Olchoroonyokie" ~ "olchoro onyokie",
    village == "Eleng'ata" ~ "eleng'ata dapash",
    TRUE ~ str_to_lower(village)
    
  ))

df6_area_smry <- df6 %>% 
  filter(type == "bare", period == "pre") %>% 
  select(village, total_area) %>% 
  rename(`RS3.1.Commons.spatial.extent.(Ha)` = total_area)
  
df3 <- df3 %>% 
  select(-`RS3.1.Commons.spatial.extent.(Ha)`) %>% 
  left_join(df6_area_smry)

# d) population size increase
df9_pop_smry <- df9 %>% 
  mutate(village = case_when(
    village == "Ngoswak" ~ "ngoswaki",
    village == "Olchoroonyokie" ~ "olchoro onyokie",
    village == "Eleng'ata" ~ "eleng'ata dapash",
    village == "Engaruka Juu" ~ "engaruka chini",
    TRUE ~ str_to_lower(village)
  )) %>% 
  mutate(pop_increase = (`2020`-`2012`) / `2012`) %>% 
  select(village, `S1.1.Human.population.size.change.(Annual.increase)` = pop_increase)

df3 <- df3 %>% 
  select(-`S1.1.Human.population.size.change.(Annual.increase)`) %>% 
  left_join(df9_pop_smry)

# replace range of values to be {1,2,3} for social monitoring, according to Majory's advise (mail from 2022-12-05)
df3 <- df3 %>% 
  mutate(`I2.1.Participation.in.social.monitoring.(enforcement)` = 
           case_when(
             `I2.1.Participation.in.social.monitoring.(enforcement)` %in% c(1,2) ~ 1,
             `I2.1.Participation.in.social.monitoring.(enforcement)` %in% c(3)   ~ 2,
             `I2.1.Participation.in.social.monitoring.(enforcement)` %in% c(4)   ~ 3
           ))


# reshape
df3_long <- df3 %>%   
  pivot_longer(cols = 2:ncol(.), names_to = "dimension", values_to = "score") %>% 
  mutate(across(where(is.character), ~str_to_lower(.x)))

# annotate with source and direction, as discussed with Majory and written in her response (mail from 2022-12-13)
df3_long <- df3_long %>% 
  mutate(
    dimension_source = case_when(
      dimension == "rs3.1.commons.spatial.extent.(ha)"                   ~ "total village area estimate from GIS-based land use analysis",
      dimension == "eco1.01.rainfall.patterns"                           ~ "extracted from PERSIANN-CCS database",
      dimension == "a2.1.economic.heterogeneity"                         ~ "household items elicited in household survey",
      dimension == "s1.1.human.population.size.change.(annual.increase)" ~ "combination of censuses and officials' estimates",
      dimension == "a1.1.1.actor.group.size.(#.of.cattle)"               ~ "estimates from interviews with grazing-related committees and district",
      dimension == "a1.1.1.actor.group.size.(#.of.sheep/goats)"          ~ "estimates from interviews with grazing-related committees and district",
      TRUE ~ "coded from qualitative interviews and focus groups" 
    ),
    dimension_coding = case_when(
      dimension == "s1.2.changes.in.ethnic.composition.(village.leader.data)" ~ "ordinal: more rapid change in ethnic composition",
      dimension == "s1.3.changes.in.livelihood.activities"                    ~ "ordinal: more changes in livelihoods",                   
      dimension == "rs2.1.commons.boundaries"                                 ~ "ordinal: clearer boundaries",                                
      dimension == "rs2.2.commons.boundary.negotiability"                     ~ "ordinal: more negotiability",                    
      dimension == "rs5.1.productivity"                                       ~ "ordinal: more productive",                                      
      dimension == "gs2.1.external.support"                                   ~ "ordinal: more external support",                                  
      dimension == "gs3.2.property.security"                                  ~ "ordinal: more property security",                                 
      dimension == "gs4.1.rules-in-use"                                       ~ "binary: in place (2) or not (1)",                                      
      dimension == "gs4.2.governance.strictness.trend"                        ~ "ordinal: trend towards more strictness",                       
      dimension == "gs5.1.external.recognition"                               ~ "ordinal: more recognition",                              
      dimension == "gs5.3.participation.in.rule.making"                       ~ "ordinal: more participation in rules",                     
      dimension == "gs5.4.participation.in.zoning"                            ~ "ordinal: more participation in zoning",                           
      dimension == "gs5.5.commons.political.power"                            ~ "ordinal: more power",                          
      dimension == "gs6.2.outsider.exclusion"                                 ~ "ordinal: more exclsion",                                
      dimension == "gs7.1.environmental.monitoring"                           ~ "ordinal: more environmental monitoring",                         
      dimension == "gs7.2.self.sanctions"                                     ~ "ordinal: less severe sanctions",                                    
      dimension == "gs7.3.external.sanctions"                                 ~ "binary: in place (2) or not (1)",                       
      dimension == "a1.1.1.actor.group.size.(#.of.cattle)"                    ~ "continuous: number of cattle",                  
      dimension == "a1.1.1.actor.group.size.(#.of.sheep/goats)"               ~ "continuous: number of sheep/goats",             
      dimension == "a2.2.interest.heterogeneity"                              ~ "ordinal: more heterogeneity",                             
      dimension == "a4.1.leadership.accountability"                           ~ "ordinal: more accountability",                         
      dimension == "a5.1.actor.group.trust"                                   ~ "ordinal: more intra-trust",                                  
      dimension == "a5.2.inter-group.trust"                                   ~ "ordinal: more inter-trust",                                 
      dimension == "a7.1.economic.dependence"                                 ~ "ordinal: more dependence",                                
      dimension == "a7.2.commons.alternatives"                                ~ "less alternatives",                             
      dimension == "i1.1.conflict.resolution"                                 ~ "binary: in place (2) or not (1)",                                   
      dimension == "i2.1.participation.in.social.monitoring.(enforcement)"    ~ "ordinal: more monitoring participation",  
      dimension == "o1.1.compliance"                                          ~ "ordinal: more compliance",                                         
      dimension == "o2.1.commons.condition.trend"                             ~ "ordinal: more improvement",                           
      dimension == "o2.3.invasives"                                           ~ "ordinal: less problems due to invasives",                                          
      dimension == "a2.1.economic.heterogeneity"                              ~ "continuous: gini coefficient estimated from PCA scores on household items",                            
      dimension == "eco1.01.rainfall.patterns"                                ~ "continuous: average of mean annual precipitation 2016-2020 (mm)",                              
      dimension == "rs3.1.commons.spatial.extent.(ha)"                        ~ "continuous: area (ha)",                    
      dimension == "s1.1.human.population.size.change.(annual.increase)"      ~ "continuous: proportion increase from 2012 to 2022"
    )
  )

# df3_long %>% #... reproduces appendix A4 table
#   group_by(village) %>% 
#   summarise(mean(score, na.rm = TRUE))

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

##################################
#### 8: administrative levels ####
##################################
df8 <- shp_projvill %>% 
  as_tibble %>% 
  select(Village, 
         ward = Ward_Name, 
         district = District_N, 
         region = Region_Nam)

#################################################
#### harmonize village names and export data ####
#################################################
dflist <- map(list(df1, df2, df3_long, df4, 
                   df5, df6, df7, df8, df9), function(x){
  
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
# 
# ################################################################
# #### afrobarometer data Round 5 [2012] + 6 [2015] +7 [2018] ####
# ################################################################
# afro5 <- as_tibble(read.spss("data/afro/tan_r5_data_july_2015.sav", to.data.frame = TRUE))
# afro6 <- as_tibble(read.spss("data/afro/tan_r6_data_eng.sav", to.data.frame = TRUE))
# afro7 <- as_tibble(read.spss("data/afro/tan_r7_data.sav", to.data.frame = TRUE))
# 
# names(afro5) <- attributes(afro5)$variable.labels
# names(afro6) <- attributes(afro6)$variable.labels
# names(afro7) <- attributes(afro7)$variable.labels
# 
# ourregio <- unique(df8$region)
# 
# afro5 %>%
#   group_by(`Province or region`) %>%
#   summarise(n=n()) %>%
#   filter(`Province or region` %in% ourregio) # n = 88, 72 in our regions (6.7% of total)
# 
# afro6 %>%
#   group_by(`Province or Region`) %>%
#   summarise(n=n()) %>%
#   filter(`Province or Region` %in% ourregio) # n = 88, 64 in our regions (6.4% of total)
# 
# afro7 %>%
#   group_by(`Province or region`) %>%
#   summarise(n=n()) %>%
#   filter(`Province or region` %in% ourregio) # n = 88, 64 in our regions (6.3% of total)

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