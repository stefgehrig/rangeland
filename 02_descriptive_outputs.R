library(sf)
library(ggrepel)
library(ggsn)
library(stars)
library(raster)
library(patchwork)
library(tidyverse)
library(rgeos)
library(furrr)
library(sp)
library(extrafont)
loadfonts(device = "win")
select <- dplyr::select
fontfam <- "Segoe UI"

#############
#############
#### map ####
#############
#############
# can choose with or without national park area shown for villages which overlap with NP 
# (for data analysis, NP area is excluded)
shp_projvill <- st_read("data/tnc/Assessment_Villages_2/assessment_villages.shp") 
shp_projvill <- shp_projvill %>% #choose correct Losirwa: Esilalei ward
  filter(!(Village == "Losirwa" & Ward_Name != "Esilalei"))
shp_distr    <- st_read("data/shapes_distr/tza_admbnda_adm2_20181019.shp")

df_projvill_for_plot <- shp_projvill %>% 
  mutate(CENTROID = map(geometry, st_centroid),
         COORDS = map(CENTROID, st_coordinates),
         COORDS_X = map_dbl(COORDS, 1),
         COORDS_Y = map_dbl(COORDS, 2))

df_projvill_for_plot <- df_projvill_for_plot %>% 
  mutate(Village = case_when(
    
    grepl("Eleng", Village)   ~ "Eleng'ata Dapash",
    grepl("ngaruka", Village) ~ "Engaruka Chini",
    grepl("Ngoswak", Village) ~ "Ngoswaki",
    grepl("lchor", Village)   ~ "Olchoro Onyokie",
    TRUE ~ Village
    ))
  
df_distr_for_plot <- shp_distr %>% 
  mutate(CENTROID = map(geometry, st_centroid),
         COORDS = map(CENTROID, st_coordinates),
         COORDS_X = map_dbl(COORDS, 1),
         COORDS_Y = map_dbl(COORDS, 2)) %>% 
  filter(ADM1_EN %in% c("Manyara", "Arusha", "Kilimanjaro", "Mara", "Mwanza", "Dodoma", "Tanga",
                        "Singida", "Tabora", "Shinyanga", "Simiyu"))

vilmap <- ggplot() +
  geom_sf(data = df_distr_for_plot, fill = "grey90", lwd = 0.1, col = "grey50") + 
  geom_sf(data = df_projvill_for_plot, aes(fill = Village), show.legend = FALSE) +
  coord_sf(xlim = c(34.5, 38.5), ylim = c(-5.5, -0.75))  + 
  geom_text_repel(data = df_projvill_for_plot,
                  aes(x = COORDS_X, y = COORDS_Y, label = Village),
                  max.overlaps = 200, size = 4,
                  box.padding = 3,
                  point.padding = 0.1,
                  segment.size = 1/4,
                  family = fontfam) +
  ggsn::scalebar(df_projvill_for_plot,
                 dist = 50,
                 dist_unit = "km",
                 st.size = 3,
                 family = fontfam,
                 anchor = c(x = 38, y = -0.75),
                 location = "topright",
                 transform = TRUE,
                 model = "WGS84") +
  theme_bw(14) + 
  scale_fill_brewer(palette="Set3") + 
  labs(x="",y="") +
  theme(text = element_text(family = fontfam),
        plot.margin = margin(1,-2,0,-3, "cm"))

ggsave("outputs/vilmap.png", plot = vilmap, width = 2200, height = 2000, dpi = 300, units = "px")

##################
#### rainfall ####
##################
df_rainfall     <- read_csv("data_processed/df_rainfall.csv")
vilrain <- ggplot(df_rainfall) +
  geom_line(aes(x = year, y= mean_annual_precip, col = village), lwd = 1) +
  scale_x_continuous(breaks = c(2010:2020)) + 
  theme_bw(14) + 
  scale_color_brewer(palette="Set3") + 
  labs(y="Mean Annual Precipit.",x="Year",col="Village") +
  theme(text = element_text(family = fontfam),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave("outputs/vilrain.png", plot = vilrain, width = 2700, height = 1600, dpi = 300, units = "px")

# #########################
# #########################
# #### tables to shiny ####
# #########################
# #########################
# df_hhsurv       <- read_csv("data_processed/df_hhsurv.csv")
# df_codescores   <- read_csv("data_processed/df_codescores.csv")
# df_landuse_vlup <- read_csv("data_processed/df_landuse_vlup.csv")
# df_landuse_area <- read_csv("data_processed/df_landuse_area.csv")

# df_admlevels    <- read_csv("data_processed/df_admlevels.csv")
# df_popul        <- read_csv("data_processed/df_popul.csv")
# 
# boxplot(df_rainfall$mean_annual_precip ~ df_rainfall$village)
