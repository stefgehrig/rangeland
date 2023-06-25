# describe between-village variation visually
# in different ways, but no hard explanatory analysis yet and no GIS outcomes
library(tidyverse)
library(glue)
library(extrafont)
library(patchwork)
library(ggrepel)
library(janitor)
library(ggiraphExtra)
library(RColorBrewer)
loadfonts()
source("00_functions.R")
fontfam <- "Segoe UI"
palcolors <- colorRampPalette(brewer.pal(11, "Spectral"))(7)

###################
#### load data ####
###################
map(list.files("data_processed", ".csv"), function(x){
  assign(x = str_remove(x, ".csv"), value = read_csv(glue("data_processed/{x}")), envir = .GlobalEnv)
})

df <- name_the_tiers(df_codescores)

##############################################################
#### bring low-tier variables on their scale for analysis ####
##############################################################
# scale all in range [0;1] and do not throw away any information
df <- df %>% 
  group_by(tier3) %>% 
  mutate(score = case_when(
    grepl("continuous", dimension_coding) ~ (score-min(score))/(max(score)-min(score)), # the min/max is what is in the data (those variables are created by Stefan)
    grepl("binary", dimension_coding)     ~ score-1,                                    # the min/max is what is what is given by content analysis coding (those variables are created by Majory)
    grepl("ordinal", dimension_coding) & score ==1 ~ 0,                                 # the min/max is what is what is given by content analysis coding "
    grepl("ordinal", dimension_coding) & score ==2 ~ 1/2,                               # the min/max is what is what is given by content analysis coding "
    grepl("ordinal", dimension_coding) & score ==3 ~ 1                                  # the min/max is what is what is given by content analysis coding "
  )) %>% 
  ungroup

###########################################################
#### remove those irrelevant for SES due to redundancy ####
###########################################################
df <- df %>% 
  filter(tier3 != "A1.1: Number of relevant actors (# Sheep/goats)") %>% 
  mutate(tier3 = ifelse(tier3=="A1.1: Number of relevant actors (# Cattle)", "A1.1: Number of relevant actors", tier3))

##############################################################################
#### all tier1 groups can be summarized due to directionality concordance ####
##############################################################################
# - Actors:
# LESS cattle + LESS econ inequality + LESS interest heterogeneity + MORE leadership accountatbility + MORE intra trust + MORE inter trust
# - Governance Systems:
# stricter rules, more participatory rules
# - Interactions:
# MORE conflict resolution in place + MORE social monitoring participation
# - Outcome:
# MORE compliance, BETTER conditions trend, LESS invasives
# - related ecosystems:
# MORE rainfall
# - resource system
# CLEARER boundaries + MORE negtiabley + MORE productive + MORE space of land
# - social setting
# LESS RAPID ethnic change, LESS RAPID population increase, LESS change in livelihoods
df_grouped_long <- df %>% 
  group_by(village, tier1) %>% 
  summarise(avg = mean(score),
            .groups = "drop")

df_grouped <- df_grouped_long%>% 
  pivot_wider(names_from = tier1, values_from = avg)

# ##########################
# #### land use changes ####
# ##########################
# # compute relative differences and log-transform them
# lup_ratios <- df_landuse_area %>% 
#   group_by(village, type, period) %>% 
#   summarise(landprop = (total_area-area_ha)/total_area, # NON bare and NON agricul
#             .groups = "drop_last") %>% 
#   summarise(pre_landprop  = landprop[period == "pre"],
#             post_landprop = landprop[period == "post"],
#             .groups = "drop") %>% 
#   mutate(ratio = (post_landprop / pre_landprop - 1)*100) %>%  # higher value here means LESS bare ground
#   select(-contains("landprop")) %>% 
#   pivot_wider(names_from = type, values_from = ratio, names_prefix = "ratio_") %>% 
#   rename("Change in\nfertile ground (%)" = ratio_bare)

####################
#### PCA Biplot ####
####################
library(FactoMineR)
library(factoextra)
vilpca <- PCA(
  X = df_grouped %>% 
    select(-village, -`Outcomes (O)`),
  scale.unit = TRUE, #  so even categories with little variation between villages get the same weight as all others in decomposition
  # but this is necessary, otherwise the single category ECO has so much weight, since we MUST scale it from min(data) to max(data)
  # as there is no coding scheme
  ncp = Inf,
  graph = FALSE)

rownames(vilpca$ind$coord) <- df_grouped$village

baseplot <- fviz_pca_biplot(vilpca,
                col.ind = NA,
                title = "") + 
  theme_classic(14) +
  labs(x = glue("PC1 ({format(round(vilpca$eig[,'percentage of variance'][1],1),nsmall=1)}%)"),
       y = glue("PC2 ({format(round(vilpca$eig[,'percentage of variance'][2],1),nsmall=1)}%)")) + 
  scale_x_continuous(limits = c(-4,3)) +
  scale_y_continuous(limits = c(-2.5,2.25)) +
  geom_point(
    data = as_tibble(vilpca$ind$coord, rownames = "village"),
    aes(x = Dim.1,
        y = Dim.2)
  ) 

# outcome interviews
p1 <- baseplot + 
  geom_label_repel(
    data = as_tibble(vilpca$ind$coord, rownames = "village") %>% 
      bind_cols(`Outcomes (O)` = df_grouped$`Outcomes (O)`),
    aes(x = Dim.1,
        y = Dim.2,
        label = village,
        fill = `Outcomes (O)`),
    alpha = 0.5,
    size =3.25,
    seed = 7654
  ) + 
  scale_fill_gradient(low = "coral", high = "darkgreen") + 
  guides(fill = guide_colourbar(barheight = 6))

png("outputs/pca_descr.png", width = 3100, height = 2600, res = 375)
p1
dev.off()

############################################
#### explanations and correlations plot ####
############################################
# ggplot(df_grouped_long) + 
#   geom_jitter(aes(x = tier1, y = avg,
#                   col = village), size = 2, alpha = 0.75, height = 0, width = 0.1) +
#   theme_classic(14) + 
#   theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
#   labs(x = "", y = "Average score", col = "Village") +
#   scale_color_manual(values = palcolors)

# lowest row
p_zero = df_grouped_long %>% 
  ggplot() + 
  geom_dotplot(aes(x = avg, fill = tier1), show.legend = FALSE, 
               binwidth=0.1, method='histodot')  +
  facet_wrap(~tier1, ncol = 7) + 
  theme_classic(14) + 
  labs(x = "Average score",
       y = "") + 
  scale_x_continuous(breaks = c(0,1/2,1), limits = c(0,1)) + 
  theme(strip.background = element_rect(fill = "grey80", linewidth = NA),
        strip.text = element_text(size = 8, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  scale_fill_manual(values = palcolors)

png("outputs/avg_scores_dist.png", width = 3000, height = 750, res = 200)
p_zero
dev.off()



























# # other rows
# plotlist <- map(sort(unique(df$tier3)), function(x){
# 
#   dat = df %>% 
#     filter(tier3 == x) 
#    
#    col = palcolors[levels(df$tier1) == unique(dat$tier1)]
# 
#    dat %>% 
#     ggplot() + 
#     geom_dotplot(aes(x = score), 
#                  alpha = 0.5,
#                  binwidth=0.1, fill = col, method='histodot')  +
#     theme_classic(14) + 
#     labs(x = "",
#          y = "",
#          subtitle = x) + 
#     scale_x_continuous(breaks = c(0,1/2,1), limits = c(0,1)) + 
#     theme(plot.margin = unit(c(0,0,0,0), "cm"),
#           plot.subtitle = element_text(size = 8),
#           axis.text.y = element_blank(),
#           axis.ticks.y = element_blank())
# })
# names(plotlist) = sort(unique(df$tier3))
# names(plotlist)
# 
# layout <- "
# ##T####
# ##S####
# ##R####
# ##Q####
# ##P####
# ##O####
# ##N####
# ##M####
# #HL####
# DGK####
# CFJ####
# BEI####
# #######
# AAAAAAA
# "
# png("akk.png", width = 4000, height = 10000, res = 300)
# p_zero + 
#   # S
#   plotlist$`S1.1: Change in human population size` +
#   plotlist$`S1.2: Changes in ethnic composition` + 
#   plotlist$`S1.3: Changes in livelihood activities` +
#   # RS
#   plotlist$`RS2.1: Commons boundaries` + 
#   plotlist$`RS2.2: Commons boundaries negotiability` +
#   plotlist$`RS3.1: Commons spatial extent` +
#   plotlist$`RS5.1: Productivity` +
#   # GS
#   plotlist$`GS2.1: External support` + 
#   plotlist$`GS3.2: Property security`+ 
#   plotlist$`GS4.1: Rules-in-use` + 
#   plotlist$`GS4.2: Governance strictness trend` + 
#   plotlist$`GS5.1: External recognition` + 
#   plotlist$`GS5.3: Participation in rule making` + 
#   plotlist$`GS5.4: Participation in zoning` +
#   plotlist$`GS5.5: Commons political power` +
#   plotlist$`GS6.2: Outsider exclusion` +
#   plotlist$`GS7.1: Environmental monitoring` + 
#   plotlist$`GS7.2: Self sanctions` +
#   plotlist$`GS7.3: External sanctions` +
#   # A
#   plotlist$`A1.1: Number of relevant actors` + 
#   plotlist$`A2.1: Economic heterogeneity` + 
#   plotlist$`A2.2: Interest heterogeneity` +
#   plotlist$`A4.1: Leadership accountability` +
#   plotlist$`A5.1: Actor group trust` +
#   plotlist$`A5.2: Inter-group trust` +
#   plotlist$`A7.1: Economic dependence` + # 26
#   plotlist$`A7.2: Commons alternatives` +
#   # I
#   plotlist$`I1.1: Conflict resolution` +
#   plotlist$`I2.1: Participation in social monitoring` +
#   # ECO
#   plotlist$`ECO1.1: Rainfall patterns` +
#   #O
#   plotlist$`O1.1: Compliance` +
#   plotlist$`O2.1: Commons condition trend` +
#   plotlist$`O2.3: Invasives` +
#   
#   plot_layout(design = layout)
# dev.off()
