library(tidyverse)
library(glue)
library(brms)
library(extrafont)
library(patchwork)
library(ggrepel)
library(ggcorrplot)
library(ggiraphExtra)
library(RColorBrewer)
loadfonts()
source("00_functions.R")
fontfam <- "Segoe UI"
palcolors <- colorRampPalette(brewer.pal(11, "Spectral"))(12)

###################
#### load data ####
###################
map(list.files("data_processed", ".csv"), function(x){
  assign(x = str_remove(x, ".csv"), value = read_csv(glue("data_processed/{x}")), envir = .GlobalEnv)
})

##############################
#### Some data processing ####
##############################
# create tier 1 names
df_codescores <- df_codescores %>% 
  mutate(tier1 = sub("\\d.*", "", dimension),
         tier1 = case_when(
           tier1 == "s"   ~ "Social, Economic, and Political Settings (S)",
           tier1 == "rs"  ~ "Resource Systems (RS)",
           tier1 == "gs"  ~ "Governance Systems (GS)",
           tier1 == "a"   ~ "Actors (A)",
           tier1 == "o"   ~ "Outcomes (O)",
           tier1 == "eco" ~ "Related Ecosystems (ECO)",
           tier1 == "i"   ~ "Interactions (I)"
         ))

# create tier 2 names
df_codescores <- df_codescores %>% 
  mutate(tier3 = case_when(
           dimension == "a1.1.1.actor.group.size.(#.of.cattle)"       ~ "A1.1: Number of relevant actors (# Cattle)",
           dimension == "a1.1.1.actor.group.size.(#.of.sheep/goats)"  ~ "A1.1: Number of relevant actors (# Sheep/goats)",
           dimension == "a2.1.economic.heterogeneity"                 ~ "A2.1: Economic heterogeneity",
           dimension == "a2.2.interest.heterogeneity"                 ~ "A2.2: Interest heterogeneity",
           dimension == "a4.1.leadership.accountability"              ~ "A4.1: Leadership accountability",
           dimension == "a5.1.actor.group.trust"                      ~ "A5.1: Actor group trust",
           dimension == "a5.2.inter-group.trust"                      ~ "A5.2: Inter-group trust",
           dimension == "a7.1.economic.dependence"                    ~ "A7.1: Economic dependence",
           dimension == "a7.2.commons.alternatives"                   ~ "A7.2: Commons alternatives",
           
           dimension == "eco1.01.rainfall.patterns"                   ~ "ECO1.1: Rainfall patterns",
           
           dimension == "gs2.1.external.support"                      ~ "GS2.1: External support",
           dimension == "gs3.2.property.security"                     ~ "GS3.2: Property security",
           dimension == "gs4.1.rules-in-use"                          ~ "GS4.1: Rules-in-use",
           dimension == "gs4.2.governance.strictness.trend"           ~ "GS4.2: Governance strictness trend",
           dimension == "gs5.1.external.recognition"                               ~ "GS5.1: External recognition",
           dimension == "gs5.4.participation.in.zoning"                            ~ "GS5.4: Participation in zoning",
           dimension == "gs5.3.participation.in.rule.making"                       ~ "GS5.3: Participation in rule making ",
           dimension == "gs5.5.commons.political.power"                            ~ "GS5.5: Commons political power",
           dimension == "gs6.2.outsider.exclusion"                                 ~ "GS6.2: Outsider exclusion",
           dimension == "gs7.1.environmental.monitoring"                           ~ "GS7.1: Environmental monitoring",
           dimension == "gs7.2.self.sanctions"                                     ~ "GS7.2: Self sanctions",
           dimension == "gs7.3.external.sanctions"                                 ~ "GS7.3: External sanctions",
      
           dimension == "i1.1.conflict.resolution"                                 ~ "I1.1: Conflict resolution",
           dimension == "i2.1.participation.in.social.monitoring.(enforcement)"    ~ "I2.1: Participation in social monitoring",
           
           dimension == "o1.1.compliance"                                          ~ "O1.1: Compliance",
           dimension == "o2.1.commons.condition.trend"                             ~ "O2.1: Commons condition trend",
           dimension == "o2.3.invasives"                                           ~ "O2.3: Invasives",
           
           dimension == "rs2.1.commons.boundaries"                                 ~ "RS2.1: Commons boundaries",
           dimension == "rs2.2.commons.boundary.negotiability"                     ~ "RS2.2: Commons boundaries negotiability",
           dimension == "rs3.1.commons.spatial.extent.(ha)"                        ~ "RS3.1: Commons spatial extent",
           dimension == "rs5.1.productivity"                                       ~ "RS5.1: Productivity",
           
           dimension == "s1.1.human.population.size.change.(annual.increase)"      ~ "S1.1: Change in human population size",
           dimension == "s1.2.changes.in.ethnic.composition.(village.leader.data)" ~ "S1.2: Changes in ethnic composition",
           dimension == "s1.3.changes.in.livelihood.activities"                    ~ "S1.3: Changes in livelihood activities"
         ))

# check data
stopifnot(sum(is.na(df_codescores$tier1))==0)
stopifnot(sum(is.na(df_codescores$tier3))==0)

# for continuous and binary tier3 variables, scale them in range [1,3] and round
df_codescores <- df_codescores %>% 
  group_by(dimension) %>% 
  mutate(score_scl = case_when(

    grepl("continuous", dimension_coding)           ~ round((score-min(score))/(max(score)-min(score)) * 2 + 1,0),
    grepl("binary", dimension_coding) &  score == 2 ~ 3,
    TRUE ~ score

  )) %>% ungroup

#######################################
#### means and SDs per DF variable ####
#######################################
summary_tier3 <- df_codescores %>% 
  group_by(`1st Tier` = tier1,
           `DF variable` = tier3
           
           ) %>% 
  summarise(`Mean (rescaled)` = mean(score_scl),
            `SD (rescaled)` = round(sd(score_scl),2),
            `Coding direction` = unique(dimension_coding),
            .groups = "drop") %>% 
  arrange(`DF variable`)

saveRDS(summary_tier3, file = "outputs/summary_tier3.rds")

#################################
#### spider plots by village ####
#################################
input_spec <- expand_grid(
  village=unique(df_codescores$village),
  tier1=unique(df_codescores$tier1)
)

spiderplots <- map(split(input_spec, seq(nrow(input_spec))),
                   function(x){

                     d <- df_codescores %>%
                       filter(tier1 == x$tier1,
                              village == x$village) %>%
                       select(score_scl, tier3) %>%
                       mutate(tier3 = str_replace(tier3, ": ", ":\n"),
                              tier3 = str_replace(tier3, " ", "\n"))
                     
                     x <- x %>% mutate(tier1 = ifelse(tier1 == "Social, Economic, and Political Settings (S)",
                                            "Social, Economic,\nand Political Settings (S)",
                                            tier1)) # with line break in long name
                     
                     p <- d  %>% 
                       pivot_wider(names_from = tier3, values_from = score_scl) %>% 
    
                       ggRadar(rescale = FALSE,
                               alpha = 0.1,
                               color = "slateblue") +
                       theme_minimal(14) +
                       theme(
                         text = element_text(family = fontfam),
                         axis.text.y = element_blank(),
                         axis.text.x = element_text(size = 9),
                         panel.grid.minor = element_blank(),
                         legend.position = "right"
                       ) +
                       scale_y_continuous(
                         limits = c(1, 3),
                         breaks = c(1, 2, 3),
                         expand = c(0, 0)
                       ) +
                       
                       labs(title = x$tier1,
                            subtitle = x$village) +
                       coord_radar()
                     
                     # label also when there is only one variable in the category,
                     # as for Related Ecosystems (ECO)
                     if(x$tier1 == "Related Ecosystems (ECO)"){
                       
                       p<-p + 
                         geom_text(data = d,aes(x = tier3, y = score_scl, label = tier3),
                                   family = fontfam,
                                   size = 4)
                       
                     }
                     return(p)
                  })

png("outputs/spiders_separate.png", width = 10000, height = 20000, res = 400)
wrap_plots(spiderplots,
           ncol = length(unique(df_codescores$tier1)),
           nrow = length(unique(df_codescores$village)))
dev.off()

#########################
#### anaylze GS vs O ####
#########################
# no more changing in directions necessary, higher values means always BETETR governance, and BETTER outcomes
# (all dimensions are on integer scale)
# gs vs o
# df_codescores %>% 
#   filter(tier1 %in% c("Outcomes (O)", "Governance Systems (GS)")) %>% 
#   select(dimension, dimension_coding) %>% 
#   distinct
df_gso <- df_codescores %>% 
  filter(tier1 %in% c("Outcomes (O)", "Governance Systems (GS)")) %>% 
  group_by(village, tier1) %>% 
  summarise(mean = mean(score_scl),
            .groups = "drop") %>% 
  pivot_wider(names_from = tier1, values_from = mean, names_prefix = "Avg. ")

# scatter plot
p1 <- df_gso %>% 
  ggplot(aes(x = `Avg. Governance Systems (GS)`,
             y = `Avg. Outcomes (O)`)) + 
  geom_point(size = 2) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam)) + 
  scale_x_continuous() + 
  scale_y_continuous(breaks = 1:3, limits = c(1,3)) +
  geom_text_repel(aes(label = village), size = 3, family = fontfam, seed = 3, col = "grey20") +
  geom_text(x = -Inf, y = Inf, label = glue("r = {format(round(
                                            cor(df_gso$`Avg. Governance Systems (GS)`,
                                            df_gso$`Avg. Outcomes (O)`), 3), nsmall = 3)}"),
            family = fontfam, col = "grey20", size = 4.5, hjust = -0.25, vjust = 1) + 
  # mark potential outliers (email may 2023)
  geom_point(
    data = df_gso %>% 
      filter(village %in% c("Kakoi", "Sangaiwe")),
    col = "red", size = 7, pch = 1
  )

png("outputs/scatter_gs_o.png", width = 1750, height = 1500, res = 350)
p1
dev.off()

# i vs o
# df_codescores %>% 
#   filter(tier1 %in% c("Outcomes (O)", "Interactions (I)")) %>% 
#   select(dimension, dimension_coding) %>% 
#   distinct
df_io <- df_codescores %>%
  filter(tier1 %in% c("Outcomes (O)", "Interactions (I)")) %>%
  group_by(village, tier1) %>%
  summarise(mean = mean(score),
            .groups = "drop") %>%
  pivot_wider(names_from = tier1, values_from = mean, names_prefix = "Avg. ")

# scatter plot
p2 <- df_io %>%
  ggplot(aes(x = `Avg. Interactions (I)`,
             y = `Avg. Outcomes (O)`)) +
  geom_point(size = 2) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam)) +
  scale_x_continuous() +
  scale_y_continuous(breaks = 1:3, limits = c(1,3)) +
  geom_text_repel(aes(label = village), size = 3, family = fontfam, seed = 3, col = "grey20") +
  geom_text(x = 1.15, y = 3.05, label = glue("r = {format(round(
                                            cor(df_io$`Avg. Interactions (I)`,
                                            df_io$`Avg. Outcomes (O)`), 3), nsmall = 3)}"),
            family = fontfam, col = "grey20", size = 4.5) + 
  # mark potential outliers (email may 2023)
  geom_point(
    data = df_io %>% 
      filter(village %in% c("Kakoi", "Sangaiwe")),
    col = "red", size = 7, pch = 1)

png("outputs/scatter_gs_i_o.png", width = 3000, height = 1500, res = 320)
p1+p2
dev.off()

# on tier3 level correlation matrix
cor_gso <- df_codescores %>% 
  filter(tier1 %in% c("Outcomes (O)", "Governance Systems (GS)")) %>% 
  select(village, tier3, score_scl) %>% 
  pivot_wider(names_from = tier3, values_from = score_scl) %>% 
  select(-village) %>% 
  cor() 

p2 <- cor_gso[grepl("GS", rownames(cor_gso)), grepl("O1", colnames(cor_gso)) | grepl("O2", colnames(cor_gso))] %>% 
  ggcorrplot(type = "full",
             lab = TRUE,
             digits = 1,
             outline.color = "black",
             colors = c("coral", "white", "slateblue"),
             legend.title = "r") +
  theme(text = element_text(family = fontfam))

png("outputs/corrm_gs_o.png", width = 2250, height = 1500, res = 350)
p2
dev.off()

# same again, but with semi-partial correlations (remove linear effect of average rainfall from outcome)
df_codescores_outc_resid <- df_codescores %>% 
  filter(tier1 %in% c("Outcomes (O)") |tier3 == "ECO1.1: Rainfall patterns") %>% 
  select(village, tier3, score_scl) %>% 
  pivot_wider(names_from = tier3, values_from = score_scl) %>% 
  mutate(across(.cols = c(`O1.1: Compliance`, `O2.1: Commons condition trend`, `O2.3: Invasives`),
                function(x){
                  # residualize outcomes
                  m<-lm(x ~ `ECO1.1: Rainfall patterns`)
                  return(residuals(m))
                })) %>% 
  select(-"ECO1.1: Rainfall patterns") %>% 
  pivot_longer(cols = c(`O1.1: Compliance`, `O2.1: Commons condition trend`, `O2.3: Invasives`),
               names_to = "tier3",
               values_to = "score_scl") %>% 
  mutate(tier1 = "Outcomes (O)")

df_codescores_outc_resid <- df_codescores %>% 
  filter(!tier3 %in% c("O1.1: Compliance", "O2.1: Commons condition trend", "O2.3: Invasives")) %>% 
  bind_rows(df_codescores_outc_resid)

cor_gso_outc_resid <- df_codescores_outc_resid %>% 
  filter(tier1 %in% c("Outcomes (O)", "Governance Systems (GS)")) %>% 
  select(village, tier3, score_scl) %>% 
  pivot_wider(names_from = tier3, values_from = score_scl) %>% 
  select(-village) %>% 
  cor() 

p3 <- cor_gso_outc_resid[grepl("GS", rownames(cor_gso_outc_resid)), 
                         grepl("O1", colnames(cor_gso_outc_resid)) | grepl("O2", colnames(cor_gso_outc_resid))] %>% 
  ggcorrplot(type = "full",
             lab = TRUE,
             digits = 1,
             outline.color = "black",
             colors = c("coral", "white", "slateblue"),
             legend.title = "r") +
  theme(text = element_text(family = fontfam))


png("outputs/corrm_gs_o_residualizedrain.png", width = 2250, height = 1500, res = 350)
p3
dev.off()

##################################
#### O soft / GS vs. GIS data ####
##################################
# data explore scale
df_landuse_area %>% 
  mutate(period = factor(period, ordered = TRUE, levels = c("pre", "post"))) %>% 
  filter(type == "bare") %>% 
  ggplot() + 
  geom_bar(aes( x= period, y = area_ha), stat = "identity") +
  facet_wrap(~ village) + 
  labs(subtitle = "bare")
df_landuse_area %>% 
  mutate(period = factor(period, ordered = TRUE, levels = c("pre", "post"))) %>% 
  filter(type == "crop") %>% 
  ggplot() + 
  geom_bar(aes( x= period, y = area_ha), stat = "identity") +
  facet_wrap(~ village) + 
  labs(subtitle = "crop")

# compute relative differences and log-transform them
lup_ratios <- df_landuse_area %>% 
  group_by(village, type, period) %>% 
  summarise(landprop = area_ha/total_area,
            .groups = "drop_last") %>% 
  summarise(pre_landprop = landprop[period == "pre"],
            post_landprop = landprop[period == "post"],
            .groups = "drop") %>% 
  mutate(ratio = log(post_landprop/pre_landprop)) %>%  # more crop or bare than previously is higher value
  select(-contains("landprop")) %>% 
  pivot_wider(names_from = type, values_from = ratio, names_prefix = "ratio_")

# scatter among changes
p35 <- lup_ratios %>% 
  ggplot(aes(x = ratio_bare,
             y = ratio_crop)) + 
  geom_point(size = 2) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam)) +
  geom_vline(xintercept = 0,lty = 2)+
  geom_hline(yintercept = 0,lty = 2) +
  labs(x = "Log(Factor change in bare ground)",
       y = "Log(Factor change in crop land)")+
  geom_text_repel(aes(label = village), size = 3, family = fontfam, seed = 3, col = "grey20") +
  geom_text(x = -Inf, y = Inf, label = glue("r = {format(round(
                                            cor(lup_ratios$ratio_bare,
                                            lup_ratios$ratio_crop), 2), nsmall = 2)}"),
            family = fontfam, col = "grey20", size = 4.5, hjust = -0.25, vjust = 1)

png("outputs/scatter_gis.png", width = 1750, height = 1500, res = 350)
p35
dev.off()

# scatter plots
df_gso_lup <- left_join(df_gso,lup_ratios, by = join_by(village))
p4 <- df_gso_lup %>% 
  ggplot(aes(x = `Avg. Governance Systems (GS)`,
             y = ratio_bare)) + 
  geom_point(size = 2) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam)) + 
  scale_x_continuous() + 
  #scale_y_continuous(breaks = 1:3, limits = c(1,3)) +
  geom_text_repel(aes(label = village), size = 3, family = fontfam, seed = 3, col = "grey20") +
  geom_text(x = -Inf, y = Inf, label = glue("r = {format(round(
                                            cor(df_gso_lup$`Avg. Governance Systems (GS)`,
                                            df_gso_lup$ratio_bare), 2), nsmall = 2)}"),
            family = fontfam, col = "grey20", size = 4.5, hjust = -0.25, vjust = 1) +
  labs(y = "Log(Factor change in bare ground)") + 
  # mark potential outliers (email may 2023)
  geom_point(
    data = df_gso_lup %>% 
      filter(village %in% c("Kakoi", "Sangaiwe")),
    col = "red", size = 7, pch = 1)

# png("outputs/scatter_gs_bare.png", width = 1750, height = 1500, res = 350)
# p4
# dev.off()

p5 <- df_gso_lup %>% 
  ggplot(aes(x = `Avg. Governance Systems (GS)`,
             y = ratio_crop)) + 
  geom_point(size = 2) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam)) + 
  scale_x_continuous() + 
  #scale_y_continuous(breaks = 1:3, limits = c(1,3)) +
  geom_text_repel(aes(label = village), size = 3, family = fontfam, seed = 3, col = "grey20") +
  geom_text(x = -Inf, y = Inf, label = glue("r = {format(round(
                                            cor(df_gso_lup$`Avg. Governance Systems (GS)`,
                                            df_gso_lup$ratio_crop), 2), nsmall = 2)}"),
            family = fontfam, col = "grey20", size = 4.5, hjust = -0.25, vjust = 1) +
  labs(y = "Log(Factor change in crop land)") + 
  # mark potential outliers (email may 2023)
  geom_point(
    data = df_gso_lup %>% 
      filter(village %in% c("Kakoi", "Sangaiwe")),
    col = "red", size = 7, pch = 1)

png("outputs/scatter_gs_bare_crop.png", width = 3000, height = 1500, res = 310)
p4+p5
dev.off()

# cor matrix factor LUP changes with all DF!
df_codescores_wide_lup <- df_codescores %>% 
  select(village, tier3, score_scl) %>% 
  pivot_wider(names_from = tier3, values_from = score_scl) %>% 
  left_join(lup_ratios)

names(df_codescores_wide_lup)[names(df_codescores_wide_lup)=="ratio_bare"] <- "Log(Factor change in bare ground)"
names(df_codescores_wide_lup)[names(df_codescores_wide_lup)=="ratio_crop"] <- "Log(Factor change in crop land)"

corrm_all <- df_codescores_wide_lup %>% 
  select(-village) %>% 
  cor()

p6 <- corrm_all %>% 
  ggcorrplot(type = "lower",
             lab = TRUE,
             digits = 1,
             outline.color = "black",
             colors = c("coral", "white", "slateblue"),
             legend.title = "r") +
  theme(text = element_text(family = fontfam))

png("outputs/corrm_all_lup.png", width = 4500, height = 3500, res = 290)
p6
dev.off()

# factor change boxplots for levels of commons condition trend
df_codescores_wide_lup <- df_codescores_wide_lup %>% 
  mutate(`O2.1: Commons condition trend_jit` = `O2.1: Commons condition trend` + rnorm(nrow(.),0,0.2),
         `O2.1: Commons condition trend` = factor(`O2.1: Commons condition trend`, levels = 1:3))

p7 <- ggplot(df_codescores_wide_lup ) +
  geom_boxplot(aes(x = `O2.1: Commons condition trend`, y =
                     `Log(Factor change in bare ground)`),
               fill = "grey95",
               outlier.color = NA)  +
  geom_point(aes(x = `O2.1: Commons condition trend_jit`, y =
                     `Log(Factor change in bare ground)`)) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam)) + 
    scale_x_discrete(drop = FALSE)  + 
  # mark potential outliers (email may 2023)
  geom_point(
    data = df_codescores_wide_lup %>% 
      filter(village %in% c("Kakoi", "Sangaiwe")),
    aes(x = `O2.1: Commons condition trend_jit`, y =
          `Log(Factor change in bare ground)`),
    col = "red", size = 7, pch = 1)

p8 <- ggplot(df_codescores_wide_lup ) +
  geom_boxplot(aes(x = `O2.1: Commons condition trend`, y =
                     `Log(Factor change in crop land)`),
               fill = "grey95",
               outlier.color = NA) +
  geom_point(aes(x = `O2.1: Commons condition trend_jit`, y =
                   `Log(Factor change in crop land)`)) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam)) + 
  scale_x_discrete(drop = FALSE)  + 
  # mark potential outliers (email may 2023)
  geom_point(
    data = df_codescores_wide_lup %>% 
      filter(village %in% c("Kakoi", "Sangaiwe")),
    aes(x = `O2.1: Commons condition trend_jit`, y =
          `Log(Factor change in crop land)`),
    col = "red", size = 7, pch = 1)

png("outputs/boxplots_commen_trend.png", width = 2250, height = 1500, res = 300)
p7+p8  
dev.off()
# 
# boxplot(df_codescores_wide_lup$`Log(Factor change in crop land)` ~ df_codescores_wide_lup$`O1.1: Compliance`)

## scatter plots GSystem vs GIS-outcomes
## after adjusting for average rainfall
df_gso_lup <- df_gso_lup %>% 
  left_join(df_codescores %>% filter(grepl("rainfall", dimension)) %>% 
              select(village, rainfall = score))

df_gso_lup <- df_gso_lup %>% 
  mutate(
    ratio_bare_res =  lm(ratio_bare ~ rainfall, data = df_gso_lup )$residuals,
    ratio_crop_res =  lm(ratio_crop ~ rainfall, data = df_gso_lup )$residuals
  )

p4res <- df_gso_lup %>% 
  ggplot(aes(x = `Avg. Governance Systems (GS)`,
             y = ratio_bare_res)) + 
  geom_point(size = 2) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam)) + 
  scale_x_continuous() + 
  #scale_y_continuous(breaks = 1:3, limits = c(1,3)) +
  geom_text_repel(aes(label = village), size = 3, family = fontfam, seed = 3, col = "grey20") +
  geom_text(x = -Inf, y = Inf, label = glue("r = {format(round(
                                            cor(df_gso_lup$`Avg. Governance Systems (GS)`,
                                            df_gso_lup$ratio_bare_res), 2), nsmall = 2)}"),
            family = fontfam, col = "grey20", size = 4.5, hjust = -0.25, vjust = 1) +
  labs(y = "Log(Factor change in bare ground)\n(residualized on rainfall)") + 
  # mark potential outliers (email may 2023)
  geom_point(
    data = df_gso_lup %>% 
      filter(village %in% c("Kakoi", "Sangaiwe")),
    col = "red", size = 7, pch = 1)

p5res <- df_gso_lup %>% 
  ggplot(aes(x = `Avg. Governance Systems (GS)`,
             y = ratio_crop_res)) + 
  geom_point(size = 2) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam)) + 
  scale_x_continuous() + 
  #scale_y_continuous(breaks = 1:3, limits = c(1,3)) +
  geom_text_repel(aes(label = village), size = 3, family = fontfam, seed = 3, col = "grey20") +
  geom_text(x = -Inf, y = Inf, label = glue("r = {format(round(
                                            cor(df_gso_lup$`Avg. Governance Systems (GS)`,
                                            df_gso_lup$ratio_crop_res), 2), nsmall = 2)}"),
            family = fontfam, col = "grey20", size = 4.5, hjust = -0.25, vjust = 1) +
  labs(y = "Log(Factor change in crop land)\n(residualized on rainfall)") + 
  # mark potential outliers (email may 2023)
  geom_point(
    data = df_gso_lup %>% 
      filter(village %in% c("Kakoi", "Sangaiwe")),
    col = "red", size = 7, pch = 1)

png("outputs/scatter_gs_bare_crop_resid.png", width = 3000, height = 1500, res = 310)
p4res+p5res
dev.off()
