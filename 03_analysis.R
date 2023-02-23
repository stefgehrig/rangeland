library(tidyverse)
library(glue)
library(brms)
library(extrafont)
library(patchwork)
library(ggrepel)
library(ggcorrplot)
loadfonts()
fontfam <- "Segoe UI"

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
  mutate(tier2 = case_when(
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
           dimension == "gs3.2.property.security"                     ~ "GS3.2: Property security", # should be 3.1, not 3.2 (see code group manager)?
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
           
           dimension == "rs2.1.commons.boundaries"                                 ~ "RS2.1: Commons boundaries", # should be 1.1, not 2.1?
           dimension == "rs2.2.commons.boundary.negotiability"                     ~ "RS2.2: Commons boundaries negotiability", # should be 1.2, not 2.2?
           dimension == "rs3.1.commons.spatial.extent.(ha)"                        ~ "RS3.1: Commons spatial extent",
           dimension == "rs5.1.productivity"                                       ~ "RS5.1: Productivity",
           
           dimension == "s1.1.human.population.size.change.(annual.increase)"      ~ "S1.1: Change in human population size",
           dimension == "s1.2.changes.in.ethnic.composition.(village.leader.data)" ~ "S1.2: Changes in ethnic composition",
           dimension == "s1.3.changes.in.livelihood.activities"                    ~ "S1.3: Changes in livelihood activities"
         ))

# check data
stopifnot(sum(is.na(df_codescores$tier1))==0)
stopifnot(sum(is.na(df_codescores$tier2))==0)

# for continuous and binary tier2 variables, scale them in range [1,3]
df_codescores <- df_codescores %>% 
  group_by(dimension) %>% 
  mutate(score_scl = case_when(

    grepl("continuous", dimension_coding)           ~ (score-min(score))/(max(score)-min(score)) * 2 + 1,
    grepl("binary", dimension_coding) &  score == 2 ~ 3,
    TRUE ~ score
    
    # # gini cutoffs
    # dimension == "a2.1.economic.heterogeneity" & score <= 0.3, 1,
    # dimension == "a2.1.economic.heterogeneity" & score <= 0.5, 2,
    # dimension == "a2.1.economic.heterogeneity" & score >  0.5, 3,

  )) %>% ungroup

#############################################
#### which dimensions have no variation? ####
#############################################
sd_of_tier2 <- df_codescores %>% 
  group_by(`1st Tier` = tier1,
           `DF variable` = tier2
           
           ) %>% 
  summarise(Mean = mean(score_scl),
            SD = round(sd(score_scl),2)) %>% 
  arrange(`DF variable`)

saveRDS(sd_of_tier2, file = "outputs/sd_of_tier2.rds")

#########################
#### anaylze GS vs O ####
#########################
# code such for the GS group: that HIGHER scores mean BETTER resource governance conditions
# code such for the O group: that HIGHER scores mean BETTER outcomes
df_codescores %>% 
  filter(!duplicated(dimension), tier1 %in% c("Outcomes (O)", "Governance Systems (GS)")) %>% 
  select(dimension, dimension_coding) %>% arrange(dimension) # only gs7.2.self.sanctions needs to be reversed

df_codescores <- df_codescores %>% 
  mutate(score_scl = ifelse(dimension == "gs7.2.self.sanctions", 4 - score_scl, score_scl))

# (all dimensions are on integer scale)
# gs vs o
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
                                            df_gso$`Avg. Outcomes (O)`), 2), nsmall = 2)}"),
            family = fontfam, col = "grey20", size = 4.5, hjust = -0.25, vjust = 1)

png("outputs/scatter_gs_o.png", width = 1750, height = 1500, res = 350)
p1
dev.off()

# # i vs o
# df_io <- df_codescores %>% 
#   filter(tier1 %in% c("Outcomes (O)", "Interactions (I)")) %>% 
#   group_by(village, tier1) %>% 
#   summarise(mean = mean(score),
#             .groups = "drop") %>% 
#   pivot_wider(names_from = tier1, values_from = mean, names_prefix = "Avg. ")
# 
# # scatter plot
# p2 <- df_io %>% 
#   ggplot(aes(x = `Avg. Interactions (I)`,
#              y = `Avg. Outcomes (O)`)) + 
#   geom_point(size = 2) +
#   theme_classic(14) +
#   theme(text = element_text(family = fontfam)) + 
#   scale_x_continuous() + 
#   scale_y_continuous(breaks = 1:3, limits = c(1,3)) +
#   geom_text_repel(aes(label = village), size = 3, family = fontfam, seed = 3, col = "grey20") +
#   geom_text(x = -Inf, y = Inf, label = glue("r = {format(round(
#                                             cor(df_io$`Avg. Interactions (I)`,
#                                             df_io$`Avg. Outcomes (O)`), 2), nsmall = 2)}"),
#             family = fontfam, col = "grey20", size = 4.5)
# 
# png("outputs/scatter_gs_i_o.png", width = 2500, height = 1500, res = 320)
# p1+p2
# dev.off()

# on tier3 level correlation matrix
cor_gso <- df_codescores %>% 
  filter(tier1 %in% c("Outcomes (O)", "Governance Systems (GS)")) %>% 
  select(village, tier2, score_scl) %>% 
  pivot_wider(names_from = tier2, values_from = score_scl) %>% 
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
  filter(tier1 %in% c("Outcomes (O)") |tier2 == "ECO1.1: Rainfall patterns") %>% 
  select(village, tier2, score_scl) %>% 
  pivot_wider(names_from = tier2, values_from = score_scl) %>% 
  mutate(across(.cols = c(`O1.1: Compliance`, `O2.1: Commons condition trend`, `O2.3: Invasives`),
                function(x){
                  # residualize outcomes
                  m<-lm(x ~ `ECO1.1: Rainfall patterns`)
                  return(residuals(m))
                })) %>% 
  select(-"ECO1.1: Rainfall patterns") %>% 
  pivot_longer(cols = c(`O1.1: Compliance`, `O2.1: Commons condition trend`, `O2.3: Invasives`),
               names_to = "tier2",
               values_to = "score_scl") %>% 
  mutate(tier1 = "Outcomes (O)")

df_codescores_outc_resid <- df_codescores %>% 
  filter(!tier2 %in% c("O1.1: Compliance", "O2.1: Commons condition trend", "O2.3: Invasives")) %>% 
  bind_rows(df_codescores_outc_resid)

cor_gso_outc_resid <- df_codescores_outc_resid %>% 
  filter(tier1 %in% c("Outcomes (O)", "Governance Systems (GS)")) %>% 
  select(village, tier2, score_scl) %>% 
  pivot_wider(names_from = tier2, values_from = score_scl) %>% 
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
  labs(y = "Log(Factor change in bare ground)")
# 
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
  labs(y = "Log(Factor change in crop land)")

png("outputs/scatter_gs_bare_crop.png", width = 3000, height = 1500, res = 310)
p4+p5
dev.off()

# cor matrix factor LUP changes with all DF!
df_codescores_wide_lup <- df_codescores %>% 
  select(village, tier2, score_scl) %>% 
  pivot_wider(names_from = tier2, values_from = score_scl) %>% 
  left_join(lup_ratios)

names(df_codescores_wide_lup)[names(df_codescores_wide_lup)=="ratio_bare"] <- "Log(Factor change in bare ground)"
names(df_codescores_wide_lup)[names(df_codescores_wide_lup)=="ratio_crop"] <- "Log(Factor change in crop land)"

corrm_all <- df_codescores_wide_lup %>% 
  select(-village) %>% 
  cor()

p6 <- corrm_all %>% 
  ggcorrplot(type = "upper",
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
p7 <- df_codescores_wide_lup %>% 
  mutate(`O2.1: Commons condition trend` = factor(`O2.1: Commons condition trend`)) %>% 
  ggplot() +
  geom_boxplot(aes(x = `O2.1: Commons condition trend`, y =
                     `Log(Factor change in bare ground)`)) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam))

p8 <- df_codescores_wide_lup %>% 
  mutate(`O2.1: Commons condition trend` = factor(`O2.1: Commons condition trend`)) %>% 
  ggplot() +
  geom_boxplot(aes(x = `O2.1: Commons condition trend`, y =
                     `Log(Factor change in crop land)`)) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam))

png("outputs/boxplots_commen_trend.png", width = 2250, height = 1500, res = 300)
p7+p8  
dev.off()

boxplot(df_codescores_wide_lup$`Log(Factor change in crop land)` ~ df_codescores_wide_lup$`O1.1: Compliance`)
