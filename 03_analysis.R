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
           tier1 == "s"   ~ "Social, economic, and political settings (S)",
           tier1 == "rs"  ~ "Resource systems (RS)",
           tier1 == "gs"  ~ "Governance systems (GS)",
           tier1 == "a"   ~ "Actors (A)",
           tier1 == "o"   ~ "Outcomes (O)",
           tier1 == "eco" ~ "Related ecosystems (ECO)",
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

# code such for the GS group: that HIGHER scores mean BETTER resource governance conditions
# code such for the O group: that HIGHER scores mean BETTER outcomes
df_codescores %>% 
  filter(!duplicated(dimension), tier1 %in% c("Outcomes (O)", "Governance systems (GS)")) %>% 
  select(dimension, dimension_coding) %>% arrange(dimension) # only gs7.2.self.sanctions needs to be reversed

df_codescores <- df_codescores %>% 
  mutate(score = ifelse(dimension == "gs7.2.self.sanctions", 4 - score, score))

#########################
#### anaylze GS vs O ####
#########################
# (all dimensions are on integer scale)
# gs vs o
df_gso <- df_codescores %>% 
  filter(tier1 %in% c("Outcomes (O)", "Governance systems (GS)")) %>% 
  group_by(village, tier1) %>% 
  summarise(mean = mean(score),
            .groups = "drop") %>% 
  pivot_wider(names_from = tier1, values_from = mean, names_prefix = "Avg. ")

# scatter plot
p1 <- df_gso %>% 
  ggplot(aes(x = `Avg. Governance systems (GS)`,
             y = `Avg. Outcomes (O)`)) + 
  geom_point(size = 2) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam)) + 
  scale_x_continuous() + 
  scale_y_continuous(breaks = 1:3, limits = c(1,3)) +
  geom_text_repel(aes(label = village), size = 3, family = fontfam, seed = 3, col = "grey20") +
  geom_text(x = -Inf, y = Inf, label = glue("r = {format(round(
                                            cor(df_gso$`Avg. Governance systems (GS)`,
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
  filter(tier1 %in% c("Outcomes (O)", "Governance systems (GS)")) %>% 
  select(village, tier2, score) %>% 
  pivot_wider(names_from = tier2, values_from = score) %>% 
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

#############################
#### O soft vs. GIS data ####
#############################


