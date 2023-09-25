library(tidyverse)
library(glue)
library(brms)
library(tidybayes)
library(bayesplot)
library(extrafont)
library(patchwork)
library(marginaleffects)
library(ggrepel)
library(ggtext)
library(ggcorrplot)
library(ggrain)
library(geomtextpath)
library(ggiraphExtra)
library(RColorBrewer)
library(ggradar2)
loadfonts()
source("00_functions.R")
fontfam <- "Segoe UI"
palcolors <- colorRampPalette(brewer.pal(12, "Set3"))(12)
gradcolors <- c(
  "#bdc9e1",
  "#67a9cf",
  "#02818a"
)
onecolor <- "#02818a"

###################
#### load data ####
###################
map(list.files("data_processed", ".csv"), function(x){
  assign(x = str_remove(x, ".csv"), value = read_csv(glue("data_processed/{x}")), envir = .GlobalEnv)
})

df <- name_the_tiers(df_codescores) %>% 
  # all to ordinal, and binary to 1-3
  mutate(score = ifelse(grepl("binary", dimension_coding) & score == 2, 3, score)) %>% 
  mutate(score = factor(score, ordered = TRUE, levels = 1:3))

################################
#### tables for description ####
################################
# relevant ordinal DF vars for ITR correlation analysis (table 1)
df_items <- df %>% 
  # ITR
  filter(grepl("Governance", tier1) |
           grepl("Interactions", tier1) | # conflict resolution
           grepl("rs2.1", dimension) | # boundaries
           grepl("rs2.2", dimension) | # boundaries
           grepl("a5.1", dimension) | # trust intra
           grepl("a5.2", dimension) | # trust inter
           grepl("a4.1", dimension) | # leadership
           grepl("Outcome", tier1))

gat_tab1 <- df_items %>% 
  # ITR
  filter(grepl("Governance", tier1) |
           grepl("Interactions", tier1) | # conflict resolution
           grepl("rs2.1", dimension) | # boundaries
           grepl("rs2.2", dimension) | # boundaries
           grepl("a5.1", dimension) | # trust intra
           grepl("a5.2", dimension) | # trust inter
           grepl("a4.1", dimension) | # leadership
           grepl("Outcome", tier1)) %>% 
  group_by(tier1, tier3) %>% 
  summarise(
    score1 = sum(score == "1"),
    score2 = sum(score == "2"),
    score3 = sum(score == "3"),
    bin = unique(grepl("binary", dimension_coding)),
    descr = unique(descr),
    .groups = "drop"
  )

saveRDS(gat_tab1, "outputs/gat_tab1.rds")

# relevant continuous variables
# ... rain
d_rain <- df_codescores %>% 
  filter(grepl("rain", dimension)) %>% 
  select(village, val = score) %>% 
  mutate(var = "rainfall")
# ... het
d_het <- df_codescores %>% 
  filter(grepl("economic.hetero", dimension)) %>% 
  select(village, val = score) %>% 
  mutate(var = "econ_het")

# # ... wealth median household (just to show correlation)
# df_hhsurv_temp <- df_hhsurv %>%
#   select(village,
#          improvedtoilet,
#          housewalltype,
#          housefloortype,
#          houserooftype,
#          num_mobile_phone,
#          num_motorcycle,
#          num_modernbed
#   ) %>%
#   mutate(across(.cols=everything(), ~ifelse(is.na(.x), 0, .x)),
#          across(where(is.numeric), ~scale(.x)))
# 
# pca_econ <- principal(df_hhsurv_temp %>% select(-village), nfactors = 1)
# pca_econ$loadings # high correlations consistently with PC1
# d_lev <- df_hhsurv_temp %>% select(village) %>%
#   bind_cols(wealth = pca_econ$scores) %>%
#   group_by(village) %>% 
#   summarise(median_hh_wealth = median(wealth))

# ... gis
d_gis <- df_landuse_area %>% 
  mutate(var = 
           paste0(type, " ", period)) %>% 
  select(village, var, val = area_ha)

df_others <- bind_rows(
  d_rain,
  d_het,
  d_gis
) %>% 
  mutate(source = 
           case_when(
             var == "bare pre" ~ "MODIS vegetation continuous fields (2015-2015)",
             var == "bare post" ~ "MODIS vegetation continuous fields (2016-2021)",
             var == "crop pre" ~ "Global cropland extent data set (2015)",
             var == "crop post" ~ "Global cropland extent data set (2019)",
             var == "econ_het" ~ "Household survey",
             var == "rainfall" ~ "PERSIANN-CCS (2016-2020)",
           )) %>% 
  mutate(
    var = case_when( 
      var == "bare pre"  ~ "Bare ground area Pre (ha)",
      var == "bare post" ~ "Bare ground area Post (ha)",
      var == "crop pre"  ~ "Cropland area Pre (ha)",
      var == "crop post" ~ "Cropland area Post (ha)",
      var == "econ_het"  ~ "Economic inequality (SD of PC1 from household assets)",
      var == "rainfall"  ~ "Mean annual rainfall (mm)"
    )
  ) %>% 
  mutate(var = factor(var, ordered = TRUE,
                      levels = c(
                        "Bare ground area Pre (ha)",
                        "Bare ground area Post (ha)",
                        "Cropland area Pre (ha)",
                        "Cropland area Post (ha)",
                        "Mean annual rainfall (mm)",
                        "Economic inequality (SD of PC1 from household assets)"
                        
                      )))

gat_tab2 <- df_others %>% 
  group_by(var) %>% 
  summarise(
    across(val, .fns = list(mean=mean, min=min, max=max)),
    source = unique(source)
  )

saveRDS(gat_tab2, "outputs/gat_tab2.rds")

# naming variables
df_landuse_area <- df_landuse_area %>% 
  mutate(period = str_to_title(period)) %>% 
  mutate(period = factor(period, ordered = TRUE, levels = c("Pre", "Post")))

#################################
#### figures for description ####
#################################
# spider plots
d_spider <- df_items %>%
  mutate(is_outcome = grepl("Outcomes", tier1)) %>%
  mutate(tier1 = paste(unique(tier1), collapse = "\n"), .by = is_outcome)%>% 
  # linebreaks
  # mutate(tier3 = str_replace_all(tier3, " ", "\n")) %>%
  # or remove all text
  mutate(tier3 = gsub("\\:.*","", tier3)) %>% 
  select(village, tier3, score, tier1) %>%
  mutate(score = as.numeric(as.character(score))) %>%
  pivot_wider(names_from = tier3, values_from = score) 

input_spec <- expand_grid(
  village = unique(d_spider$village),
  tier1 = unique(d_spider$tier1)
) %>% 
  arrange(village, desc(tier1))

spiderplots <- map(split(input_spec, seq(nrow(input_spec))),
                   function(x){
                     
  d_spider %>%
    filter(village == x$village,
           tier1 == x$tier1) %>%
    discard(~all(is.na(.x))) %>%
    map_df(~.x) %>% 
    ggRadar2(aes(facet = tier1),
            rescale = FALSE,
            alpha = 0.2,
            size = 3/4,
            clip = "off") +
    theme_minimal(14) +
    theme(
      text = element_text(family = fontfam),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 7.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text = element_text(size= 7.5),
      legend.position = "none",
    ) +
    scale_y_continuous(
      limits = c(1, 3),
      breaks = c(1, 2, 3),
      expand = c(0, 0)
    ) +
    labs(subtitle = if(grepl("Outcomes", x$tier1)) "" else x$village)  + 
    scale_colour_manual(values = c("grey20"))+ 
    scale_fill_manual(values =   c("grey20"))
})

png("outputs/gat_spiders.png", width = 2000, height = 4250, res = 225)
wrap_plots(spiderplots,
           ncol = 4,
           nrow = 6)
dev.off()

# # boxplots
# input_spec <- 
#   list(
#     c("Bare ground area Pre (ha)",
#       "Bare ground area Post (ha)"),
#     c("Cropland area Pre (ha)",
#       "Cropland area Post (ha)"),
#     c("Mean annual rainfall (mm)",
#       "Economic inequality (SD of PC1 from household assets)")
#   )
# 
# 
# boxes_list <- map(input_spec, function(x){
#   
#   set.seed(12)
#   gat_boxes <- df_others %>% 
#     filter(var %in% x) %>% 
#     mutate(x = runif(nrow(.), -0.2, 0.2)) %>% 
#     ggplot() + 
#     geom_boxplot(aes(y = val, x = x), fill = "grey80", 
#                  size = 1/8,
#                  alpha = 0.25, outlier.colour = NA) + 
#     geom_point(aes(x = x, y = val)) + 
#     ggrepel::geom_text_repel(aes(x = x, y = val, label = village),
#                              size = 3, family = fontfam, vjust = 1,
#                              segment.color = 'grey40',
#                              max.overlaps = 2e1) + 
#     facet_wrap(~ var, 
#                scales = if(grepl("area", x[1])) "fixed" else "free", 
#                ncol = 2) + 
#     theme_bw(14) +
#     theme(
#       text = element_text(family = fontfam),
#       strip.background = element_blank(),
#       axis.text.x = element_blank(),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.x = element_blank(),
#       axis.ticks.x = element_blank()
#     ) +
#     labs(x = "",
#          y = "")
# })
# 
# png("outputs/gat_boxes.png", width = 2750, height = 3250, res = 290)
# wrap_plots(boxes_list,
#            ncol = 1)
# dev.off()

# raincloud plots
p_rain_bare <- df_landuse_area %>% filter(type == "bare") %>% 
  ggplot(aes(period, area_ha)) + 
  geom_rain(rain.side = 'f1x1', id.long.var = "village", fill = "grey20", alpha = 0.2) + 
  theme_bw(14) +
  theme(
    text = element_text(family = fontfam),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(x = "Period",
       y = "Bare ground area (ha)") + 
  ggrepel::geom_text_repel(
    data = df_landuse_area %>% filter(type == "bare") %>% filter(period == "Pre"),
    aes(x = period, 
        y = area_ha, 
        label = village),
    size = 3, 
    family = fontfam, 
    hjust = -1/4,
    segment.color = NA,
    box.padding = 0.02,
    max.overlaps = 10) +
  scale_y_continuous(trans = "log", breaks = c(500,1000,2500,5000,10000,20000),
                     limits = c(500,25000))


p_rain_crop <- df_landuse_area %>% filter(type == "crop") %>% 
  ggplot(aes(period, area_ha)) + 
  geom_rain(rain.side = 'f1x1', id.long.var = "village", fill = "grey20", alpha = 0.2) + 
  theme_bw(14) +
  theme(
    text = element_text(family = fontfam),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(x = "Period",
       y = "Cropland area (ha)")+ 
  ggrepel::geom_text_repel(
    data = df_landuse_area %>% filter(type == "crop") %>% filter(period == "Pre"),
    aes(x = period, 
        y = area_ha, 
        label = village),
    size = 3, 
    family = fontfam, 
    hjust = -1/4,
    segment.color = NA,
    box.padding = 0.02,
    max.overlaps = 10) +
  scale_y_continuous(trans = "log", breaks = c(50,100,250,500,1000,2500,5000,10000,20000,30000),
                     limits = c(50,35000))

p_rain_rain <- d_rain %>% 
  ggplot(aes(1, val)) + 
  geom_rain(fill = "grey20", alpha = 0.2) +
  theme_bw(14) +
  theme(
    text = element_text(family = fontfam),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), axis.ticks.x = element_blank()
  ) +
  labs(y = "Mean annual rainfall (mm)")+ 
  ggrepel::geom_text_repel(
    aes(x = 1, 
        y = val, 
        label = village),
    size = 3, 
    family = fontfam, 
    vjust = 0,
    segment.color = NA,
    box.padding = 0.02,
    max.overlaps = 6) 

p_rain_het <- d_het %>% 
  ggplot(aes(1, val)) + 
  geom_rain(fill = "grey20", alpha = 0.2) +
  theme_bw(14) +
  theme(
    text = element_text(family = fontfam),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), axis.ticks.x = element_blank()
  ) +
  labs(y = "Economic inequality\n(SD of PC1 from household assets)")+ 
  ggrepel::geom_text_repel(
    aes(x = 1, 
        y = val, 
        label = village),
    size = 3, 
    family = fontfam, 
    vjust = 0,
    segment.color = NA,
    box.padding = 0.02,
    max.overlaps = 6) 


upr_patch <- (p_rain_bare | p_rain_crop)
lwr_patch <- (p_rain_rain | p_rain_het)

set.seed(123)
png("outputs/gat_rainclouds.png", width = 2750, height = 3300, res = 300)
(upr_patch / lwr_patch) + plot_layout(heights = c(3,2)) & plot_annotation(tag_levels = "a") & theme(text = element_text(family = fontfam))
dev.off()

###################
#### IRT model ####
###################
# fitting
df_items$itemtype = ifelse(grepl("Outcome", df_items$tier1), "Outcome", "Governance")

family <- brmsfamily("cumulative", "logit")

priors <-
  prior("constant(1)",  class = "sd", group = "village") + # strong-identifiability restriction
  prior("exponential(1.5)", class = "sd") + # a bit more regularizing than default halt-student-t(3, 0, 2.5)
  prior("exponential(1.5)", class = "sd", dpar = "disc") +  # a bit more regularizing than default halt-student-t(3, 0, 2.5)
  prior("normal(0, 2.5)", class = "Intercept") + # more dispersed that default halt-student-t(3, 0, 2.5)
  prior("normal(0, 1)", class = "Intercept", dpar = "disc") # this is also the default

formula <- bf(
  score ~ 1 + (1 |i| tier3) + (0 + itemtype | village),
  disc  ~ 1 + (1 |i| tier3))

# fit_irt_2par <- brm( # RTOOLS! warum is 4.3 nicht genug?
#   formula = formula,
#   data = df_items,
#   family = family,
#   prior = priors,
#   control   = list(adapt_delta = 0.999, max_treedepth = 15),
#   warmup    = 2e3,
#   iter      = 6e3,
#   thin      = 1,
#   chains    = 5,
#   cores     = 5,
#   seed      = 1234,
#   backend   = "cmdstanr"
# )
# 
# summary(fit_irt_2par)
# prior_summary(fit_irt_2par)
# # export model results
# saveRDS(fit_irt_2par, file = "outputs/fit_irt_2par.rds")
fit_irt_2par <- readRDS(file = "outputs/fit_irt_2par.rds")
cat(stancode(fit_irt_2par), file = "outputs/fit_irt_2par_stancode.txt")
sink("outputs/fit_irt_2par_modsummary.txt")
summary(fit_irt_2par)
sink()
sink("outputs/fit_irt_2par_priorsummary.txt")
prior_summary(fit_irt_2par)
sink()


# understand parametrization of discrimination by manually creating predictions
# ... auto predict
epreds <- posterior_epred(fit_irt_2par, newdata
                          = df_items %>% filter(
                            village == "Kimana",
                            dimension == "rs2.1.commons.boundaries"
                          ))
A <- as_tibble(epreds) %>% pull(`1.1`) %>% mean # E[Pr(Y = 1|X)]
# ... manual predict
postdraws <- spread_draws(fit_irt_2par,
                          b_Intercept[threshold],
                          b_disc_Intercept,
                          r_tier3[item, par],
                          r_tier3__disc[item, par],
                          r_village[village, itemtype]) %>%
  ungroup %>%
  filter(village == "Kimana",
         itemtype == "itemtypeGovernance",
         item == "RS2.1:.Commons.boundaries",
         threshold == 1)
B <- postdraws %>%
  transmute(
    prob_resp1 = plogis((exp(b_disc_Intercept + r_tier3__disc)) * (b_Intercept - (r_tier3 + r_village)))
  ) %>% pull %>% mean 
A == B # OK!

# trace plots inspection
mcmc_trace(fit_irt_2par, regex_pars = "disc_Intercept")
mcmc_trace(fit_irt_2par, regex_pars = "b_")

# plotting ppd
yrep_char <- posterior_predict(fit_irt_2par)
ppc1 <- ppc_bars_grouped(
  y = as.numeric(as.character(df_items$score)),
  yrep = yrep_char,
  group = df_items$tier3,
  facet_args = list(ncol =4))+ 
  theme_classic(12) +
  theme(strip.text = element_text(size = 8.5),
        text = element_text(family = fontfam),
        strip.background = element_blank()) + 
  scale_y_continuous(breaks = c(0,5,10), limits = c(0,13))+ 
  scale_x_continuous(breaks = c(1,2,3))

ppc2 <- ppc_bars_grouped(
  y = as.numeric(as.character(df_items$score)),
  yrep = yrep_char,
  group = df_items$village)  +
  theme_classic(12) +
  theme(strip.text = element_text(size = 8.5),
        text = element_text(family = fontfam),
        strip.background = element_blank()) + 
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,23))+ 
  scale_x_continuous(breaks = c(1,2,3))

png("outputs/ppcs_fit_irt_2par.png", width = 4500, height = 6000, res = 380)
ppc1/ppc2 + plot_layout(heights = c(1,2/3)) +
  plot_annotation(tag_levels = "a") & theme(text = element_text(family = fontfam))
dev.off()

# figure parameter estimates
p_re1a <- mcmc_intervals(fit_irt_2par, pars = vars(contains("r_village[") & contains("Governance")),
                         prob = 0.5, prob_outer = 0.9, point_est = "median") +
  theme_classic(12) +
  labs(subtitle = "*&#952;<sub>j</sub><sup>Gov</sup>*") +
  theme(text = element_text(family = fontfam),
        plot.subtitle = element_markdown()) + 
  scale_y_discrete(
    labels = function(x) 
      str_replace_all(str_remove_all(str_remove_all(x, "r_village\\["), ",itemtypeGovernance\\]"), "\\.", " ")
  )

p_re1b <- mcmc_intervals(fit_irt_2par, pars = vars(contains("r_village[") & contains("Outcome")),
                         prob = 0.5, prob_outer = 0.9, point_est = "median") +
  theme_classic(12) +
  labs(subtitle = "*&#952;<sub>j</sub><sup>Out</sup>*") +
  theme(text = element_text(family = fontfam),
        plot.subtitle = element_markdown())  + 
  scale_y_discrete(
    labels = function(x) 
      str_replace_all(str_remove_all(str_remove_all(x, "r_village\\["), ",itemtypeOutcome\\]"), "\\.", " ")
  )

p_re2 <- mcmc_intervals(fit_irt_2par, regex_pars = "r_tier3\\[",
                        prob = 0.5, prob_outer = 0.9, point_est = "median")+
  labs(subtitle = "*b<sub>i</sub>*") +
  theme_classic(12) +
  theme(text = element_text(family = fontfam),
        plot.subtitle = element_markdown()) + 
  scale_y_discrete(
    labels = function(x) str_replace_all(str_remove_all(str_remove_all(x, "r_tier3\\["), ",Intercept\\]"), "\\.(?=[A-Za-z])", " ")
  )

p_re3 <- mcmc_intervals(fit_irt_2par, regex_pars = "r_tier3__disc",
                        prob = 0.5, prob_outer = 0.9, point_est = "median")+
  labs(subtitle = "*a<sub>i</sub>*") +
  theme_classic(12) +
  theme(text = element_text(family = fontfam),
        plot.subtitle = element_markdown()) + 
  scale_y_discrete(
    labels = function(x) str_replace_all(str_remove_all(str_remove_all(x, "r_tier3__disc\\["), ",Intercept\\]"), "\\.(?=[A-Za-z])", " ")
  )

png("outputs/param_posts_fit_irt_2par.png", width = 4000, height = 5000, res = 420)
(p_re1a + p_re1b) / p_re2 / p_re3
dev.off()

# correlation plots
p_cor1 <- mcmc_areas_ridges(fit_irt_2par, pars = vars(contains("cor_village")),
                            prob_outer = 1, prob = 0.9) + 
  geom_vline(xintercept = 0, lty = 2, lwd = 1) + theme_classic(14) + 
  theme(text = element_text(family = fontfam),
        axis.title.x = element_markdown()) +
  labs(x = "Correlation of governance processes and<br>governance outcomes (*&#961;<sub>Gov, Out</sub>*)",
       y = "Posterior density")+ 
  scale_y_discrete(
    labels = "",
    expand = c(0,0)
  )

p_cor2 <- gather_draws(fit_irt_2par, r_village[village, itemtype]) %>%
  ungroup %>% 
  pivot_wider(values_from = .value, names_from = itemtype) %>% 
  ggplot(aes(x = itemtypeGovernance, y = itemtypeOutcome, 
             color = village, group = village, fill = village)) +
  geom_density_2d(lwd = 1, contour_var = "ndensity", breaks = c(0.1)) +
  #stat_ellipse(level = 0.9) +
  # geom_point(data = gather_draws(fit_irt_2par, r_village[village,dimension], ndraws = 50, seed = 123) %>% 
  #               pivot_wider(names_from = dimension, values_from = .value), 
  #            size = 1,
  #            alpha = 3/4,
  #            show.legend = FALSE) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam),
        axis.title.x  = element_markdown(),
        axis.title.y  = element_markdown()) +
  scale_color_manual(values = palcolors,
                     labels = function(x) 
                       str_replace_all(x, "\\.", " ")) + 
  coord_cartesian(xlim = c(-3,3), ylim = c(-3,3)) + 
  labs(x = "Quality of governance processes (*&#952;<sub>j</sub><sup>Gov</sup>*)",
       y = "Quality of governance outcomes (*&#952;<sub>j</sub><sup>Out</sup>*)",
       color = "Village")

png("outputs/cor_itm_fit_irt_2par.png", width = 4500, height = 1800, res = 360)
p_cor1 + p_cor2 +
  plot_annotation(tag_levels = "a") & theme(text = element_text(family = fontfam))
dev.off()

# table with other parameters posteriors
other_pars <- get_variables(fit_irt_2par)[grepl("b_|sd_|cor_tier3",  get_variables(fit_irt_2par))]
pars_tab <- map_dfr(other_pars, function(x){
  gather_draws(fit_irt_2par, !!as.symbol(x)) %>% 
    summarise(mean = mean(.value),
              sd = sd(.value),
              q05 = quantile(.value, 0.05),
              q50 = median(.value),
              q95 = quantile(.value, 0.95))
})  %>% 
  mutate(varlabel = 
           case_when(
             .variable == "b_Intercept[1]"                       ~ "$\\beta_1$",
             .variable == "b_Intercept[2]"                       ~ "$\\beta_2$",
             .variable == "b_disc_Intercept"                     ~ "$\\alpha$",
             .variable == "sd_tier3__Intercept"                  ~ "$\\sigma_b$",
             .variable == "sd_tier3__disc_Intercept"             ~ "$\\sigma_a$",
             .variable == "sd_village__itemtypeGovernance"       ~ "$\\sigma_\\text{Gov}$",
             .variable == "sd_village__itemtypeOutcome"          ~ "$\\sigma_\\text{Out}$",
             .variable == "cor_tier3__Intercept__disc_Intercept" ~ "$\\rho_{a, b}$"
             ), .before = ".variable"
           ) %>% 
  mutate(across(where(is.numeric), ~style_number(.x))) %>% 
  select(-.variable)
  
saveRDS(pars_tab, file = "outputs/pars_tab.rds")

################################
#### remote sensing outcome ####
################################
library(ggdist)
library(posterior)
library(distributional)

# check normality: ok!
gather_rvars(
  fit_irt_2par, r_village[village, itemtype]
) %>% 
  filter(itemtype == "itemtypeGovernance") %>% 
  mutate(mean = mean(.value),
         sd = sd(.value)) %>% 
  ggplot() + 
  stat_halfeye(aes(xdist = .value)) +
  stat_slab(aes(xdist = dist_normal(mean, sd)), fill = NA, 
            lwd = 1/2,
            color = "blue") + 
  theme_bw(14) +
  facet_wrap(~ village, scales = "free")
#... this allows simplified use in brms measurement error model

# extract posterior mean and SD per village
df_theta_gov <- gather_rvars(
  fit_irt_2par, r_village[village, itemtype]
) %>% 
  filter(itemtype == "itemtypeGovernance") %>% 
  transmute(village = village, 
            mean = mean(.value),
            sd = sd(.value))

# #######################################################################
# ##### check high vs low discrimination item to better understand that #
# ##### high discrimination items show little variation in the raw data #
# #######################################################################
# coef(fit_irt_2par)
# summary(fit_irt_2par)
# 
# postdraws %>% 
#   ungroup %>% 
#   distinct(.draw, threshold, .keep_all = TRUE) %>% 
#   group_by(threshold) %>% 
#   summarise(across(contains("b_Intercept"), mean))
# beta_1 <- -2.02  
# beta_2 <- 0.00484
# 
# postdraws %>% 
#   ungroup %>% 
#   distinct(.draw, .keep_all = TRUE) %>% 
#   summarise(across(contains("b_disc_Intercept"), mean))
# alpha <- 0.305
# 
# x <- seq(-4,4,0.01)
# 
# #### external recognition (HIGH discrimination)
# postdraws %>% 
#   ungroup %>% 
#   filter(grepl("recognition", item)) %>% 
#   distinct(.draw, .keep_all = TRUE) %>% 
#   summarise(across(contains("tier3"), mean))
# 
# a_i =   0.647
# b_i =-1.03
# # response probabilities of this item
# plot(x, 
#      plogis(exp(alpha + a_i) * (beta_1 - (b_i + x))),
#      type = "l",
#      main = "external recognition (high discr)",
#      ylim = c(0,1))
# lines(x, 
#   plogis(exp(alpha + a_i) * (beta_2 - (b_i + x))) - plogis(exp(alpha + a_i) * (beta_1 - (b_i + x))),
#   col = "blue")
# lines(x, 
#   1 - (plogis(exp(alpha + a_i) * (beta_2 - (b_i + x))) - plogis(exp(alpha + a_i) * (beta_1 - (b_i + x)))) - plogis(exp(alpha + a_i) * (beta_1 - (b_i + x))),
#   col = "green")
# 
# # estimated village locations x
# v = postdraws %>% 
#   ungroup %>% 
#   filter(itemtype == "itemtypeGovernance") %>% 
#   group_by(village) %>% 
#   summarise(m=mean(r_village)) %>% 
#   pull(m)
# abline(v = v, lty = 2)
# 
# ####  rules in use (LOW discrimination)
# postdraws %>% 
#   ungroup %>% 
#   filter(grepl("Rules-in-use", item)) %>% 
#   distinct(.draw, .keep_all = TRUE) %>% 
#   summarise(across(contains("tier3"), mean))
# a_i = -0.766
# b_i = 0.383
# # response probabilities of this item
# plot(x, 
#      plogis(exp(alpha + a_i) * (beta_1 - (b_i + x))),
#      type = "l",
#      main = "rules use (low discr)",
#      ylim = c(0,1))
# lines(x, 
#   plogis(exp(alpha + a_i) * (beta_2 - (b_i + x))) - plogis(exp(alpha + a_i) * (beta_1 - (b_i + x))),
#   col = "blue")
# lines(x, 
#   1 - (plogis(exp(alpha + a_i) * (beta_2 - (b_i + x))) - plogis(exp(alpha + a_i) * (beta_1 - (b_i + x)))) - plogis(exp(alpha + a_i) * (beta_1 - (b_i + x))),
#   col = "green")
# 
# # estimated village locations x
# abline(v = v, lty = 2)
# 
# # so we see: 
# sd(sample(c(1,2,3), prob = c(0, 0.6, 0.4), replace = TRUE, size = 1e4))
# sd(sample(c(1,2,3), prob = c(0.1, 0.2, 0.7), replace = TRUE, size = 1e4))

############################
##### ICC for one item #####
############################
# two versions: one with itemtypeOutcome (standard ICC) and one with itemtytepGovernance (should be possible due to correlation parameter?)
outcome_items <- df %>% 
  filter(grepl("Outcome", tier1)) %>% 
  pull(tier3) %>% unique

draws <- spread_draws(fit_irt_2par,
                          b_Intercept[threshold],
                          b_disc_Intercept,
                          r_tier3[item, par],
                          r_tier3__disc[item, par]) %>% 
  mutate(item = str_replace_all(item, "\\.(?=[A-Za-z])", " "))

icc_plots <- map(outcome_items, function(icc_item){
  
  draws_icc <- draws %>% 
    filter(item == icc_item) %>% 
    pivot_wider(names_from = "threshold", values_from = "b_Intercept", names_prefix = "threshold") 
  
  df_icclines <- expand_grid(
    draws_icc,
    x = seq(-4,4,0.01)
  ) %>% 
    mutate(
      pi_1 = plogis(exp(b_disc_Intercept + r_tier3__disc) * (threshold1 - (r_tier3 + x))),
      pi_2 = plogis(exp(b_disc_Intercept + r_tier3__disc) * (threshold2 - (r_tier3 + x))) - pi_1,
      pi_3 = 1 - pi_1 - pi_2
    ) %>% 
    pivot_longer(
      cols = contains("pi_"),
      names_to = "response",
      values_to = "prob"
    ) %>% 
    mutate(response = str_remove(response, "pi_"))
  
  df_icclines_smry <- df_icclines %>% 
    group_by(response, x) %>% 
    summarise(
      point = median(prob),
      lwr = quantile(prob, 0.05),
      upr = quantile(prob, 0.95)
    )
  
  p_icc1 <- df_icclines_smry %>% 
    ggplot() + 
    geom_ribbon(aes(x = x, ymin = lwr, ymax = upr, fill = response), alpha = 1/10) +
    geom_textline(aes(x = x, y = point, col = response, label = response),
                  lwd = 1, hjust = 0.1, size = 5) +
    theme_classic(14) +
    theme(text = element_text(family = fontfam),
          axis.title.x  = element_markdown(),
          legend.position = "none",
          axis.title.y = element_markdown()) + 
    scale_color_manual(values = gradcolors) + 
    scale_fill_manual(values = gradcolors) + 
    labs(y = paste0("Expected response probability for item<br>", "*", icc_item, "*"),
         x = "Quality of governance outcomes (*&#952;<sub>j</sub><sup>Out</sup>*)",
         fill = "Response\ncategory")

  return(p_icc1)
  
})

p_icc2 <- spread_draws(fit_irt_2par,
                       r_village[village, itemtype]) %>% 
  group_by(village) %>% 
  mutate(hjust_par = runif(1,0,1)) %>% 
  filter(itemtype == "itemtypeOutcome") %>% 
  ggplot() + 
  geom_density(aes(x = r_village, fill = village), 
               col = NA,
               alpha = 0.2, 
               show.legend = FALSE,
  ) +
  # geom_textdensity(aes(x = r_village, 
  #                      label = village,
  #                      fill = village)
  #                  ) +
  theme_void(14) +
  theme(text = element_text(family = fontfam)) + 
  scale_fill_manual(values = palcolors,
                    labels = function(x) 
                      str_replace_all(x, "\\.", " ")) + 
  coord_cartesian(xlim = c(-4,4))

composed_plot <- p_icc2 / icc_plots[[1]] + plot_layout(heights = c(1,4))

png("outputs/icc_fit_irt_2par.png", width = 2800, height = 1800, res = 360)
composed_plot
dev.off()

###########################
#### creating the DAGs ####
###########################
library(dagitty)
library(ggdag)

#-------------#
#### DAG 1 ####
#-------------#
dag <- dagify(gov ~ u1 + u2 + ntri,
              out ~ u1 + u3 + gov,
              ass ~ u2 + u3 + ntri,
              exposure = "gov",
              outcome = "out",
              latent = c("u1","u2","u3"),
              coords = list(
                x = c(u1 = 4, u2 = 2, u3 = 3, gov = 3, out = 5, ass = 1, ntri = 1),
                y = c(u1 = 3, u2 = 0, u3 = 0, gov = 2, out = 2, ass = 0.75, ntri = 2)
              )
)

p_dag1 <- dag %>% 
  tidy_dagitty(layout = "auto", seed = 123) %>%
  arrange(name) %>% # sort them alphabetically
  mutate(type = 
           case_when(
             name %in% c("u1", "u2", "u3") ~ "unobserved",
             name %in% c("out", "gov") ~ "target",
             name %in% c("ntri", "ass") ~ "select"
           )) %>% 
  mutate(
    # adjust for boxes
    xend_for_edges = ifelse(to == "ass" & name != "ntri", xend + 1/6, xend),
    yend_for_edges = ifelse(to == "ass" & name == "ntri", yend + 1/5, yend),
    y_for_edges    = ifelse(to == "ass" & name == "ntri", y - 1/6, y),
    x_for_edges    = ifelse(to == "gov" & name == "ntri", x + 1/8, x)
  ) %>%
  ggplot(
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    )
  ) + 
  theme_dag_blank(14) + 
  geom_dag_point(aes(shape = type,
                     stroke = type == "select",
                     size = type == "select"),
                 show.legend = FALSE) +
  geom_dag_edges(
    aes(
      xend = xend_for_edges,
      yend = yend_for_edges,
      y = y_for_edges,
      x = x_for_edges
    ),
    edge_width = 1/2
  ) +
  geom_dag_text(
    aes(color = type == "target"),
    # sort them alphabetically
    label = c( ass  = expression(Assessment),
               gov  = expression(theta^'Gov'),
               ntri = expression(NTRI),
               out  = expression(theta^'Out'),
               u1   = expression(italic(U[1])),
               u2   = expression(italic(U[2])),
               u3   = expression(italic(U[3]))),
    parse = TRUE,
    show.legend = FALSE,
    family = fontfam
  ) +
  scale_color_manual(values = c("black", onecolor)) + 
  scale_shape_manual(values = c(0, NA, 1)) + 
  scale_size_manual(values = c(20, 30))

png("outputs/dag1.png", width = 1800, height = 900, res = 235)
p_dag1 
dev.off()

#-------------#
#### DAG 2 ####
#-------------#
dag2 <- dagify(gov ~ u123 + u4 + u5 + u6,
               rain ~ u5,
               barepost ~ u123 + gov + rain + inv + barepre,
               barepre ~ u4,
               inv ~ gov + u6,
               exposure = "gov",
               outcome = "barepost",
               coords = list(
                x = c(u123 = 3, gov = 1, barepost = 5, inv = 3, u4 = 3, rain = 4, barepre = 4, u5 = 3, u6 = 2),
                y = c(u123 = 3, gov = 2, barepost = 2, inv = 1, u5 = 4, rain = 3, barepre = 3.5, u4 = 5, u6 = 0.5)
              )
)

p_dag2 <- dag2 %>% 
  tidy_dagitty(layout = "auto", seed = 123) %>%
  arrange(name) %>% # sort them alphabetically
  mutate(type = 
           case_when(
             name %in% c("u123", "u4", "u5", "u6") ~ "unobserved",
             name %in% c("gov", "barepost") ~ "target",
             name %in% c("inv", "rain", "barepre") ~ "observed"
           )) %>% 
  ggplot(
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    )
  ) + 
  theme_dag_blank(14) + 
  geom_dag_point(aes(shape = type),
                 show.legend = FALSE) +
  geom_dag_edges(
    edge_width = 1/2
  ) +
  geom_dag_text(
    aes(color = type == "target"),
    # sort them alphabetically
    label = c( barepost  = expression('Bare'['post']),
               barepre  = expression('Bare'['pre']),
               gov  = expression(theta^'Gov'),
               invasives = expression('Invasives'),
               rain = expression('Rainfall'),
               u123   = expression(italic(U[123])),
               u4   = expression(italic(U[4])),
               u5   = expression(italic(U[5])),
               u6   = expression(italic(U[6]))),
    parse = TRUE,
    show.legend = FALSE,
    family = fontfam
  ) +
  scale_color_manual(values = c("black", onecolor)) + 
  scale_shape_manual(values = c(NA, NA, 1))

png("outputs/dag2.png", width = 1800, height = 900, res = 235)
p_dag2
dev.off()

######################
#### map plotting ####
######################
library(sf)
library(ggmap)
library(ggsn)

shp_projvill <- st_read("data/tnc/Assessment_Villages_2/Assessment_villages_excl_NP.shp") 
shp_projvill <- shp_projvill %>% #choose correct Losirwa: Esilalei ward
  filter(!(Village == "Losirwa" & Ward_Name != "Esilalei"))

# cut the satellite artefact piece off Sangaiwe 
pol <- shp_projvill[shp_projvill$Village == "Sangaiwe",]
box = c(xmin = 35, ymin = -6, xmax = 36, ymax = -2)
shp_projvill[shp_projvill$Village == "Sangaiwe",] <- st_crop(pol, box)

# define excerpt
bbox = c(35, # left
         -6, # bottom
         38, # right
         -2) # top
#### geographical map ####
# maptanz <- get_stamenmap(bbox = bbox,
#                          maptype = "terrain-background")

#saveRDS(maptanz, file = "data/maptanz.RDS")
maptanz <- readRDS("data/maptanz.RDS")

# add country borders; add village borders and names;
pm <- ggmap(maptanz) +
  labs(x = "", y = "") + 
  theme(text = element_text(family = fontfam),
        plot.margin = unit(c(1,6,1/10,-2),"lines"))

# ... scalebar
pm <- pm + 
  scalebar(
    x.min = 35,
    x.max = 38,
    y.min = -6,
    y.max = -2,
    dist = 50,
    dist_unit = "km",
    st.size = 3,
    anchor = c(x = 37.75, y = -2.15),
    transform = TRUE,
    model = "WGS84",
    height = 0.01, st.dist = 0.01,
    box.fill = c("black", "white"),
    border.size = 1/2) 

# ... add arusha and kenya border
shp_border <- st_read("data/shapes_distr/ken_admbnda_adm0_iebc_20191031.shp")

pm <- pm + 
  geom_sf(data = shp_border, inherit.aes = FALSE, fill = NA) + 
  geom_text(data = tibble(x=37.5, y = -2.4), aes(x = x,y = y, label = "Kenya"), size = 6, family = fontfam) + 
  geom_text(data = tibble(x=35.75, y = -5.75), aes(x = x,y = y, label = "Tanzania"), size = 6, famipng("outputs/ppcs_fit_irt_2par.png", width = 4500, height = 6000, res = 380)
            ppc1/ppc2 + plot_layout(heights = c(1,2/3)) +
              plot_annotation(tag_levels = "a") & theme(text = element_text(family = fontfam))
            dev.off()ly = fontfam) + 
  geom_text(data = tibble(x = 36.682995, y = -3.386925), aes(x = x,y = y, label = "Arusha"), size = 4, family = fontfam,
            hjust = -1/10, fontface = "bold") + 
  geom_point(data = tibble(x = 36.682995, y = -3.386925), aes(x = x,y = y), size = 2)

# ... village borders
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

pm <- pm + 
  geom_sf(data = df_projvill_for_plot, fill = "grey", alpha = 1/2, 
          show.legend = FALSE,inherit.aes = FALSE, lwd = 0.25) + 
  geom_text_repel(data = df_projvill_for_plot,
                  aes(x = COORDS_X, y = COORDS_Y, label = Village),
                  size = 3,
                  seed = 1,
                  box.padding = 0.5,
                  segment.size = 1/4,
                  family = fontfam)

# png("outputs/map_terr_vills.png", width = 2000, height = 2450, res = 350)
# print(pm)
# dev.off()

# continent map
afrimap <- map_data(map = "world")

pafri <- ggplot() + 
  coord_fixed(xlim = c(-20, 55),
              ylim = c(-35, 40))  + 
  geom_polygon(
    data = afrimap,
    aes(x = long, y = lat, group = group),
    color = "black", fill = "grey", alpha = 1/10, lwd = 0.25) +
  labs(x = "", y = "")+ 
  theme_bw() + 
  geom_rect(
    aes(
      xmin = bbox[1],
      xmax = bbox[3],
      ymin = bbox[2],
      ymax = bbox[4]
    ),
    fill = NA,
    color = "red"
  ) + 
  theme(text = element_text(family = fontfam),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(-1,-1,-2,-1), "cm"))

png("outputs/map_terr_vills.png", width = 2150, height = 2200, res = 350)
print(
  pm + 
    inset_element(pafri, 
                  left = 0.65, bottom = 0.15, right = 1.4, top = 0.3)
)
dev.off()

####################
#### model gis #####
####################
dfgov <- df_items %>% 
  filter(tier1 != "Outcomes (O)") %>% 
  group_by(village) %>% 
  summarise(gov = mean(as.numeric(as.character(score))))

dfgis <- df_landuse_area %>%
  mutate(area_ha = area_ha / total_area) %>% 
  select(-total_area, -measurement_time) %>% 
  pivot_wider(names_from = period, values_from = area_ha) %>% 
  pivot_wider(names_from = type, values_from = c(Pre, Post)) %>% 
  left_join(d_rain %>% select(village, rain = val)) %>% 
  left_join(
    df_codescores %>% 
      filter(grepl("invasiv", dimension)) %>% 
      select(village, invasive = score)
  ) %>% 
  left_join(dfgov) %>% 
  left_join(df_threats %>% 
            # Analytical question: "Please name and describe the major threats that are affecting this rangeland.
            # A threat is a process/event that has the potential to severely damage an important function of a system.
            # The governance activities within a social-ecological system can be undermined by threats and disturbances that occur."
              filter(grepl("specie", threat)) %>% 
              select(village, invasive_binary = present))%>% 
  mutate(rain_std = as.numeric(scale(rain))) %>% 
  left_join(
    df_popul %>% select(village, pop2012 = `2012`,
                     pop2020 = `2020`)
  )

# # outcome
# dfgis2 <- dfgis %>%
#   mutate(
#     delta_bare = Post_bare-Pre_bare,
#     y = log((delta_bare)/Pre_bare + 1), # equivalent with: log(Post_bare/Pre_bare)
#     gov = as.numeric(scale(gov))
#   )

# with(dfgis, plot(Post_bare/Pre_bare))
# with(dfgis, plot(log(Post_bare/Pre_bare)))
# 
# # exploration
# df_popul %>% 
#   mutate(
#     diff = `2020`-`2012`,
#     rat = `2020`/`2012`,
#     lograt = log(rat)
#   )
# # 

# m <- lm(
#   y ~ gov + rain_std + invasive,
#   data = dfgis2
# )
# summary(m) # -0.11637 

# other models probably statistically better. See considerations in Word doc.
# 
# m2 <- lm(
#   Post_bare ~ Pre_bare + govpostmean + rain + invasive,
# 
#   # 02.3 makes more sense to be used than binary invasive threat indicator:
#   # post-period outcome from governance = mediator with effect on post bare
#   data = dfgis_std %>% mutate(Post_bare = qlogis(Post_bare),
#                               Pre_bare = qlogis(Pre_bare))
# )
#   # %>%
#   #   mutate(
#   #     across(contains("Pre_bare"), ~as.numeric(scale(.x)))
#   #   )
# #  )
# summary(m2)
# plot(m2)


# m3 <- lm(
#   Post_bare ~ Pre_bare * (gov + rain_std + invasive),
#   data = dfgis
#   # %>% 
#   #   mutate(
#   #     across(contains("Pre_bare"), ~as.numeric(scale(.x)))
#   #   )
#   )
# 
# summary(m2)
# summary(m3)
# anova(m2, m3)

# both models imply basically the same gov effect. so main result won't depend on it.
# but there are arguments for model m3 over m2!
# - statistically (could find the same via WAIC and LOO score in bayesian variant to show it)
# - substantively (multiplicativeness: a small absolute area will have less area ADDED than a large area, at same governance quality)
#   indeed plausible estimates: all coeffients plausible direction; and they have same direction in "simple effects"
# #   and "interaction effects" --> larger PRE area means larger - positive or negative - additive area effect. as it should be.
# 
# # yes, that is it!!
# # then I can plot such stuff:
# dg <- datagrid(model = m2,
#          govpostmean = seq(-4,4,0.01),
#          grid_type = "typical"
#          )
# plot_predictions(model = m2, newdata = dg, by = "govpostmean", transform = plogis)
# # and include slopes for other PRE areas, to show that smaller area
# # in beginning implies less steep effect slope of gov, of course.
# 
# # we even find the same tendency in agriculture when we use exact that model
# # (but no invasives, obviously)
# m4 <- lm(
#   Post_crop ~ Pre_crop * (gov + rain_std),
#   data = dfgis
#   %>% 
#     mutate(
#       across(contains("Pre_crop"), ~as.numeric(scale(.x)))
#     ))
# 
# summary(m4)
# dg4 <- datagrid(model = m4,
#                gov = seq(-3,3,0.01),
#                grid_type = "typical"
# )
# plot_predictions(model = m4, newdata = dg4, by = "gov")
# such directrionwould be nice story: in total clear increase from pre to post (see household data and GIS data)
# but gov limits the agricultural conversion (as is also majory's theory)

#... and, 
# a) if rain cannot be motivated or its estimate plausibilzed as covariable
# (--> more rain leads to less agriculture for fixed gov. plausible?), removing it
# still reproduces this direction.
# and, 
# b) if we should adjust for populatino growth (2020/2012), still produces
# this direction AND plausible direction for population growth (see also project report from Majory:
# population growth and agircultural expansion are clearly linked)
# but
# c) CAREFUL: note that 'Post_crop ~ Pre_crop * gov + rain_std' (analogon model of m2 for crop).
# produces opposite prediction direction. Need to understand that to not follow a weird artifact

# tabulate invasives. which to use? do they agree?
df_codescores %>%
  filter(grepl("o2.3", dimension)) %>%
  left_join(df_threats %>% filter(grepl("nvasive", threat))) %>%
  select(village, dimension, score, threat, present)

# -----------------------------------------------------------------------------------
# SO und nun koennte ich noch statt den Metern einfach in area share [0,1] arbeiten.
# erlaubt dann einfacher zB eine transformation machen, die zwischen 0 und 1 bounded (denn area
# hat keine natuerlicher upper bound und ein modell wird immer ueber das groesste village
# hinaus vorherasgen machen)
# ABER: veraendert es (im multiplikativen modell) die schaetzung? oder liefert es äquivalentes modell?
# immerhin wird ja jedes dorf mit einer anderen total area multipliziert/dividiert bei der umrechnung.
#-----------------------------------------------------------------------------------

#############################################################
#### bayesian gaussian regression with measurement error ####
#############################################################
# get the probabilistic version of governance trait
fit_irt_2par <- readRDS(file = "outputs/fit_irt_2par.rds")
df_gov_posteriors <- gather_draws(fit_irt_2par, r_village[village,dimension]) %>% 
  filter(dimension == "itemtypeGovernance") %>% 
  mutate(village = str_replace(village, "\\.", " ")) %>% 
  as_tibble() %>%
  group_by(village) %>% 
  summarise(
    govpostmean = mean(.value),
    govpostsd = sd(.value)
  )
dfgis <- dfgis %>% 
  left_join(df_gov_posteriors)

dfgis_std <- dfgis %>% 
  # logit trans
  mutate(across(contains("bare"), qlogis))%>%
  select(contains("bare"), govpostmean, govpostsd, rain, invasive) %>% 
  # standardize rain and logits of cover
  mutate(across(.cols = c(#contains("bare"), 
                          rain
                          ), ~as.numeric(scale(.x))))

# family, formula, priors
# family_bare <- brmsfamily("gaussian", "identity")
# family_bare <- brmsfamily("binomial", "logit")
prior_bare <-
  prior("normal(0, 2)",  class = "Intercept", resp = "Postbare") +
  prior("normal(0, 2)",  class = "b",         resp = "Postbare") +
  prior("exponential(1)",  class = "sigma",     resp = "Postbare") +
  #prior("constant(0)",     class = "Intercept", resp = "govpostmean") +
  prior("constant(1)",     class = "sigma",     resp = "govpostmean")

# three models with different parametric form
formula_bare1 <- bf(Post_bare ~ mi(govpostmean) + Pre_bare + rain + invasive) + gaussian(identity)
formula_bare2 <- bf(govpostmean | mi(sdy = govpostsd) ~ 0) + gaussian(identity)
  
# formula_bare2 <- 
#   bf(Post_bare ~ mi(govpostmean) * Pre_bare + rain + invasive) + 
#   bf(govpostmean | mi(sdy = govpostsd) ~ 1)
# formula_bare3 <- 
#   bf(Post_bare ~ mi(govpostmean) * (Pre_bare + rain + invasive)) + 
#   bf(govpostmean | mi(sdy = govpostsd) ~ 1)


# # compare exponential and half T
# x <- seq(0,5,0.01)
# plot(x, LaplacesDemon::dhalft(x, scale = 2.5, nu = 3), type = "l", ylim = c(0, 1))
# lines(x, dexp(x, rate = 1.5), type = "l", ylim = c(0, 1), col = "red")

# map(c("bare1"#, 
#       # "bare2", 
#       # "bare3"
#       ), function(x){

  # run sampling
fit_bare <- brm(
    formula = formula_bare1 + formula_bare2 + set_rescor(FALSE),
    data = dfgis_std,
    prior = prior_bare,
    control   = list(adapt_delta = 0.999, max_treedepth = 15),
    warmup    = 2e3,
    iter      = 6e3,
    thin      = 1,
    chains    = 5,
    cores     = 10,
    seed      = 1234,
    save_pars = save_pars(latent = TRUE),
    backend   = "cmdstanr"
  )

# export
# prior_summary(fit_bare)
# summary(fit_bare)
saveRDS(fit_bare, file = paste0("outputs/fit_bare.rds"))
cat(stancode(fit_bare), file = "outputs/fit_bare.txt")
# fit_bare <- readRDS("outputs/fit_bare.rds")

summary(fit_bare)
prior_summary(fit_bare)

# trace plots
mcmc_trace(fit_bare, regex_pars = "b_")
mcmc_trace(fit_bare, regex_pars = "sigma_")
# mcmc_trace(mod2, regex_pars = "b_")
# mcmc_trace(mod3, regex_pars = "b_")

# model comparison
# loo(mod1)
# loo(mod2)
# loo(mod3)


# loo::loo_compare(
#   loo(mod1),
#   loo(mod2),
#   loo(mod3)
# )

#####################################
#### regression table (all pars) ####
#####################################
# table with other parameters posteriors
all_pars <- get_variables(fit_bare)[grepl("Postbare_|govpost|sigma",  get_variables(fit_bare))]
all_pars_tab <- map_dfr(all_pars, function(x){
  gather_draws(fit_bare, !!as.symbol(x)) %>% 
    summarise(mean = mean(.value),
              sd = sd(.value),
              q05 = quantile(.value, 0.05),
              q50 = median(.value),
              q95 = quantile(.value, 0.95))
}) %>% 
  filter(!grepl("Yl", .variable)) %>% # not the posteriors for the measurements. not of interest. why do they change slightly?
# mcElreath: "A cool implication that will arise
# here is that information flows in both directions—the uncertainty in measurement influences
# the regression parameters in the linear model, and the regression parameters in the linear
# model also influence the uncertainty in the measurements. There will be shrinkage."
  filter(!grepl("_govpostmean", .variable)) # do not need again the variables I fixed via priors

all_pars_tab <- all_pars_tab %>% 
  mutate(varlabel = 
           case_when(
             .variable == "b_Postbare_Intercept"                  ~ "$\\gamma_0$",
             .variable == "b_Postbare_Pre_bare"                   ~ "$\\gamma_1$",
             .variable == "b_Postbare_rain"                       ~ "$\\gamma_2$",
             .variable == "b_Postbare_invasive"                   ~ "$\\gamma_3$",
             .variable == "bsp_Postbare_migovpostmean"            ~ "$\\gamma_4$",
             .variable == "sigma_Postbare"                        ~ "$\\sigma$",
           ), .before = ".variable"
  ) %>% 
  mutate(across(where(is.numeric), ~style_number(.x))) %>% 
  select(-.variable)

saveRDS(all_pars_tab, file = "outputs/fit_bare_parstab.rds")

#######################################
#### prediciton and estimate plot ####
######################################
set.seed(1234)
ppoverlay <- bayesplot::pp_check(fit_bare, resp = "Postbare", 
                                 ndraws = 50) +
  # y = as.numeric(as.character(df_items$score)),
  # yrep = yrep_char,
  # group = df_items$tier3,
  # facet_args = list(ncol =4))+ 
  labs(x = "logit(Bare<sub>post</sub>)") +
  theme_classic(12) +
  theme(#strip.text = element_text(size = 8.5),
        text = element_text(family = fontfam),
        axis.title.x = element_markdown(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
        #strip.background = element_blank()) + 
  # scale_y_continuous(breaks = c(0,5,10), limits = c(0,13))+ 
  # scale_x_continuous(limits = c(0,1))

png("outputs/ppcs_fit_bare.png", width = 1750, height = 1500, res = 300)
ppoverlay
dev.off()

# figure parameter estimates
p_bare <- mcmc_intervals(fit_bare, pars = vars(contains("_Post")),
                         prob = 0.5, prob_outer = 0.9, point_est = "median") +
  theme_classic(12) +
  # labs(subtitle = "*&#952;<sub>j</sub><sup>Gov</sup>*") +
  theme(text = element_text(family = fontfam),
        # plot.subtitle = element_markdown()
        ) #+ 
  # scale_y_discrete(
  #   labels = function(x) 
  #     str_replace_all(str_remove_all(str_remove_all(x, "r_village\\["), ",itemtypeGovernance\\]"), "\\.", " ")
  # )
p_bare

# prediction plot
# dg <- datagrid(model = fit_bare,
#                govpostmean = seq(-4,4,0.1),
#                grid_type = "counterfactual") %>% 
#   bind_cols(govpostsd = rep(dfgis_std$govpostsd, length(seq(-4,4,0.1))),
#             village = rep(dfgis$village, length(seq(-4,4,0.1))))

dg <- datagrid(model = fit_bare,
               govpostmean = seq(-4,4,0.1),
               govpostsd = median(fit_bare$data$govpostsd),
               FUN_numeric = median)

# grid with covariates at median
# 
# plot_predictions(model = fit_bare, newdata = dg, by = "govpostmean", resp = "Postbare",
#                  transform = plogis)
# 

preds_bare <- posterior_predict(
  object = fit_bare,
  resp = "Postbare",
  newdata = dg,
  seed = 1234)

preds_bare <- plogis(preds_bare) / plogis(unique(dg$Pre_bare))

df_preds_bare <- as_tibble(preds_bare)
names(df_preds_bare) <- as.character(seq(-4,4,0.1))

df_preds_bare_grp <- df_preds_bare %>% 
  pivot_longer(cols = everything(), names_to = "x", values_to = "y") %>% 
  mutate(x = as.numeric(x)) %>% 
  group_by(x) %>% 
  summarise(
    mean = mean(y),
    lwr = quantile(y, 0.05),
    upr = quantile(y, 0.95)
  )

p_pred <- df_preds_bare_grp %>% 
  ggplot() + 
  geom_hline(yintercept = 1, linetype = 2) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr), alpha = 1/10) +
  geom_line(aes(x = x, y = mean),
                lwd = 1) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam),
        axis.title.x  = element_markdown()) + 
  scale_color_manual(values = gradcolors) + 
  scale_fill_manual(values =  gradcolors) + 
  #scale_y_continuous(limits = c(0,3)) +
  labs(y = "Predicted factor change in bare ground cover",
       x = "Quality of governance processes (*&#952;<sub>j</sub><sup>Gov</sup>*)",
       fill = "Response\ncategory")

png("outputs/pred_bareground.png", width = 2800, height = 1800, res = 360)
p_pred
dev.off()












