library(tidyverse)
library(glue)
library(brms)
library(tidybayes)
library(bayesplot)
library(extrafont)
library(patchwork)
library(ggrepel)
library(ggtext)
library(ggcorrplot)
library(ggiraphExtra)
library(RColorBrewer)
library(ggradar2)
loadfonts()
source("00_functions.R")
fontfam <- "Segoe UI"
palcolors <- colorRampPalette(brewer.pal(11, "Set3"))(12)

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
             var == "bare pre" ~ "Landsat imagery 2010-2015",
             var == "bare post" ~ "Landsat imagery 2016-2021",
             var == "crop pre" ~ "Landsat imagery 2015",
             var == "crop post" ~ "Landsat imagery 2019",
             var == "econ_het" ~ "Household survey 2020",
             var == "rainfall" ~ "Satellite precipitation data 2016-2020",
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
            alpha = 0.1,
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

# boxplots
input_spec <- 
  list(
    c("Bare ground area Pre (ha)",
      "Bare ground area Post (ha)"),
    c("Cropland area Pre (ha)",
      "Cropland area Post (ha)"),
    c("Mean annual rainfall (mm)",
      "Economic inequality (SD of PC1 from household assets)")
  )


boxes_list<- map(input_spec, function(x){
  
  set.seed(12)
  gat_boxes <- df_others %>% 
    filter(var %in% x) %>% 
    mutate(x = runif(nrow(.), -0.2, 0.2)) %>% 
    ggplot() + 
    geom_boxplot(aes(y = val, x = x), fill = "grey80", 
                 size = 1/8,
                 alpha = 0.25, outlier.colour = NA) + 
    geom_point(aes(x = x, y = val)) + 
    ggrepel::geom_text_repel(aes(x = x, y = val, label = village),
                             size = 3, family = fontfam, vjust = 1,
                             segment.color = 'grey40',
                             max.overlaps = 2e1) + 
    facet_wrap(~ var, 
               scales = if(grepl("area", x[1])) "fixed" else "free", 
               ncol = 2) + 
    theme_bw(14) +
    theme(
      text = element_text(family = fontfam),
      strip.background = element_blank(),
      axis.text.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(x = "",
         y = "")
})

png("outputs/gat_boxes.png", width = 2750, height = 3250, res = 290)
wrap_plots(boxes_list,
           ncol = 1)
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

fit_irt_2par <- brm( # RTOOLS! warum is 4.3 nicht genug?
  formula = formula,
  data = df_items,
  family = family,
  prior = priors,
  control   = list(adapt_delta = 0.999, max_treedepth = 15),
  warmup    = 2e3,
  iter      = 6e3,
  thin      = 1,
  chains    = 5,
  cores     = 5,
  seed      = 1234,
  backend   = "cmdstanr"
)

summary(fit_irt_2par)
prior_summary(fit_irt_2par)
# export model results
saveRDS(fit_irt_2par, file = "outputs/fit_irt_2par.rds")
cat(stancode(fit_irt_2par), file = "outputs/fit_irt_2par_stancode.txt")
sink("outputs/fit_irt_2par_modsummary.txt")
summary(fit_irt_2par)
sink()
sink("outputs/fit_irt_2par_priorsummary.txt")
prior_summary(fit_irt_2par)
sink()
fit_irt_2par <- readRDS(file = "outputs/fit_irt_2par.rds")

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
  plot_annotation(tag_levels = "a")
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
  theme(axis.text.y = element_blank(),
        text = element_text(family = fontfam)) + 
  theme_classic(14) +
  theme(text = element_text(family = fontfam),
        plot.subtitle = element_markdown()) +
  labs(subtitle = "*&#961;<sub>Gov, Out</sub>*",
       y = "Posterior density") + 
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
  labs(x = "Quality of governance processes (*&#952;<sup>Gov</sup>*)",
       y = "Quality of governance outcomes (*&#952;<sup>Out</sup>*)",
       color = "Village")

png("outputs/cor_itm_fit_irt_2par.png", width = 4500, height = 1800, res = 360)
p_cor1 + p_cor2 +
  plot_annotation(tag_levels = "a")
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

  

