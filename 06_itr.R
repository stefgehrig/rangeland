# describe between-village variation visually
# in different ways, but no hard explanatory analysis yet and no GIS outcomes
library(tidyverse)
library(glue)
library(extrafont)
library(patchwork)
library(ggrepel)
library(janitor)
library(brms)
library(ggforce)
library(cmdstanr)
library(RColorBrewer)
library(tidybayes)
library(bayesplot)
library(broom.mixed)
library(rcartocolor)
loadfonts()
source("00_functions.R")
fontfam <- "Segoe UI"
palcolors <- carto_pal(n = 12, "Vivid")

###################
#### load data ####
###################
map(list.files("data_processed", ".csv"), function(x){
  assign(x = str_remove(x, ".csv"), value = read_csv(glue("data_processed/{x}")), envir = .GlobalEnv)
})

df <- name_the_tiers(df_codescores)

###################################
#### only use qualitative data ####
###################################
df <- df %>% 
  filter(!grepl("contin", dimension_coding)) %>% 
  mutate(score = ifelse(grepl("binary", dimension_coding) & score == 2, 3, score)) %>% 
  mutate(score = factor(score, ordered = TRUE, levels = 1:3))

#################################################################
#### Subset of indep vars suggested by Monique ("Part2"+"3") ####
#################################################################
df <- df %>% 
  filter(grepl("Governance", tier1) |
           grepl("Interactions", tier1) | # conflict resolution
           grepl("rs2.1", dimension) | # boundaries
           grepl("rs2.2", dimension) | # boundaries
           grepl("a5.1", dimension) | # trust intra
           grepl("a5.2", dimension) | # trust inter
           grepl("a4.1", dimension) | # leadership
           grepl("Outcome", tier1))
irm_items = df %>% 
  select(dimension, tier1, dimension_coding) %>% 
  distinct %>% print(n=100) 

saveRDS(irm_items, file = "outputs/irm_items.rds")
# janitor::tabyl(df_hhsurv$tribe=="Maasai")
# df_codescores %>% 
#   filter(grepl("ethn", dimension))

# summary stats
irm_items_dist = df %>% tabyl(dimension, score)
saveRDS(irm_items_dist, file = "outputs/irm_items_dist.rds")

################################
#### cluster geographically ####
################################
df <- df %>% left_join(df_admlevels)
table(df$region, df$district)
table(df$village, df$district)

####################################
### multidim item response model ###
####################################
df$itemtype = ifelse(grepl("Outcome", df$tier1), "Outcome", "Governance")

# 
# # # comparing folded normals and some corresponding exponential priors
# library(VGAM)
# # # sd = 1.5 folded normal vs exponential (1)
# # plot(density(rfoldnorm(8e3, mean = 0, sd = 1.5, a1 = 1, a2 = 1), from = 0, bw = "SJ"))
# # lines(density(rexp(8e3, rate = 1), from = 0, bw = "SJ"), col = "blue")
# # 
# # sd = 1 folded normal
# plot(density(rfoldnorm(2e4, mean = 0, sd = 1, a1 = 1, a2 = 1), from = 0, bw = "SJ"))
# lines(density(rexp(2e4, rate = 1.25), from = 0, bw = "SJ"), col = "blue")
# lines(density(rexp(2e4, rate = 2), from = 0, bw = "SJ"), col = "red")
# 

df %>% 
  distinct(itemtype, dimension, dimension_coding) %>% print(n=1e2)
# common slope continuation ratio logit model was removed, does not convergece
# "cumulative" type with common slope, i.e. proportional odds ("graded response" ITM)
family_2pl <- brmsfamily("cumulative", "logit")
prior_2pl <-
  prior("constant(1)",  class = "sd", group = "village") + # like buerkner, p. 37
  prior("exponential(2)", class = "sd") +
  prior("exponential(2)", class = "sd", dpar = "disc") + # folded normal(0,1) by burkner, p. 37, and is also default; has mean 0.8, just like expon(1.25)
  prior("student_t(3, 0, 2.5)", class = "Intercept") # default: student(df = 3, 0, scale 2.5). coudl also go Normal(0,2.5)
formula_2pl <- bf(
  score ~ 1 + (1 | tier3) + (0 + itemtype | village),
  disc  ~ 1 + (1 | tier3))

fit_ord_cum_2pl <- brm(
  formula = formula_2pl,
  data = df,
  family = family_2pl,
  prior = prior_2pl,
  control   = list(adapt_delta = 0.999, max_treedepth = 15),
  warmup    = 1e3,
  iter      = 6e3,
  thin      = 1,
  chains    = 2,
  cores     = 10,
  seed      = 1234,
  backend = "cmdstanr"
)
saveRDS(fit_ord_cum_2pl, file = "outputs/fit_ord_cum_2pl.rds")
fit_ord_cum_2pl <- readRDS("outputs/fit_ord_cum_2pl.rds")

# all evaluations finite? (could not be achieved with adjacent categories and continuation ratio models)
# table(is.finite(log_lik(fit_ord_cum_2pl)))

# summary
prior_summary(fit_ord_cum_2pl)
summary(fit_ord_cum_2pl)

# some plots
p0 = mcmc_intervals(fit_ord_cum_2pl, regex_pars = "r_village")/ # village intercepts
mcmc_intervals(fit_ord_cum_2pl, regex_pars = "r_tier3\\[")/ # DF variable tier3 intercepts
mcmc_intervals(fit_ord_cum_2pl, regex_pars = "r_tier3__disc") +  # DF variable tier3 discrimination
  plot_annotation(tag_levels = "a")& 
  theme_classic(14)
mcmc_trace(fit_ord_cum_2pl, regex_pars = "sd_")

# we get a correlation parameter :D
corvarname <- get_variables(fit_ord_cum_2pl)[grepl("cor_village", get_variables(fit_ord_cum_2pl))]
p1 = mcmc_areas_ridges(fit_ord_cum_2pl, 
                       corvarname) + 
  geom_vline(xintercept = 0, lty = 2, lwd = 1) + theme_classic(14) + 
  theme(axis.text.y = element_blank(),
        text = element_text(family = fontfam)) + 
  labs(subtitle = corvarname)
  
cor_post = as_draws(fit_ord_cum_2pl, variable = "cor_village", regex = TRUE) %>% 
  unlist %>% as.numeric
mean(cor_post>0) # 0.95

summary(fit_ord_cum_2pl)
hist(brms::rhat(fit_ord_cum_2pl), xlim = c(0.995,1.05), breaks = seq(0.985,1.05,0.001))
abline(v = 1.01)

# compare
#loo(fit_ord_cum_2pl)

# posteiror predictive checks
# CUMULATIVE
yrep_char <- posterior_predict(fit_ord_cum_2pl)
ppc1 <- ppc_bars_grouped(
  y = as.numeric(as.character(df$score)),
  yrep = yrep_char,
  group = df$dimension)+ 
  theme_classic(14)
ppc2 <- ppc_bars_grouped(
  y = as.numeric(as.character(df$score)),
  yrep = yrep_char,
  group = df$village) + 
  theme_classic(14)

png("outputs/ppcs.png", width = 6000, height = 3000, res = 375)
ppc1+ppc2
dev.off()

# # posterior scatter plot (version ellipses)
# get_variables(fit_ord_cum_2pl)
# veff = gather_draws(fit_ord_cum_2pl, r_village[village,dimension], ndraws = 50, seed = 123)
# p2 = veff %>% 
#   pivot_wider(names_from = dimension, values_from = .value) %>% 
#   ggplot(aes(x = itemtypeGovernance, y = itemtypeOutcome, color = village)) +
#   geom_point(alpha = 3/4) +
#   geom_mark_ellipse(aes(fill = village),
#                     alpha = 1/10,
#                     show.legend = FALSE,
#                     col = NA,
#                     expand = unit(1, "mm")) +
#   theme_classic(14) +
#   theme(text = element_text(family = fontfam)) +
#   scale_color_manual(values = palcolors) + 
#   scale_fill_manual(values = palcolors) + 
#   coord_cartesian(xlim = c(-4,4), ylim = c(-4,4))


# posterior scatter plot (version contours)
p2 = gather_draws(fit_ord_cum_2pl, r_village[village,dimension]) %>%
  pivot_wider(values_from = .value, names_from = dimension)%>% 
  ggplot(aes(x = itemtypeGovernance, y = itemtypeOutcome, color = village, group = village, fill = village)) +
  geom_density_2d(lwd = 0.75, contour_var = "ndensity", breaks = c(0.1)) +
  geom_point(data = gather_draws(fit_ord_cum_2pl, r_village[village,dimension], ndraws = 5e2, seed = 123) %>% 
               pivot_wider(names_from = dimension, values_from = .value), alpha = 1/4) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam)) +
  scale_color_manual(values = palcolors) + 
  coord_cartesian(xlim = c(-3,3), ylim = c(-3,3))

png("outputs/param_posts.png", width = 3000, height = 6000, res = 400)
p0
dev.off()

png("outputs/cor_itm.png", width = 5000, height = 2500, res = 320)
p1+p2+plot_layout(widths = c(1,1.5))+
  plot_annotation(tag_levels = "a")
dev.off()

############################
############################
#### GIS vs. Governance ####
############################
############################
# DAG
library(dagitty)
library(ggdag)
dag <- dagify(barePost ~ barePre + rain + inv + cropPost + gov,
              cropPost ~ cropPre + gov + rain,
              inv      ~ gov,
              rain     ~ u1,
              gov      ~ u1,
              inv      ~ u2,
              gov      ~ u2,
              barePre  ~ u3 + cropPre,
              gov      ~ u3,
              cropPre  ~ u4,
              gov      ~ u4,
              exposure = "gov",
              outcome = "barePost",
              latent = c("u1","u2","u3","u4"),
              labels = c("inv" = "invasives",
                         "barePost"="barePost",
                         "cropPost"="cropPost",
                         "barePre"="barePre",
                         "cropPre"="cropPre",
                         "rain"="rainfall",
                         "gov"="governance",
                         "u1"="U1",
                         "u2"="U2",
                         "u3"="U3",
                         "u4"="U4")
)
ggdag(dag, layout = "circle",
      text = FALSE, use_labels = "label",
      edge_type = "diagonal") + 
  theme_dag_blank(14)

dag %>% ggdag_adjustment_set(effect = "direct")

# get log factor change in gis
lup_ratios <- df_landuse_area %>%
  group_by(village, type, period) %>%
  summarise(landprop = area_ha/total_area,
            .groups = "drop_last") %>%
  summarise(pre_landprop = landprop[period == "pre"],
            post_landprop = landprop[period == "post"],
            .groups = "drop") %>%
  mutate(logratio = log(post_landprop/pre_landprop)) %>%  # more crop or bare than previously is higher value
  dplyr::select(-contains("landprop")) %>%
  pivot_wider(names_from = type, values_from = logratio, names_prefix = "logratio_")

# # posterior Outcome latent trait vs bareground change
# veff %>% 
#   filter(dimension == "itemtypeGovernance") %>% 
#   mutate(village = str_replace(village, "\\.", " ")) %>% 
#   left_join(lup_ratios) %>% 
#   ggplot(aes(x = logratio_bare, y = .value, col = village)) + 
#   geom_point() +
#   geom_smooth(method = "lm")
  
# rainfall and GIS and governance into one data frame
dfbare <- df_landuse_area %>% filter(type == "bare") %>%
  dplyr::select(-measurement_time, -total_area, -type) %>%
  pivot_wider(names_from = period, values_from = area_ha) %>% 
  rename(post_bare = post,
         pre_bare = pre)
dfcrop <- df_landuse_area %>% filter(type == "crop") %>%
  dplyr::select(-measurement_time, -type) %>%
  pivot_wider(names_from = period, values_from = area_ha) %>% 
  rename(post_crop = post,
         pre_crop = pre)

dfgis <- df_rainfall %>%
  filter(year >= 2016) %>%
  group_by(village) %>%
  summarise(rain = mean(mean_annual_precip)) %>%
  left_join(dfbare) %>% 
  left_join(dfcrop) %>% 
  left_join(df_threats %>% filter(threat == "Invasive species") %>% dplyr::select(village, invasives = present)) %>% 
  left_join(lup_ratios) %>% 
  left_join(
    df %>% 
      filter(!grepl("compliance", dimension)) %>% 
      filter(!grepl("condition.trend", dimension)) %>% 
      filter(!grepl("invasives", dimension)) %>% 
      group_by(village) %>% 
      summarise(gov = mean(as.numeric(as.character(score))))
  ) %>% 
  left_join(df %>% filter(grepl("invasives", dimension)) %>% 
              mutate(score = as.numeric(as.character(score))) %>% 
              dplyr::select(village, invasives_score = score)) %>% 
  mutate(change_crop = post_crop - pre_crop,
         rain = scale(rain))

cor(as.numeric(as.character(dfgis$invasives_score)), as.numeric(dfgis$invasives))

# exploratory model
dfgis$change_crop
post_bare
m1 <- lm(
  post_bare ~ pre_bare + rain + gov + invasives_score + change_crop,
  data = dfgis
)
summary(m1)
m2 <- lm(
  log(post_bare) ~ log(pre_bare) + rain + invasives_score + gov + log(post_crop),
  data = dfgis
)
summary(m2)
summary(m1)$r.squared # 0.9923702
summary(m2)$r.squared # 0.9956928; multiplicative model seems to fit better

with(dfgis, log((post_crop-pre_crop)/total_area))
# ... which should give approximately the same as this model
m3 <- lm(
  logratio_bare ~ rain + gov + invasives_score + logratio_crop, 
  data = dfgis
)
plot(dfgis$logratio_bare, dfgis$logratio_crop)
##
summary(m3) # -0.23, SE: 0.11, OK !
# note: summarizes the indirect effect via cropland (if it's there) and invasives (if it's there)
# [BUT CAREFUL: their effectiveness HINDERS the effectiveness as measured by bare ground change]
# and the direct effect (under assumption of my handwritten DAG). 
# note: we need BOTH in model to "work", marginally doesnt work, as, for example, 
# - Kimana has too bad GIS outcome for the masses of rainfall
# - Matale A has too bas GIS outcome for the execellence of governance
# -> mututal adjustment solves thath

# get the probabilistic version of governance trait
gov_post <- gather_draws(fit_ord_cum_2pl, r_village[village,dimension]) %>% 
  filter(dimension == "itemtypeGovernance") %>% 
  mutate(village = str_replace(village, "\\.", " ")) %>% 
  as_tibble() %>% 
  group_by(village) %>% 
  summarise(
    gov_mean = mean(.value),
    gov_sd = sd(.value)
  )
dfgis <- dfgis %>% 
  left_join(gov_post)

# model with measurement error
# .. but could do some more regularizing priors. Maybe simply normal ones for coefficients.
family_bare <- brmsfamily("gaussian", "identity")
formula_bare_ <- bf(
  log(post_bare) ~ log(pre_bare) + rain + invasives_score + me(gov_mean, gov_sd) # assumes normal
  + log(post_crop)
)

prior_bare <-
  # prior("normal(0,2.5)",  class = "b") +
  prior("constant(0)", class = "meanme") + # by parametrization summary(fit_ord_cum_2pl)
  prior("constant(1)", class = "sdme") # by parametrizatino of summary(fit_ord_cum_2pl)

fit_bare <- brm(
  formula = formula_bare_,
  data = dfgis,
  family = family_bare,
  prior = prior_bare,
  control   = list(adapt_delta = 0.999, max_treedepth = 15),
  warmup    = 1e3,
  iter      = 5e3,
  thin      = 1,
  chains    = 2,
  cores     = 10,
  seed      = 123,
  backend = "cmdstanr"
)
summary(fit_bare)
# megov_meangov_sd   Est -0.15  SD:  0.13 
saveRDS(fit_bare, file = "outputs/fit_bare.rds")

# show posterior for governance effect
corvarname <- get_variables(fit_bare)[grepl("bsp_megov_meangov_sd", get_variables(fit_bare))]
p1 = mcmc_areas_ridges(fit_bare, 
                       corvarname) + 
  geom_vline(xintercept = 0, lty = 2, lwd = 1) + theme_classic(14) + 
  theme(axis.text.y = element_blank(),
        text = element_text(family = fontfam)) + 
  labs(subtitle = corvarname)

png("outputs/megov.png", width = 2000, height = 1500, res = 320)
p1
dev.off()

# ###################################
# #### moderation by disturbance ####
# ###################################
# # add moderator to data
# covariable = df_codescores %>%
#   filter(dimension == "a2.1.economic.heterogeneity") %>%
#   select(village, het = score)
# df <- df %>%
#   left_join(covariable)
# 
# family_2pl <- brmsfamily("cumulative", "logit")
# prior_2pl <-
#   prior("constant(1)",  class = "sd", group = "village") + # like burkner, p. 37
#   prior("exponential(2)", class = "sd") +
#   prior("exponential(2)", class = "sd", dpar = "disc") + # folded normal(0,1) by burkner, p. 37, and is also default; has mean 0.8, just like expon(1.25)
#   prior("normal(0, 2.5)", class = "Intercept") # default: student(df = 3, scale 2.5)
# formula_2pl <- bf(
#   score ~ 1 + (1 | tier3) + (0 + itemtype | village) + het + het:itemtype,
#   disc  ~ 1 + (1 | tier3))
# 
# fit_ord_cum_2pl_MODERATED <- brm(
#   formula = formula_2pl,
#   data = df,
#   family = family_2pl,
#   prior = prior_2pl,
#   control   = list(adapt_delta = 0.99, max_treedepth = 15),
#   warmup    = 1e3,
#   iter      = 2e3,
#   thin      = 1,
#   chains    = 2,
#   cores     = 2,
#   seed      = 1,
#   backend = "cmdstanr"
# )
# 
# summary(fit_ord_cum_2pl)
# summary(fit_ord_cum_2pl_MODERATED)
# 
# 
# # #####################################################
# # ### GIS outcome, now also moderation & adjustment ###
# # #####################################################
# # add moderator to data
# covariable = df_codescores %>%
#   filter(dimension == "a2.1.economic.heterogeneity") %>%
#   select(village, het = score)
# df <- df %>%
#   left_join(covariable)


# df <- name_the_tiers(df_codescores)
# df <- df %>% 
#   mutate(score = ifelse(grepl("binary", dimension_coding) & score == 2, 3, score)) %>% 
#   mutate(score = factor(score, ordered = TRUE, levels = 1:3))
# 
# df
# 
# # cont & centered
# # - econ HETERO
# # - ethn HETERO (to be build)
# # - rainfall
# # - population growth
# 
# # A) Rainfall
# df = df %>% 
#   left_join(
#     df_codescores %>% filter(grepl("rain", dimension)) %>% 
#       select(village, rainfall = score)) %>% 
#   mutate(rainfall = scale(rainfall))
# 
# #### modeling with moderator ####
# formula_2pl_Hetero <- bf(
#   score ~ 1 + (1 | tier3) + (0 + itemtype | village),
#   disc  ~ 1 + (1 | tier3))
# 
# prior_2pl_Hetero <-
#   prior("constant(1)",  class = "sd", group = "village") + # like burkner, p. 37
#   prior("exponential(2)", class = "sd") +
#   prior("exponential(2)", class = "sd", dpar = "disc") + # folded normal(0,1) by burkner, p. 37, and is also default; has mean 0.8, just like expon(1.25)
#   prior("normal(0, 2.5)", class = "Intercept")# + # default: student(df = 3, scale 2.5)
#   # prior("normal(0, 1.5)", class = "b")
# 
# fit_ord_cum_2pl_Hetero <- brm(
#   formula = formula_2pl_Hetero,
#   data = df,
#   family = family_2pl,
#   prior = prior_2pl_Hetero,
#   control   = list(adapt_delta = 0.999, max_treedepth = 15),
#   warmup    = 1e3,
#   iter      = 3e3,
#   thin      = 1,
#   chains    = 2,
#   inits     = "0",
#   cores     = 2,
#   seed      = 123,
#   backend = "cmdstanr"
# )
# 
# 
# 
# summary(fit_ord_cum_2pl_Hetero)
# 
