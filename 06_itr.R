# describe between-village variation visually
# in different ways, but no hard explanatory analysis yet and no GIS outcomes
library(tidyverse)
library(glue)
library(extrafont)
library(patchwork)
library(ggrepel)
library(janitor)
library(brms)
library(cmdstanr)
library(RColorBrewer)
library(tidybayes)
library(bayesplot)
library(broom.mixed)
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
           grepl("Interactions", tier1) |
           grepl("rs2.1", dimension) |
           grepl("rs2.2", dimension) |
           grepl("a5.1", dimension) |
           grepl("a4.1", dimension) |
           grepl("Outcome", tier1))
df %>% 
  select(dimension, dimension_coding) %>% 
  distinct %>% print(n=100) 
# janitor::tabyl(df_hhsurv$tribe=="Maasai")
# df_codescores %>% 
#   filter(grepl("ethn", dimension))

################################
#### cluster geographically ####
################################
df <- df %>% left_join(df_admlevels)
table(df$region, df$district)
table(df$village, df$district)

# ##################
# #### Modeling ####
# ##################
# # 2 P Model
# family_2pl <- brmsfamily("cumulative", "logit")
# prior_ord_2pl <-
#   prior("constant(1)",    class = "sd", group = "village") +
#   prior("normal(0, 2)", class = "sd") +
#   prior("normal(0, 1)",   class = "sd", group = "tier3", dpar = "disc")
# # FULL
# formula_2pl_full <- bf(
#   score ~ 1 + (1 |i| tier1) + (1 |k| tier3) + (1 | district) + (1|village),
#   disc  ~ 1 + (1 |i| tier1) + (1 |k| tier3))
# fit_ord_2pl_full <- brm(
#   formula = formula_2pl_full,
#   data = df,
#   family = family_2pl,
#   prior = prior_ord_2pl,
#   control   = list(adapt_delta = 0.95),
#   warmup    = 1e3,
#   iter      = 2.5e3,
#   thin      = 1,
#   chains    = 2,
#   cores     = 2,
#   backend = "cmdstanr"
# )
# # REDUCED
# formula_2pl_red <- bf(
#   score ~ 1 + (1 | tier3) +  (1|village),
#   disc  ~ 1 + (1 | tier3))
# fit_ord_2pl_red <- brm(
#   formula = formula_2pl_red,
#   data = df,
#   family = family_2pl,
#   prior = prior_ord_2pl,
#   control   = list(adapt_delta = 0.95),
#   warmup    = 1e3,
#   iter      = 2.5e3,
#   thin      = 1,
#   chains    = 2,
#   cores     = 2,
#   backend = "cmdstanr"
# )

# # compare
# fit_ord_2pl_full = add_criterion(fit_ord_2pl_full, "loo", moment_match = TRUE)
# fit_ord_2pl_red = add_criterion(fit_ord_2pl_red, "loo", moment_match = TRUE)
# loo_compare(fit_ord_2pl_full,
#             fit_ord_2pl_red)


# posteriors
# mcmc_areas_ridges(fit_ord_2pl, regex_pars = "r_district") # district intercepts
# mcmc_intervals(fit_ord_2pl_red, regex_pars = "r_village") # village intercepts
# mcmc_intervals(fit_ord_2pl_red, regex_pars =  "r_tier1\\[") # DF variable tier1 intercepts
# mcmc_intervals(fit_ord_2pl_red, regex_pars =  "r_tier3\\[") # DF variable tier3 intercepts
# mcmc_intervals(fit_ord_2pl_red, regex_pars =  "r_tier3__disc") # DF variable tier3 discrimination
# careful, it is the inverse for disc (https://psyarxiv.com/x8swp/):
# "In brms, the parameter related to latent variances is called disc (short for “discrimination”), following
# conventions in item response theory. Importantly, disc is not the variance itself, but the
# inverse of the standard deviation, s. That is, s = 1/disc. Further, because disc must be
# strictly positive, it is by default modeled on the log-scale."

################
### multidim ###
################
df$itemtype = ifelse(grepl("Outcome", df$tier1), "Outcome", "Governance")

df %>% 
  distinct(itemtype, dimension, dimension_coding) %>% print(n=1e2)

# 2 P Model (CUM LOGIT)
family_2pl <- brmsfamily("cumulative", "logit")
prior_2pl <-
  prior("constant(1)",  class = "sd", group = "village") +
  prior("normal(0, 1.5)", class = "sd") +
  prior("normal(0, 1)", class = "sd", group = "tier3", dpar = "disc")

formula_2pl <- bf(
  score ~ 1 + (1 | tier3) + (0 + itemtype | village),
  disc  ~ 1 + (1 | tier3))

fit_ord_2pl <- brm(
  formula = formula_2pl,
  data = df,
  family = family_2pl,
  prior = prior_2pl,
  control   = list(adapt_delta = 0.995, max_treedepth = 15),
  warmup    = 1e3,
  iter      = 2e3,
  thin      = 1,
  chains    = 2,
  cores     = 2,
  seed      = 123,
  backend = "cmdstanr"
)

prior_summary(fit_ord_2pl)
summary(fit_ord_2pl)

mcmc_intervals(fit_ord_2pl, regex_pars = "r_village") # village intercepts
mcmc_intervals(fit_ord_2pl, regex_pars = "r_tier3\\[") # DF variable tier3 intercepts
mcmc_intervals(fit_ord_2pl, regex_pars = "r_tier3__disc") # DF variable tier3 discrimination
saveRDS(fit_ord_2pl, file = "outputs/fit_ord_2pl.rds")

# we get a correlation parameter :D
mcmc_areas_ridges(fit_ord_2pl, regex_pars = "cor_village") + 
  geom_vline(xintercept = 0, lty = 2, lwd = 1)
cor_post = as_draws(fit_ord_2pl, variable = "cor_village", regex = TRUE) %>% 
  unlist %>% as.numeric
mean(cor_post>0)
# scatter plot with 100 draws of latent traits for each village
# - with points drawn from joint
# - with lines drawn from joint (since both SD = 1, correl coeffiction = slope)
# []....

############################################
### other parametric form and check WAIC ###
############################################
# more flexibility needed? e.g., adjacent category, category-specific parameters/thresholds, ...
# see buerkner examples

###############################
### moderation & adjustment ###
###############################
# cont & centered
# - econ HETERO
# - ethn HETERO (to be build)
# - rainfall
# - population growth
