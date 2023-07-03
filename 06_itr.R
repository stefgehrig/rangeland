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
           grepl("Interactions", tier1) |
           grepl("rs2.1", dimension) |
           grepl("rs2.2", dimension) |
           grepl("a5.1", dimension) |
           grepl("a4.1", dimension) |
           grepl("Outcome", tier1))
irm_items = df %>% 
  select(dimension, dimension_coding) %>% 
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
# common slope continuation ratio logit model
# "cumulative" type with common slope, i.e. proportional odds ("graded response" ITM)
family_2pl <- brmsfamily("cumulative", "logit")
prior_2pl <-
  prior("constant(1)",  class = "sd", group = "village") + # like burkner, p. 37
  prior("exponential(2)", class = "sd") +
  prior("exponential(2)", class = "sd", dpar = "disc") + # folded normal(0,1) by burkner, p. 37, and is also default; has mean 0.8, just like expon(1.25)
  prior("normal(0, 2.5)", class = "Intercept") # default: student(df = 3, scale 2.5)
formula_2pl <- bf(
  score ~ 1 + (1 | tier3) + (0 + itemtype | village),
  disc  ~ 1 + (1 | tier3))

# fit_ord_cum_2pl <- brm(
#   formula = formula_2pl,
#   data = df,
#   family = family_2pl,
#   prior = prior_2pl,
#   control   = list(adapt_delta = 0.999, max_treedepth = 15),
#   warmup    = 1e3,
#   iter      = 3e3,
#   thin      = 1,
#   chains    = 2,
#   cores     = 2,
#   seed      = 123,
#   backend = "cmdstanr"
# )
# saveRDS(fit_ord_cum_2pl, file = "outputs/fit_ord_cum_2pl.rds")
fit_ord_cum_2pl <- readRDS("outputs/fit_ord_cum_2pl.rds")

# all evaluations finite? (could not be achieved with ajadcent categories models)
# table(is.finite(log_lik(fit_ord_cum_2pl)))

# summary
prior_summary(fit_ord_cum_2pl)
summary(fit_ord_cum_2pl)

# some plots
p0 = mcmc_intervals(fit_ord_cum_2pl, regex_pars = "r_village")/ # village intercepts
mcmc_intervals(fit_ord_cum_2pl, regex_pars = "r_tier3\\[")/ # DF variable tier3 intercepts
mcmc_intervals(fit_ord_cum_2pl, regex_pars = "r_tier3__disc") +  # DF variable tier3 discrimination
  plot_annotation(tag_levels = "a")
mcmc_trace(fit_ord_cum_2pl, regex_pars = "sd_")

# we get a correlation parameter :D
p1 = mcmc_areas_ridges(fit_ord_cum_2pl, regex_pars = "cor_village") + 
  geom_vline(xintercept = 0, lty = 2, lwd = 1) + theme_classic(10)
cor_post = as_draws(fit_ord_cum_2pl, variable = "cor_village", regex = TRUE) %>% 
  unlist %>% as.numeric
mean(cor_post>0) # 0.90

summary(fit_ord_cum_2pl)
hist(brms::rhat(fit_ord_cum_2pl), xlim = c(0.995,1.05), breaks = seq(0.985,1.05,0.001))
abline(v = 1.01)

# compare
loo(fit_ord_cum_2pl)

# posteiror predictive checks
# CUMULATIVE
yrep_char <- posterior_predict(fit_ord_cum_2pl)
ppc_bars_grouped(
  y = as.numeric(as.character(df$score)),
  yrep = yrep_char,
  group = df$dimension)
ppc_bars_grouped(
  y = as.numeric(as.character(df$score)),
  yrep = yrep_char,
  group = df$village)

# scatter plot with some draws of latent traits for each village
get_variables(fit_ord_cum_2pl)
veff = gather_draws(fit_ord_cum_2pl, r_village[village,dimension], ndraws = 50, seed = 123)

p2 = veff %>% 
  pivot_wider(names_from = dimension, values_from = .value) %>% 
  ggplot(aes(x = itemtypeGovernance, y = itemtypeOutcome, color = village)) +
  geom_point(alpha = 3/4) +
  geom_mark_ellipse(aes(fill = village),
                    alpha = 1/10,
                    show.legend = FALSE,
                    col = NA,
                    expand = unit(1, "mm")) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam)) +
  scale_color_manual(values = palcolors) + 
  scale_fill_manual(values = palcolors) + 
  coord_cartesian(xlim = c(-4,4), ylim = c(-4,4))

png("outputs/param_posts.png", width = 3000, height = 6000, res = 400)
p0
dev.off()

png("outputs/cor_itm.png", width = 5000, height = 2500, res = 320)
p1+p2+plot_layout(widths = c(1,3))+
  plot_annotation(tag_levels = "a")
dev.off()

# #####################################################
# ### GIS outcome, now also moderation & adjustment ###
# #####################################################
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
