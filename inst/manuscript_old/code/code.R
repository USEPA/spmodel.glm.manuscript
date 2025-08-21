#=========================================================#
# Load Software ####
#=========================================================#

library(spmodel)
library(ggplot2)
library(dplyr)
library(emmeans)

#=========================================================#
# Section 3.1 Binary Data ####
#=========================================================#

head(moose)

spbin <- spglm(
  formula = presence ~ elev + strat + elev:strat,
  family = binomial,
  data = moose,
  spcov_type = "spherical"
)

summary(spbin)

tidy(spbin)

moose$strat2 <- factor(moose$strat, levels = c("M", "L"))
update(spbin, formula = presence ~ elev + strat2 + elev:strat2) |>
  summary()

emtrends(spbin, "strat", "elev")

glance(spbin)

head(augment(spbin))

varcomp(spbin)

head(predict(spbin, newdata = moose_preds))

head(augment(
  spbin,
  newdata = moose_preds,
  type.predict = "response",
  interval = "prediction"
))

# fit nonspatial model using spglm()
bin <- spglm(
  formula = presence ~ elev + strat + elev:strat,
  family = binomial,
  data = moose,
  spcov_type = "none"
)
# fit nonspatial model using glm()
bin_glm <- glm(
  formula = presence ~ elev + strat + elev:strat,
  family = binomial,
  data = moose
)
# compare fixed effect coefficients and standard errors up to four decimals
data.frame(
  est_none = coef(bin),
  est_glm = coef(bin_glm),
  se_none = sqrt(diag(vcov(bin))),
  se_glm = sqrt(diag(vcov(bin_glm)))
) |>
  apply(2, round, digits = 4)

glances(spbin, bin)

loocv(spbin) |>
  apply(2, round, digits = 4)
loocv(bin) |>
  apply(2, round, digits = 4)

AUROC(spbin)
AUROC(bin)

## Figures ####

### Figure 1 ####
okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
moose_comb <- bind_rows(moose |> mutate(samp = "yes"), moose_preds |> mutate(samp = "no"))
ggplot(moose_comb, aes(color = presence, shape = samp)) +
  geom_sf(size = 2) +
  scale_color_manual(name = "moose", values = okabe[1:2], breaks = c(1, 0), labels = c("yes", "no")) +
  scale_shape_manual(name = "sampled", values = c(19, 17), breaks = c("yes", "no"), labels = c("yes", "no")) +
  theme_bw(base_size = 16) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2)
  ) +
  geom_sf(size = 2, data = moose_comb %>% filter(samp == "no"), color = "red", shape = 17)

### Figure 2 ####
spbin_aug <- augment(spbin)

ggplot(spbin_aug, aes(color = .hat)) +
  geom_sf(size = 2) +
  scale_color_viridis_c(option = "E", name = ".hat") +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

ggplot(spbin_aug, aes(color = .std.resid)) +
  geom_sf(size = 2) +
  scale_color_viridis_c(option = "A", limits = c(-2, 2)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

### Figure 3 ####
plot(spbin, which = 4)
plot(spbin, which = 7)

### Figure 4 ####
preds_aug <- augment(
  spbin,
  newdata = moose_preds,
  type.predict = "response",
  interval = "prediction"
)
spbin_aug <- augment(spbin, type.predict = "response", interval = "prediction")
comb_aug <- bind_rows(spbin_aug %>% mutate(Type = "Fitted"), preds_aug %>% mutate(Type = "Pred"))

ggplot(comb_aug, aes(color = .fitted, shape = Type)) +
  geom_sf(size = 2) +
  scale_color_viridis_c(option = "H", name = "Prob", limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw()

### Figure 5 ####

ggplot(preds_aug, aes(color = .lower)) +
  geom_sf(size = 2, pch = 17) +
  scale_color_viridis_c(option = "H", name = "Lower", limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

ggplot(preds_aug, aes(color = .upper)) +
  geom_sf(size = 2, pch = 17) +
  scale_color_viridis_c(option = "H", name = "Upper", limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

#=========================================================#
# Section 3.2 Count Data ####
#=========================================================#

sppois <- spglm(
  formula = count ~ elev + strat + elev:strat,
  family = poisson,
  data = moose,
  spcov_type = "gaussian",
  anisotropy = TRUE
)

spnbin <- update(sppois, family = nbinomial)

glances(sppois, spnbin, sort_by = "AIC") |>
  subset(select = c(model, npar, AIC, AICc, BIC))

loocv(sppois) |>
  apply(2, round, digits = 4)
loocv(spnbin) |>
  apply(2, round, digits = 4)

spnbin_iso <- update(spnbin, anisotropy = FALSE)
glances(spnbin_iso, spnbin, sort_by = "AIC") |>
  subset(select = c(model, npar, AIC, AICc, BIC))

tidy(spnbin, effects = "spcov")

## Figures ####

### Figure 6 ####

ggplot(moose_comb, aes(color = count, shape = samp)) +
  geom_sf(size = 2) +
  scale_color_binned(breaks = c(0, 1, 2, 5, 10, 20, 40), type = "viridis") +
  scale_shape_manual(name = "sampled", values = c(19, 17), breaks = c("yes", "no"), labels = c("yes", "no")) +
  theme_bw(base_size = 14) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  geom_sf(size = 2, data = moose_comb %>% filter(samp == "no"), color = "red", shape = 17)

### Figure 7 ####

plot(spnbin_iso, which = 8)
plot(spnbin, which = 8)

#=========================================================#
# Section 3.3 Skewed Data ####
#=========================================================#

seal$trend <- exp(seal$log_trend)

spgam <- spgautor(
  formula = trend ~ 1,
  family = Gamma,
  data = seal,
  spcov_type = "sar",
  random = ~ stock
)
spinvg <- update(spgam, family = inverse.gaussian)
glances(spgam, spinvg, sort_by = "AIC") |>
  subset(select = c(model, npar, AIC, AICc, BIC))

tidy(spinvg, effects = "randcov")

# output omitted in manuscript; head() used here
head(predict(spinvg, type = "response", interval = "prediction"))

head(augment(
  spinvg,
  newdata = spinvg$newdata,
  type.predict = "response",
  interval = "prediction"
))

## Figures ####

### Figure 8 ####

seal$sampled <- factor(if_else(is.na(seal$trend), "no", "yes"), levels = c("yes", "no"))
ggplot(seal, aes(fill = sampled, color = stock)) +
  geom_sf(size = 2) +
  theme_bw(base_size = 14) +
  scale_fill_manual(name = "sampled", values = okabe[1:2], breaks = c("yes", "no"), labels = c("yes", "no")) +
  scale_color_manual(name = "stock", values = c("grey40", "grey80"), breaks = c(8, 10), labels = c(8, 10)) +
  scale_y_continuous(breaks = seq(57, 59, length.out = 5)) +
  scale_x_continuous(breaks = seq(-139, -134, length.out = 3))

ggplot(seal, aes(fill = trend)) +
  geom_sf(size = 2) +
  theme_bw(base_size = 14) +
  scale_fill_viridis_c(option = "D", begin = 0.5, limits = c(0, max(seal$trend))) +
  scale_y_continuous(breaks = seq(57, 59, length.out = 5)) +
  scale_x_continuous(breaks = seq(-139, -134, length.out = 3))

#=========================================================#
# Section 3.4 Proportion Data ####
#=========================================================#

spbeta <- spglm(
  formula = z/100 ~ water + tarp + water:tarp,
  family = "beta",
  data = caribou,
  spcov_type = "matern",
  xcoord = x,
  ycoord = y
)

tidy(anova(spbeta))

spemm <- emmeans(spbeta, ~ water + tarp)
spemm

update(spemm, type = "response")

head(pairs(spemm, adjust = "none"))

spgautor_mods <- spgautor(
  formula = turnout ~ log_income,
  family = beta,
  data = texas,
  spcov_type = c("car", "sar"),
  cutoff = 2e5,
  estmethod = "ml"
)

glances(spgautor_mods) |>
  subset(select = c(model, npar, AIC, AICc, BIC))

tidy(spgautor_mods$car)

reduced_car <- update(spgautor_mods$car, formula = turnout ~ 1)
tidy(anova(reduced_car, spgautor_mods$car))

## Figures ####

### Figure 9 ####

ggplot(caribou, aes(x = x, y = y, color = z, shape = tarp, size = water)) +
  geom_point() +
  scale_color_viridis_c(option = "H") +
  scale_size_discrete(range = c(2, 4)) +
  theme_bw(base_size = 14) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.03, 'npc')
  )

### Figure 10 ####

texas$samp <- factor(if_else(is.na(texas$turnout), "no", "yes"), levels = c("yes", "no"))
ggplot(texas, aes(color = turnout, shape = samp)) +
  geom_sf(size = 2) +
  scale_color_viridis_c(option = "D") +
  scale_shape_manual(name = "sampled", values = c(19, 17), breaks = c("yes", "no"), labels = c("yes", "no")) +
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks = seq(-94, -106, length.out = 3)) +
  geom_sf(size = 2, data = texas %>% filter(samp == "no"), color = "red", shape = 17)

#=========================================================#
# Session Information ####
#=========================================================#

sessionInfo()

# R code spun into .md and .html using knitr::spin(code.R)
