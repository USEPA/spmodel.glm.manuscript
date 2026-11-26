#=========================================================#
# Section 3: Modeling moose presence in Alaska, USA ####
#=========================================================#

library("spmodel")

head(moose)

head(moose_preds)

#==========================================#
# Section 3.1: Model Fitting ####
#==========================================#

spbin <- spglm(
  formula = presence ~ elev + strat,
  family = binomial,
  data = moose,
  spcov_type = "exponential"
)

summary(spbin)

tidy(spbin, conf.int = TRUE)

#==========================================#
# Section 3.2: Model Comparison ####
#==========================================#

bin <- spglm(
  formula = presence ~ elev + strat,
  family = binomial,
  data = moose,
  spcov_type = "none"
)
glance(spbin)
glance(bin)

bin_ml <- update(bin, estmethod = "ml")
bin_glm <- glm(
  formula = presence ~ elev + strat,
  family = binomial,
  data = moose
)
logLik(bin_ml)
logLik(bin_glm)

loocv(spbin)
loocv(bin)

spbin2 <- update(spbin, spcov_type = "gaussian")
glances(spbin, spbin2)[, c("model", "AIC", "AICc", "BIC")]

#==========================================#
# Section 3.3: Model Diagnostics ####
#==========================================#

augment(spbin)

varcomp(spbin)

#==========================================#
# Section 3.4: Prediction ####
#==========================================#

predict(spbin, newdata = moose_preds)[1:5]

predict(spbin, newdata = moose_preds, type = "response")[1:5]

predict(spbin, newdata = moose_preds, interval = "prediction")[1:5, ]

augment(spbin, newdata = moose_preds, interval = "prediction")

#=========================================================#
# Section 4: Additional Applications ####
#=========================================================#

#==========================================#
# Section 4.1: Modeling moose counts in Alaska, USA ####
#==========================================#

sppois <- spglm(
  formula = count ~ elev + strat,
  family = poisson,
  data = moose,
  spcov_type = "spherical"
)
spnb <- update(sppois, family = nbinomial)

BIC(sppois, spnb)

sppois_anis <- update(sppois, anisotropy = TRUE)
spnb_anis <- update(spnb, anisotropy = TRUE)

BIC(sppois, spnb, sppois_anis, spnb_anis)

#==========================================#
# Section 4.2: Modeling lake conductivity in Southwest, USA ####
#==========================================#

spgam <- spglm(
  formula = cond ~ temp * state + origin,
  family = "Gamma",
  data = lake,
  spcov_type = "cauchy",
  partition_factor = ~ year
)

summary(spgam)

anova(spgam)

pairs(emmeans::emmeans(spgam, ~ state | temp))

emmeans::emtrends(spgam, ~ state, var = "temp")

#==========================================#
# Section 4.3: Modeling harbor seal trends in Alaska, USA ####
#==========================================#

seal$is_decreasing <- seal$trend < 1
spbin_seal <- spgautor(
  formula = is_decreasing ~ 1,
  family = binomial,
  data = seal,
  spcov_type = "car",
  random = ~ stock
)

tidy(spbin_seal, conf.int = TRUE)

emmeans::emmeans(spbin_seal, ~ 1, type = "response")

predict(spbin_seal, type = "response", interval = "prediction")[1:5, ]

#==========================================#
# Section 4.4: Modeling voter turnout in Texas, USA ####
#==========================================#

spbeta_geo <- spglm(
  formula = turnout ~ log_income,
  family = "beta",
  data = texas,
  spcov_type = "matern"
)

spbeta_auto <- spgautor(
  formula = turnout ~ log_income,
  family = "beta",
  data = texas,
  spcov_type = "car",
  cutoff = 1e5
)

AIC(spbeta_geo, spbeta_auto)

spbeta_full_ml <- update(spbeta_geo, estmethod = "ml")
spbeta_reduced_ml <- update(spbeta_full_ml, formula = turnout ~ 1)
anova(spbeta_full_ml, spbeta_reduced_ml)


#=========================================================#
# Figures ####
#=========================================================#

library("ggplot2", verbose = FALSE)
library("dplyr", verbose = FALSE)
library("patchwork")
theme_caption_sized <- function(size = 14) {
  theme_bw(base_size = size) +
    theme(
      axis.text = element_text(size = size),
      axis.title = element_text(size = size),
      legend.text = element_text(size = size),
      legend.title = element_text(size = size),
      plot.tag = element_text(size = size)
    )
}

#==========================================#
# Figure 1 ####
#==========================================#
h <- seq(0, 1, length.out = 1000)
range1 <- 0.2
dat1 <- data.frame(Distance = h, Correlation = exp(-h/range1), Range = "0.2")
range2 <- 0.5
dat2 <- data.frame(Distance = h, Correlation = exp(-h/range2), Range = "0.5")
range3 <- 0.8
dat3 <- data.frame(Distance = h, Correlation = exp(-h/range3), Range = "0.8")
dat <- bind_rows(dat1, dat2, dat3)
dat$Range <- factor(dat$Range, levels = c("0.8", "0.5", "0.2"))
cor_range <- ggplot(dat, aes(x = Distance, y = Correlation, color = Range, linetype = Range)) +
  geom_line(linewidth = 1.2) +
  scale_color_viridis_d() +
  theme_caption_sized(13)

h <- seq(0, 1, length.out = 1000)
range <- 0.5
dat1 <- data.frame(Distance = h, Correlation = exp(-h/range), Type = "Exponential")
dat2 <- data.frame(Distance = h, Correlation = exp(-(h/range)^2), Type = "Gaussian")
dat3 <- data.frame(Distance = h, Correlation = (1 - 1.5 * h/range + 0.5 * (h/range)^3) * (h <= range), Type = "Spherical")
dat <- bind_rows(dat1, dat2, dat3)
cor_type <- ggplot(dat, aes(x = Distance, y = Correlation, color = Type, linetype = Type)) +
  geom_line(linewidth = 1.2) +
  scale_color_viridis_d() +
  labs(color = "Function", linetype = "Function") +
  theme_caption_sized(13)
correlations <- cor_range | cor_type
correlations

#==========================================#
# Figure 2 ####
#==========================================#

okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
moose_comb <- bind_rows(moose |> mutate(samp = "yes"), moose_preds |> mutate(samp = "no"))
obs_pred_presence <- ggplot(moose_comb, aes(color = presence, shape = samp)) +
  geom_sf(size = 2) +
  scale_color_manual(name = "moose", values = okabe[1:2], breaks = c(1, 0), labels = c("yes", "no")) +
  scale_shape_manual(name = "sampled", values = c(19, 17), breaks = c("yes", "no"), labels = c("yes", "no")) +
  theme_caption_sized(15) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2)
  ) +
  geom_sf(size = 2, data = moose_comb %>% filter(samp == "no"), color = "red", shape = 17)
obs_pred_presence

#==========================================#
# Figure 3 ####
#==========================================#

spbin_aug <- augment(spbin)

p1 <- ggplot(spbin_aug, aes(color = .hat)) +
  geom_sf(size = 1) +
  scale_color_viridis_c(option = "E", name = ".hat") +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_caption_sized(14)

p2 <- ggplot(spbin_aug, aes(color = .std.resid)) +
  geom_sf(size = 1) +
  scale_color_viridis_c(option = "A", limits = c(-2, 2)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_caption_sized(14)

p1 + p2

#==========================================#
# Figure 4 ####
#==========================================#

#+ fig.width=10, fig.height=5
old_par <- par(no.readonly = TRUE)
par(mfrow = c(1, 2), mar = c(5.1, 4.1, 4.1, 3.2))
par(cex = 1.5, cex.main = 1, cex.axis = 0.95, cex.sub = 0.95)
plot(spbin, which = c(4, 7))
par(old_par)

#==========================================#
# Figure 5 ####
#==========================================#

preds_aug <- augment(
  spbin,
  newdata = moose_preds,
  type.predict = "response",
  interval = "prediction"
)
spbin_aug <- augment(spbin, type.predict = "response")
comb_aug <- bind_rows(spbin_aug %>% mutate(Type = "Fitted"), preds_aug %>% mutate(Type = "Pred"))

obs_pred_fitted <- ggplot(comb_aug, aes(color = .fitted, shape = Type)) +
  geom_sf(size = 2) +
  scale_color_viridis_c(option = "H", name = "Prob", limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_caption_sized(16)
obs_pred_fitted

#==========================================#
# Figure 6 ####
#==========================================#

p1 <- ggplot(preds_aug, aes(color = .lower)) +
  geom_sf(pch = 17) +
  scale_color_viridis_c(option = "H", name = "Lower", limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_caption_sized(14)

p2 <- ggplot(preds_aug, aes(color = .upper)) +
  geom_sf(pch = 17) +
  scale_color_viridis_c(option = "H", name = "Upper", limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_caption_sized(14)

p1 + p2

#==========================================#
# Figure 7 ####
#==========================================#

obs_pred_count <- ggplot(moose_comb, aes(color = count, shape = samp)) +
  geom_sf(size = 2) +
  scale_color_binned(breaks = c(0, 1, 2, 5, 10, 20, 40), type = "viridis") +
  scale_shape_manual(name = "sampled", values = c(19, 17), breaks = c("yes", "no"), labels = c("yes", "no")) +
  theme_caption_sized(16) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  geom_sf(size = 2, data = moose_comb %>% filter(samp == "no"), color = "red", shape = 17)
obs_pred_count

#==========================================#
# Figure 8 ####
#==========================================#

#+ fig.width=10, fig.height=5
old_par <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
par(cex = 1.5, cex.main = 1, cex.axis = 0.95, cex.sub = 0.95)
plot(spnb, which = 8)
plot(spnb_anis, which = 8)
par(old_par)

#==========================================#
# Figure 9 ####
#==========================================#
seal$sampled <- factor(if_else(is.na(seal$trend), "no", "yes"), levels = c("yes", "no"))
seal$trend_category <- factor(
  case_when(
    seal$trend > 1 ~ "Increasing",
    seal$trend < 1 ~ "Decreasing"
  ),
  levels = c("Increasing", "Decreasing")
)
p1 <- ggplot(seal, aes(fill = sampled, color = stock)) +
  geom_sf(size = 0.25) +
  theme_caption_sized(14) +
  scale_fill_manual(name = "sampled", values = okabe[1:2], breaks = c("yes", "no"), labels = c("yes", "no")) +
  scale_color_manual(name = "stock", values = c("grey40", "grey80"), breaks = c(8, 10), labels = c(8, 10)) +
  scale_y_continuous(breaks = seq(57, 59, length.out = 5)) +
  scale_x_continuous(breaks = seq(-139, -134, length.out = 3))

p2 <- ggplot(seal, aes(fill = trend_category)) +
  geom_sf(size = 0.25) +
  theme_caption_sized(14) +
  scale_fill_manual(name = "Trend", values = c("Increasing" = okabe[3], "Decreasing" = okabe[4]), breaks = c("Increasing", "Decreasing"), na.value = "grey50") +
  scale_y_continuous(breaks = seq(57, 59, length.out = 5)) +
  scale_x_continuous(breaks = seq(-139, -134, length.out = 3))

p1 + p2

#==========================================#
# Figure 10 ####
#==========================================#

texas$samp <- factor(if_else(is.na(texas$turnout), "no", "yes"), levels = c("yes", "no"))
obs_prop2 <- ggplot(texas, aes(color = turnout, shape = samp)) +
  geom_sf(size = 2) +
  scale_color_viridis_c(option = "D") +
  scale_shape_manual(name = "sampled", values = c(19, 17), breaks = c("yes", "no"), labels = c("yes", "no")) +
  theme_caption_sized(16) +
  scale_x_continuous(breaks = seq(-94, -106, length.out = 3)) +
  geom_sf(size = 2, data = texas %>% filter(samp == "no"), color = "red", shape = 17)
obs_prop2

#=========================================================#
# Session Information ####
#=========================================================#

sessionInfo()

# R code spun into .md and .html using knitr::spin("code.R")
