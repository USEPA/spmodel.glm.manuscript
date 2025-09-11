# Preliminaries
library(tidyverse)
library(spmodel)
library(patchwork)
library(here)
fig_path <- here("inst", "manuscript", "figures")


#######################
######## Figure 1
#######################
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
  theme_bw(base_size = 14)
cor_range
ggsave(
  filename = paste0(fig_path, "/figure-01.png"),
  plot = cor_range,
  dpi = 300,
  height = 6.07,
  width = 6.49
)

#######################
######## Figure 1
#######################
h <- seq(0, 1, length.out = 1000)
range <- 0.5
dat1 <- data.frame(Distance = h, Correlation = exp(-h/range), Type = "Exponential")
dat2 <- data.frame(Distance = h, Correlation = exp(-(h/range)^2), Type = "Gaussian")
dat3 <- data.frame(Distance = h, Correlation = (1 - 1.5 * h/range + 0.5 * (h/range)^3) * (h <= range), Type = "Spherical")
dat <- bind_rows(dat1, dat2, dat3)
cor_type <- ggplot(dat, aes(x = Distance, y = Correlation, color = Type, linetype = Type)) +
  geom_line(linewidth = 1.2) +
  scale_color_viridis_d() +
  theme_bw(base_size = 14)
cor_type
ggsave(
  filename = paste0(fig_path, "/figure-02.png"),
  plot = cor_type,
  dpi = 300,
  height = 6.07,
  width = 6.49
)


#######################
######## Figure 1
#######################

okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
moose_comb <- bind_rows(moose |> mutate(samp = "yes"), moose_preds |> mutate(samp = "no"))
obs_pred_presence <- ggplot(moose_comb, aes(color = presence, shape = samp)) +
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
obs_pred_presence
ggsave(
  filename = paste0(fig_path, "/figure-1.png"),
  plot = obs_pred_presence,
  dpi = 300,
  height = 5,
  width = 7.68
)

#######################
######## Figure 2
#######################


spbin <- spglm(
  formula = presence ~ elev + strat,
  family = binomial,
  data = moose,
  spcov_type = "exponential"
)
spbin_aug <- augment(spbin)

p1 <- ggplot(spbin_aug, aes(color = .hat)) +
  geom_sf(size = 1) +
  scale_color_viridis_c(option = "E", name = ".hat") +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

p2 <- ggplot(spbin_aug, aes(color = .std.resid)) +
  geom_sf(size = 1) +
  scale_color_viridis_c(option = "A", limits = c(-2, 2)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

p1 + p2
ggsave(
  filename = paste0(fig_path, "/figure-2.png"),
  plot = p1 + p2,
  dpi = 300,
  height = 3,
  width = 9
)

########################
####### Figure 3
########################


png(paste0(fig_path, "/figure-3.png"), height = 5, width = 10, units = "in", res = 300)
par(mfrow = c(1, 2))
plot(spbin, which = 4)
plot(spbin, which = 7)
par(mfrow = c(1, 1))
dev.off()


########################
####### Figure 4
########################

preds_aug <- augment(
  spbin,
  newdata = moose_preds,
  type.predict = "response",
  interval = "prediction"
)
spbin_aug <- augment(spbin, type.predict = "response", interval = "prediction")
comb_aug <- bind_rows(spbin_aug %>% mutate(Type = "Fitted"), preds_aug %>% mutate(Type = "Pred"))

obs_pred_fitted <- ggplot(comb_aug, aes(color = .fitted, shape = Type)) +
  geom_sf(size = 2) +
  scale_color_viridis_c(option = "H", name = "Prob", limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw()

ggsave(
  filename = paste0(fig_path, "/figure-4.png"),
  plot = obs_pred_fitted,
  dpi = 300,
  height = 5,
  width = 7.68
)

########################
####### Figure 5
########################

p1 <- ggplot(preds_aug, aes(color = .lower)) +
  geom_sf(pch = 17) +
  scale_color_viridis_c(option = "H", name = "Lower", limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

p2 <- ggplot(preds_aug, aes(color = .upper)) +
  geom_sf(pch = 17) +
  scale_color_viridis_c(option = "H", name = "Upper", limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  theme_bw(base_size = 14)

p1 + p2
ggsave(
  filename = paste0(fig_path, "/figure-5.png"),
  plot = p1 + p2,
  dpi = 300,
  height = 3,
  width = 9
)

########################
####### Figure 6
########################

obs_pred_count <- ggplot(moose_comb, aes(color = count, shape = samp)) +
  geom_sf(size = 2) +
  scale_color_binned(breaks = c(0, 1, 2, 5, 10, 20, 40), type = "viridis") +
  scale_shape_manual(name = "sampled", values = c(19, 17), breaks = c("yes", "no"), labels = c("yes", "no")) +
  theme_bw(base_size = 14) +
  scale_y_continuous(breaks = seq(62.6, 63.6, length.out = 5)) +
  scale_x_continuous(breaks = seq(-148.5, -146, length.out = 3)) +
  geom_sf(size = 2, data = moose_comb %>% filter(samp == "no"), color = "red", shape = 17)
obs_pred_count
ggsave(
  filename = paste0(fig_path, "/figure-6.png"),
  plot = obs_pred_count,
  dpi = 300,
  height = 5,
  width = 7.68
)

########################
####### Figure 7
########################

spnbin <- spglm(
  formula = count ~ elev + strat,
  family = nbinomial,
  data = moose,
  spcov_type = "spherical",
  anisotropy = TRUE
)

spnbin_iso <- update(spnbin, anisotropy = FALSE)

png(paste0(fig_path, "/figure-7.png"), height = 5, width = 10, units = "in", res = 300)
par(mfrow = c(1, 2))
plot(spnbin_iso, which = 8)
plot(spnbin, which = 8)
par(mfrow = c(1, 1))
dev.off()

########################
####### Figure 8
########################
seal$trend <- seal$log_trend
seal$sampled <- factor(if_else(is.na(seal$trend), "no", "yes"), levels = c("yes", "no"))
p1 <- ggplot(seal, aes(fill = sampled, color = stock)) +
  geom_sf(size = 2) +
  theme_bw(base_size = 14) +
  scale_fill_manual(name = "sampled", values = okabe[1:2], breaks = c("yes", "no"), labels = c("yes", "no")) +
  scale_color_manual(name = "stock", values = c("grey40", "grey80"), breaks = c(8, 10), labels = c(8, 10)) +
  scale_y_continuous(breaks = seq(57, 59, length.out = 5)) +
  scale_x_continuous(breaks = seq(-139, -134, length.out = 3))

p2 <- ggplot(seal, aes(fill = trend)) +
  geom_sf(size = 2) +
  theme_bw(base_size = 14) +
  scale_fill_viridis_c(option = "D", begin = 0.5, limits = c(min(seal$trend), max(seal$trend))) +
  scale_y_continuous(breaks = seq(57, 59, length.out = 5)) +
  scale_x_continuous(breaks = seq(-139, -134, length.out = 3))

p1 + p2
ggsave(
  filename = paste0(fig_path, "/figure-8.png"),
  plot = p1 + p2,
  dpi = 300,
  height = 3,
  width = 9
)


########################
####### Figure 10
########################

texas$samp <- factor(if_else(is.na(texas$turnout), "no", "yes"), levels = c("yes", "no"))
obs_prop2 <- ggplot(texas, aes(color = turnout, shape = samp)) +
  geom_sf(size = 2) +
  scale_color_viridis_c(option = "D") +
  scale_shape_manual(name = "sampled", values = c(19, 17), breaks = c("yes", "no"), labels = c("yes", "no")) +
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks = seq(-94, -106, length.out = 3)) +
  geom_sf(size = 2, data = texas %>% filter(samp == "no"), color = "red", shape = 17)

ggsave(
  filename = paste0(fig_path, "/figure-10.png"),
  plot = obs_prop2,
  dpi = 300,
  height = 5,
  width = 7.68
)
