# Preliminaries
library(tidyverse)
library(spmodel)
library(patchwork)
library(here)
fig_path <- here("inst", "jss", "manuscript", "figures")

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
  formula = presence ~ elev + strat + elev:strat,
  family = binomial,
  data = moose,
  spcov_type = "spherical"
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
  formula = count ~ elev + strat + elev:strat,
  family = nbinomial,
  data = moose,
  spcov_type = "gaussian",
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
seal$trend <- exp(seal$log_trend)
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
  scale_fill_viridis_c(option = "D", begin = 0.5, limits = c(0, max(seal$trend))) +
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
####### Figure 9
########################

obs_prop <- ggplot(caribou, aes(x = x, y = y, color = z, shape = tarp, size = water)) +
  geom_point() +
  scale_color_viridis_c(option = "H") +
  scale_size_discrete(range = c(2, 4)) +
  theme_bw(base_size = 14) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.03, 'npc')
  )

ggsave(
  filename = paste0(fig_path, "/figure-9.png"),
  plot = obs_prop,
  dpi = 300,
  height = 5,
  width = 7.68
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
