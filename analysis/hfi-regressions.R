library('sf')         # to work with spData maps
library('sp')         # to import spatial layers
library('ctmm')       # using the github version (0.6.1)
library('raster')     # to import human footprint index raster
library('dplyr')      # for data wrangling
library('purrr')      # for functional mapping
library('ggplot2')    # for plotting
library('cowplot')    # for plot grids
library('mgcv')       # for fitting GAMs
library('gratia')     # for GAM plots
theme_set(theme_bw())
N <- 74 # number of tapirs

# color palette
pal <- c('#4477AA', '#ff8c00', '#66CCEE', '#009900',
         '#CCBB44', '#EE6677', '#AA3377', '#BBBBBB')

tapirs <- readRDS('models/tapirs-final.rds') %>% # tapir data
  mutate(tau.velocity.est = tau.velocity.est / 60 / 60, # from seconds to hours
         tau.velocity.low = tau.velocity.low / 60 / 60,
         tau.velocity.high = tau.velocity.high / 60 / 60)

# hfi regressions ####
hfi.raster <- raster('data/hfi-layers/ml_hfi_v1_2019.nc') # HFI

# extract mean hfi and hr areas
plot(tapirs$akde[[5]])
extract(hfi.raster, as.sf(tapirs$akde[[5]])) # [[1]]=lwr, [[2]]=est, [[3]]=upr

tapirs <-
  mutate(tapirs,
         hfi.mean = map_dbl(1:N,
                            function(i)
                              extract(hfi.raster,
                                      as.sf(akde[[i]]))[[2]] %>% # take estimate
                              mean(na.rm = TRUE)),
         hr.size = map_dbl(akde,
                           function(a) summary(a)$CI[2]))
unique(warnings()) # warning on change of projection is ok

# hfi on hr size ####
# Gamma GLM regression
m0 <- gam(hr.size ~ hfi.mean,
          family = Gamma('log'),
          data = tapirs,
          method = 'REML')
summary(m0)

pred0 <- tibble(hfi.mean = seq(0.003, 0.31, length.out = 400))
pred0 <- bind_cols(pred0,
                   predict(m0, newdata = pred0, se.fit = TRUE)) %>%
  mutate(est = exp(fit),
         lwr = exp(fit - 1.96 * se.fit),
         upr = exp(fit + 1.96 * se.fit))

# no significant improvement with a gamma GAM
m1 <- gam(hr.size ~ s(hfi.mean),
          family = Gamma('log'),
          data = tapirs,
          method = 'REML')
summary(m1)

# regression plot
hfi.hr <-
  ggplot() +
  geom_ribbon(aes(hfi.mean, ymin = lwr, ymax = upr), pred0, alpha = 0.2) +
  geom_line(aes(hfi.mean, est), pred0) +
  geom_segment(aes(x = hfi.mean, xend = hfi.mean, y = area.low,
                   yend = area.high, color = region.lab), tapirs, lwd = 0.5,
               alpha = 0.5) +
  geom_point(aes(hfi.mean, hr.size, color = region.lab), tapirs, alpha = 0.9) +
  scale_color_manual('Region', values = pal[1:3]) +
  labs(x = 'ml-HFI', y = expression('Home Range Area'~(km^2))) +
  theme(legend.position = 'top'); hfi.hr

AIC(m0, m1)

# accounting for different regions also doesn't make a difference
m2 <- gam(hr.size ~ s(hfi.mean, by = region),
          family = Gamma('log'),
          data = tapirs,
          method = 'REML')
summary(m2)
AIC(m0, m1, m2)
gratia::draw(m2)

# hfi on average speed ####
m3 <- gam(speed.est ~ s(hfi.mean), # restricted to be linear by REML
          family = Gamma('log'),
          data = tapirs,
          method = 'REML')
summary(m3)
draw(m3)

pred3 <-
  bind_cols(select(pred0, hfi.mean),
            predict(m3, newdata = pred0, se.fit = TRUE)) %>%
  mutate(est = exp(fit),
         lwr = exp(fit - 1.96 * se.fit),
         upr = exp(fit + 1.96 * se.fit))

# regression plot
hfi.speed <-
  ggplot(tapirs) +
  geom_ribbon(aes(hfi.mean, ymin = lwr, ymax = upr), pred3, alpha = 0.2) +
  geom_line(aes(hfi.mean, est), pred3) +
  geom_segment(aes(x = hfi.mean, xend = hfi.mean, y = speed.low,
                   yend = speed.high, color = region.lab), lwd = 0.5,
               alpha = 0.5) +
  geom_point(aes(hfi.mean, speed.est, color = region.lab), alpha = 0.9)+
  scale_color_manual('Region', values = pal[1:3]) +
  labs(x = 'ml-HFI', y = 'Average speed (km/day)') +
  theme(legend.position = 'top')

# without Mata Atlantica
filter(tapirs, region != 'atlantica') %>%
  ggplot() +
  geom_ribbon(aes(hfi.mean, ymin = lwr, ymax = upr),
              filter(pred0, hfi.mean >= 0.138), alpha = 0.2) +
  geom_line(aes(hfi.mean, est), filter(pred0, hfi.mean >= 0.138)) +
  geom_segment(aes(x = hfi.mean, xend = hfi.mean, y = speed.low,
                   yend = speed.high, color = region.lab), lwd = 0.5,
               alpha = 0.5) +
  geom_point(aes(hfi.mean, speed.est, color = region.lab), alpha = 0.9)+
  scale_color_manual('Region', values = pal[2:3]) +
  labs(x = 'ml-HFI', y = expression('Home Range Area'~(km^2))) +
  theme(legend.position = 'top')

# save plot
plot_grid(get_legend(hfi.hr),
          plot_grid(hfi.hr + theme(legend.position = 'none'),
                    hfi.speed + theme(legend.position = 'none'),
                    ncol = 1, labels = c('a)', 'b)')),
          ncol = 1, rel_heights = c(0.05, 1))
ggsave('figures/hfi-regressions.png', height = 4, width = 3.23, scale = 1.5,
       bg = 'white')

gam(speed.est ~ hfi.mean,
    family = Gamma('log'),
    data = filter(tapirs, region != 'atlantica'),
    method = 'REML') %>%
  summary()

# hfi on tau_v ####
m4 <- gam(tau.velocity.est ~ hfi.mean, # restricted to be linear by REML
          family = Gamma('log'),
          data = tapirs,
          method = 'REML')
summary(m4)

# without outlier estimate
m4 <- gam(tau.velocity.est ~ hfi.mean,
          family = Gamma('log'),
          data = filter(tapirs, tau.velocity.est < 1.5),
          method = 'REML')
summary(m4)

pred4 <-
  bind_cols(tibble(hfi.mean = c(0.004, 0.31)),
            predict(m4, newdata = tibble(hfi.mean = c(0.004, 0.31)),
                    se.fit = TRUE)) %>%
  mutate(est = exp(fit),
         lwr = exp(fit - 1.96 * se.fit),
         upr = exp(fit + 1.96 * se.fit))
