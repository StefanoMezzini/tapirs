library('sf')        # to work with spData maps
library('sp')        # to import spatial layers
library('ctmm')      # using the github version (0.6.1)
library('raster')    # to import human footprint index raster
library('dplyr')     # for data wrangling
library('purrr')     # for functional mapping
library('ggplot2')   # for plotting
library('mgcv')      # for fitting GAMs
theme_set(theme_bw())
N <- 74 # number of tapirs

# color palette
pal <- c('#4477AA', '#ff8c00', '#66CCEE', '#009900',
         '#CCBB44', '#EE6677', '#AA3377', '#BBBBBB')

tapirs <- readRDS('models/tapirs-final.rds') # tapir data
sa <- filter(spData::world, continent == 'South America') # south america layer
hfi.raster <- raster('data/hfi-layers/ml_hfi_v1_2019.nc') # HFI

# extract mean hfi and hr areas
plot(tapirs$akde[[5]])
extract(hfi.raster, as.sf(tapirs$akde[[5]])) # [[1]]=lwr, [[2]]=est, [[3]]=upr

tapirs <-
  mutate(tapirs,
         region = factor(region), # need factors for GAMs
         region.lab = factor(region.lab,
                             levels = c('Mata Atlantica','Pantanal','Cerrado')),
         hfi.mean = map_dbl(1:N,
                            function(i)
                              extract(hfi.raster,
                                      as.sf(akde[[i]]))[[2]] %>% # take estimate
                              mean(na.rm = TRUE)),
         hr.size = map_dbl(akde,
                           function(a) summary(a)$CI[2]))
unique(warnings())

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

pred1 <- tibble(hfi.mean = seq(0.003, 0.31, length.out = 400),
                est = predict(m1, newdata = tibble(hfi.mean), type = 'response'))

ggplot() +
  geom_ribbon(aes(hfi.mean, ymin = lwr, ymax = upr), pred0, alpha = 0.2) +
  geom_line(aes(hfi.mean, est), pred0) +
  geom_point(aes(hfi.mean, hr.size, color = region.lab), tapirs, alpha = 0.9) +
  scale_color_manual('Region', values = pal[1:3]) +
  labs(x = 'Human Footprint Index', y = expression('Home Range Area'~(km^2))) +
  theme(legend.position = 'top')
ggsave('figures/hr-hfi-regression.png', height = 3, width = 5, scale = 1.5)

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
m3 <- gam(speed.est ~ hfi.mean,
          family = Gamma('log'),
          data = tapirs,
          method = 'REML')
summary(m3)
plot(m3, all.terms = TRUE, trans = exp) # on multiplicative scale

# quick regression plot
ggplot(tapirs) +
  geom_smooth(aes(hfi.mean, speed.est), method = 'lm', color = 'black',
              lwd = 1, na.rm = TRUE) +
  geom_point(aes(hfi.mean, speed.est, color = region.lab), alpha = 0.9)+
  scale_color_manual('Region', values = pal[1:3]) +
  labs(x = 'Human Footprint Index', y = expression('Home Range Area'~(km^2))) +
  theme(legend.position = 'top')
ggsave('figures/speed-hfi-regression.png', height = 3, width = 5, scale = 1.5)

# without Mata Atlantica
filter(tapirs, region != 'atlantica') %>%
ggplot() +
  geom_smooth(aes(hfi.mean, speed.est), method = 'lm', color = 'black', lwd = 1,
              na.rm = TRUE) +
  geom_point(aes(hfi.mean, speed.est, color = region.lab), alpha = 0.9)+
  scale_color_manual('Region', values = pal[2:3]) +
  labs(x = 'Human Footprint Index', y = expression('Home Range Area'~(km^2))) +
  theme(legend.position = 'top')

gam(speed.est ~ hfi.mean,
    family = Gamma('log'),
    data = filter(tapirs, region != 'atlantica'),
    method = 'REML') %>%
  summary()
