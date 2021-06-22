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

tapirs <- readRDS('models/tapirs-final.rds') # tapir data
sa <- filter(spData::world, continent == 'South America') # south america layer
hfi.raster <- raster('data/hfi-layers/ml_hfi_v1_2019.nc') # HFI

# extract mean hfi and hr areas
plot(tapirs$akde[[5]])
extract(hfi.raster, as.sf(tapirs$akde[[5]])) # [[1]]=lwr, [[2]]=est, [[3]]=upr

tapirs <-
  mutate(tapirs,
         hfi.mean = map_dbl(1:N,
                       function(i)
                         extract(hfi.raster,
                                 as.sf(akde[[i]]))[[2]] %>% # only take estimate
                         mean(na.rm = TRUE)),
         hr.size = map_dbl(akde,
                           function(a) summary(a)$CI[2]))
unique(warnings())

# Gamma GLM regression
m0 <- gam(hr.size ~ hfi.mean,
          family = Gamma('log'),
          data = tapirs,
          method = 'REML')
summary(m0)

pred0 <- tibble(hfi.mean = seq(0.003, 0.31, length.out = 400),
               est = predict(m0, newdata = tibble(hfi.mean), type = 'response'))

# no significant improvement with a gamma GAM
m1 <- gam(hr.size ~ s(hfi.mean),
          family = Gamma('log'),
          data = tapirs,
          method = 'REML')
summary(m1)

pred1 <- tibble(hfi.mean = seq(0.003, 0.31, length.out = 400),
               est = predict(m1, newdata = tibble(hfi.mean), type = 'response'))

ggplot() +
  geom_line(aes(hfi.mean, est), pred0, color = 'darkorange') +
  geom_line(aes(hfi.mean, est), pred1, color = 'forestgreen') +
  geom_point(aes(hfi.mean, hr.size), tapirs, alpha = 0.5) +
  labs(x = 'Human Footprint Intex', y = expression('Home Range Area'~(km^2)))
ggsave('figures/hr-hfi-regression.png', height = 3, width = 5, scale = 1.5)

AIC(m0, m1)
