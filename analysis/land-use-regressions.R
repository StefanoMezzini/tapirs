library('tidyr')      # for data wrangling (want to mask tidyr::extract())
library('sf')         # to work with spData maps
library('sp')         # to import spatial layers
library('ctmm')       # using the github version (0.6.1)
library('raster')     # to import human footprint index raster
library('dplyr')      # for data wrangling
library('purrr')      # for functional mapping
library('ggplot2')    # for plotting
library('cowplot')    # for plot grids
library('mgcv')       # for fitting GAMs
theme_set(theme_bw())
N <- 74 # number of tapirs

# color palette
pal <- c('#4477AA', '#ff8c00', '#66CCEE', '#009900',
         '#CCBB44', '#EE6677', '#AA3377', '#BBBBBB')

tapirs <- readRDS('models/tapirs-final.rds') # tapir data
sa <- filter(spData::world, continent == 'South America') # south america layer

# land use regressions ####
# land use rasters
lu.atl <- raster('data/spatial-layers/1- ATLANTIC FOREST/MA_Habitat.tif')
lu.pan <-raster('data/spatial-layers/2- PANTANAL/PANTANAL_USO_SOLO_03_2017.tif')
lu.cer <- raster('data/spatial-layers/3- CERRADO/CERRADO_USO_SOLO_09_2016.tif')

# extract land use raster and land use types
tapirs <-
  mutate(tapirs,
         region = factor(region), # need factors for GAMs
         region.lab = factor(region.lab,
                             levels = c('Mata Atlantica','Pantanal','Cerrado')),
         lu.raster =
           map(1:N,
               function(i) {
                 r <- tapirs$region[i]
                 
                 rast <-
                   case_when(r == 'atlantica' ~ 'lu.atl',
                             r == 'pantanal' ~ 'lu.pan',
                             r == 'cerrado' ~ 'lu.cer') %>%
                   get() %>%
                   extract(as.sf(tapirs$akde[[i]]))
                 
                 cat(i, '\n', sep = '')
                 # count unique values in estimate layer
                 tot <-
                   table(rast[[2]]) %>%
                   as.data.frame()
                 
                 if(r == 'atlantica') {
                   tot <-
                     mutate(tot,
                            type = case_when(Var1 == 2 ~ '?',
                                             TRUE ~ 'forest'))
                 } else if(r == 'cerrado'){
                   tot <- mutate(tot,
                                 type =
                                   case_when(Var1 == 1 ~ 'urban',
                                             Var1 == 2 ~ 'savannah',
                                             Var1 == 3 ~ 'floodplain',
                                             Var1 == 4 ~ 'savannah',
                                             Var1 == 5 ~ 'savannah',
                                             Var1 == 6 ~ 'crop',
                                             Var1 == 7 ~ 'crop',
                                             Var1 == 8 ~ 'dirt',
                                             Var1 == 9 ~ 'plantation',
                                             Var1 == 10 ~ 'forest',
                                             Var1 == 11 ~ 'forest',
                                             Var1 == 12 ~ 'water',
                                             Var1 == 13 ~ 'crop',
                                             Var1 == 14 ~ 'plantation'))
                 } else {
                   tot <-
                     mutate(tot,
                            type = case_when(Var1 == 1 ~ 'pasture',
                                             Var1 == 2 ~ 'floodplain',
                                             Var1 == 3 ~ 'forest',
                                             Var1 == 4 ~ 'floodplain',
                                             Var1 == 5 ~ 'forest',
                                             Var1 == 6 ~ 'pasture',
                                             Var1 == 7 ~ 'headquarters'))
                 }
                 
                 tot <-
                   tot %>%
                   group_by(type) %>%
                   summarize(value = sum(Freq)) %>%
                   pivot_wider(names_from = type, values_from = value)
               }))
unique(warnings())

tapirs <-
  full_join(tapirs,
            with(tapirs,
                 bind_cols(name = name,
                           mutate(bind_rows(lu.raster),
                                  total = rowSums(bind_rows(lu.raster),
                                                  na.rm = TRUE)))) %>%
              pivot_longer(-c('total', 'name'), names_to = 'type') %>%
              mutate(value = value / total) %>%
              pivot_wider(names_from = 'type', values_from = 'value'),
            by = 'name') %>%
  pivot_longer(`?`:plantation, names_to = 'lu', values_to = 'value') %>%
  mutate(value = if_else(is.na(value), 0, value)) %>%
  pivot_wider(names_from = lu, values_from = value)

# land use on hr size ####
# groups with less than 5 unique values
select(tapirs, `?`:plantation) %>%
  pivot_longer(-c()) %>%
  filter(value > 0) %>%
  group_by(name) %>%
  summarize(unique = length(unique(value)),
            min = min(value),
            max = max(value)) %>%
  filter(unique > 5, unique < 10)

# modelling ####
# Gamma GAM regression on home range estimate
m.hr <- gam(area.est ~ s(floodplain, k = 10) + s(dirt, k = 5),
            # s(pasture, k = 10) +
            # s(forest, k = 10) +
            # s(crop, k = 5) +
            # s(savannah, k = 10) +
            # s(water, k = 5),
            family = Gamma('log'),
            data = tapirs,
            method = 'REML')
summary(m.hr)

# Gamma GAM regression on home range estimate
m.speed <- gam(speed.est ~ s(floodplain, k = 5) + s(dirt, k = 5),
      # s(pasture, k = 5) +
      # s(savannah, k = 5) +
      # s(forest, k = 5) +
      # s(headquarters, k = 5) +
      # s(crop, k = 5) +
      # s(water, k = 5),
      family = Gamma('log'),
      data = tapirs,
      method = 'REML')
summary(m.speed)

# initial plots
gratia::draw(m.hr, residuals = TRUE)
gratia::draw(m.speed, residuals = TRUE)

# predictions ####
pred0 <- tibble(field.pasture = seq(0, 0.525, length.out = 100))
pred1 <- tibble(floodable = seq(0, 0.71, length.out = 100))
pred2 <- tibble(unclean = seq(0, 0.41, length.out = 100))
pred0 <- bind_cols(pred0,
                   predict(m0, newdata = pred0, se.fit = TRUE)) %>%
  mutate(est = exp(fit),
         lwr = exp(fit - 1.96 * se.fit),
         upr = exp(fit + 1.96 * se.fit))

# regression plot
hfi.hr <-
  ggplot() +
  geom_ribbon(aes(hfi.mean, ymin = lwr, ymax = upr), pred0, alpha = 0.2) +
  geom_line(aes(hfi.mean, est), pred0) +
  geom_point(aes(hfi.mean, hr.size, color = region.lab), tapirs, alpha = 0.9) +
  scale_color_manual('Region', values = pal[1:3]) +
  labs(x = 'ml-HFI', y = expression('Home Range Area'~(km^2))) +
  theme(legend.position = 'top')

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

pred3 <-
  bind_cols(select(pred0, hfi.mean),
            predict(m3, newdata = pred0, se.fit = TRUE)) %>%
  mutate(est = exp(fit),
         lwr = exp(fit - 1.96 * se.fit),
         upr = exp(fit + 1.96 * se.fit))

# regression plot
hfi.speed <-
  ggplot(tapirs) +
  geom_ribbon(aes(hfi.mean, ymin = lwr, ymax = upr), pred0, alpha = 0.2) +
  geom_line(aes(hfi.mean, est), pred0) +
  geom_point(aes(hfi.mean, speed.est, color = region.lab), alpha = 0.9)+
  scale_color_manual('Region', values = pal[1:3]) +
  labs(x = 'ml-HFI', y = 'Average speed (km/day)') +
  theme(legend.position = 'top')

# without Mata Atlantica
filter(tapirs, region != 'atlantica') %>%
  ggplot() +
  geom_smooth(aes(hfi.mean, speed.est), method = 'lm', color = 'black', lwd = 1,
              na.rm = TRUE) +
  geom_point(aes(hfi.mean, speed.est, color = region.lab), alpha = 0.9)+
  scale_color_manual('Region', values = pal[2:3]) +
  labs(x = 'ml-HFI', y = expression('Home Range Area'~(km^2))) +
  theme(legend.position = 'top')

gam(speed.est ~ hfi.mean,
    family = Gamma('log'),
    data = filter(tapirs, region != 'atlantica'),
    method = 'REML') %>%
  summary()

# final regression plot
plot_grid(get_legend(hfi.hr),
          plot_grid(hfi.hr + theme(legend.position = 'none'),
                    hfi.speed + theme(legend.position = 'none'),
                    labels = c('a.', 'b.')),
          ncol = 1, rel_heights = c(0.05, 1))
ggsave('figures/hfi-regressions.png', height = 3, width = 6.86, scale = 1.3,
       bg = 'white')
