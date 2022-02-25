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
library('gratia')     # for GAM plots
theme_set(theme_bw() +
            theme(text = element_text(size = 14),
                  panel.grid = element_blank()))

N <- 74 # number of tapirs

# color palette
pal <- c('#4477AA', '#ff8c00', '#66CCEE', '#009900',
         '#CCBB44', '#EE6677', '#AA3377', '#BBBBBB')

# extract land use raster and land use types
tapirs <- readRDS('models/tapirs-land-use.rds') %>% # tapir data
  mutate(tau.velocity.est = tau.velocity.est / 60 / 60, # from seconds to hours
         tau.velocity.low = tau.velocity.low / 60 / 60,
         tau.velocity.high = tau.velocity.high / 60 / 60)

if(FALSE) { # re-crop rasters
  # land use rasters
  lu.atl <- raster('data/spatial-layers/1- ATLANTIC FOREST/MA_Habitat.tif')
  lu.pan <- raster('data/spatial-layers/2- PANTANAL/PANTANAL_USO_SOLO_03_2017.tif')
  lu.cer <- raster('data/spatial-layers/3- CERRADO/CERRADO_USO_SOLO_09_2016.tif')
  
  # tapir data without clipped rasters
  tapirs <- readRDS('models/tapirs-final.rds')
  
  tapirs <-
    mutate(tapirs,
           region = factor(region), # need factors for GAMs
           region.lab = if_else(region.lab == 'Mata Atlantica', 'Atlantic forest',
                                region.lab) %>% factor(),
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
  
  #saveRDS(tapirs, 'models/tapirs-land-use.rds')
}

# land use on hr size ####
# groups with less than 5 unique values
select(tapirs, `?`:plantation) %>%
  pivot_longer(-c()) %>%
  group_by(name) %>%
  summarize(unique = length(unique(value)),
            min = min(value),
            max = max(value)) %>%
  arrange(desc(unique)) %>%
  mutate(`>5` = unique > 5,
         `>10` = unique > 10)

# modelling ####
# Gamma GAM regression on home range estimate
m.hr <- gam(area.est ~ s(dirt, k = 5),
            # s(floodplain, k = 10) +
            # s(pasture, k = 10) +
            # s(forest, k = 10) +
            # s(crop, k = 5) +
            # s(savannah, k = 10) +
            # s(water, k = 5),
            family = Gamma('log'),
            data = tapirs,
            method = 'REML')
summary(m.hr)

# Gamma GAM regression on average speed estimate
m.speed <- gam(speed.est ~
                 s(forest, k = 10) +
                 s(floodplain, k = 10) +
                 s(savannah, k = 10) +
                 s(dirt, k = 10) +
                 s(pasture, k = 5) +
                 s(crop, k = 5) +
                 s(water, k = 5),
               family = Gamma('log'),
               data = tapirs,
               method = 'REML')
m.speed0 <- gam(speed.est ~ forest,
                family = Gamma('log'),
                data = tapirs,
                method = 'REML')
AIC(m.speed, m.speed0) # use speed0
rm(m.speed)

# Gamma GAM regression on average tau_v estimate
filter(tapirs, !is.na(tau.velocity.est)) %>%
  select(`?`:plantation) %>%
  pivot_longer(-c()) %>%
  group_by(name) %>%
  summarize(unique = length(unique(value)),
            min = min(value),
            max = max(value)) %>%
  arrange(desc(unique)) %>%
  mutate(`>5` = unique > 5,
         `>10` = unique > 10) %>%
  filter(`>5`)

m.tauv <- gam(tau.velocity.est ~
                s(forest, k = 10) +
                s(floodplain, k = 10) +
                s(savannah, k = 10) +
                s(dirt, k = 5) +
                s(pasture, k = 5) +
                s(crop, k = 5) +
                s(water, k = 5),
              family = Gamma('log'),
              data = tapirs,
              method = 'REML')
summary(m.tauv)
AIC(m.tauv)
m.tauv <- gam(tau.velocity.est ~
                s(forest, k = 10) +
                # s(floodplain, k = 10) +
                # s(savannah, k = 10) +
                # s(dirt, k = 5) +
                # s(pasture, k = 5) +
                # s(crop, k = 5) +
                s(water, k = 5),
              family = Gamma('log'),
              data = tapirs,
              method = 'REML')
AIC(m.tauv) # use smaller model
summary(m.tauv)

# Gamma GAM regression on average tau_v estimate without outlier
m.tauv.1 <- gam(tau.velocity.est ~
                  s(forest, k = 10) +
                  s(floodplain, k = 10) +
                  s(savannah, k = 10) +
                  s(dirt, k = 5) +
                  s(pasture, k = 5) +
                  s(crop, k = 5) +
                  s(water, k = 5),
                family = Gamma('log'),
                data = filter(tapirs, tau.velocity.est < 1.5),
                method = 'REML')
summary(m.tauv.1)
AIC(m.tauv.1) # 647.7876
m.tauv.1 <- gam(tau.velocity.est ~
                  s(forest, k = 10) +
                  # s(floodplain, k = 10) +
                  # s(savannah, k = 10) +
                  # s(dirt, k = 5) +
                  # s(pasture, k = 5) +
                  # s(crop, k = 5) +
                  s(water, k = 5),
                family = Gamma('log'),
                data = filter(tapirs, tau.velocity.est < 1.5),
                method = 'REML')
AIC(m.tauv.1)
summary(m.tauv.1)

# initial plots
draw(m.hr, residuals = TRUE)
plot_grid(draw(m.tauv, residuals = TRUE), draw(m.tauv.1, residuals = TRUE),
          ncol = 1)

# predictions ####
# lu on hr
pred <- tibble(dirt = seq(0, 0.22, length.out = 100))
pred <- bind_cols(pred,
                  predict(m.hr, newdata = pred, se.fit = TRUE)) %>%
  mutate(est = exp(fit),
         lwr = exp(fit - 1.96 * se.fit),
         upr = exp(fit + 1.96 * se.fit))

# regression plot
p <-
  ggplot() +
  geom_ribbon(aes(dirt, ymin = lwr, ymax = upr), pred, alpha = 0.2) +
  geom_line(aes(dirt, est), pred) +
  geom_point(aes(dirt, area.est, color = region.lab), tapirs, alpha = 0.9) +
  scale_color_manual('Region', values = pal[1:3]) +
  labs(x = 'Proportion of exposed soil in the habitat',
       y = expression('Home Range Area'~(km^2))) +
  theme(legend.position = 'top',
        panel.grid = element_blank()); p

# ggsave('figures/lu-regression-hr.png', height = 1.5, width = 3.23, scale = 2)

# lu on tau_v
pred1 <- tibble(forest = seq_min_max(tapirs$forest, n = 100),
                water = mean(tapirs$water, na.rm = TRUE))
pred1 <-
  bind_cols(pred1, predict(m.tauv.1, newdata = pred1, se.fit = TRUE,
                           terms = 's(forest)')) %>%
  mutate(est = exp(fit),
         lwr = exp(fit - 1.96 * se.fit),
         upr = exp(fit + 1.96 * se.fit))

pred2 <- tibble(forest = mean(tapirs$forest, na.rm = TRUE),
                water = seq_min_max(tapirs$water, n = 100))
pred2 <-
  bind_cols(pred2, predict(m.tauv.1, newdata = pred2, se.fit = TRUE,
                           terms = 's(water)')) %>%
  mutate(est = exp(fit),
         lwr = exp(fit - 1.96 * se.fit),
         upr = exp(fit + 1.96 * se.fit))

# regression plot
p.forest <-
  ggplot() +
  geom_ribbon(aes(forest, ymin = lwr, ymax = upr), pred1, alpha = 0.2) +
  geom_line(aes(forest, est), pred1) +
  geom_segment(aes(x = forest, xend = forest, y = tau.velocity.low,
                   yend = tau.velocity.high, color = region.lab), tapirs,
               lwd = 0.5, alpha = 0.5) +
  geom_point(aes(forest, tau.velocity.est, color = region.lab,
                 shape = tau.velocity.est > 1.5),
             filter(tapirs, ! is.na(tau.velocity.est)), alpha = 0.9) +
  scale_shape_manual('Outlier', values = c(19, 4), labels = c('No', 'Yes')) +
  scale_color_manual('Region', values = pal[1:3]) +
  labs(x = 'Proportion of forested habitat',
       y = 'Directional persistence (hrs)') +
  theme(legend.position = 'none')
p.water <-
  ggplot() +
  geom_ribbon(aes(water, ymin = lwr, ymax = upr), pred2, alpha = 0.2) +
  geom_line(aes(water, est), pred2) +
  geom_segment(aes(x = water, xend = water, y = tau.velocity.low,
                   yend = tau.velocity.high, color = region.lab), tapirs,
               lwd = 0.5, alpha = 0.5) +
  geom_point(aes(water, tau.velocity.est, color = region.lab,
                 shape = tau.velocity.est > 1.5),
             filter(tapirs, ! is.na(tau.velocity.est)), alpha = 0.9) +
  scale_shape_manual('Outlier', values = c(19, 4), labels = c('No', 'Yes'))+
  scale_color_manual('Region', values = pal[1:3]) +
  labs(x = 'Proportion of water in the habitat',
       y = 'Directional persistence (hrs)') +
  theme(legend.position = 'none')
p <- plot_grid(get_legend(p.forest + theme(legend.position = 'top')),
               p.forest, p.water,
               ncol = 1, rel_heights = c(0.2, 1, 1),
               labels = c(NA, 'a)', 'b)')); p

ggsave('figures/lu-regression-tau-v_OUTLIERS.png', height = 4.125, width = 3.23,
       scale = 2, bg = 'white')

################################################
# Regression plot without outliers
################################################
p.dirt <-
  ggplot() +
  geom_ribbon(aes(dirt, ymin = lwr, ymax = upr), pred, alpha = 0.2) +
  geom_line(aes(dirt, est), pred) +
  geom_segment(aes(x = dirt, xend = dirt, y = area.low, yend = area.high,
                   color = region.lab), tapirs, lwd = 0.5, alpha = 0.5) +
  geom_point(aes(dirt, area.est, color = region.lab), tapirs, alpha = 0.9) +
  scale_color_manual('Region', values = pal[1:3]) +
  labs(x = 'Proportion of exposed soil in the habitat',
       y = expression('Home Range Area'~(km^2))) +
  theme(legend.position = 'none',
        panel.grid = element_blank())

p.forest <-
  ggplot() +
  geom_ribbon(aes(forest, ymin = lwr, ymax = upr), pred1, alpha = 0.2) +
  geom_line(aes(forest, est), pred1) +
  geom_segment(aes(x = forest, xend = forest, y = tau.velocity.low,
                   yend = tau.velocity.high, color = region.lab),
               filter(tapirs, tau.velocity.est < 1.5), lwd = 0.5,
               alpha = 0.5) +
  geom_point(aes(forest, tau.velocity.est, color = region.lab),
             filter(tapirs, tau.velocity.est < 1.5), alpha = 0.9) +
  scale_color_manual('Region', values = pal[1:3]) +
  labs(x = 'Proportion of forested habitat',
       y = 'Directional persistence (hrs)') +
  theme(legend.position = 'none')

p.water <-
  ggplot() +
  geom_ribbon(aes(water, ymin = lwr, ymax = upr), pred2, alpha = 0.2) +
  geom_line(aes(water, est), pred2) +
  geom_segment(aes(x = water, xend = water, y = tau.velocity.low,
                   yend = tau.velocity.high, color = region.lab),
               filter(tapirs, tau.velocity.est < 1.5),
               lwd = 0.5, alpha = 0.5) +
  geom_point(aes(water, tau.velocity.est, color = region.lab),
             filter(tapirs, tau.velocity.est < 1.5), alpha = 0.9) +
  scale_color_manual('Region', values = pal[1:3]) +
  labs(x = 'Proportion of water in the habitat',
       y = 'Directional persistence (hrs)') +
  theme(legend.position = 'none')

p <-
  plot_grid(
    get_legend(p.dirt + theme(legend.position = 'top')),
    p.dirt, p.forest, p.water,
    ncol = 1, rel_heights = c(0.2, 1, 1, 1),
    labels = c(NA, 'a)', 'b)', 'c)')); p

ggsave('figures/lu-regression-tau-v_NEW.png', height = 6, width = 3.23,
       scale = 2, bg = 'white')
