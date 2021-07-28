library('sf')         # to work with spData maps
library('sp')         # to import spatial layers
library('ctmm')       # using the github version (0.6.1)
library('raster')     # to import human footprint index raster
library('MODISTools') # for NDVI raster
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

# hfi regressions ####
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

# ndvi regressions (not done) ####
mt_products() %>% filter(grepl('NDVI', description))
mt_bands('MOD13Q1') %>% filter(grepl('NDVI', band))

raw.atl <- read.csv('data/cleaned/atlantica.csv')
raw.pan <- read.csv('data/cleaned/pantanal.csv')
raw.cer <- read.csv('data/cleaned/cerrado.csv')

# download data and write it to a csv file
if(FALSE) {
  mt_subset(product = 'MOD13Q1',
            lon = median(raw.atl$location.long),
            lat = median(raw.atl$location.lat),
            band = '250m_16_days_NDVI',
            start = min(raw.atl$timestamp) %>% as.Date(),
            end = max(raw.atl$timestamp) %>% as.Date(),
            km_lr = 15, # km left-right
            km_ab = 15, # km above-below
            site_name = 'mata-atlantica', # arbitrary, for writing temp file
            internal = FALSE,
            progress = TRUE,
            out_dir = '~/GitHub/tapirs/data/ndvi-layers')
  mt_subset(product = 'MOD13Q1',
            lon = median(raw.pan$Longitude),
            lat = median(raw.pan$Latitude),
            band = '250m_16_days_NDVI',
            start = min(raw.pan$timestamp, na.rm = TRUE) %>% as.Date(),
            end = max(raw.pan$timestamp, na.rm = TRUE) %>% as.Date(),
            km_lr = 7.5, # km left-right
            km_ab = 10, # km above-below
            site_name = 'ndvi-pan', # arbitrary, for writing temp file
            internal = FALSE,
            progress = TRUE,
            out_dir = '~/GitHub/tapirs/data/ndvi-layers')
  mt_subset(product = 'MOD13Q1',
            lon = median(raw.cer$Longitude),
            lat = median(raw.cer$Latitude),
            band = '250m_16_days_NDVI',
            start = min(raw.cer$timestamp, na.rm = TRUE) %>% as.Date(),
            end = max(raw.cer$timestamp, na.rm = TRUE) %>% as.Date(),
            km_lr = 35, # km left-right
            km_ab = 17.5, # km above-below
            site_name = 'ndvi-cer', # arbitrary, for writing temp file
            internal = FALSE,
            progress = TRUE,
            out_dir = '~/GitHub/tapirs/data/ndvi-layers')
}

# import ndvi data
ndvi.atl <- read.csv('data/ndvi-layers/mata-atlantica_MOD13Q1_250m_16_days_NDVI_1997-07-102006-12-05.csv')

ndvi.atl.grouped <-
  ndvi.atl %>%
  mutate(year = lubridate::year(calendar_date),
         doy = lubridate::yday(calendar_date))

m.atl <- bam(value ~ s(year, k = 7, bs = 'cr') + s(doy, k = 20, bs = 'cr'),
             data = ndvi.atl.grouped,
             discrete = TRUE,
             method = 'fREML',
             control = gam.control(4))
summary(m.atl)
plot(m.atl, pages = 1, scale = 0)
gratia::draw(m.atl)
summary(m.atl)
unique(ndvi.atl$calendar_date) %>%
  group_by(calendar_date) %>%
  summarize()
