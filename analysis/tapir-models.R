library('R.utils') # for withTimeout()
library('readxl') # for read_xlsx()
library('ctmm') # using the github version (0.6.1)
library('dplyr') # for data wrangling
library('purrr') # for vectorization
library('tidyr') # for data wrangling
library('ggplot2') # for plotting
theme_set(theme_bw())
N <- 74 # number of tapirs

# for remote parallel computation (linux)
NCORES <- 15
Sys.setenv(MC_CORES = NCORES) # set max number of cores to NCORES
Sys.getenv('MC_CORES')        # should return NCORES

# for local parallel computation (windows)
library('furrr') # for parallel computation
NCORES <- 4 # number of cores, not number of logical processors!
plan(multisession, workers = NCORES)

# import data and convert to `telemetry` format, then plot by position and time
# 0 + 912 + 193 = 1105 initial outliers (see files in tapirs/data)
# 0 + 0 + 0 = 0 additional outliers (see files in tapirs/data/cleaned)
atlantica <- read.csv('data/1_ATLANTICFOREST_11.csv') %>% # degraded
  as.telemetry(timeformat = '%Y-%m-%d %H:%M', mark.rm = TRUE)

pantanal <- read.csv('data/2_PANTANAL_ERRORDATASET_FINAL.csv') %>% # agriculture
  arrange(individual.local.identifier, timestamp) %>% # to avoid warning
  as.telemetry(timeformat = '%Y-%m-%d %H:%M:%S', mark.rm = TRUE)

cerrado <- read.csv('data/3_CERRADO_ERRORDATASET_FINAL.csv') %>% # reference
  as.telemetry(timeformat = '%Y-%m-%d %H:%M:%S', mark.rm = TRUE)

# add User Equivalent Range Error (UERE)
pantanal.calib <- read.csv('data/CALIBRATION_Pantanal.csv') %>% as.telemetry()
pantanal.uere <- uere.fit(pantanal.calib)
uere(pantanal) <- pantanal.uere

cerrado.calib <- read.csv('data/CALIBRATION_Cerrado.csv') %>% as.telemetry()
cerrado.uere <- uere.fit(pantanal.calib)
uere(cerrado) <- cerrado.uere

traits <-
  bind_rows(read_xlsx('data/RESULTS - SPATIAL ECOLOGY.xlsx',
                      sheet = 'ATLANTIC FOREST (11)',
                      range = 'A3:D13',
                      col_names = c('name', 'method', 'sex', 'age')) %>%
              mutate(region = 'atlantica'),
            read_xlsx('data/RESULTS - SPATIAL ECOLOGY.xlsx',
                      sheet = 'PANTANAL (46)',
                      range = 'A3:D48',
                      col_names = c('name', 'method', 'sex', 'age')) %>%
              mutate(region = 'pantanal'),
            read_xlsx('data/RESULTS - SPATIAL ECOLOGY.xlsx',
                      sheet = 'CERRADO (19)',
                      range = 'A3:D21',
                      col_names = c('name', 'method', 'sex', 'age')) %>%
              mutate(region = 'cerrado')) %>%
  mutate(name.short = map_chr(name,
                              function(x) substr(x,
                                                 gregexpr(' ', x)[[1]][1],
                                                 100)),
         name.short = gsub(' ', '', name.short),
         name.short = gsub('-', '', name.short))

# two individuals not in Cerrado dataset
filter(traits, name.short %in% c('SILVIO', 'SOFIA'))

# fit models ####
if(FALSE) {
  tapirs <-
    bind_rows(tibble(region = 'atlantica', name = names(atlantica)),
              tibble(region = 'pantanal', name = names(pantanal)),
              tibble(region = 'cerrado', name = names(cerrado))) %>%
    mutate(name.short = map_chr(name,
                                function(x) substr(x,
                                                   gregexpr('_', x)[[1]][2],
                                                   100)),
           name.short = gsub('_', '', name.short)) %>%
    left_join(select(traits, -region, -name), by = 'name.short') %>%
    # no map_***() available for telemetry, variogram, and ctmm objects
    mutate(data = map2(region, name,
                       function(x, y) get(x)[[y]]),
           calib = region != 'atlantica',
           svf = map(data, variogram),
           theta0 = map(1:N,
                        function(i) ctmm.guess(data = data[[i]],
                                               CTMM = ctmm(error = calib[i]),
                                               variogram = svf[[i]],
                                               interactive = FALSE)))
  
  # not unsing map() because a single error loses all progress
  # 23, 24 cause:
  # Error in if (any(error > 0)) { : missing value where TRUE/FALSE needed
  tictoc::tic() # ~ 7 minutes/model
  for(i in 1:N) {
    tryCatch({ # print errors, but continue running models
      tapirs$model[i] <-
        list(ctmm.select(data = tapirs$data[[i]], CTMM = tapirs$theta0[[i]],
                         control = list(method = 'pNewton', cores = NCORES)))
      cat(i, '\n')
    }, error = function(e){cat("ERROR:",conditionMessage(e), "\n")})
  }
  tictoc::toc()
  saveRDS(tapirs, file = 'models/tapirs-calibrated.rds')
  
  # Five warnings:
  # In ctmm.fit(data, GUESS, trace = trace2, ...) :
  # pREML failure: indefinite ML Hessian or divergent REML gradient.
  
} else {
  tapirs <- readRDS('models/tapirs.rds')
}

# plot of times
raw <-
  bind_rows(read.csv('data/1_ATLANTICFOREST_11.csv') %>%
              transmute(region = 'Mata Atlantica',
                        name = individual.local.identifier,
                        timestamp = timestamp),
            read.csv('data/2_PANTANAL_ERRORDATASET_FINAL.csv') %>%
              arrange(individual.local.identifier, timestamp) %>%
              transmute(region = 'Pantanal',
                        name = individual.local.identifier,
                        timestamp = timestamp),
            read.csv('data/3_CERRADO_ERRORDATASET_FINAL.csv') %>%
              transmute(region = 'Cerrado',
                        name = individual.local.identifier,
                        timestamp = timestamp)) %>%
  mutate(region = factor(region,
                         levels = c('Mata Atlantica', 'Pantanal', 'Cerrado')),
         name2 = lag(name),
         t = as.POSIXct(timestamp, format = '%Y-%m-%d %H:%M'),
         t2 = lag(t),
         diff.mins = if_else(name == name2, t - t2, NA_real_) %>% as.numeric(),
         diff.hours = diff.mins / 60,
         diff.hours.2d = if_else(diff.hours < 24 * 2, diff.hours, 24 * 2)) %>%
  as_tibble()

# times between measurements
ggplot(raw, aes(diff.hours.2d)) +
  facet_grid(region ~ .) +
  geom_density(bw = 0.5, fill = 'red', alpha = 0.5) +
  labs(x = 'Hours', y = 'Density',
       title = 'Time between measurements, to a max of 48 hours')
ggsave('figures/waiting-times-density.png', width = 5, height = 6)

save.plots <- function(i) {
  r <- tapirs$region[[i]]
  n <- tapirs$name[[i]]
  model <- tapirs$model[[i]]
  
  pdf(file = paste0('models/diagnostics/', r, '-', n, '.pdf'), width = 16,
      height = 9) # save to pdf
  layout(t(1:2)) # plot both diagnostic plots in one page
  diagn <- outlie(tapirs$data[[i]]) # data w diagn
  plot(diagn, units = FALSE) # speed vs distance diagnostic
  
  # plot best model at 3 different zooms
  # layout(1)
  # plot(fun, CTMM = model, col.CTMM = 'red', fraction = 5e-4)
  # plot(fun, CTMM = model, col.CTMM = 'red', fraction = 0.01)
  # plot(fun, CTMM = model, col.CTMM = 'red', fraction = 0.5)
  dev.off()
  print(i)
}

# open plots with SumatraPDF, use Ctrl+Shift+Right to move to next file
### for outlier analysis, make sure speeds are < 1 m/s = 3.6 km/h
### small clusters that deviate from the main cluster are ok
### remove measurement errors/outliers e.g. due to GPS error
future_map(1:N, save.plots, .options = furrr_options(seed = NULL))
dev.off() # make sure all pdf devices are closed

# estimate speed and home range
tapirs <- mutate(tapirs,
                 # missing speeds are returned as m/s while others as km/day
                 speed.est = future_map(best.model, function(x) list(speed(x)),
                                        .options = furrr_options(seed = NULL)))
unique(warnings()) # 33 half have fractal movement

# withTimeout() cannot be used with map()
tictoc::tic() # ~ 3 minutes without weights
for(i in 1:N) {
  tapirs$akde[[i]] <- akde(data = tapirs$data[[i]],
                           CTMM = tapirs$best.model[[i]])
  # withTimeout(akde(data = tapirs$data[[i]],
  #                  CTMM = tapirs$best.model[[i]],
  #                  # mata atlantica has VHF data => irregular lags
  #                  weights = tapirs$region[i] == 'atlantica'),
  #             substitute = TRUE,
  #             timeout = 60 * 60 * 4, # warning if t > 4 hours for a single est
  #             onTimeout = 'warning')
  if(i %% 5 == 0) cat(i, '\n')
}
tictoc::toc()

plot.estimates <- function(ROW) {
  AKDE <- tapirs[ROW, 'akde'][[1]]
  DATA <- tapirs[ROW, 'data'][[1]]
  
  pdf(paste0('figures/akde/', tapirs[ROW, 'region'], '-', tapirs[ROW, 'name'],
             '.pdf'), width = 12, height = 6.75)
  plot(AKDE) # 95% quantile of home range distribution with 95% CIs
  plot(DATA, add = TRUE) # add datapoints
  dev.off()
}

# extract estimates
tapirs <-
  bind_cols(tapirs,
            bind_rows(map(tapirs$best.model, # autocorrelation parameters
                          function(x) {
                            y <- c(tau = x$tau)
                            if(is.null(y)) y <- c(tau.position = NA,
                                                  tau.velocity = NA)
                            y
                          })),
            bind_rows(map(tapirs$speed.est, as.data.frame)), # speed estimates
            map(1:N, function(i) tapirs$akde[[i]]$DOF.area %>% # akde df
                  t() %>%
                  as.data.frame()) %>%
              bind_rows() %>%
              rename(x.df = x, y.df = y))

# AKDE plots with data, open plots with SumatraPDF
with(tapirs, future_map(1:N, plot.estimates,
                        .options = furrr_options(seed = NULL)))
dev.off() # make sure all pdf devices are closed

# summary plots ####
tapirs <-
  mutate(tapirs,
         name.short = map_chr(name,
                              function(x) substr(x,
                                                 gregexpr('_', x)[[1]][2],
                                                 100)),
         name.short = gsub('_', '', name.short)) %>%
  left_join(select(traits, -region, -name), by = 'name.short') %>%
  mutate(adult = if_else(age %in% c('ADULT', 'ADULT-OLD'), 'Yes', 'No'))

# speed estimates (only one speed estimate in mata atlantica)
s.box <-
  tapirs %>%
  select(region, name, low, est, high, age, sex, adult) %>%
  pivot_longer(low:high, names_to = 'parameter') %>%
  mutate(parameter = factor(parameter, levels = c('low', 'est', 'high')))%>%
  ggplot() +
  facet_grid(. ~ parameter) +
  scale_color_brewer(palette = 6, type = 'qual') +
  labs(x = NULL, y = 'Speed (km/day)') +
  theme(legend.position = 'top')
ggsave('figures/speed-boxplots.png',
       plot = s.box + geom_boxplot(aes(x = region, y = value)),
       width = 5, height = 4)
ggsave('figures/speed-boxplots-sex.png',
       plot = s.box + geom_boxplot(aes(x = region, y = value, color = sex)),
       width = 5, height = 4)
ggsave('figures/speed-boxplots-age.png',
       plot = s.box + geom_boxplot(aes(x = region, y = value, color = adult)),
       width = 5, height = 4)

# tau estimates (only one speed estimate in mata atlantica)
t.box <- tapirs %>%
  select(region, name, tau.position, tau.velocity, sex, adult) %>%
  pivot_longer(tau.position:tau.velocity, names_to = 'parameter') %>%
  mutate(parameter = case_when(parameter == 'tau.position' ~ 'Position',
                               parameter == 'tau.velocity' ~ 'Velocity')) %>%
  ggplot() +
  facet_wrap(. ~ parameter, scales = 'free_y') +
  scale_y_log10() +
  labs(x = NULL, y = expression(Autocoerrelation~parameter~(tau))) +
  scale_color_brewer(palette = 6, type = 'qual') +
  theme(legend.position = 'top')
ggsave('figures/tau-boxplots.png',
       plot = t.box + geom_boxplot(aes(x = region, y = value)),
       width = 5, height = 4)
ggsave('figures/tau-boxplots-sex.png',
       plot = t.box + geom_boxplot(aes(x = region, y = value, color = sex)),
       width = 5, height = 4)
ggsave('figures/tau-boxplots-age.png',
       plot = t.box + geom_boxplot(aes(x = region, y = value, color = adult)),
       width = 5, height = 4)

# home range degrees of freedom
df.box <- tapirs %>%
  select(region, name, x.df, y.df, sex, adult) %>%
  pivot_longer(c(x.df, y.df), names_to = 'parameter') %>%
  ggplot() +
  facet_wrap(. ~ parameter, scales = 'free_y') +
  labs(x = NULL, y = 'AKDE degrees of freedom') +
  scale_color_brewer(palette = 6, type = 'qual') +
  theme(legend.position = 'top')
ggsave('figures/adke-df-boxplots.png',
       plot = df.box + geom_boxplot(aes(x = region, y = value)),
       width = 5, height = 4)
ggsave('figures/adke-df-boxplots-sex.png',
       plot = df.box + geom_boxplot(aes(x = region, y = value, color = sex)),
       width = 5, height = 4)
ggsave('figures/adke-df-boxplots-age.png',
       plot = df.box + geom_boxplot(aes(x = region, y = value, color = adult)),
       width = 5, height = 4)

# home range comparisons and figures
?ctmm::meta()

metafor::#package