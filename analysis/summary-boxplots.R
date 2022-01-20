library('ctmm')    # using the github version (0.6.1)
library('mgcv')    # for calculating summary statistics
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('ggplot2') # for plotting
library('cowplot') # for grid of plots
theme_set(theme_bw() +
            theme(text = element_text(size = 14),
                  panel.grid = element_blank()))

# color palette
pal <- c('#4477AA', '#ff8c00', '#66CCEE', '#009900',
         '#CCBB44', '#EE6677', '#AA3377', '#BBBBBB')

# import data
tapirs <- readRDS('models/tapirs-final.rds') %>%
  mutate(sex = if_else(sex == 'FEMALE', 'Female', 'Male'),
         adult = if_else(adult == 'Yes', 'Adult', 'Young'),
         name = factor(name,
                       levels = c(unique(name), 'Atlantic forest',
                                  'Pantanal', 'Cerrado', 'Overall')),
         region.lab = if_else(region.lab == 'Mata Atlantica', 'Atlantic forest',
                              region.lab) %>%
           factor(levels = c('Atlantic forest', 'Pantanal', 'Cerrado'))) %>%
  # select(region.lab, name, speed.est, area.est, age, sex, adult) %>%
  mutate(sex_r = interaction(sex, region.lab, lex.order = TRUE),
         adult_r = interaction(adult, region.lab, lex.order = TRUE))

# means and CIs
m.speed.sex <- gam(speed.est ~ sex + region.lab,
                   family = Gamma('log'),
                   data = tapirs,
                   method = 'REML')
m.speed.age <- gam(speed.est ~ adult + region.lab,
                   family = Gamma('log'),
                   data = tapirs,
                   method = 'REML')

# predictions ---
## speed, sex
summ_ss <-
  expand_grid(sex = unique(tapirs$sex),
              region.lab = unique(tapirs$region.lab)) %>%
  tibble(bind_cols(predict(m.speed.sex, se.fit = TRUE,
                           tibble(sex = sex, region.lab = region.lab)))) %>%
  mutate(sex_r = interaction(sex, region.lab, lex.order = TRUE),
         est = round(exp(fit), 2),
         lwr = round(exp(fit - 1.96 * se.fit), 2),
         upr = round(exp(fit + 1.96 * se.fit), 2))
## speed, adult
summ_sa <-
  expand_grid(adult = unique(tapirs$adult),
              region.lab = unique(tapirs$region.lab)) %>%
  tibble(bind_cols(predict(m.speed.age, se.fit = TRUE,
                           tibble(adult = adult, region.lab = region.lab)))) %>%
  mutate(adult_r = interaction(adult, region.lab, lex.order = TRUE),
         est = round(exp(fit), 2),
         lwr = round(exp(fit - 1.96 * se.fit), 2),
         upr = round(exp(fit + 1.96 * se.fit), 2))

## home range, sex
hr_estimates <- function(REGION, SEX = NA, ADULT = NA) {
  if(! is.na(SEX)) {
    d <- tapirs %>% filter(sex == SEX) # filter to a single sex  
  } else {
    d <- tapirs %>% filter(adult == ADULT) # filter to a single sex
  }
  
  d <-
    d %>%
    filter(region.lab == REGION) %>% # filter to a single region
    pull(akde) %>%
    meta(plot = FALSE) %>%
    as_tibble() %>%
    slice(1) %>%
    rename(lwr = low,
           upr = high) %>%
    mutate(region.lab = REGION, sex = SEX, adult = ADULT)
  
  if(! is.na(SEX)) {
    d <- d %>% mutate(sex_r = paste(sex, region.lab, sep = '.'))
  } else {
    d <- d %>% mutate(adult_r = paste(adult, region.lab, sep = '.'))
  }
}

## home range, sex
summ_as <-
  expand_grid(sex = unique(tapirs$sex),
              region.lab = unique(tapirs$region.lab)) %>%
  mutate(bind_cols(purrr::map2_dfr(region.lab, sex,
                                   \(x, y) hr_estimates(REGION = x, SEX = y))))

## home range, adult
summ_aa <-
  expand_grid(adult = unique(tapirs$adult),
              region.lab = unique(tapirs$region.lab)) %>%
  mutate(bind_cols(purrr::map2_dfr(region.lab, adult,
                                   \(x, y) hr_estimates(REGION = x, ADULT = y))))

# function for summary plots ----
summary_plot <- function(y = 'speed', group = c('sex', 'adult')) {
  
  x <- paste0(group, '_r')
  summarized <- get(paste0('summ_', substr(y, 1, 1), substr(x, 1, 1)))
  
  if(length(group) > 1) stop('Select only one group.')
  
  colnames(tapirs)[grepl(x, colnames(tapirs))] <- 'x'
  colnames(tapirs)[grepl(y, colnames(tapirs))] <- 'y'
  colnames(summarized)[grepl(x, colnames(summarized))] <- 'x'
  
  rm(y)
  
  p <- 
    ggplot(tapirs) +
    
    # data
    geom_jitter(aes(x, y, color = region.lab), shape = 4, width = 0.2, size=2) +
    # CIs
    geom_errorbar(aes(x, ymin = lwr, ymax = upr, color = region.lab),
                  summarized, lwd = 3, width = 0) +
    # means
    geom_point(aes(x, est), summarized, size = 2) +
    geom_point(aes(x, est), summarized, color = 'white') +
    
    # theme
    scale_fill_manual('Region', values = pal, aesthetics = c('fill', 'color'),
                      labels = c('Atlantic forest', 'Pantanal', 'Cerrado')) +
    scale_y_continuous('Speed (km/day)') +
    theme(legend.position = 'none')
  
  # add appropriate labels depending on grouping
  if(group == 'sex') {
    p + scale_x_discrete(NULL, breaks = c('Female.Pantanal', 'Male.Pantanal'),
                         labels = c('Female', 'Male'))
  } else {
    p + scale_x_discrete(NULL, breaks = c('Adult.Pantanal', 'Young.Pantanal'),
                         labels = c('Adult', 'Young'))
  }
}

# summary plots ----
spe.s <- summary_plot(group = 'sex', y = 'speed.est'); spe.s
spe.a <- summary_plot(group = 'adult', y = 'speed.est'); spe.a
hr.s <- summary_plot(group = 'sex', y = 'area.est'); hr.s
hr.a <- summary_plot(group = 'adult', y = 'area.est'); hr.a

plot_grid(get_legend(spe.s + theme(legend.position = 'top')),
          plot_grid(spe.s, spe.a, hr.s, hr.a, ncol = 2, label_y = 1.08,
                    labels = c('a)', 'b)', 'c)', 'd)')),
          ncol = 1, rel_heights = c(0.1, 1))

ggsave('figures/stripcharts.png', width = 6.86, height = 4.5, dpi = 300,
       bg = 'white', scale = 1.3)
