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
                              region.lab),
         sex_r = paste(sex, region.lab, sep = '_') %>%
           factor(levels = c('Female_Atlantic forest', 'Female_Pantanal',
                             'Female_Cerrado', 'Male_Atlantic forest',
                             'Male_Pantanal', 'Male_Cerrado')),
         adult_r = paste(adult, region.lab, sep = '_') %>%
           factor(levels = c('Adult_Atlantic forest', 'Adult_Pantanal',
                             'Adult_Cerrado', 'Young_Atlantic forest',
                             'Young_Pantanal', 'Young_Cerrado')))

# means and CIs
m_speed_sex <- gam(speed.est ~ sex + region.lab,
                   family = Gamma('log'),
                   data = tapirs,
                   method = 'REML')
m_speed_age <- gam(speed.est ~ adult + region.lab,
                   family = Gamma('log'),
                   data = tapirs,
                   method = 'REML')

# predictions ---
## speed, sex
summ_ss <-
  expand_grid(sex = unique(tapirs$sex),
              region.lab = unique(tapirs$region.lab)) %>%
  tibble(bind_cols(predict(m_speed_sex, se.fit = TRUE,
                           tibble(sex = sex, region.lab = region.lab)))) %>%
  mutate(sex_r = paste(sex, region.lab, sep = '_') %>%
           factor(levels = levels(tapirs$sex_r)),
         est = round(exp(fit), 2),
         lwr = round(exp(fit - 1.96 * se.fit), 2),
         upr = round(exp(fit + 1.96 * se.fit), 2))
## speed, adult
summ_sa <-
  expand_grid(adult = unique(tapirs$adult),
              region.lab = unique(tapirs$region.lab)) %>%
  tibble(bind_cols(predict(m_speed_age, se.fit = TRUE,
                           tibble(adult = adult, region.lab = region.lab)))) %>%
  mutate(adult_r = paste(adult, region.lab, sep = '_') %>%
           factor(levels = levels(tapirs$adult_r)),
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
    d <- d %>% mutate(sex_r = paste(sex, region.lab, sep = '_') %>%
                        factor(levels = levels(tapirs$sex_r)))
  } else {
    d <- d %>% mutate(adult_r = paste(adult, region.lab, sep = '_') %>%
                        factor(levels = levels(tapirs$adult_r)))
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
summary_plot <- function(Y, group = c('sex', 'adult')) {
  
  x <- paste0(group, '_r')
  summarized <- get(paste0('summ_', substr(Y, 1, 1), substr(x, 1, 1)))
  
  if(length(group) > 1) stop('Select only one group.')
  
  colnames(tapirs)[grepl(x, colnames(tapirs))] <- 'x'
  colnames(tapirs)[grepl(Y, colnames(tapirs))] <- 'y'
  colnames(summarized)[grepl(x, colnames(summarized))] <- 'x'
  
  p <- 
    ggplot(tapirs) +
    # CIs
    geom_errorbar(aes(x, ymin = lwr, ymax = upr, color = region.lab),
                  summarized, lwd = 3, width = 0, alpha = 0.5) +
    # data
    geom_jitter(aes(x, y, color = region.lab), shape = 4, width = 0.2, size = 2,
                na.rm = TRUE) +
    # means
    geom_point(aes(x, est), summarized, size = 2) +
    geom_point(aes(x, est), summarized, color = 'white') +
    
    # theme
    scale_fill_manual('Region', values = pal, aesthetics = c('fill', 'color'),
                      breaks = c('Atlantic forest', 'Pantanal', 'Cerrado')) +
    theme(legend.position = 'none')
  
  # add appropriate labels depending on grouping
  if(group == 'sex') {
    p <- p +
      scale_x_discrete(NULL, breaks = c('Female_Pantanal', 'Male_Pantanal'),
                       labels = c('Female', 'Male'))
  } else {
    p <- p +
      scale_x_discrete(NULL, breaks = c('Adult_Pantanal', 'Young_Pantanal'),
                       labels = c('Adult', 'Young'))
  }
  
  # add appropriate y label
  if(Y == 'speed.est') {
    p + scale_y_continuous('Speed (km/day)')
  } else {
    p + scale_y_continuous(expression(Home~range~(km^2)))
  }
}

# summary plots ----
spe_s <- summary_plot(group = 'sex', Y = 'speed.est')
spe_a <- summary_plot(group = 'adult', Y = 'speed.est')
hr_s <- summary_plot(group = 'sex', Y = 'area.est')
hr_a <- summary_plot(group = 'adult', Y = 'area.est')

plot_grid(get_legend(spe_s +
                       theme(legend.position = 'top')),
          plot_grid(spe_s, spe_a, hr_s, hr_a, ncol = 2, label_y = 1.08,
                    labels = c('a)', 'b)', 'c)', 'd)')),
          ncol = 1, rel_heights = c(0.1, 1))

ggsave('figures/stripcharts.png', width = 6.86, height = 4.5, dpi = 300,
       bg = 'white', scale = 1.3)
