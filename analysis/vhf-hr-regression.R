library('ctmm')    # using the github version (0.6.1)
library('dplyr')   # for data wrangling
library('purrr')   # for vectorized computation
library('ggplot2') # for plotting
theme_set(theme_bw() +
            theme(text = element_text(size = 14),
                  panel.grid = element_blank(),
                  legend.position = 'none'))

# color palette
pal <- c('#4477AA', '#ff8c00', '#66CCEE', '#009900',
         '#CCBB44', '#EE6677', '#AA3377', '#BBBBBB')

# add correct averages for speeds and tau parameters ###########################
tapirs <- readRDS('models/tapirs-final.rds') %>%
  mutate(vhf =
           map_chr(data,
                   function(x) {
                     case_when(is.null(x$class) ~ 'VHF',
                               any(grepl('VHF', unique(x$class))) ~ 'GPS & VHF',
                               !any(grepl('VHF', unique(x$class))) ~ 'GPS')
                   }))

ggplot(tapirs) +
  geom_boxplot(aes(vhf, area.est)) +
  labs(x = NULL, y = bquote('Estimated 95% home range area'~(km^2)))

m <- glm(area.est ~ vhf, family = Gamma('log'), data = tapirs)
summary(m)

ggsave('figures/vhf-hr-boxplots.png', width = 3.23, height = 2, scale = 2)
