source('code/local_weather.R')
library(glue)
library(ggtext)

snow_data <- local_weather %>% 
    select(date, snow) %>% 
    drop_na(snow) %>% 
    mutate(
        month = month(date),
        snow_year = if_else(date < ymd(glue('{year(date)}-08-01')),
                            year(date) - 1,
                            year(date))) %>%
    select(month, snow, snow_year) %>% 
    filter(snow_year != 1947 & snow_year != 2022)

snow_data %>% 
    group_by(snow_year) %>% 
    mutate(total_snow = sum(snow)) %>% 
    ggplot(aes(x = snow_year, y = total_snow)) +
    geom_line()

total_snow <- snow_data %>% 
    filter(snow_year == 2021) %>% 
    group_by(snow_year) %>% 
    summarise(total_snow = sum(snow) / 10) %>% 
    pull(total_snow)

snow_data %>% 
    group_by(snow_year, month) %>% 
    summarise(snow = sum(snow), .groups = 'drop') %>% 
    mutate(month = factor(month, levels = c(8:12, 1:7)),
           is_this_year = 2021 == snow_year) %>% 
    ggplot(aes(x = month, y = snow, group = snow_year,
               color = is_this_year)) +
    geom_line(show.legend = FALSE) +
    scale_x_discrete(breaks = c(9, 11, 1, 3, 5),
                     labels = month.abb[c(9, 11, 1, 3, 5)],
                     expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0, 1000, 200),
                       labels = seq(0, 100, 20)) +
    scale_color_manual(name = NULL,
                       breaks = c(T, F),
                       values = c('dodgerblue', 'gray')) +
    labs(
        y = 'Total monthly snowfall (cm)',
        title = glue('The <span style = "color:dodgerblue">snow year 2021</span> had a total of {total_snow} cm of snow')) +
    theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(),
        plot.title.position = "plot", # move title to the left a bit
        plot.title = element_markdown() # Need this to make the span tag work and also include library ggtext
    )

ggsave('figures/snow_by_snow_year.png', width = 6, height =4)
