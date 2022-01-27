setwd("~/Shared/dissertation/introduction")

library(tidyverse)
library(scales)

# data from
# https://www.migrationpolicy.org/programs/data-hub/charts/immigrant-population-over-time

foreign_born_data <- 
  read_csv("share.csv") 

foreign_born_data %>%
  ggplot(
    aes(
      x = year
    )
  ) +
  geom_area(
    aes(
      y = count/3000000,
      fill = "Number of Immigrants (M)"
    ),
    alpha = .6
  ) +
  geom_line(
    aes(
      y = share,
      color = "Share of U.S. population (%)"
    ),
    size = 1.5,
  ) +
  
  scale_x_continuous(
    expand = c(0, 0),
    breaks = c(seq(1850 , 2020, 10), 2019)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    name = "Immigrants as % share of the U.S. population",
    limits = c(0, 18),
    labels = label_number(accuracy = 1, suffix = "%"),
    sec.axis = 
      sec_axis(~ ./3000000,
               name = "Number of Immigrants",
               labels = function(x) paste0(x*10000000, "M"))
  ) +
  geom_vline(
    xintercept = 1882
  ) +
  geom_label(
    aes(
      x = 1883,
      y = 17,
      label = "1882 Chinese Exclusion Act"
    ),
    family = "Roboto Condensed",
    size = 4, 
    fontface = "bold"
  ) +
  geom_vline(
    xintercept = 1924
  ) +
  geom_label(
    aes(
      x = 1924,
      y = 15.5,
      label = "1924 Johnson-Reed Act"
    ),
    family = "Roboto Condensed",
    size = 4, 
    fontface = "bold"
  ) +
  geom_vline(
    xintercept = 1965
  ) +
  geom_label(
    aes(
      x = 1965,
      y = 13,
      label = "1965 Hart-Cellar Act"
    ),
    family = "Roboto Condensed",
    size = 4, 
    fontface = "bold"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    text = element_text(family = "Roboto Condensed", size = 14, face = "bold"),
    axis.title.y.left = element_text(vjust = 1.5),
    axis.title.y.right = element_text(vjust = 1.5),
    axis.text.x = element_text(angle = 60, hjust = 1),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  ) 



# unauthorized


undocumented_data <- tibble::tribble(
                  ~year,      ~dhs,      ~pew,
                  1990L,  3500000L,  3500000L,
                  1991L,  4025000L,        NA,
                  1992L,  4204000L,        NA,
                  1993L,  4492000L,        NA,
                  1994L,  4750000L,        NA,
                  1995L,  5146000L,  5700000L,
                  1996L,  5581000L,        NA,
                  1997L,  5862000L,        NA,
                  1998L,  6098000L,        NA,
                  1999L,  6488000L,        NA,
                  2000L,  8460000L,  8600000L,
                  2001L,        NA,        NA,
                  2002L,        NA,        NA,
                  2003L,        NA,        NA,
                  2004L,        NA,        NA,
                  2005L, 10490000L, 11100000L,
                  2006L, 11310000L, 11600000L,
                  2007L, 11780000L, 12200000L,
                  2008L, 11600000L, 11700000L,
                  2009L, 10750000L, 11300000L,
                  2010L, 11590000L, 11400000L,
                  2011L, 11510000L, 11500000L,
                  2012L, 11430000L, 11200000L,
                  2013L, 11210000L, 11200000L,
                  2014L, 11460000L, 11100000L,
                  2015L, 11440000L, 11000000L,
                  2016L, 11750000L, 10700000L,
                  2017L, 11410000L, 10500000L
                  )

undocumented_data %>%
  rename(
    DHS = dhs,
    PEW = pew
  ) %>%
  pivot_longer(
    cols = -year
  ) %>% 
  drop_na() %>%
  ggplot(
    aes(
      x = year,
      y = value,
      color = as.factor(name),
      #group = value
    )
  ) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1989, 2018),
    breaks = c(seq(1990, 2017, 5), 2017)
  ) +
  scale_y_continuous(
    name = "Estimated Number of Undocummented Immigrants",
    expand = c(0, 0),
    limits = c(2500000, 13000000),
    label = number_format(scale = .000001, accuracy = 1, suffix = "M")
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    text = element_text(family = "Roboto Condensed", size = 14, face = "bold"),
    axis.title.y.left = element_text(vjust = 1.5),
    axis.title.y.right = element_text(vjust = 1.5),
    axis.text.x = element_text(angle = 60, hjust = 1),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  ) 
  




#### DACA applications

daca_data <- 
  tibble::tribble(
    ~year,     ~type, ~approved,
    2012L, "Initial",      1684,
    2013L, "Initial",    470598,
    2014L, "Initial",    135921,
    2014L, "Renewal",     22234,
    2015L, "Initial",     90827,
    2015L, "Renewal",    419502,
    2016L, "Initial",     52992,
    2016L, "Renewal",    145821,
    2017L, "Initial",     47132,
    2017L, "Renewal",    414777,
    2018L, "Initial",     24381,
    2018L, "Renewal",    294960,
    2019L, "Initial",      1775,
    2019L, "Renewal",    385670,
    2020L, "Initial",      1792,
    2020L, "Renewal",    292916,
    2021L, "Initial",      5779,
    2021L, "Renewal",    217626
    )

daca_data %>%
  filter(
    type == "Initial"
  ) %>%
  mutate(Total = cumsum(approved)) %>%
  rename(`Newly Approved` = approved) %>%
  pivot_longer(
    cols = c(-year, -type)
  ) %>%
  mutate(
    name = as.factor(name)
  ) %>%
  ggplot(
    aes(
      x = year,
      y = value,
      color = name
    )
  ) +
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(size = 3) +
  geom_label(
    aes(
      x = 2021,
      y = 880000
    ),
    label = "833K",
    show.legend = FALSE,
    family = "Roboto Condensed",
    size = 4, 
    fontface = "bold",
    fill = "darkorange",
    color = "white"
  ) +
  geom_label(
    aes(
      x = 2021,
      y = 60000
    ),
    label = "5.8K",
    show.legend = FALSE,
    family = "Roboto Condensed",
    size = 4, 
    fontface = "bold",
    fill = "#2ca25f",
    color = "white"
  ) +
  scale_x_continuous(
   # expand = c(0, 0),
    limits = c(2012, 2021),
    breaks = c(seq(2012, 2021, 1))
  ) +
  scale_y_continuous(
    name = "DACA Applications Approved",
  #  expand = c(0, 0),
    limits = c(0, 900000),
    label = number_format(big.mark = ",")
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    text = element_text(family = "Roboto Condensed", size = 14, face = "bold"),
    axis.title.y.left = element_text(vjust = 1.5),
    axis.title.y.right = element_text(vjust = 1.5),
    axis.text.x = element_text(angle = 60, hjust = 1),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  ) 
