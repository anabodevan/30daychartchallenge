#' 1 - FRACTIONS / COMPARISSONS 


# packages 

library(pacman)

pacman :: p_load('ggplot2', 'treemapify', 'viridis',
                 'extrafont', 'ggtext', 'tidyverse', 'scales')

# data cleaning

data <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-03-21/languages.csv')

df <- df[number_of_users > 20000][
  , title := str_to_upper(title)  # Convert titles to uppercase
][
  , label := paste0(title, "\n", comma(number_of_users))  # Create combined label
]

summary(df)

df <- df[title != "PATHON"]

# plot 

p <- ggplot(df, aes(
  area = number_of_users,
  fill = number_of_users,
  label = label)) +
  
  # Treemap geometry
  geom_treemap(
    layout = "squarified",
    color = "white",
    size = 1,
    radius = unit(4, "pt")
  ) +
  
  # Text labels
  geom_treemap_text(
    color = "white",
    size = 12,
    family = "Roboto",
    padding.x = unit(2, "mm"),
    padding.y = unit(2, "mm"),
    min.size = 2
  ) +
  
  # 
  scale_fill_stepsn(
    colors = viridis(6),  #
    breaks = c(50000, 100000, 500000, 1000000, 5000000),
    trans = "log2",
    labels = c("50K", "100K", "500K", "1M", "5M"),
    guide = guide_colorsteps(
      barwidth = unit(0.5, "lines"),
      barheight = unit(10, "lines"),
      title.position = "left",
      title.hjust = 0.5
    )
  ) +
  
  # Titles and annotations
  labs(
    title = "Programming Languages by User Count",
    subtitle = "Languages with >20,000 users",
    caption = "Source: Programming Language DataBase | Visualization by Ana Bodevan",
    fill = "Number of Users"
  ) +
  
  # Theme customization
  theme_void(base_family = "Roboto") +
  theme(
    plot.background = element_rect(fill = "#f5f5f5", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(
      size = 18, 
      face = "bold", 
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 14,
      hjust = 0.5,
      color = "gray20",
      margin = margin(b = 20)
    ),
    plot.caption = element_text(
      size = 10,
      hjust = 1,
      color = "gray60",
      margin = margin(t = 15)
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 10,
      angle = 90,
      vjust = 0.5,
      hjust = 0.5
    ),
    legend.text = element_text(size = 8)
  )

ggsave(
  plot = p, filename = "Rplot.png",
  width = 9, height = 10, units = "in", dpi = 600
)
