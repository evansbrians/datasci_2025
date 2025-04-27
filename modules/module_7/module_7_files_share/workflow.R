# setup -------------------------------------------------------------------

library(tidyverse)

dc_birds <- 
  read_rds('data/raw/district_birds.rds')

# plot bird counts by diet guild ------------------------------------------

dc_birds %>% 
  pluck("counts") %>% 
  select(spp, count) %>% 
  
  # Get life history info:
  
  left_join(
    dc_birds %>% 
      pluck("birds") %>% 
      
      # Convert life histories to title case:
      
      mutate(
        across(foraging:diet,
               str_to_title)),
    by = join_by(spp == species)) %>% 
  
  # Remove unused columns and reorder:
  
  select(common_name:diet, count) %>% 
  
  # Summarize data for plotting:
  
  summarize(
    n_birds = sum(count),
    .by = diet) %>% 
  
  # Plot data:
  
  ggplot(
    aes(x = diet,
        y = n_birds)) +
  geom_bar(
    stat = "identity",
    fill = "#dcdcdc",
    color = "black") +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 4000)) +
  labs(
    title = "Bird counts by diet guild",
    x = "Diet guild",
    y = "Birds observed") +
  
  # Thematic elements:
  
  theme(
    
    # Panel elements:
    
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    
    # Axis elements:
    
    axis.line = 
      element_line(
        color = "black",
        lineend = "round"),
    axis.line.x = element_line(linewidth = 0.5),
    axis.line.y = element_line(linewidth = 0.3),
    
    # Text elements:
    
    axis.text = 
      element_text(
      size = 10,
      family = "serif"),
    axis.title = 
      element_text(
        size = 12,
        family = "serif"),
    axis.title.x = 
      element_text(
        margin = 
          margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0)),
    axis.title.y = 
      element_text(
        margin = 
          margin(
            t = 0,
            r = 5,
            b = 0,
            l = 0)),
    plot.title = 
      element_text(
        size = 14,
        family = "serif",
        hjust = 0.5))

# plot bird counts by foraging guild --------------------------------------

dc_birds %>% 
  pluck("counts") %>% 
  select(spp, count) %>% 
  
  # Get common names and diet:
  
  left_join(
    dc_birds %>% 
      pluck("birds") %>% 
      
      # Convert life histories to title case:
      
      mutate(
        across(foraging:diet,
               str_to_title)),
    by = join_by(spp == species)) %>% 
  
  # Remove unused columns and reorder:
  
  select(common_name:diet, count) %>% 
  
  # Summarize data for plotting:
  
  summarize(
    n_birds = sum(count),
    .by = foraging) %>% 
  
  # Plot data:
  
  ggplot(
    aes(x = foraging,
        y = n_birds)) +
  geom_bar(
    stat = "identity",
    fill = "#dcdcdc",
    color = "black") +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 5000)) +
  labs(
    title = "Bird counts by foraging guild",
    x = "Foraging guild",
    y = "Birds observed") +
  
  # Thematic elements:
  
  theme(
    
    # Panel elements:
    
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    
    # Axis elements:
    
    axis.line = 
      element_line(
        color = "black",
        lineend = "round"),
    axis.line.x = element_line(linewidth = 0.5),
    axis.line.y = element_line(linewidth = 0.3),
    
    # Text elements:
    
    axis.text = 
      element_text(
        size = 10,
        family = "serif"),
    axis.title = 
      element_text(
        size = 12,
        family = "serif"),
    axis.title.x = 
      element_text(
        margin = 
          margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0)),
    axis.title.y = 
      element_text(
        margin = 
          margin(
            t = 0,
            r = 5,
            b = 0,
            l = 0)),
    plot.title = 
      element_text(
        size = 14,
        family = "serif",
        hjust = 0.5))

# plot bird mass by diet guild --------------------------------------------

dc_birds %>% 
  pluck("captures") %>% 
  select(spp, mass) %>% 
  
  # Get common names and diet:
  
  left_join(
    dc_birds %>% 
      pluck("birds") %>% 
      
      # Convert life histories to title case:
      
      mutate(
        across(foraging:diet,
               str_to_title)),
    by = join_by(spp == species)) %>% 
  
  # Remove unused columns and reorder:
  
  select(common_name:diet, mass) %>% 
  
  # Plot data:
  
  ggplot(
    aes(x = diet,
        y = mass)) +
  geom_boxplot(
    fill = "#dcdcdc",
    na.rm = TRUE) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 125)) +
  labs(
    title = "Bird mass by diet guild",
    x = "Diet guild",
    y = "Mass") +

  # Thematic elements:
  
  theme(
    
    # Panel elements:
    
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    
    # Axis elements:
    
    axis.line = 
      element_line(
        color = "black",
        lineend = "round"),
    axis.line.x = element_line(linewidth = 0.5),
    axis.line.y = element_line(linewidth = 0.3),
    
    # Text elements:
    
    axis.text = 
      element_text(
        size = 10,
        family = "serif"),
    axis.title = 
      element_text(
        size = 12,
        family = "serif"),
    axis.title.x = 
      element_text(
        margin = 
          margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0)),
    axis.title.y = 
      element_text(
        margin = 
          margin(
            t = 0,
            r = 5,
            b = 0,
            l = 0)),
    plot.title = 
      element_text(
        size = 14,
        family = "serif",
        hjust = 0.5))

# plot bird mass by foraging guild ----------------------------------------

dc_birds %>% 
  pluck("captures") %>% 
  select(spp, mass) %>% 
  
  # Get common names and diet:
  
  left_join(
    dc_birds %>% 
      pluck("birds") %>% 
      
      # Convert life histories to title case:
      
      mutate(
        across(foraging:diet,
               str_to_title)),
    by = join_by(spp == species)) %>% 
  
  # Remove unused columns and reorder:
  
  select(common_name:diet, mass) %>% 
  
  # Plot data:
  
  ggplot(
    aes(x = foraging,
        y = mass)) +
  geom_boxplot(
    fill = "#dcdcdc",
    na.rm = TRUE) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 125)) +
  labs(
    title = "Bird mass by foraging guild",
    x = "Foraging guild",
    y = "Mass") +

  # Thematic elements:
  
  theme(
    
    # Panel elements:
    
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    
    # Axis elements:
    
    axis.line = 
      element_line(
        color = "black",
        lineend = "round"),
    axis.line.x = element_line(linewidth = 0.5),
    axis.line.y = element_line(linewidth = 0.3),
    
    # Text elements:
    
    axis.text = 
      element_text(
        size = 10,
        family = "serif"),
    axis.title = 
      element_text(
        size = 12,
        family = "serif"),
    axis.title.x = 
      element_text(
        margin = 
          margin(
            t = 10,
            r = 0,
            b = 0,
            l = 0)),
    axis.title.y = 
      element_text(
        margin = 
          margin(
            t = 0,
            r = 5,
            b = 0,
            l = 0)),
    plot.title = 
      element_text(
        size = 14,
        family = "serif",
        hjust = 0.5))
