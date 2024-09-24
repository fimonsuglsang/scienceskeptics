
library(tidyverse)
library(patchwork)

# this script produces all of the figures for the article
# it needs the data drom the figs.r script. if it has been run before then load it using:
load("data/figs.rdata")

# fig1
# skepticism/doubting across sociodemographic predictors

# making data ready for plotting
fig1_preds %>% 
  
  mutate(
    
    x = case_when(
      str_detect(x, "^[235689]$") == T & var == "pol" ~ NA,
      str_detect(x, "^[235689]$") == T & var == "rel" ~ NA,
      str_detect(x, "^[1245]$") == T & var == "scilit" ~ NA,
      .default = x
    ),
    
    x = fct_inorder(x), 
    var = fct_inorder(var),
    x = fct_relevel(x, "No full-time education", after = 4),
    x = fct_relevel(x, "1 Not at all religious or spiritual", after = 4),
    x = fct_relevel(x, "1 Left", after = 4)
  ) %>% 
  
  filter(x != "None of the above") %>% 
  separate(model, c("model", "dk"), "base") %>% 
  
  mutate(model = case_when(
    model == "reg_c" ~ "Climate",
    model == "reg_e" ~ "Evolution"
  )) %>% 
  mutate(model = fct_rev(model)) %>% 
  
  mutate(dk = case_when(
    dk == "" ~ "Skepticism",
    dk == "dk" ~ "Doubt"
  )) %>% 
  
  mutate(
    var = case_when(
      var == "age" ~ "Age",
      var == "gender" ~ "Gender",
      var == "edu" ~ "Education",
      var == "scilit" ~ "Science\nliteracy",
      var == "urb" ~ "Urbanization",
      var == "bills" ~ "Dificulty\npaying bills",
      var == "socscale" ~ "Self-rated\nsocial class",
      var == "pol" ~ "Political\nleft-right scale",
      var == "rel" ~ "Level of\nreligiosity",
    )
  ) %>% 
  
  mutate(var = fct_relevel(var, 
                           "Age", "Gender", "Urbanization", "Education", 
                           "Self-rated\nsocial class", "Dificulty\npaying bills", 
                           "Science\nliteracy",
                           "Political\nleft-right scale", "Level of\nreligiosity")) %>% 
  
  
  
  mutate(adj = mean(predicted), .by = c(var, model, dk)) %>% 
  mutate(across(c(predicted, conf.low, conf.high), \(x) x-adj)) %>% 
  
  #then plotting it using ggplot
  ggplot(aes(y = x, x = predicted, xmin = conf.low, xmax = conf.high, color = model)) +
  
  geom_pointrange(position = position_dodge(-.6), size = .3) +
  
  scale_y_discrete(limits=rev) +
  facet_grid(var~dk, scales = "free", space = "free") +
  jtools::theme_nice(legend.pos = "top") +
  
  scale_x_continuous(breaks = c(-.1, 0, .1, .2), labels = c("-10%", "Mean", "+10%", "+20%")) +
  scale_color_manual(values = c("#003d73", "#e2001a") ) +
  
  xlab("Predicted Probability") +
  ylab("") +
  
  theme(
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(hjust = 1),
    strip.text.y = element_text(hjust = 0, angle = 0)
  )

ggsave("./figures/fig1.tiff", width = 16, height = 20, units = "cm", dpi = 300)


# fig2
# skepticism/doubting across attitudes

# making data ready for plotting
bind_rows(
  fig2_preds %>% 
    mutate(dep = case_when(
      model == "reg_catts" ~ "cskep",
      model == "reg_eatts" ~ "eskep",
      model == "reg_cattsdk" ~ "cdoubt",
      model == "reg_eattsdk" ~ "edoubt"
    )
    ) %>% 
    mutate(model = "full"),
  fig2_preds_countries %>% mutate(model = "country")
) %>%  
  mutate(adj = case_when(x == 0 ~ predicted)) %>% 
  mutate(adj = max(adj, na.rm = T), .by = c(country, dep, var)) %>% 
  mutate(across(c(predicted, conf.low, conf.high), \(x)  x-adj)) %>% 
  
  mutate(dep = case_when(
    dep == "cskep" ~ "Climate\nskepticism",
    dep == "cdoubt" ~ "Climate\ndoubt",
    dep == "eskep" ~ "Evolution\nskepticism", 
    dep == "edoubt" ~ "Evolution\ndoubt"
  )) %>% 
  mutate(var = case_when(
    var == "scientist [-2:2 by=.25]" ~ "Scientist\nperceptions",
    var == "promise [-2:2 by=.25]" ~ "Promise of\nscience ",
    var == "techoptimism [-2:2 by=.25]" ~ "Technology\noptimism"
  )) %>% 

#then plotting it using ggplot
ggplot() +
  aes(x = x, y = predicted, group = country) +
  
  geom_line(
    data = . %>% filter(model == "country"),
    alpha = .1,
    size = .6,
    stat="smooth"
  ) +
  geom_line(
    data = . %>% filter(model == "full"),
    aes(color = dep),
    size = 1.1,
    stat="smooth"
  ) +
  
  geom_ribbon(
    data = . %>% filter(model == "full"),
    aes(fill = dep, ymin = conf.low, ymax = conf.high), 
    alpha = .3
  ) +
  
  facet_grid(var~dep) + 
  coord_cartesian(ylim = c(-.21, .21)) +
  
  jtools::theme_nice(legend.pos = "none") +
  scale_y_continuous(breaks = c(-.2, -.1, 0, .1, .2), labels = c("-20%", "-10%", "Mean", "10%", "20%")) +
  scale_fill_manual(values = c("#003d73", "#002546", "#e2001a", "#5b0c0c")) +
  scale_color_manual(values =c("#003d73", "#002546", "#e2001a", "#5b0c0c")) +
  
  xlab("Standard Deviations From Mean") +
  ylab("Predicted Probability") +
  
  theme(
    legend.title = element_blank(),
    panel.spacing = unit(1, "cm")
  )

ggsave("./figures/fig2.tiff", width = 16, height = 18, units = "cm", dpi = 300)

# fig3
# skepticism/doubting across relation to science

# making data ready for plotting
bind_rows(
  fig3_preds %>% 
    mutate(dep = case_when(
      model == "reg_cenga" ~ "cskep",
      model == "reg_eenga" ~ "eskep",
      model == "reg_cengadk" ~ "cdoubt",
      model == "reg_eengadk" ~ "edoubt"
    )
    ) %>% 
    mutate(model = "full"),
  fig3_preds_countries %>% mutate(model = "country")
) %>%  
  mutate(adj = case_when(x == "Aware" ~ predicted, x == "Moderately interested" ~ predicted)) %>% 
  mutate(adj = max(adj, na.rm = T), .by = c(country, dep, var)) %>% 
  mutate(across(c(predicted, conf.low, conf.high), \(x)  x-adj)) %>% 
  
  mutate(dep = case_when(
    dep == "cskep" ~ "Climate\nskepticism",
    dep == "cdoubt" ~ "Climate\ndoubt",
    dep == "eskep" ~ "Evolution\nskepticism", 
    dep == "edoubt" ~ "Evolution\ndoubt"
  )) %>% 
  mutate(var = case_when(
    var == "engage" ~ "Engagement cluster",
    var == "sciint" ~ "Interest in science"
  )) -> engdata

#then plotting it using ggplot
ggplot(engdata %>% filter(var == "Engagement cluster")) +
  aes(x = x, y = predicted, group = country, ymin = conf.low, ymax = conf.high) +
  xlab("") +
  ggplot(engdata %>% filter(var == "Interest in science")) +
  aes(x = x %>% fct_rev(), y = predicted, group = country, ymin = conf.low, ymax = conf.high) +
  xlab("Standard Deviations From Mean") +
  
  plot_layout(ncol = 1) &
  
  geom_pointrange(
    data = . %>% filter(model == "full"),
    aes(color = dep)
  ) &
  geom_point(
    data = . %>% filter(model == "country") %>% 
      filter(x != "Aware" & x != "Moderately interested"),
    position = position_dodge(.3),
    alpha = .1
  ) &
  geom_pointrange(
    data = . %>% filter(model == "full"),
    aes(color = dep)
  ) &
  
  facet_grid(var~dep, scales = "free") & 
  coord_cartesian(ylim = c(-.21, .21)) &
  
  jtools::theme_nice(legend.pos = "none") &
  scale_y_continuous(breaks = c(-.2, -.1, 0, .1, .2), labels = c("-20%", "-10%", "Mean", "10%", "20%")) &
  scale_fill_manual(values = c("#003d73", "#002546", "#e2001a", "#5b0c0c")) &
  scale_color_manual(values = c("#003d73", "#002546", "#e2001a", "#5b0c0c")) &
  
  ylab("Predicted Probability") &
  
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.spacing = unit(1, "cm")
  )

ggsave("./figures/fig3.tiff", width = 16, height = 18, units = "cm", dpi = 300)


