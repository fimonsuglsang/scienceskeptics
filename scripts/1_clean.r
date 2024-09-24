
library(tidyverse)

# reading the raw eurobarometer data
raw <- haven::read_sav("./data/ebs_526_SPSS.sav")


# data recoded

raw %>% 
  # getting strings for countries
  mutate(country = as_factor(B)) %>%
  # merging the Germanies, and removing country code
  mutate(
    country = str_replace(country, "EG", "Germany (DE)"),
    country = str_replace(country, "WG", "Germany (DE)"),
    country = str_remove(country, " \\(\\w\\w\\)")
  ) %>% 
  # Removing Albania due to data issues
  filter(country != "Albania") %>%
  
  # Re-coding science skepticism variables
  transmute(
    cskep = case_when(as_factor(QA20_9) == "True" ~ 1, as_factor(QA20_9) == "False" ~ 0),
    cdoubt = case_when(as_factor(QA20_9) == "Don't know" ~ 1, as_factor(QA20_9) == "False" ~ 0),
    eskep = case_when(as_factor(QA20_8) == "False" ~ 1, as_factor(QA20_8) == "True" ~ 0),
    edoubt = case_when(as_factor(QA20_8) == "Don't know" ~ 1, as_factor(QA20_8) == "True" ~ 0),
    
    # Cleaning control variables
    pol = case_when(D1 < 11 ~ D1),
    edu = case_when(D8R<6 ~ D8R) %>%  
      as_factor(),
    rel = case_when(D90.1 < 11 ~ D90.1),
    gender = as_factor(D10) %>% 
      str_remove(" / .*") %>% 
      as_factor(),
    age = SD5 %>% na_if(5) %>% 
      as_factor(),
    urb = D25 %>% na_if(4) %>% 
      as_factor(),
    bills = D60 %>% na_if(4) %>% 
      as_factor(),
    socscale = case_when(D63<6 ~ D63) %>% 
      as_factor(),
    sciint = QA2_2 %>% 
      na_if(4) %>% 
      as_factor(),
    # keeping id
    id = Unique_ID,
    # re-coding attitude and engagement variables
    across(QA14_1:QA14_12, \(x) 5 - (na_if(x, 5))),
    across(paste0("QA10_", 1:6), \(x) 6 - na_if(x, 6)),
    QA10_12 = case_when(QA10_1>0 ~ QA10_1, QA10_2>0 ~ QA10_2),
    across(paste0("QA11_", 1:3), \(x) na_if(x, 6)),
    
    # creating science literacy measure
    across(c(QA20_2, QA20_4, QA20_8), \(x) 1+x),
    across(matches("QA20"), \(x) case_when(x == 2 ~ T, .default = F))
   ) %>% 
  mutate(scilit = rowSums(select(., paste0("QA20_", c(1:6))))) -> clean 

# computing factor scores for science attitudes
bind_cols(
  clean,
  # running factor analysis
  psych::factor.scores(
    clean %>% select(QA10_12, QA10_3, QA10_4, QA10_5, QA10_6, QA11_1, QA11_2, QA11_3),
    psych::fa(clean %>% select(QA10_12, QA10_3, QA10_4, QA10_5, QA10_6, QA11_1, QA11_2, QA11_3), 3)
  )$scores %>% as_tibble() %>% rename(scientist = MR2, promise = MR1, techoptimism = MR3)
) %>% 
  
  # standardizing
  mutate(
    promise = (promise-mean(promise, na.rm = T))/sd(promise, na.rm = T),
    techoptimism = (techoptimism-mean(techoptimism, na.rm = T))/sd(techoptimism, na.rm = T),
    scientist = (scientist-mean(scientist, na.rm = T))/sd(scientist, na.rm = T)
  ) -> facto

# computing science engagement clusters

# pca analysis
princomp(
  clean %>%  
    select(starts_with("QA14_")) %>%
    drop_na(), 
  cor = TRUE 
) -> pca

bind_cols(
  clean %>% drop_na(starts_with("QA14_")), 
  # clustering respondents
  hclust(
    dist(
      factoextra::get_pca_ind(pca)$coord[,1:2], 
      method = "euclidean"
    ), 
    method = "ward.D2", 
    members = NULL
  ) %>% 
    cutree(k = 4)
) %>% 
  rename(engage = length(names(.))) %>% 
  mutate(
    engage = case_when(
      engage == 1 ~ "Proactive",
      engage == 2 ~ "Invested",
      engage == 3 ~ "Disengaged",
      engage == 4 ~ "Aware"
    )
  ) %>% 
  mutate(
    engage = fct_relevel(engage, "Disengaged", "Aware", "Invested", "Proactive")
  )-> clust



