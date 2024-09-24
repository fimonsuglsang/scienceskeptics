library(tidyverse)
library(modelsummary)
library(ggeffects)

# this script produces the appendix analyses.
# run "clean.r" prior to running this script.
# and load the regression data
load(file = "data/regs.rdata")


# 2.	Descriptive statistics
# this creates and saves tables containing descriptive statistics

datasummary_skim(
  clean %>% 
    mutate(across(1:4, \(x) as_factor(x))) %>% 
    select(
      cskep, eskep, cdoubt, edoubt,
      age, gender, edu, urb, bills, socscale
    ),
  type = "categorical",
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/table2_1.docx")

datasummary_skim(
  clean %>% select(
    scilit, pol, rel
  ),
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/table2_2.docx")

datasummary_skim(
  facto %>% select(
    scientist, promise, techoptimism
  ),
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/table2_3.docx")

datasummary_skim(
  clust %>% select(
    sciint, engage
  ),
  type = "categorical",
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/table2_4.docx")


# 3.	Who are the skeptics: Regression output
# this creates and saves tables containing the regressions used in figure 1
modelsummary(
  list(
    "Climate, skepticism" = reg_cbase,
    "Climate, doubters" = reg_cbasedk,
    "Evolution, skepticism" = reg_ebase,
    "Evolution, doubters" = reg_ebasedk
  ),
  stars = T,
  output = "flextable",
  title = "base"
) %>% 
  
  flextable::save_as_docx(path = "tables/table3.docx")

# 4.	Who are the skeptics: Regression excluding world views variables
# this creates and saves tables containing the regressions from figure 1, but excluding world-view variables
modelsummary(
  list(
    "Climate, skepticism" = reg_cbasenoww,
    "Climate, doubters" = reg_cbasedknoww,
    "Evolution, skepticism" = reg_ebasenoww,
    "Evolution, doubters" = reg_ebasedknoww
  ),
  stars = T,
  output = "flextable",
  title = "no world-view vars"
) %>% 
  
  flextable::save_as_docx(path = "tables/table4.docx")

# 5.	What do the skeptics think about science: Index construction
# this creates and saves tables containing information on attitude scale formation
psych::fa(clean %>% select(
  "Science and technology make our lives easier, healthier and more comfortable" = QA10_12,
  "Science prepares the younger generation to act as well-informed citizens" = QA10_3,
  "Thanks to scientific and technological advances, the Earth’s natural resources will be" = QA10_4,
  "Thanks to science and technology, there will be more opportunities for future generation" = QA10_5,
  "Artificial intelligence and automation will create more jobs than they will eliminate" = QA10_6,
  "We can no longer trust scientists to tell the truth about controversial scientific and technological issues " = QA11_1,
  "Scientists only look at very specific issues and do not consider problems from a wider perspective" = QA11_2,
  "Nowadays, the problems we are facing are so complex that scientists are no longer able to understand them" = QA11_3
), 3) %>% 
  loadings() %>% 
  unclass() %>% 
  as_tibble(rownames = "var") %>% 
  rename(scientist = MR2, promise = MR1, techoptimism = MR3) %>% 
  mutate(across(2:4, \(x) round(x, 2))) %>% 
  
  flextable::flextable() %>% 
  flextable::save_as_docx(path = "tables/table5.docx")

#eigenvalues:
eigen(cor(
    clean %>% select(matches("QA10"), matches("QA11"), -QA10_1, -QA10_2),
    use = "pairwise.complete.obs"))$values %>% 
  tibble() %>% 
  mutate(component = paste("Factor", 1:8))
  
# 6.	What do the skeptics think about science: Regression output
# this creates and saves tables containing the regressions used in figure 2
modelsummary(
  list(
    "Climate, skepticism" = reg_catts,
    "Climate, doubters" = reg_cattsdk,
    "Evolution, skepticism" = reg_eatts,
    "Evolution, doubters" = reg_eattsdk
  ),
  stars = T,
  output = "flextable",
  title = "attitudes"
) %>% 
  
  flextable::save_as_docx(path = "tables/table6.docx")

# 7.	How invested are the skeptics in science: Engagement clusters
# this creates and saves tables containing information on the engagement measure
princomp(
  clean %>%  
    select(starts_with("QA14_")) %>%
    drop_na(), 
  cor = TRUE 
) -> pca

#eigenvalues:
pca$sdev %>% 
  tibble() %>% 
  mutate(component = paste("Component", 1:12))

princomp(
  clean %>% 
    select(starts_with("QA14_")) %>%
    rename(
      "Talk about science and technology-related issues with family or friends" = "QA14_1",
      "Watch documentaries, or read science and technology-related publications, magazines or books" = "QA14_2",
      "Visit science and technology museums" = "QA14_3",
      "Study science and technology-related issues in your free time, for instance on a face-to-face or online course" = "QA14_4",
      "Sign petitions or join demonstrations on science and technology matters such as nuclear power, biotechnology, the environment or climate change " = "QA14_5",
      "Attend public meetings or debates about science and technology" = "QA14_6",
      "Take part in the activities of a non-governmental organisation dealing with science and technology related issues" = "QA14_7",
      "Contact public authorities or political leaders about science and technology-related issues" = "QA14_8",
      "Provide personal data for scientific research" = "QA14_9",
      "Take part in clinical trials" = "QA14_10",
      "Lend your computer’s processing power to contribute to research on complex scientific questions" = "QA14_11",
      "Actively take part in scientific projects by developing research questions, collecting data, discussing the findings with others, etc." = "QA14_12"
    ) %>% 
    drop_na(), 
  cor = TRUE 
) %>% 
  loadings() %>% 
  unclass() %>% 
  as_tibble(rownames = "var") %>% 
  rename(General = Comp.1, Informative = Comp.2) %>% 
  select(1:3) %>% 
  mutate(across(2:3, \(x) round(x, 2))) %>% 
  
  flextable::flextable() %>% 
  flextable::save_as_docx(path = "tables/table7_1.docx")

clust %>% 
  select(engage, starts_with("QA14_")) %>% 
  pivot_longer(2:13) %>% 
  summarise(mean = mean(value), .by = c(engage, name)) %>% 
  pivot_wider(names_from = engage, values_from = mean) %>% 
  mutate(across(2:5, \(x) round(x, 2))) %>% 
  relocate(name, Disengaged, Aware, Invested, Proactive) %>% 
  
  flextable::flextable() %>% 
  flextable::save_as_docx(path = "tables/table7_2.docx")


hclust(
  dist(
    factoextra::get_pca_ind(pca)$coord[,1:2], 
    method = "euclidean"
  ), 
  method = "ward.D2", 
  members = NULL
) -> tree

plot(tree, labels = F, main = "", xlab = "", ylab = "", sub = "")

#respondents per cluster
clust %>% 
  count(engage) %>% 
  pivot_wider(names_from = engage, values_from = n) 



# 8.	How invested are the skeptics in science: Regression output
# this creates and saves tables containing the regressions used in figure 3
modelsummary(
  list(
    "Climate, skepticism" = reg_cenga,
    "Climate, doubters" = reg_cengadk,
    "Evolution, skepticism" = reg_eenga,
    "Evolution, doubters" = reg_eengadk
  ),
  stars = T,
  output = "flextable",
  title = "engagement"
) %>% 
  
  flextable::save_as_docx(path = "tables/table8.docx")






