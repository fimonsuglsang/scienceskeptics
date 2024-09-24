library(tidyverse)
library(ggeffects)

#reading the regressions, must be run first
load("data/regs.rdata")

#making a list of all countries to loop over
clean %>% 
  select(country) %>% 
  distinct() -> clist


# the remainder of the document runs a series of loops
# each loop extracts specific predicted probabilities based on the regressions
# this is slightly computationally heavy, and as such results are saved at the end

#FIGURE 1 DATA

# figure 1 presents predicted probabilities for each sociodemographic variable
# these are extracted below

##empty tibble
fig1_preds <- tibble()
##predprob loop
for(i in 1:4){
  for(var in  c("age", "gender", "edu", "urb", "bills", "socscale", "pol", "rel")) {
    bind_rows(
      fig1_preds,
      ggpredict(list(reg_cbase, reg_cbasedk, reg_ebase, reg_ebasedk)[[i]], terms = var) %>% 
        as_tibble() %>% 
        mutate(
          var = paste(var), 
          model = c("reg_cbase", "reg_cbasedk", "reg_ebase", "reg_ebasedk")[[i]], 
          x = as_factor(x)
        )
    ) -> fig1_preds
    print(paste(var, "in", c("reg_cbase", "reg_cbasedk", "reg_ebase", "reg_ebasedk")[[i]]))
  }
}

for(i in 1:2){
  for(var in  c("scilit")) {
    bind_rows(
      fig1_preds,
      ggpredict(list(reg_cbase, reg_ebase)[[i]], terms = var) %>% 
        as_tibble() %>% 
        mutate(
          var = paste(var), 
          model = c("reg_cbase", "reg_ebase")[[i]], 
          x = as_factor(x)
        )
    ) -> fig1_preds
    print(paste(var, "in", c("reg_cbase", "reg_ebase")[[i]]))
  }
}


#FIGURE 2 DATA

# figure 2 shows the predicted probabilities across science attitudes
# these are extracted in the loop below

##empty tibble
fig2_preds <- tibble()
##predprop loop
for(i in 1:4){
  for(var in  c("promise [-2:2 by=.25]", "scientist [-2:2 by=.25]", "techoptimism [-2:2 by=.25]")) {
    bind_rows(
      fig2_preds,
      ggpredict(list(reg_catts, reg_cattsdk, reg_eatts, reg_eattsdk)[[i]], terms = var) %>% 
        as_tibble() %>% 
        mutate(
          var = paste(var), 
          model = c("reg_catts", "reg_cattsdk", "reg_eatts", "reg_eattsdk")[[i]]
        )
    ) -> fig2_preds
    print(paste(var, "in", c("reg_catts", "reg_cattsdk", "reg_eatts", "reg_eattsdk")[[i]]))
  }
}

# then the same analysis is run per country
fig2_preds_countries <- tibble()

# first for skeptics
for(c in 1:nrow(clist)){
  t_1 <- filter(facto, country == paste(clist[c,]))
  
  for(dep in c("cskep", "eskep")){
    t_2 <- rename(t_1, dep = paste(dep))
    t_3 <- glm(
      dep~age+gender+edu+scilit+pol+rel+scientist+promise+techoptimism, 
      data = t_2,
      family = binomial(link = "logit")
    )
    
    for(var in  c("promise [-2:2 by=.25]", "scientist [-2:2 by=.25]", "techoptimism [-2:2 by=.25]")) {
      bind_rows(
        fig2_preds_countries,
        ggpredict(t_3, terms = var) %>% 
          as_tibble() %>% 
          mutate(
            country = paste(clist[c,]),
            dep = paste(dep),
            var = paste(var)
          )) -> fig2_preds_countries
    }}
  print(paste(clist[c,], "done,", c, "of", nrow(clist)))
}

# then for doubting
for(c in 1:nrow(clist)){
  t_1 <- filter(facto, country == paste(clist[c,]))
  
  for(dep in c("cdoubt", "edoubt")){
    t_2 <- rename(t_1, dep = paste(dep))
    t_3 <- glm(
      dep~age+gender+edu+pol+rel+scientist+promise+techoptimism, 
      data = t_2,
      family = binomial(link = "logit")
    )
    
    for(var in  c("promise [-2:2 by=.25]", "scientist [-2:2 by=.25]", "techoptimism [-2:2 by=.25]")) {
      bind_rows(
        fig2_preds_countries,
        ggpredict(t_3, terms = var) %>% 
          as_tibble() %>% 
          mutate(
            country = paste(clist[c,]),
            dep = paste(dep),
            var = paste(var)
          )) -> fig2_preds_countries
    }}
  print(paste(clist[c,], "done,", c, "of", nrow(clist)))
}


#FIGURE 3 DATA

# figure 3 show predicted probabilities across relationship with science

##empty tibble
fig3_preds <- tibble()
##predprob loop
for(i in 1:4){
  for(var in  c("sciint", "engage")) {
    bind_rows(
      fig3_preds,
      ggpredict(list(reg_cenga, reg_cengadk, reg_eenga, reg_eengadk)[[i]], terms = var) %>% 
        as_tibble() %>% 
        mutate(
          var = paste(var), 
          model = c("reg_cenga", "reg_cengadk", "reg_eenga", "reg_eengadk")[[i]]
        )
    ) -> fig3_preds
    print(paste(var, "in", c("reg_cenga", "reg_cengadk", "reg_eenga", "reg_eengadk")[[i]]))
  }
}


# then the same analysis is run per country
fig3_preds_countries <- tibble()

# first for skeptics
for(c in 1:nrow(clist)){
  t_1 <- filter(clust, country == paste(clist[c,]))
  
  for(dep in c("cskep", "eskep")){
    t_2 <- rename(t_1, dep = paste(dep))
    t_3 <- glm(
      dep~age+gender+edu+scilit+pol+rel+sciint+engage, 
      data = t_2,
      family = binomial(link = "logit")
    )
    
    for(var in  c("sciint", "engage")) {
      bind_rows(
        fig3_preds_countries,
        ggpredict(t_3, terms = var) %>% 
          as_tibble() %>% 
          mutate(
            country = paste(clist[c,]),
            dep = paste(dep),
            var = paste(var)
          )) -> fig3_preds_countries
    }}
  print(paste(clist[c,], "done,", c, "of", nrow(clist)))
}

# then for doubting
for(c in 1:nrow(clist)){
  t_1 <- filter(clust, country == paste(clist[c,]))
  
  for(dep in c("cdoubt", "edoubt")){
    t_2 <- rename(t_1, dep = paste(dep))
    t_3 <- glm(
      dep~age+gender+edu+pol+rel+sciint+engage, 
      data = t_2,
      family = binomial(link = "logit")
    )
    
    for(var in  c("sciint", "engage")) {
      bind_rows(
        fig3_preds_countries,
        ggpredict(t_3, terms = var) %>% 
          as_tibble() %>% 
          mutate(
            country = paste(clist[c,]),
            dep = paste(dep),
            var = paste(var)
          )) -> fig3_preds_countries
    }}
  print(paste(clist[c,], "done,", c, "of", nrow(clist)))
}


save(list = ls(pattern = "fig\\d"), file = "data/figs.rdata")

