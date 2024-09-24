
library(tidyverse)
library(lme4)

# this script runs all the regressions for the analyses in the article. 
# as regressions are computationally heavy, they are all saved at the end.
# run "clean.r" prior to running this script.

#Climate skepticism

glmer(
  cskep~1+age+gender+edu+urb+bills+socscale+scilit+pol+rel+(1|country),
  data = clean,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_cbase

glmer(
  cskep~1+age+gender+edu+urb+bills+socscale+scilit+(1|country),
  data = clean,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_cbasenoww

glmer(
  cskep~1+age+gender+edu+scilit+pol+rel+scientist+promise+techoptimism+(1|country),
  data = facto,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_catts

glmer(
  cskep~1+age+gender+edu+scilit+pol+rel+sciint+engage+(1|country),
  data = clust,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_cenga

#Climate doubt

glmer( 
  cdoubt~1+age+gender+edu+urb+bills+socscale+pol+rel+(1|country),
  data = clean,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_cbasedk

glmer( 
  cdoubt~1+age+gender+edu+urb+bills+socscale+(1|country),
  data = clean,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_cbasedknoww

glmer(
  cdoubt~1+age+gender+edu+pol+rel+scientist+promise+techoptimism+(1|country),
  data = facto,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_cattsdk

glmer(
  cdoubt~1+age+gender+edu+pol+rel+sciint+engage+(1|country),
  data = clust,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_cengadk

#Evolution skepticism

glmer(
  eskep~1+age+gender+edu+urb+bills+socscale+scilit+pol+rel+(1|country),
  data = clean,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_ebase

glmer(
  eskep~1+age+gender+edu+urb+bills+socscale+scilit+(1|country),
  data = clean,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_ebasenoww

glmer(
  eskep~1+age+gender+edu+scilit+pol+rel+scientist+promise+techoptimism+(1|country),
  data = facto,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_eatts

glmer(
  eskep~1+age+gender+edu+scilit+pol+rel+sciint+engage+(1|country),
  data = clust,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_eenga

#Evolution doubt

glmer(
  edoubt~1+age+gender+edu+urb+bills+socscale+pol+rel+(1|country),
  data = clean,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_ebasedk

glmer(
  edoubt~1+age+gender+edu+urb+bills+socscale+(1|country),
  data = clean,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_ebasedknoww

glmer(
  edoubt~1+age+gender+edu+pol+rel+scientist+promise+techoptimism+(1|country),
  data = facto,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_eattsdk

glmer(
  edoubt~1+age+gender+edu+pol+rel+sciint+engage+(1|country),
  data = clust,
  family = binomial(link='logit'),
  control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> reg_eengadk


save(list = ls(pattern = "reg_"), file = "data/regs.rdata")


