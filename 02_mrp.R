####################################################################
### Title: Implementing MrP (Post-stratification)
### Author: Magdalena Bennett
### Date Created: 05/29/2022
### Edits: [05/29/2022] - Created code (based on https://timmastny.com/blog/multilevel-mrp-tidybayes-brms-stan/)
###                       (Note: This is only done for voters in Chile)
###        [06/06/2022] - Updated new data (may 2Q)
####################################################################

#Clear memory
rm(list = ls())

#Clear the console
cat("\014")

library(haven)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(firasans)
library(brms)
library(rstan)
library(stringr)
library(lme4)
library(tidybayes)
library(broom.mixed)

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

main_dir <- "C:/Users/mc72574/Dropbox/UT/UT Research/Apruebo/data/"

pc_marzo2 <- read_sav(paste0(main_dir,"PulsoCiudadano/BBDD_Tracking Opinion_Marzo_q2_v3.sav"))
pc_abril1 <- read_sav(paste0(main_dir,"PulsoCiudadano/pulso_ciudadano_abril_2022_q1.sav"))
pc_abril2 <- read_sav(paste0(main_dir,"PulsoCiudadano/PULSO_CIUDADANO_DDBB_FINAL_20220429_v1_spss_WEB.sav"))
pc_mayo1 <- read_sav(paste0(main_dir,"PulsoCiudadano/221283_BBDD_Tracking Opinion_Mayo_ola1_13052022_WEB_v2.sav"))
pc_mayo2 <- read_sav(paste0(main_dir,"PulsoCiudadano/Pulso Ciudadano_Mayo_Q2_27052022_v1_spss_WEB.sav"))

# We are going to try to increase the sample size:
id_mayo2 <- unique(pc_mayo2$ID)
id_mayo1_not_after <- pc_mayo1$ID[!(pc_mayo1$ID %in% id_mayo2)]
id_abril2_not_after <- pc_abril2$ID[!(pc_abril2$ID %in% c(id_mayo1_not_after,id_mayo2))]
id_abril1_not_after <- pc_abril1$ID[!(pc_abril1$ID %in% c(id_mayo1_not_after, id_abril2_not_after,id_mayo2))]
id_marzo2_not_after <- pc_marzo2$ID[!(pc_marzo2$ID %in% c(id_mayo1_not_after, id_abril2_not_after, id_abril1_not_after,id_mayo2))]

# Create panel:
# P158: Voted in the previous referendum
# P310: What option are you going to vote in referendum 2022
# P327: Intention to go vote in referendum 2022
# P330: What option do you think will win in the referendum 2022
# P332: What option did you vote in the previous referendum

pc_mayo2 <- pc_mayo2 %>% select(ID, MES_MED, SEXO, EDAD, RANGOEDAD, REGION, DISTRITO_ELECTORAL, P13_1, P14, P310, P158) %>%
  rename(vote_option2022_pc = P310,
         political_pos_pc = P13_1,
         political_coal_pc = P14) %>%
  mutate(likelyvoter2020 = ifelse(P158 == 1, 1, 0)) %>%
  select(-P158)

pc_mayo1 <- pc_mayo1 %>% filter(ID %in% id_mayo1_not_after) %>% select(ID, MES_MED, SEXO, EDAD, RANGOEDAD, REGION, DISTRITO_ELECTORAL, P13_1, P14, P310, P158) %>%
  rename(vote_option2022_pc = P310,
         political_pos_pc = P13_1,
         political_coal_pc = P14) %>%
  mutate(likelyvoter2020 = ifelse(P158 == 1, 1, 0)) %>%
  select(-P158)

# How responses distribute between previous "voters" and "non-voters" 
#table(pc_mayo1$P158, pc_mayo1$vote_option2022_pc) %>% prop.table(., margin = 1)

#pc_mayo1 <- pc_mayo1 %>% mutate(apruebo2020 = ifelse(P332 == 1, 1, 0),
#                                likelyvoter2020 = ifelse(P158 == 1, 1, 0)) %>%
#  select(-c(P332, P158))

pc_abril2 <- pc_abril2 %>% filter(ID %in% id_abril2_not_after) %>% select(ID, MES_MED, SEXO, EDAD, RANGOEDAD, REGION, DISTRITO_ELECTORAL, P13_1, P14, P310) %>%
  rename(vote_option2022_pc = P310,
         political_pos_pc = P13_1,
         political_coal_pc = P14)

pc_abril1 <- pc_abril1 %>% filter(ID %in% id_abril1_not_after) %>% select(ID, MES_MED, SEXO, EDAD, RANGOEDAD, REGION, DISTRITO_ELECTORAL, P13_1, P14, P310) %>%
  rename(vote_option2022_pc = P310,
         political_pos_pc = P13_1,
         political_coal_pc = P14)

pc_marzo2 <- pc_marzo2 %>% filter(ID %in% id_marzo2_not_after) %>% select(ID, MES_MED, SEXO, EDAD, RANGOEDAD, REGION, DISTRITO_ELECTORAL, P13_1, P14, P310) %>%
  rename(vote_option2022_pc = P310,
         political_pos_pc = P13_1,
         political_coal_pc = P14)

pc_encuesta <- pc_mayo2 %>% add_row(pc_mayo1) %>% add_row(pc_abril2) %>% add_row(pc_abril1) %>% add_row(pc_marzo2)

pc_encuesta$vote_option2022_pc %>% attr('labels')
pc_encuesta$political_pos_pc %>% attr('labels')
pc_encuesta$political_coal_pc %>% attr('labels')


d <- read.csv(paste0(main_dir,"working/d_2022_potential_voters.csv"))

distritos <- read.csv(paste0(main_dir,"Servel/distrito_comuna.csv"))

votos_apruebo_distritos <- read.csv(paste0(main_dir,"Servel/votos_apruebo_distrito.csv"))

d <- left_join(d, distritos, by = "Comuna")

# Create a district level dataset, to add to the poll
d$DISTRITO_ELECTORAL <- as.numeric(str_replace_all(d$Distrito, "DISTRITO ", ""))

d$REGION[d$REGION == "Aisen Del Gral. Carlos Ibañez"] <- "11"
d$REGION[d$REGION == "Arica Y Parinacota"] <- "15"
d$REGION[d$REGION == "De Antofagasta"] <- "2"
d$REGION[d$REGION == "De Atacama"] <- "3"
d$REGION[d$REGION == "De Coquimbo"] <- "4"
d$REGION[d$REGION == "De La Araucania"] <- "9"
d$REGION[d$REGION == "De Los Lagos"] <- "10"
d$REGION[d$REGION == "De Los Rios"] <- "14"
d$REGION[d$REGION == "De Magallanes Y Antartica Ch."] <- "12"
d$REGION[d$REGION == "De Ñuble"] <- "16"
d$REGION[d$REGION == "De Tarapaca"] <- "1"
d$REGION[d$REGION == "De Valparaiso"] <- "5"
d$REGION[d$REGION == "Del Biobio"] <- "8"
d$REGION[d$REGION == "Del Libertador Bdo. O'Higgins"] <- "6"
d$REGION[d$REGION == "Del Maule"] <- "7"
d$REGION[d$REGION == "Metropolitana De Santiago"] <- "13"

d$REGION <- as.numeric(d$REGION)


d_district <- d %>% group_by(DISTRITO_ELECTORAL, Comuna) %>% filter(row_number()==1)

d_district <- d_district %>% group_by(DISTRITO_ELECTORAL) %>% summarise(part_distrito_2021 = sum(vote_comuna2021_2da)/sum(n_comuna2021_2da),
                                                                  n_distrito_2021 = sum(n_comuna2021_2da),
                                                                  perc_boric_2021 = sum(Boric_2021_2da)/(sum(Boric_2021_2da) + sum(Kast_2021_2da)),
                                                                  part_region2009_1era = mean(part_region2009_1era))


d_district <- left_join(d_district, votos_apruebo_distritos, by = "DISTRITO_ELECTORAL")

pc_encuesta <- left_join(pc_encuesta, d_district, by = "DISTRITO_ELECTORAL")

# Rename variables (so they make sense when we merge)

pc_encuesta <- pc_encuesta %>% mutate(edad_cat = ifelse(RANGOEDAD==2, "18-30",
                                                        ifelse(RANGOEDAD==3, "31-40",
                                                               ifelse(RANGOEDAD==4, "41-50", "51+")))) %>%
  mutate(female = ifelse(SEXO==2, 1, 0),
         edad_cat = factor(edad_cat, levels = c("18-30","31-40","41-50","51+")))

# Combining demographic groups:

pc_encuesta <- pc_encuesta %>% mutate(age.voter = interaction(edad_cat, likelyvoter2020))

# change column names for natural join with marriage.data
d <- d %>% mutate(female = ifelse(Sexo=="femenino", 1, 0),
                  edad_cat = factor(edad_cat, levels = c("18-30","31-40","41-50","51+")),
                  likelyvoter2020 = Votaron,
                  age.voter = interaction(edad_cat, likelyvoter2020))

d <- right_join(d_district, d, by = "DISTRITO_ELECTORAL")

#Let's only select likely voters:
d_ps <- d %>% filter(potential_voter2022==1) %>%
  group_by(REGION, DISTRITO_ELECTORAL, edad_cat, female, likelyvoter2020) %>%
  summarize(freq = n(),
            freq_distrito = mean(n_distrito_2021)) %>%
  mutate(cpercent_distrito = freq/freq_distrito)

d_ps <- right_join(d_district, d_ps, by = "DISTRITO_ELECTORAL")

d_ps <- d_ps %>% mutate(age.voter = interaction(edad_cat,likelyvoter2020))

d_ps <- right_join(votos_apruebo_distritos, d_ps, by = "DISTRITO_ELECTORAL")

d_ps <- d_ps %>% rename(PercApruebo = PercApruebo.x) %>% select(-PercApruebo.y)

#############################################################################
#### Models
#############################################################################

# Check which polls we keep:
pc_encuesta <- pc_encuesta %>% filter(MES_MED==202205)# %>% group_by(DISTRITO_ELECTORAL) %>%
#  mutate(PercApruebo = mean(apruebo2020))

pc_encuesta <- pc_encuesta %>% mutate(apruebo_low = ifelse(vote_option2022_pc==1, 1, 0),
                                      apruebo_high = ifelse(vote_option2022_pc==1 | vote_option2022_pc==3, 1, 0),
                                      apruebo_mid = ifelse(vote_option2022_pc==1 | 
                                                             (vote_option2022_pc==3 & political_pos_pc<=3), 1, 0))

approx.mod.high <- glmer(formula = apruebo_high ~ (1|age.voter) + (1|DISTRITO_ELECTORAL) + (1|REGION) + (1|female) +
                      PercApruebo + part_distrito_2021 + perc_boric_2021 + part_region2009_1era,
                    data = pc_encuesta, family = binomial(link="logit"))

summary(approx.mod.high)


## High-approval

bayes.mod.high <- brm(apruebo_high ~ (1|age.voter) +  (1|female) + (1|edad_cat) + (1|DISTRITO_ELECTORAL) + (1|REGION) + 
                       PercApruebo + part_distrito_2021 + perc_boric_2021 + part_region2009_1era,
                     data = pc_encuesta, family=bernoulli(),
                     prior=c(set_prior("normal(0,0.2)", class='b'),
                             set_prior("normal(0,0.2)", class='sd', group="edad_cat"),
                             set_prior("normal(0,0.2)", class='sd', group="female"),
                             set_prior("normal(0,0.2)", class='sd', group="age.voter"),
                             set_prior("normal(0,0.2)", class='sd', group="DISTRITO_ELECTORAL"),
                             set_prior("normal(0,0.2)", class='sd', group="REGION")))

summary(bayes.mod.high)

ps.bayes.mod.high <- bayes.mod.high %>%
  add_predicted_draws(newdata = d_ps, allow_new_levels=TRUE, ndraws = 100) %>%
  rename(apruebo = .prediction) %>%
  mean_qi() %>%
  mutate(apruebo = apruebo * cpercent_distrito) %>%
  group_by(DISTRITO_ELECTORAL) %>%
  summarise(apruebo = sum(apruebo))


approx_sd <- broom::tidy(approx.mod.high) %>%
  filter(stringr::str_detect(term, "sd_"))

bayes.mod.high %>%
  gather_samples(`sd_.*`, regex=TRUE) %>%
  ungroup() %>%
  mutate(group = stringr::str_replace_all(term, c("sd_" = "","__Intercept"=""))) %>%
  ggplot(aes(y=group, x = estimate)) + 
  ggridges::geom_density_ridges(aes(height=..density..),
                                rel_min_height = 0.01, stat = "density",
                                scale=1.5) + 
  geom_point(data=approx_sd)


sim.bayes.mod.high <- predict(bayes.mod.high, newdata=d_ps, allow_new_levels=TRUE, ndraws=500, summary=FALSE)
