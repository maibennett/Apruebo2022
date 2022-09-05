####################################################################
### Title: Characterizing likely voters (mandatory setting)
### Author: Magdalena Bennett
### Date Created: 05/27/2022
### Edits: [05/27/2022] - Created code
###        [05/29/2022] - Included voting for last election (2020)
###                       and historic participation before 2012
###                       (voluntary voting)
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
library(foreach)

main_dir <- "C:/Users/mc72574/Dropbox/UT/UT Research/Apruebo/data/"

d <- read.csv(paste0(main_dir,"Servel/VW_VOTARON_2020PLEB_Datos completos.csv"), 
              sep=";", encoding = "UTF-8")

d <- d %>% mutate(edad_cat = ifelse(Edad<=30,"18-30",
                                    ifelse(Edad<=40,"31-40",
                                           ifelse(Edad<=50,"41-50","51+"))),
                  Votaron = ifelse(is.na(Votaron),0,Votaron))

d_comunas <- read.csv(paste0(main_dir,"Servel/participacion_comuna.csv"))

d <- left_join(d, d_comunas, by="Comuna")

d <- d %>% group_by(REGION, Comuna) %>% mutate(part_comuna2020 = mean(Votaron, na.rm = TRUE))

# Keep only voting in Chile for now
d <- d %>% filter(Pais.Domicilio=="Chile")

###########################################
##### Characteristics
##########################################
col1 = "#541690"
col2 = "#FF4949"
options(scipen=999)
library(scales)

# Edad

ggplot(data = d, aes(x = Edad, color = factor(Votaron, levels = c(0,1), labels = c("No", "Si")),
                     fill = factor(Votaron, levels = c(0,1), labels = c("No", "Si")))) +
  geom_histogram(bins = 83, lwd = 1) +
  xlim(18,100) +
  scale_y_continuous(labels = label_number(suffix = " m", scale = 1e-3)) +
  scale_color_manual(values = c(col1, col2), name = "Votaron 2020?") +
  scale_fill_manual(values = c(alpha(col1, 0.4), alpha(col2, 0.4)), name = "Votaron 2020?") +
  theme_bw()+
  xlab("Edad") + ylab("Num. personas") +
  theme_ipsum_fsc() + #plain
  theme(plot.margin=unit(c(1,1,1,1),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  theme(axis.title.x = element_text(size=18),#margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=18),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size=14),legend.position=c(0.8,0.8),
        legend.title = element_text(size=15),
        legend.text = element_text(size=13),
        title = element_text(size=14))



d_region <- d_comunas %>% group_by(REGION) %>% summarise(vote_region2021_2da = sum(vote_comuna2021_2da),
                                                         inscritos_region2021_2da = mean(inscritos_region2021_2da),
                                                         inscritos_region2009_1era = mean(inscritos_region2009_1era),
                                                         part_region2009_1era = mean(part_region2009_1era),
                                                         part_region2021_2da = vote_region2021_2da/inscritos_region2021_2da)

order_region <- c("Arica Y Parinacota", "De Tarapaca", "De Antofagasta", "De Atacama", "De Coquimbo",
                  "De Valparaiso", "Metropolitana De Santiago","Del Libertador Bdo. O'Higgins", "Del Maule", "De Ñuble", "Del Biobio",
                  "De La Araucania", "De Los Rios", "De Los Lagos", "Aisen Del Gral. Carlos Ibañez", "De Magallanes Y Antartica Ch.")

d_region <- d_region %>% arrange(factor(REGION, levels = order_region))

d_region2 <- data.frame(REGION = c(d_region$REGION, d_region$REGION),
                        n = c(d_region$inscritos_region2021_2da, d_region$vote_region2021_2da),
                        group = c(rep("Inscritos", 16), rep("Votantes", 16)),
                        part = c(rep(d_region$part_region2021_2da, 2))) 

ggplot(data = d_region2, aes(x = factor(REGION, levels = order_region), y = n, color = factor(group),
                     fill = factor(group))) +
  geom_bar(position = "stack",
           stat = "identity", lwd=1) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-3)) +
  scale_color_manual(values = c(col1, col2), name = "Poblacion") +
  scale_fill_manual(values = c(alpha(col1, 0.4), alpha(col2, 0.4)), name = "Poblacion") +
  theme_bw()+
  xlab("Region") + ylab("Num. personas") +
  theme_ipsum_fsc() + #plain
  theme(plot.margin=unit(c(1,1,1,1),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  theme(axis.title.x = element_text(size=18),#margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size=8, angle = 90, hjust = 1),
        axis.title.y = element_text(size=18),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size=14),legend.position=c(0.2,0.8),
        legend.title = element_text(size=15),
        legend.text = element_text(size=13),
        title = element_text(size=14))

d_region2 %>% filter(REGION != "Metropolitana De Santiago") %>%
ggplot(data = ., aes(x = droplevels(factor(REGION, levels = order_region)), y = n, color = factor(group),
                             fill = factor(group))) +
  geom_bar(position = "stack",
           stat = "identity", lwd=1) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-3)) +
  scale_color_manual(values = c(col1, col2), name = "Poblacion") +
  scale_fill_manual(values = c(alpha(col1, 0.4), alpha(col2, 0.4)), name = "Poblacion") +
  theme_bw()+
  xlab("Region") + ylab("Num. personas") +
  theme_ipsum_fsc() + #plain
  theme(plot.margin=unit(c(1,1,1,1),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  theme(axis.title.x = element_text(size=18),#margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size=8, angle = 90, hjust = 1),
        axis.title.y = element_text(size=18),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size=14),legend.position=c(0.2,0.8),
        legend.title = element_text(size=15),
        legend.text = element_text(size=13),
        title = element_text(size=14))


d_region2 %>% filter(group != "Votantes") %>%
ggplot(data = ., aes(x = factor(REGION, levels = order_region), y = part*100)) +
  geom_bar(color = col2, fill = alpha(col2, 0.4), position = "stack",
           stat = "identity", lwd=1) +
  scale_y_continuous(labels = label_number(suffix = " %", scale = 1)) +
  theme_bw()+
  xlab("Region") + ylab("Participacion 2021 (2da Vuelta)") +
  theme_ipsum_fsc() + #plain
  theme(plot.margin=unit(c(1,1,1,1),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  theme(axis.title.x = element_text(size=18),#margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size=8, angle = 90, hjust = 1),
        axis.title.y = element_text(size=18),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size=14),legend.position="none",
        legend.title = element_text(size=15),
        legend.text = element_text(size=13),
        title = element_text(size=14))

# Vote is mandatory, so we make the following assumptions:
# p1: Probability that someone that voted in 2020 votes again. We assume this probability is close to 1
# p2: Probability that someone is a "new voter", meaning didn't vote in 2020 but did vote in 2021 (presidential election, 2nd ballot)
#     We will assume the probability that someone is a new voter at the county level. E.g. (Votos 2021 - Votos 2020)/Total
# p3l: Probability someone doesn't show up, taken from 1-participation when voting was mandatory but inscription voluntary (2009).
#      We assume this is a lower bound, given that people that registered voluntarily are more interested in voting.
# p3h: p3l x 2.

d <- d %>% mutate(p1 = 0.90,
                  p2 = ifelse(part_comuna2021_2da >= part_comuna2020,
                              part_comuna2021_2da - part_comuna2020, 0.01),
                  p3l = 1 - part_region2009_1era,
                  p3h = p3l*2)


d$id <- seq(1, nrow(d))

estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

# Create these aux variables
d$new_voters = 0
d$potential_voters2021 = 0

# With this, we can simulate how voting would look like:

sim = 100

# pred_high <- foreach(
#   s = 1:sim, 
#   .combine = 'cbind',
#   .verbose=TRUE
# ) %do% {
#   set.seed(s)
#   
#   gc()
#   
#   betaparams <- estBetaParams(d$p2[d$Votaron==0], d$p2[d$Votaron==0]/10)
#   
#   p2 <- rbeta(length(d$p2[d$Votaron==0]), shape1 = betaparams$alpha, shape2 = betaparams$beta)
#   
#   d$new_voters[d$Votaron==0] <- rbinom(nrow(d[d$Votaron==0,]), 1, p2)
#   
#   d$potential_voters2021 <- d$Votaron + d$new_voters
#   
#   prob_voting <- rep(0, nrow(d))
#   
#   betaparams2 <- estBetaParams(d$p1[d$potential_voters2021==1], d$p1[d$potential_voters2021==1]/25)
#   
#   prob_voting[d$potential_voters2021==1] = rbeta(length(d$p1[d$potential_voters2021==1]), shape1 = betaparams2$alpha, shape2 = betaparams2$beta)
#   
#   betaparams3 <- estBetaParams(d$p3l[d$potential_voters2021==0], d$p3l[d$potential_voters2021==0]/10)
#   
#   prob_voting[d$potential_voters2021==0] = 1 - rbeta(length(d$p3l[d$potential_voters2021==0]), shape1 = betaparams3$alpha, shape2 = betaparams3$beta)
#   
#   prob_voting[prob_voting<0] <- 0
#   prob_voting[prob_voting>1] <- 1
#   
#   rbinom(nrow(d), 1, prob_voting)
# }


pred_low <- foreach(
  s = 1:sim, 
  .combine = 'cbind',
  .verbose=TRUE
) %do% {
  set.seed(s+sim)
  
  gc()
  
  betaparams <- estBetaParams(d$p2[d$Votaron==0], d$p2[d$Votaron==0]/10)
  
  p2 <- rbeta(length(d$p2[d$Votaron==0]), shape1 = betaparams$alpha, shape2 = betaparams$beta)
  
  d$new_voters[d$Votaron==0] <- rbinom(nrow(d[d$Votaron==0,]), 1, p2)
  
  d$potential_voters2021 <- d$Votaron + d$new_voters
  
  prob_voting <- rep(0, nrow(d))
  
  betaparams2 <- estBetaParams(d$p1[d$potential_voters2021==1], d$p1[d$potential_voters2021==1]/25)
  
  prob_voting[d$potential_voters2021==1] = rbeta(length(d$p1[d$potential_voters2021==1]), shape1 = betaparams2$alpha, shape2 = betaparams2$beta)
  
  betaparams3 <- estBetaParams(d$p3h[d$potential_voters2021==0], d$p3h[d$potential_voters2021==0]/10)
  
  prob_voting[d$potential_voters2021==0] = 1 - rbeta(length(d$p3h[d$potential_voters2021==0]), shape1 = betaparams3$alpha, shape2 = betaparams3$beta)
  
  prob_voting[prob_voting<0] <- 0
  prob_voting[prob_voting>1] <- 1
  
  rbinom(nrow(d), 1, prob_voting)
}

sims_pred_low <- data.frame(sim = 1:100, 
                             participation = colMeans(pred_low))

ggplot(data = sims_pred_low, aes(x = participation)) +
  geom_density(col = "#0C0A3E", lwd = 1) +
  geom_vline(aes(xintercept = mean(sims_pred_low$participation)), lty = 2, lwd = 1.1, col = "#7B1E7A") +
  theme_bw()+
  xlab("Average participation by simulation") + ylab("Density") +
  theme_ipsum_fsc() + #plain
  theme(plot.margin=unit(c(1,1,1,1),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  theme(axis.title.x = element_text(size=18),#margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=18),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size=14),legend.position="none",
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        title = element_text(size=14))
  


d$prob_vote_2022 = rowMeans(pred_low)


ggplot(data = d, aes(x = prob_vote_2022)) +
  geom_density(col = "#0C0A3E", lwd = 1) +
  geom_vline(aes(xintercept = mean(d$prob_vote_2022)), lty = 2, lwd = 1.1, col = "#7B1E7A") +
  theme_bw()+
  xlab("Probability of voting") + ylab("Density") +
  theme_ipsum_fsc() + #plain
  theme(plot.margin=unit(c(1,1,1,1),"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(colour = "dark grey"))+
  theme(axis.title.x = element_text(size=18),#margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=18),#margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size=14),legend.position="none",
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        title = element_text(size=14))

set.seed(100)
d$potential_voter2022 = rbinom(nrow(d), 1, d$prob_vote_2022)

write.csv(d, file = paste0(main_dir,"working/d_2022_potential_voters.csv"))

