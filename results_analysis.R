library(tidyverse)
library(benford.analysis)
library(devtools)
## devtools::install_github("kkalininMI/EFToolkit")
## library(EFToolkit)
library(rgdal)
library(sf)
library(lme4)

setwd("D:\\Dropbox\\My projects\\Elections\\Azerbaijan")

az_vote <- read_csv("az_vote.csv")
names(az_vote) <- gsub('\r|\n|\\s', '', iconv(names(az_vote), to='ASCII', sub=''))

precint_data <- az_vote %>%
  group_by(Sekidairsi, Mntq, candidate)%>%
  summarise_all(funs(sum))

precint_data_nonvoting <- distinct(precint_data, Sekidairsi, Mntq, .keep_all = TRUE)
precint_data_nonvoting$turnout <- precint_data_nonvoting$`4`/precint_data_nonvoting$`1`
precint_data_nonvoting$precid <- precint_data_nonvoting$Sekidairsi*1000+precint_data_nonvoting$Mntq

ggplot(precint_data_nonvoting, aes(turnout))+
  # geom_histogram(aes(y = ..density..))+
  geom_density(aes(y=..density..), colour="blue")+
  stat_function(fun = dnorm, 
                args = c(mean = mean(precint_data_nonvoting$turnout), sd = sd(precint_data_nonvoting$turnout)),
                col = "tomato")+
  theme_light()+
  labs(title="Distribution of Turnout values per precinct",
       x="Turnout",
       y="Density",
       caption = "2020 elections to Azerbaijan's National Assembly")

district_max <- az_vote %>%
  group_by(Sekidairsi, candidate)%>%
  summarize(votes=sum(votes))%>%
  filter(votes == max(votes)) %>%
  arrange(Sekidairsi, candidate, votes)

benford_turnout <- benford(precint_data_nonvoting$`5`)
plot(benford_turnout)
suspects <- getSuspects(benford_turnout, precint_data_nonvoting)
suspects

benford_turnout

benford_votes <- benford(precint_data_nonvoting$votes)
plot(benford_votes)
a <- suspectsTable(benford_votes)

az_coord <- read_csv("az_vote_addr.csv")%>%
  mutate(precid = district*1000+id)%>%
  left_join(precint_data_nonvoting, by="precid")

az_shp <- readOGR("gadm36_AZE_shp\\gadm36_AZE_2.shp", stringsAsFactors = F)


ggplot() + 
  geom_polygon(data = az_shp,
               aes(x = long, y = lat, group = group),
               colour = "black", fill = NA)+
  geom_point(data = az_coord, aes(x = x, y = y, color=turnout),
             size = 1, 
             shape = 16)+
  scale_color_continuous(low="#488f31", high="#de425b")+
  theme_void()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  labs(title="Precinct-level turnout",
       caption = "2020 elections to Azerbaijan's National Assembly")

az_webcams <- read_csv("az_webcams.csv") %>%
  gather(distr, precinct, -district)%>%
  filter(!is.na(precinct))%>%
  subset(select=c(district, precinct))%>%
  mutate(precid = district*1000+precinct)

precint_data_nonvoting <- precint_data_nonvoting %>%
  left_join(az_webcams, by="precid")%>%
  mutate(webcam=case_when(!is.na(district) ~ 1,
                          T ~ 0))

fraud_mod <- lmer(turnout~webcam+(1|Sekidairsi), data=precint_data_nonvoting)
summary(fraud_mod)

fixef(fraud_mod)
