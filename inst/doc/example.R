## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(max.print=50)
data.table::setDTthreads(1)

## -----------------------------------------------------------------------------
library(efdm)

## ----setup, warning = FALSE, message = FALSE----------------------------------
library(dplyr)
library(ggplot2)
library(sf)

## -----------------------------------------------------------------------------
data(example)

## -----------------------------------------------------------------------------
actprob <- example$actprob
actprob

## -----------------------------------------------------------------------------
transprobs_ff <- expand.grid(vol0=1:15, age0=1:35, vol1=1, age1=1, prob=1)
transprobs_ff

## ----ff-----------------------------------------------------------------------
ff <- define_activity("ff", c("vol", "age"), transprobs_ff)

## ----pairdata-----------------------------------------------------------------
example$noman_pairs

## ----statespace---------------------------------------------------------------
statespace <- actprob %>% select(c(region, soil, sp, vol, age))
statespace

## -----------------------------------------------------------------------------
transprobs_noman <- estimatetransprobs(c("vol", "age"), # the dynamic variables of the activity
                     example$noman_pairs,  # pair data for no management
                     statespace=statespace,
                     factors=c("soil"), # Information for different soil types
                                        # is used with smaller weight
                     by=c("region","sp"), # Separate estimation for each
                                          # region and species
                     prior=prior_grow("age"))

## ----noman--------------------------------------------------------------------
noman <- define_activity("noman", c("vol", "age"), transprobs_noman)

## -----------------------------------------------------------------------------
transprobs_noman %>%
  arrange(vol0, age0, soil, region, sp, vol1, age1)

## ----thin---------------------------------------------------------------------
transprobs_thin <- estimatetransprobs(c("vol", "age"),
                     example$thin_pairs, # pair data for thinning
                     statespace=statespace,
                     factors=c("soil"),
                     by=c("region","sp"),
                     prior=prior_grow("age"))
thin <- define_activity("thin", c("vol", "age"), transprobs_thin)

## -----------------------------------------------------------------------------
state0 <- example$initial_state
state0

## -----------------------------------------------------------------------------
activities <- list(noman, thin, ff)
states1 <- runEFDM(state0, actprob, activities, 20)

## -----------------------------------------------------------------------------
states1 %>%
  arrange(soil, region, sp, vol, age, time, activity)

## -----------------------------------------------------------------------------
example$vol_coef %>%
  arrange(vol, sp, species)

## -----------------------------------------------------------------------------
example$drain_coef %>%
  arrange(-vol, sp, assort, activity)

## -----------------------------------------------------------------------------
example$income_coef

## -----------------------------------------------------------------------------
states1 <- states1 %>% mutate(time = factor(2016 + time*5))

## -----------------------------------------------------------------------------
volume <- merge(states1, example$vol_coef) %>%
  mutate (volume=area*volume) %>% #area is multiplied with m3/ha volume the get total volume in m3
  filter(species != "all")
ggplot(volume) +
  scale_fill_viridis_d(end = 0.9) +
  geom_bar(aes(x=time, weight=volume/1000000000, fill=species)) +
  labs(y=NULL,title=expression(paste("Growing stock, bil.",m^3)), x="Year", fill="") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = -90))

## -----------------------------------------------------------------------------
states1$ageclass <- cut(states1$age, breaks=c(0,10,20,30,35), include.lowest = TRUE, 
                        #labels=c("0-50","51-100","101-150","150+"))
                        labels=c("-50","-100","-150","150+"))
states1$region <- factor(states1$region, labels = c("South", "Middle", "North"))
ggplot(subset(states1, time %in% c(2016,2066,2116))) +
  geom_bar(aes(x=ageclass, weight=area/1000000, fill=region)) +  
  scale_fill_viridis_d(end = 0.9) +
  facet_grid(cols=vars(time)) +
  labs(y=NULL, title="Area, mill.ha", x="Ageclass", fill=NULL) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = -90))

## -----------------------------------------------------------------------------
euro <- merge(example$drain_coef, example$income_coef) %>% mutate(euro = euro*drain)
removal <- merge(states1, euro) %>% mutate(income=euro*area)

ggplot(subset(removal,!time %in% c(2116))) + 
  geom_bar(aes(x=time, weight=income/5000000000, fill=assort)) +
  scale_fill_viridis_d(end = 0.9) +
  labs(y=NULL,title = "Income, bil.€/year", x="5-year intervals", fill=NULL ) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = -90))

## -----------------------------------------------------------------------------
transprobs_ff_age_species <- statespace %>%
  select(vol0=vol, age0=age, sp0=sp, region) %>%
  unique %>%
  group_by(region, sp0, vol0, age0) %>%
  summarize(data.frame(vol1=1, age1=1, sp1=c('other','spruce'),
                       prob=case_when(
                         sp0=='other' && region=='South' ~ c(1, 0),
                         sp0=='other' && region=='Middle' ~ c(1, 0),
                         sp0=='other' && region=='North' ~ c(0.8, 0.2), #in other dominated forest
                                        # in northern boreal vegetation zone 20% of final felled area
                                        # change to spruce dominated forest
                         sp0=='spruce' && region=='South' ~ c(0.3, 0.7),
                         sp0=='spruce' && region=='Middle' ~ c(0.2, 0.8),
                         sp0=='spruce' && region=='North' ~ c(0, 1))))
ff_age_species <- define_activity("ff", c("vol", "age", "sp"), transprobs_ff_age_species)

## -----------------------------------------------------------------------------
activities2 <- list(noman, thin, ff_age_species)
states2 <- runEFDM(state0, actprob, activities2, 20)

## -----------------------------------------------------------------------------
# Compute proportion of spruces in each time and region
prop_spruces <- states2 %>%
  group_by(region, time) %>%
  summarise(proportion = 100*sum((sp=="spruce")*area)/sum(area)) %>%
  filter(time %in% c(0, 10, 20)) %>%
  mutate(time = 2016 + 5*time)

# Add bio-geographical regions (MetsaKasvVyoh provided in efdm package) to the data
prop_spruces <- merge(MetsaKasvVyoh, prop_spruces)
prop_spruces %>% ggplot() +
  geom_sf(aes(fill=proportion)) +
  facet_wrap(vars(time)) +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  labs(fill="Proportion" ,title="Proportion of spruce dominated forests, %") +
  guides(fill = guide_colourbar(frame.colour = "grey20",ticks.colour = "grey20")) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = -90))

## -----------------------------------------------------------------------------
statespace3 <- statespace
statespace3$landuse <- "forest"
agriculture <- unique(statespace3 %>% mutate(sp=0, vol=0, age=0, landuse="agriculture"))
other <- unique(statespace3 %>% mutate(soil=0, sp=0, vol=0, age=0, landuse="other"))
statespace3 <- rbind(statespace3, agriculture, other)

## -----------------------------------------------------------------------------
transprobs_defor_to_agri <- statespace3 %>%
  filter(landuse=="forest") %>%
  select(vol0=vol, age0=age, sp0=sp,landuse0=landuse) %>%
  unique %>%
  mutate(vol1=0, age1=0, sp1=0, landuse1="agriculture", prob=1)
defor_to_agri <- define_activity("defor_to_agri",
                                 c("vol", "age", "sp", "landuse"),
                                 transprobs_defor_to_agri)

## -----------------------------------------------------------------------------

transprobs_defor_to_other <- statespace3 %>%
  filter(landuse=="forest") %>%
  select(soil0=soil, vol0=vol, age0=age, sp0=sp,landuse0=landuse) %>%
  unique %>%
  mutate(soil1=0, vol1=0, age1=0, sp1=0, landuse1="other", prob=1)
defor_to_other <- define_activity("defor_to_other",
                                  c("soil", "vol", "age", "sp", "landuse"),
                                  transprobs_defor_to_other)

## -----------------------------------------------------------------------------
transprobs_aff <- statespace3 %>%
  filter(landuse=="agriculture") %>%
  select(soil, vol0=vol, age0=age, region, sp0=sp, landuse0=landuse) %>%
  mutate(vol1=1, age1=1, sp1="other", landuse1="forest", prob=1)
transprobs_aff <- rbind(transprobs_aff %>% mutate(sp1="spruce", prob=0.5),
                        transprobs_aff %>% mutate(sp1="other", prob=0.5))
affor <- define_activity("affor",
                         c("vol", "age", "sp", "landuse"),
                         transprobs_aff)

## -----------------------------------------------------------------------------
donothing <- define_activity("donothing", character())
activities3 <- list(noman, thin, ff, defor_to_other, defor_to_agri, affor, donothing)

## -----------------------------------------------------------------------------
state03 <- state0
state03$landuse <- "forest"
state03 <- rbind(state03,  agriculture %>% mutate(area=rep(c(1552000/2,997000/2,76000/2),each=2)),
                 other %>% mutate(area=c(721000,507000,112000)))

## -----------------------------------------------------------------------------
actprob3 <- actprob
actprob3$landuse <- "forest"
# Define probabilities for new activities
actprob3$defor_to_other <- 0.0002
actprob3$defor_to_agri <- 0.00025
actprob3$affor <- 0
actprob3$donothing <- 0
# Normalise probabilities after adding new activities
actnames <- c("noman", "thin", "ff", "defor_to_other", "defor_to_agri", "affor", "donothing")
actprob3[actnames] <- actprob3[actnames]/rowSums(actprob3[actnames])
# Define activity probabilities also for agriculture and other land uses
actprob3 <- rbind(actprob3, 
        agriculture %>% mutate(thin=0, ff=0, noman=0, defor_to_other=0, defor_to_agri=0, affor=0.0005, donothing=0.9995),
        other %>% mutate(thin=0, ff=0, noman=0, defor_to_other=0, defor_to_agri=0, affor=0, donothing=1))

## -----------------------------------------------------------------------------
states3 <- runEFDM(state03, actprob3, activities3, 20)

## -----------------------------------------------------------------------------
#pie charts of land use areas
LUareas <- states3 %>%
  group_by(time, landuse) %>%
  summarise(area = sum(area)) %>%
  ungroup()
LUareas <- LUareas %>%
  mutate(time = factor(time, labels = seq(2016,2116,5)))
totalarea <- sum(state03$area)
LUareas %>% filter(time %in% c("2016","2066","2116")) %>%
  mutate(area = area/totalarea) %>%
  ggplot() +
  geom_bar(aes(x="",y=area, fill=landuse), stat="identity", width=1, color = "white") +
  scale_fill_viridis_d(end = 0.9) +
  coord_polar("y", start=0) +
  facet_wrap(vars(time)) +
  labs(y=NULL,fill="Land use", x=NULL)+
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  geom_text(aes(x="",y = rep(c(0.03, 0.33, 0.95), 3), label = round(area*100,1)),
            nudge_x=0.25, size=2.25,
            color=rep(c("grey20","white","white"), 3), fontface="bold") +
  theme(legend.position = "bottom")

