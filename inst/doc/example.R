## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning = FALSE, message = FALSE----------------------------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(efdm)
data(example)
theme_set(theme(legend.position = "bottom", axis.text.x = element_text(angle = -90)))

## ----statespace---------------------------------------------------------------
statespace <- example$actprob %>% select(-c(thin, ff, noman))
head(statespace)

## ----pairdata-----------------------------------------------------------------
head(example$noman_pairs)

## ----noman--------------------------------------------------------------------
act <- define_activity("noman", c("vol", "age"))
act <- build_statespace(act, statespace,
                        factors=c("soil"), by=c("region","sp"))
noman <- estimatetransprobs(act, example$noman_pairs, prior_grow("age"))

## ----thin---------------------------------------------------------------------
act <- define_activity("thin", c("vol", "age"))
act <- build_statespace(act, statespace,
                        factors=c("soil"), by=c("region","sp"))
thin <- estimatetransprobs(act, example$thin_pairs, prior_grow("age"))

## ----ff-----------------------------------------------------------------------
ff <- define_activity("ff", c("vol", "age"))
transprobs(ff) <- unique(statespace %>% select(vol0=vol, age0=age)) %>% mutate(vol1=1, age1=1, prob=1)
head(transprobs(ff))

## -----------------------------------------------------------------------------
activities <- list(noman, thin, ff)
state0 <- example$initial_state
actprob <- example$actprob
head(actprob)

## -----------------------------------------------------------------------------
states1 <- runEFDM(state0, actprob, activities, 20)
head(states1)

## -----------------------------------------------------------------------------
head(example$vol_coef)

## -----------------------------------------------------------------------------
head(example$drain_coef)

## -----------------------------------------------------------------------------
head(example$income_coef)

## -----------------------------------------------------------------------------
states1 <- states1 %>% mutate(time = factor(2016 + time*5))

## -----------------------------------------------------------------------------
tilstate<-merge(states1, example$vol_coef) %>% mutate (volume=area*volume)
ggplot(subset(tilstate,species!='all')) + scale_fill_viridis_d(end = 0.9) +
  geom_bar(aes(x=time, weight=volume/1000000000,fill=species)) + 
  labs(y=NULL,title=expression(paste("Growing stock, bil.",m^3)), x="Year", fill="")

## -----------------------------------------------------------------------------
states1$ageclass <- cut(states1$age, breaks=c(0,10,20,30,35), include.lowest = TRUE, 
                        #labels=c("0-50","51-100","101-150","150+"))
                        labels=c("-50","-100","-150","150+"))
states1$region <- factor(states1$region, labels = c("South", "Middle", "North"))
ggplot(subset(states1, time %in% c(2016,2066,2116))) +
  geom_bar(aes(x=ageclass, weight=area/1000000, fill=region)) +  
  scale_fill_viridis_d(end = 0.9) +
  facet_grid(cols=vars(time)) + labs(y=NULL, title="Area, mill.ha", x="Ageclass", fill=NULL)

## -----------------------------------------------------------------------------
euro<- merge(example$drain_coef, example$income_coef) %>% mutate(euro = euro*drain)
removal <- merge(states1, euro) %>% mutate(income=euro*area)

ggplot(subset(removal,!time %in% c(2116))) + 
  geom_bar(aes(x=time, weight=income/5000000000, fill=assort)) +
  scale_fill_viridis_d(end = 0.9) +
  labs(y=NULL,title = "Income, bil.€/year", x="5-year intervals", fill=NULL )

## -----------------------------------------------------------------------------
ff_age_species <- define_activity("ff", c("vol", "age", "sp"))
transprobs(ff_age_species) <- unique(filter(statespace) %>% 
                                       select(vol0=vol, age0=age, sp0=sp, region)) %>% 
  group_by(region, sp0, vol0, age0) %>%
  summarize(data.frame(vol1=1, age1=1, sp1=c('other','spruce'),
                       prob=case_when(sp0=='other' && region=='South' ~ c(1, 0),
                                      sp0=='other' && region=='Middle' ~ c(1, 0),
                                      sp0=='other' && region=='North' ~ c(0.8, 0.2),
                                      sp0=='spruce' && region=='South' ~ c(0.3, 0.7),
                                      sp0=='spruce' && region=='Middle' ~ c(0.2, 0.8),
                                      sp0=='spruce' && region=='North' ~ c(0, 1))))

## -----------------------------------------------------------------------------
activities2 <- list(noman, thin, ff_age_species)
states2 <- runEFDM(state0, actprob, activities2, 20)

## -----------------------------------------------------------------------------
statespace3 <- statespace
statespace3$landuse <- "forest"
agri <- unique(statespace3 %>% mutate(sp=0, vol=0, age=0, landuse="agri"))
other <- unique(statespace3 %>% mutate(soil=0, sp=0, vol=0, age=0, landuse="other"))
statespace3 <- rbind(statespace3, agri, other)

## -----------------------------------------------------------------------------
defor_to_agri <- define_activity("defor_to_agri", 
                                          c("vol", "age", "sp", "landuse"))
transprobs(defor_to_agri) <- unique(filter(statespace3, landuse=="forest") %>% 
                                               select(vol0=vol, age0=age, sp0=sp,landuse0=landuse) %>% 
                                               mutate(vol1=0, age1=0, sp1=0, landuse1="agri", prob=1))

## -----------------------------------------------------------------------------
defor_to_other <- define_activity("defor_to_other", 
                                        c("soil", "vol", "age", "sp", "landuse"))
transprobs(defor_to_other) <- unique(filter(statespace3, landuse=="forest") %>% 
                                             select(soil0=soil, vol0=vol, age0=age, sp0=sp,landuse0=landuse) %>% 
                                             mutate(soil1=0, vol1=0, age1=0, sp1=0, landuse1="other", prob=1))

## -----------------------------------------------------------------------------
affor <- define_activity("affor", c("vol", "age", "sp", "landuse"))
aff <- filter(statespace3, landuse=="agri") %>% 
  select(soil, vol0=vol, age0=age, region, sp0=sp, landuse0=landuse) %>% 
  mutate(vol1=1, age1=1, sp1="other", landuse1="forest", prob=1)
aff <- rbind(aff %>% mutate(sp1="spruce", prob=0.5), 
             aff %>% mutate(sp1="other", prob=0.5))
transprobs(affor) <- aff

## -----------------------------------------------------------------------------
donothing <- define_activity("donothing", character())
activities3 <- list(noman, thin, ff, defor_to_other, defor_to_agri, affor, donothing)

## -----------------------------------------------------------------------------
state03 <- state0
state03$landuse <- "forest"
state03 <- rbind(state03,  agri %>% mutate(area=rep(c(1552000/2,997000/2,76000/2),each=2)),
                 other %>% mutate(area=c(721000,507000,112000)))

## -----------------------------------------------------------------------------
actprob3 <- actprob
actprob3$defor_to_other <- 0.0002
actprob3$defor_to_agri <- 0.00025
actprob3$affor <- 0
actprob3$landuse <- "forest"
actprob3$donothing <- 0
actnames <- setdiff(names(actprob3), names(state03))
actprob3[actnames] <- actprob3[actnames]/rowSums(actprob3[actnames])
actprob3 <- rbind(actprob3, 
        agri %>% mutate(thin=0, ff=0, noman=0, defor_to_other=0, defor_to_agri=0, affor=0.0005, donothing=0.9995),
        other %>% mutate(thin=0, ff=0, noman=0, defor_to_other=0, defor_to_agri=0, affor=0, donothing=1))

## -----------------------------------------------------------------------------
states3 <- runEFDM(state03, actprob3, activities3, 20)

