# PPFP Decomposition Graphic
# Kristin Bietsch, PhD
# Avenir Health
# June 26, 2020


require(xlsx)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

setwd("C:/Users/KristinBietsch/Files/Track20/PAA2018/PPFP")
data <- read.csv("PPFP Data.csv")

###########################################################
# decomposing 6 month ppfp

data.2mr<- data %>% filter( !is.na(most_recent)) %>%
                select(most_recent,  Country,   del_Fac, del_home,   ppfp_6_all,   ppfp_6_fac,   ppfp_6_home)  %>%
                rename(delFac=del_Fac, delhome=del_home,   ppfp6all=ppfp_6_all,   ppfp6fac=ppfp_6_fac,   ppfp6home=ppfp_6_home )


data.2mr_wide <- data.2mr %>% gather(Variable, Value, delFac:ppfp6home) %>% mutate(Variable=paste(Variable, most_recent, sep=".")) %>% select(-most_recent) %>% spread(Variable, Value)
 
data.2mr_wide <- data.2mr_wide %>% mutate(place.2= (delFac.2-delFac.1) * ((ppfp6fac.2+ppfp6fac.1)/2) + (delhome.2-delhome.1) * ((ppfp6home.2+ppfp6home.1)/2),
                                          rates.2= (ppfp6fac.2-ppfp6fac.1)*((delFac.2 + delFac.1)/2) + (ppfp6home.2 - ppfp6home.1) * ((delhome.2 + delhome.1)/2)   )




graphic.data <- data.2mr_wide %>%  mutate(place.1= 0, rates.1=0, ppfp6all.2=ppfp6all.1) %>% 
  select( Country, ppfp6all.1, ppfp6all.2, place.1, rates.1, place.2, rates.2) %>% 
  gather(Variable, Value, ppfp6all.1:rates.2) %>% separate(Variable, c("Group", "Survey")) %>%
  mutate(Value=Value*100)



ggplot(graphic.data, aes(x=Survey, y=Value, fill=factor(Group, levels=c("place", "rates", "ppfp6all")))) + 
  geom_bar(stat="identity")+ 
  labs(x = "", y="", fill="") +
  ggtitle("Family Planning Use at 6 Month Post Partum")+
  scale_fill_discrete(breaks=c("ppfp6all", "place", "rates" ),
                      labels=c("PPFP Earlier Survey", "Change: Place", "Change: Rates")) +
  facet_grid(cols = vars(Country))  +
  theme_bw() +
  theme(legend.position="bottom") 
