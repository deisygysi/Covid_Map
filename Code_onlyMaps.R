########################
### Author: Deisy Gysi <deisy.ccnr@gmail.com>
### Date: Oct 16 2020
### Description: Plot infected cases of COVID-19
########################


### COVID Projections
require(dplyr)
require(RCurl)
require(ggplot2)
require(ggthemes)
require(magrittr)
require(tidyr)
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

urlfile <-'https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv'
x <- getURL(urlfile, ssl.verifypeer = FALSE)
COVID <- read.csv(textConnection(x))

C_c = 100 #min cases
Conf = COVID %>% dplyr::select(Country.Region, Date, Confirmed)
Conf = aggregate(Conf$Confirmed, 
                 by=list(Conf$Date, Conf$Country.Region), sum)
Conf_n = subset(Conf, Conf$x > C_c)
names(Conf_n) = c("date", "country", "Confirmed")

Conf_n$t = 1

Conf_n %<>%
  group_by(country) %>%
  mutate(ticker = cumsum(t))

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

Conf = COVID %>% dplyr::select(Country.Region, Date, Confirmed, Deaths)%>% 
  group_by(Country.Region, Date) %>%
  filter(Confirmed > C_c) %>%
  summarise(., sum_Death = sum(Deaths), sum_Conf = sum(Confirmed)) %>%
  mutate(taxa = sum_Death/sum_Conf) %>%
  mutate(t = 1) %>%
  group_by(Country.Region) %>%
  mutate(ticker = cumsum(t))

desc = Conf %>% 
  group_by(Country.Region) %>% 
  summarise(., max = max(taxa), median = median(taxa), avr = mean(taxa), 
            Conf = max(sum_Conf), Deaths = max(sum_Death)) %>%
  arrange(., desc(median))


names(desc)[1] = "name"
desc$name = ifelse(desc$name == "US", "United States", as.character(desc$name))
desc$name = ifelse(desc$name == "Czechia", "Czech Rep.", as.character(desc$name))
desc$name = ifelse(desc$name == "Bosnia and Herzegovina", "Bosnia and Herz.", as.character(desc$name))
desc$name = ifelse(desc$name == "Congo (Brazzaville)", "Congo", as.character(desc$name))
desc$name = ifelse(desc$name == "Congo (Kinshasa)", "Congo", as.character(desc$name))

x = dplyr::left_join(world, desc)
subset(x$name, is.na(x$avr))
x$`%` <- x$max*100

x$Pop_Aff = (x$Conf/x$pop_est)*100


ggplot(x) +
  aes(fill = Pop_Aff, col = Pop_Aff) +
  geom_sf(size = 1L) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  scale_color_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "%. Populacao infectada por COVID-19",
       caption = "fonte de dados: John Hopkings") 
