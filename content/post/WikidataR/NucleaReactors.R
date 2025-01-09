###https://github.com/TS404/WikidataR
###a lot of useful references to SPARQL learning material
###note glitter package to make SPARQL queries without SPARQL specific knowledge
# trying  SPARQL query with WikidataR
library(WikidataR)
library(WikidataQueryServiceR)

homep <- getwd()
#id of the nuclear power plant 
NPP_id <- find_item("Nuclear Power Plant")

nr <- query_wikidata(
#PREFIX wd: <http://www.wikidata.org/entity/>
#PREFIX wdt: <http://www.wikidata.org/prop/direct/>
'SELECT  ?nuclearReactor ?nuclearReactorLabel ?Capacity ?StatusLabel ?GeoCoordinates ?CountryLabel 
WHERE {
  
  ?nuclearReactor wdt:P31 wd:Q134447;
  wdt:P2109 ?Capacity;
  wdt:P5817 ?Status;
  wdt:P625 ?GeoCoordinates;
  wdt:P17 ?Country.
  SERVICE wikibase:label {bd:serviceParam wikibase:language "en".}
  
}')


write.csv(nr, "NuclearReactors.csv") #downloaded data for map
cat("
 library(tidyverse) # data handling
  library(magrittr) # pipes
  library(cowplot) # combine ggplots
  library(markdown)", # supports coord_sf to define bbox,
file = "libs-blog.R")

source("libs-blog.R")
unlink("libs-blog.R")

default_background_color <- "#FFFFFF"
default_font_color <- "#333333"
default_font_family  <-  "Georgia" #BellTopo Sans does not render in pdf
# define a map theme (no axis, light grid as in timogrossenbacher.ch/2018/03/categorical-spatial-interpolation-with-r/)
#map template
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = default_font_family,
                          color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      # borders and margins
      # plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      # panel.border = element_blank(),
      # panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = default_font_color),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = default_font_color),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = default_font_color,
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      # plot.caption = element_text(size = 7,
      #                             hjust = .5,
      #                             margin = margin(t = 0.2,
      #                                             b = 0,
      #                                             unit = "cm"),
      #                             color = "#939184"),
      ...
    )
}

#prepare map using giscoR
cat("
library(giscoR) # spatial data handling
library(sf) # spatial data handling
library(mapsf) # choropletLayer & legendChoro
library(ggspatial)", # supports coord_sf to define bbox,
file = "libs-Gisco.R")

source("libs-Gisco.R")
unlink("libs-Gisco.R")
#world map in WGS84 as the location ccordinates use this projection
#Equal Earth projection - Greenwich centered epsg 8857 nlot available in giscoR
wm <-gisco_get_countries(year = "2016",epsg = "4326",
                         resolution = "60")

# Nuclear Reactors as sf: extract lat and along from GeoCoordinates string first
nr$Lat <- stringr::str_sub(nr$GeoCoordinates, 
                           start=stringr::str_locate(nr$GeoCoordinates, " ")[,1]+1,
                           end = stringr::str_length(nr$GeoCoordinates)-1)
nr$Long <- stringr::str_sub(nr$GeoCoordinates, start = 7,
                           end = stringr::str_locate(nr$GeoCoordinates, " ")[,1])

nr_sf <- st_as_sf(nr, coords= c("Long", "Lat"), crs = 4326)

#  coord_sf(xlim=range(nr$Long, ylim=nr$Lat)) 
wm_bbox <- c(xlim=range(nr$Long), ylim=range(nr$Lat))
# cleaning
#nr <- nr %>% select(c(2:6,8:9 ))
nr[nr$StatusLabel=="building or structure under construction","StatusLabel"] <- "under construction"
nr[nr$StatusLabel=="proposed building or structure","StatusLabel"] <- "planned"


cls <- c("cancelled" = "#3ea5af", "planned" = "#ff9933", "under construction"="#f6bb88",
         "in use" ="#f5be44",  "decommissioned"= "#6ebcc3")

lbs <- c("decommissioned", "in use","under construction", "planned", "cancelled")



NRmap <- ggplot(wm) +
  # first add world bckgrnd  
  geom_sf(fill= "#ffffff",
          colour = "#aaabbb",
          size = 0.3) +
  coord_sf(xlim=range(nr$Long), ylim=range(nr$Lat)) +
  geom_sf(
    data = nr_sf,
    mapping = aes(
      fill = "#ffffff",
      alpha = 0.6,
      colour = StatusLabel, 
      size = Capacity),
    show.legend = T) +
  scale_colour_manual(
    labels = lbs,
    breaks = lbs,
    values = cls,
    name = "Status",
    guide = guide_legend(
      order = 1, 
      direction = "horizontal",
      title.position = "top"
    )) + 
  scale_size(
    guide = guide_legend(
      order = 2, 
      direction = "horizontal",
      title.position = "top"
    )) +
   guides(fill = "none") +
   guides(alpha = "none") +
  # add titles
  labs(x=NULL,
       y=NULL,
       title = "Nuclear reactors by status and capacity",
       subtitle = "wikidata, as of january 2025") +

  #caption = default_caption
  # adds theme
  theme_map() +
  #theme(legend.position = "bottom")  +
  theme(
    legend.background = element_blank(),
    legend.position = c(.3, .1))

ggsave("NuclearReactors.png", width = 8, height = 6, units = "in")
