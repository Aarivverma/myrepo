rm(list = ls())
library(ggplot2)
library(tidyverse)
library(sp)
library(RColorBrewer)

district_wise <- read_csv("https://data.covid19bharat.org/csv/latest/district_wise.csv", 
                col_types = cols(Migrated_Other = col_skip(), 
                Delta_Confirmed = col_skip(), Delta_Active = col_skip(), 
                 Delta_Recovered = col_skip(), Delta_Deceased = col_skip(), 
                District_Notes = col_skip(), Last_Updated = col_skip()))

district_wise <- district_wise %>% filter(!District %in% c("Unassigned", "Unknown", "Others", "Other State", "Other Region" , "Airport Quarantine" ))
up_today <- district_wise %>% 
  filter(State == "Uttar Pradesh" & District != "Unknown")



setwd("C:/Users/msi/Desktop/work/Spacial_ploting_covid_data in r")
ind1 = readRDS("gadm36_IND_2_sp.rds")

up2 = (ind1[ind1$NAME_1=="Uttar Pradesh",])

up_today$District <- recode(up_today$District, Ayodhya = "Faizabad",
                             Bhadohi = "Sant Ravi Das Nagar",
                             Prayagraj = "Allahabad",
                             Shrawasti = "Shravasti",
                             Siddharthnagar = "Siddharth Nagar" )#new_name = old_name

district_wise_p <- up_today %>% filter(up_today$District %in% ind1@data[["NAME_2"]] )
district_wise_p <- district_wise_p[order(district_wise_p$District),]
#ind2 = readOGR("India_Districts_ADM2_GADM.shp")

#map of India with states coloured with an arbitrary fake data
# ind1$NAME_1 = as.factor(ind1$NAME_1)
# ind1$fake.data = district_wise$Confirmed
# ind1$fake.data1 = district_wise$Deceased
# spplot(ind1,"NAME_1",  col.regions=rgb(0,ind1$fake.data1,0), colorkey=T, main="Indian States")
# spplot(ind1, "fake.data1")


# plotting districts of a State, in this case West Bengal

#spplot(up2,"NAME_1", main = "Uttar pradesh Districts", colorkey =F)

# colouring the districts with some simulated, fake data
up2$NAME_2 = as.factor(up2$NAME_2)
up2$confirmed = district_wise_p$Confirmed
up2$death = district_wise_p$Deceased
up2$active = district_wise_p$Active
mycolours <- brewer.pal(8, "YlOrRd")



lbls <- as.character(up2$NAME_2)
spl <- list('sp.text', coordinates(up2), lbls, cex=.6, col="white")

spplot(up2[,"active"], par.settings = list(axis.line = list(col =      "transparent")), main = "# of Deaths by Covid in UP ", cuts = 7, col = "transparent", col.regions = mycolours)

spplot(up2, c("confirmed","active","death"),  colorkey=list(space = "left", height = 0.4), scales=list(draw = TRUE), sp.layout = spl)
text(up2, up2$NAME_2, cex=0.5)
today<- Sys.Date() 




library(tmap)
library(tmaptools)
tm_shape(up2) + tm_polygons(col = 'active',  cut = 10, title = "Active cases in UP", palette = "Spectral") +
tm_style("white") + 
  #tm_scale_bar(position = c("center", "bottom")) +
  tm_text(c("NAME_2"), size = 0.5) +
  tm_layout(bg.color = "white",
            inner.margins = c(.004,.003, .06, .001), 
            earth.boundary = TRUE, 
            space.color = "grey90") +
  tm_text("NAME_2", size = 0.5) +
  tm_layout(main.title = "No. of Active Cases in Districts of UP today ",
            main.title.position = "center",
            #main.title.color = "Red",
            legend.text.color = "black",
            legend.position =  c("RIGHT", "TOP"), 
            #legend.outside=TRUE
            )
 


# By GG Plot
library(ggplot2)

library(classInt)

# get quantile breaks. Add .00001 offset to catch the lowest value





up2@data$id <- rownames(up2@data)
newup2 <- fortify(up2, region = "id")
newdf <- merge(newup2, up2@data, by = "id")
cnames <- aggregate(cbind(long, lat) ~ NAME_2+active+death+confirmed, data=newdf, FUN=mean) # for labeling names
cnames$NAME_2 <- recode(cnames$NAME_2 , Faizabad = "Ayodhya",
                             "Sant Ravi Das Nagar" = "Bhadohi",
                            )
###Breaks
breaks_qt <- classIntervals(c(min(newdf$death) - .00001,newdf$death), n = 7, style = "quantile")

breaks_qt


myplot <- ggplot() +
  geom_polygon(data = newdf, aes(fill = death, 
                                 x = long, 
                                 y = lat, 
                                 group = group)) +
  ggtitle("# of Deaths by Covid in UP") + 
  theme(plot.title = element_text(hjust = 0.5))
myplot


library(scales)
library(ggmap)
library(viridis)

Myplot <- ggplot() +
  
  geom_polygon(data = newdf, aes(fill = death, 
                                 x = long, 
                                 y = lat, 
                                 group = group)) +
geom_text(data=cnames,aes(long, lat, 
                    label = paste("",NAME_2,"\n",death,"")),
          colour = "blue", size = 3) + #, check_overlap = TRUE
    coord_map() + 
     theme_nothing(legend = TRUE) + 
  ggtitle("# of Death by Covid in UP") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_viridis(option = "D", direction = -1) # option A:D

Myplot




###############GIS Tool
#install.packages("GISTools")
library(GISTools)
myshading = auto.shading(up2$death, n=7,
                         cols=brewer.pal(7, "Blues"))
choropleth(up2, up2$death, main = "# of Deaths in UP")
choro.legend(px = 'bottomleft', sh=myshading, fmt = "%4.1f", cex=0.8)


