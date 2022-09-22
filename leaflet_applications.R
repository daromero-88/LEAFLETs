######EXAMPLE OF LEAFLET APPLICATION#######

#By Daniel Romero-Alvarez


#PACKAGES AND LIBRARIES--------------------------

#mapping and extra dependencies
library (devtools)
library(raster)
library(sp)
library(rgeos)
library (rgdal)
library (maptools)
library(mapdata)

#dynamic maps with leaflet: 
library (leaflet) 

#WORKING DIRECTORY-------------------------------

setwd ('/Users/daniel/Documents/GitHub/LEAFLETs')


#MAPS AND FILES----------------------------------

#in built world map: 
data("wrld_simpl", package = "maptools")
plot (wrld_simpl)
WGS84 = crs(wrld_simpl) # geographic projection

#subsetting for a particular country: 
wrld_simpl@data

pr = subset (wrld_simpl, NAME == 'Peru' )
plot (pr)


#Leptospirosis in Ecuador: 
lp1 = readOGR ('./provincias_lepto2/provincias.shp')


#MAP FOR VISUALIZATION--------------------------

#' Leaflet works with providers that allow different types of 
#' visualization of the spatial objects. Multiple providers can be found 
#' in the website: http://leaflet-extras.github.io/leaflet-providers/preview/index.html
#' Some of them are of free-use, others need registration: https://github.com/leaflet-extras/leaflet-providers

#Map of Ecuador 
leaflet(lp1) %>% #add the corresponding shapefile 
  addProviderTiles("OpenStreetMap.Mapnik") %>% #select provider 
  setView(lat=-1.83, lng=-78.18, zoom= 7) #positiong of the map ans zoom 
  

#Map of Peru   
leaflet(pr) %>% #add the corresponding shapefile 
  addProviderTiles("Esri.WorldImagery") %>% #select provider 
  setView(lat=-12, lng=-75, zoom= 4.5) #positiong of the map ans zoom 
  

#CHLOROPLETH MAP IN LEAFLET--------------------------

#' Establish the breaks of the chloropleth by examining the variable
#' using a histogram 

colnames(lp1@data) #variable names inside the shapefile 

#histogram 
lp1@data %>% 
  ggplot( aes(x=as.numeric(lepsum_def))) + 
  geom_histogram(bins=20, fill='#69b3a2', color='white') +
  xlab("Leptospirosis cases") + 
  ggtitle ('Var = lepsum_def') +
  theme_bw()

#selecting division bins: 
manual_bins = c(0,1,50, 150,200,250,Inf) 

title3 = 'Leptospirosis' #adding a title

#establishing a color pallete following the division bins: 

con_pal1 = colorRampPalette(c('#efedf5', '#756bb1'))

mypalette2 = colorBin(palette= con_pal1(length(manual_bins)), #**
                      domain=lp1@data$lepsum_def, 
                      na.color="transparent", 
                      bins=manual_bins)

#' ** you can select a full color palette from the default ones 
#' from the corresponding package colorRampPalette, in that case 
#' you only write the ramp in the argument palette: palette = 'YlOrRd'
#' https://www.r-bloggers.com/2015/04/create-colorful-graphs-in-r-with-rcolorbrewer-and-plotly/


#'Text that will appear when moving the mouse over the map features
#'you can add any particular line of text here: 

text3 = paste ('Province: ', lp1@data$DPA_DESP, "<br/>", 
               'Cases: ', lp1@data$lepsum_def, "<br/>",
               'Random: Great work!', 
               sep = '') %>%
  lapply(htmltools::HTML)

#code for the map itself 
leaflet(lp1) %>% 
  addProviderTiles("OpenStreetMap.Mapnik")  %>% #select appropriate provider
  setView(lat=-1.83, lng=-78.18, zoom= 7) %>% #select correct view
  addPolygons(fillColor = ~mypalette2(lepsum_def), 
              weight = 1, color = 'gray', fillOpacity = 0.7, 
              stroke = T, label = text3, 
              labelOptions = labelOptions(noHide = F, textsize = "10px")) %>% 
  addMiniMap(tiles = providers$Esri.WorldStreetMap,
             toggleDisplay = TRUE)  %>%
  addControl(title3, position = "topleft") %>%
  addLegend(pal=mypalette2, values=~lepsum_def, 
            opacity=0.9, title = "Leptospirosis cases", position = "bottomleft" )

#' KEY DETAILS: 
#' Notice the argument label that allows to add the text of each feature
#' as defined in the corresponding object. You control individual characteristics
#' of these labels with the labelOptions argument. 
#' The only way to add titles in this maps is with addControl 
#' Notice the different opacities of the shapefile itslef and the legend both 
#' controlled with their corresponding arguements. 
#' Also, see the mini-map added with the corresponding argument and using 
#' a different provider than the main map. 

#' SAVING THE MAP: 
#' In order to save the map to share by email or other ways, you need
#' to export it manually using the Save as Web Page option in the 
#' export section of R studio. An html file will be created with the assigned 
#' name. If you are following the tutorial exactly you will have a map that 
#' it is identical to the one attached in this repository.



#OCCURRENCES IN LEAFLET--------------------------

#' First, I will create random occurrences in the Peruvian map: 
#' 

pt_random = spsample (pr, n = 21, 'random') 

plot (pr)
points (pt_random, pch= 3, cex = 0.7, col= 'red')

#' Add categories positive = 1 and negativ = 2 to the random points for 
#' visualization purposes: 

dff = as.data.frame (cbind (pt_random@coords, pos_neg= sample (1:2, size = dim(pt_random@coords)[1], replace = T)))
dff$pos_neg2 = ifelse(dff$pos_neg==1, 'Positive', 'Negative')

#Mapping the points in leaflet: 
#' Beacuase the points are already defined in the space, 
#' we don't need any particular shapefile to delimit them 
#' they will be plotted correctly in according to their coordinate

title4 = 'Diagnosis'

#' colors for two categories, red and blue

pal4 = colorFactor(c(c('#91bfdb', '#d7191c')), 
                   domain = unique(dff$pos_neg2)) #color palette for legend: negative/positive

#labels for each point: 
text_map = paste ('Longitude: ', round (dff$x, digits = 3), "<br/>",
                  'Latitude: ', round (dff$y, digits = 3), "<br/>",
                  'Pos/Neg: ', dff$pos_neg , "<br/>",
                  'Positive/Negative: ', dff$pos_neg2, "<br/>",
                  sep = '') %>%
  lapply(htmltools::HTML)


#' We can assign the entire leaflet into an object that should be 
#' called in order to be displayed

tissues = leaflet(dff) %>% 
  addProviderTiles("Esri.WorldStreetMap") %>%
  #addTiles() %>%
  #setView(lng = -99.544729, lat = 39.379578, zoom = 17) %>% 
  addCircleMarkers(lng = ~x, lat = ~y, 
                   popup = c(~as.character(pos_neg2)),
                   radius = 3, color = ~pal4(pos_neg2),
                   stroke = T, fillOpacity = 0.8, label = text_map, 
                   labelOptions = labelOptions(noHide = F, textsize = "10px")) %>% 
  addMiniMap(tiles = providers$Esri.WorldStreetMap,
             toggleDisplay = TRUE, position = 'bottomleft')  %>%
  addControl(title4, position = "topleft") %>%
  addLegend(pal=pal4, values=~pos_neg2, 
            opacity=0.9, title = "Diagnosis", position = "topright" ) 

#' KEY DETAILS: 
#' Because the points are represented as spatial coordinates
#' we don't need to establish any particular shapefile to depict the 
#' points. 
#' Notice the utilization of the Esri.WorldStreetMap provider which is super nice
#' Be aware of the definition of the color palette as factors because instead of a ramp 
#' we need to use exactly the two colors defined because we are using dichotomic
#' categories. This can be incremented according to the categories to be depicted. 
#' 

#' SAVING THE MAP: 
#' Same as before. The map should be exported as a Save as Web Page object. 
#' It has been added also to this repository. 




