# EarthQuakeAnalyzer

[![Build Status](https://travis-ci.org/DanielAyllon/EarthQuakeAnalyzer.svg?branch=master)](https://travis-ci.org/DanielAyllon/EarthQuakeAnalyzer)

I get two important errors; hope someone can help me with this:
1. With the Geoms: when I use EQ_PRIMARY to define the size of the circles I get an error that disappears when I get rid of the size parameter:
Error in check.length(gparname) : 
  'gpar' element 'fontsize' must not be length 0
In addition: Warning messages:
1: Removed 561 rows containing missing values (geom_timeline). 
2: In is.na(colour) :
  is.na() applied to non-(list or vector) of type 'NULL'
  
2. With the leaflet interactive map: When I use again that column to define the size of the circles through the radius parameter
then the popup window doesn't pop up; the problem diasppears when I get rid of the radius paramameter in the addCircleMarkers function
