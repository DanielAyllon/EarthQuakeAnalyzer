#' ggplot geom to plot a time line of earthquakes
#'
#' This function reads an Earthquake dataset in a dataframe format to plot a time line of earthquakes ranging from the minimum to the maximum dates with a point for each earthquake.
#' The x aesthetic is a date and an optional y aesthetic is a factor that stratifies the data by country, in which case multiple time lines will be plotted, one for each country.
#' In addition to showing the dates on which the earthquakes occur, it is also possible to show the magnitudes (i.e. Richter scale value) and the number of deaths associated with each earthquake.
#'
#' @param x A character vector of the dates when the earthquakes took place.
#' @param y Optional aesthetic which contains a character vector of the country names, wherein the earthquakes took place.
#' @param fill Optional aesthetic which contains a numeric vector of the number of deaths in each earthquake, which will define the colour of the points to be plotted.
#' @param size Optional aesthetic which contains a numeric vector of the magnitude (in the Richter scale) of each earthquake, which will define the size of the points to be plotted.
#' @param alpha Optional aesthetic that represents a number between 0 and 1, which indicates the level of transparency of represented points.
#'
#' @return An earthquakes' timeline geom object which can be added to a ggplot.
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom ggplot2 layer
#' @importFrom grid pointsGrob
#' @importFrom grid unit
#' @importFrom grid gpar
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' quakedata<-eq_clean_data("signif.txt")
#' ggplot() + geom_timeline(data = quakedata,aes(x = DATETIME, y = COUNTRY, size = EQ_PRIMARY, fill = TOTAL_DEATHS)) + scale_color_gradient()
#' }
#'
#' @export
GeomTimeline<- ggplot2::ggproto("GeomTimeline",
                                ggplot2::Geom,
                          required_aes = "x",
                          default_aes = ggplot2::aes(y=0.1,fill = "blue",colour="blue",
                                                     size=0.01,alpha=0.7,stroke = 0.5,
                                                     shape = 21),
                          draw_key = ggplot2::draw_key_point,
                          draw_panel = function(data, panel_scales, coord) {

                 mydata<-dplyr::mutate(data,x=lubridate::as_date(x)) %>%
                 dplyr::mutate(x=lubridate::decimal_date(x)) %>%
                 dplyr::mutate(y=as.numeric(y)) %>%
                 dplyr::filter(!is.na(x)) %>%
                 dplyr::filter(!is.na(fill)) %>%
                 dplyr::filter(!is.na(size)) %>%
                 dplyr::mutate(size=size/100)

           xmin<-min(mydata$x)
           xmax<-max(mydata$x)
           panel_scales$x.range<-c(xmin,xmax)


           coords <- coord$transform(mydata, panel_scales)

           grid::pointsGrob(
                   x = grid::unit(coords$x,"native"),
                   y = grid::unit(coords$y,"native"),
                   pch = coords$shape,
                   size = grid::unit(coords$size,"native"),
                   gp = grid::gpar(
                           col = coords$colour,
                           alpha=coords$alpha,
                           fill = coords$fill)
                   )
                          }
)

#' Function to display the earthquake timelines
#'
#' The function displays timelines of earthquakes based on the \code{GeomTimeline}.
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
        ggplot2::layer(
                geom = GeomTimeline, mapping = mapping,
                data = data, stat = stat, position = position,
                show.legend = show.legend, inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, ...)
        )
}


#' ggplot geom to plot text annotations for a time line of earthquakes
#'
#' This function reads an Earthquake dataset in a dataframe format to create annotations to the plotted data.This geom adds a vertical line to each data point with a text annotation showing the location (or other information) of the earthquake attached to each line.
#' The x aesthetic is a date and an optional y aesthetic is a factor that stratifies the data by country, in which case multiple time lines will be plotted, one for each country. The label aesthetic indicates the name of the location of the earthquake (or the information to be dislayed).
#' There is the option to subset to n_max number of earthquakes, where the function takes the n_max largest (by magnitude) earthquakes.
#'
#' @param x A character vector ofthe dates when the earthquakes took place.
#' @param label A character vector of the names of the locations wherein earthquakes took place.
#' @param y Optional aesthetic which contains a character vector of the country names wherein the earthquakes took place.
#' @param n_max Optional aesthetic that represents a number indicating the number of the largest earthquakes (by magnitude) to be annotated.
#'@param magnitude Optional aesthetic which contains a numeric vector of the magnitude (in the Richter scale) of each earthquake.
#'
#' @return A geom to plot text labels for location (but also for date, magnitude, deaths, etc) linked to selected earthquakes which can be added to a ggplot.
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom ggplot2 layer
#' @importFrom grid pointsGrob
#' @importFrom grid segmentsGrob
#' @importFrom grid textGrob
#' @importFrom grid gTree
#' @importFrom grid gList
#' @importFrom grid unit
#' @importFrom grid gpar
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr slice
#'
#' @examples
#' \dontrun{
#' quakedata<-eq_clean_data("signif.txt") %>% eq_location_clean()
#' ggplot() + geom_timeline(data = quakedata,aes(x = DATETIME, y = COUNTRY, size = EQ_PRIMARY, colour = TOTAL_DEATHS)) + geom_timeline_label(data = quakedata,aes(x = DATETIME, label = LOCATION_NAME,y = COUNTRY, n_max = 10, magnitude = EQ_PRIMARY)) + scale_color_gradient()
#' }
#'
#' @export
GeomTimelineLabel<- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                required_aes =c("x","label"),
                                default_aes = ggplot2::aes(y=0.1,n_max=NULL,magnitude=NULL,colour="grey"),
                                draw_key = ggplot2::draw_key_text,
                                draw_panel = function(data, panel_scales, coord, fontsize, angle) {

     n_earthquakes<-data$n_max[1]
     mydata<-dplyr::mutate(data,x=lubridate::as_date(x)) %>%
             dplyr::mutate(x=lubridate::decimal_date(x)) %>%
             dplyr::mutate(y=as.numeric(y)) %>%
             dplyr::filter(!is.na(x)) %>%
             dplyr::filter(!is.na(magnitude))


     myannotationdata<-dplyr::arrange(mydata,dplyr::desc(magnitude)) %>%
             dplyr::slice(1:n_earthquakes)


     xmin<-min(mydata$x)
     xmax<-max(mydata$x)
     panel_scales$x.range<-c(xmin,xmax)


     coords <- coord$transform(mydata, panel_scales)
     coordsannot<-coord$transform(myannotationdata, panel_scales)


     linesQuakes<-
     grid::segmentsGrob(
             x0 = grid::unit(coordsannot$x,"native"),
             y0 = grid::unit(coordsannot$y,"native"),
             x1 = grid::unit(coordsannot$x,"native"),
             y1 = grid::unit((coordsannot$y)+0.1,"native"),
             gp = grid::gpar(col = coords$colour)
     )

     textsquakes<-
     grid::textGrob(
             label=coords$label,
             x = grid::unit(coordsannot$x,"native"),
             y = grid::unit((coordsannot$y)+0.12,"native"),
             just = "left",
             rot = angle,
             gp = grid::gpar(col = "grey50",
                             fontsize = fontsize)
             )

     grid::gTree(children=grid::gList(linesQuakes,textsquakes))
                                }
)

#' Function to display the annotated labels for the earthquake timelines
#'
#' The function displays the annotated labels for the timelines of earthquakes based on the \code{GeomTimelineLabel}.
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,fontsize = 10,
                                angle = 45, ...) {
        ggplot2::layer(
                geom = GeomTimelineLabel, mapping = mapping,
                data = data, stat = stat, position = position,
                show.legend = show.legend, inherit.aes = inherit.aes,
                params = list(na.rm = na.rm,fontsize = fontsize,angle = angle, ...)
        )
}
