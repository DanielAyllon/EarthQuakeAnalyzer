#' Function to visualize the epicenters of earthquakes with annotation in a leaflet interactive map
#'
#' This function reads a filtered data frame with earthquakes to visualize. The function maps the epicenters (LATITUDE/LONGITUDE) and annotates each point in a pop-up window containing annotation data stored in a column of the data frame. The user is able to choose which column is used for the annotation in the pop-up with the function argument named annot_col.
#'  Each earthquake is shown with a circle, and the radius of the circle is proportional to the earthquake's magnitude.
#'
#' @param dataframe A filtered dataframe with the earthquakes to visualize.
#' @param annot_col A character string indicating the column used for the annotation in the pop-up window.
#'
#' @return A leaflet fully interactive map plotting the epicenters of selected earthquakes.
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' eq_clean_data("signif.txt") %>% dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATETIME) >= 2000) %>% eq_map(annot_col = "DATETIME")
#' }
#'
#' @export
eq_map<-function(dataframe,annot_col){
        mydataframe<-dataframe[,c("LATITUDE","LONGITUDE","EQ_PRIMARY",annot_col)]
        leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(data = mydataframe, radius = ~ ifelse(!is.na(EQ_PRIMARY),EQ_PRIMARY,2),lng = ~ LONGITUDE, lat = ~ LATITUDE, popup = ~ mydataframe[,annot_col])
}



#' Function to create annotations to be displayed in the maps created by eq_map()
#'
#' This function takes the filtered and cleaned dataframe as an argument and creates an HTML label that can be used as the annotation text in a leaflet map created by eq_map().
#' This function puts together a character string for each earthquake that shows the cleaned location (as cleaned by the eq_location_clean() function), the magnitude, and the total number of deaths, with boldface labels for each. If an earthquake is missing values for any of these, both the label and the value are skipped for that element of the tag.
#'
#' @param dataframe A filtered and cleaned (as cleaned by the eq_location_clean() function) dataframe with the earthquakes to visualize.
#'
#' @return A dataframe containing a column named "popup_text" with the character string to be used as input in the eq_map() function.
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' eq_clean_data("signif.txt") %>% dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATETIME) >= 2000) %>% eq_create_label() %>% eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label<-function(dataframe){
        mydataframe<-dplyr::select(dataframe,LATITUDE,LONGITUDE,LOCATION_NAME,EQ_PRIMARY,TOTAL_DEATHS) %>%
                dplyr::mutate(popup_text=paste("<b>Location:</b>", LOCATION_NAME, "<br />"),popup_text=ifelse(!is.na(EQ_PRIMARY),paste(popup_text,"<b>Magnitude:</b>", EQ_PRIMARY, "<br />"),popup_text),
popup_text=ifelse(!is.na(TOTAL_DEATHS),paste(popup_text,      "<b>Total deaths:</b>", TOTAL_DEATHS, "<br />"),popup_text))
}
