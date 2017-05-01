#' Clean downloaded data frame
#'
#' This function reads an Earthquake dataset in a tab-delimited file to clean some of the variables.
#' The function first checks whether the file to be read actually exists.If it does not exist, then it downloads from the NOOA website.
#' Then, the function adds a date column created by uniting the year, month, day of each earthquake and converting it to the Date class.
#' Finally, the function converts the LATITUDE and LONGITUDE columns to numeric class.
#'
#' @param filename A character vector of the name of the tab-delimited file to be read.
#'
#' @return This function returns a clean data frame.
#'
#' @importFrom utils download.file
#' @importFrom readr read_delim
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom tidyr unite
#' @importFrom lubridate ymd
#'
#' @examples
#' \dontrun{
#' eq_clean_data("signif.txt")
#' }
#'
#' @export
eq_clean_data<-function(filename) {
        if(!file.exists(filename)) {
                utils::download.file("https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt",filename)
        }
        EarthquakeData<-readr::read_delim(filename,delim = "\t") %>%
                dplyr::filter(!is.na(YEAR), YEAR > 0, !is.na(MONTH), !is.na(DAY)) %>%
                dplyr::mutate(YEAR = as.character(YEAR)) %>%
                dplyr::mutate(NEWYEAR=ifelse(nchar(YEAR)==1,paste0("000",YEAR),
                                      ifelse(nchar(YEAR)==2,paste0("00",YEAR),
                                             ifelse(nchar(YEAR)==3,paste0("0",YEAR),YEAR)))) %>%
                tidyr::unite(DATETIME, NEWYEAR, MONTH, DAY) %>%
                dplyr::mutate(DATETIME = lubridate::ymd(DATETIME)) %>%
                dplyr::mutate(LATITUDE = as.numeric(LATITUDE),LONGITUDE = as.numeric(LONGITUDE),EQ_PRIMARY = as.numeric(EQ_PRIMARY),TOTAL_DEATHS = as.numeric(TOTAL_DEATHS),YEAR = as.numeric(YEAR))
        EarthquakeData
}




#' Clean up LOCATION_NAME column from downloaded data frame
#'
#' This function reads an Earthquake dataset in a dataframe format to clean the LOCATION_NAME column by stripping out the country name (including the colon) and convert names to title case (as opposed to all caps).
#'
#' @param EarthquakeData A dataframe.
#'
#' @return This function returns a data frame with the column LOCATION_NAME cleaned.
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_trim
#' @importFrom stringr str_to_title
#'
#' @examples
#' \dontrun{
#' eq_location_clean(EarthquakeDataFrame)
#' eq_clean_data("signif.txt") %>% eq_location_clean()
#' }
#'
#' @export
eq_location_clean<-function(EarthquakeData) {
        n_earthquakes<-length(EarthquakeData$LOCATION_NAME)
        clean_loc_name<-data.frame()
        for(i in 1:n_earthquakes){
                myname<-as.character(EarthquakeData$LOCATION_NAME[[i]])
                colon_loc<-regexpr("\\:[^\\:]*$", myname)[[1]]
                myname<-substring(myname,colon_loc + 1) %>%
                        stringr::str_trim(.) %>%
                        stringr::str_to_title(.)
                clean_loc_name[i,1]<-myname
        }
        colnames(clean_loc_name)<-"LOCATION_NAME"
        myData<-dplyr::select(EarthquakeData,-LOCATION_NAME)
        myData<-cbind(myData,clean_loc_name)
        myData
}
