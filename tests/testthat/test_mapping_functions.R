library(testthat)

## Testing eq_map(), and eq_create_label() functions
test_that("Visualizing data in leaflet map",{
        quakedata<-eq_clean_data("signif.txt")
        expect_that(
                quakedata %>% dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATETIME) >= 2000) %>% eq_map(annot_col = "DATETIME"),
                is_a("htmlwidget")
        )

        labelled_qd<-quakedata  %>% dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATETIME) >= 2000) %>% eq_create_label()
        expect_true(
                "popup_text" %in% colnames(labelled_qd)
        )
})


