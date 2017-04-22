library(testthat)

## Testing eq_clean_data(), and eq_location_clean() functions
test_that("Reading & cleaning data",{
        download.file("https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt","signif.txt")
        CleanDf<-eq_clean_data("signif.txt")
        expect_that(CleanDf,is_a("data.frame"))
        expect_that(CleanDf$DATETIME,is_a("Date"))
        expect_that(CleanDf,is_identical_to(eq_clean_data("NonSense.txt")))

        CleanLocDf<-eq_location_clean(CleanDf)
        expect_that(eq_location_clean("signif.txt"),throws_error())
        expect_that(CleanLocDf,is_a("data.frame"))
})

