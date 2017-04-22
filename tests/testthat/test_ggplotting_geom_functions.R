library(testthat)

## Testing geom_timeline(), and geom_timeline_label() functions
test_that("Plotting data with ggplot2",{
        quakedata<-eq_clean_data("signif.txt")
        expect_that(
                GeomTimeline,
                is_a("Geom")
        )
        expect_that(
        ggplot() + geom_timeline(data = quakedata,aes(x = DATETIME)),
        is_a("ggplot")
        )
        expect_that(
                ggplot() + geom_timeline_label(data = quakedata,aes(x = DATETIME)),
                throws_error("geom_timeline_label requires the following missing aesthetics: label")
        )
})


