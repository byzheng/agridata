test_that("silo", {
    skip_if(TRUE)
    lat <- -35
    lon <- 147
    start = as.Date("2024-01-01")
    finish = as.Date("2024-01-10")
    email <- "bangyou.zheng@csiro.au"
    met <- silo_grid(lat = lat, lon = lon, email = email, start = start, finish = finish)
    expect_equal(grepl("weather.met.weather", met), TRUE)

    station <- "72150"
    met <- silo_ppd(station = station, email = email, start = start, finish = finish)
    expect_equal(grepl("weather.met.weather", met), TRUE)
})
