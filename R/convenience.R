age5yrGrp <- function() {

  x <- seq(0,130, by = 5)
  a <- dplyr::lead(x) - 1
  lab <- glue::glue("{x}-{a}")[-length(x)]

  br <- newBreaks(
    name = "5-Year Age Groups",
    breaks = x
  )

  br$labels <- c(lab, paste0(dplyr::last(x), "+"))

  return(br)
}
