#' API for the population in Linkoping population from year 1970 and 2020
#'
#' The function returns a data.frame with variables year, gender and the population count.
#' @return Returns a data.frame with population data in Linköping
#' @export
#'
#' @examples
#' pop <- linkoping_pop()
linkoping_pop <- function(){

  data <- httr::GET("https://api.kolada.se/v2/data/municipality/0580/kpi/N01951")

  httr::stop_for_status(data)

  pop_contents <- httr::content(data, as = "text")
  pop_contents <- jsonlite::fromJSON(pop_contents)


  k1 <- do.call("rbind", args = pop_contents$values$values)

  #Skapar vi år variabel
  Year <- rep(pop_contents$values$period, each = 3)


  pop_df <- cbind(Year, k1)
  pop_df$gender <- as.factor(pop_df$gender)
  pop_df[, c("count", "status")] <- NULL
  return(pop_df)
}
