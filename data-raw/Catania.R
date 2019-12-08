library(dplyr)
library(readr)

if(FALSE) {
  # Recover from 'isnar'
  data(Catania, package="isnar")
  Catania %>%
    as.table() %>%
    as_tibble() %>%
    rename(male=man, female=woman) %>%
    write_csv("Catania.csv")
}



read_csv(
  "Catania.csv",
  col_types = cols(
    male = col_character(),
    female = col_character(),
    n = col_double()
  )
) %>%
  with(
    tapply(n, list(male=male, female=female), identity)
  ) %>%
  as.table() -> Catania

usethis::use_data(Catania)
