library(dplyr)
library(readr)

if(FALSE) {
  # Recover from 'isnar'
  data(EFnet, package="isnar")
  d <- igraph::as_data_frame(EFnet, what = "both")

  # vdb
  d$vertices %>%
    as_tibble() %>%
    transmute(
      name = name,
      race = type
    ) %>%
    readr::write_csv("EF3-vdb.csv")

  # edb
  d$edges %>%
    as_tibble() %>%
    readr::write_csv("EF3-edb.csv")
}




edb <- readr::read_csv(
  "EF3-edb.csv",
  col_types = cols(
    from = col_character(),
    to = col_character()
  )
)

vdb <- readr::read_csv(
  "EF3-vdb.csv",
  col_types = cols(
    name = col_character(),
    race = col_integer()
  )
)


EF3 <- igraph::graph_from_data_frame(
  as.data.frame(edb),
  vertices=as.data.frame(vdb),
  directed=FALSE
)

usethis::use_data(EF3)



