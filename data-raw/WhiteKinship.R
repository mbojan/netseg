library(dplyr)
library(readr)

if(FALSE) {
  # Recover from 'isnar'
  data(Wnet, package="isnar")
  d <- igraph::as_data_frame(Wnet, what = "both")

  # vdb
  d$vertices %>%
    as_tibble() %>%
    mutate(
      gender = ifelse(gender == 1, "female", "male")
    ) %>%
    readr::write_csv("WhiteKinship-vdb.csv")

  # edb
  d$edges %>%
    as_tibble() %>%
    readr::write_csv("WhiteKinship-edb.csv")
}


edb <- readr::read_csv(
  "WhiteKinship-edb.csv",
  col_types = cols(
    from = col_character(),
    to = col_character()
  )
)

vdb <- readr::read_csv(
  "WhiteKinship-vdb.csv",
  col_types = cols(
    name = col_character(),
    gender = col_character()
  )
)


WhiteKinship <- igraph::graph_from_data_frame(
  as.data.frame(edb),
  vertices=as.data.frame(vdb),
  directed=FALSE
  )

usethis::use_data(WhiteKinship)


