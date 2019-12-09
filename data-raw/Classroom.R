library(readr)
library(igraph)

setwd(here::here("data-raw"))

if(FALSE) {
  library(dplyr)
  # Recover from 'isnar'
  data(IBE121, package="isnar")

  d <- igraph::as_data_frame(IBE121, what="both")

  d$vertices %>%
    transmute(
      id = as.integer(name),
      gender = ifelse(female, "Girl", "Boy")
    ) %>%
    write_csv("classroom-vdb.csv")

  d$edges %>%
    filter(question == "play") %>%
    transmute(
      from = as.integer(from),
      to = as.integer(to)
    ) %>%
    write_csv("classroom-edb.csv")
}

igraph::graph_from_data_frame(
  read_csv("classroom-edb.csv",
           col_types = cols(
             from = col_double(),
             to = col_double()
           )
  ) %>%
    as.data.frame(),
  vertices = read_csv("classroom-vdb.csv",
                      col_types = cols(
                        id = col_double(),
                        gender = col_character()
                      )
  ) %>%
    as.data.frame(),
  directed = TRUE
) -> Classroom

usethis::use_data(Classroom)
