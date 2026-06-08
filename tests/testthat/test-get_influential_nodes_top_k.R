
testthat::test_that("Detect influential nodes in a non-balanced setting",{
  # Since this is not the balanced definition, it should have
  # k_top nodes as the influential nodes.
  er_toy <- igraph::sample_gnp(1000, 0.01)
  er_toy_degrees <- degree(er_toy)
  k_top <- 10
  mems <- append(rep(1,500), rep(0,500))
  k_top_expected <-order(er_toy_degrees, decreasing =  TRUE)[1:10]
  k_top_found <-get_influential_nodes_top_k(er_toy, mems, k_top = k_top)
  testthat::expect_equal(sort(k_top_expected), sort(k_top_found))
})

testthat::test_that("Detect influential nodes in a balanced setting", {
  # In this case from each group it should find k_top / group_number influential
  # nodes, we are going to try two groups for sake of simplicity.

  er_toy <- igraph::sample_gnp(1000, 0.01)
  er_toy_degrees <- igraph::degree(er_toy)
  k_top <- 10
  mems <- rep(0:1, 500)

  g1_indices <- which(mems == 1)
  g1_degrees <- er_toy_degrees[g1_indices]
  g1_top_5 <- g1_indices[order(g1_degrees, decreasing = TRUE)[1:5]]

  g2_indices <- which(mems == 0)
  g2_degrees <- er_toy_degrees[g2_indices]
  g2_top_5 <- g2_indices[order(g2_degrees, decreasing = TRUE)[1:5]]

  k_top_expected <- c(g1_top_5, g2_top_5)

  k_top_found <- get_influential_nodes_top_k(er_toy, mems, k_top = k_top, balanced = TRUE)

  testthat::expect_equal(sort(k_top_expected), sort(k_top_found))
})
