testthat::test_that("Calculates RWC for the Python graph and checks whether it is in the specified interval", {

  set.seed(42)
  file_path <- testthat::test_path("../testdata", "test_rwc_network.gml")
  g <- igraph::read_graph(file_path, format = "gml")
  nodes_per_group <- igraph::vcount(g) / 2
  mems <- rep(1:2, each = nodes_per_group)
  score <- random_walk_controversy(
    membership = mems,
    graph = g,
    mode = "all",
    n_sim = 10000,
    k_top = 10,
    balanced = TRUE,
    maximum_walk_length = 20,
    verbose = FALSE
  )
  python_baseline <- 0.7694675430557305
  expected_margin_of_error <- 0.1642 # 99CI
  testthat::expect_equal(score, python_baseline, tolerance = expected_margin_of_error)
})
