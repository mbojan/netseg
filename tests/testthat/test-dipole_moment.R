testthat::test_that("Dipole moment on a symmetric SBM", {
  set.seed(3)
  n_groups <- 2
  nodes_per_group <- 100
  total_nodes <- n_groups * nodes_per_group
  p_in <- 0.08
  p_out <- 0.004

  n_sim <- 100
  scores <- numeric(n_sim)
  pref_matrix <- matrix(c(p_in, p_out,
                          p_out, p_in),
                        nrow = 2, byrow = TRUE)

  for(i in 1:n_sim) {

    g <- igraph::sample_sbm(n = total_nodes,
                            pref.matrix = pref_matrix,
                            block.sizes = c(nodes_per_group, nodes_per_group),
                            directed = TRUE)
    mems <- rep(1:2, each = nodes_per_group)
    igraph::V(g)$membership <- mems

    scores[i] <- dipole_moment(membership = mems,
                               graph = g,
                               ego_included = TRUE,
                               k_top = 10,
                               balanced = TRUE,
                               mode = "in")
  }
  mean_score <- mean(scores)
  sd_score <- sd(scores)
  margin_of_error <- 1.96 * sd_score
  lower_bound <- mean_score - margin_of_error
  upper_bound <- mean_score + margin_of_error


  testthat::expect_equal(0.2893930981512981, mean_score, tolerance = upper_bound - lower_bound)
  # Generative models are stochastic, hence I am testing whether the calculated score
  # of python's case fall within the 99CI of the generated models in here.
})
