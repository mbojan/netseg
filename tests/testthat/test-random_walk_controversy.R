make_sbm_graph <- function(prob_within, prob_between, nodes_per_group = 100,
                           n_groups = 2, directed = FALSE,
                           require_connected = prob_between > 0, max_attempts = 100) {
  preference_matrix <- matrix(prob_between, nrow = n_groups, ncol = n_groups)
  diag(preference_matrix) <- prob_within
  for (attempt in seq_len(max_attempts)) {
    sbm_graph <- igraph::sample_sbm(nodes_per_group * n_groups,
                                    pref.matrix = preference_matrix,
                                    block.sizes = rep(nodes_per_group, n_groups),
                                    directed = directed)
    if (!require_connected || igraph::is_connected(sbm_graph, mode = "weak")) {
      return(sbm_graph)
    }
  }
  stop("Could not sample a connected SBM; increase prob_within or prob_between.")
}

select_top_degree_by_group <- function(graph, membership, k_per_group, mode = "all") {
  node_degrees <- igraph::degree(graph, mode = mode)
  unlist(lapply(sort(unique(membership)), function(group_id) {
    group_nodes <- which(membership == group_id)
    group_nodes[order(node_degrees[group_nodes], decreasing = TRUE)][seq_len(k_per_group)]
  }))
}

collect_warning_messages <- function(expr) {
  warning_messages <- character(0)
  result <- withCallingHandlers(expr, warning = function(warning_condition) {
    warning_messages <<- c(warning_messages, conditionMessage(warning_condition))
    invokeRestart("muffleWarning")
  })
  list(result = result, warnings = warning_messages)
}

score_sbm <- function(prob_within, prob_between, calc_mode = "ovso", n_groups = 2,
                      nodes_per_group = 100, k_per_group = 5, n_sim = 1000,
                      maximum_walk_length = 100) {
  sbm_graph <- make_sbm_graph(prob_within, prob_between, nodes_per_group, n_groups)
  group_membership <- rep(seq_len(n_groups), each = nodes_per_group)
  random_walk_controversy(
    membership = group_membership,
    graph = sbm_graph,
    mode = "all",
    n_sim = n_sim,
    influential_nodes = select_top_degree_by_group(sbm_graph, group_membership, k_per_group),
    maximum_walk_length = maximum_walk_length,
    calc_mode = calc_mode,
    verbose = FALSE
  )
}


prob_within_group <- 0.05
prob_between_levels <- c(none = 0.05, moderate = 0.01, strong = 0.001)


testthat::test_that("OVSO RWC increases with SBM polarization (2 groups)", {
  set.seed(42)
  scores <- vapply(prob_between_levels,
                   function(prob_between) score_sbm(prob_within_group, prob_between),
                   numeric(1))

  testthat::expect_lt(abs(scores[["none"]]), 0.1)
  testthat::expect_gt(scores[["moderate"]], 0.05)
  testthat::expect_lt(scores[["moderate"]], 0.5)
  testthat::expect_gt(scores[["strong"]], 0.5)
  testthat::expect_true(all(diff(scores) > 0))
})

testthat::test_that("Individual RWC increases with SBM polarization (2 groups)", {
  set.seed(42)
  scores <- vapply(prob_between_levels,
                   function(prob_between) score_sbm(prob_within_group, prob_between,
                                                    calc_mode = "individual"),
                   numeric(1))

  testthat::expect_lt(abs(scores[["none"]]), 0.1)
  testthat::expect_gt(scores[["strong"]], 0.5)
  testthat::expect_true(all(diff(scores) > 0))
})

testthat::test_that("OVSO RWC increases with SBM polarization (3 groups)", {
  set.seed(42)
  scores <- vapply(prob_between_levels,
                   function(prob_between) score_sbm(prob_within_group, prob_between,
                                                    n_groups = 3),
                   numeric(1))

  testthat::expect_lt(abs(scores[["none"]]), 0.1)
  testthat::expect_gt(scores[["strong"]], 0.5)
  testthat::expect_true(all(diff(scores) > 0))
})

testthat::test_that("Fully separated SBM yields RWC close to 1 and warns about connectivity", {
  set.seed(42)
  sbm_graph <- make_sbm_graph(prob_within_group, prob_between = 0)
  group_membership <- rep(1:2, each = 100)

  captured <- collect_warning_messages(random_walk_controversy(
    membership = group_membership,
    graph = sbm_graph,
    mode = "all",
    n_sim = 200,
    influential_nodes = select_top_degree_by_group(sbm_graph, group_membership, 5),
    maximum_walk_length = 100,
    verbose = TRUE
  ))

  testthat::expect_true(any(grepl("not weakly connected", captured$warnings)))
  testthat::expect_gt(captured$result, 0.95)
})


testthat::test_that("Walks stuck at sinks raise the premature-ending warning", {
  set.seed(42)
  sbm_graph <- igraph::as_directed(make_sbm_graph(prob_within_group, 0.01), mode = "mutual")
  group_membership <- rep(1:2, each = 100)
  influential_nodes <- select_top_degree_by_group(sbm_graph, group_membership, 5, mode = "out")

  sink_nodes <- setdiff(c(1:10, 101:110), influential_nodes)
  sbm_graph <- igraph::delete_edges(sbm_graph, igraph::incident(sbm_graph, sink_nodes, mode = "out"))

  captured <- collect_warning_messages(random_walk_controversy(
    membership = group_membership,
    graph = sbm_graph,
    mode = "out",
    n_sim = 200,
    influential_nodes = influential_nodes,
    maximum_walk_length = 100,
    verbose = TRUE
  ))

  testthat::expect_true(any(grepl("ended prematurely", captured$warnings)))
  testthat::expect_true(is.finite(captured$result))
})

testthat::test_that("Short walks raise the never-visited warning", {
  set.seed(42)
  sbm_graph <- make_sbm_graph(prob_within_group, 0.01)
  group_membership <- rep(1:2, each = 100)

  captured <- collect_warning_messages(random_walk_controversy(
    membership = group_membership,
    graph = sbm_graph,
    mode = "all",
    n_sim = 500,
    influential_nodes = select_top_degree_by_group(sbm_graph, group_membership, 5),
    maximum_walk_length = 1,
    verbose = TRUE
  ))

  testthat::expect_true(any(grepl("never visited an influential node", captured$warnings)))
})

testthat::test_that("Unreachable influential nodes raise an error", {
  set.seed(42)
  sbm_graph <- igraph::add_vertices(make_sbm_graph(prob_within_group, 0.01), nv = 2)
  group_membership <- c(rep(1:2, each = 100), 1, 2)
  isolated_influential_nodes <- c(201, 202)

  testthat::expect_error(
    random_walk_controversy(
      membership = group_membership,
      graph = sbm_graph,
      mode = "all",
      n_sim = 200,
      influential_nodes = isolated_influential_nodes,
      maximum_walk_length = 50,
      verbose = FALSE
    ),
    "None of the walks reached an influential node"
  )
})

testthat::test_that("verbose = FALSE suppresses walk-level warnings", {
  set.seed(42)
  sbm_graph <- make_sbm_graph(prob_within_group, 0.01)
  group_membership <- rep(1:2, each = 100)

  testthat::expect_no_warning(random_walk_controversy(
    membership = group_membership,
    graph = sbm_graph,
    mode = "all",
    n_sim = 500,
    influential_nodes = select_top_degree_by_group(sbm_graph, group_membership, 5),
    maximum_walk_length = 1,
    verbose = FALSE
  ))
})

testthat::test_that("Invalid arguments raise the expected errors and warnings", {
  set.seed(42)
  sbm_graph <- make_sbm_graph(prob_within_group, 0.01)
  group_membership <- rep(1:2, each = 100)
  influential_nodes <- select_top_degree_by_group(sbm_graph, group_membership, 5)

  testthat::expect_error(
    random_walk_controversy(group_membership, sbm_graph,
                            influential_nodes = influential_nodes, k_top = 5),
    "mutually exclusive")
  testthat::expect_error(
    random_walk_controversy(group_membership, sbm_graph),
    "Either a set of influential nodes")
  testthat::expect_error(
    random_walk_controversy(group_membership, sbm_graph,
                            influential_nodes = influential_nodes, mode = "both"),
    "'mode' parameter must be")
  testthat::expect_error(
    random_walk_controversy(group_membership, sbm_graph,
                            influential_nodes = influential_nodes, n_sim = 0),
    "positive integer for n_sim")
  testthat::expect_error(
    random_walk_controversy(group_membership, sbm_graph,
                            influential_nodes = influential_nodes, maximum_walk_length = 0),
    "positive integer for maximum_walk_length")
  testthat::expect_error(
    random_walk_controversy(group_membership, sbm_graph, influential_nodes = integer(0)),
    "does not contain any element")
  testthat::expect_error(
    random_walk_controversy(group_membership, sbm_graph,
                            influential_nodes = which(group_membership == 1)),
    "has no non-influential nodes")
  testthat::expect_error(
    random_walk_controversy("party", sbm_graph, influential_nodes = influential_nodes),
    "no vertex property")
  testthat::expect_error(
    random_walk_controversy(group_membership, "not_a_graph",
                            influential_nodes = influential_nodes),
    "Expected an igraph object")

  testthat::expect_warning(
    random_walk_controversy(group_membership, sbm_graph, mode = "all", n_sim = 200,
                            influential_nodes = influential_nodes, balanced = TRUE,
                            maximum_walk_length = 100, verbose = FALSE),
    "Balance parameter will be ignored")
})
