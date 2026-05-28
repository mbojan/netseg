
reduce_matrix_rwc <- function(matrix_in, group_idx) {
  val <- matrix_in[group_idx, group_idx]
  row_sum_excl <- sum(matrix_in[group_idx, ]) - val
  col_sum_excl <- sum(matrix_in[, group_idx]) - val

  total_sum <- sum(matrix_in)
  others_sum <- total_sum - (row_sum_excl + col_sum_excl + val)

  matrix(c(val, row_sum_excl, col_sum_excl, others_sum), nrow = 2, byrow = TRUE)
}

walk_rwc <- function(graph,
                     start_nodes,
                     membership,
                     influential_nodes,
                     walk_length = 10000,
                     mode = "out",
                     verbose = TRUE) {

  walks <- lapply(start_nodes, function(start) {
    as.integer(igraph::random_walk(graph, start = start, steps = walk_length, mode = mode))
  })

  walk_lengths <- lengths(walks)

  complete_indices <- which(walk_lengths == walk_length + 1)

  if (length(walks) != length(complete_indices)) {
    if (verbose) {
      warning(sprintf("%d walks out of %d ended prematurely.
                      This might indicate that the graph is not connected or
                      some walkers got stuck. RWC will be calculated with completed walks",
                      length(walks) - length(complete_indices), length(walks)), call. = FALSE)
    }
  }

  if (length(complete_indices) == 0) {
    stop("All walks ended prematurely.")
  }

  results_mat <- do.call(rbind, walks[complete_indices])
  is_inf <- matrix(results_mat %in% influential_nodes,
                   nrow = nrow(results_mat),
                   ncol = ncol(results_mat))
  successful_hits <- rowSums(is_inf)

  no_hit_mask <- successful_hits == 0
  n_no_hits <- sum(no_hit_mask)

  if (n_no_hits > 0) {
    if (verbose) {
      warning(sprintf("%d walks out of %d never visited an influential node.",
                      n_no_hits, nrow(results_mat)), call. = FALSE)
    }
    if (n_no_hits == nrow(results_mat)) {
      stop("None of the completed walks reached an influential node.")
    }
  }

  valid_hits_idx <- which(!no_hit_mask)
  results_mat_valid <- results_mat[valid_hits_idx, , drop = FALSE]
  is_inf_valid <- is_inf[valid_hits_idx, , drop = FALSE]
  results_indexed <- max.col(is_inf_valid, ties.method = "first")
  hit_node_ids <- results_mat_valid[cbind(1:nrow(results_mat_valid), results_indexed)]
  membership_first_hit <- membership[hit_node_ids]

  start_nodes_successful <- start_nodes[complete_indices][valid_hits_idx]
  start_nodes_membership <- membership[start_nodes_successful]
  uq_membership <- sort(unique(membership))
  mm <- table(factor(start_nodes_membership, levels = uq_membership),
              factor(membership_first_hit, levels = uq_membership))

  return(as.matrix(mm))
}


#' Random Walk Controversy
#'
#' @description Calculates the Random Walk Controversy with an arbitrary number of groups.
#' @param membership Vector of group memberships or a string denoting a vertex attribute.
#' @param graph An \code{igraph} graph object.
#' @param maximum_walk_length Integer. Maximum number of steps a walker can take.
#' @param n_sim Integer. Number of random walks to simulate.
#' @param influential_nodes List of vertex IDs considered influential.
#' @param mode Character. "out", "in", or NULL for random walks in directed graphs.
#' @param k_top Integer. Number of highest-degree nodes to classify as influential.
#' @param null_models List of \code{igraph} objects representing baseline graphs.
#' @param balanced Logical. Distribute influential nodes equally across all groups.
#' @param calc_mode Character. "ovso" or "individual".
#' @param verbose Logical. If TRUE, emits warnings for prematurely ended walks.
#'
#' @return A numeric Random Walk Controversy score.
#' @references Garimella, Kiran, et al. "Quantifying controversy on social media." (2018).
#' @export
random_walk_controversy <- function(membership, graph, maximum_walk_length = 1000,
                                    n_sim = 10000, influential_nodes = NULL,
                                    mode = NULL, k_top = NULL, null_models = NULL,
                                    balanced = FALSE, calc_mode = "ovso",
                                    verbose = TRUE) {


  if (!is.igraph(graph)) stop("Expected an igraph object for the graph parameter.")

  if (!igraph::is_connected(graph, mode = "weak") && verbose) {
    warning("Graph is not weakly connected, this
            might result in unexpected behaviour", call. = FALSE)
  }

  if (is.character(membership) && length(membership) == 1) {
    if (!membership %in% igraph::vertex_attr_names(graph)) {
      stop("There is no vertex property with the given
           group membership name in the graph")
    }
    membership <- igraph::vertex_attr(graph, membership)
  }

  membership <- as.integer(as.factor(as.character(membership)))

  if (n_sim <= 0) stop("Expected a positive integer for n_sim.")
  if (maximum_walk_length <= 0) stop("Expected a positive integer for maximum_walk_length.")

  if (!is.null(influential_nodes) && !is.null(k_top)) {
    stop("'influential_nodes' and 'k_top' are mutually exclusive arguments.")
  }

  if (is.null(influential_nodes) && is.null(k_top)) {
    stop("Either a set of influential nodes or the threshold 'k_top' should be given.")
  }

  if (!is.null(mode) && !(mode %in% c("in", "out", "all"))) {
    stop("'mode' parameter must be 'in', 'out', 'all', or NULL.")
  }

  r_mode <- ifelse(is.null(mode), "out", mode)

  if (!is.null(influential_nodes) && balanced) {
    warning("A list of influential nodes supplied with 'balanced' = TRUE.
            Balance parameter will be ignored.", call. = FALSE)
  }

  if (!is.null(k_top)) {
    if (k_top <= 0) stop("Expected a positive integer for k_top.")
    influential_nodes <- get_influential_nodes_top_k(graph, membership, mode = r_mode, k_top = k_top, balanced = balanced)
  } else {
    if (length(influential_nodes) == 0) stop("Given Influential nodes list does not contain any element.")
    influential_nodes <- as.integer(influential_nodes)
  }

  unique_groups <- sort(unique(membership))
  valid_nodes_by_group <- list()

  for (g in unique_groups) {
    g_nodes <- which(membership == g)
    valid_g_nodes <- setdiff(g_nodes, influential_nodes)
    if (length(valid_g_nodes) == 0) {
      stop(sprintf("Group %s has no non-influential nodes to start walks from.", as.character(g)))
    }
    valid_nodes_by_group[[as.character(g)]] <- valid_g_nodes
  }

  chosen_groups <- sample(unique_groups, size = n_sim, replace = TRUE)
  counts <- table(factor(chosen_groups, levels = unique_groups))

  start_nodes <- integer(n_sim)
  start_idx <- 1
  for (g in unique_groups) {
    count <- counts[[as.character(g)]]
    if (count > 0) {
      start_nodes[start_idx:(start_idx + count - 1)] <- sample(valid_nodes_by_group[[as.character(g)]], size = count, replace = TRUE)
      start_idx <- start_idx + count
    }
  }

  mm_walk_matrix <- walk_rwc(graph = graph,
                             start_nodes = start_nodes,
                             membership = membership,
                             influential_nodes = influential_nodes,
                             walk_length = maximum_walk_length,
                             mode = r_mode,
                             verbose = verbose)

  if (calc_mode == "individual") {

    col_sums <- colSums(mm_walk_matrix)
    col_sums[col_sums == 0] <- 1
    ratio_mm_walk_matrix <- sweep(mm_walk_matrix, 2, col_sums, FUN = "/")

    diags <- diag(ratio_mm_walk_matrix)
    upper_prod <- prod(ratio_mm_walk_matrix[upper.tri(ratio_mm_walk_matrix)])
    lower_prod <- prod(ratio_mm_walk_matrix[lower.tri(ratio_mm_walk_matrix)])

    rwc <- prod(diags) - (upper_prod * lower_prod)
    if (is.null(null_models)) return(rwc)
    null_rwcs <- sapply(null_models, function(null_graph) {
      if (!is.null(k_top)) {
        inf_nodes_null <- get_influential_nodes_top_k(null_graph,
                                                      membership,
                                                      mode = r_mode,
                                                      k_top = k_top,
                                                      balanced = balanced)
      } else {
        inf_nodes_null <- influential_nodes
      }

      mm_null <- walk_rwc(graph = null_graph, start_nodes = start_nodes, membership = membership,
                          influential_nodes = inf_nodes_null, walk_length = maximum_walk_length,
                          mode = r_mode, verbose = verbose)

      c_sums <- colSums(mm_null)
      c_sums[c_sums == 0] <- 1
      r_mm_null <- sweep(mm_null, 2, c_sums, FUN = "/")

      d <- diag(r_mm_null)
      u_p <- prod(r_mm_null[upper.tri(r_mm_null)])
      l_p <- prod(r_mm_null[lower.tri(r_mm_null)])

      return(prod(d) - (u_p * l_p))
    })

    return(rwc - mean(null_rwcs, na.rm = TRUE))

  } else if (calc_mode == "ovso") {

    unique_group_size <- nrow(mm_walk_matrix)
    ovso <- numeric(unique_group_size)

    for (uq_group in 1:unique_group_size) {
      mm_walk_matrix_reduced <- reduce_matrix_rwc(mm_walk_matrix, uq_group)
      c_sums <- colSums(mm_walk_matrix_reduced)
      c_sums[c_sums == 0] <- 1
      ratio_mm <- sweep(mm_walk_matrix_reduced, 2, c_sums, FUN = "/")

      d <- diag(ratio_mm)
      u_p <- prod(ratio_mm[upper.tri(ratio_mm)])
      l_p <- prod(ratio_mm[lower.tri(ratio_mm)])
      ovso[uq_group] <- prod(d) - (u_p * l_p)
    }

    ovso_rwc <- mean(ovso, na.rm = TRUE)

    if (is.null(null_models)) return(ovso_rwc)

    ovso_out <- sapply(null_models, function(null_graph) {
      if (!is.null(k_top)) {
        inf_nodes_null <- get_influential_nodes_top_k(null_graph, membership,
                                                      mode = r_mode,
                                                      k_top = k_top,
                                                      balanced = balanced)
      } else {
        inf_nodes_null <- influential_nodes
      }

      mm_null <- walk_rwc(graph = null_graph, start_nodes = start_nodes, membership = membership,
                          influential_nodes = inf_nodes_null, walk_length = maximum_walk_length,
                          mode = r_mode, verbose = verbose)

      ovso_null_inner <- numeric(unique_group_size)
      for (uq_group in 1:unique_group_size) {
        mm_red <- reduce_matrix_rwc(mm_null, uq_group)
        c_sums <- colSums(mm_red)
        c_sums[c_sums == 0] <- 1
        r_mm <- sweep(mm_red, 2, c_sums, FUN = "/")

        d <- diag(r_mm)
        u_p <- prod(r_mm[upper.tri(r_mm)])
        l_p <- prod(r_mm[lower.tri(r_mm)])
        ovso_null_inner[uq_group] <- prod(d) - (u_p * l_p)
      }
      return(mean(ovso_null_inner, na.rm = TRUE))
    })

    return(ovso_rwc - mean(ovso_out, na.rm = TRUE))
  }
}
