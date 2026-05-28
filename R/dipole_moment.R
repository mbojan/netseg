run_label_propagation <- function(graph,
                                  influential_nodes,
                                  membership,
                                  ego_included,
                                  max_iter,
                                  update_tolerance,
                                  mode) {
  n_nodes <- igraph::vcount(graph)
  inf_membership <- membership[influential_nodes]
  inf_opinions <- ifelse(inf_membership == min(membership), -1, 1)

  opinion_array <- numeric(n_nodes)
  opinion_array[influential_nodes] <- inf_opinions

  if (!igraph::is_connected(graph, mode = "weak")) {
    warning("The input graph is not weakly connected.", call. = FALSE)
  }

  A <- igraph::as_adj(graph, sparse = TRUE)
  if (igraph::is_directed(graph)) {
    if (mode == "in") {
      A <- Matrix::t(A)
    } else if (mode == "all" || is.null(mode)) {
      A <- A + Matrix::t(A)
      A[A > 1] <- 1
    }
  }

  if (ego_included) {
    diag_A <- Matrix::diag(A)
    Matrix::diag(A) <- diag_A + 1
  } else {
    Matrix::diag(A) <- 0
  }
  row_sums <- Matrix::rowSums(A)
  zero_deg <- row_sums == 0
  row_sums[zero_deg] <- 1

  inv_D <- Matrix::Diagonal(x = 1 / row_sums)
  T_matrix <- inv_D %*% A
  if (any(zero_deg)) {
    zero_idx <- which(zero_deg)
    T_matrix[cbind(zero_idx, zero_idx)] <- 1
  }
  for (current_iter in 1:max_iter) {
    next_opinions <- as.numeric(T_matrix %*% opinion_array)
    next_opinions[influential_nodes] <- inf_opinions

    max_change <- max(abs(next_opinions - opinion_array))
    opinion_array <- next_opinions

    if (max_change < update_tolerance) {
      break
    }
  }

  if (current_iter == max_iter && max_change >= update_tolerance) {
    warning(sprintf(
      "Reached specified maximum number of iterations for propagation
      but total change is still bigger than tolerance.
      Last cycle's biggest opinion change is %f, tolerance is %f.",
      max_change, update_tolerance),
      call. = FALSE)
  }

  return(opinion_array)
}

calculate_dipole_moment <- function(graph, opinion_array) {
  v <- igraph::vcount(graph)
  v_plus <- sum(opinion_array > 0)
  v_minus <- v - v_plus

  delta_A <- abs(v_plus - v_minus) / v

  gc_plus <- mean(opinion_array[opinion_array > 0])
  gc_minus <- mean(opinion_array[opinion_array < 0])

  d <- abs(gc_plus - gc_minus) / 2
  dipole <- (1 - delta_A) * d

  return(dipole)
}




#' Dipole Moment
#'
#' @description Calculates the dipole moment (Morales) to quantify structural polarization in a network.
#'
#' @param membership A list/vector of group memberships, or a string for a vertex attribute.
#' @param graph An \code{igraph} graph object representing the network.
#' @param ego_included Logical. If TRUE, a node's current opinion is factored into its neighborhood average.
#' @param update_tolerance Numeric. Convergence threshold between 0 and 1.
#' @param max_iter Integer. Maximum number of update cycles.
#' @param k_top Integer. Number of highest-degree nodes to select as influential.
#' @param influential_nodes Vector of integers. Indices of nodes with fixed extreme opinions.
#' @param balanced Logical. Ensure equal number of top-degree nodes from both communities.
#' @param mode Character. "in", "out", or NULL.
#' @param null_models List of \code{igraph} graph objects.
#'
#' @return A numeric dipole moment score.
#' @references Morales, Alfredo Jose, et al. "Measuring political polarization: Twitter shows the two sides of Venezuela." (2015).
#' @export
dipole_moment <- function(membership,
                          graph,
                          ego_included = TRUE,
                          update_tolerance = 1e-6,
                          max_iter = 10000,
                          k_top = 10,
                          influential_nodes = NULL,
                          balanced = TRUE,
                          mode = "in",
                          null_models = NULL) {


  if (!is.igraph(graph)) stop("Expected an igraph object for the graph parameter.")

  if (is.character(membership) && length(membership) == 1) {
    if (!membership %in% igraph::vertex_attr_names(graph)) {
      stop("There is no vertex property with the given group membership name.")
    }
    membership <- igraph::vertex_attr(graph, membership)
  }

  mem_factor <- as.factor(as.character(membership))
  uq_membership <- levels(mem_factor)
  membership <- as.integer(mem_factor)

  if (length(uq_membership) != 2) {
    stop(sprintf("Dipole moment can only be calculated with exactly two unique
                 membership values, given membership contains %d unique values.",
                 length(uq_membership)))
  }

  if (max_iter <= 0){
    stop("Expected a positive integer for max_iter.")
    }
  if (update_tolerance <= 0 || update_tolerance >= 1){
    stop("Expected a float in between 0 and 1 for 'update_tolerance'.")
    }
  if (!is.logical(ego_included)){
    stop("Expected a boolean value for 'ego_included'.")
    }

  if (!is.null(influential_nodes) && !is.null(k_top)) {
    stop("'influential_nodes' and 'k_top' are mutually exclusive arguments.")
  }

  if (!is.null(mode) && !(mode %in% c("in", "out"))) {
    stop("'mode' parameter must be a string either 'in', 'out' or NULL.")
  }

  if (!is.null(influential_nodes) && balanced) {
    warning("A list of influential nodes supplied with the parameter
            'balanced' = TRUE.
            Balance parameter will be ignored.", call. = FALSE)
  }

  if (!is.null(k_top)) {
    if (k_top <= length(uq_membership)) {
      stop(sprintf("Expected at least 1 influential node for each unique group.
                   Received %d influential nodes for %d groups.",
                   k_top, length(uq_membership)))
    }
    influential_nodes <- get_influential_nodes_top_k(graph,
                                                     membership,
                                                     mode = mode,
                                                     k_top = k_top,
                                                     balanced = balanced)
  } else {
    if (length(influential_nodes) == 0) {
      stop("Given influential nodes list does not contain any elements.")
    }
  }

  if (!is.null(null_models) && !all(sapply(null_models, inherits, "igraph"))) {
    stop("Null models can only contain igraph objects.")
  }

  opinion_array <- run_label_propagation(graph,
                                         influential_nodes,
                                         membership,
                                         ego_included,
                                         max_iter,
                                         update_tolerance,
                                         mode = mode)
  dipole <- calculate_dipole_moment(graph, opinion_array)

  if (is.null(null_models)) {
    return(dipole)
  } else {
    dipole_null_scores <- sapply(null_models, function(null_graph) {
      if (!is.null(k_top)) {
        inf_nodes_null <- get_influential_nodes_top_k(null_graph, membership,
                                                      mode = mode,
                                                      k_top = k_top,
                                                      balanced = balanced)
      } else {
        inf_nodes_null <- influential_nodes
      }
      opinion_array_null <- run_label_propagation(null_graph, inf_nodes_null,
                                                  membership,
                                                  ego_included,
                                                  max_iter,
                                                  update_tolerance,
                                                  mode)
      return(calculate_dipole_moment(null_graph, opinion_array_null))
    })

    return(dipole - mean(dipole_null_scores, na.rm = TRUE))
  }
}
