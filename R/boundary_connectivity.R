get_boundary_nodes_relaxed <- function(graph, membership, mode = NULL) {
  is_directed <- igraph::is_directed(graph)
  edges <- igraph::ends(graph, igraph::E(graph), names = FALSE)

  if (nrow(edges) == 0) {
    return(integer(0))
  }

  if (!is_directed) {
    if (!is.null(mode)) {
      warning("An undirected graph is given as an argument but a mode for directed
              definition of boundary is stated, mode will be ignored", call. = FALSE)
    }
    s <- edges[, 1]
    t <- edges[, 2]
    # This is the fastest way I could think because
    # it has a direct logical condition similar to
    # the original implementation netseg-python.

    cross_mask <- membership[s] != membership[t]
    has_outside <- logical(igraph::vcount(graph))
    has_outside[c(s[cross_mask], t[cross_mask])] <- TRUE

    internal_mask <- membership[s] == membership[t]
    has_inside <- logical(igraph::vcount(graph))
    has_inside[c(s[internal_mask], t[internal_mask])] <- TRUE

    boundary_nodes <- which(has_outside & has_inside)
    return(as.integer(boundary_nodes))

  } else {
    if (is.null(mode)) {
      stop("A directed graph is passed as an argument but 'mode' is NULL")
    }

    s <- edges[, 1]
    t <- edges[, 2]
    mask <- membership[s] != membership[t]
    mutual_mask <- igraph::which_mutual(graph)

    valid_inter_edges <- mutual_mask & mask
    candidates <- unique(c(s[valid_inter_edges], t[valid_inter_edges]))

    same_group_mask <- membership[s] == membership[t]

    if (mode == "out") {
      valid_inner_nodes <- unique(s[same_group_mask])
    } else if (mode == "in") {
      valid_inner_nodes <- unique(t[same_group_mask])
    } else {
      stop("'mode' can only be 'in' or 'out'.")
    }

    boundary_nodes <- intersect(candidates, valid_inner_nodes)
    return(as.integer(boundary_nodes))
  }
}

get_boundary_nodes <- function(graph, membership, mode = NULL) {
  is_directed <- igraph::is_directed(graph)
  edges <- igraph::ends(graph, igraph::E(graph), names = FALSE)

  if (nrow(edges) == 0) {
    return(integer(0))
  }

  if (!is_directed) {
    if (!is.null(mode)) {
      warning("An undirected graph is given as an argument but a mode for
              directed definition of boundary is stated, mode will be ignored",
              call. = FALSE)
    }
    s <- edges[, 1]
    t <- edges[, 2]

    mask <- membership[s] != membership[t]
    is_candidate <- logical(igraph::vcount(graph))
    is_candidate[c(s[mask], t[mask])] <- TRUE

    s_is_cand <- is_candidate[s]
    t_is_cand <- is_candidate[t]

    boundary_edge_mask <- xor(s_is_cand, t_is_cand)

    s_bound <- s[boundary_edge_mask]
    t_bound <- t[boundary_edge_mask]
    s_cand_filtered <- s_is_cand[boundary_edge_mask]

    boundary_nodes <- ifelse(s_cand_filtered, s_bound, t_bound)
    return(as.integer(unique(boundary_nodes)))

  } else {
    if (is.null(mode)) {
      stop("A directed graph is passed as an argument but 'mode' is NULL")
    }
    s <- edges[, 1]
    t <- edges[, 2]
    diff_group_mask <- membership[s] != membership[t]
    mutual_mask <- igraph::which_mutual(graph)

    valid_inter_edges <- diff_group_mask & mutual_mask
    candidates <- unique(c(s[valid_inter_edges], t[valid_inter_edges]))

    if (!mode %in% c("in", "out")) {
      return(as.integer(candidates))
    }

    is_candidate <- logical(igraph::vcount(graph))
    is_candidate[candidates] <- TRUE

    same_group_mask <- membership[s] == membership[t]
    s_same <- s[same_group_mask]
    t_same <- t[same_group_mask]

    s_is_cand <- is_candidate[s_same]
    t_is_cand <- is_candidate[t_same]

    if (mode == "out") {
      valid_condition <- s_is_cand & (!t_is_cand)
      valid_boundary_nodes <- s_same[valid_condition]
    } else if (mode == "in") {
      valid_condition <- t_is_cand & (!s_is_cand)
      valid_boundary_nodes <- t_same[valid_condition]
    } else {
      stop("'mode' can be either 'in' or 'out'")
    }

    return(as.integer(unique(valid_boundary_nodes)))
  }
}

count_bc_balance <- function(graph, boundary_nodes, membership, mode = NULL) {

  if (igraph::is_directed(graph) && !is.null(mode)) {
    neigh_list <- igraph::ego(graph, order = 1,
                              nodes = boundary_nodes,
                              mode = mode,
                              mindist = 1)
  } else {
    neigh_list <- igraph::ego(graph, order = 1,
                              nodes = boundary_nodes,
                              mindist = 1)
  }

  is_boundary <- logical(igraph::vcount(graph))
  is_boundary[boundary_nodes] <- TRUE

  bc_balance <- numeric(length(boundary_nodes))

  for (i in seq_along(boundary_nodes)) {
    b_node <- boundary_nodes[i]
    neighbors <- as.numeric(neigh_list[[i]])

    if (length(neighbors) == 0) {
      di_count <- 0
      db_count <- 0
    } else {
      neighbor_is_bound <- is_boundary[neighbors]
      diff_community <- membership[b_node] != membership[neighbors]

      di_count <- sum(!neighbor_is_bound)
      db_count <- sum(neighbor_is_bound & diff_community)
    }

    bc_balance[i] <- (di_count + 0.0001) / (di_count + db_count + 0.0001)
  }

  return(bc_balance)
}

process_null_graph_bc <- function(null_graph, membership, relax, mode = NULL) {
  if (relax) {
    boundary_nodes <- get_boundary_nodes_relaxed(null_graph, membership, mode = mode)
  } else {
    boundary_nodes <- get_boundary_nodes(null_graph, membership, mode = mode)
  }

  if (length(boundary_nodes) == 0) {
    return(NA_real_)
  }

  bc_balance_null <- count_bc_balance(null_graph, boundary_nodes, membership, mode = mode)
  return(mean(bc_balance_null, na.rm = TRUE))
}



#' Boundary Connectivity
#'
#' @description Calculates the Boundary Connectivity with an arbitrary number of groups.
#' Boundary connectivity is designed to capture the behavior of the nodes
#' on the boundary—nodes that effectively interact with the (potentially)
#' opposing group.
#'
#' @param membership A vector of integers representing group memberships, or a string for a vertex attribute.
#' @param graph An \code{igraph} graph object.
#' @param null_models A list of \code{igraph} graph objects for baseline comparison.
#' @param relax Logical. If TRUE, uses the relaxed definition of a boundary node.
#' @param mode Character. "out", "in", or NULL. Determines directionality requirement.
#'
#' @return A numeric Boundary Connectivity score, or NA if no boundary nodes exist.
#' @references Guerra, Pedro, et al. "A measure of polarization on social media networks based on community boundaries." (2013).
#' @export
boundary_connectivity <- function(membership, graph, null_models = NULL, relax = FALSE, mode = NULL) {

  if (!igraph::is_connected(graph, mode = "weak")) {
    warning("Graph is not weakly connected, this might result in unexpected behaviour", call. = FALSE)
  }

  if (is.character(membership)) {
    # This case we directly pull from the exogenous nodal attribute
    if (!membership %in% igraph::vertex_attr_names(graph)) {
      stop("There is no vertex property with the given group membership name in the graph")
    }
    membership <- igraph::vertex_attr(graph, membership)
  }

  if (!igraph::is_igraph(graph)) {
    # Type of an igraph object is a list? So I used is_igraph from igraph as a logical check. If you
    # know something better for this please edit this section.
    stop(sprintf("Expected an igraph object for the graph parameter, received %s instead.", class(graph)[1]))
  }

  if (!is.null(null_models)) {
    if (!all(sapply(null_models, inherits, "igraph"))) {
      stop("Null models can only contain igraph objects.")
    }
  }

  if (length(membership) != igraph::vcount(graph)) {
    stop(sprintf("Shape mismatch: %d memberships for %d vertices.", length(membership), igraph::vcount(graph)))
  }

  if (!is.numeric(membership)) {
    membership <- as.integer(as.factor(as.character(membership)))
  }

  if (!is.logical(relax)) {
    stop("'relax' parameter can only be a logical (boolean)")
  }

  if (relax) {
    boundary_nodes <- get_boundary_nodes_relaxed(graph, membership, mode = mode)
  } else {
    boundary_nodes <- get_boundary_nodes(graph, membership, mode = mode)
  }

  if (length(boundary_nodes) == 0) {
    warning("There are no boundary nodes in the graph, returning NA.", call. = FALSE)
    return(NA_real_)
  }

  if (is.null(null_models)) {
    bc_balance <- count_bc_balance(graph, boundary_nodes, membership = membership, mode = mode)
    return(mean(bc_balance, na.rm = TRUE) - 0.5)

  } else {
    bc_balance <- count_bc_balance(graph, boundary_nodes, membership = membership, mode = mode)
    bc_score <- mean(bc_balance, na.rm = TRUE)

    expected_ratio <- sapply(null_models, function(nm) {
      process_null_graph_bc(nm, membership, relax, mode)
    })

    if (any(is.na(expected_ratio))) {
      warning("At least one null-model does not contain boundary nodes.", call. = FALSE)
    }

    expected_ratio_mean <- mean(expected_ratio, na.rm = TRUE)

    if (is.na(expected_ratio_mean)) {
      stop("None of the null-models contain boundary nodes.")
    } else {
      return(bc_score - expected_ratio_mean)
    }
  }
}

# -- Notes

# This is the fastest way I could find, currently it does not add any more dependencies than the original igraph.
# But that is a requirement for netseg anyway.
# One issue that I do not understand is that it swaps the position of the first index boundary node and second index
# boundary node. I believe this is due to logical conditions running, and I am adding it in reverse. This is not an
# issue since the ordering will not matter when it comes to the calculation.

