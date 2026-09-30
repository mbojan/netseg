get_boundary_nodes_relaxed <- function(graph, membership, mode = NULL) {

  is_directed <- igraph::is_directed(graph) # Check the directionality
  edges <- igraph::ends(graph, igraph::E(graph), names = FALSE) # Gets edgelist

  if (nrow(edges) == 0) {
    return(integer(0)) # No edges should return zero boundary nodes by definition
  }

  if (!is_directed) {
    if (!is.null(mode)) {
      # If mode is given but the network is undirected, raises warning
      warning("An undirected graph is given as an argument but a mode for directed
              definition of boundary is stated, mode will be ignored", call. = FALSE)

    }
    s <- edges[, 1] # source nodes
    t <- edges[, 2] # target nodes


    cross_mask <- membership[s] != membership[t] # different membership nodes.
    has_outside <- logical(igraph::vcount(graph)) # initiate for each node
    has_outside[c(s[cross_mask], t[cross_mask])] <- TRUE # these are the ones w out

    internal_mask <- membership[s] == membership[t] # These are internal nodes
    has_inside <- logical(igraph::vcount(graph))
    has_inside[c(s[internal_mask], t[internal_mask])] <- TRUE

    boundary_nodes <- which(has_outside & has_inside)
    # this is relax definition and simple, we do not care if the other one
    # is also a boundary node.


    return(igraph::V(graph)[as.integer(boundary_nodes)])


  } else {
    if (is.null(mode)) {
      stop("A directed graph is passed as an argument but 'mode' is NULL")
    }

    s <- edges[, 1]
    t <- edges[, 2]
    mask <- membership[s] != membership[t]
    mutual_mask <- igraph::which_mutual(graph)

    valid_inter_edges <- mutual_mask & mask
    # lets pull the candidates first.
    candidates <- unique(c(s[valid_inter_edges], t[valid_inter_edges]))

    same_group_mask <- membership[s] == membership[t]
    # apply the same version but according to the mode.
    if (mode == "out") {
      valid_inner_nodes <- unique(s[same_group_mask])
    } else if (mode == "in") {
      valid_inner_nodes <- unique(t[same_group_mask])
    } else {
      stop("'mode' can only be 'in' or 'out'.")
    }

    boundary_nodes <- intersect(candidates, valid_inner_nodes)

    return(igraph::V(graph)[as.integer(boundary_nodes)])
  }
}

get_boundary_nodes <- function(graph, membership, mode = NULL) {
  # This function is updated and it returns a vertex sequence now as suggested.
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
    # everything is the same as the relaxed definition
    # but in this case, one of the nodes should be boundary
    # and the other one should be internal.
    # So we pull an xor operator, it returns TRUE only
    # two nodes are different type.
    boundary_edge_mask <- xor(s_is_cand, t_is_cand)

    s_bound <- s[boundary_edge_mask]
    t_bound <- t[boundary_edge_mask]
    s_cand_filtered <- s_is_cand[boundary_edge_mask]

    boundary_nodes <- ifelse(s_cand_filtered, s_bound, t_bound)

    return(igraph::V(graph)[as.integer(unique(boundary_nodes))])

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

    return(igraph::V(graph)[as.integer(unique(valid_boundary_nodes))])
  }
}

count_bc_balance <- function(graph, boundary_nodes, membership, mode = NULL) {
  # This is the math definition of the metric. In this function we
  # just calculate the ratio.
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

    # for each node assign bc balance score

  for (i in seq_along(boundary_nodes)) {
    b_node <- boundary_nodes[i]
    neighbors <- as.numeric(neigh_list[[i]])

    if (length(neighbors) == 0) {
      # If is disconnected, this is a check, by definition
      # this should not be possible. But in directed
      # version this might be possible.
      di_count <- 0
      db_count <- 0

    } else {
      neighbor_is_bound <- is_boundary[neighbors]
      diff_community <- membership[b_node] != membership[neighbors]

      # basic counting.
      di_count <- sum(!neighbor_is_bound & !diff_community)
      # According to the original paper's definition at p.5
      db_count <- sum(diff_community)

    }

    bc_balance[i] <- (di_count) / (di_count + db_count)
  }

  return(bc_balance)
}

process_null_graph_bc <- function(null_graph, membership, relax, mode = NULL) {
  # basically we are applying the same function again and again in the null
  # models.
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
#' @description Calculates boundary connectivity for networks with an arbitrary
#' number of groups. Boundary connectivity captures the behaviour of boundary
#' nodes, i.e. nodes that interact with the other groups.
#' A node is a boundary node if and only if it has at least one edge to a node
#' in a different group and at least one edge to a member of its own group that
#' is not connected to any node belonging to another group.
#' The index is computed as the average, across boundary nodes, of the ratio of
#' within-group edges to total edges. An expected value under a null model is
#' then subtracted from this average.
#'
#' @details
#' When \code{relax} = TRUE the function uses the relaxed definition of a node is
#' considered on the boundary if it is connected to a group other than its own
#' and is also connected to at least one node from its own group, even if the
#' node it is connected to is also a boundary node.
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
    # not receiving igraph object case
    stop(sprintf("Expected an igraph object for the graph parameter, received %s instead.", class(graph)[1]))
  }

  if (!is.null(null_models)) {
    if (!all(sapply(null_models, inherits, "igraph"))) {
      stop("Null models can only contain igraph objects.")
    }
  }

  if (length(membership) != igraph::vcount(graph)) {
    # If membership list does not match the number of vertices.
    stop(sprintf("Shape mismatch: %d memberships for %d vertices.", length(membership), igraph::vcount(graph)))
  }

  if (!is.numeric(membership)) {
    # If membership is given as non numeric, forcing it to string, factor for
    # transforming them ordered numbers and as integer to make them numbers
    membership <- as.integer(as.factor(as.character(membership)))
  }

  if (!is.logical(relax)) {
    stop("'relax' parameter can only be a logical (boolean)")
  }
  # below we get the boundary nodes according to definition.
  if (relax) {
    boundary_nodes <- get_boundary_nodes_relaxed(graph, membership, mode = mode)
  } else {
    boundary_nodes <- get_boundary_nodes(graph, membership, mode = mode)
  }
  #If there are no boundary nodes, internal function returns zero.
  #Since the metric is not defined, returning NA_real_ is appropriate.
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
    # If null models are given, we are calculating the expected value
    # from the counts.
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


