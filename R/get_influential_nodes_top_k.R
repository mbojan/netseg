get_influential_nodes_top_k <- function(graph,
                                        membership,
                                        mode = "out",
                                        k_top = 10,
                                        balanced = FALSE) {

  degs <- igraph::degree(graph, mode = ifelse(is.null(mode), "all", mode))

  if (k_top >= length(degs)) {
    stop(sprintf("Number of influential nodes required (%d) is more
                 than the total number of nodes in the graph (%d)", k_top, length(degs)))
  } else if (k_top >= length(degs) / 4) {
    # This is only rational I believe, picking quarter of the nodes as
    # influential nodes is not a good practice, raising a warning is
    # beneficial for the user in here.
    warning("Number of influential nodes is bigger than a quarter
            of the nodes in the graph. This might cause overestimation of
            the polarization.", call. = FALSE)
  }

  if (!balanced) {

    idx <- order(degs, decreasing = TRUE)[1:k_top]
    return(idx)
  } else {
    uq_memberships <- unique(membership)
    n_uq_memberships <- length(uq_memberships)
    balanced_inf_nodes <- integer(0)

    k_per_community <- k_top %/% n_uq_memberships
    #If it is the balanced approach, we are sharing the k_top
    # with number of unique groups. Nonetheless I am not sure
    # whether we should do this strict and if it is not divisible
    # raise an error, because this can still find influential
    # nodes that is not balanced, since it is direct
    # integer division, it might affect the score, I am leaving this
    # to you.

    for (mem in uq_memberships) {
      sub_mem_mask <- membership == mem
      sub_indices <- which(sub_mem_mask)
      sub_degrees <- degs[sub_indices]

      if (length(sub_degrees) < k_per_community) {
        stop(sprintf("Some of the groups do not have enough nodes to have
                        %d per group influential nodes. Try decreasing the
                        number of influential nodes.",
                        k_per_community))
      }

      take_k <- min(k_per_community, length(sub_degrees))
      idx <- order(sub_degrees, decreasing = TRUE)[1:take_k]
      balanced_inf_nodes <- c(balanced_inf_nodes, sub_indices[idx])
    }
    return(balanced_inf_nodes)
  }
}
