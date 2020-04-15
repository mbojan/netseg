context("Checking multigraphs")


g <- igraph::make_graph(~ A --+ B, A --+ B, C, simplify = FALSE)
V(g)$type <- c(1, 1, 2)
plot(g)
mixingm(g, "type")
mixingm(g, "type", full=TRUE)
