context("Checking multigraphs")

library(igraph)
g <- igraph::make_graph(~ A --+ B, A --+ B, B --+ c, simplify = FALSE)
V(g)$type <- c(1, 1, 2)
plot(g)
mixingm(g, "type")
a <- mixingm(g, "type", full=TRUE)
d <- as.data.frame(as.table(a))

# Null tie-based model
fit1 <- glm(Freq ~ ego*tie + alter*tie + ego*alter, data=d, family=poisson("log"))
predict(fit1, type="response")

# Null contact model
fit2 <- glm(Freq ~ ego + alter, data=d, family=poisson("log"), subset=tie == "TRUE")
predict(fit2, type="response")
