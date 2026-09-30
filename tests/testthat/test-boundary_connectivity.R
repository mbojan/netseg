testthat::context("Testing Boundary Connectivity")


testthat::test_that("Testing the correct value for WhiteKinship data",{

  bc <- boundary_connectivity("gender",WhiteKinship, relax = FALSE)
  testthat::expect_equal(bc, 0.15)
  # Manual calculation returns 0.15 for this network.
  # There are three boundary nodes (Brother's Daughter,
  # Brother's Son, Sister's Son) They have (0.6, 0.75, 0.6)
  # d_i / d_i + d_b ratio. Mean of this is 0.65, minus 0.5
  # would be equal to 0,15.

})

testthat::test_that("Testing boundary node detection",{
  bnodes <- get_boundary_nodes(WhiteKinship,
                               igraph::get.vertex.attribute(WhiteKinship,
                                                            "gender"))
  testthat::expect_equal(length(bnodes$name), 3)
  testthat::expect_equal(sum(bnodes$name == c("Brother's Daughter",
                                              "Brother's Son",
                                              "Sister's Son")), 3 )


  # The above equation means that there are exactly 3 boundary nodes
  # with the above names.

})

testthat::test_that("Testing boundary node detection for directed graphs",{
  # mode definition out test first
  bnodes <- get_boundary_nodes(Classroom,
                               igraph::get.vertex.attribute(Classroom, "gender"),
                               mode = "out")
  testthat::expect_equal(length(bnodes$name), 2)
  testthat::expect_equal(sum(bnodes$name == c(1006,1042)), 2)
  # mode definition in test
  bnodes <- get_boundary_nodes(Classroom,
                               igraph::get.vertex.attribute(Classroom, "gender"),
                               mode = "in")
  testthat::expect_equal(length(bnodes$name), 2)
  testthat::expect_equal(sum(bnodes$name == c(1006,1042)), 2)
})

testthat::test_that("boundary_connectivity warns and returns the correct value for Classroom data", {
  # Since the graph is not weakly connected, it might result
  # with unexpected behaviour warnings should come up.
  expected_warning_message <- "Graph is not weakly connected, this might result in unexpected behaviour"

  testthat::expect_warning(
    boundary_connectivity_value <- boundary_connectivity("gender", Classroom, mode = "out"),
    regexp = expected_warning_message,
    fixed  = TRUE
  )
  # Below is the manual value that we would expect when the
  # mode is out.
  testthat::expect_equal(boundary_connectivity_value, 0.25)
})






