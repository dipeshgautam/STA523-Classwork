context("Test reading graphs")

test_that("Bad graph files", {
  expect_error(read_graph("test_files/bad_graph1.dot"))
  expect_error(read_graph("test_files/bad_graph2.dot"))
  expect_error(read_graph("test_files/bad_graph3.dot"))
  expect_error(read_graph("test_files/bad_graph4.dot"))
  expect_error(read_graph("test_files/bad_graph5.dot"))
  expect_error(read_graph("test_files/bad_graph6.dot"))
  expect_error(read_graph("test_files/bad_graph7.dot"))
  expect_error(read_graph("test_files/bad_graph8.dot"))
  expect_error(read_graph("test_files/bad_graph9.dot"))
  expect_error(read_graph("test_files/bad_graph10.dot"))
})

test_that("Valid graph files", {

  g1 = list(A = list(edges  =2L,
                     weights=1),
            B = list(edges  =3L,
                     weights=1),
            C = list(edges  =5L,
                     weights=1),
            D = list(edges  =2L,
                     weights=1),
            E = list(edges  =c(6L,4L),
                     weights=c(1,1)),
            F = list(edges=integer(),
                     weights=numeric()))           
  
  expect_true(is_isomorphic(g1, read_graph("test_files/graph1.dot")))
  expect_true(is_isomorphic(g1, read_graph("test_files/graph2.dot")))


  g3 = list(A = list(edges  =2L,
                     weights=14),
            B = list(edges  =c(3L,4L),
                     weights=c(23,13)),
            D = list(edges  =1L,
                     weights=5),
            F = list(edges  =c(1L,5L),
                     weights=c(43,33)),
            N = list(edges  =c(1L,2L,4L),
                     weights=c(33,22,11)))

  expect_true(is_isomorphic(g2, read_graph("test_files/graph3.dot")))


  g4 = list("A A" = list(edges  =2L,
                         weights=1),
            B     = list(edges  =1L,
                         weights=1),
            "C C" = list(edges  =1L,
                         weights=1))
  
  expect_true(is_isomorphic(g4, read_graph("test_files/graph4.dot")))


  g5 = list(A = list(edges  =2L,
                     weights=1e10),
            B = list(edges  =integer(),
                     weights=numeric()))

  expect_true(is_isomorphic(g5, read_graph("test_files/graph5.dot")))
})




