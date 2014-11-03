context("Test reading graphs")



test_that("Invalid graph", {
  bad_g1 = list(list())
  bad_g2 = list(list(edges = 1L))
  bad_g3 = list(list(weights = 1))

  expect_error(write_graph(bad_g1, tempfile()))
  expect_error(write_graph(bad_g2, tempfile()))
  expect_error(write_graph(bad_g3, tempfile()))
})

test_that("Directory and file existence", {
  g1 = list(list(edges = 1L, weights = 1))

  file = tempfile()
  write(NULL,file) # create file

  expect_error(write_graph(g1, file)) # error on attempted overwrite  


  bad_dir = file.path(dirname(file),"doesnt_exist","graph.dot")
  expect_error(write_graph(g1, bad_dir())) # error if directory doesn't exist
})


test_that("File write then read", {
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

  g4 = list("A A" = list(edges  =2L,
                         weights=1),
            B     = list(edges  =1L,
                         weights=1),
            "C C" = list(edges  =1L,
                         weights=1))

  g5 = list(A = list(edges  =2L,
                     weights=1e10),
            B = list(edges  =integer(),
                     weights=numeric()))

  file = tempfile()
  write_graph(g1, file)
  expect_true(is_isomorphic(g1, read_graph(file)))
  
  file = tempfile()
  write_graph(g2, file)
  expect_true(is_isomorphic(g2, read_graph(file)))
  
  file = tempfile()
  write_graph(g3, file)
  expect_true(is_isomorphic(g3, read_graph(file)))
  
  file = tempfile()
  write_graph(g4, file)
  expect_true(is_isomorphic(g4, read_graph(file)))
  
  file = tempfile()
  write_graph(g5, file)
  expect_true(is_isomorphic(g5, read_graph(file)))
})
