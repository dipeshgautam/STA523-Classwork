context("Test PageRank")

g1 = list(list(edges   = c(1L, 2L),
               weights = c(1, 1)),
          list(edges   = c(1L, 2L),
               weights = c(1, 1)))

g2 = list(list(edges   = c(2L),
               weights = c(1)),
          list(edges   = c(1L),
               weights = c(1)))

g3 = list(list(edges   = c(1L),
               weights = c(1)),
          list(edges   = c(2L),
               weights = c(1)))

bad_g1 = list(list(edges   = c(1L, 2L),
                   weights = c(0, 0)),
              list(edges   = c(1L, 2L),
                   weights = c(0, 0)))

test_that("Adjacency Matrix", {
    expect_equivalent(get_adj_mat(g1), matrix(c(1,1,1,1),ncol=2,nrow=2))
    expect_equivalent(get_adj_mat(g2), matrix(c(0,1,1,0),ncol=2,nrow=2))
    expect_equivalent(get_adj_mat(g3), matrix(c(1,0,1,0),ncol=2,nrow=2))
})

test_that("Transition Matrix", {
    expect_equivalent(get_adj_mat(g1, TRUE), matrix(c(0.5,0.5,0.5,0.5),ncol=2,nrow=2))
    expect_equivalent(get_adj_mat(g2, TRUE), matrix(c(0,1,1,0),ncol=2,nrow=2))
    expect_equivalent(get_adj_mat(g3, TRUE), matrix(c(1,0,1,0),ncol=2,nrow=2))
})

test_that("Adjacency Matrix - Validity", {
    expect_error(get_adj_mat(bad_g1), "is_valid")
    expect_error(get_adj_mat(1), "is_valid")
    expect_error(get_adj_mat(list()), "is_valid")
    expect_error(get_adj_mat(list(list())), "is_valid")
})



test_that("Stationary Distribution", {
    t1 = get_adj_mat(g1,TRUE)
    t2 = get_adj_mat(g2,TRUE)
    t3 = get_adj_mat(g3,TRUE)

    expect_equivalent(get_stationary_dist(t1), matrix(c(0.5,0.5,0.5,0.5),ncol=2,nrow=2))
    expect_equivalent(get_stationary_dist(t2), matrix(c(1,0,0,1),ncol=2,nrow=2))
    expect_equivalent(get_stationary_dist(t3), matrix(c(1,0,0,1),ncol=2,nrow=2))

    expect_error(get_stationary_dist(get_adj_mat(g1)), "rowSums")
})


test_that("PageRank", {
    t1 = get_adj_mat(g1,TRUE)
    
    bad_t1 = get_adj_mat(g1)

    i1 = matrix(c(1,0),ncol=2)
    i2 = matrix(c(0,1),ncol=2)
    i3 = matrix(c(0.5,0.5),ncol=2)

    bad_i1 = matrix(c(1,1),ncol=2)
    bad_i2 = matrix(c(0.5,0.5),nrow=2)
    bad_i3 = matrix(c(1),ncol=1)
    

    expect_equal(dim(page_rank(t1,i1)), c(1,2)) 
    expect_equal(dim(page_rank(t1,i2)), c(1,2)) 
    expect_equal(dim(page_rank(t1,i3)), c(1,2)) 

    expect_equal(sum(page_rank(t1,i1)), 1) 
    expect_equal(sum(page_rank(t1,i2)), 1) 
    expect_equal(sum(page_rank(t1,i3)), 1) 

    expect_error(page_rank(bad_t1,i1))

    expect_error(page_rank(t1,bad_i1))
    expect_error(page_rank(t1,bad_i2))
    expect_error(page_rank(t1,bad_i3))
})


