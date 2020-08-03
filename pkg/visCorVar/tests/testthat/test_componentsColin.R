
test_that("test of the function componentsColin",{
  
  mat_cor_comp1  = matrix(0,
                          nrow = 5,
                          ncol = 5)
  mat_cor_comp1[, 1] = c(1, 0.95, 0.9, 0.3, 0.4)
  mat_cor_comp1[, 2] = c(0.95, 1, 0.86, 0.3, 0.4)
  mat_cor_comp1[, 3] = c(0.9, 0.86, 1, 0.3, 0.4)
  mat_cor_comp1[, 4] = c(0.3, 0.3, 0.3, 1, 0.9)
  mat_cor_comp1[, 5] = c(0.4, 0.4, 0.4, 0.9, 1)
  
  
  mat_cor_comp2  = matrix(0,
                          nrow = 5,
                          ncol = 5)
  mat_cor_comp2[, 1] = c(1, 0.87, 0.92, 0.2, 0.3)
  mat_cor_comp2[, 2] = c(0.87, 1, 0.88, 0.2, 0.3)
  mat_cor_comp2[, 3] = c(0.92, 0.88, 1, 0.2, 0.3)
  mat_cor_comp2[, 4] = c(0.2, 0.2, 0.2, 1, 0.89)
  mat_cor_comp2[, 5] = c(0.3, 0.3, 0.3, 0.89, 1)
  
  cutoff_comp = 0.8
  
  
  res = componentsColin(mat_cor_comp1 = mat_cor_comp1,
                        mat_cor_comp2 = mat_cor_comp2,
                        cutoff_comp = cutoff_comp)
  
  cond1 = all(res[[1]] == 1:3)
  cond2 = all(res[[2]] == 4:5)
  cond3 = cond1 & cond2
  
  expect_true(cond3)
  
})
