test_that("test of the function computeBlockSelect",{

  names_blocks = paste0("X", 1:5)
  
  mat_cor_comp1  = matrix(0,
                          nrow = 5,
                          ncol = 5)
  mat_cor_comp1[, 1] = c(1, 0.95, 0.9, 0.3, 0.4)
  mat_cor_comp1[, 2] = c(0.95, 1, 0.86, 0.3, 0.4)
  mat_cor_comp1[, 3] = c(0.9, 0.86, 1, 0.3, 0.4)
  mat_cor_comp1[, 4] = c(0.3, 0.3, 0.3, 1, 0.9)
  mat_cor_comp1[, 5] = c(0.4, 0.4, 0.4, 0.9, 1)
  rownames(mat_cor_comp1) = colnames(mat_cor_comp1) = names_blocks
  

  mat_cor_comp2  = matrix(0,
                          nrow = 5,
                          ncol = 5)
  mat_cor_comp2[, 1] = c(1, 0.87, 0.92, 0.2, 0.3)
  mat_cor_comp2[, 2] = c(0.87, 1, 0.88, 0.2, 0.3)
  mat_cor_comp2[, 3] = c(0.92, 0.88, 1, 0.2, 0.3)
  mat_cor_comp2[, 4] = c(0.2, 0.2, 0.2, 1, 0.89)
  mat_cor_comp2[, 5] = c(0.3, 0.3, 0.3, 0.89, 1)
  rownames(mat_cor_comp2) = colnames(mat_cor_comp2) = names_blocks
  
  cutoff_comp = 0.8


  res = computeBlockSelect(mat_cor_comp1 = mat_cor_comp1,
                           mat_cor_comp2 = mat_cor_comp2,
                           cutoff_comp = cutoff_comp)

  list_vec_names_blocks = res$list_vec_names_blocks
  
  list_vec_names_blocks_test = list()
  list_vec_names_blocks_test[[1]] = "X1"
  list_vec_names_blocks_test[[2]] = "X2"
  list_vec_names_blocks_test[[3]] = "X3"
  list_vec_names_blocks_test[[4]] = c("X1", "X2")
  list_vec_names_blocks_test[[5]] = c("X1", "X3")
  list_vec_names_blocks_test[[6]] = c("X2", "X3")
  list_vec_names_blocks_test[[7]] = c("X1", "X2", "X3")
  list_vec_names_blocks_test[[8]] = "X4"
  list_vec_names_blocks_test[[9]] = "X5"
  list_vec_names_blocks_test[[10]] = c("X4", "X5")
  
  vec_cond = c() 
  
  for(i in 1:length(list_vec_names_blocks))
  {
    names_blocks_i = list_vec_names_blocks[[i]]
    
    cond_i = FALSE
    
    for(j in 1:length(list_vec_names_blocks_test))
    {
      names_blocks_test_j = list_vec_names_blocks_test[[j]]
      
      if(length(names_blocks_i) == length(names_blocks_test_j))
      {
        if(all(sort(names_blocks_i) == sort(names_blocks_test_j)))
        {
          cond_i = TRUE
          
        }
        
      }
      
    } # End for(j in 1:length(list_vec_names_blocks_test)).
    
    vec_cond = c(vec_cond, cond_i)
    
  } # End for(i in 1:length(list_vec_names_blocks)).
  
  cond = all(vec_cond == TRUE)

  expect_true(cond)

})
