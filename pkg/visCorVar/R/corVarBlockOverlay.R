# The function matrixCorComp computes the correlation matrix between the comp[1] components and the 
# correlation matrix between the comp[2] components.

matrixCorComp <-function(res_block_splsda,
                         comp = 1:2)
{
  names_blocks_temp = res_block_splsda$names$blocks
  index_Y = which(names_blocks_temp == "Y")
  names_blocks = names_blocks_temp[- index_Y]
  vec_index_blocks = sapply(1:length(names_blocks), FUN = function(i){
    res = which(res_block_splsda$names$blocks == names_blocks[i])
    
    return(res)
  })
  
  # Computation of the correlation matric between the comp[1] components of each block
  # and the correlation matrix between the comp[2] components of each block.
  
  mat_comp1 = sapply(1:length(vec_index_blocks), FUN = function(i){
    res = res_block_splsda$variates[[vec_index_blocks[i]]][, comp[1]]
    
    return(res)
  })
  
  colnames(mat_comp1) = names_blocks
  mat_cor_comp1 = cor(mat_comp1)
  
  mat_comp2 = sapply(1:length(vec_index_blocks), FUN = function(i){
    res = res_block_splsda$variates[[vec_index_blocks[i]]][, comp[2]]
    
    return(res)
  })
  
  colnames(mat_comp2) = names_blocks
  mat_cor_comp2 = cor(mat_comp2)
  
  return(list(mat_cor_comp1 = mat_cor_comp1,
              mat_cor_comp2 = mat_cor_comp2))
}


# The function componentsColin returns a list. The ith element of this list contains the
# indices of the blocks such that, for each pair of block, the comp[1] component of block 1
# is highly correlated with the comp[1] component of block 2 in absolute value and the comp[2] component
# of block 1 is highly correlated with comp[2] component of block 2 in absolute value.

componentsColin <-function(mat_cor_comp1,
                           mat_cor_comp2,
                           cutoff_comp)
{
  res = list()
  
  index = 1:dim(mat_cor_comp1)[1]
  i = 1
  compt = 1
  
  while(length(index) != 0 & compt <= dim(mat_cor_comp1)[1])
  {
    index_i = index[i]
    res[[compt]] = c(index_i)
    
    index2 = index[-i]
    
    if(length(index2) != 0)
    {
      
      for(j in index2)
      {
        if(length(res[[compt]]) == 1)
        {
          if(abs(mat_cor_comp1[j, index_i]) > cutoff_comp & abs(mat_cor_comp2[j, index_i]) > cutoff_comp)
          {
            res[[compt]] = c(res[[compt]], j)
            index = index[- c(which(index == j))]
            
          }
          
          # End if(length(res[[compt]]) == 1).
        }else{
          
          index3 = sapply(1:length(res[[compt]]), FUN = function(k){
            index_k = res[[compt]][k]
            cond = abs(mat_cor_comp1[index_k, j]) > cutoff_comp & abs(mat_cor_comp2[index_k, j]) > cutoff_comp
            
            return(cond)
          })
          
          if(all(index3))
          {
            res[[compt]] = c(res[[compt]], j)
            index = index[- c(which(index == j))]
            
          } # End if(all(index3)).
          
          
        }
        
      } # End for(j in index2).
      
      # End if(length(index2) != 0).
    }else{
      res[[compt]] = index_i
      
    }
    
    index = index[- c(which(index == index_i))]
    compt = compt + 1
    
  } # End while(length(index) != 0 & compt <= dim(mat_cor_comp1)[1]).
  
  if(length(index) != 0)
  {
    for(i in 1:length(index))
    {
      res = c(res, list(index[i]))
      
    }
    
  }
  
  return(res)
}


# The function computeBlockSelect determines all the possible combinations of blocks
# whose correlation circles can be overlaid.

computeBlockSelect <-function(mat_cor_comp1,
                              mat_cor_comp2,
                              cutoff_comp)
{
  
  list_vec_index_block_select = componentsColin(mat_cor_comp1,
                                                mat_cor_comp2,
                                                cutoff_comp)
  
  names_blocks = colnames(mat_cor_comp1)
  list_vec_names_blocks = list()
  
  # list_vec_names_blocks is a list containing all the possible combinations of blocks
  # whose correlation circles can be overlaid.
  
  for(i in 1:length(list_vec_index_block_select))
  {
    vec_index_block_select_i = list_vec_index_block_select[[i]]
    
    for(k in 1:length(vec_index_block_select_i))
    {
      mat_comb = combn(vec_index_block_select_i, m = k) 
      
      list_vec_names_blocks_i = lapply(1:dim(mat_comb)[2], FUN = function(i){
        comb_i = mat_comb[, i]
        res = names_blocks[comb_i]
        
        return(res)
      })
      
      for(j in 1:length(list_vec_names_blocks_i))
      {
        list_vec_names_blocks = c(list_vec_names_blocks, list(list_vec_names_blocks_i[[j]]))
        
      } # End for(j in 1:length(list_vec_blocks_i)).
      
    } # End for(k in 1:length(vec_index_block_select_i)).
    
  } # End for(i in 1:length(list_vec_index_block_select)).
  
  list_vec_names_blocks = unique(list_vec_names_blocks)
  
  return(list(list_vec_index_block_select = list_vec_index_block_select,
              list_vec_names_blocks = list_vec_names_blocks))
  
}


# The function computeCorCompVar computes, for each selected block variable, the correlations between
# this selected block variable and the components (whose indices are in comp).

computeCorCompVar <-function(res_block_splsda,
                             comp = c(1:2))
{
  name_blocks_temp = res_block_splsda$names$blocks
  index_Y = which(name_blocks_temp == "Y")
  name_blocks1 = name_blocks_temp[- index_Y]
  index_blocks1 = sapply(1:length(name_blocks1), FUN = function(i){
    res = which(res_block_splsda$names$blocks == name_blocks1[i])
    
    return(res)
  })
  
  # Computation, for each selected block variable, the correlations between this
  # variable and the components (whose indices are in comp).
  
  vec_names_blocks2 = c()
  list_cor_comp_var_global = list()
  
  for(i in 1:length(index_blocks1))
  {
    index_blocks1_i = index_blocks1[i]
    name_block_i = res_block_splsda$names$blocks[index_blocks1_i]
    
    vec_var_select_i = c()
    list_comp_i = list()
    
    for(j in 1:length(comp))
    {
      index_comp_j = comp[j]
      list_comp_i[[j]] = res_block_splsda$variates[[index_blocks1_i]][, index_comp_j]
      vec_var_select_comp_i_j = selectVar(res_block_splsda,
                                          comp = index_comp_j)[[index_blocks1_i]][[1]]
      
      vec_var_select_i = c(vec_var_select_i, vec_var_select_comp_i_j)
      
    } # End for(j in 1:length(comp)).
    
    vec_var_select_i = unique(vec_var_select_i)
    
    block_i = res_block_splsda$X[[index_blocks1_i]][, vec_var_select_i]
    
    list_cor_comp_var = list()
    
    for(j in 1:length(list_comp_i))
    {
      list_cor_comp_var[[j]] = sapply(1:dim(block_i)[2], FUN = function(k){
        cor(list_comp_i[[j]], block_i[, k])
      })
      
    } # End for(j in 1:length(liste_comp_i)).
    
    mat_cor_allcomp_allvar = Reduce(cbind, list_cor_comp_var)
    rownames(mat_cor_allcomp_allvar) = colnames(block_i)
    
    vec_name_blocks_i = rep(name_block_i, dim(block_i)[2])
    vec_names_blocks2 = c(vec_names_blocks2, vec_name_blocks_i)
    
    list_cor_comp_var_global[[i]] = mat_cor_allcomp_allvar
    
    
  } # End for(i in 1:length(blocks)).
  
  
  mat_cor_comp_var_global = Reduce(rbind, list_cor_comp_var_global)
  dataframe_cor_comp_var_global = data.frame(cbind(rownames(mat_cor_comp_var_global),
                                                   vec_names_blocks2,
                                                   mat_cor_comp_var_global))
  
  colnames(dataframe_cor_comp_var_global) = c("variable",
                                              "block",
                                              paste0("cor_var_comp", comp))
  
  dataframe_cor_comp_var_global[, 1:2] = apply(dataframe_cor_comp_var_global[, 1:2], 2, as.character)
  dataframe_cor_comp_var_global[, 3:dim(dataframe_cor_comp_var_global)[2]] = apply(dataframe_cor_comp_var_global[, 3:dim(dataframe_cor_comp_var_global)[2]], 2, as.numeric)
  
  
  return(dataframe_cor_comp_var_global)
  
}


# The function addResponseVariables computes, for each response variable, for each
# group of blocks, the correlations between this response variable and the components 
# (whose indices are in comp) of the first block of this group of blocks.

addResponseVariables <-function(res_block_splsda,
                                dataframe_cor_comp_var_global,
                                list_vec_index_block_select,
                                block_Y,
                                comp)
{
  
  list_dataframe_cor_allcomp_response_var = list()
  vec_group_name_blocks = c()
  
  for(i in 1:length(list_vec_index_block_select))
  {
    vec_index_block_select_i = list_vec_index_block_select[[i]]
    indice_first_block_i = vec_index_block_select_i[1]
    
    list_cor_comp_var = list()
    
    for(j in 1:length(comp))
    {
      index_comp_j = comp[j]
      comp_j = res_block_splsda$variates[[indice_first_block_i]][, index_comp_j]
      
      list_cor_comp_var[[j]] = sapply(1:dim(block_Y)[2], FUN = function(k){
        res = cor(comp_j, block_Y[, k])
        
        return(res)
      })
      
    } # End for(j in 1:length(liste_comp_i)).
    
    mat_cor_allcomp_response_var = Reduce(cbind, list_cor_comp_var)
    rownames(mat_cor_allcomp_response_var) = colnames(block_Y)
    
    dataframe_allcomp_response_var = data.frame(colnames(block_Y),
                                                rep("Y", dim(block_Y)[2]),
                                                mat_cor_allcomp_response_var)
    
    colnames(dataframe_allcomp_response_var) = c("variable",
                                                 "block",
                                                 paste0("cor_var_comp", comp))
    
    
    list_dataframe_cor_allcomp_response_var[[i]] = dataframe_allcomp_response_var
    
    group_names_blocks_i = res_block_splsda$names$blocks[vec_index_block_select_i]
    vec_group_name_blocks = c(vec_group_name_blocks, paste(group_names_blocks_i, collapse = "-"))
    
    
  } # End for(i in 1:length(list_vec_index_block_select)).
  
  names(list_dataframe_cor_allcomp_response_var) = vec_group_name_blocks
  
  list_dataframe_cor_comp_var_global = list()
  
  for(i in 1:length(list_vec_index_block_select))
  {
    vec_index_block_select_i = list_vec_index_block_select[[i]]
    group_names_blocks_i = res_block_splsda$names$blocks[vec_index_block_select_i]
    index_i = which(dataframe_cor_comp_var_global$bloc%in%group_names_blocks_i == TRUE)
    dataframe_cor_comp_var_global_index_i = dataframe_cor_comp_var_global[index_i, ]
    
    dataframe_cor_comp_block_var_and_response_var_global = rbind(dataframe_cor_comp_var_global_index_i,
                                                                 list_dataframe_cor_allcomp_response_var[[i]])
    
    dataframe_cor_comp_block_var_and_response_var_global[, 1:2] = apply(dataframe_cor_comp_block_var_and_response_var_global[, 1:2], 2, as.character)
    dataframe_cor_comp_block_var_and_response_var_global[, 3:dim(dataframe_cor_comp_block_var_and_response_var_global)[2]] = apply(dataframe_cor_comp_block_var_and_response_var_global[, 3:dim(dataframe_cor_comp_block_var_and_response_var_global)[2]], 2, as.numeric)
    
    
    list_dataframe_cor_comp_var_global[[i]] = dataframe_cor_comp_block_var_and_response_var_global
    
    
  } # End for(i in 1:length(list_vec_index_block_select)).
  
  names(list_dataframe_cor_comp_var_global) = vec_group_name_blocks
  
  
  return(list_dataframe_cor_comp_var_global)
  
}

# The function computeCorVarInterest computes the correlations between the variables of interest
# and the components (whose indices are in comp).

computeCorVarInterest <-function(var_interest,
                                 res_block_splsda,
                                 list_vec_index_block_select,
                                 comp)
{
  names_blocks_temp = res_block_splsda$names$blocks
  index_Y = which(names_blocks_temp == "Y")
  names_blocks = names_blocks_temp[- index_Y]
  
  list_cor_comp_interest_var = list()
  
  
  for(i in 1:length(list_vec_index_block_select))
  {
    vec_index_block_select_i = list_vec_index_block_select[[i]]
    names_blocks_i = names_blocks[vec_index_block_select_i]
    
    list_cor_comp_interest_var[[i]] = c()
    
    for(j in 1:length(names_blocks_i))
    {
      name_block_i_j = names_blocks_i[j]
      index_block_select_i_j = vec_index_block_select_i[j]
      block_i_j = res_block_splsda$X[[index_block_select_i_j]]
      name_var_block_i_j = colnames(block_i_j)
      
      index_var_interest_in_block_i_j = sapply(1:length(var_interest), FUN = function(k){
        index_i_j_k = which(name_var_block_i_j == var_interest[k])
        
        if(length(index_i_j_k) != 0)
        {
          res = TRUE
          
        }else{
          res = FALSE
          
        }
        
        return(res)
      })
      
      if(length(which(index_var_interest_in_block_i_j == TRUE)) != 0)
      {
        var_interest_block_i_j = var_interest[which(index_var_interest_in_block_i_j == TRUE)]
        block_i_j_select = block_i_j[, var_interest_block_i_j]
        list_comp_i_j = list()
        liste_cor_comp_var_i_j = list()
        
        for(k in 1:length(comp))
        {
          index_comp_k = comp[k]
          list_comp_i_j[[k]] = res_block_splsda$variates[[index_block_select_i_j]][, index_comp_k]
          
        } # End for(k in 1:length(comp)).
        
        
        for(k in 1:length(list_comp_i_j))
        {
          liste_cor_comp_var_i_j[[k]] = sapply(1:dim(block_i_j_select)[2], FUN = function(l){
            res = cor(list_comp_i_j[[k]], block_i_j_select[, l])
            
            return(res)
          })
          
        } # End for(k in 1:length(list_comp_i_j)).
        
        mat_cor_allcomp_allvar = Reduce(cbind, liste_cor_comp_var_i_j)
        
        dataframe_cor_comp_interest_var = data.frame(cbind(var_interest_block_i_j,
                                                           rep(name_block_i_j, length(var_interest_block_i_j)),
                                                           mat_cor_allcomp_allvar))
        colnames(dataframe_cor_comp_interest_var) = c("variable",
                                                      "block",
                                                      paste0("cor_var_comp", comp))

        dataframe_cor_comp_interest_var[, 1:2] = apply(dataframe_cor_comp_interest_var[, 1:2], 2, as.character)
        dataframe_cor_comp_interest_var[, 3:dim(dataframe_cor_comp_interest_var)[2]] = apply(dataframe_cor_comp_interest_var[, 3:dim(dataframe_cor_comp_interest_var)[2]], 2, as.numeric)
        
        if(j == 1)
        {
          list_cor_comp_interest_var[[i]] = dataframe_cor_comp_interest_var
          
        }else{
          list_cor_comp_interest_var[[i]] = rbind(list_cor_comp_interest_var[[i]],
                                                  dataframe_cor_comp_interest_var)
          
        }
        
        
        # End if(length(which(index_var_interest_in_block_i_j == TRUE)) != 0).           
      }
      
      
    } # End for(j in 1:length(block_i)).
    
    
  } # End for(i in 1:length(liste_vec_indice_blockSelect)).
  
  
  return(list(var_interest = var_interest,
              list_cor_comp_interest_var = list_cor_comp_interest_var))  
  
}

#' Determination of the correlation circles that can be overlaid and computation of the correlations
#' 
#' @description Determines which blocks can be overlaid. Performs the computation of the correlations between the components and the selected block variables, the response
#' variables and the variables of interest.
#' @param res_block_splsda object of class sgccda. Output of the function block.splsda.
#' @param block_Y matrix of size n*(number of response variables). The coefficient (i, j) of block_Y is equal to 1 if the ith
#' sample tallies with the jth response variable. 
#' @param cutoff_comp numeric. If for two blocks, the correlation between the comp[1] component of a block and the comp[1] component of
#' the other block is greater than cutoff_comp in absolue value and the correlation between the comp[2] component of a block and the comp[2] component of
#' the other block is greater than cutoff_comp in absolue value, the correlation circles for these two blocks can be overlaid.
#' @param var_interest (optional) characters vector. va_interest contains the variables of interest that will be added in the network.
#' @param comp integer vector. comp contains the indices of the components for which the computation of the correlations between
#'  the components and the selected block variables, the response variables and the variables of interest is performed. Default comp = 1:2 . 
#' @details The function matCorAddVar first computes the correlation matrix for the comp[1] components of the blocks and
#' the comp[2] components of the blocks. These two matrices enable then to determine group of blocks for which the correlation circles
#' can be overlaid and all the combinations of group of blocks that can be overlaid. Finally, for each group of blocks, this function
#' computes the correlations between the components (whose indices are in comp) and the selected block variables, the response variables and 
#' variables of interest (optional).
#' @return a list containing
#' \itemize{
#' \item \emph{mat_cor_comp1} type : matrix. mat_cor_comp1 is the correlation matric between the components of index comp[1].
#' \item \emph{mat_cor_comp2} type : matrix. mat_cor_comp1 is the correlation matric between the components of index comp[2].
#' \item \emph{cutoff_comp} type numeric. Input parameter of the function matCorAddVar.
#' \item \emph{comp} type : integer vector. Input parameter of the function matCorAddVar.
#' \item \emph{list_vec_index_block_select} list of integer. This list contains the indices of the blocks whose correlation circles can be overlaid.
#' \item \emph{list_vec_names_blocks.} list of characters vectors. list_vec_names_blocks contains all the possible combinations of the blocks whose correlation circles can be overlaid.
#' \item \emph{list_cor_comp_selected_var_resp_var} type list of dataframes. For the ith group of blocks, list_cor_comp_selected_var_resp_var.
#' contains the correlations between the components (whose indices are in comp) and the selected block variables and the response variables.
#' \item \emph{res_compute_cor_var_interest.} type : list. The first element of this list contains the
#' variables of interest. The second element of this list contains a list of dataframes. The ith element
#' of this list contains, for each variable of interest, the correlation between this variable of interest
#' and the components (whose indices are in comp).
#' \item \emph{res_block_splsda.} type : sgccda. Output of the function block.splsda .
#' }
#' @examples
#' load(system.file("extdata", "res_data_integration.rda", package="visCorVar"))
#' load(system.file("extdata", "block_Y.rda", package="visCorVar"))
#' load(system.file("extdata", "var_interest.rda", package="visCorVar"))
#' comp = 1:2
#' cutoff_comp = 0.8
#' res_matCorAddVar = matCorAddVar(res_data_integration, block_Y,
#'  cutoff_comp, var_interest, comp)
#' @export
matCorAddVar<-function(res_block_splsda,
                       block_Y,
                       cutoff_comp,
                       var_interest = NULL,
                       comp)
{

  liste_mat_cor_comp = matrixCorComp(res_block_splsda = res_block_splsda,
                                     comp = comp)  
  
  mat_cor_comp1 = liste_mat_cor_comp$mat_cor_comp1
  mat_cor_comp2 = liste_mat_cor_comp$mat_cor_comp2
  
  
  liste_block_select = computeBlockSelect(mat_cor_comp1 = mat_cor_comp1,
                                         mat_cor_comp2 = mat_cor_comp2,
                                         cutoff_comp = cutoff_comp)
  
  list_vec_index_block_select = liste_block_select$list_vec_index_block_select
  list_vec_names_blocks = liste_block_select$list_vec_names_blocks
  
  dataframe_cor_comp_var_global = computeCorCompVar(res_block_splsda = res_block_splsda,
                                                    comp = comp)
  
  
  list_cor_comp_selected_var_resp_var = addResponseVariables(res_block_splsda = res_block_splsda,
                                                             dataframe_cor_comp_var_global = dataframe_cor_comp_var_global,
                                                             list_vec_index_block_select = list_vec_index_block_select,
                                                             block_Y = block_Y,
                                                             comp = comp)
  
  if(!is.null(var_interest))
  {
    res_compute_cor_var_interest = computeCorVarInterest(var_interest = var_interest,
                                                       res_block_splsda = res_block_splsda,
                                                       list_vec_index_block_select = list_vec_index_block_select,
                                                       comp = comp)    

  }else{
    res_compute_cor_var_interest = NULL
    
  }
  

  return(list(mat_cor_comp1 = mat_cor_comp1,
              mat_cor_comp2 = mat_cor_comp2,
              cutoff_comp = cutoff_comp,
              list_cor_comp_selected_var_resp_var = list_cor_comp_selected_var_resp_var,
              list_vec_index_block_select = list_vec_index_block_select,
              list_vec_names_blocks = list_vec_names_blocks,
              comp = comp,
              res_compute_cor_var_interest = res_compute_cor_var_interest,
              res_block_splsda = res_block_splsda
  ))
  
}
