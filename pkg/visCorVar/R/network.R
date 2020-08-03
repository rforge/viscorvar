#' Computation of similarity matrices 
#' @description Performs the computation of the similarity matrices for each group of blocks.
#' @param res_matCorAddVar res_matCorAddVar is the output of the function matCorAddVar.
#' @details The function computeMatSimilarity performs the computation of the similarity matrices for each group of blocks. The similarity
#' between two variables is an approximation of the correlation between these two variables.
#' @return a list containing
#' \itemize{
#' \item \emph{liste_mat_similarity_group} type : list of matrices. The ith element of liste_mat_similarity_group contains the similarity matrices for the blocks
#' of the ith group of blocks and the response variables.
#' \item \emph{res_matCorAddVar.} The output of the function matCorAddVar.
#' }
#' @examples
#' data(res_data_integration)
#' data(block_Y)
#' data(var_interest)
#' comp = 1:2
#' cutoff_comp = 0.8
#' res_matCorAddVar = matCorAddVar(res_data_integration, block_Y,
#'  cutoff_comp, var_interest, comp)
#' res_compute_mat_similarity = computeMatSimilarity(res_matCorAddVar)

#' @export
computeMatSimilarity <-function(res_matCorAddVar)
{
  list_cor_comp_selected_var_resp_var = res_matCorAddVar$list_cor_comp_selected_var_resp_var
  list_cor_comp_interest_var = res_matCorAddVar$res_compute_cor_var_interest$list_cor_comp_interest_var
  comp = res_matCorAddVar$comp
  var_interest = res_matCorAddVar$res_compute_cor_var_interest$var_interest
  
  list_mat_similarity_group = list()
  
  # Computation, for each group of blocks, the similarity matrices between each pair of blocks.
  
  for(i in 1:length(list_cor_comp_selected_var_resp_var))
  {
    cor_comp_selected_var_resp_var_i = list_cor_comp_selected_var_resp_var[[i]]
    
    if(!is.null(var_interest))
    {
      cor_comp_var_interest_i = list_cor_comp_interest_var[[i]]
      names_blocks_i = unique(c(cor_comp_selected_var_resp_var_i$block, cor_comp_var_interest_i$block))
      
      coord = lapply(1:length(names_blocks_i), FUN = function(j){
        name_block_i_j = names_blocks_i[j]
        ind_j = which(cor_comp_selected_var_resp_var_i$block == name_block_i_j)
        
        name_var_interest_i_j = cor_comp_var_interest_i[which(cor_comp_var_interest_i$block == name_block_i_j), "variable"]
        
        cor_comp_selected_var_resp_var_i_j = as.matrix(cor_comp_selected_var_resp_var_i[ind_j, paste0("cor_var_comp", comp)], drop = FALSE)
        rownames(cor_comp_selected_var_resp_var_i_j) = cor_comp_selected_var_resp_var_i[ind_j, "variable"]
        name_var_block_i_j = cor_comp_selected_var_resp_var_i[ind_j, "variable"]
        
        if(length(name_var_interest_i_j) != 0)
        {
          ind_j2 = which(name_var_interest_i_j%in%name_var_block_i_j)
          
          if(length(which(ind_j2 == TRUE)) != 0)
          {
            name_var_interest_i_j2 = name_var_interest_i_j[- which(ind_j2 == TRUE)]  
            
          }else{
            name_var_interest_i_j2 = name_var_interest_i_j
            
          }
          
          if(length(name_var_interest_i_j2) != 0)
          {
            ind_j3 = sapply(1:dim(cor_comp_var_interest_i)[1], FUN = function(j){
              variable_i_j = cor_comp_var_interest_i[j, "variable"]
              index_i_j = which(name_var_interest_i_j2 == variable_i_j)
              
              if(length(index_i_j) != 0)
              {
                res = TRUE
                
              }else{
                res = FALSE
                
              }
              
              return(res)
            })
            
            cor_comp_var_interest_i_j = as.matrix(cor_comp_var_interest_i[which(ind_j3 == TRUE), paste0("cor_var_comp", comp)], drop = FALSE)
            rownames(cor_comp_var_interest_i_j) = cor_comp_var_interest_i[which(ind_j3 == TRUE), "variable"]
            res = rbind(cor_comp_selected_var_resp_var_i_j, cor_comp_var_interest_i_j)
            rownames(res) = c(rownames(cor_comp_selected_var_resp_var_i_j), rownames(cor_comp_var_interest_i_j))
            
          }else{
            res = cor_comp_selected_var_resp_var_i_j
            rownames(res) = rownames(cor_comp_selected_var_resp_var_i_j)
            
          }
          
        }else{
          res = cor_comp_selected_var_resp_var_i_j
          rownames(res) = rownames(cor_comp_selected_var_resp_var_i_j)
          
        }
        
        return(res)
      })  
      
      
      # End if(!is.null(var_interest))
    }else{
      names_blocks_i = unique(cor_comp_selected_var_resp_var_i$block)
      
      coord = lapply(1:length(names_blocks_i), FUN = function(j){
        name_block_i_j = names_blocks_i[j]
        ind_j = which(cor_comp_selected_var_resp_var_i$block == name_block_i_j)
        cor_comp_selected_var_resp_var_i_j = as.matrix(cor_comp_selected_var_resp_var_i[ind_j, paste0("cor_var_comp", comp)], drop = FALSE)
        name_var_block_i_j = cor_comp_selected_var_resp_var_i[ind_j, "variable"]
        res = cor_comp_selected_var_resp_var_i_j
        rownames(res) = name_var_block_i_j
        
        return(res)
      })  
      
      
    }
    
    
    
    M_block = list()
    l = 1
    
    vec_names_blocks_i = c()
    
    for(j in 1:(length(names_blocks_i) - 1))
    {
      name_block_i_j = names_blocks_i[j]
      
      for(k in (j + 1):length(names_blocks_i))
      {
        name_block_i_k = names_blocks_i[k]
        
        M_block[[l]] = coord[[j]][, drop = FALSE] %*% t(coord[[k]][, drop = FALSE])
        rownames(M_block[[l]]) = rownames(coord[[j]])
        colnames(M_block[[l]]) = rownames(coord[[k]])
        
        name_blocks_j_k = paste(c(name_block_i_j, name_block_i_k), collapse = "-")
        vec_names_blocks_i = c(vec_names_blocks_i, name_blocks_j_k)
        
        l = l + 1
        
      } # Fin for(k in (j + 1):length(blocks)).
        
    } # Fin for(j in 1:(length(blocks) - 1)).
    names(M_block) = vec_names_blocks_i
    
    list_mat_similarity_group[[i]] = M_block
    
  } # Fin for(i in 1:length(liste_dataframe_Cor_comp_var_global)).
  
  names(list_mat_similarity_group) = names(list_cor_comp_selected_var_resp_var) 
  
  return(list(list_mat_similarity_group = list_mat_similarity_group,
              res_matCorAddVar = res_matCorAddVar))
  
}


#' Network of correlated variables 
#' @param res_compute_mat_similarity res_compute_mat_similarity is the output of the function computeMatSimilarity
#' @param names_block_variables type : character vector. names_block_variables contains the names of selected block variables. 
#' @param names_response_variables type : charcater vector. names_response_variables contains the names of the response variables which
#' will be in the network.
#' @param cutoff type : numeric. If the similarity between two selected block variables is larger than cutoff in absolute value,
#' these variable will be in the network.
#' @details The function networkVar create a network of correlated variables. The variables of interest and 
#' the response variables will be in the network. If the similarity between two selected block variables is larger than cutoff in absolute value,
#' these variable will be in the network.
#' @return a list containing
#' \itemize{
#' \item \emph{gR} type : igraph graph. This object contains all the information needed to create a network. This
#' object can be exported in graphml format.
#' \item \emph{cutoff} type : numeric. An input parameter of the function networkVar.
#' }
#' @examples
#' library(RColorBrewer)
#' data(res_data_integration)
#' data(block_Y)
#' data(var_interest)
#' comp = 1:2
#' cutoff_comp = 0.8
#' res_matCorAddVar = matCorAddVar(res_data_integration, block_Y,
#'  cutoff_comp, var_interest, comp)
#' list_cor_comp_selected_var_resp_var = res_matCorAddVar$list_cor_comp_selected_var_resp_var
#' list_vec_index_block_select = res_matCorAddVar$list_vec_index_block_select
#' mat_cor_comp1 = res_matCorAddVar$mat_cor_comp1
#' mat_cor_comp2 = res_matCorAddVar$mat_cor_comp2
#' res_compute_mat_similarity = computeMatSimilarity(res_matCorAddVar)
#' names_blocks = c("X1", "X3")
#' names_response_variables = c("A", "B")
#' comp = 1:2
#' names_block_variables = circleCor(list_cor_comp_selected_var_resp_var, list_vec_index_block_select,
#'   mat_cor_comp1, mat_cor_comp2, names_blocks, names_response_variables, comp, 0.85, -1,
#'    1, -1, 1, colorRampPalette(brewer.pal(9, "Spectral"))(dim(mat_cor_comp1)[1] + 1), rad.in = 0.5, 
#'    0.7, 0.8, c(1.2, 0), 20)
#' names_resp_var2 = c("A")
#' res_networkVar = networkVar(res_compute_mat_similarity, names_block_variables, names_resp_var2,
#'   0)
#' @export
networkVar <-function(res_compute_mat_similarity,
                      names_block_variables,
                      names_response_variables,
                      cutoff = 0)
{
  list_mat_similarity_group = res_compute_mat_similarity$list_mat_similarity_group
  res_matCorAddVar = res_compute_mat_similarity$res_matCorAddVar
  
  res_block_splsda = res_matCorAddVar$res_block_splsda
  var_interest = res_matCorAddVar$res_compute_cor_var_interest$var_interest  
  cutoff_comp = res_matCorAddVar$cutoff_comp
  comp = res_matCorAddVar$comp
  
  # Check if we can create a network for the variables contained in names_block_variables.
  
  # Look for the group of blocks associated with names_block_variables.
  
  index_group_names_block_variables = sapply(1:length(list_mat_similarity_group), FUN = function(i){
    list_mat_similarity_group_i = list_mat_similarity_group[[i]]
    boolean = FALSE
    j = 1
    
    while((j <= length(list_mat_similarity_group_i))&!boolean)
    {
      mat_similarity_group_i_j = list_mat_similarity_group_i[[j]]
      vec_var_block1 = rownames(mat_similarity_group_i_j)
      vec_var_block2 = colnames(mat_similarity_group_i_j)
      
      vec_var_block1_block2 = c(vec_var_block1, vec_var_block2)
      
      if(any(vec_var_block1_block2%in%names_block_variables))
      {
        boolean = TRUE
        
      }
      
      j = j + 1
      
    } # End while((j <= length(list_mat_similarity_group_i))&!boolean).
    
    res = boolean
    
    return(res)
  }) 
  
  
  if(length(which(index_group_names_block_variables == TRUE)) >= 2)
  {
    stop("The block variables have to belong to only one element of liste_mat_similarity_group.")
    
  }else{
    list_mat_similarity = list_mat_similarity_group[[which(index_group_names_block_variables == TRUE)]]
    
    if(!is.null(var_interest))
    {
      allvariables = sapply(1:length(res_block_splsda$X), FUN = function(i){
        res = colnames(res_block_splsda$X[[i]])
        
        return(res)
      })
      allvariables = unique(unlist(allvariables))
      
      index_variable_interest_not_in_allVariables = var_interest%in%allvariables
      
      if(length(which(index_variable_interest_not_in_allVariables == FALSE)) != 0)
      {
        var_interest_not_in_allvariables = var_interest[which(index_variable_interest_not_in_allVariables == FALSE)]
        
        warning(paste0(length(var_interest_not_in_allvariables), " variables of interest ", paste(var_interest_not_in_allvariables, collapse = ","), "
                are not block variables. These variables will not be in the network."))
      }
      
      var_interest2 = var_interest[which(index_variable_interest_not_in_allVariables == TRUE)]
      
      
    }else{
      var_interest2 = var_interest
      
    }
    
    
    if(!is.null(names_block_variables) & !is.null(var_interest2))
    {
      var_com = intersect(names_block_variables, var_interest2)
      
      if(length(var_com) != 0)
      {
        index_var_com = sapply(1:length(var_com), FUN = function(i){
          res = which(names_block_variables == var_com[i])
          
          return(res)
        })
        names_block_variables2 = names_block_variables[- index_var_com]
        
      }else{
        names_block_variables2 = names_block_variables
        
      }
      
      names_block_variables3 = c(names_block_variables2, var_interest2)
      
    }else if(!is.null(names_block_variables) & is.null(var_interest2))
    {
      names_block_variables3 = names_block_variables
      
    }else if(is.null(names_block_variables) & !is.null(var_interest2))
    {
      names_block_variables3 = var_interest2
      
    }else{
      stop("You have to provide block variables and variables of interest in order to create a network.")
      
    }
    
    names_block_variables_and_response_variables = c(names_block_variables3, names_response_variables)
    
    blocks_list_mat_similarity_temp1 = sapply(1:length(list_mat_similarity), FUN = function(i){
      names_block1_block2_i = names(list_mat_similarity)[i]
      ch = strsplit(names_block1_block2_i, split = "-")[[1]]
      name_block1 = ch[1]
      name_block2 = ch[2]
      res = c(name_block1, name_block2)
      
      return(res)
    })
    
    blocks_list_mat_similarity = unique(as.vector(blocks_list_mat_similarity_temp1))
    
    index_blocks_list_mat_similarity_temp = sapply(1:length(blocks_list_mat_similarity), FUN = function(i){
      res = which(res_block_splsda$names$blocks == blocks_list_mat_similarity[i])
      
      return(res)
    })
    
    index_Y = which(res_block_splsda$names$blocks == "Y")
    index_blocks_liste_mat_similarity = index_blocks_list_mat_similarity_temp[index_blocks_list_mat_similarity_temp != index_Y]
    
    boolean_pos_cor = TRUE
    
    # Check if the components are highly positively correlated.
    
    if(length(index_blocks_liste_mat_similarity) == 1)
    {
      
      
    }else{
      for(i in 1:length(comp))
      {
        comp_i = comp[i]
        
        for(j in 1:(length(index_blocks_liste_mat_similarity) - 1))
        {
          index_blocks_liste_mat_similarity_j = index_blocks_liste_mat_similarity[j]
          comp_indice_blocks_liste_matSimilarity_j = res_block_splsda$variates[[index_blocks_liste_mat_similarity_j]][, comp_i]
          
          
          for(k in (j + 1):length(index_blocks_liste_mat_similarity))
          {
            index_blocks_liste_mat_similarity_k = index_blocks_liste_mat_similarity[k]
            comp_indice_blocks_liste_matSimilarity_k = res_block_splsda$variates[[index_blocks_liste_mat_similarity_k]][, comp_i]
            
            cor = cor(comp_indice_blocks_liste_matSimilarity_j, comp_indice_blocks_liste_matSimilarity_k)
            
            boolean_pos_cor = boolean_pos_cor & cor > cutoff_comp
            
            
          } # End for(k in (j + 1):length(index_blocks_liste_mat_similarity)).
          
        } # End for(j in 1:(length(index_blocks_liste_mat_similarity)) - 1).
        
      } # End for(i in 1:length(comp)).
      
      
    }
    
    
    
    if(!boolean_pos_cor)
    {
      stop("For each pair of blocks, the ith component of the first block and the ith component of the second block have
            to be positively correlated in order to create a network.")
      
    }else{
      list_mat_similarity_temp = lapply(1:length(list_mat_similarity), FUN = function(j){
        mat_similarity_j = list_mat_similarity[[j]]
        
        index_row_mat_similarity_j  = which(rownames(mat_similarity_j)%in%names_block_variables_and_response_variables == TRUE)
        index_col_mat_similarity_j  = which(colnames(mat_similarity_j)%in%names_block_variables_and_response_variables == TRUE)
        
        if((length(index_row_mat_similarity_j) != 0) &(length(index_col_mat_similarity_j) != 0))
        {
          res = mat_similarity_j[index_row_mat_similarity_j, index_col_mat_similarity_j, drop = FALSE]   
          
        }else{
          res = NA
          
        }
        
        return(res)
      })  
      names(list_mat_similarity_temp) = names(list_mat_similarity)
      
      index_NA_list_mat_similarity_temp = sapply(1:length(list_mat_similarity_temp), FUN = function(i){
        list_mat_similarity_temp_i = list_mat_similarity_temp[[i]]
        
        if(is.matrix(list_mat_similarity_temp_i))
        {
          res = FALSE
          
        }else{
          if(is.na(list_mat_similarity_temp_i))
          {
            res = TRUE
            
          }else{
            res = FALSE
            
          }
          
        }
        
        return(res)
      })
      
      
      list_mat_similarity_temp2 = list_mat_similarity_temp[!index_NA_list_mat_similarity_temp]
      
      w = c()
      node.X1 = c()
      node.X2 = c()
      vec_group = c()
      vec_names_variables = c()
      
      for(i in 1:length(list_mat_similarity_temp2))
      {
        
        names_block1_block2_i = names(list_mat_similarity_temp2)[i]
        mat_similarity_temp2_i = list_mat_similarity_temp2[[i]]
        
        X1 = rownames(mat_similarity_temp2_i)
        X2 = colnames(mat_similarity_temp2_i)
        
        rep.X1 = rep(X1, each = length(X2))
        rep.X2 = rep(X2, length(X1))
        
        node.X1 = c(node.X1, rep.X1)
        node.X2 = c(node.X2, rep.X2)
        
        ch = strsplit(names_block1_block2_i, split = "-")[[1]]
        name_block1 = ch[1]
        name_block2 = ch[2]
        vec_group = c(vec_group, c(rep(name_block1, length(X1)), rep(name_block2, length(X2))))
        vec_names_variables = c(vec_names_variables, c(X1, X2))
        
        w = c(w, as.vector(t(mat_similarity_temp2_i)))
        
      } # End for(i in 1:length(liste_mat_similarity_temp2)).
      
      dup = duplicated(vec_names_variables)
      vec_names_variables = vec_names_variables[!dup]
      vec_group = vec_group[!dup]
      
      nodes = data.frame(name = vec_names_variables,
                         group = vec_group)
      
      # gR
      relations = data.frame(from = node.X1,
                             to = node.X2,
                             weight = w)
      
      # idx
      if(!is.null(names_block_variables) & !is.null(var_interest2) & !is.null(names_response_variables))
      {
        
        idx = sapply(1:dim(relations)[1], FUN = function(i){
          node.X1_i = relations$from[i]
          node.X2_i = relations$to[i]
          
          if(node.X1_i%in%var_interest2 | node.X2_i%in%var_interest2)
          {
            res = TRUE
            
          }else if(node.X1_i%in%names_response_variables | node.X2_i%in%names_response_variables){
            res = TRUE
            
          }else{
            res = abs(w)[i] >= cutoff
            
          }
          
          return(res)
        }) 
        
      }else if(!is.null(names_block_variables) & is.null(var_interest2) & !is.null(names_response_variables))
      {
        idx = sapply(1:dim(relations)[1], FUN = function(i){
          node.X1_i = relations$from[i]
          node.X2_i = relations$to[i]
          
          if(node.X1_i%in%names_response_variables | node.X2_i%in%names_response_variables){
            res = TRUE
            
          }else{
            res = abs(w)[i] >= cutoff
            
          }
          
          return(res)
        }) 
        
      }else if(is.null(names_block_variables) & !is.null(var_interest2) & !is.null(names_response_variables))
      {
        idx = rep(TRUE, dim(relations)[1])
        
      }
      
      relations = relations[idx, , drop = FALSE]
      
      gR = graph.data.frame(relations,
                            directed = FALSE,
                            vertices = nodes)
      
      # Deletion of nodes with no edge.
      gR = delete.vertices(gR, which(degree(gR) == 0))
      
      res = list(gR = gR)
      res$cutoff = cutoff
      
      return(res)
      
      
    }
    
    
  }
  
}
