#' Overlaying of correlation circles 
#' @description Performs the overlaying of correlation circles and retrieves selected block variables and performs a zoom in the correlation circle.
#' @param list_dataframe_cor_comp_var_global type list of dataframes. For the ith group of blocks, list_cor_comp_selected_var_resp_var
#' contains the correlations between the components and the selected block variables and the response variables.
#' @param list_vec_index_block_select type : list of integer. This list contains the indices of the blocks whose correlation circles can be overlaid.
#' @param mat_cor_comp1 type : matrix. mat_cor_comp1 is the correlation matrix between the components of index comp[1].
#' @param mat_cor_comp2 type : matrix. mat_cor_comp2 is the correlation matric between the components of index comp[2].
#' @param names_blocks type : characters vector. names_blocks contains the blocks names whose the correlation
#' circles will be overlaid.
#' @param names_response_variables type : characters vector. names_response_variables contains the names of the response 
#' variables that will be plotted in the correlation circle.
#' @param comp integer vectors. comp contains the indices of the components used in the correlation circle. Default comp = 1:2 . 
#' @param cutoff type : numeric. The selected block variables whose correlation between comp[1] component or 
#' comp[2] component is greater thant cutoff in absolute value will be plotted.
#' @param min.X type : numeric. min.X is the lower bound of the rectangle for the X axis. 
#' @param max.X type : numeric. max.X is the upper bound of the rectangle for the X axis. 
#' @param min.Y type : numeric. min.Y is the lower bound of the rectangle for the Y axis. 
#' @param max.Y type : numeric. max.Y is the upper bound of the rectangle for the Y axis. 
#' @param vec_col type : character vector. The ith coordinate of vec_col is the color of the selected block variables of the ith block.
#' @param rad.in type : numeric. rad.in is the radius of inner circle.
#' @param cex type : numeric. cex is the size of the selected block variables plotted in the correlation circle. 
#' @param cex_legend type : numeric. cex_legend is the size of the block names in the legend.
#' @param pos type :  numeric. pos enables to position the legend.
#' @param pch type :  integer. pch specifies the points type in the legend.
#' @details
#' The function circleCor performs the overlaying of correlation circles if these correlation circles can be overlaid.  
#' This function performs a zoom in a rectangle of the correlation circle and plots only the selected block variables contained in this rectangle.
#' These selected block variables in this rectangle are in the output of the function circleCor. 
#' @return type : character vector. The output of the function circleCor contains the selected blocks variables
#' contained in the rectangle.
#' @examples
#' library(RColorBrewer)
#' load(system.file("extdata", "res_data_integration.rda", package="visCorVar"))
#' load(system.file("extdata", "block_Y.rda", package="visCorVar"))
#' load(system.file("extdata", "var_interest.rda", package="visCorVar"))
#' comp = 1:2
#' cutoff_comp = 0.8
#' res_matCorAddVar = matCorAddVar(res_data_integration, block_Y, cutoff_comp, var_interest, comp)
#' list_cor_comp_selected_var_resp_var = res_matCorAddVar$list_cor_comp_selected_var_resp_var
#' list_vec_index_block_select = res_matCorAddVar$list_vec_index_block_select
#' mat_cor_comp1 = res_matCorAddVar$mat_cor_comp1
#' mat_cor_comp2 = res_matCorAddVar$mat_cor_comp2
#' names_blocks = c("X1", "X3")
#' names_response_variables = c("A", "B")
#' comp = 1:2
#' select_block_variable = circleCor(list_cor_comp_selected_var_resp_var, list_vec_index_block_select,
#'   mat_cor_comp1, mat_cor_comp2, names_blocks, names_response_variables, comp, 0.85, -1,
#'    1, -1, 1, colorRampPalette(brewer.pal(9, "Spectral"))(dim(mat_cor_comp1)[1] + 1), rad.in = 0.5, 
#'    0.7, 0.8, c(1.2, 0), 20)

#' @export
circleCor <-function(list_dataframe_cor_comp_var_global,
                     list_vec_index_block_select,
                     mat_cor_comp1,
                     mat_cor_comp2,
                     names_blocks,
                     names_response_variables,
                     comp = 1:2,
                     cutoff = 0.85,
                     min.X = -1,
                     max.X = 1,
                     min.Y = -1,
                     max.Y = 1,
                     vec_col = colorRampPalette(brewer.pal(9, "Spectral"))(dim(mat_cor_comp1)[1] + 1),
                     rad.in = 0.5,
                     cex = 0.7,
                     cex_legend = 0.8,
                     pos = c(1.2, 0),
                     pch = 20)
{
  
  # Check if min.X <= max.X and min.Y <= max.Y
  
  if((min.X > max.X) | (min.Y > max.Y))
  {
    stop("max.X has to be greater than min.X and max.Y has to be greater than min.Y .")
    
  }
  
  # Check if the circle plots can be overlaid.
  
  index_blocks = sapply(1:length(names_blocks), FUN = function(i){
    res =  which(colnames(mat_cor_comp1) == names_blocks[i])
    
    return(res)
  })
  
  boolean = FALSE
  i = 1
  
  while(i <= length(list_vec_index_block_select) & !boolean)
  {
    vec_index_block_select_i = list_vec_index_block_select[[i]]
    cond = length(which(vec_index_block_select_i%in%index_blocks == TRUE)) == length(names_blocks)
    
    if(cond)
    {
      boolean = TRUE
      
    }
    
    i = i + 1
    
  } # End while(i <= length(liste_vec_indice_blockSelect) & !boolean).
  
  var_select = NULL
  
  if(!boolean)
  {
    stop(paste0("The blocks : ", paste(names_blocks, collapse = ", "), " can not be overlaid."))
    
  }else{
    # Retrieving of the group of blocks whose vec_names_blocks belong to.
    
    index_names_blocks = sapply(1:length(list_dataframe_cor_comp_var_global), FUN = function(i){
      name_temp_i = names(list_dataframe_cor_comp_var_global)[i]
      name_i = strsplit(name_temp_i, split = "-")[[1]]
      res = all(names_blocks%in%name_i)
      
      return(res)
    })
    
    names_group_blocks = names(list_dataframe_cor_comp_var_global)[which(index_names_blocks == TRUE)]
    names_group_blocks2 = strsplit(names_group_blocks, split = "-")[[1]]
    index_group_blocks = unique(c(which(colnames(mat_cor_comp1) == names_group_blocks2[1], index_blocks)))
    
    dataframe_cor_comp_var_global = list_dataframe_cor_comp_var_global[[which(index_names_blocks == TRUE)]]
    dataframe_cor_comp_var_global_temp1 = dataframe_cor_comp_var_global
    
    cond_names_response_variables = all(names_response_variables%in%dataframe_cor_comp_var_global_temp1$variable)
    
    if(!cond_names_response_variables)
    {
      stop("All the correlations between the response variables and the comp[1] component and the correlations 
            between the responses variables and the comp[2] component have not been computed.")
      
    }else{
      
      mat_cor_comp_var_global_temp1 = t(sapply(1:dim(dataframe_cor_comp_var_global_temp1)[1], FUN = function(i){
        dataframe_cor_comp_var_global_temp1_i = dataframe_cor_comp_var_global_temp1[i, ] 
        name_block_i = dataframe_cor_comp_var_global_temp1_i$block
        cor1  = dataframe_cor_comp_var_global_temp1_i[paste0("cor_var_comp", comp[1])]
        cor2  = dataframe_cor_comp_var_global_temp1_i[paste0("cor_var_comp", comp[2])]
        
        if(name_block_i == "Y")
        {
          cor1_sign = cor1
          cor2_sign = cor2
          
        }else{
          index_block_comp1_i =  which(colnames(mat_cor_comp1) == name_block_i)   
          index_block_comp2_i =  which(colnames(mat_cor_comp2) == name_block_i)
          
          cor1_sign = sign(mat_cor_comp1[index_group_blocks[1], index_block_comp1_i])*cor1
          cor2_sign = sign(mat_cor_comp2[index_group_blocks[1], index_block_comp2_i])*cor2
          
        }
        
        res = c(dataframe_cor_comp_var_global_temp1_i[1:2], cor1_sign, cor2_sign)
        
        return(res)
      }))
      
      dataframe_cor_comp_var_global_temp2 = as.data.frame(mat_cor_comp_var_global_temp1)
      dataframe_cor_comp_var_global_temp2[, 1:2] = apply(dataframe_cor_comp_var_global_temp2[, 1:2], 2, as.character)
      colnames(dataframe_cor_comp_var_global_temp2) = colnames(dataframe_cor_comp_var_global_temp2)
      
      index1 = sapply(1:dim(dataframe_cor_comp_var_global_temp2)[1], FUN = function(i){
        name_block_i = dataframe_cor_comp_var_global_temp2$block[i]
        
        if(name_block_i == "Y")
        {
          res = dataframe_cor_comp_var_global_temp2$variable[i]%in%names_response_variables
          
        }else{
          res = name_block_i%in%names_blocks
          
        }
        
        return(res)
      })
      dataframe_cor_comp_var_global_temp3 = dataframe_cor_comp_var_global_temp2[index1, ]
      mat_cor_comp_var_temp1 = apply(dataframe_cor_comp_var_global_temp3[ , 3:4], 2, as.numeric)
      rownames(mat_cor_comp_var_temp1) = dataframe_cor_comp_var_global_temp3$variable
      
      # index2 contains the indices of the selected block variables highly correlated with either
      # the first component or the second component and contained in the rectangle and the indices
      # of the response variables.

      index2 = sapply(1:dim(mat_cor_comp_var_temp1)[1], FUN = function(k){
        cor1 = mat_cor_comp_var_temp1[k, 1]
        cor2 = mat_cor_comp_var_temp1[k, 2]
        names_block_and_response_k = dataframe_cor_comp_var_global_temp3[k, 2]
        
        if(names_block_and_response_k == "Y")
        {
          cond2 = TRUE
          
        }else{
          cond1 = abs(cor1) > cutoff | abs(cor2) > cutoff
          cond2 = cor1 > min.X & cor1 < max.X & cor2 > min.Y & cor2 < max.Y & cond1
          
        }
        
        return(cond2)
      })
      
      dataframe_cor_comp_var_global_temp4 = dataframe_cor_comp_var_global_temp3[index2, ]
      mat_cor_comp_var = mat_cor_comp_var_temp1[index2, , drop = FALSE]
      var_select_temp = dataframe_cor_comp_var_global_temp4$variable
      index_Y = which(dataframe_cor_comp_var_global_temp4$block == "Y")
      
      if(length(index_Y) != 0)
      {
        var_select = var_select_temp[- index_Y]
        
      }else{
        var_select = var_select_temp
        
      }
      
      # Plot of the overlaying of the correlation circles.
      circle = list()
      circle[[1]] = ellipse(0, levels = 1, t = 1)
      circle[[2]] = ellipse(0, levels = 1, t = rad.in)
      circle = data.frame(do.call("rbind", circle), "Circle" = c(rep("Main circle", 100), rep("Inner circle", 100)))
      
      main_circle = circle[grep("Main circle", circle[, 3]), ]
      inner_circle = circle[grep("Inner circle", circle[, 3]), ]
      
      plot(main_circle[, 1], main_circle[, 2],
           type = "l",
           xlab = paste0("component ", comp[1]),
           ylab = paste0("component ", comp[2]))
      
      points(inner_circle[, 1], inner_circle[, 2],
             type = "l")
      
      if(dim(mat_cor_comp_var)[1] != 0)
      {
        names_blocks_and_response = unique(dataframe_cor_comp_var_global_temp4$block)
        index_block_and_response = sapply(1:length(names_blocks_and_response), FUN = function(i){
          index = which(colnames(mat_cor_comp1) == names_blocks_and_response[i])
          
          if(length(index) != 0)
          {
            res = index
            
          }else{
            res = dim(mat_cor_comp1)[1] + 1
            
          }
          
          return(res)
        })
        vec_col_select = vec_col[index_block_and_response]
        
        if(length(names_blocks_and_response) == 1)
        {
          points(mat_cor_comp_var[, 1], mat_cor_comp_var[, 2],
                 col  = NULL)
          
          text(mat_cor_comp_var[, 1], mat_cor_comp_var[, 2],
               labels = rownames(mat_cor_comp_var),
               cex = cex,
               col = vec_col_select[1])
          
        }else{
          
          nb_var_block = cumsum(sapply(1:length(names_blocks_and_response), FUN = function(j){
            res = length(which(dataframe_cor_comp_var_global_temp4$block == names_blocks_and_response[j]))
            
            return(res)
          }))
          
          for(i in 1:length(nb_var_block))
          {
            if(i == 1)
            {
              index_nb_var1 = 1:nb_var_block[1]
              
              mat_cor_comp_var2 = mat_cor_comp_var[index_nb_var1, , drop = FALSE]
              
              points(mat_cor_comp_var2[, 1], mat_cor_comp_var2[, 2],
                     col  = NULL)
              
              text(mat_cor_comp_var2[, 1], mat_cor_comp_var2[, 2],
                   labels = rownames(mat_cor_comp_var2),
                   cex = cex,
                   col = rep(vec_col_select[i], dim(mat_cor_comp_var2)[1]))
              
              
            }else{
              index_nb_var2 = (nb_var_block[i - 1] + 1):nb_var_block[i]
              
              mat_cor_comp_var2 = mat_cor_comp_var[index_nb_var2, , drop = FALSE]
              
              points(mat_cor_comp_var2[, 1], mat_cor_comp_var2[, 2],
                     col  = NULL)
              
              text(mat_cor_comp_var2[, 1], mat_cor_comp_var2[, 2],
                   labels = rownames(mat_cor_comp_var2),
                   cex = cex,
                   col = rep(vec_col_select[i], dim(mat_cor_comp_var2)[1]))
              
              
            }
            
          }
          
        }
        
        par(xpd = TRUE)
        legend(x = pos[1], y = pos[2],
               legend = names_blocks_and_response,
               pch = pch,
               col = vec_col_select,
               cex = cex_legend)
        
        
      }else{
        warning("There is no variables in this rectangle of the correlation circle.")
        
      }
      
      
    }
    
    
    
  }
  
  return(var_select)
  
}
