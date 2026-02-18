# Function to 'interpret' i.e., print summary statistics neatly, for mixed effects models

abcd_policy_gtInterpret = function(model, dvlab, return.table = F) {
  
  if (return.table == F) {
    gtobj = gtLmerInterpret(model$model, alternate.ci = T, rowname.dict = pretty_names, return.table = F) %>%
      tab_header(title = paste0("Dependent variable: ", dvlab, sep = "")) %>%
      tab_options(table.font.names = "Times New Roman")
    
    return(gtobj)
  } else if (return.table == T) {
    gtLmerObj = gtLmerInterpret(model$model, alternate.ci = T, rowname.dict = pretty_names, return.table = T)
    
    gtobj = gtLmerObj$gt_obj %>%
      tab_header(title = paste0("Dependent variable: ", dvlab, sep = "")) %>%
      tab_options(table.font.names = "Times New Roman")
    
    print(gtobj)
    
    return_list = list("gt_table" = gtobj, "raw_table" = gtLmerObj$table_obj)
    
    return(return_list)
  }
}