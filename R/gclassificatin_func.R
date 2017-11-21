#' A function that extracts the nodes of a data.tree object where the given
#' attribute is not NA, not called by the user
#' @export
#' @name extract_node_v_helper
#' @author Stefan Lüdtke
#' @param  data_tree A data.tree object
#' @param  attribute Character string that specifies the attribute of interest 
#' @examples
#' test = extract_node_v(data_tree, attribute = "station_id")

extract_node_v_helper = function(data_tree, attribute) {
  all_fields = data_tree$fieldsAll
  # check if the desired field exists in the tree
  if (any(attribute %in% all_fields)){
        temp_df = data.tree::ToDataFrameNetwork(data_tree, attribute) %>%
          filter(complete.cases(.))
  } else{ 
    # break if not
    stop(paste0("Attribute ", attribute, " is not in the list of fields\n", 
                "Available fields are: ", all_fields))
  }
  # we extract the "to" node and return a vector
  return(as.vector(temp_df[, 'to']))
}
#  -------------------------------------------------------------

#' A helper function that is not called by the user
#' @export
#' @name reduce_tree_helper
#' @author Stefan Lüdtke
#' @param  x A node name to start with
#' @param  data_tree A tree to operate on 
#' @return a data frame with 1 row and the node name from the next ancestor 

reduce_tree_helper =  function(x, data_tree, attribute) {
  # find the subtree of the node_id of interest
  temp_tree = FindNode(data_tree, x)
  # extract the levels of the tree but only for ancestors
  temp_df = data.frame(level = temp_tree$Get('level', traversal = "ancestor"), 
                       name = temp_tree$Get('name', traversal = "ancestor"),
                       att = temp_tree$Get(attribute, traversal = "ancestor")) %>%
    # attach the source node
    mutate(to = x) %>%  dplyr::rename(from = name) %>%
    # filter/discard each station where the attribute is not missing
    filter(!is.na(att)) %>%
    # filter/discard each station with the same from-to combination
    filter(from != to) %>%
    # select the closest node that has the attribute
    filter(level == max(level)) %>% 
    # keep only the columns of interest
    select(-level, -att)
  if (nrow(temp_df) == 0) {
    return_df = NULL
  }else {
    return_df = temp_df
  }
  return(return_df)
}
#  -------------------------------------------------------------

#' A function that creates new (reduced) tree based on an attribute that must
#' not be NA
#' @export
#' @name reduce_tree
#' @author Stefan Lüdtke
#' @param  a data.tree object that provides the source
#' @param  a list/vector of nodes that build up the tree
#' @return a data.tree object 
#' @examples
#' test_tree_reduced = reduce_tree(base_tree_b, attribute = 'station_id')

reduce_tree = function(data_tree, attribute) {
  node_ids = extract_node_v(data_tree, attribute)
  tree_reduced = ldply(node_ids, reduce_tree_helper, data_tree, attribute) %>% 
    FromDataFrameNetwork()
  return(tree_reduced)
}
#  -------------------------------------------------------------
