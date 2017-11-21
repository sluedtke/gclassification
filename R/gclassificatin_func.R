#' A function that extracts the nodes of a data.tree object where the given
#' attribute is not NA.
#' A helper function that is not called by the user

#' @export
#' @name extract_node_v_helper
#' @author Stefan Lüdtke
#' @param  data_tree A data.tree object
#' @param  attribute Character string that specifies the attribute of interest 
#' @examples
#' test = extract_node_v(base_tree_a, attribute = "station_id")

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

#' A function that takes two data.tree object, where reduced_tree is a subtree
#' of full_tree.
#' A helper function that is not called by the user

#' @export
#' @name link_root_node
#' @author Stefan Lüdtke
#' @param  full_tree data.tree object that provides the source
#' @param  reduced_tree data.tree object as a subtree of full_tree
#' @return data.tree object 
#' @examples
#' test = link_root_node(base_tree_b, reduced_tree_b)

link_root_node = function(full_tree, reduced_tree){
  root_child = reduced_tree$root$name
  root_node = full_tree$root$name
  if (root_child != root_node){
     return_df = data.frame(from = root_node, to = root_child) 
  } else {
    return_df = NULL
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
  node_ids = extract_node_v_helper(data_tree, attribute)
  # get all nodes of interest in from - to style
  temp_tree_reduced = ldply(node_ids, reduce_tree_helper, data_tree, attribute) %>%
    FromDataFrameNetwork()
  root_df = link_root_node(full_tree = data_tree, reduced_tree = temp_tree_reduced)
  return_tree = rbind(ToDataFrameNetwork(temp_tree_reduced), root_df) %>%
    FromDataFrameNetwork()
  return(return_tree)
}
#  -------------------------------------------------------------
