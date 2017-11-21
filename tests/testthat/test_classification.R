#  ------------------------------

library(testthat)
library(plyr)
library(tidyverse)
library(data.tree)

#  ------------------------------
mapping_df_a =  data.frame(subbasinID = c(1, 2, 6, 7, 12),
                      station_id = c("a", "b", "c", "d", "e"),
                      stringsAsFactors = F)
#
# create a test tree that we think we understand quite well
base_tree_a = read_tsv("../../data/example_tree.tsv") %>%
  dplyr::left_join(mapping_df_a) %>%
  dplyr::rename(Parent = subbasinID, Child = nextID) %>%
  as.data.frame() %>%
  data.tree::FromDataFrameNetwork()

#  ------------------------------
mapping_df_b =  data.frame(subbasinID = c(1, 3, 7, 8, 12, 14),
                      station_id = c("a", "b", "c", "d", "e", "f"),
                      stringsAsFactors = F)
#
# create a test tree that we think we understand quite well
base_tree_b = read_tsv("../../data/example_tree.tsv") %>%
  dplyr::left_join(mapping_df_b) %>%
  dplyr::rename(Parent = subbasinID, Child = nextID) %>%
  as.data.frame() %>%
  data.tree::FromDataFrameNetwork()

test_tree = data.tree::Clone(base_tree_a)

#  ------------------------------
#  ------------------------------
context("Create the tree object from the textfile:
        Tree looks like that:
        #  ------------------------------
        # -1
        #  ¦--1
        #     ¦--3
        #     ¦   ¦--7
        #     ¦   ¦   ¦--8
        #     ¦   ¦   ¦   ¦--11
        #     ¦   ¦   ¦   ¦--12
        #     ¦   ¦   ¦   ¦   °--14
        #     ¦   ¦   ¦   °--13
        #     ¦   ¦   ¦--9
        #     ¦   ¦   °--10
        #     ¦   °--6
        #     °--2
        #         ¦--4
        #         °--5
        #  ------------------------------\n")
#  -------------------------------------------------------------

context("Testing my understanding of the functions with regard to leaves and
        levels provided by the package")

# Test whether all node names are unique.
test_that("Test unique names", {
              expect_true(AreNamesUnique(base_tree_a))
})

# Number of leaves
test_that("Test the number of leaves", {
              # final leaves
              expect_equal(length(base_tree_a$leaves), 8)
              # within the tree
              # Here we find 11, 14, 13, so 3 leaves
              expect_equal(length(FindNode(base_tree_a, "8")$leaves), 3)
              # Here we find 14 only so a singe leave
              expect_equal(length(FindNode(base_tree_a, "12")$leaves), 1)
})

# The number of levels or the hierarchy of the tree
test_that("Test the number of levels", {
              # returns a named vector (node names) with the associated level
              # We have 6 levels (at node 14) in total
              tree_levels = base_tree_a$Get("level")
              expect_equal(max(tree_levels), 7)
              # we have level 1 at node 1
              expect_equal(as.numeric(tree_levels["1"]), 2)
              # that should be the same if we "find" the node
              expect_equal(as.numeric(FindNode(base_tree_a, "1")$level), 2)
              # we have level 4 at node 10
              expect_equal(as.numeric(tree_levels["10"]), 5)
              # that should be the same if we "find" the node
              expect_equal(as.numeric(FindNode(base_tree_a, "10")$level), 5)
})

#  -------------------------------------------------------------
context("Testing my understanding with regard to clone and subtree functions
        provided by the package")

# cloning a subtree
sub_a = Clone(FindNode(base_tree_a, "12"))
sub_b = Clone(FindNode(base_tree_a, "8"))
sub_c = Clone(FindNode(base_tree_a, "7"))

test_that("Test the extracting and cloning subtrees", {
              expect_true(isRoot(sub_a$"12"))
              expect_true(isRoot(sub_a$"8"))
})

test_that("Check the new levels", {
              expect_equal(as.numeric(FindNode(sub_a, "14")$level), 2)
              expect_equal(as.numeric(FindNode(sub_b, "11")$level), 2)
              expect_equal(as.numeric(FindNode(sub_b, "14")$level), 3)
})

#  -------------------------------------------------------------
context("Check the function to extract only nodes with a required attribute")

test_that("Extract only nodes with a required attribute from cloned trees", {
            test_a = extract_node_v_helper(sub_a, attribute = "station_id")
            expect_equal(character(0),  test_a)
            test_b = extract_node_v_helper(sub_b, attribute = "station_id")
            expect_equal("12",  test_b)
            test_c = extract_node_v_helper(sub_c, attribute = "station_id")
            expect_equal("12", test_c)
})

test_that("Extract only nodes with a required attribute from the base tree", {
            test_vect = extract_node_v_helper(base_tree_a, attribute = "station_id")
            expect_equal(sort(c("1", "2", "6", "7", "12")), sort(test_vect))
            # that one should fail
            expect_error(extract_node_v_helper(base_tree_a, attribute = "false_column"))
})


#  -------------------------------------------------------------
context("Check the function to reduce the tree to nodes with a required
        attribute only")

test_that("Test the reduced tree on example base_tree_a", {
            test_tree_reduced = reduce_tree(base_tree_a,
                                            attribute = 'station_id')
            test_reduced = test_tree_reduced$Get('height')
            names(test_reduced) = NULL
            expect_equal(sort(c(1, 1, 1, 2, 3)), sort(test_reduced))
})

test_that("Test the reduced tree on example base_tree_b", {
        test_tree_reduced = reduce_tree(base_tree_b,
                                        attribute = 'station_id')
            test_reduced = test_tree_reduced$Get('height')
            names(test_reduced) = NULL
            expect_equal(sort(c(1, 2, 3, 4, 5, 6)), sort(test_reduced))
})
