#  ------------------------------

library(testthat)
library(tidyverse)
library(data.tree)

#  ------------------------------
mapping_df =  data.frame(subbasinID = c(1, 2, 6, 7, 12),
                      station_id = c("a", "b", "c", "d", "e"), 
                      stringsAsFactors = F)
#
# create a test tree that we think we understand quite well
test_tree = read_tsv("./example_tree.tsv") %>%
  left_join(mapping_df) %>%
  rename(Parent = subbasinID, Child = nextID) %>%
  as.data.frame() %>%
  FromDataFrameNetwork() 

context("Create the tree object from the textfile:
        Tree looks like that:
        #  ------------------------------
        # 0
        # ¦--1
        #    ¦--3
        #    ¦   ¦--7
        #    ¦   ¦   ¦--8
        #    ¦   ¦   ¦   ¦--11
        #    ¦   ¦   ¦   ¦--12
        #    ¦   ¦   ¦   ¦   °--14
        #    ¦   ¦   ¦   °--13
        #    ¦   ¦   ¦--9
        #    ¦   ¦   °--10
        #    ¦   °--6
        #    °--2
        #        ¦--4
        #        °--5
        #  ------------------------------\n")


#  -------------------------------------------------------------
context("Testing my understanding of the functions with regard to leaves and
        levels provided by the package")

# Test whether all node names are unique.
test_that("Test unique names", {
              expect_true(AreNamesUnique(test_tree))
})

# Number of leaves
test_that("Test the number of leaves", {
              # final leaves
              expect_equal(length(test_tree$leaves), 8)
              # within the tree
              # Here we find 11, 14, 13, so 3 leaves
              expect_equal(length(FindNode(test_tree, "8")$leaves), 3)
              # Here we find 14 only so a singe leave
              expect_equal(length(FindNode(test_tree, "12")$leaves), 1)
})

# The number of levels or the hierarchy of the tree
test_that("Test the number of levels", {
              # returns a named vector (node names) with the associated level
              # We have 6 levels (at node 14) in total
              tree_levels = test_tree$Get("level")
              expect_equal(max(tree_levels), 7)
              # we have level 1 at node 1
              expect_equal(as.numeric(tree_levels["1"]), 2)
              # that should be the same if we "find" the node
              expect_equal(as.numeric(FindNode(test_tree, "1")$level), 2)
              # we have level 4 at node 10
              expect_equal(as.numeric(tree_levels["10"]), 5)
              # that should be the same if we "find" the node
              expect_equal(as.numeric(FindNode(test_tree, "10")$level), 5)
})

#  -------------------------------------------------------------
context("Testing my understanding with regard to clone and subtree functions
        provided by the package")

# cloning a subtree
sub_a = Clone(FindNode(test_tree, "12"))
sub_b = Clone(FindNode(test_tree, "8"))

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
