package main.scala

import scala.annotation.tailrec
import scala.util.Random

trait Tree {
  def getLeftSubtree: Tree
  def getRightSubtree: Tree
  def getNodeData: Int
}

def generateTree(itemCount: Int): Tree = {
  def constructTree(tree: Tree, itemsLeft: Int): Tree = {
    if tree == null then
      constructTree(Node(Random.between(10,100), null, null), itemsLeft-1)
    if itemsLeft == 0 then
      tree
    else
      constructTree(insertTree(Random.between(10,100), tree), itemsLeft-1)
  }
  constructTree(null, itemCount)
}

def getMaxLevel(tree: Tree): Int = {
  def getMaxLevel(tree: Tree, level: Int): Int = {
    if (tree == null) level-1
    else if (tree.getLeftSubtree == null && tree.getRightSubtree == null) level
    else math.max(
      getMaxLevel(tree.getLeftSubtree, level+1),
      getMaxLevel(tree.getRightSubtree, level+1)
    )
  }
  getMaxLevel(tree, 0)
}

def printTree(tree: Tree): Unit = {
  if (tree == null) {
    println("Tree is empty!")
    return
  }
  
  val max_level = getMaxLevel(tree)
  
  @tailrec
  def dig_item(tree: Tree, route: Int, level: Int): Tree = {
    if level == 0 || tree == null then tree
    else if level == 1 then {
      if route == 0 then
        tree.getLeftSubtree
      else if route == 1 then
        tree.getRightSubtree
      else
        throw IllegalArgumentException(s"Inadmissible binary code {$route} for tree layer.")
    } else {
      dig_item(
        if route / Math.pow(2, level-1).toInt == 0 then tree.getLeftSubtree else tree.getRightSubtree,
        route % Math.pow(2, level-1).toInt,
        level - 1
      )
    }
  }

  def printTree_helper(tree: Tree, level: Int): Unit = {
    val levels_left = max_level - level
    val dashes = if (levels_left > 0) 3 * Math.pow(2, levels_left-1).toInt - 1 else 1
    val spaces = (if (levels_left > 1) 3 * (Math.pow(2, levels_left-1).toInt - 1) else 0)
      + (if levels_left > 0 then 2 else 0)

    (0 until Math.pow(2, level).toInt).foreach(counter => {
      val tree_item = dig_item(tree, counter, level)

      print(" " * spaces)
      if tree_item != null && tree_item.getLeftSubtree != null then
        print("╭" + "─" * (dashes-1))
      else print(" " * dashes)

      tree_item match
        case Node(value, _, _) => print(s"${Console.GREEN}($value)${Console.RESET}")
        case Leaf(value) =>       print(s"${Console.BLUE}[$value]${Console.RESET}")
        case _ =>                 print(" " * 4)

      if tree_item != null && tree_item.getRightSubtree != null then
        print("─" * (dashes-1) + "╮")
      else print(" " * dashes)
      print(" " * spaces)
    })
    println()
  }

  (0 to max_level).foreach(printTree_helper(tree, _))
}

def insertTree(value: Int, tree: Tree): Tree = {
  if (tree == null) return Node(value, null, null)

  if (tree.getLeftSubtree == null && value < tree.getNodeData)
    return Node(tree.getNodeData, Leaf(value), tree.getRightSubtree)
  if (tree.getRightSubtree == null && value >= tree.getNodeData)
    return Node(tree.getNodeData, tree.getLeftSubtree, Leaf(value))

  if (value < tree.getNodeData)
    Node(tree.getNodeData, insertTree(value, tree.getLeftSubtree), tree.getRightSubtree)
  else
    Node(tree.getNodeData, tree.getLeftSubtree, insertTree(value, tree.getRightSubtree))
}

@tailrec
def contains(value: Int, tree: Tree): Boolean = {
  if (value < tree.getNodeData) {
    if (tree.getLeftSubtree != null)
      contains(value, tree.getLeftSubtree)
    else false
  }
  else if (value > tree.getNodeData) {
    if (tree.getRightSubtree != null)
      contains(value, tree.getRightSubtree)
    else false
  }
  else true
}

def sum(tree: Tree): Int = {
  def sum_helper(left: Tree, right: Tree, acc: Int): Int = {
    val a = if (left == null) 0 else
      sum_helper(left.getLeftSubtree, left.getRightSubtree, left.getNodeData)
    val b = if (right == null) 0 else
      sum_helper(right.getLeftSubtree, right.getRightSubtree, right.getNodeData)
    a + b + acc
  }
  sum_helper(tree.getLeftSubtree, tree.getRightSubtree, tree.getNodeData)
}