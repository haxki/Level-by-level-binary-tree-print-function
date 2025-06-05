package main.scala

case class Node(value: Int, left: Tree, right: Tree) extends Tree {
  def getLeftSubtree: Tree = left
  def getRightSubtree: Tree = right
  def getNodeData: Int = value
}

object Node {
  def apply(value: Int, left: Tree, right: Tree): Node = new Node(value, left, right)
}