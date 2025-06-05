package main.scala

case class Leaf(value: Int) extends Tree {
  def getLeftSubtree: Tree = null
  def getRightSubtree: Tree = null
  def getNodeData: Int = value
}

object Leaf {
  def apply(value: Int) = new Leaf(value)
}