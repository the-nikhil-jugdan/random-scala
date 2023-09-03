// Abstract Class IntSet, can't create an instance.
// IntSet() would be an illegal call
// All classes extend java.lang.Object class
// Can also override methods with @override
abstract class IntSet: // Super class of Empty and Non Empty
  // Abstract member incl
  def incl(x: Int): IntSet
  // Abstract member contains
  def contains(x: Int): Boolean
  // Abstract member union
  def union(other: IntSet): IntSet
end IntSet

// Set Implementation as binary trees
// These are persistent data structures

// Empty Set Tree
object Empty extends IntSet: // Object definition for Empty Intset Object
  def contains(x: Int): Boolean    = false
  def incl(x: Int): IntSet         = NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other
end Empty

// Non Empty Set Tree
class NonEmpty(elem: Int, left: IntSet, right: IntSet)
    extends IntSet: // Sub class of IntSet
  def contains(x: Int): Boolean =
    if x < elem then left.contains(x)
    else if x > elem then right.contains(x)
    else true
  def incl(x: Int): IntSet =
    if x < elem then NonEmpty(elem, left.incl(x), right)
    else if x > elem then NonEmpty(elem, left, right.incl(x))
    else this

  def union(other: IntSet): IntSet = left.union(right).union(other).incl(elem)

end NonEmpty

// Extras

// Companion Objects
// Scala can have a class and object with same name since there are separate namespaces for types and values
// Classes belong to type namespace and objects belong to term namespace
// If a class and object with same name are in same source file they are called companion objects.
// Companion Objects play similar role to static class definitions in Java

// Two ways to run a program
object HelloWorld:
  // Similar syntax to Java
  def main(args: Array[String]): Unit = println("Hello World")
end HelloWorld

// The @main decorator makes the function as entry point to scala program
// More convinient
@main
def main(): Unit = println("Hello World")

// Dynamic Binding OR Dynamic Method Dispatch
// Code invoked by the method call depends on the runtime type of the object that contains the method

// Lecture 3.5
// Functions as objects
object IntSet:
  def apply()               = Empty
  def apply(x: Int)         = Empty.incl(x)
  def apply(x: Int, y: Int) = Empty.incl(x).incl(y)
end IntSet
