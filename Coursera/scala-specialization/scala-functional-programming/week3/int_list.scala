// Lecture 3.3 Polymorphism

// Using Type parameter [T] to define list for any type
trait List[T]:
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
end List

// Directly overrides head and tail from List trait
class Cons[T](val head: T, val tail: List[T]) extends List[T]:
  def isEmpty = false
end Cons

class Nil[T] extends List[T]:
  def isEmpty = true
  def head    = throw new NoSuchElementException("Nil.head")
  def tail    = throw new NoSuchElementException("Nil.tail")
end Nil

def singleTon[T](elem: T) = Cons[T](elem, Nil[T])

def nth[T](xs: List[T], n: Int): T = if xs.isEmpty || n < 0 then
  throw new IndexOutOfBoundsException("Index out of list range")
else if n == 0 then xs.head
else nth(xs.tail, n - 1)
