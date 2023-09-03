// Lecture 3.4 Objects Everywhere

// abstract class Boolean: 

//     def ifThenElse[T](t: => T, e: => T):T

//     def && (x: => Boolean): Boolean = ifThenElse(x, false)

//     def || (x: => Boolean): Boolean = ifThenElse(true, x)

//     def unary_! :Boolean = ifThenElse(false, true)
    
//     def == (x: Boolean): Boolean = ifThenElse(x, x.unary_!)

//     def != (x: Boolean): Boolean = ifThenElse(x.unary_!, x)

//     def ==> (x: Boolean): Boolean = ifThenElse(x, true)

// end Boolean

// object True extends Boolean:
//     def ifThenElse[T](t: => T, e: => T): T = t
// end True

// object False extends Boolean:
//     def ifThenElse[T](t: => T, e: => T): T = e
// end False

// Implementing a Natural Numbers class

abstract class Nat:
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat
    def + (that: Nat): Nat
    def - (that: Nat): Nat

end Nat

object Zero extends Nat:
    def isZero: Boolean = true
    def predecessor: Nat = throw new Exception()
    def successor: Nat = Succ(Zero)
    def + (that: Nat): Nat = that
    def - (that: Nat): Nat = if that.isZero then this else throw new Exception()
    override def toString(): String = "Zero"
end Zero

// Strictly positive numbers
class Succ(n: Nat)  extends Nat:
    def isZero: Boolean = false
    def predecessor: Nat = n
    def successor: Nat = Succ(this)
    def + (that: Nat): Nat = Succ(n + that)
    def - (that: Nat): Nat = if that.isZero then this else n - that.predecessor
    override def toString(): String = s"Succ($n)"
end Succ

val one = Succ(Zero)
val two = Succ(one)
val three = Succ(two)


class PureClasses:
    def main(args: Array[String]): Unit = println(one + two)
end PureClasses

@main
def main() = println(two + three)