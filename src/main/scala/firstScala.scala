import scala.annotation.tailrec
import scala.collection.immutable.Nil.:::

object ex extends App {
  // checking for member
  // [[HW1: (10 pts) complete the member function
  def member(a: Int, b: List[Int]): Boolean = {
    val size = b.length
    var contains = false
    for (i <- 0 to  size - 1){
      if (b(i) == a){
        contains = true
      }
    }
    //return
    contains
  }

  val l4 = List(1, 2, 3, 4)
  println(member(2, l4))
  println(member(5, l4))
  // HW1]]

  // [[HW2: (10 pts) complete the list equality function
  // Assume that list of chars are passed as parameters
  val l1 = List('A', 'B', 'C', 'D')
  val l5 = List('A', 'C', 'D')

  def equalsimp(a: List[Char], b: List[Char]): Boolean = {

 var eq = true
    var s1 = a.length
    var s2 = b.length
    if (s1 != s2) {
      eq = false
      return eq
    }
for(i <- 0 to s1){
  if(a(i) != b(i)){
    eq = false
  }
}
    eq
  }

  println(equalsimp(l1, l5))
  val l6 = List('A', 'C', 'D')
  println(equalsimp(l6, l5))
  // HW2]]

  // [[HW3: (10 pts) Complete the append function
  def append[A](l1: List[A], l2: List[A]): List[A] = {
    l1 ::: l2
    }
  println(append(l1, l6))
  println(append(l5, l1))

  val l7 = List(10, 11)
  println(append(l4, l7))
  // HW3]]

  // [[HW4: (10 pts) Complete the quadratic_roots function
  // hint: for square root, you can call math.sqrt() function
  def quadratic_roots(a: Int, b: Int, c: Int) = {
    val radicand = ((b*b) - (4 * a * c))
    var root_part : Double = 0
    if (radicand < 0){
      root_part = math.sqrt(-1 * ((b*b) - (4 * a * c)))
    }else{
      val root_part = math.sqrt((b*b) - (4 * a * c))
    }
    val minus_b  : Double = (-1 * b).asInstanceOf[Double]

    List((minus_b + root_part), (minus_b - root_part))

  }

  println(quadratic_roots(1, -2, -3))
  // HW4]]

  // [[HW5: (5 pts) Complete the factorial functin using recursion
  def factorial(n: Int): BigInt = {
    if (n > 1) {
      n * factorial(n - 1)
    } else {
       1
    }
  }
  println(factorial(10))
  println(factorial(100))
  println(factorial(10000)) // Does it work?
  // HW5]]

  // [[HW6: (5 pts) Complete the factorial function with tail recursion
  def factHelper(n: Int, factPartial: BigInt): BigInt ={
    if (n < 1){
      factPartial
    }else{
      factHelper((n-1), (n * factPartial))
    }
  }



  def tailFactorial(n: Int): BigInt = {
    factHelper(n, 1)

  }

  println(tailFactorial(10000)) // much faster and no stack overflow
  // HW6]]

  // [[HW7: (10 pts) Complete the function composition
  def h[A, B, C](f: B => C, g: A => B, x : A): C ={
    f(g(x))
  }


  val add2 = (x: Int) => x + 2
  val times3 = (x: Int) => x * 3
  println(h(add2, times3, 4))

  //HW7]

  // [[HW8: (10 pts) Complete the third_list function
  // it should return the third element of the list
  def third_list[A](ls: List[A]): A ={
    ls(3)
  }


  val l1 = List(1, 2, 3, 4)
  println(third_list(l1))
  println(third_list(List(1,2,(3,4,5), 6, 7)))
  // HW8]]

  // [[HW9: (10 pts) Complete the nth_list function
  // this function returns the nth element in the list
  def nth_list[A](n: Int, ls: List[A]): A = {
    ls(n)
  }
  println(nth_list(2, l1))
  println(nth_list(3, List(1,2,(3,4,5), 6, 7)))
  // HW9]]

  // [[HW10: (10 pts) Complete the following function
  def applyall[A, B](fun:A => B, l:List[A]): List[B] ={
    var newList: List[B] = new List[B]
    for(i <- 0 to length(l)){
      newList = newList ::: fun(l(i))
    }
    newList
  }


  def cube(x: Int): Int = x * x * x

  val l1 = List(2,3,4)
  println(applyall(cube, l1))
  println(applyall((x: Int) => x * x * x, List(3, 4, 5)))
  // HW10]]
  // [[HW11: (10 pts) Find the length of a list recursively
  def length[A](l: List[A]): Int = {
    if(l != null) {
      1 + length(l.tail)
    }else
      1

  }
    println(length(l1))
  println(length(List(1,2,(3,4,5), 6, 7)))
  }


