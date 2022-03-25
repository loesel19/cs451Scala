import scala.annotation.tailrec

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
    var newSize = l1.length + l2.length
    var newList = l1
    for (i <- 0 to l2.length){
      var ind = i + l2.length
   //  newList(ind) = l2(i)
    }

    newList
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
}