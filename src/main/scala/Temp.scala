
object hw5 extends App{
  abstract class IntList{
    def head: Int
    def tail: IntList
    def isEmpty: Boolean
    def add(elem: Int): IntList
    def isEqual(list: IntList): Boolean
    def createList(num: Int): IntList = {
      var newList = new IntList {
        override def head: Int = num % 10

        override def tail: IntList =

        override def isEmpty: Boolean = false

        override def add(elem: Int): IntList = {
          elem + head + tail
        }

        override def isEqual(list: IntList): Boolean = ???
      }
    }

  }
  case object Empty extends IntList{
    def head: Int = throw new NoSuchElementException
    def tail: IntList = throw new NoSuchElementException
    def isEmpty: Boolean = true
    def add(elem: Int): IntList = new Cons(elem, Empty)
    override def toString: String = ???
    def isEqual(list: IntList): Boolean = ???
  }
  class Cons(h: Int, t: IntList) extends IntList {
    def head: Int = h
    def tail: IntList = t
    def isEmpty: Boolean = false
    def add(elem: Int): IntList = new Cons(elem, this)
    override def toString: String = {
      var str: String = "[" + this.head + " "
      var tempTail:IntList = this.tail
      while(tempTail != Nil){
        str = str + tempTail.head + " "
        tempTail = tempTail.tail
      }
    str
    }
    def isEqual(list: IntList): Boolean = ???
  }
  val list: IntList = new Cons(1, Empty)
  println(list)
}
