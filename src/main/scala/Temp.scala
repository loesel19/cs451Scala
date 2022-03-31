import java.util.NoSuchElementException

object hw5 extends App{
  abstract class IntList{
    def head: Int
    def tail: IntList
    def isEmpty: Boolean
    def add(elem: Int): IntList
    def isEqual(list: IntList): Boolean
    def createList(num: Int): IntList

  }
  case object Empty extends IntList{
    def head: Int = throw new NoSuchElementException
    def tail: IntList = throw new NoSuchElementException
    def isEmpty: Boolean = true
    def add(elem: Int): IntList = new Cons(elem, Empty)
    override def toString: String = "Empty"
    def isEqual(list: IntList): Boolean = {
      if (list.head.isInstanceOf[Nothing]){
        true
      }else
        false
    }

    override def createList(num: Int): IntList = {}
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

    override def createList(num: Int): IntList = {

    }
  }
  val list: IntList = new Cons(1, Empty)
  println(list)
}
