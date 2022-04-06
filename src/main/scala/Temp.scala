import java.util.NoSuchElementException
import scala.runtime.Nothing$

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
    override def toString: String = "[]"
    def isEqual(list: IntList): Boolean = {
      if (list.equals(this)){
        true
      }else
        false
    }

    override def createList(num: Int): IntList = ???
  }
  class Cons(h: Int, t: IntList) extends IntList {
    def head: Int = h
    def tail: IntList = t
    def isEmpty: Boolean = false
    def add(elem: Int): IntList = {createList(elem)}
    override def toString: String = {
      var str: String = "[" + this.head
      var tempTail:IntList = this.tail
      while(tempTail != Empty){
        str = str + " " + tempTail.head
        tempTail = tempTail.tail
      }
    str + "]"
    }
    def isEqual(list: IntList): Boolean = {
      if(this.isEmpty && list.isEmpty){
        true
      }else if(this.isEmpty || list.isEmpty) {
      false
      }else{
        if(this.head == list.head){
          this.tail.isEqual(list.tail)
        }else{
          false
        }
      }
    }

    override def createList(num: Int): IntList = {
      new Cons(num, this)
    }
  }
  def addInt(l1: IntList, l2: IntList): IntList = {
    //lets get the simple addition list
    var overFlowList = simpleAdd(l1, l2)
    //now lets see if its head is > 9
    if(overFlowList.head > 9){
      new Cons(1, new Cons(overFlowList.head - 10, minusTen(overFlowList.tail)))
    }else{
      minusTen(overFlowList)
    }

  }

  def minusTen(overFlowList: IntList): IntList = {
    if (!overFlowList.isEmpty){
      if (overFlowList.head > 10){
        new Cons(overFlowList.head - 10, minusTen(overFlowList.tail))
      }else
        new Cons(overFlowList.head, minusTen(overFlowList.tail))
    }else
      Empty
  }
  def simpleAdd(l1: IntList, l2: IntList): IntList = {
    /**
     * this method will recursively generate an IntList with no carry over addition
     */
    if (l1.isEmpty && l2.isEmpty) {
      Empty
    } else {
      if(!l1.tail.isEmpty && !l2.tail.isEmpty){
        //if we check the values here we can see if we need to add 1 to the head for carry over
        if(l1.tail.head + l2.tail.head < 9) {
          new Cons(l1.head + l2.head, simpleAdd(l1.tail, l2.tail))
        }else{
          //TODO figure out how to subract 10 from the tail here
          var x = new Cons(l1.head + l2.head + 1, simpleAdd(l1.tail, l2.tail))

          x
        }
      }else{
        new Cons(l1.head + l2.head, simpleAdd(l1.tail, l2.tail))
      }

    }
    //todo just pass full result of this to new function, which first checks if first elem (head) is > 9. if so add a new
    //todo head with a 1, and then go through the rest of the list, and create a new list side by side, but subtract 10
    //todo from any of the original list elements that are > 9 when we add them to the new list.

  }

}
