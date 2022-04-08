import hw5.{Cons, Empty}

import java.util.NoSuchElementException
import javax.swing.event.InternalFrameListener
import scala.runtime.Nothing$

object hw5 extends App{
     override def main(args: Array[String]): Unit ={
      println("*******Q1*******")
       var list1 = new Cons(1, new Cons(2, Empty))
       println(list1) // Should print [1 2]
       var listE = Empty
       println(listE)

       println("*******Q3*******")
       list1 = new Cons(3, new Cons(4, new Cons(5, Empty)))
        var list2 = new Cons(1, new Cons(2, new Cons(3, Empty)))
       println(addIntQuestion3(list1, list2)) //simpleAdd is the one for question 2

       println("*******Q4*******")
       val list12 = new Cons(8, new Cons(7, new Cons(2, Empty)))
       val list13 = new Cons(8, new Cons(8, new Cons(2, Empty)))
       val list14 = new Cons(8, new Cons(7, new Cons(9, Empty)))
       println(addIntQuestion4(list1, list12))
       println(addIntQuestion4(list13, list2))
       println(addIntQuestion4(list14, list1))

       println("*******Q5*******")
       val list3 = new Cons(1, new Cons(2, new Cons(3, new Cons(4, Empty))))
       println(addInt(list1, list3))
       println(addInt(list3, list2))
       println(addInt(list3, list14))

       println("*******Q6*******")
       println(intValue(list3))

       println("*******Q7*******")
       val list4 = createList(321)
       println(list4)

       println("*******Q8*******")
       println(subInt(list1, list2))
       val list5:IntList = createList(657)
       val list6 = createList(334)
       val list7 = subInt(list5, list6)
       println(list7)

       println("*******Q9*******")
       var l1 = createList(523)
       var l2 = createList(3251)
       var l3 = subInt(l1, l2)
       var l4 = subInt(l2, l1)
       println(intValue(l3)) // should print negative number –[1 5 2 3]
       println(intValue(l4))
    }
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

    /**
     * defined methods
     */
    override def toString: String = {
      //quick patch to add in negative number implementation
      var str: String = ""
      var tempTail = this.tail
      if (this.head > 0) {
         str = "[" + this.head
      }else{
         str = "-[" + (-1 * this.head)
      }
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

    override def createList(num: Int): IntList = ???
  }
  def subInt(l1: IntList, l2: IntList): IntList = {
    val minusList = getMinuses(l1, l2)
    //we can do the get value on this minus list, and it should work
    val minusVal = intValue(minusList)
    //we can just call createList and with how it is implemented the if our value is negative
    //the lists head element will be negative so we can just add a quick fix to the toString method
    //and we will be all set
    createList(minusVal)
  }
  def getMinuses(l1: IntList, l2: IntList): IntList = {
    val length1 = getLength(l1)
    val length2 = getLength(l2)
    if (length1 == length2){
      if(l1.isEmpty){
        Empty
      }else {
        new Cons((l1.head - l2.head), getMinuses(l1.tail, l2.tail))
      }
    }else if(length1 > length2){
      new Cons(l1.head, getMinuses(l1.tail, l2))
    }else{
      new Cons(-1 * l2.head, getMinuses(l1, l2.tail))
    }
  }
  def createList(num: Int): IntList = {
    val revList = reversedList(num)
    properList(revList)
  }
  def properList(reversed: IntList): IntList = {
    if (reversed.isEmpty){
      Empty
    }else{
      new Cons(getTailVal(reversed), properList(removeTail(reversed)))
    }
  }
  def reversedList(num: Int): IntList = {
    if (num == 0){
      Empty
    }else{
      new Cons(num % 10, reversedList(num/10))
    }
  }
  def getLength(l: IntList): Int = {
    if(!l.isEmpty){
      1 + getLength(l.tail)
    }else
      0
  }
  def getTailVal(l: IntList): Int = {
    if (l.tail.isEmpty){
      l.head
    }else{
      getTailVal(l.tail)
    }
  }
  def removeTail(l: IntList): IntList = {
    if(l.tail.isEmpty){
      Empty
    }else{
      new Cons(l.head, removeTail(l.tail))
    }
  }
  def addInt(l1: IntList, l2: IntList): IntList = {
    //again like question 4 we will get a list that has overflow in it, but only needs
    //10 subtracted from the overflow elements
    val overFlowList: IntList = addIntQuestion5(l1, l2)
    //like question 4 we have to see if the head is > 9 so we can add a new digit
    if(overFlowList.head > 9){
      new Cons(1, new Cons(overFlowList.head - 10, minusTen(overFlowList.tail)))
    }else{
      minusTen(overFlowList)
    }

  }
  def addIntQuestion5(l1: IntList, l2: IntList): IntList = {
    /**
     * this is going to be the addInt from question 5, allowing different size lists.
     * We could just use the intValue function to get the values of the 2 lists as actual integers
     * and then add them using the languages arithmetic, and convert that to an intList. However,
     * I am sure that will lose points...
     */
    val length1: Int = getLength(l1) //use a recursive method to get lengths
    val length2: Int = getLength(l2)
    if (length1 == length2){
      simpleAdd(l1, l2)
    }else{
      if (length1 > length2){
        if (addIntQuestion5(l1.tail, l2).head > 9){
          new Cons(l1.head + 1, addIntQuestion5(l1.tail, l2))
        }else {
          new Cons(l1.head, addIntQuestion5(l1.tail, l2))
        }
      }else{
        if(addIntQuestion5(l1, l2.tail).head > 9){
          new Cons(l2.head + 1, addIntQuestion5(l1, l2.tail))
        }else {
          new Cons(l2.head, addIntQuestion5(l1, l2.tail))
        }
      }
    }

  }
  def intValue(list: IntList): Int = {
    if(list.isEmpty){
      0
    }else{
      getTailVal(list) + (10 * intValue(removeTail(list)))
    }
  }

  def addIntQuestion4(l1: IntList, l2: IntList): IntList = {
    /**
     * this will add lists together that have the same length
     */
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
      if (overFlowList.head > 9){
        new Cons(overFlowList.head - 10, minusTen(overFlowList.tail))
      }else
        new Cons(overFlowList.head, minusTen(overFlowList.tail))
    }else
      Empty
  }
  def simpleAdd(l1: IntList, l2: IntList): IntList = {
    /**
     * this method will recursively generate an IntList with carry over addition, however
     * another method, minusTen is needed as well as some logic in addInt to make this work
     * properly
     */
    if (l1.isEmpty && l2.isEmpty) {
      Empty
    } else {
      if(!l1.tail.isEmpty && !l2.tail.isEmpty){
        //if we check the values here we can see if we need to add 1 to the head for carry over
        if(l1.tail.head + l2.tail.head < 9) {
          new Cons(l1.head + l2.head, simpleAdd(l1.tail, l2.tail))
        }else{
          var x = new Cons(l1.head + l2.head + 1, simpleAdd(l1.tail, l2.tail))
          x
        }
      }else{
        new Cons(l1.head + l2.head, simpleAdd(l1.tail, l2.tail))
      }

    }
    //todoxx just pass full result of this to new function, which first checks if first elem (head) is > 9. if so add a new
    //todoxx head with a 1, and then go through the rest of the list, and create a new list side by side, but subtract 10
    //todoxx from any of the original list elements that are > 9 when we add them to the new list.

  }
  def addIntQuestion3(l1: IntList, l2: IntList):IntList = {
    /**
     * this method will recursively generate an IntList with no carry over addition
     */
    if (l1.isEmpty && l2.isEmpty) {
      Empty
    } else {
      new Cons(l1.head + l2.head, simpleAdd(l1.tail, l2.tail))

    }
  }
}
