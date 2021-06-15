trait Queue {
  var queue:List[Double] = List.empty
  var head: Int = -1
  var tail: Int = -1
  def enqueue(elements: Double): String = {
    if(head == -1 && tail == -1) {
      head = head + 1
      tail = tail + 1
      queue = queue ::: List(elements)
      "element is enqueued"
    }
    else {
      tail = tail + 1
      queue = queue ::: List(elements)
      "element is enqueued"
    }
  }
  def dequeue(elements: Int): String = {
    if(head == -1 && tail == -1) {
      "Queue Underflow"
    }
    else if(head == tail ) {
      queue = queue.drop(elements)
      head = -1
      tail = -1
      "element dequeued"
    }
    else {
      queue = queue.drop(1)
      head = head + 1
      "element dequeued"
    }
  }
  def getQueue: List[Double] = {
    queue
  }
}
class DoubleQueue extends Queue {
  override def enqueue(elements: Double): String = {
    if(head == -1 && tail == -1) {
      head = head + 1
      tail = tail + 1
      val ItemDouble = elements * 2
      queue = queue ::: List(ItemDouble)
      "element is enqueued.."
    }
    else {
      tail = tail + 1
      val ItemDouble = elements * 2
      queue = queue ::: List(ItemDouble)
      "element is enqueued.."
    }}}
class SquareQueue extends Queue {
  override def enqueue(elements: Double): String = {
    if(head == -1 && tail == -1 ) {
      head = head + 1
      tail = tail + 1
      val ItemSquare = elements * elements
      queue = queue ::: List(ItemSquare)
      "element is enqueued.."
    }
    else {
      tail = tail + 1
      val ItemSquare = elements * elements
      queue = queue ::: List(ItemSquare)
      "element is enqueued.."
    }}}
object QueueObject extends App {
  val obj1 = new DoubleQueue
  val Obj2 = new SquareQueue
  println(obj1.enqueue(10))
  println(obj1.enqueue(20))
  println(obj1.enqueue(30))
  println(obj1.enqueue(40))
  println(obj1.enqueue(50))
  println(obj1.getQueue)
  println(Obj2.enqueue(10))
  println(Obj2.enqueue(20))
  println(Obj2.enqueue(30))
  println(Obj2.enqueue(40))
  println(Obj2.enqueue(50))
  println(Obj2.getQueue)
  println(Obj2.dequeue(10))
  println(Obj2.getQueue)
  println(Obj2.dequeue(20))
  println(Obj2.getQueue)
}
