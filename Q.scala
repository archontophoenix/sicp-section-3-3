package queue

object Q { // purely functional queue

  case class Queue [A] (front: List[A] = Nil, revRear: List[A] = Nil) {
    def take: Option[(A,Queue[A])] = front.headOption match {
      case Some(a) =>
        Some((a,Queue(front.tail,revRear)))
      case None =>
        if (revRear.isEmpty) None else Queue(revRear.reverse).take
    }
    def put (a: A): Queue[A] = Queue(front,a :: revRear)
    override def toString: String = (front ++ revRear.reverse).mkString(" ")
    // Operations for Deque are symmetric:
    //    . putFront conses its argument onto front
    //    . takeRear removes element from rear, or if rear is empty and front
    //      is not, reverses front and turns it into rear
  }

  def main (args: Array[String]): Unit = {
    val q = Queue(List(1,2,3))
    println(q)
    val (h1,q1) = q.take.get
    println(s"Took $h1; left with $q1")
    val q2 = q1.put(4)
    println(s"Put 4; now have $q2")
    val (h3,q3) = q2.take.get
    println(s"Took $h3; left with $q3")
    val (h4,q4) = q3.take.get
    println(s"Took $h4; left with $q4")
    val (h5,q5) = q4.take.get
    println(s"Took $h5; left with $q5")
    val end = q5.take
    println(s"Tried to take; got $end")
  }

}
