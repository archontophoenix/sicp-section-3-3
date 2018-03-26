package circuits

import language.implicitConversions

// Circuit exercises of Section 3.3.4, with variations.
object Circuits {

  // Interface to circuit components, regardless of how they're implemented or
  // what they do. You can use this interface to build a circuit, but you have
  // to choose a particular implementation of Components to do anything with
  // the resulting circuit.
  trait Components {
    type Wire
    def orGate (a: Wire, b: Wire): Wire
    def andGate (a: Wire, b: Wire): Wire
    def inverter (a: Wire): Wire
  }

  // Library of interesting components that can be mixed in with an
  // implementation of Components
  trait Library { this: Components =>
    def halfAdder (a: Wire, b: Wire): (Wire,Wire) = {
      val d = orGate(a,b)
      val c = andGate(a,b)
      val e = inverter(c)
      val s = andGate(d,e)
      (s,c)
    }
    def fullAdder (a: Wire, b: Wire, cIn: Wire): (Wire,Wire) = {
      val (s,c1) = halfAdder(b,cIn)
      val (sum,c2) = halfAdder(a,s)
      val cOut = orGate(c1,c2)
      (sum,cOut)
    }
    // Ex 3.29: a \/ b = ~ (~a /\ ~ b)
    def compositeOrGate (a: Wire, b: Wire): Wire =
      inverter(andGate(inverter(a),inverter(b)))
    // Ex 3.30
    def rippleCarryAdder (ab: Seq[(Wire,Wire)], c: Wire): (Seq[Wire],Wire) =
      ab.headOption match {
        case None =>
          (Seq.empty,c)
        case Some((a,b)) =>
          val (s1,c1) = fullAdder(a,b,c)
          val (rest,cn) = rippleCarryAdder(ab.tail,c1)
          (s1 +: rest,cn)
      }
  }

  // An implementation of components as functions whose input is a time and
  // whose output is a boolean.
  trait FunctionOfTime extends Components {
    type Time = Double
    type Signal = Boolean
    type Wire = Time => Signal
    val orGateDelay: Time = 1.0
    val andGateDelay: Time = 1.0
    val inverterDelay: Time = 1.0
    def afterDelay (delay: Time, wire: Wire) = (t: Time) => wire(t - delay)
    def orGate (a: Wire, b: Wire) = afterDelay(orGateDelay,t => a(t) || b(t))
    def andGate (a: Wire, b: Wire) = afterDelay(andGateDelay,t => a(t) && b(t))
    def inverter (a: Wire): Wire = afterDelay(inverterDelay,! a(_))
    // Functions specific to this implementation:
    def alwaysFalse: Wire = _ => false
    def alwaysTrue: Wire = _ => true
    def trueUntil (change: Time): Wire = t => t < change
    def falseUntil (change: Time): Wire = t => change <= t
  }

  // Test adding 2 and 3 by examining output from ripple carry adder over time
  def functionOfTimeTest: Unit = {
    val library = new FunctionOfTime with Library
    import library._
    val zero = alwaysFalse
    val one = falseUntil(0.0)
    val two = Seq(zero,one,zero)
    val three = Seq(one,one,zero) // low-order bits first!
    val (outputs,carry) = rippleCarryAdder(two.zip(three),zero)
    println("\nTesting add of 2 (010) and 3 (110) with FunctionOfTime\n")
    (0 to 20).foreach { t =>
      def digitStr (bo: Boolean) = if (bo) "1" else "0"
      val outputStr = outputs.map(w => digitStr(w(t))).mkString(" ")
      println(s"At time $t, output = $outputStr, carry = ${digitStr(carry(t))}")
    }
  }

  // An implementation of components whose behavior is a sequence of events
  trait Scheduled extends Components {
    import collection.immutable.SortedSet
    type ID = String // unique identifier for each wire in a circuit
    trait Wire {
      def id: ID
    }
    private case class Or (a: Wire, b: Wire, id: ID) extends Wire
    private case class And (a: Wire, b: Wire, id: ID) extends Wire
    private case class Not (a: Wire, id: ID) extends Wire
    private case class Name (id: ID) extends Wire
    val freshness = new java.util.concurrent.atomic.AtomicLong
    def freshId () = "_" + freshness.getAndIncrement
    type Time = Double
    val orGateDelay: Time = 1.0
    val andGateDelay: Time = 1.0
    val inverterDelay: Time = 1.0
    trait Signal {
      def and (sig: Signal): Signal
      def or (sig: Signal): Signal
      def not: Signal
    }
    def orGate (a: Wire, b: Wire): Wire = Or(a,b,freshId)
    def andGate (a: Wire, b: Wire): Wire = And(a,b,freshId)
    def inverter (a: Wire): Wire = Not(a,freshId)
    // Functions specific to this implementation:
    def wire (id: ID): Wire = Name(id)
    def rename (id: ID, wire: Wire): Wire = wire match {
      case Or(a,b,_) => Or(a,b,id)
      case And(a,b,_) => And(a,b,id)
      case Not(a,_) => Not(a,id)
      case Name(_) => Name(id)
    }
    object Zero extends Signal {
      def and (sig: Signal) = Zero
      def or (sig: Signal) = sig
      def not = One
      override def toString: String = "0"
    }
    object One extends Signal {
      def and (sig: Signal) = sig
      def or (sig: Signal) = One
      def not = Zero
      override def toString: String = "1"
    }
    case class Event (id: ID, time: Time, signal: Signal) {
      override def toString: String = s"At time $time, $id => $signal"
    }
    implicit object EventOrdering extends Ordering[Event] {
      def compare (a: Event, b: Event) = a.time compare b.time match {
        case 0 => a.id compare b.id match {
          case 0 => (a.signal,b.signal) match {
            case (Zero,One) => -1
            case (One,Zero) => 1
            case _ => 0
          }
          case idCompare => idCompare
        }
        case timeCompare => timeCompare
      }
    }
    implicit def toEvent (e: (ID,Time,Int)): Event = e match {
      case (id,t,0) => Event(id,t,Zero)
      case (id,t,1) => Event(id,t,One)
      case (id,_,i) => sys.error(s"Not a valid signal for $id: $i")
    }
    type Actions = Map[ID,Set[(State,Time,Signal) => Event]]
    type Agenda = SortedSet[Event]
    type State = Map[ID,Signal]
    def schedule (circuits: Set[Wire], initEvents: Event*): Stream[Event] = {
      val actions = buildActions(circuits)
      val initAgenda = buildAgenda(initEvents)
      val initState: State = Map.empty.withDefaultValue(Zero)
      def events (agenda: Agenda, state: State): Stream[Event] =
        agenda.headOption match {
          case None =>
            Stream.empty
          case Some(e) =>
            val Event(id,now,signal) = e
            val es: Set[Event] =
              actions.getOrElse(id,Set.empty).map(act => act(state,now,signal))
            val outOfSequence = es.filter(_.time < now)
            if (! outOfSequence.isEmpty)
              sys.error(
                s"At time $now, can't schedule earlier events $outOfSequence")
            else
              Stream.cons(e,events(agenda.tail ++ es,state + (id -> signal)))
        }
      events(initAgenda,initState)
    }
    def interesting (e: Event): Boolean = e.id.headOption != Some('_')
    private def combine [A,B] (
        m0: Map[A,Set[B]], m1: Map[A,Set[B]]): Map[A,Set[B]] =
      (m0.keySet ++ m1.keySet).map { a =>
        a -> (m0.getOrElse(a,Set.empty) ++ m1.getOrElse(a,Set.empty))
      }.toMap
    private def buildActions (circuits: Set[Wire]): Actions =
      circuits.foldLeft(Map.empty: Actions) { case (acts,wire) =>
        combine(acts,actions(wire))
      }
    private def actions (circuit: Wire): Actions = circuit match {
      case Or(a,b,id) =>
        combine(
          combine(actions(a),actions(b)),
          Map(
            a.id ->
              Set(
                (st,t,sig) => Event(id,t + orGateDelay,sig.or(st(b.id)))),
            b.id ->
              Set(
                (st,t,sig) => Event(id,t + orGateDelay,sig.or(st(a.id))))))
      case And(a,b,id) =>
        combine(
          combine(actions(a),actions(b)),
          Map(
            a.id ->
              Set(
                (st,t,sig) => Event(id,t + andGateDelay,sig.and(st(b.id)))),
            b.id ->
              Set(
                (st,t,sig) => Event(id,t + andGateDelay,sig.and(st(a.id))))))
      case Not(a,id) =>
        combine(
          actions(a),
          Map(
            a.id ->
              Set(
                (st,t,sig) => Event(id,t + inverterDelay,sig.not))))
      case Name(_) => Map.empty
    }
    private def buildAgenda (events: Seq[Event]): Agenda =
      events.foldLeft(SortedSet.empty: Agenda) {
        case (agenda,Event(id,time,signal)) => agenda + Event(id,time,signal)
      }
  }

  def scheduledTest: Unit = {
    val library = new Scheduled with Library
    import library._
    val zero = wire("Z")
    val two = Seq(wire("A0"),wire("A1"),wire("A2"))
    val three = Seq(wire("B0"),wire("B1"),wire("B2"))
    val (outputs,carry) = rippleCarryAdder(two.zip(three),zero)
    val namedOutputs =
      outputs.zipWithIndex.map { case (wire,i) => rename(s"S$i",wire) }
    println("\nTesting add of 2 (010) and 3 (110) with Scheduled\n")
    val events =
      schedule(
        namedOutputs.toSet + rename("C",carry),
        ("Z",0.0,0),
        ("A0",0.0,0),
        ("A1",0.0,1),
        ("A2",0.0,0),
        ("B0",0.0,1),
        ("B1",0.0,1),
        ("B2",0.0,0)).filter(interesting)
    println(events.toList.mkString("\n"))
  }

  def main (args: Array[String]): Unit = {
    functionOfTimeTest
    scheduledTest
  }

}
