package y2023

import common._

import scala.annotation.tailrec

class Day20 extends Solution {
  /** Representation of a pulse in the wiring network */
  case class Pulse(src: String, dest: String, level: Boolean)

  /**
   * Abstract base class for all nodes in the system
   * @param name Name of the node
   * @param out Outputs that this node will send pulses to
   */
  abstract class Node(val name: String, val out: List[String]) {
    /**
     * Function called on a node when a pulse arrives.
     * @param p The pulse entering the node
     * @return Tuple of (new node state, outgoing pulses)
     */
    def trigger(p: Pulse): (Node, List[Pulse])
  }
  //Broadcaster: Incoming pulse is duplicated to all outputs
  case class Broadcaster(override val name: String, override val out: List[String])
    extends Node(name, out) {
    override def trigger(p: Pulse): (Node, List[Pulse]) = {
      (this, out.map(o => Pulse(this.name, o, p.level)))
    }
  }

  /**
   * FlipFlops invert their state when a low pulse arrives, sending a pulse with opposite level to all children
   * @param name Name of the node
   * @param out Outputs that this node will send pulses to
   * @param state Current state. Initializes to false
   */
  case class FlipFlop(override val name: String, override val out: List[String], state: Boolean)
    extends Node(name, out) {
    override def trigger(p: Pulse): (Node, List[Pulse]) = {
      if (!p.level) { //On low pulse, invert state and send inverted state to all others
        val newff = FlipFlop(this.name, this.out, !this.state)
        (newff, out.map(o => Pulse(this.name, o, newff.state)))
      } else { //On high pulse, ignore it
        (this, Nil)
      }
    }
  }

  /**
   * Con modules track latest pulse from all inputs. If all latest were 1, sends low pulse. Otherwise sends high pulse
   * @param name Name of the node
   * @param out Outputs that this node will send pulses to
   * @param in Input nodes that send pulses to this Con module
   * @param state Tracking state of all inputs. Mapping (name -> latest pulse value)
   */
  case class Con(override val name: String, in: List[String], override val out: List[String], state: Map[String, Boolean])
    extends Node(name, out) {
    override def trigger(p: Pulse): (Node, List[Pulse]) = {
      val newState = state.updated(p.src, p.level)
      //If all latest receives are 1, pulse is 0. Otherwise, pulse is 1
      val pulses = out.map(o => Pulse(this.name, o, !newState.forall(_._2)))
      (Con(this.name, this.in, this.out, newState), pulses)
    }
  }
  //Class representing the state of the network. Also tracks num. high and low pulses sent
  case class NetState(net: Map[String, Node], high: Long, low: Long)

  /**
   * Parse the given network, returning a network graph of all nodes
   * @param inp The input network
   * @return A map from (node label -> node), where each node contains its own state, inputs and outputs
   */
  def parseNetwork(inp: List[String]): Map[String, Node] = {
    /**
     * Internal loop function
     * @param inp The input lines. One line is processed on each iteration
     * @param types The type of each module in the network. Mapping (name -> type)
     * @param ins The inputs to a given module (name -> names of inputs).
     *            Stored because we don't know ahead of time if a module is con or FF.
     * @param outs The outputs for a given module (name -> names of outputs)
     * @return Mapping (node label -> node) for all nodes in the graph
     */
    @tailrec
    def loop(inp: List[String], types: Map[String, String], ins: Map[String, List[String]], outs: Map[String, List[String]]): Map[String, Node] = {
      inp match {
        case Nil => //Finalize mapping
          val r = types.map{
            case (name, "broadcaster") => (name, Broadcaster("broadcaster", outs(name)))
            case (name, "%") => (name, FlipFlop(name, outs(name), state = false))
            case (name, "&") => (name, Con(name, ins(name), outs(name), ins(name).map(i => (i,false)).toMap))
          }
          //Remaining node that has no output (rx / output node). Add as "con" node that doesn't generate any pulses
          val rem = ins.keySet.filterNot(types.keySet)
          rem.foldLeft(r){case (m,name) => m.updated(name, Con(name, ins(name), Nil, ins(name).map(i => (i, false)).toMap))}
        case in::tail =>
          val (t,i,o) = in match { //t,i,o = types, ins, outs
            case s"broadcaster -> $out" =>
              (types.updated("broadcaster", "broadcaster"), ins.updated("broadcaster", List("button")), outs.updated("broadcaster", out.split(", ").toList))
            case _ =>
              val typ = in.substring(0, 1)
              val dashPos = in.indexOf("-")
              val name = in.substring(1, dashPos-1)
              val out = in.substring(dashPos+3).split(", ").toList
              val i = out.foldLeft(ins){case (m,s) => m.updatedWith(s){
                case Some(v) => Some(name::v);
                case None => Some(List(name))
              }}
              (types.updated(name, typ), i, outs.updated(name, out))
          }
          loop(tail, t, i, o)
      }
    }
    loop(inp, Map.empty, Map.empty, Map.empty)
  }

  /**
   * Send pulses throughout the network. Stops when no more pulses are generated (problem part 1)
   * or when a pulse matching a predicate is matched (problem part 2)
   * @param queue Queue of pulses
   * @param network The wire network
   * @param pred Predicate used to check for a pulse
   * @return (Network state, boolean indicating if predicate was matched)
   */
  @tailrec
  final def sendPulses(queue: Queue[Pulse], network: NetState, pred: Pulse => Boolean): (NetState, Boolean) = {
    val (p,q) = queue.deq()
    p match {
      case None => (network, false)
      case Some(pulse) if pred(pulse) => (network, true)
      case Some(pulse) =>
        val (newNode, ps) = network.net(pulse.dest).trigger(pulse)
        val newNet = network.net.updated(pulse.dest, newNode)
        val (h,l) = if (pulse.level) (network.high+1, network.low) else (network.high, network.low+1)
        val q2 = q.enqAll(ps)
        sendPulses(q2, NetState(newNet, h, l), pred)
    }
  }

  //Was used for debugging: Created a graphviz digraph for easier inspection of the graph
  def toDot(inp: Map[String, Node]): Unit = {
    println("digraph G {")
    //Print arrows
    inp.foreach{ case (_, node) => println(s"  ${node.name} -> ${node.out.mkString("", ", ", "")};")}
    //Set box types for everything but broadcaster and rx/output
    inp.foreach{
      case (_, FlipFlop(name,_,_)) => println(s"  $name [shape=box];")
      case (_, Con(name,_,_,_)) => println(s"  $name [shape=diamond];")
      case _ => ()
    }
    println("}")
  }

  override def solvePart1(inp: List[String]): Any = {
    val network = parseNetwork(inp)
    val q = Queue(Pulse("button", "broadcaster", level = false))

    //Press it a thousand times, generating a thousand results
    val res = (0 until 1000).foldLeft(NetState(network, 0, 0)){case (netstate,_) =>
      sendPulses(q, netstate, _ => false)._1 //Just extract network state as pred never matches
    }
    res.low * res.high
  }

  override def solvePart2(inp: List[String]): Any = {
    val network = parseNetwork(inp)
    val q = Queue(Pulse("button", "broadcaster", level = false))

    //Parent module to rx/output is always a conjunction. Find that one
    val parent = if (network.contains("output")) {
      network("output").asInstanceOf[Con].in.head
    } else {
      network("rx").asInstanceOf[Con].in.head
    }
    //Grandparents of that conjunction module. All of these must send a HIGH pulse on
    //the same cycle for the rx/output node to receive a low pulse
    val grandParents = network(parent).asInstanceOf[Con].in

    //Loop function to detect when a high pulse into a given node is found
    def loop(net: NetState, pred: Pulse => Boolean, iter: Long): Long = {
      val (newNet, matched) = sendPulses(q, net, pred)
      if (matched) {
        iter
      } else {
        loop(newNet, pred, iter + 1)
      }
    }

    grandParents.map{gp =>
      val pred = (p: Pulse) => p.src == gp && p.dest == parent && p.level
      loop(NetState(network, 0, 0), pred, 1L) //Start iteration from 1, such that pulse found on first iteration corresponds to one button press
    }.product
  }
}

object Day20 extends App {
  Solution.solve(new Day20, 20, 2023, 11687500L, 1)
}