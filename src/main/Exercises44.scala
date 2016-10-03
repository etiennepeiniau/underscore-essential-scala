package main

object Exercises44 {

  def main(args: Array[String]): Unit = {
  }

}

sealed trait TrafficLight

case object Red extends TrafficLight

case object Orange extends TrafficLight

case object Green extends TrafficLight


sealed trait Calculation

case class Succeed(result: Int) extends Calculation

case class Fail(reason: String) extends Calculation

case class BottledWater(size: Int, source: Source, carbonated: Boolean)

sealed trait Source

case object Well extends Source
case object Spring extends Source
case object Tap extends Source