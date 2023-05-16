package herdingcats

sealed trait TrafficLight
object TrafficLight {
  import cats.Eq  
  case object Red extends TrafficLight
  case object Yellow extends TrafficLight
  case object Green extends TrafficLight

  def red: TrafficLight = Red
  def yellow: TrafficLight = Yellow
  def green: TrafficLight = Green

  implicit val trafficLightEq: Eq[TrafficLight] = 
    new Eq[TrafficLight] {
      override def eqv(x: TrafficLight, y: TrafficLight): Boolean = x == y
    }  
}

object TrafficLightMain extends App {
  import cats.syntax.eq._  
  val eq = (TrafficLight.Red: TrafficLight) === (TrafficLight.Green: TrafficLight)
  println(eq)

  val eq2 = TrafficLight.red === TrafficLight.yellow
  println(eq2)  
}

