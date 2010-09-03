
object MontyHall {
    import probability.Probability._

    sealed abstract class Door
    object A extends Door { override def toString = "A" }
    object B extends Door { override def toString = "B" }
    object C extends Door { override def toString = "C" }

    sealed abstract class Winning
    object Looser extends Winning { override def toString = "Looser"}
    object Winner extends Winning { override def toString = "Winner"}

    sealed abstract class Strategie
    object Stay   extends Strategie { override def toString = "stay" }
    object Switch extends Strategie { override def toString = "switch" }

    private val doors = List(A,B,C)

    private def selectDoor(s:Strategie, chosen:Door, open:Door) = 
      s match {
        case Stay   => single(chosen)
        case Switch => uniform(doors.filter({x=> x!=chosen && x!=open}))
      }

    def experiment(s:Strategie) = 
        for ( hidden <- uniform(doors); // 1. hide price
              chosen <- uniform(doors); // 2. let player choose door
              // 3. open a door
              open <- uniform(doors.filter{x=> x!=hidden && x!=chosen});
              // allow player to switch door:
              chosen <- selectDoor(s, chosen, open))
          yield(if (chosen == hidden) {
                    Winner
                } else {
                    Looser
                })

    def main(arg:Array[String]) = 
        for (strategie <- List(Stay, Switch)) {
            println(strategie.toString ++ ":\n" ++ experiment(strategie).toString ++ "\n")
        }
}

