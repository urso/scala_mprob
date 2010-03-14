
object DrugTest {
    import probability._
    import Probability._

    sealed abstract class Status
    object User extends Status { override def toString = "User" }
    object Clean extends Status { override def toString = "Clean" }

    sealed abstract class TestResult
    object Positive extends TestResult { override def toString = "Positive" }
    object Negative extends TestResult { override def toString = "Negative" }

    val PosIfUser = 0.99
    val PosIfClean = 0.01

    val drugTest = choose(0.001, User, Clean).dep { u =>
        choose(if(u == User) PosIfUser else PosIfClean, Positive, Negative).dep { t =>
            single((u,t))
        }
    }

    val drugTest2 = drugTest.filter { ut:(Status, TestResult) =>
        ut._2 == Positive
    }

    def main(args:Array[String]) = 
        println(drugTest.toString ++ "\n")
        println(drugTest2.toString ++ "\n")
}

