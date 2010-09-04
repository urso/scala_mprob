
package probability

/**
 * Distribution represents a discrete probability distribution with an monadic
 * interface for combining distributions and doing bayesian like inference.
 * It is recommended to use the distribution creation functions in the
 * probability.Probably object or the Distribution companion object to create a
 * Distribution object.
 *
 * For each value in a distribution object a probability value of type Double
 * in the range [0, 1] is stored.
 *
 */
class Distribution[A](private var map:scala.collection.Map[A,Double]) 
{
    import scala.collection._

    normalizeProbabilities

    private lazy val randomGenerator = new scala.util.Random

    /**
     * returns a new iterator
     */
    def iterator = map.iterator

    private def normalizeProbabilities = {
        var sum = map.valuesIterator.reduceLeft{ _ + _ }
        if (sum != 1.0) map = map.mapValues { _ / sum }
    }

    /**
     * returns an event its probability in this distribution.
     *
     * @param value event to find probability for
     * @return the probability [0, 1]
     */
    def probability(value : A) : Double = map.getOrElse(value, 0)

    /**
     * finds the most probable event in distribution and returns
     * its value and probability.
     *
     * @return tuple of value and its probability
     */
    def mostProbable : (A, Double) = map.reduceLeft { (best, value) =>
        if (best._2 < value._2) value 
        else                    best
    }

    /**
     * returns a randomly chosen value and its probability from distribution.
     * The probability a value returned depends on its probability in the
     * distribution.
     *
     * @return value and probability of randomly selected value
     */
    def pick : (A, Double) = {
        val r = randomGenerator.nextDouble
        var sum = 0.0
        for ((k, p) <- map) {
            sum += p
            if (r < sum) return (k,p)
        }
        return map.toStream.head
    }

    /**
     * returns a new distribution by applying the event function
     * to the distribution.
     *
     * @param fn event function
     * @return new distribution
     */
    def map[B](fn : A => B) : Distribution[B] = 
        Distribution(iterator.map { d => (d._2, fn(d._1)) })

    /**
     * return a new distribution by filtering values in distribution.
     */
    def filter(pred : A => Boolean) = Distribution(map.filterKeys(pred(_)))

    /**
     * returns probability of all events the predicate returns true for.
     */
    def query(pred : A => Boolean) : Double = 
        map.foldLeft(0.0) { (sum, d) => 
            if (pred(d._1)) sum + d._2
            else            sum
        }

    /**
     * monadic distribution combinator (monadic 'bind').
     * Using dep more complex probability distributions
     * with dependent and independent values can be generated.
     */
    def dep[B](f : A => Distribution[B]) : Distribution[B] =
        Distribution(
          this.iterator.flatMap { d:(A,Double) =>
            f(d._1).iterator.map { d2:(B,Double) =>
              (d._2 * d2._2, d2._1)
            }
          })

    def flatMap[B](f: A => Distribution[B]) : Distribution[B] = dep(f)

    /**
     * multiplies 2 independent distributions.
     */
    def mult[B](dist2:Distribution[B]) : Distribution[(A,B)] = 
        mult(dist2, {(a:A,b:B) => (a,b)})

    /**
     * multiplies 2 independent distributions and applies 
     * an event funtion on multiplied distributions.
     */
    def mult[B,C](dist2:Distribution[B], fn: (A,B) => C) = 
        this.dep { d:A =>
            dist2.map { k:B => fn(d,k) }
        }

    /**
     * multiplies 2 independent distributions.
     */
    def *[B,C](dist2:Distribution[B]) = this.mult(dist2)

    override def toString : String = {
        val sb = new StringBuilder
        map.foreach { kp:(A,Double) =>
            sb ++= kp._1.toString
            sb ++= " : " 
            sb ++= (kp._2 * 100.0).toString
            sb ++= "%\n"
        }
        sb.toString
    }

    def flatten[B] = Distribution.flatten(this.asInstanceOf[Distribution[Distribution[B]]])

    def distance(d2:Distribution[A]) : Double = {
      val keys = new mutable.HashSet[A]
      keys ++= (map.keys ++ d2.map.keys)
      keys.foldLeft(0.0) { (sum, k) => 
        val tmp = this.probability(k) - d2.probability(k)
        sum + tmp * tmp
      }
    }

    def adjustProbabilisticMinimum(newMin:Double = 0.01) = {
      new Distribution[A](map.mapValues { p:Double => 
        if (p > newMin) p else newMin
      })
    }
}

object Distribution {
    import Utils._
    import scala.collection._

    /**
     * creates a new discrete probability distribution from the map.
     */
    def apply[A](map:Map[A,Double]) = new Distribution(map)

    /**
     * create a new discrete probability Distribution from any iterator object
     */
    def apply[A](data:Iterator[(Double, A)]) : Distribution[A] = {
        val m = block1(new mutable.HashMap[A,Double]) { m =>
            data.foreach { item:(Double,A) =>
                val v : Double = m.getOrElse(item._2, 0)
                m += item._2 -> (v + item._1)
            }
        }
        //new Distribution[A](m.readOnly)
        new Distribution[A](m)
    }

    /**
     * create a new discrete probability Distribution from any iterable object
     */
    def apply[A](data:Iterable[(Double, A)]) : Distribution[A] = 
        Distribution(data.iterator)

    /**
     * creates a new discrete probability distribution from an iterable of data
     * objects and a "distribution" mapping.
     *
     * @param fn distribution mapping function: [0,1] => Double
     */
    def apply[A](data:Iterable[A], fn: Double => Double) : Distribution[A] = {
        val buf = new mutable.ArrayBuffer[A]
        buf ++= data
        val length = buf.length
        var i : Double = 0.0;

        Distribution[A]( buf.map { v =>
            i += 1.0
            (fn(i / length), v)
        })
    }

    def flatten[B](d:Distribution[Distribution[B]]) = d.dep[B]( x => x)

}

