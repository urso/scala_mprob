
package probability

/**
 * convenience functions for Distribution class.
 */
object Probability {
    import Utils._
    import Math._

    /**
     * creates a new uniformly distributed discrete probability distribution
     * for all objects in the iterable.
     */
    def uniform[A](data:Iterable[A]) : Distribution[A] = Distribution(data, K(1))

    /**
     * creates a new linearly distributed discrete probability distribution for
     * all objects in the iterable.
     */
    def linear[A](data:Iterable[A]) : Distribution[A] = Distribution(data, {x => x})

    /**
     * creates a new discrete probability distribution for all objects in the
     * iterable with a distribution of exp(-x)
     */
    def negExp[A](data:Iterable[A]) : Distribution[A] = Distribution(data, {x => exp(-x)})

    /**
     * creates a gaussian function with set mean and standard deviation.
     */
    def normalDistShape(mean : Double, dev : Double)(x:Double) = {
        val u = (x - mean) / dev;
        exp(-.5 * u * u) / sqrt(2 * Pi)
    }

    /**
     * creates a new discrete probability distribution for all objects in the
     * iterable having a normal distribution.
     */
    def normal[A](data:Iterable[A]) : Distribution[A] = Distribution(data, normalDistShape(0.5, 0.5))

    /**
     * creates binary distribution with first object 'a1' having probability p
     * and a2 having probability (1 - p)
     *
     * @param p probability of first value (must be in range [0,1])
     * @param a1 first value with probability p
     * @param a2 second value with probability (1 - p)
     *
     * @return new binary Distribution
     */
    def flip[A](p:Double, a1:A, a2:A) : Distribution[A] =
        Distribution( Map ( a1 -> p, a2 -> (1.0 - p)))

    /**
     * create boolean distribution with true having probability p and false
     * having probability (1 - p)
     */
    def flip(p:Double) : Distribution[Boolean] = flip(p, true, false)

    /**
     * create a distribution with only one value having a probability of 1.0
     */
    def single[A](a:A) : Distribution[A] = Distribution( Map { a -> 1.0} )

    /**
     * condition guards the evaluation of the probability 'tree' from advancing
     * if a predicate does not hold. Can be used for bayesian inference and
     * early pruning of search space.
     *
     * (use with normalize on final distribution to do bayesian inference).
     */
    def condition[A](b:Boolean, f : => Distribution[Option[A]]) = if (b) f else single(None)

    def just[A](a:A)(b:A) : Boolean = a == b

    def oneOf[A](as:A*)(x:A) : Boolean = as.contains(x)

    /**
     * computes expectation value of the distribution.
     */
    def expectation(d:Distribution[Double]) : Double = 
        d.iterator.foldLeft(0.0) { (sum, kp) => sum + kp._1 * kp._2 }

    /**
     * computes the variance of the distribution
     */
    def variance(d:Distribution[Double]) : Double = {
        val e = expectation(d)
        d.iterator.foldLeft(0.0) { (sum, kp) =>
            val tmp = (kp._1 - e)
            sum + tmp * tmp  * kp._2
        }
    }

    /**
     * computes the standard deviation of the discrete 
     * Distribution.
     */
    def std_dev(d:Distribution[Double]) : Double = sqrt(variance(d))

    /**
     * normalizes distribution by filtering out all None values.
     */
    def normalize[A](d:Distribution[Option[A]]) : Distribution[A] = 
        d.filter { s:Option[A] => s.isDefined }.map { a:Option[A] => a.get}
}

