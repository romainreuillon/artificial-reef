package reef


@main def hello: Unit =
  val test = Reef.reef(
    V = 0, //Fishing cost per unit effort
    alpha = 0, //Ratio of MPA
    gama = 0, //Distribution of Fishing effort between fishing area and artificial reef
    deltaK = 0, //Fish carrying capacity per AR
    K = 100, //Fish carrying capacity of the environment
    a = 2, //Fish mobility
    c = 1,
    p = 1, //Fish price
    q = 1, //Fish catchability
    r = 0.5, //Fish growth rate
    beta0 = 1, //AR attraction parameter 1
    sigma = 0, //AR attraction parameter 2
    n0 = 1,
    E0 = 1,
    t = 10)
  println(test.toVector)

object Reef {

  def reef(
    V: Double,
    alpha: Double,
    gama: Double,
    sigma: Double,
    K: Double,
    deltaK: Double,
    a: Double,
    c: Double,
    p: Double,
    q: Double,
    r: Double,
    n0: Double,
    E0: Double,
    beta0: Double,
    t: Double,
    step: Double = 0.001) =

    def beta(v: Double) =
      beta0 * V / (1 + sigma * V)

    def nu1 =
      def p1 = a / ((1 - alpha) * K) + beta(V)
      def p2 = (a / ((1 - alpha) * K)) + beta(V) + (a / (alpha * K + V * deltaK))
      p1 / p2

    import math.*
    def dn(state: Array[Double], t: Double) =
      def n = state(0)
      def E = state(1)
      def p1 = (pow(nu1, 2.0) * n) / (alpha * K + V * deltaK)
      def p2 = (pow(1.0 - nu1, 2.0) * n) / ((1 - alpha) * K)
      def p3 = (q * gama * nu1 * n * E)
      def p4 = q * (1 - gama) * (1 - nu1) * n * E

      r * n * (1.0 - p1 - p2) - p3 - p4

    def dE(state: Array[Double], t: Double) =
      def n = state(0)
      def E = state(1)
      def p1 = p * q * gama * nu1 * n
      def p2 = p * q * (1 - gama) * (1 - nu1) * n

      (p1 + p2 - c) * E

    Dynamic(dn, dE).integrate(Array(n0, E0), step, Vector(t)).last
}