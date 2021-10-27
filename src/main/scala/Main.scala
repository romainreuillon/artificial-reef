package reef


@main def hello: Unit =
  val test = Reef.reef(

    c = 1, //cout d'exploitation
    p = 1, //Fish price

    q = 1, //Fish catchability
    K = 100, //Fish carrying capacity of the environment
    r = 0.5, //Fish growth rate
    a = 2, //Fish mobility

    alpha = 0.2, //Ratio of MPA
    deltaK = 10, //Fish carrying capacity per AR
    beta0 = 0.5, //AR attraction parameter 1
    sigma = 0.1, //AR attraction parameter 2

    V = 2.56, // Volume de recif ( (10m2)
    gama = 0, //Distribution of Fishing effort between fishing area and artificial reef

    // conditions initiale (pour EDO)
    n0 = 1,
    E0 = 1,
    t = 10)

  println(test.toVector)

@main def graph(gama: Double) =
  def curve(gama: Double) =
    (BigDecimal(0.0) to 10.0 by 1) map { V =>
      Reef.equilibrium(
        c = 1, //cout d'exploitation
        p = 1, //Fish price

        q = 1, //Fish catchability
        K = 100, //Fish carrying capacity of the environment
        r = 0.5, //Fish growth rate
        a = 2, //Fish mobility

        alpha = 0.2, //Ratio of MPA
        deltaK = 5, //Fish carrying capacity per AR
        beta0 = 1, //AR attraction parameter 1
        sigma = 0.1, //AR attraction parameter 2

        V = V.toDouble, // Volume de recif ( (10m2)
        gama = gama, //Distribution of Fishing effort between fishing area and artificial reef

        // conditions initiale (pour EDO)
        n0 = 1,
        E0 = 1,
        t = 10).sum
    }

  println(curve(gama))


@main def pattern =
  def f(gama: Double, V: Double) =
    Reef.equilibrium(
      c = 1, //cout d'exploitation
      p = 1, //Fish price

      q = 1, //Fish catchability
      K = 100, //Fish carrying capacity of the environment
      r = 0.5, //Fish growth rate
      a = 2, //Fish mobility

      alpha = 0.2, //Ratio of MPA
      deltaK = 5, //Fish carrying capacity per AR
      beta0 = 1, //AR attraction parameter 1
      sigma = 0.1, //AR attraction parameter 2

      V = V, // Volume de recif ( (10m2)
      gama = gama, //Distribution of Fishing effort between fishing area and artificial reef

      // conditions initiale (pour EDO)
      n0 = 1,
      E0 = 1,
      t = 10).sum

  println(Reef.seuilOptimum(f))
  println(Reef.seuilEfficacite(f))


object Reef {

  // F (gama, V) => capture totale
  def seuilOptimum(f: (Double, Double) => Double) =
    (BigDecimal(0.0) to 1.0 by 0.001).view.find { gama =>
      val f0 = f(gama.toDouble, 0.0)
      val f10 = f(gama.toDouble, 10.0)
      f10 >= 2 * f0
    }

  // F (gama, V) => capture totale
  def seuilEfficacite(f: (Double, Double) => Double) =
    (BigDecimal(0.0) to 1.0 by 0.001).sortBy { gama =>
      val f0 = f(gama.toDouble, 0.0)
      val f10 = f(gama.toDouble, 10.0)
      math.abs(f0 - f10)
    }.head.toDouble

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


  def equilibrium(
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
    import math.*

    def beta(v: Double) =
      beta0 * V / (1 + sigma * V)

    def nu1 =
      def p1 = a / ((1 - alpha) * K) + beta(V)
      def p2 = (a / ((1 - alpha) * K)) + beta(V) + (a / (alpha * K + V * deltaK))
      p1 / p2

    def n_eq_compute =
      c / (p * q * (gama * nu1 + (1 - gama) *(1-nu1)))

    def E_eq =
      math.max(
        r / (q*(gama*nu1 + (1-gama) * (1 - nu1))) * (1 - (pow(nu1,2)) * n_eq_compute / (alpha * K + V * deltaK) - (pow(1-nu1,2))*n_eq_compute/ ((1 - alpha) * K)),
        0.0
      )

    def K_Tilde =
      1 / ( pow(nu1,2)/(alpha*K + V *deltaK) +  (pow((1-nu1),2))/((1-alpha)*K) )

    def n_eq = if(E_eq == 0) K_Tilde else n_eq_compute


  // ATTENTION : Si E_eq devient négatif, c'est bien sur qu'il n'y a plus
    // de capture stable a l'equilibre possible, et donc il faut passer à
    // l'autre équilibre E_eq=0 qui devient alors stable, avec n_eq = K_tilde
//  i=find(E_etoile<0);
//  E_etoile(i)=0;
//  K_tilde = 1 ./ ( (nu1_etoile.^2)./(alpha_.*K + N.*dK) +  (nu2_etoile.^2)./((1-alpha_).*K) );
//  n_eq(i) = K_tilde;

    def Captures_zoneAH =
      q *(nu1* n_eq *gama * E_eq)

    def Captures_zone_peche =
      q*((1-nu1)*n_eq*(1-gama)*E_eq)

    def Captures_total =
      Captures_zoneAH + Captures_zone_peche

    Vector(Captures_zoneAH, Captures_zone_peche)

}