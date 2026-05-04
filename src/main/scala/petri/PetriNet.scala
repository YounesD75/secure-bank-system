package petri

// ── Structures de base ────────────────────────────────────────────────────────

case class Place(id: String, tokens: Int)

case class Transition(
  id:      String,
  label:   String,
  inputs:  Map[String, Int],  // placeId -> nb jetons consommés
  outputs: Map[String, Int]   // placeId -> nb jetons produits
)

// ── Réseau de Pétri (immuable) ────────────────────────────────────────────────
//
// netGuards : gardes par transition — transitionId -> (placeId -> prédicat sur tokens)
// Une transition n'est franchissable que si TOUS ses prédicats de garde sont satisfaits
// en plus des exigences de jetons standard (simule un read-arc / arc inhibiteur).

case class PetriNet(
  places:      Map[String, Place],
  transitions: Map[String, Transition],
  netGuards:   Map[String, Map[String, Int => Boolean]] = Map.empty
) {

  // Marquage courant : placeId -> nb jetons
  def marking: Map[String, Int] =
    places.map { case (id, p) => id -> p.tokens }

  // Retourne une copie du réseau avec un marquage arbitraire (gardes conservées)
  def withMarking(m: Map[String, Int]): PetriNet =
    copy(places = places.map { case (id, p) => id -> p.copy(tokens = m.getOrElse(id, 0)) })

  // Surcharge avec gardes explicites (placeId -> prédicat) en plus des jetons
  def isEnabled(t: Transition, guards: Map[String, Int => Boolean]): Boolean = {
    val tokensOk = t.inputs.forall { case (pid, need) =>
      places.get(pid).exists(_.tokens >= need)
    }
    val guardsOk = guards.forall { case (pid, pred) =>
      places.get(pid).exists(p => pred(p.tokens))
    }
    tokensOk && guardsOk
  }

  // Délègue aux gardes stockées dans le réseau pour cette transition
  def isEnabled(t: Transition): Boolean =
    isEnabled(t, netGuards.getOrElse(t.id, Map.empty))

  // Franchit t si les conditions (tokens + gardes explicites) sont remplies
  def fireWithGuards(t: Transition, guards: Map[String, Int => Boolean]): Option[PetriNet] =
    if (isEnabled(t, guards)) Some(fire(t)) else None

  // Retourne un nouveau PetriNet avec le marquage mis à jour après franchissement de t
  def fire(t: Transition): PetriNet = {
    val updated = places.map { case (id, p) =>
      val delta = t.outputs.getOrElse(id, 0) - t.inputs.getOrElse(id, 0)
      id -> p.copy(tokens = p.tokens + delta)
    }
    copy(places = updated)
  }

  // Liste des transitions franchissables depuis le marquage courant
  def enabledTransitions: List[Transition] =
    transitions.values.filter(isEnabled).toList.sortBy(_.id)

  // BFS : explore tous les marquages atteignables en au plus maxSteps franchissements
  def reachabilityAnalysis(maxSteps: Int): Set[Map[String, Int]] = {
    var visited  = Set(marking)
    var frontier = visited

    (1 to maxSteps).foreach { _ =>
      if (frontier.nonEmpty) {
        val next = for {
          m        <- frontier
          net2      = withMarking(m)
          t        <- net2.enabledTransitions
          nextMark  = net2.fire(t).marking
          if !visited.contains(nextMark)
        } yield nextMark

        visited  = visited ++ next
        frontier = next
      }
    }
    visited
  }

  // Retourne true si au moins un marquage atteignable est un deadlock
  def hasDeadlock(maxSteps: Int): Boolean =
    reachabilityAnalysis(maxSteps).exists { m =>
      withMarking(m).enabledTransitions.isEmpty
    }

  // Retourne true si property est vraie sur TOUS les marquages atteignables
  def checkLTL(property: Map[String, Int] => Boolean, maxSteps: Int): Boolean =
    reachabilityAnalysis(maxSteps).forall(property)

  // Pour tout marquage m où trigger(m) est vrai, vérifie que forbidden(m) est faux
  def checkSafetyFromStates(
    trigger:   Map[String, Int] => Boolean,
    forbidden: Map[String, Int] => Boolean,
    maxSteps:  Int
  ): Boolean =
    reachabilityAnalysis(maxSteps).forall(m => !trigger(m) || !forbidden(m))

  // Vérifie que sum(coef(p) * tokens(p)) est constant sur tous les marquages atteignables
  def checkPInvariant(coefficients: Map[String, Int]): Boolean = {
    def value(m: Map[String, Int]): Int =
      coefficients.foldLeft(0) { case (acc, (id, coef)) =>
        acc + coef * m.getOrElse(id, 0)
      }
    val ref = value(marking)
    reachabilityAnalysis(30).forall(m => value(m) == ref)
  }
}

// ── Instanciation SecureBank + rapport ───────────────────────────────────────

object PetriNetBuilder {

  def main(args: Array[String]): Unit = report()

  /*
   * Réseau de Pétri du système d'authentification SecureBank :
   *
   *  Places :
   *    P0 Idle (1 jeton)   P1 AwaitingAuth   P2 TokenEmitted
   *    P3 FailedAttempt    P4 TokenActive     P5 ValidatedSession
   *    P6 TokenRevoked     P7 AccountLocked
   *
   *  Transitions :
   *    T0 authenticate   : P0 → P1
   *    T1 auth_success   : P1 → P2
   *    T2 auth_failure   : P1 → P0 + P3
   *    T3 store_token    : P2 → P4
   *    T4 check_token    : P4 → P5
   *    T5 disconnect     : P5 → P0
   *    T6 revoke_token   : P4 → P0 + P6
   *    T7 account_locked : 3×P3 → P7   [garde : P5 == 0]
   *    T4 check_token    : P4 → P5    [garde : P7 == 0]
   *
   *  Gardes :
   *    T7 : P5 == 0 — le verrouillage ne peut survenir pendant une consultation de solde.
   *    T4 : P7 == 0 — impossible de consulter le solde si le compte est déjà verrouillé.
   *  Ces deux gardes ensemble rendent {P5=1, P7=1} inatteignable → LTL3 vérifiée.
   *
   *  P-invariant : P0 + P1 + P2 + P4 + P5 = 1  (conservation de la session)
   */
  def secureBankNet: PetriNet = {
    val places = Map(
      "P0" -> Place("P0", 1),   // Idle
      "P1" -> Place("P1", 0),   // AwaitingAuth
      "P2" -> Place("P2", 0),   // TokenEmitted
      "P3" -> Place("P3", 0),   // FailedAttempt (compteur)
      "P4" -> Place("P4", 0),   // TokenActive / InSession
      "P5" -> Place("P5", 0),   // ValidatedSession (balance accessible)
      "P6" -> Place("P6", 0),   // TokenRevoked
      "P7" -> Place("P7", 0)    // AccountLocked
    )

    val transitions = Map(
      "T0" -> Transition("T0", "authenticate",
                inputs  = Map("P0" -> 1),
                outputs = Map("P1" -> 1)),
      "T1" -> Transition("T1", "auth_success",
                inputs  = Map("P1" -> 1),
                outputs = Map("P2" -> 1)),
      "T2" -> Transition("T2", "auth_failure",
                inputs  = Map("P1" -> 1),
                outputs = Map("P0" -> 1, "P3" -> 1)),
      "T3" -> Transition("T3", "store_token",
                inputs  = Map("P2" -> 1),
                outputs = Map("P4" -> 1)),
      "T4" -> Transition("T4", "check_token",
                inputs  = Map("P4" -> 1),
                outputs = Map("P5" -> 1)),
      "T5" -> Transition("T5", "disconnect",
                inputs  = Map("P5" -> 1),
                outputs = Map("P0" -> 1)),
      "T6" -> Transition("T6", "revoke_token",
                inputs  = Map("P4" -> 1),
                outputs = Map("P0" -> 1, "P6" -> 1)),
      "T7" -> Transition("T7", "account_locked",
                inputs  = Map("P3" -> 3),
                outputs = Map("P7" -> 1))
    )

    // Gardes :
    //   T7 ne franchit que si P5 == 0 (pas de verrouillage pendant une consultation de solde)
    //   T4 ne franchit que si P7 == 0 (pas de consultation de solde si compte verrouillé)
    val guards: Map[String, Map[String, Int => Boolean]] = Map(
      "T7" -> Map("P5" -> ((t: Int) => t == 0)),
      "T4" -> Map("P7" -> ((t: Int) => t == 0))
    )

    PetriNet(places, transitions, netGuards = guards)
  }

  def report(): Unit = {
    val net = secureBankNet
    val sep = "═" * 55

    println(sep)
    println("  SecureBank — Analyse du réseau de Pétri")
    println(sep)

    // ── Marquage initial ──────────────────────────────────────────
    println("\n── Marquage initial")
    net.marking.toList.sortBy(_._1).foreach { case (id, t) =>
      println(f"   $id%-4s : $t jeton(s)")
    }

    // ── Transitions franchissables depuis M0 ──────────────────────
    println("\n── Transitions franchissables (M0)")
    net.enabledTransitions match {
      case Nil => println("   Aucune")
      case ts  => ts.foreach(t => println(s"   ${t.id} [${t.label}]"))
    }

    // ── Analyse d'atteignabilité BFS ──────────────────────────────
    val reachable = net.reachabilityAnalysis(maxSteps = 10)
    println(s"\n── Marquages atteignables (BFS, 10 étapes) : ${reachable.size} état(s)")
    reachable.toList
      .map(m => m.filter(_._2 > 0).toList.sortBy(_._1))
      .sortBy(_.map { case (id, t) => s"$id$t" }.mkString)
      .foreach { entries =>
        val desc = if (entries.isEmpty) "∅"
                   else entries.map { case (id, t) => s"$id=$t" }.mkString(", ")
        println(s"   { $desc }")
      }

    // ── Deadlock ──────────────────────────────────────────────────
    val dead = net.hasDeadlock(maxSteps = 10)
    println(s"\n── Deadlock détecté (10 étapes) : $dead")
    if (!dead) println("   → Réseau vivant : chaque état atteignable a au moins une transition franchissable.")

    // ── P-invariant ───────────────────────────────────────────────
    val inv = net.checkPInvariant(
      Map("P0" -> 1, "P1" -> 1, "P2" -> 1, "P4" -> 1, "P5" -> 1)
    )
    println(s"\n── P-invariant P0+P1+P2+P4+P5=1 (30 étapes) : $inv")
    if (inv) println("   → Conservation de la session : exactement 1 jeton dans l'état session à tout moment.")

    // ── Vérification LTL ─────────────────────────────────────────
    println(s"\n── Vérification LTL (maxSteps=30)")

    // LTL1 — checkSafetyFromStates : pour tout état où P6>0, P5 doit être 0
    val ltl1 = net.checkSafetyFromStates(
      trigger   = m => m.getOrElse("P6", 0) > 0,
      forbidden = m => m.getOrElse("P5", 0) > 0,
      maxSteps  = 30
    )
    println(s"""
   LTL1 — G(¬valid_token → ¬balance_visible)
   Formule  : ∀m. P6(m)>0 → P5(m)=0   [checkSafetyFromStates]
   Résultat : $ltl1
   Note     : FAUX — P6 est un compteur monotone sans arc de retrait.
              Après toute révocation P6>0 persiste ; une nouvelle session
              peut atteindre P5=1. Correction nécessaire : garde P6==0 sur T3/T4,
              ou modélisation par jeton d'identité de token (coloured Petri net).""")

    // LTL2 — checkLTL : propriété de vivacité, attendue false par BFS pur
    val ltl2 = net.checkLTL(
      property = m => !(m.getOrElse("P3", 0) >= 3 && m.getOrElse("P7", 0) == 0),
      maxSteps = 30
    )
    println(s"""
   LTL2 — G(failures >= 3 → AF account_locked)
   Formule  : G(¬(P3≥3 ∧ P7=0))   [checkLTL]
   Résultat : $ltl2
   Note     : FAUX (attendu) — le BFS visite les états {P3≥3, P7=0} AVANT
              que T7 soit franchi. AF est une propriété de vivacité temporelle ;
              checkLTL ne vérifie que des propriétés de sûreté sur états isolés,
              pas des formules avec opérateur F (CTL* / µ-calcul requis).""")

    // LTL3 — checkLTL : corrigé par la garde P5==0 sur T7
    val ltl3 = net.checkLTL(
      property = m => !(m.getOrElse("P7", 0) > 0 && m.getOrElse("P5", 0) > 0),
      maxSteps = 30
    )
    println(s"""
   LTL3 — G(account_locked → AG ¬valid_session)
   Formule  : G(¬(P7>0 ∧ P5>0))   [checkLTL]
   Résultat : $ltl3
   Note     : ${if (ltl3) "VRAI ✓ — corrigé par deux gardes complémentaires :\n              · T7 garde P5==0 : pas de verrouillage pendant une consultation.\n              · T4 garde P7==0 : pas de consultation si compte verrouillé.\n              La garde T7 seule ne suffit pas — T7 pouvait franchir avant P5=1\n              puis la session atteindre P5. T4 bloque ce chemin résiduel."
               else       "FAUX — gardes insuffisantes, {P5=1, P7=1} reste atteignable."}""")

    // LTL4 — checkSafetyFromStates : symétrique de LTL1
    val ltl4 = net.checkSafetyFromStates(
      trigger   = m => m.getOrElse("P6", 0) > 0,
      forbidden = m => m.getOrElse("P5", 0) > 0,
      maxSteps  = 30
    )
    println(s"""
   LTL4 — G(token_revoked → ¬balance_visible)
   Formule  : ∀m. P6(m)>0 → P5(m)=0   [checkSafetyFromStates]
   Résultat : $ltl4
   Note     : FAUX — même cause que LTL1 (P6 monotone, pas d'inhibition de T4).
              La propriété est satisfaite dans le système Akka réel : TokenStore
              rejette les tokens révoqués via ValidateToken → TokenInvalid,
              mais cette invariance d'identité n'est pas exprimable en Pétri scalaire.""")

    println(s"\n$sep\n")
  }
}
