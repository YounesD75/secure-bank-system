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

case class PetriNet(
  places:      Map[String, Place],
  transitions: Map[String, Transition]
) {

  // Marquage courant : placeId -> nb jetons
  def marking: Map[String, Int] =
    places.map { case (id, p) => id -> p.tokens }

  // Vérifie que chaque place d'entrée de t a assez de jetons
  def isEnabled(t: Transition): Boolean =
    t.inputs.forall { case (pid, need) =>
      places.get(pid).exists(_.tokens >= need)
    }

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

  // Reconstruit un PetriNet avec un marquage arbitraire (structure identique)
  private def fromMarking(m: Map[String, Int]): PetriNet =
    copy(places = places.map { case (id, p) => id -> p.copy(tokens = m.getOrElse(id, 0)) })

  // BFS : explore tous les marquages atteignables en au plus maxSteps franchissements
  def reachabilityAnalysis(maxSteps: Int): Set[Map[String, Int]] = {
    var visited  = Set(marking)
    var frontier = visited

    (1 to maxSteps).foreach { _ =>
      if (frontier.nonEmpty) {
        val next = for {
          m       <- frontier
          net2     = fromMarking(m)
          t       <- net2.enabledTransitions
          nextMark = net2.fire(t).marking
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
      fromMarking(m).enabledTransitions.isEmpty
    }

  // Retourne true si property est vraie sur TOUS les marquages atteignables en maxSteps étapes
  def checkLTL(property: Map[String, Int] => Boolean, maxSteps: Int): Boolean =
    reachabilityAnalysis(maxSteps).forall(property)

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
   *    T7 account_locked : 3×P3 → P7
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
      "P7" -> Place("P7", 0)    // AccountLocked (absorbant)
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

    PetriNet(places, transitions)
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
    if (!dead) println("   → Le réseau est vivant : chaque état atteignable a au moins une transition franchissable.")

    // ── P-invariant ───────────────────────────────────────────────
    val inv = net.checkPInvariant(
      Map("P0" -> 1, "P1" -> 1, "P2" -> 1, "P4" -> 1, "P5" -> 1)
    )
    println(s"\n── P-invariant P0+P1+P2+P4+P5=1 (30 étapes) : $inv")
    if (inv) println("   → Conservation de la session : exactement 1 jeton dans l'état session à tout moment.")

    // ── Vérification LTL ─────────────────────────────────────────────────────
    println(s"\n── Vérification LTL (checkLTL, maxSteps=30)")

    case class LtlSpec(
      name:     String,
      formula:  String,
      property: Map[String, Int] => Boolean,
      note:     String
    )

    val ltlSpecs = List(
      LtlSpec(
        name    = "LTL1 — G(¬valid_token → ¬balance_visible)",
        formula = "G(¬(P5>0 ∧ P6>0))",
        property = m => !(m("P5") > 0 && m.getOrElse("P6", 0) > 0),
        note    = "FAUX : P6 n'est jamais consommé ; après toute révocation P6>0 reste permanent.\n" +
                  "        Une nouvelle session valide peut atteindre P5=1 avec P6>0 encore présent.\n" +
                  "        Le modèle ne distingue pas l'identité des tokens — limitation du marquage scalaire."
      ),
      LtlSpec(
        name    = "LTL2 — G(failures >= 3 → AF account_locked)",
        formula = "G(¬(P3≥3 ∧ P7=0))",
        property = m => !(m.getOrElse("P3", 0) >= 3 && m.getOrElse("P7", 0) == 0),
        note    = "FAUX (attendu) : le BFS explore les états intermédiaires où P3≥3 avant que T7\n" +
                  "        soit franchi. Il s'agit d'une propriété de vivacité (AF) que la vérification\n" +
                  "        par marquages atteignables seuls ne peut pas prouver — il faudrait un model\n" +
                  "        checker LTL complet (CTL* / µ-calcul) pour établir AF account_locked."
      ),
      LtlSpec(
        name    = "LTL3 — G(account_locked → AG ¬valid_session)",
        formula = "G(¬(P7>0 ∧ P5>0))",
        property = m => !(m.getOrElse("P7", 0) > 0 && m.getOrElse("P5", 0) > 0),
        note    = "FAUX : T7 (account_locked) ne consomme que P3 et ne touche pas l'état session.\n" +
                  "        Il peut franchir pendant que la session est en P5 (balance consultée),\n" +
                  "        produisant {P5=1, P7=1}. Un modèle correct nécessiterait un arc P5→T7\n" +
                  "        ou une inhibition de T4/T5 par P7."
      ),
      LtlSpec(
        name    = "LTL4 — G(token_revoked → ¬balance_visible)",
        formula = "G(¬(P6>0 ∧ P5>0))",
        property = m => !(m.getOrElse("P6", 0) > 0 && m.getOrElse("P5", 0) > 0),
        note    = "FAUX : même raison que LTL1 — P6 est un compteur monotone croissant.\n" +
                  "        La présence de P6>0 coexiste avec P5=1 d'une nouvelle session valide.\n" +
                  "        Propriété satisfaite dans le système réel (TokenStore invalide\n" +
                  "        les tokens révoqués), mais non modélisable par marquage scalaire seul."
      )
    )

    ltlSpecs.foreach { spec =>
      val result = net.checkLTL(spec.property, maxSteps = 30)
      println(s"\n   ${spec.name}")
      println(s"   Formule  : ${spec.formula}")
      println(s"   Résultat : $result")
      println(s"   Note     : ${spec.note}")
    }

    println(s"\n$sep\n")
  }
}
