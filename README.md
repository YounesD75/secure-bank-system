# SecureBank Auth System

Modélisation et vérification formelle d'un système d'authentification bancaire distribué.
Projet de fin de semestre — Systèmes Distribués (Akka/Scala · Réseaux de Pétri · LTL · Spark).

## Stack technique

| Composant | Version |
|---|---|
| Scala | 2.13.12 |
| SBT | 1.9.8 |
| Akka Typed | 2.6.20 |
| Apache Spark | 3.4.1 |
| ScalaTest | 3.2.17 |
| Logback | 1.2.11 |

## Acteurs implémentés

| Acteur | Rôle |
|---|---|
| `AuthServer` | Vérifie les credentials, émet les tokens, gère le lock après 3 échecs |
| `TokenStore` | Stocke et révoque les tokens, comptabilise les échecs par utilisateur |
| `Client` | Machine à états : Idle → AwaitingAuth → Session → Bloqué |
| `ResourceServer` | Valide le token avant de donner accès au solde |
| `Attacker` | Simule brute-force, replay d'ancien token, credential stuffing |

## Tests

| Suite | Ce qui est couvert |
|---|---|
| `AuthServerSpec` | Émission de token, rejet de credentials invalides, verrouillage après 3 échecs |
| `TokenStoreSpec` | Stockage, révocation, réponse `TokenInvalid` sur token révoqué |
| `ResourceServerSpec` | Accès autorisé avec token valide, refus avec token révoqué |
| `AttackerSpec` | Brute-force détecté, replay rejeté, credential stuffing bloqué |
| `ClientSpec` | Cycle nominal Idle → Auth → Session → Idle, transition vers Bloqué |
| `SecureBankIntegrationSpec` | Scénario complet multi-acteurs end-to-end |
| `SecurityAnalyzerSpec` | Pipeline Spark : comptage d'événements, détection brute-force |

## Réseau de Pétri (`petri/PetriNet.scala`)

### Places

| Place | Nom | Description |
|---|---|---|
| P0 | Idle | Client en attente (1 jeton initial) |
| P1 | AwaitingAuth | Requête d'authentification envoyée |
| P2 | TokenEmitted | Authentification réussie, token généré |
| P3 | FailedAttempt | Compteur d'échecs (cumulatif) |
| P4 | TokenActive | Token stocké, prêt à être utilisé |
| P5 | ValidatedSession | Solde accessible |
| P6 | TokenRevoked | Token révoqué (place monotone) |
| P7 | AccountLocked | Compte verrouillé définitivement |

### Transitions

| Transition | Label | Arc |
|---|---|---|
| T0 | authenticate | P0 → P1 |
| T1 | auth_success | P1 → P2 |
| T2 | auth_failure | P1 → P0 + P3 |
| T3 | store_token | P2 → P4 |
| T4 | check_token | P4 → P5 · *garde P7=0* |
| T5 | disconnect | P5 → P0 |
| T6 | revoke_token | P4 → P0 + P6 |
| T7 | account_locked | 3×P3 → P7 · *garde P5=0* |

### Gardes et propriétés vérifiées

- **P-invariant** `P0+P1+P2+P4+P5 = 1` — conservation de la session (exactement 1 jeton dans l'état session).
- **LTL3** `G(account_locked → ¬valid_session)` — **VRAI** grâce aux deux gardes complémentaires sur T4 et T7.
- **LTL1/LTL4** `G(token_revoked → ¬balance_visible)` — FAUX en Pétri scalaire (P6 monotone) ; propriété satisfaite dans le système Akka réel via `TokenInvalid`.
- **LTL2** `G(failures≥3 → AF account_locked)` — FAUX en BFS pur (opérateur F non supporté par `checkLTL`).

## Couche analytique Spark (`analytics/`)

| Fichier | Rôle |
|---|---|
| `SecurityEvent.scala` | Case class `SecurityEvent(user, eventType, timestamp, details)` |
| `ParquetEventWriter.scala` | Écrit les événements Akka en Parquet dans `data/security_events/` |
| `SecurityAnalyzer.scala` | Lit les Parquet, calcule stats globales et détecte brute-force/suspects |
| `SparkSecurityAnalysis.scala` | Point d'entrée autonome pour l'analyse post-simulation |

Le `SecurityAnalyzer` tourne en mode `local[*]` — aucun cluster requis.

## Commandes

```bash
# Lancer la simulation complète (3 scénarios : connexion normale, brute-force, post-blocage)
sbt run

# Lancer toute la suite de tests
sbt test

# Lancer un seul spec
sbt "testOnly securebank.AuthServerSpec"
```

Pour exécuter l'analyse du réseau de Pétri depuis la console interactive :

```bash
sbt console
```

```scala
// Dans le REPL sbt
import petri.PetriNetBuilder
PetriNetBuilder.report()
```

## Structure

```
secure-bank-system/
├── build.sbt
├── data/
│   └── security_events/        # Parquet produits par la simulation
└── src/
    ├── main/
    │   ├── resources/
    │   │   └── log4j2.xml
    │   └── scala/
    │       ├── Protocols.scala          # Messages partagés entre acteurs
    │       ├── AuthServer.scala
    │       ├── TokenStore.scala
    │       ├── Client.scala
    │       ├── ResourceServer.scala
    │       ├── Attacker.scala
    │       ├── Main.scala               # Simulation (3 scénarios)
    │       ├── BigDataSimulation.scala
    │       ├── SimpleWebServer.scala
    │       ├── analytics/
    │       │   ├── SecurityEvent.scala
    │       │   ├── ParquetEventWriter.scala
    │       │   ├── SecurityAnalyzer.scala
    │       │   └── SparkSecurityAnalysis.scala
    │       └── petri/
    │           └── PetriNet.scala       # PetriNet + PetriNetBuilder.report()
    └── test/
        └── scala/securebank/
            ├── AuthServerSpec.scala
            ├── TokenStoreSpec.scala
            ├── ClientSpec.scala
            ├── ResourceServerSpec.scala
            ├── AttackerSpec.scala
            ├── SecureBankIntegrationSpec.scala
            └── SecurityAnalyzerSpec.scala
```
