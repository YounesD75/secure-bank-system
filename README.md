# SecureBank Auth System

Modélisation et vérification formelle d'un système d'authentification bancaire distribué.
Projet de fin de semestre — Systèmes Distribués (Akka/Scala · Réseaux de Pétri · LTL · Spark).

---

---

### ÉTAPE 1 — Montrer la structure du projet

> "Le projet est structuré en trois couches : les acteurs Akka qui gèrent la logique métier,
> un réseau de Pétri pour la vérification formelle, et une couche Spark pour l'analyse des événements."

**Pointer :**
- `src/main/scala/*.scala` → les 5 acteurs
- `src/main/scala/petri/` → le modèle formel
- `src/main/scala/analytics/` → le pipeline Spark

---

### ÉTAPE 2 — Lancer la simulation

**Commande :**
```bash
sbt run
```

**Attendre les logs et pointer au fur et à mesure :**

Option [3] — securebank.SecureBankApp (Démonstration Scriptée) :

#### Scénario 1 — Connexion légitime (t = 0 ms)
```
[Client:alice] Connexion normale...
[Client:alice] Authentifié — a3f8c2d1... [user=alice, expire=29s]
[Client:alice] Session ouverte — accès au compte disponible
```
> "Alice envoie `Authenticate` à l'AuthServer. L'AuthServer vérifie les credentials dans sa Map,
> génère un token UUID avec TTL 30 secondes, et l'enregistre dans le TokenStore.
> En Pétri : transitions T0 → T1 → T3, on passe de P0 (Idle) à P4 (TokenActive)."

#### Scénario 1b — Consultation du solde (t = 400 ms)
```
[Client:alice] Demande de solde...
[ResourceServer] Token valide — solde alice : 4250.75 €
[Client:alice] Solde : 4250,75 €
```
> "Alice demande son solde. Le ResourceServer ne fait pas confiance au token directement —
> il demande au TokenStore de valider. C'est la séparation des responsabilités.
> Transition T4 : P4 → P5 (ValidatedSession), avec la garde P7=0 vérifiée."

```
[Client:alice] Déconnexion — retour à Idle
```
> "Déconnexion : T5, P5 → P0. Alice repasse en Idle."

#### Scénario 2 — Brute-force (t = 800 ms)
```
[Attacker:bob] Lancement brute-force...
[Attacker:bob] Brute-force échec — tentative 2 (brutepass_2)
[Attacker:bob] Brute-force échec — tentative 3 (brutepass_3)
[Attacker:bob] LTL vérifiée : G(failures >= 3 → AF account_locked)
```
> "L'Attacker envoie des mots de passe aléatoires. À chaque échec, le TokenStore incrémente
> un compteur interne et écrit un événement AUTH_FAILURE en Parquet.
> Au 3ème échec, il verrouille le compte et répond AccountLocked.
> En Pétri : 3 franchissements de T2 accumulent 3 jetons en P3, puis T7 tire → P7.
> L'Attacker log lui-même que la propriété LTL est vérifiée en runtime."

#### Scénario 3 — Credential stuffing (t = 2 300 ms)
```
[Attacker:multi] Credential stuffing — 6 couples
[Attacker:multi] Stuffing — essai alice:123456
[Attacker:multi] Stuffing — essai bob:password
[Attacker:multi] Stuffing — essai admin:admin
...
[Attacker:multi] Credential stuffing terminé
```
> "Un deuxième Attacker essaie 6 couples issus de fuites réelles : alice:123456, bob:password…
> Aucun ne passe. Quand il essaie bob, le compte est déjà verrouillé —
> l'AuthServer répond AccountLocked immédiatement, sans même vérifier le mot de passe."

Option [1] 
>"petri.PetriNetBuilder : Effectue la vérification formelle mathématique. Elle calcule les marquages atteignables et les >P-invariants pour prouver l'absence de deadlocks." 

Option [2] 
>"securebank.BigDataSimulation : Mode "Stress-Test". Elle lance 50 acteurs (utilisateurs et attaquants) en simultané pour générer un volume massif d'événements stockés au format Parquet."

Option [4] 
>"securebank.SimpleWebServer : Lance l'interface graphique (Dashboard). Elle permet de visualiser sous forme de graphiques les données analytiques produites par les simulations."

Options [5] & [6]
>"analytics.SecurityAnalyzer : Analyseurs Spark effectuant des requêtes SQL sur les fichiers Parquet pour identifier les comportements frauduleux a posteriori."

---

### ÉTAPE 3 — Lancer les tests

**Commande :**
```bash
sbt test
```

**Ce qu'on voit :**
```
[info] SecureBankIntegrationSpec:
[info] - should permettre un flux complet : Auth -> Obtention du Token -> Lecture du Solde
[info] - should bloquer toute la chaîne après une attaque par force brute (LTL 1 & 4)
[info] TokenStoreSpec:
[info] - should valider un token actif
[info] - should refuser un token révoqué
[info] - should refuser un token expiré
[info] - should bloquer un compte après 3 échecs
...
[info] Tests: succeeded 17, failed 0, canceled 1, ignored 0, pending 0
[info] All tests passed.
```

> "J'ai un test par acteur plus un test d'intégration end-to-end.
> Le test canceled c'est SecurityAnalyzerSpec — Spark 3.4.1 utilise Hadoop 3.3.4
> qui appelle une méthode retirée en Java 23 (JEP 486). J'ai instrumenté le test
> pour qu'il se cancèle proprement avec un message explicatif plutôt que de crasher."

**Ouvrir [SecureBankIntegrationSpec.scala](src/test/scala/securebank/SecureBankIntegrationSpec.scala) et pointer :**
- le flux Auth → token → GetBalance → BalanceOk(alice, 4250.75)
- le cas brute-force : 3 échecs → AccountLocked, puis même le bon mot de passe est bloqué

---

### ÉTAPE 4 — Analyse du réseau de Pétri

**Commande :**
```bash
sbt console
```

**Puis dans le REPL :**
```scala
import petri.PetriNetBuilder
PetriNetBuilder.report()
```

**Lire et commenter chaque bloc au fur et à mesure :**

```
── Marquage initial
   P0   : 1 jeton(s)   ← le système démarre en Idle
   P1–P7: 0 jeton(s)
```
> "Un seul jeton en P0 — le client est au repos, rien d'autre n'est actif."

```
── Transitions franchissables (M0)
   T0 [authenticate]
```
> "Depuis l'état initial, seule T0 est franchissable. On ne peut rien faire
> d'autre que s'authentifier. Le réseau est bien contraint dès le départ."

```
── Marquages atteignables (BFS, 10 étapes) : N état(s)
```
> "Le BFS explore tous les états atteignables par franchissement de transitions.
> On peut inspecter chaque marquage et vérifier qu'aucun état interdit n'apparaît."

```
── Deadlock détecté (10 étapes) : false
   → Réseau vivant
```
> "Pas de deadlock — le système ne peut pas se bloquer dans un état sans issue,
> sauf P7 qui est un état absorbant voulu : un compte verrouillé le reste définitivement."

```
── P-invariant P0+P1+P2+P4+P5=1 : true
   → Conservation de la session
```
> "Ce P-invariant prouve qu'on ne peut pas être simultanément en train de s'authentifier
> ET en session. La somme de ces jetons vaut toujours 1 — c'est l'unicité de session."

```
── LTL3 — G(account_locked → AG ¬valid_session)
   Résultat : true ✓
```
> "La propriété centrale : si le compte est verrouillé, aucune session valide ne peut exister.
> VRAI grâce à deux gardes complémentaires : T7 ne franchit pas si P5=1 (pas de verrouillage
> pendant une session active), et T4 ne franchit pas si P7=1 (pas de nouvelle session
> si compte verrouillé). J'ai eu besoin des deux — la garde sur T7 seule laissait un chemin résiduel."

```
── LTL1/LTL4 — Résultat : false
```
> "LTL1 et LTL4 sont FAUSSES en Pétri scalaire. P6 est un compteur monotone sans arc inhibiteur,
> il ne peut pas bloquer T4. Mais la propriété est garantie dans le système Akka réel :
> le TokenStore rejette activement les tokens révoqués via TokenInvalid.
> C'est une limite connue du Pétri non coloré — il faudrait un réseau de Pétri coloré
> pour modéliser l'identité des tokens."

---

## Référence technique

### Stack

| Composant | Version |
|---|---|
| Scala | 2.13.12 |
| SBT | 1.9.8 |
| Akka Typed | 2.6.20 |
| Apache Spark | 3.4.1 |
| ScalaTest | 3.2.17 |
| Logback | 1.2.11 |

### Acteurs

| Acteur | Rôle |
|---|---|
| `AuthServer` | Vérifie les credentials, émet les tokens, gère le lock après 3 échecs |
| `TokenStore` | Stocke et révoque les tokens, comptabilise les échecs par utilisateur |
| `Client` | Machine à états : Idle → AwaitingAuth → Session → Bloqué |
| `ResourceServer` | Valide le token avant de donner accès au solde |
| `Attacker` | Simule brute-force, replay d'ancien token, credential stuffing |

### Tests

| Suite | Ce qui est couvert |
|---|---|
| `AuthServerSpec` | Émission de token, rejet de credentials invalides, verrouillage après 3 échecs |
| `TokenStoreSpec` | Stockage, révocation, réponse `TokenInvalid` sur token révoqué |
| `ResourceServerSpec` | Accès autorisé avec token valide, refus avec token révoqué |
| `AttackerSpec` | Brute-force détecté, replay rejeté, credential stuffing bloqué |
| `ClientSpec` | Cycle nominal Idle → Auth → Session → Idle, transition vers Bloqué |
| `SecureBankIntegrationSpec` | Scénario complet multi-acteurs end-to-end |
| `SecurityAnalyzerSpec` | Pipeline Spark : comptage d'événements, détection brute-force *(canceled sur Java 23+)* |

### Réseau de Pétri — places

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

### Réseau de Pétri — transitions

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

### Couche Spark (`analytics/`)

| Fichier | Rôle |
|---|---|
| `SecurityEvent.scala` | Case class `SecurityEvent(user, eventType, timestamp, details)` |
| `ParquetEventWriter.scala` | Écrit les événements Akka en Parquet dans `data/security_events/` |
| `SecurityAnalyzer.scala` | Lit les Parquet, calcule stats globales et détecte brute-force/suspects |
| `SparkSecurityAnalysis.scala` | Point d'entrée autonome pour l'analyse post-simulation |

Mode `local[*]` — aucun cluster requis.

### Structure

```
secure-bank-system/
├── build.sbt
├── data/
│   └── security_events/        # Parquet produits par la simulation
└── src/
    ├── main/scala/
    │   ├── Protocols.scala          # Messages partagés entre acteurs
    │   ├── AuthServer.scala
    │   ├── TokenStore.scala
    │   ├── Client.scala
    │   ├── ResourceServer.scala
    │   ├── Attacker.scala
    │   ├── Main.scala               # Simulation (3 scénarios)
    │   ├── analytics/
    │   │   ├── SecurityEvent.scala
    │   │   ├── ParquetEventWriter.scala
    │   │   ├── SecurityAnalyzer.scala
    │   │   └── SparkSecurityAnalysis.scala
    │   └── petri/
    │       └── PetriNet.scala       # PetriNet + PetriNetBuilder.report()
    └── test/scala/securebank/
        ├── AuthServerSpec.scala
        ├── TokenStoreSpec.scala
        ├── ClientSpec.scala
        ├── ResourceServerSpec.scala
        ├── AttackerSpec.scala
        ├── SecureBankIntegrationSpec.scala
        └── SecurityAnalyzerSpec.scala
```