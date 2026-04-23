# SecureBank Auth System

Modélisation et vérification formelle d'un système d'authentification bancaire distribué.
Projet de fin de semestre — Systèmes Distribués (Akka/Scala + Réseaux de Pétri + LTL).

## Stack technique
- **Akka Typed 2.6.20** — modèle acteurs
- **Scala 2.13.12**
- **SBT 1.9.8**
- Apache Spark (à venir) — détection d'anomalies

## Acteurs implémentés

| Acteur | Statut | Rôle |
|---|---|---|
| `AuthServer` | ✅ | Vérifie credentials, émet tokens, gère le lock |
| `TokenStore` | ✅ | Stocke/révoque tokens, compte les échecs |
| `Client` | ✅ | Machine à états : Idle → Auth → Session → Bloqué |
| `ResourceServer` | 🔜 | Vérifie token avant accès au solde |
| `Attacker` | 🔜 | Simule brute-force, replay, credential stuffing |

## Réseau de Pétri — places couvertes

| Place | Description | Couvert |
|---|---|---|
| P0 | Client Idle | ✅ |
| P1 | Requête Auth | ✅ |
| P2 | Auth OK | ✅ |
| P3 | Échecs (accumulation) | ✅ |
| P4 | Token Actif | ✅ |
| P5 | En Session | ✅ |
| P6 | Token Révoqué | ✅ (TokenStore) |
| P7 | Compte Bloqué | ✅ |

## Lancer la simulation

```bash
sbt run
```

## Scénarios simulés
1. Connexion normale (alice) — T0→T1→T3→T4→T5
2. Brute-force (user1) — T0→T2×3→T7→P7
3. Tentative post-blocage — AccountLocked immédiat

## Structure
```
src/main/scala/
├── Protocols.scala    # Messages partagés
├── AuthServer.scala   # Acteur 1
├── TokenStore.scala   # Acteur 2
├── Client.scala       # Acteur 3
└── Main.scala         # Simulation (3 scénarios)
```
