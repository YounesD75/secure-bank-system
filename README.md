SecureBank - Guide de démarrage (Branche Rayan)

Ce projet utilise Akka Typed pour gérer la sécurité bancaire. La base (Personne 1) est prête.
 Ce qui fonctionne déjà :

    AuthServer : Reçoit les demandes de connexion et vérifie les identifiants.

    TokenStore :

        Génère un Token si l'auth réussit.

        Sécurité : Verrouille un compte après 3 échecs (Règle métier validée).

Mission pour la Personne 2 (Accès) :
Ton but est de créer les acteurs qui utilisent mon système :

    ResourceServer : Un acteur qui ne donne accès au "solde bancaire" que si le TokenStore confirme que le token est valide.

    Client : Un acteur qui tente de se connecter, récupère un token, et accède à la ressource.

    Attacker : Un acteur qui bombarde l'auth de mauvais mots de passe pour tester mon verrouillage automatique.

Protocole à utiliser :

Regarde Protocols.scala. Tu y trouveras :

    Authenticate(user, pass, replyTo) pour se connecter.

    AuthSuccess(token) ou AccountLocked pour les réponses.

Lancer le test de base :
Bash

sbt run

Vérifie les logs : tu verras l'admin réussir et l'utilisateur "user1" se faire bloquer après 3 essais.
