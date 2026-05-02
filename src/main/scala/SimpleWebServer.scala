// src/main/scala/securebank/SimpleWebServer.scala
package securebank

import java.net.InetSocketAddress
import com.sun.net.httpserver.{HttpServer, HttpExchange}
import java.nio.charset.StandardCharsets

object SimpleWebServer {
  
  def main(args: Array[String]): Unit = {
    val ports = Seq(8081, 8082, 8888, 9000)
    var server: Option[HttpServer] = None
    
    for (port <- ports) {
      try {
        server = Some(HttpServer.create(new InetSocketAddress(port), 0))
        println(s"✅ Serveur demarre sur le port $port")
        port
      } catch {
        case _: java.net.BindException =>
          println(s"⚠️ Port $port dejà utilise, essai suivant...")
      }
    }
    
    server match {
      case Some(s) => startServer(s)
      case None =>
        println("❌ Aucun port disponible. Liberez un port entre 8081-9000")
        println("   Commande: sudo lsof -i :8081 puis sudo kill -9 PID")
        System.exit(1)
    }
  }
  
  def startServer(server: HttpServer): Unit = {
    val port = server.getAddress.getPort
    
    server.createContext("/", (exchange: HttpExchange) => {
      val html = getDashboardHtml()
      sendResponse(exchange, 200, "text/html", html)
    })
    
    server.createContext("/api/stats", (exchange: HttpExchange) => {
      val stats = securebank.analytics.SecurityAnalyzer.analyze()
      val json = stats match {
        case Some(s) => 
          s"""{
            "totalEvents": ${s.totalEvents},
            "authFailures": ${s.authFailures},
            "authSuccess": ${s.authSuccess},
            "lockedAccounts": ${s.lockedAccounts},
            "revokedTokens": ${s.revokedTokens},
            "bruteForceUsers": [${s.bruteForceUsers.map(u => s""""$u"""").mkString(",")}],
            "suspiciousUsers": [${s.suspiciousUsers.map { case (u, f, succ) => s"""{"user":"$u","failures":$f,"success":$succ}""" }.mkString(",")}]
          }"""
        case None => 
          """{"error": "Aucune donnee. Lancez SecureBankApp d'abord"}"""
      }
      sendResponse(exchange, 200, "application/json", json)
    })
    
    server.setExecutor(null)
    server.start()
    
    println("╔════════════════════════════════════════════════════════════╗")
    println("║   🌐 SecureBank Dashboard                                  ║")
    println(s"║   http://localhost:$port                                    ║")
    println("║                                                            ║")
    println("║   ℹ️  Pour generer des donnees :                            ║")
    println("║   sbt 'runMain securebank.SecureBankApp'                  ║")
    println("╚════════════════════════════════════════════════════════════╝")
    
    println("\nAppuyez sur Ctrl+C pour arrêter le serveur")
    Thread.currentThread().join()
  }
  
  def sendResponse(exchange: HttpExchange, statusCode: Int, contentType: String, content: String): Unit = {
    val bytes = content.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.set("Content-Type", contentType)
    exchange.sendResponseHeaders(statusCode, bytes.length)
    val os = exchange.getResponseBody
    os.write(bytes)
    os.close()
  }

def getDashboardHtml(): String = """
<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>SecureBank - Security Dashboard</title>
    <style>
        :root {
            --bg-body: #0f172a;
            --bg-card: #1e293b;
            --accent: #38bdf8;
            --text-main: #f8fafc;
            --text-dim: #94a3b8;
            --danger: #ef4444;
            --success: #22c55e;
            --border: rgba(255, 255, 255, 0.1);
        }

        * { margin: 0; padding: 0; box-sizing: border-box; }
        body { 
            font-family: 'Inter', -apple-system, sans-serif; 
            background: var(--bg-body); 
            color: var(--text-main); 
            line-height: 1.6;
            padding: 40px 20px;
        }

        .container { max-width: 1100px; margin: 0 auto; }

        header { 
            display: flex; 
            justify-content: space-between; 
            align-items: center; 
            margin-bottom: 40px; 
            border-bottom: 1px solid var(--border);
            padding-bottom: 20px;
        }

        h1 { font-size: 1.5rem; font-weight: 600; letter-spacing: -0.025em; }
        h1 span { color: var(--accent); }
        
        h2 { font-size: 1.1rem; margin-bottom: 15px; color: var(--text-main); display: flex; align-items: center; gap: 8px; }

        .refresh-btn { 
            background: var(--accent); 
            color: #000; 
            border: none; 
            padding: 8px 16px; 
            border-radius: 6px; 
            cursor: pointer; 
            font-weight: 600; 
            font-size: 14px;
            transition: opacity 0.2s;
        }
        .refresh-btn:hover { opacity: 0.9; }

        .grid { 
            display: grid; 
            grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); 
            gap: 16px; 
            margin-bottom: 40px; 
        }

        .card { 
            background: var(--bg-card); 
            border: 1px solid var(--border);
            border-radius: 12px; 
            padding: 20px; 
            transition: transform 0.2s;
        }

        .card h3 { font-size: 12px; text-transform: uppercase; letter-spacing: 0.05em; color: var(--text-dim); margin-bottom: 8px; }
        .card .value { font-size: 24px; font-weight: 700; }
        
        .bad { color: var(--danger); }
        .good { color: var(--success); }

        .tables-section { display: grid; grid-template-columns: repeat(auto-fit, minmax(450px, 1fr)); gap: 24px; }

        .table-container { 
            background: var(--bg-card); 
            border: 1px solid var(--border);
            border-radius: 12px; 
            padding: 20px;
            overflow: hidden;
        }

        table { width: 100%; border-collapse: collapse; font-size: 14px; }
        th { text-align: left; color: var(--text-dim); font-weight: 500; padding: 12px 8px; border-bottom: 1px solid var(--border); }
        td { padding: 12px 8px; border-bottom: 1px solid rgba(255,255,255,0.05); }

        .status-pill { padding: 2px 8px; border-radius: 4px; font-size: 11px; font-weight: bold; text-transform: uppercase; }
        .status-bad { background: rgba(239, 68, 68, 0.2); color: var(--danger); }

        .info-box { 
            margin-top: 40px; 
            background: rgba(56, 189, 248, 0.05); 
            border: 1px solid rgba(56, 189, 248, 0.2);
            border-radius: 8px; 
            padding: 20px; 
            font-size: 14px;
        }
        code { color: var(--accent); background: rgba(0,0,0,0.3); padding: 2px 6px; border-radius: 4px; font-family: 'Courier New', monospace; }

        .footer { margin-top: 40px; text-align: center; color: var(--text-dim); font-size: 12px; }
    </style>
</head>
<body>
<div class="container">
    <header>
        <h1>🔒 Secure<span>Bank</span> Analytics</h1>
        <button class="refresh-btn" onclick="loadData()">Rafraîchir</button>
    </header>
    
    <div class="grid" id="statsCards">
        <!-- Skeleton loader simple -->
        <div class="card"><h3>Chargement...</h3><div class="value">-</div></div>
    </div>
    
    <div class="tables-section">
        <div class="table-container">
            <h2>🚨 Utilisateurs suspects</h2>
            <table id="suspiciousTable">
                <thead><tr><th>Utilisateur</th><th>Échecs</th><th>Succès</th><th>Statut</th></tr></thead>
                <tbody><tr><td colspan="4">En attente de données...</td></tr></tbody>
            </table>
        </div>
        
        <div class="table-container">
            <h2>🔒 Comptes bloqués</h2>
            <table id="lockedTable">
                <thead><tr><th>Utilisateur</th><th>Raison de l'alerte</th></tr></thead>
                <tbody><tr><td colspan="2">En attente de données...</td></tr></tbody>
            </table>
        </div>
    </div>
    
    <div class="info-box">
        <p><strong>Console d'administration :</strong> Pour alimenter ce tableau de bord, exécutez <code>sbt "runMain securebank.SecureBankApp"</code> dans votre terminal. Le système analyse les logs en temps réel via Spark.</p>
    </div>
    
    <div class="footer">
        © 2026 SecureBank Infra • Security Monitoring Engine
    </div>
</div>

<script>
async function loadData() {
    try {
        const response = await fetch('/api/stats');
        const data = await response.json();
        
        if (data.error) {
            document.getElementById('statsCards').innerHTML = `<div class="card" style="grid-column: 1/-1"><h3>Statut</h3><div class="value bad">${data.error}</div></div>`;
            return;
        }
        
        document.getElementById('statsCards').innerHTML = `
            <div class="card"><h3>Événements</h3><div class="value">${data.totalEvents || 0}</div></div>
            <div class="card"><h3>Échecs Auth</h3><div class="value bad">${data.authFailures || 0}</div></div>
            <div class="card"><h3>Succès Auth</h3><div class="value good">${data.authSuccess || 0}</div></div>
            <div class="card"><h3>Bloqués</h3><div class="value bad">${data.lockedAccounts || 0}</div></div>
            <div class="card"><h3>Révocations</h3><div class="value">${data.revokedTokens || 0}</div></div>
            <div class="card"><h3>Brute-force</h3><div class="value bad">${data.bruteForceUsers?.length || 0}</div></div>
        `;
        
        const suspBody = document.querySelector('#suspiciousTable tbody');
        if (data.suspiciousUsers?.length > 0) {
            suspBody.innerHTML = data.suspiciousUsers.map(u => `
                <tr><td><strong>${u.user}</strong></td><td class="bad">${u.failures}</td><td class="good">${u.success}</td><td><span class="status-pill status-bad">Suspect</span></td></tr>
            `).join('');
        } else {
            suspBody.innerHTML = '<tr><td colspan="4" style="text-align:center; padding: 20px; color: var(--text-dim)">Aucune activité suspecte</td></tr>';
        }
        
        const lockBody = document.querySelector('#lockedTable tbody');
        if (data.bruteForceUsers?.length > 0) {
            lockBody.innerHTML = data.bruteForceUsers.map(u => `
                <tr><td><strong>${u}</strong></td><td class="bad">Multiples échecs détectés</td></tr>
            `).join('');
        } else {
            lockBody.innerHTML = '<tr><td colspan="2" style="text-align:center; padding: 20px; color: var(--text-dim)">Aucun compte verrouillé</td></tr>';
        }
        
    } catch (err) {
        console.error('Erreur API:', err);
    }
}

loadData();
setInterval(loadData, 5000);
</script>
</body>
</html>
  """.stripMargin
}
