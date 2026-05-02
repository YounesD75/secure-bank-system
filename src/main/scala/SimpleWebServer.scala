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
<html>
<head>
    <meta charset="UTF-8">
    <title>SecureBank - Security Dashboard</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body { font-family: 'Segoe UI', sans-serif; background: #0a0e27; color: #fff; padding: 20px; }
        .container { max-width: 1200px; margin: 0 auto; }
        h1 { color: #00d4ff; margin-bottom: 20px; }
        h2 { color: #00d4ff; margin: 20px 0 10px; }
        .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; margin-bottom: 30px; }
        .card { background: rgba(255,255,255,0.1); border-radius: 10px; padding: 20px; }
        .card h3 { font-size: 14px; opacity: 0.7; }
        .card .value { font-size: 28px; font-weight: bold; }
        .bad { color: #ff6b6b; }
        .good { color: #00ff88; }
        table { width: 100%; border-collapse: collapse; background: rgba(255,255,255,0.05); border-radius: 10px; overflow: hidden; margin-top: 10px; }
        th, td { padding: 12px; text-align: left; border-bottom: 1px solid rgba(255,255,255,0.1); }
        th { background: rgba(0,212,255,0.2); }
        .refresh-btn { background: #00d4ff; border: none; padding: 10px 20px; border-radius: 5px; cursor: pointer; margin-bottom: 20px; color: #0a0e27; font-weight: bold; }
        .footer { margin-top: 30px; text-align: center; opacity: 0.5; font-size: 12px; }
        .alert { background: rgba(255,107,107,0.2); border-left: 4px solid #ff6b6b; padding: 10px; margin: 10px 0; }
        .info { background: rgba(0,212,255,0.1); border-left: 4px solid #00d4ff; padding: 10px; margin: 10px 0; }
        code { background: #1a1a2e; padding: 4px 8px; border-radius: 4px; font-family: monospace; }
    </style>
</head>
<body>
<div class="container">
    <h1>🔒 SecureBank - Security Analytics Dashboard</h1>
    <button class="refresh-btn" onclick="loadData()">🔄 Rafraîchir</button>
    
    <div class="grid" id="statsCards">
        <div class="card"><h3>Chargement...</h3><div class="value">-</div></div>
    </div>
    
    <div style="display: flex; gap: 20px; flex-wrap: wrap;">
        <div style="flex: 1; min-width: 300px;">
            <h2>🚨 Utilisateurs suspects</h2>
            <table id="suspiciousTable">
                <thead><tr><th>Utilisateur</th><th>echecs</th><th>Succès</th><th>Statut</th></tr></thead>
                <tbody><tr><td colspan="4">Chargement...</td></tr></tbody>
            </table>
        </div>
        <div style="flex: 1; min-width: 300px;">
            <h2>🔒 Comptes bloques</h2>
            <table id="lockedTable">
                <thead><tr><th>Utilisateur</th><th>Raison</th></tr></thead>
                <tbody><tr><td colspan="2">Chargement...</td></tr></tbody>
            </table>
        </div>
    </div>
    
    <div class="info">
        <strong>ℹ️ Comment utiliser :</strong><br>
        1. Lancez la simulation : <code>sbt "runMain securebank.SecureBankApp"</code><br>
        2. Les evenements sont ecrits dans <code>data/security_events/</code><br>
        3. Rafraîchissement automatique toutes les 5 secondes
    </div>
    
    <div class="footer">
        SecureBank Analytics - Propulse par Apache Spark
    </div>
</div>

<script>
async function loadData() {
    try {
        const response = await fetch('/api/stats');
        const data = await response.json();
        
        if (data.error) {
            document.getElementById('statsCards').innerHTML = `
                <div class="card"><h3>⚠️ Aucune donnee</h3>
                <div class="value">0</div>
                <div style="font-size:12px;">${data.error}</div>
                </div>
            `;
            return;
        }
        
        document.getElementById('statsCards').innerHTML = `
            <div class="card"><h3>📊 Total evenements</h3><div class="value">${data.totalEvents || 0}</div></div>
            <div class="card"><h3>❌ echecs Auth</h3><div class="value bad">${data.authFailures || 0}</div></div>
            <div class="card"><h3>✅ Succès Auth</h3><div class="value good">${data.authSuccess || 0}</div></div>
            <div class="card"><h3>🔒 Comptes bloques</h3><div class="value bad">${data.lockedAccounts || 0}</div></div>
            <div class="card"><h3>🔑 Tokens revoques</h3><div class="value">${data.revokedTokens || 0}</div></div>
            <div class="card"><h3>⚔️ Brute-force</h3><div class="value bad">${data.bruteForceUsers?.length || 0}</div></div>
        `;
        
        if (data.suspiciousUsers && data.suspiciousUsers.length > 0) {
            document.querySelector('#suspiciousTable tbody').innerHTML = data.suspiciousUsers.map(u => `
                <tr><td><strong>${u.user}</strong></td><td class="bad">${u.failures}</td><td class="good">${u.success}</td><td class="bad">⚠️ Suspect</td></tr>
            `).join('');
        } else {
            document.querySelector('#suspiciousTable tbody').innerHTML = '<tr><td colspan="4">✅ Aucun utilisateur suspect</td></tr>';
        }
        
        if (data.bruteForceUsers && data.bruteForceUsers.length > 0) {
            document.querySelector('#lockedTable tbody').innerHTML = data.bruteForceUsers.map(u => `
                <tr><td><strong>${u}</strong></td><td class="bad">3+ echecs consecutifs</td></tr>
            `).join('');
        } else {
            document.querySelector('#lockedTable tbody').innerHTML = '<tr><td colspan="2">✅ Aucun compte bloque</td></tr>';
        }
        
    } catch (err) {
        console.error('Erreur:', err);
    }
}

loadData();
setInterval(loadData, 5000);
</script>
</body>
</html>
  """.stripMargin
}