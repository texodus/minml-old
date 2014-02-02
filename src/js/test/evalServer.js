var http = require('http');

http.createServer(function (req, res) {
    var body = "";
    req.on('data', function (chunk) {
        body += chunk;
    });
    req.on('end', function () {
       res.writeHead(200, {'Content-Type': 'text/plain'});
       console.log("BODY: " + body);
       var result;
       try {
           result = eval(body);
       } catch (e) {
           result = "Node.js error: " + e;
       }
       console.log("RESULT: " + result);
       res.end(result + "");
    }); 
}).on('listening', function() {
  console.log('Server running at http://127.0.0.1:1337/');
}).listen(1337, '127.0.0.1');

