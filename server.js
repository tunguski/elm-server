const http = require('http');
const elm = require('./main');
const static = require('node-static');

global.XMLHttpRequest = require("xhr2");


var app = elm.Example.worker();

// TODO: switch to redis
var requestCache = {};


app.ports.sendResponse.subscribe(function(response) {
  var reqRes = requestCache[response.idRequest];
  if (reqRes) {
    response.headers.forEach(function (header) {
      reqRes.res.setHeader(header[0], header[1]);
    });
    reqRes.res.setHeader('Access-Control-Allow-Origin', 'http://localhost:8000');

    if (response.statusCode) {
      reqRes.res.statusCode = response.statusCode;
    }
    if (response.body) {
      reqRes.res.write(response.body);
    }
    reqRes.res.end();
    requestCache[response.idRequest] = undefined;

    console.log('' + new Date().getTime() - reqRes.startTime + 'ms: ' + reqRes.req.url);
  }
});


const server = http.createServer((req, res) => {
  var id = '' + Math.random();
  requestCache[id] = { 
    req : req, 
    res : res, 
    startTime : new Date().getTime() 
  };

  var headers = [];
  req.headers

  for(var header in req.headers) {
    headers.push([ header, req.headers[header] ]);
  }

  app.ports.request.send({ 
    id : id, 
    url : req.url, 
    headers : headers,
    method : req.method 
  });
});


const file = new static.Server('./public');
 
http.createServer(function (request, response) {
    request.addListener('end', function () {
        // 
        // Serve files! 
        // 
        file.serve(request, response);
    }).resume();
}).listen(8000);


server.on('clientError', (err, socket) => {
  socket.end('HTTP/1.1 400 Bad Request\r\n\r\n');
});


var port = 8080;
server.listen(port);
console.log('\nServer listening on port ' + port);
