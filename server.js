const http = require('http');
const elm = require('./main');

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


server.on('clientError', (err, socket) => {
  socket.end('HTTP/1.1 400 Bad Request\r\n\r\n');
});


var port = 8080;
server.listen(port);
console.log('\nServer listening on port ' + port);
