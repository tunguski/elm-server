const http = require('http');
const static = require('node-static');
const watch = require('watch')


global.XMLHttpRequest = require("xhr2");


var app;


// TODO: switch to redis
var requestCache = {};


function startElmServer() {
  delete require.cache[require.resolve('./main')]
  var elm = require('./main');
  app = elm.Example.worker();
  
  
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
}


startElmServer();


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


function elmBuilder (paths, cmd, postBuild) {
  var builder = {
    changed : false
  };

  
  var exec;


  function maybeBuild () {
    if (builder.changed && !exec) {
      exec = require('child_process').exec;
      builder.changed = false;
      
      console.log('Starting build ' + paths);
      exec(cmd, function(error, stdout, stderr) {
        exec = undefined;

        console.log(stdout);
        console.log(stderr);
        if (postBuild) {
          postBuild();
        }
        if (builder.changed) {
          maybeBuild();
        }
      });
    }
  }


  paths.forEach(function (path) {
    watch.watchTree(path, { ignoreDotFiles : true }, function (f, curr, prev) {
      if (typeof f == "object" && prev === null && curr === null) {
        // Finished walking the tree
      } else if (prev === null) {
        // f is a new file
        builder.changed = true;
      } else if (curr.nlink === 0) {
        // f was removed
        builder.changed = true;
      } else {
        // f was changed
        builder.changed = true;
      }
  
      maybeBuild();
    });
  });

  return builder;
}


const clientBuilder = elmBuilder(['client/'],
    'elm-make client/Main.elm     --output public/example.js');
const serverBuilder = elmBuilder(['src/', 'example/'],
    'elm-make example/Example.elm --output main.js',
    startElmServer);


http.createServer(function (request, response) {
  request.addListener('end', function () {
    file.serve(request, response);
  }).resume();
}).listen(8000);


server.on('clientError', (err, socket) => {
  socket.end('HTTP/1.1 400 Bad Request\r\n\r\n');
});


var port = 8080;
server.listen(port);
console.log('\nServer listening on port ' + port);
