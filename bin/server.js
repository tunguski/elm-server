const http = require('http');
const httpProxy = require('http-proxy');
const static = require('node-static');
const watch = require('watch')
const url = require('url');


global.XMLHttpRequest = require("xhr2");


var app;


// TODO: switch to redis
const requestCache = {};

const proxy = httpProxy.createProxyServer();

function startElmServer() {
  delete require.cache[require.resolve('./target/server')]
  var elm = require('./target/main');
  app = elm.Example.worker();


  app.ports.sendResponsePort.subscribe(function(response) {
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

        function isJsonString(str) {
          try {
            JSON.parse(str);
          } catch (e) {
            return false;
          }
          return true;
        }

        if (isJsonString(response.body)
            && !response.headers["Content-Type"]) {
          reqRes.res.setHeader("Content-Type", "application/json");
        }

        reqRes.res.write(response.body);
      }
      reqRes.res.end();
      requestCache[response.idRequest] = undefined;

      console.log('' + new Date().getTime() - reqRes.startTime + 'ms: ' + reqRes.req.url);
    } else {
      console.log("[SEVERE ERROR] Could not find request id!");
      console.log(requestCache);
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

  function objectToMap (obj) {
    var map = [];

    for(var key in obj) {
      map.push([ key, obj[key] ]);
    }

    return map;
  }

  var body = [];
  req.on('data', function(chunk) {
    body.push(chunk);
  }).on('end', function() {

    if (body.length === 0) {
      body = "";
    } else {
      body = Buffer.concat(body).toString();
    }

    // at this point, `body` has the entire request body stored in it as a string

    app.ports.request.send({
      id : id,
      time : new Date().getTime(),
      url : req.url.substr(0, req.url.indexOf("?")) || req.url,
      headers : objectToMap(req.headers),
      queryParams : objectToMap(url.parse(req.url, true).query),
      method : req.method,
      body : body
    });
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
    watch.watchTree(path, { ignoreDotFiles : true, interval: 1 }, function (f, curr, prev) {
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


const clientBuilder = elmBuilder(['src/'],
    'elm-make srv/Client.elm --output public/client.js');
const serverBuilder = elmBuilder(['src/'],
    'elm-make srv/Server.elm --output target/server.js',
    startElmServer);


const httpPort = 8000;
http.createServer(function (request, response) {
  if (request.url.startsWith("/api")) {
    proxy.web(request, response, { target: 'http://localhost:8080' });
  } else {
    request.addListener('end', function () {
      file.serve(request, response);
    }).resume();
  }
}).listen(httpPort);
console.log('\nHttp server listening on port ' + httpPort);


server.on('clientError', (err, socket) => {
  socket.end('HTTP/1.1 400 Bad Request\r\n\r\n');
});


const port = 8080;
server.listen(port);
console.log('\nBackend server listening on port ' + port);

