var fs = require("fs");
var path = require("path");
var http = require("http");
var querystring = require('querystring');

var port = 8080;
var password = "abc123";


var requestHandler = function(request, response) {
console.log(request.url);

	var post_data = [], post_data_size = 0;
	request.on("data", function(data) {
		post_data.push(data);
		post_data_size += data.length;
		if(post_data_size > 1e6) {
			post_data = [];
			response.writeHead(413, {'Content-Type': 'text/plain'}).end();
			request.connection.destroy();
		}
	});

	request.on("end", function() { try {
		var post = querystring.parse(post_data.join(""));
		var get = querystring.parse(request.url.replace(/[^\?]+/, ""));
// console.log(post_data, post, get);
		
		if (request.url.match(/\?action=auth/)) {
			if(post["password"] == password) {
				response.writeHead(200, "OK", {
					"Content-Type": "text/html;charset=utf-8",
					"Set-Cookie": "littlelisp-password="+password+";"
				});
				response.end("ok");
				return;
			}
			else {
				response.writeHead(200, "OK", {
					"Content-Type": "text/html;charset=utf-8"
				});
				response.end("Error! Password invalid!");
				return;
			}
		}
	
		if (request.url.match(/\?action=list/)) {
			response.writeHead(200, "OK", {
				"Content-Type": "text/html;charset=utf-8"
			});
			
			var files = fs.readdirSync(".");
			for (var file of files) 
				if (file.match(/\.lisp$/)) {
					response.write(file + "\n");
				}
				else if (fs.statSync(file).isDirectory()) {
					var files2 = fs.readdirSync(file);
					for (var file2 of files2)
						if (file2.match(/\.lisp$/))
							response.write(file + "/" + file2 + "\n");
				}
				
			response.end();
		}
	
		else if (request.url.match(/\?action=get/) && (get.file||"").match(/^[a-zA-Z.\/0-9-]+$/)) {
			response.writeHead(200, "OK", {
				"Content-Type": "text/plain;charset=utf-8"
			});
			response.end(fs.readFileSync(get.file.replace(/\/\.+\//g, "/")));
		}
		
		else if (request.url.match(/\?action=save/)) {
			if ((get.file||"").match(/^[a-zA-Z.\/0-9-]+$/)) {
				
				// backup
				fs.renameSync(get.file.replace(/\/\.+\//g, "/"), get.file.replace(/\/\.+\//g, "/")+"~");
					
				// write files
				fs.writeFileSync(get.file.replace(/\/\.+\//g, "/"), post.lisp);
				fs.writeFileSync(get.file.replace(/\/\.+\//g, "/")+".js", post.lisp_js);
		
				response.writeHead(200, "OK", {
					"Content-Type": "text/html;charset=utf-8"
				});
				response.end("ok");
				return;
			}
			else {
				response.writeHead(200, "OK", {
					"Content-Type": "text/html;charset=utf-8"
				});
				response.end("Error! File name invalid!");
				return;
			}
		}
		
		else if (request.url == "/") {
			response.writeHead(200, "OK", {
				"Content-Type": "text/html;charset=utf-8",
				"Set-Cookie": "littlelisp-save-url=%2Flittlelisp.ide-backend.js;"
			});
			response.end(fs.readFileSync("littlelisp.ide.html"));
		}
		else if (["/littlelisp.js", "/littlelisp.ide.js"].indexOf(request.url) > -1) {
			response.writeHead(200, "OK", {
				"Content-Type": "text/javascript;charset=utf-8"
			});
			response.end(fs.readFileSync(request.url.replace(/^\//, "")));
		}
		
		else {
			response.writeHead(404, "Not Found");
			response.end();
		}
		
		} catch(ex) {
			console.error(ex.stack||ex);
			response.writeHead(505, "Server Error");
			response.end(ex.stack||ex);
		}
	});
}

var server = http.createServer(requestHandler);
server.listen(port, function(err) {
    if (err) {
        return console.log('something bad happened', err);
    }
    console.log("server is listening on "+port);
})