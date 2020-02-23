;(function(exports) {

var Context = function(scope, parent, input, index) {
	this.scope = scope;
	this.parent = parent;
	this.input = input;
	this.indx = index || -1;
	this.result = [];
	this.thiz = (parent || {}).thiz;

	this.get = function(identifier, set_ctx_thiz) {
		
		// obj.field ?
		if (identifier.indexOf(".") > -1) {
			var prefix = identifier.substring(0, identifier.indexOf("."));
			var obj = undefined;
			
			if (prefix in this.scope)
				obj = this.scope[prefix];
			else if (this.parent !== undefined)
				obj = this.parent.get(prefix);
			
			if(set_ctx_thiz)
				this.thiz = obj;
			
			if(obj)
				return obj[identifier.substring(identifier.indexOf(".")+1)];
			else
				return undefined;
		}
		
		if (identifier in this.scope)
			return this.scope[identifier];
		
		if (this.parent !== undefined)
			return this.parent.get(identifier);
	};
	
	this.set = function(identifier, value) {
		
		// obj.field = value ?
		if (identifier.indexOf(".") > -1) {
			var prefix = identifier.substring(0, identifier.indexOf("."));
			var obj = undefined;
			
			if (prefix in this.scope)
				obj = this.scope[prefix];
			else if (this.parent !== undefined)
				obj = this.parent.get(prefix);
			
			// если не существует объект, то создадим новый
			if (obj === undefined || obj === null)
				throw "Error! "+prefix+" is "+obj+"!";
			
			obj[identifier.substring(identifier.indexOf(".")+1)] = value;
			return value;
		}
		
		this.scope[identifier] = value;
		return value;
	}
};

var interpret = function(input, scope) {
	
	// lisp
	if(input instanceof Array) {
		var ctx = new Context(scope || window, undefined, input, -1);
// 		return interp_loop(ctx);
		
		while(ctx.indx < ctx.input.length) {
			ctx = interp_step(ctx);
		}
	
		return ctx.result;
	}
	
	// var
	else if (input.type === "identifier")
		return (scope || window)[ input[0].value ];
			
	// string/number
	else if (input.type === "number" || input.type === "string")
		return input.value;
	
	// ???
	else
		throw "Unrecognized - "+JSON.stringify(input);
}

var interp_step = function(ctx) {
	
	/* обработаем тек.элемент */
	if (ctx.indx > -1) {

		if (ctx.input[ctx.indx] instanceof Array) {
			ctx = new Context(ctx.scope, ctx, ctx.input[ctx.indx], -1);
		}
		
		// var
		else if (ctx.input[ctx.indx].type === "identifier") {
			ctx.result[ctx.indx] = ctx.get(ctx.input[ctx.indx].value, ctx.indx == 0);
		}
			
		// string/number
		else if (ctx.input[ctx.indx].type === "number" || ctx.input[ctx.indx].type === "string") {
			ctx.result[ctx.indx] = ctx.input[ctx.indx].value;
		}
			
		// ???
		else
			throw new Exception("Unrecognized - "+ctx.input[ctx.indx]);
	}
	
	/* решим куда сдвигать курсор */
	while(1)
	
	// (let x ...)
	if (ctx.input[0].value == "let") {
		
		// let
		if (ctx.indx == -1) {
			ctx.indx = 2;
			return ctx;
		}
		
		// let-value
		if (ctx.indx == 2) {
			ctx.scope[ctx.input[1].value] = ctx.result[2];
			ctx.result = ctx.result[2];
			
			// parent
			if (ctx.parent) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue;
			}
			
			// no-parent
			if (ctx.parent === undefined) {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
		debugger;
	}
		
	// (if ... (...) (...))
	else if (ctx.input[0].value == "if") {
		
		// if
		if (ctx.indx == -1) {
			ctx.indx = 1;
			return ctx;
		}
		
		// if-test
		if (ctx.indx == 1) {
			
			// -> true
			if (!!ctx.result[1]) { 
				ctx.indx = 2; 
				return ctx; 
			}
			
			// -> false
			if (ctx.input[3] !== undefined) { 
				ctx.indx = 3; 
				return ctx; 
			}
			
			ctx.result = undefined;
			
			// parent
			if (ctx.parent) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue;
			}
			
			// no-parent
			if (ctx.parent === undefined) {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
		// if-true,if-false
		if (ctx.indx == 2 || ctx.indx == 3) {
			ctx.result = ctx.result[ctx.indx];
			ctx.indx = ctx.input.length;
			
			// parent
			if (ctx.parent) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue;
			}
			
			// no-parent
			if (ctx.parent === undefined) {
				return ctx;
			}
		}
		
		debugger;
	}
	
	// (lambda (x y) (...))
	else if (ctx.input[0].value == "lambda") {
		var input = ctx.input;
		ctx.result = function() {
			var lambdaScope = {};
			var lambdaArgNames = input[1];
			for(var i = 0; i < lambdaArgNames.length; i++)
				lambdaScope[lambdaArgNames[i].value] = arguments[i] || undefined;
			
			return new Context(lambdaScope, undefined, input[2]);
		};
		
		ctx.indx = ctx.input.length;
		
		// parent
		if (ctx.parent) {
			ctx.parent.result[ctx.parent.indx] = ctx.result;
			ctx = ctx.parent;
			continue;
		}
		
		// no-parent
		if (ctx.parent === undefined) {
			ctx.indx = ctx.input.length;
			return ctx;
		}
		
		debugger;
	}
	
	// (<func> ...)
	else if (ctx.result[0] instanceof Function) {
		
		// вычеслим след. аргумент
		if (ctx.indx+1 < ctx.input.length) {
			ctx.indx++;
			return ctx;
		}
		
		// вызов завершён -> возвращаем результат наверх
		else if (ctx.indx == ctx.input.length) {
			ctx.result = ctx.result[ctx.indx];
			
			// parent
			if (ctx.parent) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue;
			}
			
			// no-parent
			else {
				return ctx;
			}
		}
		
		// аргументы готовы -> делаем вызов функции
		else {
			var call_result = ctx.result[0].apply(ctx.thiz, ctx.result.slice(1));
			
			ctx.indx = ctx.input.length; // отметим, что вызов был сделан уже
			
			// если вернули Context то это наша lambda, def...
			if(call_result instanceof Context) {
				call_result.parent = ctx; // управление должно будет вернуться к нам
				return call_result;
			}
			
			// иначе простая функция JS была вызвана
			else {
				ctx.result[ctx.indx] = call_result;
				continue;
			}
		}
	}
	
	// (....)
	else {
		
		// двигаемся в право ->
		ctx.indx++;
		if (ctx.indx < ctx.input.length)
			return ctx;
		
		// иначе наверх ^|
		else if (ctx.parent) {
			ctx.parent.result[ctx.parent.indx] = ctx.result;
			ctx = ctx.parent;
			continue;
		}
		
		// выше некуда - finish
		else {
			ctx.indx = ctx.input.length;
			return ctx;
		}
	}

}

// токены -> дерево списков
var analize = function(input, list) {
	if (list === undefined)
		return analize(input, []);
	
	// берём токен на распознание
	var token = input.shift();
	
	if (token === undefined)
		return list.pop();
	
	if (token === "(") {
		list.push(analize(input, []));
		return analize(input, list);
	} 
	
	if (token === ")")
		return list;
	
	if (!isNaN(parseFloat(token)))
		list.push({ type:'number', value: parseFloat(token) });
	
	else if (token[0] === '"' && token.slice(-1) === '"')
		list.push({ type:'string', value: token.slice(1, -1) });
	
	else 
		list.push({ type:'identifier', value: token });
	
	return analize(input, list);
};

// строка -> токены
var tokenize = function(input) {
	return input.split('"')
			.map(function(x, i) {
				if (i % 2 === 0) { // not in string
					return x.replace(/\(/g, ' ( ')
							.replace(/\)/g, ' ) ');
				} else { // in string
					return x.replace(/ /g, "!whitespace!");
				}
			})
			.join('"')
			.trim()
			.split(/\s+/)
			.map(function(x) {
				return x.replace(/!whitespace!/g, " ");
			});
};

// tokenize+analize
var parse = function(input) {
	return analize(tokenize(input));
};

exports.littleLisp = {
	parse: parse,
	interpret: interpret
};
})(typeof exports === 'undefined' ? this : exports);
