;(function(exports) {
var library = {
	first: function(x) {
		return x[0];
	},

	rest: function(x) {
		return x.slice(1);
	},

	print: function(x) {
		console.log(x);
		return x;
	}
};

var Context = function(scope, parent) {
	this.scope = scope;
	this.parent = parent;

	this.get = function(identifier) {
		if (identifier in this.scope)
			return this.scope[identifier];
		
		if (this.parent !== undefined)
			return this.parent.get(identifier);
	};
};

var special = {
	let: function(input, context) {
		context.scope[input[1]] = interpret(input[2], context);
		return context.scope[input[1]];
	},

	lambda: function(input, context) {
		return function() {
			var lambdaScope = {};
			var lambdaArgNames = input[1];
			for(var i = 0; i < lambdaArgNames.length; i++)
				lambdaScope[lambdaArgNames[i].value] = arguments[i] || undefined;

			return interpret(input[2], new Context(lambdaScope, context));
		};
	},

	if: function(input, context) {
		return interpret(input[1], context) ?
			interpret(input[2], context) :
			interpret(input[3], context);
	}
};

var interpretList = function(input, context) {
	
	// оператор языка? - передадим в обработчик управление
	if (input.length > 0 && input[0].value in special)
		return special[input[0].value](input, context);
	
	// отработаем все элементы списка
	var list = input.map(function(x) { 
		return interpret(x, context); 
	});
	
	// функция? - вернём результат функции
	if (list[0] instanceof Function)
		return list[0].apply(undefined, list.slice(1));

	// иначе просто список возвращяем
	return list;
};

var interpret = function(input, context) {
	// первый запуск
	if (context === undefined)
		return interpret(input, new Context(library));
	
	if (input instanceof Array)
		return interpretList(input, context);
	
	if (input.type === "identifier")
		return context.get(input.value);
	
	if (input.type === "number" || input.type === "string")
		return input.value;
};

// токены -> дерево списков
var parse_lists = function(input, list) {
	if (list === undefined)
		return parse_lists(input, []);
	
	// берём токен на распознание
	var token = input.shift();
	
	if (token === undefined)
		return list.pop();
	
	if (token === "(") {
		list.push(parse_lists(input, []));
		return parse_lists(input, list);
	} 
	
	if (token === ")")
		return list;
	
	if (!isNaN(parseFloat(token)))
		list.push({ type:'number', value: parseFloat(token) });
	
	else if (token[0] === '"' && token.slice(-1) === '"')
		list.push({ type:'string', value: token.slice(1, -1) });
	
	else 
		list.push({ type:'identifier', value: token });
	
	return parse_lists(input, list);
};

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

var parse = function(input) {
	return parse_lists(tokenize(input));
};

exports.littleLisp = {
	parse: parse,
	interpret: interpret
};
})(typeof exports === 'undefined' ? this : exports);
