;(function(exports) {

var atom_alphabet = 
"qwfpgjluyarstdhneiozxcvbkmQWFPGJLUYARSTDHNEIOZXCVBKM" + // Latin
"-+*/%<>=!|&" + // arithmetic + logic
":#'`" + // symbol chars + quote + backquote
"яжфпгйлуыюшщарстдхнеиоэзчцвбкмъьЯЖФПГЙЛУЫЮШЩАРСТДХНЕИОЭЗЧЦВБКМЬЪёЁ" + // Cyrillic
"_.,?@"; // additional atom chars

var gensym_counter = 1; // for macros

var Context = function(scope, parent, input, index, source, file, ctx_type) {
	this.type = ctx_type || 0; // undef/0 - list, 2 - catch, 4 - while,for,switch, 16 - function, 32 - nested debugging, 64 - macros
	this.parent = parent;
	this.scope = scope;
	this.input = input;
	this.indx = index || 0;
	this.result = [];
	this.this_for_elem1 = undefined;
	this.source = source || (parent ? parent.source : "no source!!!");
	this.file = file || (parent ? parent.file : "(no file)");
}

Context.prototype.range = function(type, point) {
		
	// start_offset
	if (type == 1) {
		
		// this.input is scalar (123, "abc"...)
		if (this.input instanceof Array == false) 
			return 0;
		
		// (...)
		if (this.indx == this.input.length-1) {
			return (this.input[this.input.length-1] >> 16);
		}
		
		// list
		if (this.input[this.indx] instanceof Array)
			return this.input[this.indx][this.input[this.indx].length-1] >> 16;
		
		// atom/string/number...
		if (this.input[this.indx])
			return this.input[Math.min(this.indx, this.input.length-1)] >> 16;
		
		// выполнение кода завершено!
		if (this.indx == this.input.length)
			return 0;
	}
	
	// end_offset
	if (type == 2) {
		
		// this.input is scalar (123, "abc"...)
		if (this.input instanceof Array == false) 
			return this.source.length;
		
		// (...)
		if (this.indx == this.input.length-1) {
			return (this.input[this.input.length-1] >> 16) + (this.input[Math.min(this.indx, this.input.length-1)] & 32767);
		}
		
		// list
		if (this.input[this.indx] instanceof Array)
			return (this.input[this.indx][this.input[this.indx].length-1] >> 16) 
				+ (this.input[this.indx][this.input[this.indx].length-1] & 32767);
		
		// atom/string/number
		if (this.input[this.indx])
			return (this.input[Math.min(this.indx, this.input.length-1)] >> 16) 
				+ (this.input[Math.min(this.indx, this.input.length-1)] & 32767);
				
		// выполнение кода завершено!
		if (this.indx == this.input.length)
			return this.source.length;

	}
	
	debugger;
}

Context.prototype.set = function(identifier, value, is_unset_mode) {
		
	// obj.field1... ?
	var prefix_len = identifier.indexOf(".");
	
	// identifier only
	if (prefix_len < 0) {
		identifier = identifier == "this" ? "thiz" : identifier;
		if (is_unset_mode)
			delete this.scope[identifier];
		else 
			this.scope[identifier] = value;
		
		return value;
	}
	
	// identifier.field_name1.field_name2...
	var prefix = identifier.substring(0, prefix_len);
	
	// base object
	for (var curr_ctx = this; curr_ctx; curr_ctx = curr_ctx.parent) {
		if (prefix in curr_ctx.scope) {
			var obj = curr_ctx.scope[prefix];
			break;
		}
		if (prefix == "this" && "thiz" in curr_ctx.scope) {
			var obj = curr_ctx.scope.thiz;
			break;
		}
		if ( ! curr_ctx.parent ) {
			var obj = window[prefix]; // на худший случай
			break;
		}
	}
	
	// если не существует объект сообщим
	if (obj === undefined || obj === null)
		throw "Error! "+prefix+" is "+(obj == null ? "null" : typeof obj)+"!";
	
	// field chain...
	while (obj && identifier.indexOf(".", prefix_len+1) > -1) {
		prefix = identifier.substring(prefix_len+1, identifier.indexOf(".", prefix_len+1));
		prefix_len += prefix.length + 1;
		
		// .@fieldname
		if (prefix[0] == '@')
		for (var cnt = prefix[0] == '@' ? prefix[1] == '@' ? 2 : 1 : 0, 
			prefix = prefix.substring(cnt); 
			cnt > 0; cnt--) {
			
			var found = false;
			for (var curr_ctx = this; curr_ctx; curr_ctx = curr_ctx.parent) {
				if (prefix in curr_ctx.scope) {
					prefix = curr_ctx.scope[prefix];
					found = true;
					break;
				}
			}
			if ( ! found )
				throw "Not found value for \"@"+prefix+"\"!";
		}
		
		if (prefix in obj)
			obj = obj[prefix];
		else if (identifier.indexOf(".", prefix_len) < 0)
			throw "Error! No " + identifier.substring(0, prefix_len) + " found!";
	}
	
	// end field_name
	prefix = identifier.substring(prefix_len+1);
	
	// .@fieldname
	if (prefix[0] == '@')
	for (var cnt = prefix[0] == '@' ? prefix[1] == '@' ? 2 : 1 : 0, 
		prefix = prefix.substring(cnt); 
		cnt > 0; cnt--) {
		
		var found = false;
		for (var curr_ctx = this; curr_ctx; curr_ctx = curr_ctx.parent) {
			if (prefix in curr_ctx.scope) {
				prefix = curr_ctx.scope[prefix];
				found = true;
				break;
			}
		}
		if ( ! found )
			throw "Not found value for \"@"+prefix+"\"!";
	}
	
	// просто попросили удалить
	if (is_unset_mode)
		delete obj[prefix];
	else
		obj[prefix] = value;
		
	return value;
};

Context.prototype.interpret_elem = function() {
	
	// list
	if (this.input[this.indx] instanceof Array) {
		
		// в пустой список не надо заходить
		if (this.input[this.indx].length == 1) {
			this.result[this.indx] = [];
			return true;
		}
		
		// заходим внутрь списка и вычисляем его (если ещё нет результата)
		else if (this.indx in this.result == false) {
			this.result[this.indx] = new Context(this.scope, this, this.input[this.indx], undefined, this.source, this.file);
			return false;
		}
		
		// ???
		debugger;
		throw new Error("???");
	} 
	
	var char_code = this.source.charCodeAt(this.input[this.indx] >> 16);
	
	// atom (quoted atom)
	if (char_code == 39 /* ' */ || char_code == 96 /* ` */ || char_code == 58 /* : */ || char_code == 35 /* # */ || char_code == 38 /* & */) {
		this.result[this.indx] = new String(this.source.substring((this.input[this.indx] >> 16) +1, (this.input[this.indx] >> 16) + (this.input[this.indx] & 32767)));
		this.result[this.indx].atom_prefix = String.fromCharCode(char_code);
		return true;
	}
	
	// string
	else if (char_code == 34 /* " */) {
		var text = this.source.substring((this.input[this.indx] >> 16), (this.input[this.indx] >> 16) + (this.input[this.indx] & 32767));
		try { this.result[this.indx] = JSON.parse(text); } 
		catch(ex) { this.result[this.indx] = JSON.parse(text.replace(/\n/g, "\\n")); }
		return true;
	}
	
	// number
	else if (char_code > 47 && char_code < 58 /* 0-9 */ 
	|| (char_code == 45 && (this.input[this.indx] & 32767) && "0123456789".indexOf(this.source[(this.input[this.indx] >> 16)+1]) > -1)) {
		var val = this.source.substring(this.input[this.indx] >> 16, (this.input[this.indx] >> 16) + (this.input[this.indx] & 32767));
		this.result[this.indx] = (this.input[this.indx] & 32768) 
			? parseFloat(val) 
			: parseInt(val);
		return true;
	}
	
	// embedded JSON
	else if (char_code == 123 /* { */ || char_code == 91 /* [ */) {
		var text = this.source.substring((this.input[this.indx] >> 16), (this.input[this.indx] >> 16) + (this.input[this.indx] & 32767));
		this.result[this.indx] = eval('('+text+')');
		return true;
	}
	
	// var
	else if (atom_alphabet.indexOf(this.source[this.input[this.indx] >> 16]) > -1) {
		// иногда по привычке поподаются @ перед идентификатором
		if (this.source[this.input[this.indx] >> 16] == '@')
			var identifier = this.source.substring(
				(this.input[this.indx] >> 16) + 1, 
				(this.input[this.indx] >> 16) + (this.input[this.indx] & 32767));
		// случаи когда ,var или ,@var
		else if (this.source[this.input[this.indx] >> 16] == ',')
			var identifier = this.source.substring(
				(this.input[this.indx] >> 16) + (this.source[(this.input[this.indx] >> 16)+1] == '@' ? 2 : 1), 
				(this.input[this.indx] >> 16) + (this.input[this.indx] & 32767));
		else
			var identifier = this.source.substring(
				this.input[this.indx] >> 16, 
				(this.input[this.indx] >> 16) + (this.input[this.indx] & 32767));
	
		if (this.input[this.indx] & 32768)
			/* not special constant */;
			
		// special constants
		else if (char_code == 116 && identifier == "this")
			identifier = "thiz";
		// TODO Т, TRUE, True, False, NIL...
		else if (char_code == 116 && identifier == "true") 
			return this.result[this.indx] = true;
		else if (char_code == 102 && identifier == "false") 
			return this.result[this.indx] = false, true;
		else if (char_code == 78 && identifier == "NIL") 
			return this.result[this.indx] = null, true;
		else if (char_code == 110 && identifier == "null") 
			return this.result[this.indx] = null, true;
		else if (char_code == 117 && identifier == "undefined") 
			return this.result[this.indx] = undefined, true;
		
		// var_name
		if ((this.input[this.indx] & 32768) == 0) {
			
			// var1 - ищем в цепочке scope выше
			for (var parent_ctx = this;; parent_ctx = parent_ctx.parent) {
				
				// исчерпали ctx, тогда используем window/global
				if ( ! parent_ctx ) {
					// (кастыль) на случай если первый элемент функция
					if (this.indx == 0)
						this.this_for_elem1 = window;
				
					return this.result[this.indx] = window[identifier], true;
				}
			
				if (identifier in parent_ctx.scope) {
				
					// (кастыль) на случай если первый элемент функция
					if (this.indx == 0)
						this.this_for_elem1 = parent_ctx.scope;
					
					return this.result[this.indx] = parent_ctx.scope[identifier], true;
				}
			}
		}
		
		// obj.fieldname1.fieldname2...
		var prefix_len = identifier.indexOf(".");
		var prefix = identifier.substring(0, prefix_len);
		
		// base object
		if (prefix_len == 0) {
			var obj = this.result[this.indx-1];
		}
		else {
			
			// ищем по цепочке наверх
			for (var curr_ctx = this; curr_ctx; curr_ctx = curr_ctx.parent) {
				if (prefix in curr_ctx.scope) {
					var obj = curr_ctx.scope[prefix];
					break;
				}
				if (prefix == "this" && "thiz" in curr_ctx.scope) {
					var obj = curr_ctx.scope.thiz;
					break;
				}
				if ( ! curr_ctx.parent ) {
					var obj = window[prefix]; // на худший случай
					break;
				}
			}
			
			// если не существует объект сообщим
			if (obj === undefined || obj === null)
				throw new Error("\"" + prefix+"\" is "+(typeof obj)+"!");
		}
		
		// field chain?
		while (obj && identifier.indexOf(".", prefix_len+1) > -1) {
			prefix = identifier.substring(prefix_len+1, identifier.indexOf(".", prefix_len+1));
			prefix_len += prefix.length + 1;
			
			// .@fieldname
			if (prefix[0] == '@') {
				var identifier2 = prefix.substring(1);
				for (var curr_ctx = this; curr_ctx; curr_ctx = curr_ctx.parent) {
					if (identifier2 in curr_ctx.scope) {
						prefix = curr_ctx.scope[identifier2];
						break;
					}
				}
				if (prefix[0] == '@')
					throw new Error("Not found value for \"@"+prefix+"\"!");
			}
			
			if (prefix in obj)
				obj = obj[prefix];
			else 
				obj = undefined;
		}
		
		// (кастыль) на случай если первый элемент функция
		if (this.indx == 0)
			this.this_for_elem1 = obj;
		
		
		identifier = identifier.substring(prefix_len+1);
		
		// .@fieldname
		if (identifier[0] == '@') {
			var identifier2 = identifier.substring(1);
			for (var curr_ctx = this; curr_ctx; curr_ctx = curr_ctx.parent) {
				if (identifier2 in curr_ctx.scope) {
					identifier = curr_ctx.scope[identifier2];
					break;
				}
			}
			if (identifier == undefined)
				throw new Error("Value of \"@"+identifier2+"\" is undefined!");
			if (identifier[0] == '@')
				throw new Error("Not found value for \""+identifier+"\"!");
		}
		
		if(obj)
			return this.result[this.indx] = obj[identifier], true;
		else
			return this.result[this.indx] = undefined, true;
		
		debugger;
		throw new Error("???");
	}
				
	// ???
	else
		throw new Error("Unrecognized: " + 
			this.source.substring(this.input[this.indx] >> 16, (this.input[this.indx] >> 16) + (this.input[this.indx] & 32767)) + 
			"\nSource fragment: " + 
			this.source.substring(this.input[this.indx] >> 16, (this.input[this.indx] >> 16) + 30)+"...");
}

Context.prototype.interpret_list = function(debugging, step_over_ctx) {
	
	var ctx = this;
		
	/* решим кто следующий */
	for(var prt_cnt = 0; prt_cnt < 1000; prt_cnt++) try {
	
	if (step_over_ctx && prt_cnt > 0 && ctx == step_over_ctx && ctx.indx in ctx.result == false) 
		return ctx;
		
	var off = ctx.input[0] instanceof Array == false ? ctx.input[0] >>> 16 : 0;
	var len = ctx.input[0] instanceof Array == false ? (ctx.input[0] & 32767) : 0;
	
	// (&&,|| ...)
	if (len == 2 && "|&".indexOf(ctx.source[off]) > -1) {
		
		// operator (skip)
		if (ctx.indx == 0) {
			ctx.indx = 1;
			return ctx;
		}
		
		if (ctx.input.length < 4)
			throw "Need minimum 2 arguments!";
		
		// вычеслим первый аргумент
		if (ctx.indx == 1) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx]; /* return Context */
  
			// преобразуем в boolean принудительно
			ctx.result[ctx.indx] = !!ctx.result[ctx.indx];
			
			ctx.indx++;
			return ctx;
		}
		
		// пока есть аргументы и предыдущий результат True(&&) или False(||)
		while (ctx.indx < ctx.input.length-1 && ctx.result[ctx.indx-1] == (ctx.source[off] == '&')) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx]; /* return Context */

			// преобразуем в boolean принудительно
			ctx.result[ctx.indx] = !!ctx.result[ctx.indx];
			
			ctx.indx++;
			return ctx;
		}
		
		ctx.result = ctx.result[ctx.indx-1];
		
		// return to parent or no-parent (or debugging ctx)
		if (ctx.parent && !(ctx.type & 32)) {
			ctx.parent.result[ctx.parent.indx] = ctx.result;
			ctx = ctx.parent;
			continue;
		}
		else {
			ctx.indx = ctx.input.length;
			return ctx;
		}
		
	}
	
	// (+- ...)
	if (len > 0 && "+-*/%<>!=".indexOf(ctx.source[off]) > -1) {
		
		// operator (skip)
		if (ctx.indx == 0) {
			ctx.indx = 1;
			return ctx;
		}
		
		if (ctx.input.length < 3)
			throw "Need minimum 1 argument!";
		
		// вычисляем аргумент и производим действие
		if (ctx.indx < ctx.input.length-1) {
			
			// вычесляем аргумент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx]; /* return Context */

			// ++, --
			if ((len == 2 && ctx.source[off] == '+' && ctx.source[off+1] == '+' || (len == 2 && ctx.source[off] == '-' && ctx.source[off+1] == '-'))) {
				ctx.result[ctx.indx] = ctx.result[ctx.indx]*1 + (ctx.source[off] == '+' ? 1 : -1);
				ctx.set(
					ctx.source.substring(ctx.input[ctx.indx] >> 16,
						(ctx.input[ctx.indx] >> 16) + (ctx.input[ctx.indx] & 32767)),
					ctx.result[ctx.indx]
				);
				
				ctx.indx++;
				return ctx;
			}
			
			// !, !!
			else if (len == 1 && ctx.source[off] == "!") {
				ctx.result[ctx.indx] = ctx.indx == 1 
					? !ctx.result[ctx.indx] 
					: (!ctx.result[ctx.indx] && ctx.result[ctx.indx-1]);
					
				ctx.indx++;
				return ctx;
			}
			else if (len == 2 && ctx.source[off] == "!" && ctx.source[off+1] == "!") {
				ctx.result[ctx.indx] = ctx.indx == 1 
					? !!ctx.result[ctx.indx] 
					: (!!ctx.result[ctx.indx] && ctx.result[ctx.indx-1]);
				
				ctx.indx++;
				return ctx;
			}
			
			// для последующих операторов нужно более 1ого аргумента
			else if (ctx.indx == 1) {
				ctx.indx++;
				return ctx;
			}
			
			// <
			else if (len == 1 && ctx.source[off] == "<")
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] < ctx.result[ctx.indx];
			
			// <=
			else if (len == 2 && ctx.source[off] == "<" && ctx.source[off+1] == "=")
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] <= ctx.result[ctx.indx];
			
			// >
			else if (len == 1 && ctx.source[off] == ">")
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] > ctx.result[ctx.indx];
			
			// >=
			else if (len == 2 && ctx.source[off] == ">" && ctx.source[off+1] == "=")
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] >= ctx.result[ctx.indx];
			
			// =,==
			else if (ctx.source[off] == "=" && (len == 1 || len == 2 && ctx.source[off+1] == "="))
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] == ctx.result[ctx.indx];
			
			// ===
			else if (len == 3 && ctx.source[off] == "=" && ctx.source[off+1] == "=" && ctx.source[off+2] == "=")
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] === ctx.result[ctx.indx];
			
			// !=
			else if (len == 2 && ctx.source[off] == "!" && ctx.source[off+1] == "=")
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] != ctx.result[ctx.indx];
			
			// !==
			else if (len == 3 && ctx.source[off] == "!" && ctx.source[off+1] == "=" && ctx.source[off+2] == "=")
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] !== ctx.result[ctx.indx];
			
			// +
			else if (len == 1 && ctx.source[off] == "+")
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] + ctx.result[ctx.indx];
				
			// -
			else if (len == 1 && ctx.source[off] == "-")
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] - ctx.result[ctx.indx];
			
			// *
			else if (len == 1 && ctx.source[off] == "*")
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] * ctx.result[ctx.indx];
			
			// / (divide)
			else if (len == 1 && ctx.source[off] == "/")
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] / ctx.result[ctx.indx];
			
			// %
			else if (len == 1 && ctx.source[off] == "%")
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] % ctx.result[ctx.indx];
			
			// ^
			else if (len == 2 && ctx.source[off] == "^")
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] ^ ctx.result[ctx.indx];
			
			// &
			else if (len == 1 && ctx.source[off] == '&')
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] & ctx.result[ctx.indx];
			
			// |
			else if (len == 1 && ctx.source[off] == '|')
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] | ctx.result[ctx.indx];
			
			// >>
			else if (len == 2 && ctx.source[off] == '>' && ctx.source[off+1] == '>')
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] >> ctx.result[ctx.indx];
			
			// >>
			else if (len == 3 && ctx.source[off] == '>' && ctx.source[off+1] == '>' && ctx.source[off+2] == '>')
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] >>> ctx.result[ctx.indx];
			
			// <<
			else if (len == 2 && ctx.source[off] == '<' && ctx.source[off+1] == '<')
				ctx.result[ctx.indx] = ctx.result[ctx.indx-1] << ctx.result[ctx.indx];

			else
				throw new Error("Unknown operator: "+ctx.source.substring(off, off+len));
			
			// след.аргумент
			ctx.indx++;
			return ctx;
		}
		
		// return to parent or no-parent (or debugging ctx)
		if (ctx.parent && !(ctx.type & 32)) {
			ctx.parent.result[ctx.parent.indx] = ctx.result[ctx.indx-1];
			ctx = ctx.parent;
			continue;
		}
		else {
			ctx.result = ctx.result[ctx.indx-1];
			ctx.indx = ctx.input.length;
			return ctx;
		}
	}
	
	// (quote ...) '(....) `(....)
	if ((len == 2 && (ctx.source[off] == "'" || ctx.source[off] == "`") 
		&& ctx.source[off+1] == "(")
		|| (len == 5 && ctx.source[off] == "q" && ctx.source[off+1] == "u" 
		&& ctx.source[off+2] == "o" && ctx.source[off+3] == "t" 
		&& ctx.source[off+4] == "e")) {
		
		var quote_list = function(input, start_i, level) {
			// infinity recursion protection
			if (level > 20) {
				throw new Error("Max deep level for quoting exceeded! (max level 20)");
				return [];
			}
			
			var result = [];
			for (var i = start_i; i < input.length-1; i++) {
				if (input[i] instanceof Array) {
					result.push(quote_list(input[i], 0, level+1));
					continue;
				}
				
				var first_char = ctx.source[input[i] >> 16];
				
				// inner quoted list
				if (input[i] & 32767 == 2 && 
					(first_char == "'" || first_char == '`') && ctx.source[(input[i] >> 16) + 1] == '(')
					result.push(quote_list(input[i], 1, level+1));
					
				// number
				else if ("0123456789".indexOf(first_char) > -1 || (first_char == '-' && "0123456789".indexOf(ctx.source[(input[i] >> 16) + 1]) > -1)) {
					result.push((input[i] & 32768) 
						? parseFloat(ctx.source.substring(input[i] >> 16, (input[i] >> 16) + (input[i] & 32767)))
						: parseInt(ctx.source.substring(input[i] >> 16, (input[i] >> 16) + (input[i] & 32767))));
				}
				// symbol+quoted_atom
				else if ("'`:#@&".indexOf(first_char) > -1) {
					var atom = new String(ctx.source.substring((input[i] >> 16) + 1, (input[i] >> 16) + (input[i] & 32767)));
					atom.atom_prefix = first_char;
					result.push(atom);
				}
				// unquoted_atom
				else if (first_char == ',') {
					var tmp_ctx = new Context(ctx.scope, ctx, input, i, ctx.source, ctx.file, 0);
					tmp_ctx.interpret_elem();
					
					// ,@list - надо добавить все элементы в результат
					if (ctx.source[(input[i] >> 16) + 1] == '@') {
						if (tmp_ctx.result[i] instanceof Array)
						for (var ii = 0; ii < tmp_ctx.result[i].length; ii++)
							result.push(tmp_ctx.result[i][ii]);
						continue;
					}	
					else
						result.push(tmp_ctx.result[i]);
				}
				// string
				else if (first_char == '"')
					result.push(JSON.parse(ctx.source.substring((input[i] >> 16), (input[i] >> 16) + (input[i] & 32767))));
				// JSON
				else if (first_char == '[' || first_char == '{') {
					result.push(eval("("+ctx.source.substring((input[i] >> 16), (input[i] >> 16) + (input[i] & 32767))+")"));
				}
				// atom (var_name)
				else {
					var atom = new String(ctx.source.substring((input[i] >> 16), (input[i] >> 16) + (input[i] & 32767)));
					atom.atom_prefix = "";
					result.push(atom);
				}
			}
			
			return result;
		}
		ctx.result = quote_list(ctx.input, 1, 1);
		
		// return to parent or no-parent (or debugging ctx)
		if (ctx.parent && !(ctx.type & 32)) {
			ctx.parent.result[ctx.parent.indx] = ctx.result;
			ctx = ctx.parent;
			continue;
		}
		else {
			ctx.indx = ctx.input.length;
			return ctx;
		}
	}
	
	// (set/setq x ...)
	if ((len == 3 || len == 4) && ctx.source[off] == "s" 
	&& ctx.source[off+1] == "e" && ctx.source[off+2] == "t" 
	&& (len == 4 ? ctx.source[off+3] == "q" : true)) {
		
		// skip set/setq
		if (ctx.indx == 0) {
			ctx.indx = 1;
			return ctx;
		}
		
		// set-var_name
		if (ctx.indx % 2 == 1 && ctx.indx < ctx.input.length-1) {
			
			// setq
			if (len == 4)
				ctx.result[ctx.indx] = ctx.source.substring(ctx.input[ctx.indx] >> 16, (ctx.input[ctx.indx] >> 16) + (ctx.input[ctx.indx] & 32767));
			
			// set
			else {
				// вычесляем переменную (если список, то переключаемся на него)
				if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
					return ctx.result[ctx.indx]; /* return Context */
			}
				
			ctx.indx++;
			return ctx;
		}
		
		// set-value
		if (ctx.indx % 2 == 0 && ctx.indx < ctx.input.length-1) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx];
			
			ctx.set(ctx.result[ctx.indx-1], ctx.result[ctx.indx]);
			
			// есть ли ещё пары?
			if (ctx.indx < ctx.input.length-2) {
				ctx.indx++;
				return ctx;
			}
			
			// закончили с присвоением
			ctx.result = ctx.result[ctx.indx];
			
			// return to parent or no-parent (or debugging ctx)
			if (ctx.parent && !(ctx.type & 32)) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue;
			}
			else {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
		debugger; // TODO нехватка аргумнтов
		throw new Error("???");
	}
	
	// (unset/unsetq x ...)
	if ((len == 5 || len == 6) && ctx.source[off] == "u" 
	&& ctx.source[off+1] == "n" && ctx.source[off+2] == "s" 
	&& ctx.source[off+3] == "e" && ctx.source[off+4] == "t" 
	&& (len == 6 ? ctx.source[off+5] == "q" : true)) {
		
		// skip first token
		if (ctx.indx == 0) {
			ctx.indx = 1;
			return ctx;
		}
		
		if (ctx.indx < ctx.input.length-1) {
			// setq
			if (len == 6)
				ctx.result[ctx.indx] = ctx.source.substring(ctx.input[ctx.indx] >> 16, (ctx.input[ctx.indx] >> 16) + (ctx.input[ctx.indx] & 32767));
			
			// set
			else {
				// вычесляем переменную (если список, то переключаемся на него)
				if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
					return ctx.result[ctx.indx]; /* return Context */
			}
			
			// делаем дело
			ctx.set(ctx.result[ctx.indx], undefined, true);
			
			ctx.indx++;
			return ctx;
		}
		
		// закончили
		ctx.result = undefined;
		
		// return to parent or no-parent (or debugging ctx)
		if (ctx.parent && !(ctx.type & 32)) {
			ctx.parent.result[ctx.parent.indx] = ctx.result;
			ctx = ctx.parent;
			continue;
		}
		else {
			ctx.indx = ctx.input.length;
			return ctx;
		}
	}
	
	// (get/put ...) ?
	else if (len == 3 && ctx.source[off+2] == "t" && 
		((ctx.source[off] == "g" && ctx.source[off+1] == "e") ||
		(ctx.source[off] == "p" && ctx.source[off+1] == "u"))) {
		
		// skip get/put
		if (ctx.indx == 0) {
			ctx.indx = 1;
			return ctx;
		}
		
		// вычеслим аргументы все
		if (ctx.indx < ctx.input.length-1) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx]; /* return Context */
			
			ctx.indx++;
			return ctx;
		}
		
		// совершим операцию
		// get...
		if(ctx.source[off] == 'g') {
			var obj = ctx.result[1];
			for(var i = 2; i < ctx.input.length-1; i++)
				if(obj) obj = obj[ctx.result[i]];
  
			ctx.result = obj;
		}
		// put...
		else {
			var obj = ctx.result[1];
			for(var i = 2; i < ctx.input.length-3; i++)
				if(ctx.result[i] in obj) obj = obj[ctx.result[i]];
				else obj = obj[ctx.result[i]] = {};
  
			ctx.result = obj[ctx.result[ctx.input.length-3]] = ctx.result[ctx.input.length-2];
		}
		
		// return to parent or no-parent (or debugging ctx)
		if (ctx.parent && !(ctx.type & 32)) {
			ctx.parent.result[ctx.parent.indx] = ctx.result;
			ctx = ctx.parent;
			continue;
		}
		else {
			ctx.indx = ctx.input.length;
			return ctx;
		}
		
		debugger;
		throw new Error("???");
	}
	
	// (let (...) ...)
	else if (len == 3 && ctx.source[off] == "l" && ctx.source[off+1] == "e" 
		&& ctx.source[off+2] == "t") {
		
		// let (skip)
		if (ctx.indx == 0) {
			ctx.scope = {};
			ctx.indx++;
			return ctx;
		}
		
		// let-set
		if (ctx.indx == 1) {
			if (ctx.input[ctx.indx] instanceof Array == false)
				throw new Error("Argument 2 must be list!");

			// вычеслим set-часть
			if (!ctx.result[ctx.indx]) {
			
				var new_ctx = new Context(ctx.scope, ctx, [-65536 + 1], 1, ctx.source, ctx.file, 0);
			
				// переупакуем в let-set-part
				for (var i = 0; i < ctx.input[1].length-1; i++)
					if (ctx.input[1][i] instanceof Array) {
						new_ctx.input.push(ctx.input[1][i][0]);
						new_ctx.input.push(ctx.input[1][i][1]);
					}
					else { 
						new_ctx.input.push(ctx.input[1][i]);
						new_ctx.input.push(undefined);
					}
				new_ctx.input.push(ctx.input[1][ctx.input[1].length-1]);
				
				// запускаем вычесление
				return new_ctx;
			}
			
			// создадим свой scope
// 			ctx.scope = {};
// 			for (var i = 1; i < ctx.result[1].length; i+=2)
// 				ctx.scope[ctx.result[1][i]] = ctx.result[1][i+1];
			
			// переключимся на тело let
			ctx.indx++;
			return ctx;
		}
		
		// let-body
		if (ctx.indx > 1 && ctx.indx < ctx.input.length-1) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx]; /* return Context */
			
			// след.элемент ->
			if (ctx.indx < ctx.input.length-2) {
				ctx.indx++;
				return ctx;
			}
			
			ctx.result = ctx.result[ctx.indx];
			
			// return to parent or no-parent (or debugging ctx)
			if (ctx.parent && !(ctx.type & 32)) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue;
			}
			else {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
		throw new Error("???");
		debugger;
	}
	// [let-set-part]
	else if (off == 65535 && len == 1) {
		
		// name
		if (ctx.indx % 2 == 1) {
			ctx.result[ctx.indx] = ctx.source.substring(ctx.input[ctx.indx] >> 16, (ctx.input[ctx.indx] >> 16) + (ctx.input[ctx.indx] & 32767));
			ctx.indx++;
			return ctx;
		}
		
		// value
		if (ctx.indx % 2 == 0) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx]; /* return Context */
			
			// пара var_name, var_value вычеслена -> добавим в let-scope
			ctx.scope[ctx.result[ctx.indx-1]] = ctx.result[ctx.indx];
			
			// след.элемент?
			if (ctx.indx < ctx.input.length-2) {
				ctx.indx++;
				return ctx;
			}
			
			// return to parent or no-parent (or debugging ctx)
			if (ctx.parent && !(ctx.type & 32)) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue;
			}
			else {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
	}
	
	// (if () ... :else ... :elseif ...)
	else if (len == 2 && ctx.source[off] == "i" && ctx.source[off+1] == "f") {
		
		// if (skip)
		if (ctx.indx == 0) {
			ctx.result[0] = 0;
			ctx.indx++;
			return ctx;
		}
		
		// :else, :elseif (skip)
		if (ctx.indx == ctx.result[0]) {
			
			// проверим след. элемент на :else/:elseif
			if (ctx.input.length > ctx.result[0]+1 && ctx.input[ctx.indx+1] instanceof Array == false && (ctx.input[ctx.indx+1] & 32767) >= 5) {
				var off2 = ctx.input[ctx.indx+1] >> 16;
				var len2 = (ctx.input[ctx.indx+1] & 32767)
				var next_is_else = ctx.source[off2] == ':' 
					&& ctx.source[off2+1] == 'e' && ctx.source[off2+2] == 'l' 
					&& ctx.source[off2+3] == 's' && ctx.source[off2+4] == 'e' 
					&& (len2 == 5 || len2 == 7 
					&& ctx.source[off2+5] == 'i' && ctx.source[off2+6] == 'f');
			}
			else 
				var next_is_else = false;
			
			// если дальше не :else, а инструкция -> то переключимся на неё
			if (ctx.input.length > ctx.indx+2 && next_is_else == false) {
				ctx.indx++; 
				return ctx;
			}
			
			// :else keyword found -> return undefined
			ctx.indx == ctx.input.length;
			ctx.result = undefined;
			
			// return to parent or no-parent (or debugging ctx)
			if (ctx.parent && !(ctx.type & 32)) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue; // return ctx; // for step_over
			}
			else {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
		// if-test, elseif-test
		if (ctx.indx == ctx.result[0]+1 && (ctx.input[ctx.result[0]] & 32767) != 5) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx]; /* return Context */
			
			ctx.result[ctx.indx] = !!ctx.result[ctx.indx]; // TODO one atom in list?
			
			// -> true
			if (ctx.result[ctx.indx]) { 
				
				// проверим след. элемент на :else/:elseif
				if (ctx.input.length > ctx.result[0]+1 && ctx.input[ctx.indx+1] instanceof Array == false && (ctx.input[ctx.indx+1] & 32767) >= 5) {
					var off2 = ctx.input[ctx.indx+1] >> 16;
					var len2 = (ctx.input[ctx.indx+1] & 32767)
					var next_is_else = ctx.source.charCodeAt(off2) == 58 && ctx.source.charCodeAt(off2+1) == 101 && ctx.source.charCodeAt(off2+2) == 108 && ctx.source.charCodeAt(off2+3) == 115 && ctx.source.charCodeAt(off2+4) == 101 && (len2 == 5 || 
					len2 == 7 && 
					ctx.source.charCodeAt(off2+5) == 105 && ctx.source.charCodeAt(off2+6) == 102);
				}
				else 
					var next_is_else = false;
				
				// если дальше не :else, а инструкция -> то переключимся на неё
				if (ctx.input.length > ctx.indx+2 && next_is_else == false) {
					ctx.indx++; 
					return ctx;
				}
				// иначе ctx.parent...
			}
			
			// -> false
			else { 
				
				// ищем :else/:elseif параметр/маркер
				for (ctx.indx++; ctx.indx < ctx.input.length-1; ctx.indx++)
				if (ctx.input[ctx.indx] instanceof Array == false) {
					var off2 = ctx.input[ctx.indx] >> 16;
					var len2 = (ctx.input[ctx.indx] & 32767)
					if (ctx.source.charCodeAt(off2) == 58 && ctx.source.charCodeAt(off2+1) == 101 && ctx.source.charCodeAt(off2+2) == 108 && ctx.source.charCodeAt(off2+3) == 115 && ctx.source.charCodeAt(off2+4) == 101 && (len2 == 5 || 
					len2 == 7 && 
					ctx.source.charCodeAt(off2+5) == 105 && ctx.source.charCodeAt(off2+6) == 102)) {
						ctx.result[0] = ctx.indx;
						return ctx;
					}
				}
				
				// no :else keyword found
			}
			
			ctx.indx == ctx.input.length;
			ctx.result = undefined;
			
			// return to parent or no-parent (or debugging ctx)
			if (ctx.parent && !(ctx.type & 32)) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue; // return ctx; // for step_over
			}
			else {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
		// if-true,if-false
		if (ctx.indx > 1 && ctx.indx < ctx.input.length-1) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx]; /* return Context */
			
			// след.элемент ->
			if ( ctx.indx < ctx.input.length-2) {
				
				// проверим след.элемент на :else/:elseif
				if (ctx.input[ctx.indx+1] instanceof Array == false && (ctx.input[ctx.indx+1] & 32767) >= 5) {
					var off2 = ctx.input[ctx.indx+1] >> 16;
					var len2 = (ctx.input[ctx.indx+1] & 32767)
					var next_is_else = ctx.source.charCodeAt(off2) == 58 && ctx.source.charCodeAt(off2+1) == 101 && ctx.source.charCodeAt(off2+2) == 108 && ctx.source.charCodeAt(off2+3) == 115 && ctx.source.charCodeAt(off2+4) == 101 && (len2 == 5 || 
					len2 == 7 && 
					ctx.source.charCodeAt(off2+5) == 105 && ctx.source.charCodeAt(off2+6) == 102);
				}
				else
					var next_is_else = false;
				
				// если можно переключится на след. то переключимся
				if (next_is_else == false) {
					ctx.indx++;
					return ctx;
				}
			}
			
			ctx.result = ctx.result[ctx.indx];
		
			// return to parent or no-parent (or debugging ctx)
			if (ctx.parent && !(ctx.type & 32)) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue;
			}
			else {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
		debugger;
		throw new Error("???");
	}
	
	// (while ... ...)
	else if (len == 5 && ctx.source[off] == "w" && ctx.source[off+1] == "h" 
		&& ctx.source[off+2] == "i" && ctx.source[off+3] == "l" 
		&& ctx.source[off+4] == "e") {
		
		if (ctx.input.length < 3)
			throw "Need minimum 1 argument!";
		
		// while (skip)
		if (ctx.indx == 0) {
			ctx.indx = 1;
			if (!ctx.type) ctx.type |= 4; // + type=while
			return ctx;
		}
		
		// while-test
		if (ctx.indx == 1) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx]; /* return Context */
			
			// -> true
			// TODO check one atom in the list(?)
			if (!!ctx.result[1]) { 
				// почистим результат, если это не первая итерация
				ctx.result.splice(2);
				
				ctx.indx = 2;
				return ctx; 
			}
			
			// -> false
			ctx.result = ctx.result[ctx.result.length-1];
			
			// return to parent or no-parent (or debugging ctx)
			if (ctx.parent && !(ctx.type & 32)) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue;
			}
			else {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
		// while-body
		if (ctx.indx < ctx.input.length-1) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx]; /* return Context */
			
			// след.элемент body?
			if (ctx.indx+1 < ctx.input.length-1) {
				ctx.indx++;
				return ctx; // for step_over
			}
			
			// почистим test-результат
			delete ctx.result[1];
			
			// и перезапустим
			ctx.indx = 1;
			return ctx;
		}
		
		debugger;
		throw new Error("???");
	}
	
	// (catch ...)
	else if (len == 5 && ctx.source[off] == "c" && ctx.source[off+1] == "a" 
		&& ctx.source[off+2] == "t" && ctx.source[off+3] == "c" 
		&& ctx.source[off+4] == "h") {
		
		// catch (skip)
		if (ctx.indx == 0) {
			ctx.indx = 1;
			ctx.type |= 2; // + type=catch
			return ctx;
		}
		
		// catch-body
		if (ctx.indx == 1) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx]; /* return Context */
			
			ctx.result = ctx.result[1];
			
			// return to parent or no-parent (or debugging ctx)
			if (ctx.parent && !(ctx.type & 32)) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue;
			}
			else {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
	}
	
	// (throw ...)
	else if (len == 5 && ctx.source[off] == "t" && ctx.source[off+1] == "h" 
		&& ctx.source[off+2] == "r" && ctx.source[off+3] == "o" 
		&& ctx.source[off+4] == "w") {
		
		// throw (skip)
		if (ctx.indx == 0) {
			ctx.indx = 1;
			return ctx;
		}
		
		// throw-body
		if (ctx.indx == 1) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx]; /* return Context */
			
			ctx.indx == ctx.input.length-1;
			
			// кидаем эксепшен
			throw ctx.result[1];
		}
		
		// если попросили продолжить в дебагере
		if (ctx.indx == ctx.input.length-1) {
			
			// return to parent or no-parent (or debugging ctx)
			if (ctx.parent && !(ctx.type & 32)) {
				ctx.parent.result[ctx.parent.indx] = ctx.result[1];
				ctx = ctx.parent;
				continue;
			}
			else {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
	}
		
	// (lambda (x y) (...))
	else if (len == 6 && ctx.source[off] == "l" && ctx.source[off+1] == "a" 
		&& ctx.source[off+2] == "m" && ctx.source[off+3] == "b" 
		&& ctx.source[off+4] == "d" && ctx.source[off+5] == "a") {
		
		// lambda (skip)
		if (ctx.indx == 0) {
			ctx.indx = 1;
			return ctx;
		}
		
		// arguments
		if (ctx.indx == 1) {
			
			// список аргументов проверим на breakpoint-ы сами
			if (debugging && ctx.file in breakpoints && ctx.input[1].length > 1) {
				var brks = breakpoints[ctx.file];
				for (var i = 0; i < ctx.input[1].length-1; i++)
					if (brks[ctx.input[1][i]]) throw "Breakpoint!";
			}
			
			ctx.indx++;
			return ctx;
		}

		// body
		if (ctx.indx == 2) {
			
			var __littlelisp = {
				args_names: ctx.input[2],
				body: ctx.input.slice(3), 
				source: ctx.source, 
				file: ctx.file
			};
			
			// создадим JS-функцию 
			ctx.result = function(){ return function() {
				var lambdaScope = {arguments: arguments, thiz: this};
				var lambdaArgNames = __littlelisp.args_names;
				for(var i = 0; i < lambdaArgNames.length-1; i++)
					lambdaScope[ __littlelisp.source.substring(lambdaArgNames[i] >> 16, (lambdaArgNames[i] >> 16) + (lambdaArgNames[i] & 32767))] = arguments[i] || undefined;
				
				return littleLisp.interpret( __littlelisp.body, lambdaScope,  __littlelisp.source,  __littlelisp.file);
			}}();
			
			// дополнительно сохраним исходный код функции на теле функции
			ctx.result.__littlelisp = __littlelisp;

			// return to parent or no-parent (or debugging ctx)
			if (ctx.parent && !(ctx.type & 32)) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue;
			}
			else {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
		debugger;
		throw new Error("???");
	}
	
	// (defun <name> (args...) (...body...)) 
	// (defmeth <name> (args...) (...body...)) 
	// (defmeth-static <name> (args...) (...body...))
	// (defclassmeth <name> (args...) (...body...))
	// (defmacro <name> (args...) (...body...))
	else if (len > 3 && ctx.source[off] == "d" && ctx.source[off+1] == "e" 
		&& ctx.source[off+2] == "f" 
		&& (len == 5 && ctx.source[off+3] == "u" && ctx.source[off+4] == "n"
		||  len == 8 && ctx.source[off+3] == 'm' && ctx.source[off+4] == 'a' 
			&& ctx.source[off+5] == 'c' && ctx.source[off+6] == 'r'
			&& ctx.source[off+7] == 'o'
		||  len == 7 && ctx.source[off+3] == 'm' && ctx.source[off+4] == 'e' 
			&& ctx.source[off+5] == 't' && ctx.source[off+6] == 'h'
		||  len == 12 && ctx.source[off+3] == 'c'
			&& ctx.source[off+4] == 'l' && ctx.source[off+5] == 'a'
			&& ctx.source[off+6] == 's' && ctx.source[off+7] == 's'
			&& ctx.source[off+8] == 'm' && ctx.source[off+9] == 'e'
			&& ctx.source[off+10] == 't' && ctx.source[off+11] == 'h'
		||  len == 14 && ctx.source[off+3] == 'm'
			&& ctx.source[off+4] == 'e' && ctx.source[off+5] == 't'
			&& ctx.source[off+6] == 'h' && ctx.source[off+7] == '-'
			&& ctx.source[off+8] == 's' && ctx.source[off+9] == 't'
			&& ctx.source[off+10] == 'a' && ctx.source[off+11] == 't'
			&& ctx.source[off+12] == 'i' && ctx.source[off+13] == 'c')) {
		
		// def* <name> (skip)
		if (ctx.indx == 0 || ctx.indx == 1) {
			ctx.indx++;
			return ctx;
		}
		
		// arguments
		if (ctx.indx == 2) {
			
			// список аргументов проверим на breakpoint-ы сами
			if (debugging && ctx.file in breakpoints && ctx.input[2].length > 1) {
				var brks = breakpoints[ctx.file];
				for (var i = 0; i < ctx.input[2].length-1; i++)
					if (brks[ctx.input[2][i]]) throw "Breakpoint!";
			}
			
			ctx.indx++;
			return ctx;
		}

		// body
		if (ctx.indx == 3) {
			
			var __littlelisp = {
				args_names: ctx.input[2],
				body: ctx.input.slice(3), 
				source: ctx.source, 
				file: ctx.file
			};
			
			// macros?
			if (len == 8) __littlelisp.is_macros = true;
			
			// создадим функцию 
			var func = function(){ return function() {
				var new_scope = {arguments: arguments, thiz: this};
				var arg_names = __littlelisp.args_names;
				for(var i = 0; i < arg_names.length-1; i++)
					new_scope[__littlelisp.source.substring(arg_names[i] >> 16, (arg_names[i] >> 16) + (arg_names[i] & 32767))] = arguments[i] || undefined;
				
				return littleLisp.interpret(__littlelisp.body, new_scope, __littlelisp.source, __littlelisp.file);
			}}();
			
			// дополнительно сохраним исходный код функции на теле функции
			func.__littlelisp = __littlelisp;
			
			var func_name = ctx.source.substring(ctx.input[1] >> 16, (ctx.input[1] >> 16) + (ctx.input[1] & 32767));
			
			// defmeth?
			if (len == 7)
				func_name = func_name.substring(0, func_name.lastIndexOf('.')) + '.prototype' + func_name.substring(func_name.lastIndexOf('.'));
			
			// ... и сохраним её под именем (или присвоим полю объекта)
			ctx.set(func_name, func);
			
			ctx.result = undefined; // не будем возвращать функцию
				
			// return to parent or no-parent (or debugging ctx)
			if (ctx.parent && !(ctx.type & 32)) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue;
			}
			else {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
		debugger;
		throw new Error("???");
	}
	
	// (defclass <name> ...)
	else if (len == 8 && ctx.source[off] == "d" && ctx.source[off+1] == "e" 
		&& ctx.source[off+2] == "f" && ctx.source[off+3] == "c" 
		&& ctx.source[off+4] == "l" && ctx.source[off+5] == "a" 
		&& ctx.source[off+6] == "s" && ctx.source[off+7] == "s") {
		
		// defclass (skip)
		if (ctx.indx == 0) {
			ctx.indx = 1;
			return ctx;
		}
		
		// class_name
		if (ctx.indx == 1) {
			// ClassName
			ctx.result[1] = ctx.source.substring(ctx.input[1] >> 16, (ctx.input[1] >> 16) + (ctx.input[1] & 32767));
			ctx.indx++;
			return ctx;
		}
		
		// :extends ... :instvars ... :classvars ... :constructor ...
		if (ctx.indx < ctx.input.length-1) {

			var token1 = ctx.source.substring(ctx.input[ctx.indx] >> 16, (ctx.input[ctx.indx] >> 16) + (ctx.input[ctx.indx] & 32767));
			// TODO maybe interpret_elem()?
			var token2 = ctx.indx+1 >= ctx.input.length-1
				? undefined
				: ctx.input[ctx.indx+1] instanceof Array 
				? ctx.input[ctx.indx+1] // list
				: ctx.source.substring(ctx.input[ctx.indx+1] >> 16, (ctx.input[ctx.indx+1] >> 16) + (ctx.input[ctx.indx+1] & 32767));
		
			// :extends SuperClassName
			if (token1.match(/[:@`']extends/))
				ctx.result[ctx.indx] = ctx.result[1]+".prototype.__proto__ = " + token2 + ".prototype; "+ctx.result[1]+".__proto__ = "+token2+"; ";
			
			// :contstructor func_name
			else if (token1.match(/[:@`']constructor/)) 
				ctx.result[0] = ctx.result[1]+".prototype['"+token2+"'].apply(this, arguments);";
		
			// :instvars/public ...
			else if (token1.match(/[:@`'](instvars|public)/)) {
				ctx.result[ctx.indx] = "";
				
				// :instvars (...)
				if(ctx.input[ctx.indx+1] instanceof Array)
				for (var i = 1; i < token2.length-1; i++) {
					token2[i] = ctx.source.substring(token2[i] >> 16, (token2[i] >> 16) + (token2[i] & 32767)).trim(":@'`\"");
				}
				
				// :instvars "..."
				else if(token2[0] == '"') token2 = token2.substring(1, token2.length-1).split(" ");
				
				// :instvars 'only_one_var
				else 
					token2 = [token2.trim(":@'`")];
				
				// соберём js-код создания полей объекта
				for (var i = 0; i < token2.length; i++) 
					if(token2[i] !== "")
						ctx.result[ctx.indx] = ctx.result[ctx.indx] + ctx.result[1] + ".prototype['" + token2[i] + "'] = window." + ctx.result[1] + ".prototype['" + token2[i] + "']||undefined; ";
		
			}
			
			// :classvars/:static
			else if (token1.match(/[:@`'](classvars|static)/)) {
				ctx.result[ctx.indx] = "";
				
				// :classvars (...)
				if(ctx.input[ctx.indx+1] instanceof Array)
				for (var i = 1; i < token2.length-1; i++) {
					token2[i] = ctx.source.substring(token2[i] >> 16, (token2[i] >> 16) + (token2[i] & 32767)).trim(":@'`\"");
				}
				
				// :classvars "..."
				else if(token2[0] == '"') token2 = token2.substring(1, token2.length-1).split(" ");
				
				// :classvars 'only_one_var
				else 
					token2 = [token2.trim(":@'`")];
				
				// соберём js-код создания полей класса
				for (var i = 0; i < token2.length; i++) 
					if(token2[i] !== "")
						ctx.result[ctx.indx] = ctx.result[ctx.indx] + ctx.result[1] + "['" + token2[i] + "'] = window."+ctx.result[1]+"['" + token2[i] + "']||undefined; ";
			}
			
			// :initializer func_name?
			else {
				console.error(token1+" not implemented yet!");
			}
			
			ctx.indx += 2;
			return ctx;
		}

		// возмём название класса
		var class_name = ctx.result[1];
		
		// Создадим анонимную функцию для нового класа
		var new_class = eval("(function " + class_name + "(){" + (ctx.result[0]||"") + "})");
		
		// сохраняем ссылку на старый prototype класса если класс уже существует
		if (class_name in window)
			new_class.prototype = window[class_name].prototype;

		// instvars, classvars, extends...
		new_class.__littlelisp.initialize_code = ctx.result.slice(2).join("\n");
		
		// для удобного последующего редактирования
		new_class.__littlelisp.source = ctx.source.substring(ctx.input[ctx.input.length-1] >> 16, (ctx.input[ctx.input.length-1] >> 16) + (ctx.input[ctx.input.length-1] & 32767));
		new_class.__littlelisp.file = ctx.file;
		
		// зарегистрируем наконец новый класс (заменим существующий)
		window[class_name] = new_class;
		eval(window[class_name].__littlelisp.initialize_code);
		
		ctx.result = undefined; // не возвращаем результат
		
		// return result to parent/no-parent
		if (ctx.parent) {
			ctx.parent.result[ctx.parent.indx] = ctx.result;
			ctx = ctx.parent;
			continue;
		}
		else {
			ctx.indx = ctx.input.length;
			return ctx;
		}
	}
	
	// (new <name> args...)
	else if (len == 3 && ctx.source[off] == "n" && ctx.source[off+1] == "e" && ctx.source[off+2] == "w") {
		
		// new (skip)
		if (ctx.indx == 0) {
			ctx.indx = 1;
			return ctx;
		}
		
		// args...
		if (ctx.indx <= ctx.input.length-2) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx]; /* return Context */
			
			// есть следующий аргумент? - переключимся на него
			if (ctx.indx < ctx.input.length-2) {
				ctx.indx++;
				return ctx;
			}
		}
		
		// завершили создание объекта?
		if (ctx.indx == ctx.input.length-1 && ctx.result[0]) {
			ctx.result = ctx.result[0];
			
			// return to parent or no-parent (or debugging ctx)
			if (ctx.parent && !(ctx.type & 32)) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue;
			}
			else {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
		/* создаём объект */
		var class_name = ctx.source.substring(ctx.input[1] >> 16, (ctx.input[1] >> 16) + (ctx.input[1] & 32767));
		
		// проверим на существование класса
		if (class_name in window == false || window[class_name] instanceof Function == false)
			throw 'Not found class/function "'+ctx.result[1]+'" in global scope!';
			// new Error("message");
		
		// lisp-class
		if (window[class_name].__littlelisp) {
			ctx.result[0] = {};
			ctx.result[0].__proto__ = ctx.result[1].prototype;
			
			// найдём конструктор
			var ctor_name = ctx.result[1].toString().match(/{[^.]+\.prototype\['([^']+)'\]/);
			if (ctor_name) {
				
				// существует ли конструктор/функция?
				if (ctor_name[1] in ctx.result[1].prototype == false)
					throw "Constructor/function " + ctor_name[0] + " not found!";
			
				// проверим конструктор
				var ctor = ctx.result[1].prototype[ctor_name[1]];
				if (ctor instanceof Function == false)
					throw "Constructor " + ctor_name[0] + " not a function!";
			
				// lisp-функция
				if ("__littlelisp" in ctor){
					var new_scope = {arguments: ctx.result.slice(2), thiz: ctx.result[0]};
					var arg_names = ctor.__littlelisp.args_names;
					for(var i = 0; i < arg_names.length-1; i++)
					new_scope[ctor.__littlelisp.source.substring(arg_names[i] >> 16, 
						(arg_names[i] >> 16) + (arg_names[i] & 32767))] 
						= ctx.result[i+2] || undefined;
						
					ctx.indx = ctx.input.length-1; // отметим, что завершили создание объекта заранее
					
					return new Context(new_scope, ctx, ctor.__littlelisp.body, 0, ctor.__littlelisp.source, ctor.__littlelisp.file, 16 /* type=function */);
				}
			}
		}
		
		// native js-class
		else if (ctx.input.length == 3)
			ctx.result[0] = new ctx.result[1]();
		else {
			var ctor_code = ["ctx.result[0] = new ctx.result[1]("];
			for (var i = 2; i < ctx.input.length-1; i++)
				ctor_code.push((i > 2 ? ", ctx.result[" : "ctx.result[") + i + "]");
			ctor_code.push(")");
			eval(ctor_code.join(""));
		}
		
		ctx.indx = ctx.input.length-1; // отметим, что завершили создание
		
		continue;
	}
	
	// (return [...])
	else if (len == 6 && ctx.source[off] == "r" && ctx.source[off+1] == "e" 
		&& ctx.source[off+2] == "t" && ctx.source[off+3] == "u" 
		&& ctx.source[off+4] == "r" && ctx.source[off+5] == "n") {
		
		// первым делом вычислим аргумент (если он есть)
		if (ctx.indx == 0 && ctx.input.length > 2) {
			ctx.indx = 1;
			return ctx;
		}
		
		// вычислим аргумент (если есть)
		if (ctx.indx == 1 && ctx.input.length > 2) {
			
			// вычисляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx]; /* return Context */

		}
		
		// найдём стартовый контекст функции
		for (var func_ctx = ctx; func_ctx.parent; func_ctx = func_ctx.parent) {
			if (func_ctx && func_ctx.type & 16+32) // type=func+dbg
				break;
		}
		
		ctx.result = ctx.result[ctx.indx];
			
		// return to parent or no-parent (or debugging ctx)
		if (func_ctx.parent && !(ctx.type & 32)) {
			func_ctx.parent.result[func_ctx.parent.indx] = ctx.result;
			ctx = func_ctx.parent;
			continue;
		}
		else {
			ctx.indx = ctx.input.length;
			return ctx;
		}
	}
	
	// (break [...])
	else if (len == 5 && ctx.source[off] == "b" && ctx.source[off+1] == "r" 
		&& ctx.source[off+2] == "e" && ctx.source[off+3] == "a" 
		&& ctx.source[off+4] == "k") {
		
		// первым делом вычеслим аргумент если он есть
		if (ctx.indx == 0 && ctx.input.length == 3) {
			ctx.indx = 1;
			return ctx;
		}
		
		// либо аргумента нет, либо вычислили
		if (ctx.indx == 1 && ctx.input.length == 3 || ctx.input.length == 2) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx == 1 && ctx.indx in ctx.result == false && !ctx.interpret_elem())
				return ctx.result[ctx.indx]; /* return Context */

			// найдём стартовый контекст цикла
			for (var return_ctx = ctx.parent; return_ctx; return_ctx = return_ctx.parent) {
				if (return_ctx && return_ctx.type & 4+16+32 /* while+func+dbg */)
					break;
			}
			
			ctx.result = ctx.input.length == 2 ? undefined : ctx.result[1];
			
			// return to parent or no-parent (or debugging ctx)
			if (return_ctx.parent && !(ctx.type & 32)) {
				return_ctx.parent.result[return_ctx.indx] = ctx.result;
				ctx = return_ctx.parent;
				continue;
			}
			else {
				return_ctx.indx = return_ctx.input.length;
				return return_ctx;
			}
		}
		
		debugger;
		throw new Error("???");
	}
	
	// (continue)
	// TODO (continue <LEVEL>)?
	else if (len == 8 && ctx.source[off] == "c" && ctx.source[off+1] == "o" 
		&& ctx.source[off+2] == "n" && ctx.source[off+3] == "t" 
		&& ctx.source[off+4] == "i" && ctx.source[off+5] == "n" 
		&& ctx.source[off+6] == "u" && ctx.source[off+7] == "e") {
		
		// skip first token
		if (ctx.indx == 0) {
			ctx.indx = 1;
			return ctx;
		}
			
		if (ctx.indx == 1) {
			
			ctx.result = undefined;
			
			// найдём стартовый контекст цикла
			for (var return_ctx = ctx.parent; return_ctx; return_ctx = return_ctx.parent) {
				if (return_ctx && return_ctx.type & 4)  // type=while
					break;
			}
			
			// переключимся на контекст цикла обратно
			if (return_ctx && return_ctx.type & 4) {
				ctx = return_ctx;
				ctx.result[ctx.indx] = undefined;
				
				// while ?
				if (ctx.source[ctx.input[0] >> 16] == 'w') { 
					ctx.result = []; // почистим для след.итерации
					ctx.indx = 1; // switch to while-test
				}
				return ctx;
			}
			
			// иначе ведём себя как break
			// return to parent or no-parent (or debugging ctx)
			if (ctx.parent && !(ctx.type & 32)) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				continue;
			}
			else {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
		debugger;
		throw new Error("???");
	}
	
	// (debugger)
	else if (len == 8 && ctx.source[off] == "d" && ctx.source[off+1] == "e" 
		&& ctx.source[off+2] == "b" && ctx.source[off+3] == "u" 
		&& ctx.source[off+4] == "g" && ctx.source[off+5] == "g"
		&& ctx.source[off+6] == "e" && ctx.source[off+7] == "r") {
		
		// вызовем отладчик броузера
		debugger;
		
		// return to parent or no-parent (or debugging ctx)
		if (ctx.parent && !(ctx.type & 32)) {
			ctx.parent.result[ctx.parent.indx] = ctx.result;
			continue;
		}
		else {
			ctx.indx = ctx.input.length;
			return ctx;
		}
	}
	
	// (typeof ... :eq ...)
	else if (len == 6 && ctx.source[off] == "t" && ctx.source[off+1] == "y" 
		&& ctx.source[off+2] == "p" && ctx.source[off+3] == "e" 
		&& ctx.source[off+4] == "o" && ctx.source[off+5] == "f") {

		if (ctx.input.length < 3)
			throw "Need minimum 1 argument!";
		
		// typeof (skip)
		if (ctx.indx == 0) {
			ctx.indx = 1;
			return ctx;
		}
		
		if (ctx.indx < ctx.input.length-1) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
					return ctx.result[ctx.indx];
			
			// следующий?
			if (ctx.indx < ctx.input.length-2) {
				ctx.indx++;
				continue;
			}
		}
		
		// делаем дело
		if (ctx.indx < ctx.input.length-1) {

			// вычесляем тип с поправкой на lisp-типы
			var type = ctx.result[1] instanceof String && "atom_prefix" in ctx.result[1] 
				? "atom"
				: typeof ctx.result[1];
			
			// :eq ... ?
			if (ctx.result[2] == "eq")
				ctx.result = type == ctx.result[3];
			else
				ctx.result = type;
		
			
			// return to parent or no-parent (or debugging ctx)
			if (ctx.parent && !(ctx.type & 32)) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue;
			}
			else {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
		debugger;
		throw new Error("???");
	}
	
	// (gensym ...)
	else if (len == 6 && ctx.source[off] == "g" && ctx.source[off+1] == "e" 
		&& ctx.source[off+2] == "n" && ctx.source[off+3] == "s" 
		&& ctx.source[off+4] == "y" && ctx.source[off+5] == "m") {
		
		// gensym (skip)
		if (ctx.indx == 0) {
			ctx.indx = 1;
			return ctx;
		}
		
		// есть аргумент
		if (ctx.indx == 1 && ctx.input.length == 3) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
					return ctx.result[ctx.indx];
			
			ctx.indx++;
// 			continue;
		}
		
		// делаем дело
		if (ctx.indx == ctx.input.length-1) {

			ctx.result = new String((ctx.result[1] || "GTMP") + (gensym_counter++));
			ctx.result.atom_prefix = "";
			
			// return to parent or no-parent (or debugging ctx)
			if (ctx.parent && !(ctx.type & 32)) {
				ctx.parent.result[ctx.parent.indx] = ctx.result;
				ctx = ctx.parent;
				continue;
			}
			else {
				ctx.indx = ctx.input.length;
				return ctx;
			}
		}
		
		debugger;
		throw new Error("???");
	}
	
	// (<macros> ...)
	else if (ctx.result.length && ctx.result[0] instanceof Function && (ctx.result[0].__littlelisp||{}).is_macros) {
		
		// аргументы макроса не вычесляются
		if (ctx.indx < ctx.input.length-1) {
			function macros_decoder(input, i) {
				var result = [];

				// list
				if (input[i] instanceof Array) {
					for (var ii = 0; ii < input[i].length-1; ii++)
						result.push(macros_decoder(input[i], ii));
					return result;
				}
				
				var first_char = ctx.source[input[i] >> 16];
		
				// number
				if ("0123456789".indexOf(first_char) > -1 || (first_char == '-' && "0123456789".indexOf(ctx.source[(input[i] >> 16) + 1]) > -1))
					result.push((input[i] & 32768)
						? parseFloat(ctx.source.substring(input[i] >> 16, (input[i] >> 16) + (input[i] & 32767)))
						: parseInt(ctx.source.substring(input[i] >> 16, (input[i] >> 16) + (input[i] & 32767))));
				// string
				else if (first_char == '"')
					result.push(JSON.parse(ctx.source.substring((input[i] >> 16), (input[i] >> 16) + (input[i] & 32767))));
				// inner quoted list
				if (input[i] & 32767 == 2 && 
					(first_char == "'" || first_char == "`") && ctx.source[(input[i] >> 16) + 1] == '(') {
					var quote = new String("quote"); quote.atom_prefix = "";
					result.push(quote);
				}
				// embedded JSON
				else if (first_char == '{' || first_char == '[') {
					result.push(eval("("+ctx.source.substring((input[i] >> 16), (input[i] >> 16) + (input[i] & 32767))+")"));
				}
				// symbol+quoted_atom
				else if ("'`:#@&".indexOf(first_char) > -1) {
					var atom = new String(ctx.source.substring((input[i] >> 16) + 1, (input[i] >> 16) + (input[i] & 32767)));
					atom.atom_prefix = first_char;
					result.push(atom);
				}
				// atom (var_name)
				else {
					var atom = new String(ctx.source.substring((input[i] >> 16), (input[i] >> 16) + (input[i] & 32767)));
					atom.atom_prefix = "";
					result.push(atom);
				}
				return result[0];				
			} // macros_encoder
			
			// трансформируем аргумент в данные
			ctx.result[ctx.indx] = macros_decoder(ctx.input, ctx.indx);
			
			// двигаемся к след.элементу списка
			if (ctx.indx+1 < ctx.input.length-1) {
				ctx.indx++;
				return ctx;
			}
		}
		
		// вызов макроса завершён успешно
		if (ctx.indx == ctx.input.length-1 && ctx.result.length == ctx.input.length) { 
			
			function macros_encoder(elem, level, new_source) {
				if (elem instanceof Array) {
					if (elem.length == 0)
						return new_source.push("()");
					
					// преобразуем все элементы в строки
					var start_new_source_len = new_source.length;
					for (var i = 0; i < elem.length; i++) {
						macros_encoder(elem[i], level + 1, new_source);
						
						// откроем скобку
						if (i == 0) {
							new_source[start_new_source_len] = 
							(level > 0 ? '\n' : '') + 
							(new Array(level+1)).join("\t") + '(' + new_source[start_new_source_len];
						}
					}
					
					// закроем скобку
					new_source[new_source.length-1] = new_source[new_source.length-1] + ')';
				}
				else if (elem == null)
					new_source.push("null");
				else if (typeof elem == "number" || typeof elem == "boolean")
					new_source.push(elem.toString());
				else if (typeof elem == "undefined")
					new_source.push("undefined");
				else if (typeof elem == "string")
					new_source.push(JSON.stringify(elem));
				else if (elem instanceof String && "atom_prefix" in elem)
					new_source.push(elem.atom_prefix + elem);
				else { // object
					new_source.push(JSON.stringify(elem));
				}
			}
			
			// преобазуем в исходный-текст
			var new_source = [];
			macros_encoder(ctx.result[ctx.result.length-1], 0, new_source);
			new_source = new_source.join(' ');
			
			// парсим в код
			var new_input = parse(new_source);
			
			// заблокируем дальнейшее выполнение
			ctx.indx = ctx.input.length;
			
			// заменяем ctx вызова макроса на ctx нового кода
			return new Context(ctx.scope, ctx.parent, 
				new_input, 0,
				new_source, 
				"<macros-result>"
			);
		}
		
		// переключимся непосредственно на вызов макроса
		ctx.indx = ctx.input.length-1
		
		// упакуем аргументы в новый scope
		var new_scope = {arguments: ctx.result.slice(1), thiz: ctx.this_for_elem1};
		var arg_names = ctx.result[0].__littlelisp.args_names;
		for(var i = 0; i < arg_names.length-1; i++)
			new_scope[ctx.result[0].__littlelisp.source.substring(arg_names[i] >> 16, (arg_names[i] >> 16) + (arg_names[i] & 32767))] 
				= ctx.result[i+1];
		
		// TCO - оптимизация хвостовой рекурсии.
		for (var up_ctx = ctx; up_ctx; up_ctx = up_ctx.parent) {
				
			// если не в хвосте, то прервём оптимизацию
			if (up_ctx.indx < up_ctx.input.length-2) break;
				
			// в хвосте функции?
			if (up_ctx.type & 16) {
					
				// если тело одинаковое, то это рекурсия
				if (up_ctx.input == ctx.result[0].__littlelisp.body) {
						
					// переиспользуем контекст функции и вернём его
					up_ctx.scope = new_scope;
					up_ctx.indx = 0;
					up_ctx.result = [];
					return up_ctx;
				}
			}
		}
		
		// делаем вызов макроса
		return new Context(new_scope, ctx, 
			ctx.result[0].__littlelisp.body, 
			0, // ctx.indx
			ctx.result[0].__littlelisp.source, 
			ctx.result[0].__littlelisp.file, 
			16 + 64 /* type=function+macros */);
	}
	
	// (....)
	else {
		
		// вычесляем элемент (если список, то переключаемся на него)
		if (ctx.indx >= 0 && ctx.indx < ctx.input.length-1 && ctx.indx in ctx.result == false) {
			
			// вычесляем элемент (если список, то переключаемся на него)
			if (ctx.indx in ctx.result == false && !ctx.interpret_elem())
					return ctx.result[ctx.indx];
		}
		
		// двигаемся к след.элементу списка
		if (ctx.indx+1 < ctx.input.length-1) {
			ctx.indx++;
			return ctx;
		}
		
		/* все элименты обработали */
		
		// тогда возможно это вызов функции?
		if (ctx.result[0] instanceof Object && "apply" in ctx.result[0] && ctx.result[0].apply instanceof Function) {
			
			// если вызов завершён -> возвращаем результат наверх
			if (ctx.indx == ctx.input.length-1) {
				
				// return to parent or no-parent (or debugging ctx)
				if (ctx.parent && !(ctx.type & 32)) {
					ctx.parent.result[ctx.parent.indx] = ctx.result[ctx.indx];
					ctx = ctx.parent;
					continue;
				}
				else {
					ctx.result = ctx.result[ctx.indx];
					ctx.indx = ctx.input.length;
					return ctx;
				}
			}
			
			// иначе пометим ctx заранее, что вызов был сделан
			ctx.indx = ctx.input.length-1; 
			
			// может это наша функция?
			if (ctx.result[0] instanceof Function && "__littlelisp" in ctx.result[0]) {
				var new_scope = {arguments: ctx.result.slice(1), thiz: ctx.this_for_elem1};
				var arg_names = ctx.result[0].__littlelisp.args_names;
				for(var i = 0; i < arg_names.length-1; i++)
					new_scope[ctx.result[0].__littlelisp.source.substring(arg_names[i] >> 16, 
						(arg_names[i] >> 16) + (arg_names[i] & 32767))] 
						= ctx.result[i+1];
				
				// Оптимизация хвостовой рекурсии.
				for (var up_ctx = ctx; up_ctx; up_ctx = up_ctx.parent) {
					
					// если не в хвосте, то прервём выполнение
					if (up_ctx.indx < up_ctx.input.length-2) break;
					
					// в хвосте функции?
					if (up_ctx.type & 16) {
						
						// если тело одинаковое, то это рекурсия
						if (up_ctx.input == ctx.result[0].__littlelisp.body) {
							
							// переиспользуем контекст функции и вернём его
							up_ctx.scope = new_scope;
							up_ctx.indx = 0;
							up_ctx.result = [];
							return up_ctx;
						}
					}
				}
						
				return new Context(new_scope, ctx, 
								ctx.result[0].__littlelisp.body, 
								0, // ctx.indx
								ctx.result[0].__littlelisp.source, 
								ctx.result[0].__littlelisp.file, 
								16 /* type=function */);
			}
			
			// иначе простая js-функция
			ctx.result[ctx.indx] = ctx.result[0].apply(ctx.this_for_elem1, ctx.result.slice(1));
			continue;
		}
		
		// иначе наверх ^|
		else if (ctx.parent && !(ctx.type & 32)) {
			ctx.parent.result[ctx.parent.indx] = (ctx.type & 16) 
				? ctx.result.pop() // в функции вернём последнее вычесленное значение
				: ctx.result; 
			ctx = ctx.parent;
			continue;
		}
		
		// выше некуда - finish
		else {
			ctx.result = (ctx.type & 16) ? ctx.result.pop() : ctx.result;
			ctx.indx = ctx.input.length;
			return ctx;
		}
	}

	} 
	catch(ex) {
		
		// найдём ближайший контекст catch
		var catch_ctx_found = false;
		for (var catch_ctx = ctx; catch_ctx; catch_ctx = catch_ctx.parent) {
			if (catch_ctx.type & 32 /* nested_debugging */) 
				break;
			if (catch_ctx && catch_ctx.type & 2 /* catch */) {
				catch_ctx.result = ex;
				ctx = catch_ctx;
				catch_ctx_found = true;
				break;
			}
		}
		
		// rethrow if not found a catch ctx
		if (catch_ctx_found == false) {
			console.info(ex.stack);
			throw ex;
		}
		
		// return result to parent/no-parent
		if (ctx.parent && !(ctx.type & 32)) {
			ctx.parent.result[ctx.parent.indx] = ctx.result;
			ctx = ctx.parent;
			continue;
		}
		else {
			ctx.indx = ctx.input.length;
			return ctx;
		}
		
	} // while(prt_cnt)

};

var interpret = function(input, scope, source, file) {
	
	// list
	if(input instanceof Array) {
		var ctx = new Context(scope, undefined, input, undefined, source, file);
		
		while(ctx.indx < ctx.input.length) {
			ctx = ctx.interpret_list();
		}
	
		return ctx.result;
	}
	
	// atom/string/number/JSON...
	else {
		var var_ctx = new Context(scope, undefined, [input, input], 0, source, file);
		if (var_ctx.interpret_elem() == false) debugger;
		return var_ctx.result[0];
	}
}

var parse = function(source, fragment_start_p) {
	var tok_start = undefined,
		tok_len = 0,
		list_stack = [[]],
		last_close_bracket = 0;
		first_open_bracket = undefined;
	for (var p = fragment_start_p || 0; p < source.length; p++) {
	
		var val = source[p];
		
		// ;; T_COMMENT
		if (val == ';' && source.length > p+1/* && source[p+1] == ';'*/) {
			while (source[p] != '\n' && p+1 < source.length) p++;
			continue;
		}
		
		// // T_COMMENT
		if (val == '/' && source.length > p+1 && source[p+1] == '/') {
			while (source[p] != '\n' && p+1 < source.length) p++;
			continue;
		}
		
		// /* T_COMMENT */
		if (val == '/' && source.length > p+1 && source[p+1] == '*') {
			for (p++; p < source.length; p++)
				if (source[p] == '/' && source[p-1] == '*') { 
					p++; // skip '/'
					break; 
				}
  
			continue;
		}
		
		// T_STRING
		if (val == '"') {
			
			// выделяем всю строку
			tok_start = p;
			for (p++; p < source.length; p++) 
				if (source[p] == '"' && source[p-1] != '\\') break;
			tok_len = p - tok_start + 1; // +1 закрывающая кавычка
			
			if (fragment_start_p == undefined)
				list_stack[list_stack.length-1].push((tok_start << 16) + tok_len);
			
			continue;
		}
		
		// T_SPACE: пробелы
		if (" \n\r\t".indexOf(val) > -1) {
			for (p++; p < source.length; p++) 
				if (" \n\r\t".indexOf(source[p]) < 0) { 
					p--; // for(;;p++)
					break;
				}
			
			continue;
		}
		
		// '(...)
		if ((val == '`' || val == "'") && p+1 < source.length && source[p+1] == '(') {
			if (first_open_bracket === undefined) first_open_bracket = p;
				
			var new_list = [(p << 16) + 1, (p << 16) + 2];
			list_stack[list_stack.length-1].push(new_list);
			list_stack.push(new_list);
			
			p++; // '(
			continue;
		}
		
		// (...)
		if (val == '(') {
			if (first_open_bracket === undefined) first_open_bracket = p;
			
			var new_list = [(p << 16) + 1];
			list_stack[list_stack.length-1].push(new_list);
			list_stack.push(new_list);
			continue;
		}
		if (val == ')') {
			// переносим число с началом и длиной списка в конец списка
			var start_list = list_stack[list_stack.length-1].shift();
			list_stack[list_stack.length-1].push(start_list + (p - (start_list >> 16)));
			
			// лишняя закрывающая скобка!
			if(list_stack.length == 1) {
				throw new Error("Еxtra bracket detected -> " + source.substring(p, Math.min(source.length, p+200)));
			}
			
			// убираем из стека
			list_stack.pop();
			last_close_bracket = p;
			
			// если поверхностный парсинг запросили, то только списки выделяем
			if (fragment_start_p != undefined && list_stack.length == 1) 
				break;
			
			continue;
		}
		
		// {...}[...] - embedded JSON
		if (val == '{' || val == '[') {
			
			tok_start = p;
			var embed_stack = [val == '[' ? 11 : 13], embed_top = 0;
			for (p++; p < source.length; p++) {
				if ((source[p] == ']' || source[p] == '}') && embed_top == 0) {
					break;
				}
				else if (source[p] == ']' && embed_stack[embed_top] == 11) 
					embed_top--;
				else if (source[p] == '[' && embed_stack[embed_top] > 10)
					embed_stack[++embed_top] = 11;
				else if (source[p] == '}' && embed_stack[embed_top] == 13) 
					embed_top--;
				else if (source[p] == '{' && embed_stack[embed_top] > 10)
					embed_stack[++embed_top] = 13;
				else if (source[p] == "'" && embed_stack[embed_top] == 3)
					embed_top--;
				else if (source[p] == "'" && embed_stack[embed_top] > 10)
					embed_stack[++embed_top] = 3;
				else if (source[p] == '\\' && p < source.length-1 && source[p+1] == "'" && embed_stack[embed_top] == 3)
					p++;
				else if (source[p] == '"' && embed_stack[embed_top] == 2)
					embed_top--;
				else if (source[p] == '"' && embed_stack[embed_top] > 10)
					embed_stack[++embed_top] = 2;
				else if (source[p] == '\\' && p < source.length-1 && source[p+1] == '"' && embed_stack[embed_top] == 2)
					p++;
				else if (source[p] == '/' && p < source.length-1 && source[p+1] == '*' && embed_stack[embed_top] > 10)
					embed_stack[++embed_top] = 4;
				else if (source[p] == '*' && p < source.length-1 && source[p+1] == '/' && embed_stack[embed_top] == 4) {
					embed_top--;
					p++;
				}
				else if (source[p] == '/'&& p < source.length-1 && source[p] == '/' && embed_stack[embed_top] > 10)
					while(p < source.length && source[p] != '\n') p++;
			}
			
			tok_len = p - tok_start + 1;
			list_stack[list_stack.length-1].push((tok_start << 16) + tok_len);
			
			continue;
		}
		
		// если поверхностный парсинг запросили, то только списки выделяем
		if (fragment_start_p != undefined) { 
			if (list_stack.length == 1) 
				break;
			else 
				continue;
		}
		
		// T_NUMBER: числа
		if ("0123456789".indexOf(val) > -1 
		|| (val == '-' && p+1 < source.length && "0123456789".indexOf(source[p+1]) > -1)) {
			
			// выделим число
			tok_start = p; var float_flag = 0;
			for (p++; p < source.length; p++) 
				if ("0123456789.".indexOf(source[p]) < 0) {
					p--; // for(;;p++)
					break;
				}
				else
					// Float?
					if (float_flag == 0 && source[p] == '.') 
						float_flag = 1 << 15;
					
			tok_len = p - tok_start + 1;
			list_stack[list_stack.length-1].push((tok_start << 16) + float_flag + tok_len);
			
			continue;
		}
		
		// T_ATOM: новый атом
		if (atom_alphabet.indexOf(val) > -1) {
			tok_start = p; var dot_detected_flag = 0;
			for (p++; p < source.length; p++) 
				if (atom_alphabet.indexOf(source[p]) < 0 && "0123456789-".indexOf(source[p]) < 0) {
					p--; // for(;;p++)
					break;
				}
				else if (dot_detected_flag == 0 && source[p] == '.')
					dot_detected_flag = 1 << 15;
			
			// итоговая длина токена получилась
			tok_len = p - tok_start + 1;
			
			// (синтаксический сахар) если арефметический знак на втором месте, то переставим его на первое место
			if (list_stack[list_stack.length-1].length == (list_stack.length == 1 ? 1 : 2)
			&& "+-*/%<>!=&|".indexOf(source[tok_start]) > -1 
			&& (tok_len < 2 || source[tok_start] != '&' || "+-*/%<>!=&|".indexOf(source[tok_start+1]) > -1) /* exclude &keywords */) {
				list_stack[list_stack.length-1].push(list_stack[list_stack.length-1][(list_stack.length == 1 ? 0 : 1)]);
				list_stack[list_stack.length-1][(list_stack.length == 1 ? 0 : 1)] = (tok_start << 16) + tok_len;
			}
			else {
				list_stack[list_stack.length-1].push((tok_start << 16) + tok_len + dot_detected_flag);
			}
			
			continue;
		}
		
		// что это?
		debugger;
		throw new Error("???");
	}
	
	// весь исходник целиком выделим под конец
	list_stack[0].push((first_open_bracket << 16) + last_close_bracket+1);
	
	// если потеряли хоть одну незакрытую скобку, то список не закроется
	if (list_stack.length > 1) {
		throw new Error("No "+(list_stack.length-1)+" closing brackets found at the end!");
	}
	
	// если распарсили только один токен (atom/string/number...), то его и вернём
	if (list_stack[0].length == 2)
		return list_stack[0][0];
	else 
		return list_stack[0];
};

/* Debugger */

var breakpoints = {};
var debuggers = [];

var Debugger = function(source_or_ctx, file, parent_ctx, onerror_callback) {
	this.stop_now = false;
	this.onerror = onerror_callback;
	
	if (source_or_ctx instanceof Context)
		this.ctx = source_or_ctx;
	else
		this.ctx = new Context(parent_ctx ? parent_ctx.scope : window, parent_ctx, parse(source_or_ctx), undefined, source_or_ctx, file, 32 /* debugging_type */);

	debuggers.push(this); // добавим себя в список существующих дебагеров
}

Debugger.prototype.step_in = function() {

	if (this.ctx.indx < this.ctx.input.length)
		this.ctx = this.ctx.interpret_list();
	else
		alert("Evaluting is finished!");
};

Debugger.prototype.step_over = function(breakpoints_enabled) {
	
	var ctx_saved = this.ctx;
	
	if (this.ctx.indx >= this.ctx.input.length && !this.ctx.parent)
		alert("Evaluting is finished!");
	
	if (this.stop_now) this.stop_now = false; // отключим если включено
	
	while (this.ctx.indx < this.ctx.input.length && !this.stop_now) {
		
		// breakpoints
		if (breakpoints_enabled && this.ctx.file in breakpoints 
		&& breakpoints[this.ctx.file][this.ctx.input[this.ctx.indx]]) {
			if (this.onerror) this.onerror({type: "debugger.breakpoint", debugger: this});
			throw "Breakpoint!";
		}
		
		this.ctx = this.ctx.interpret_list(undefined, ctx_saved);
		
		// либо на одном уровни, либо мы уже ниже
		for (var curr_ctx = ctx_saved; curr_ctx; curr_ctx = curr_ctx.parent) {
			if (curr_ctx == this.ctx) return;
		}
	}
}

Debugger.prototype.step_out = function(breakpoints_enabled) {
	var ctx_saved = this.ctx.parent || this.ctx;
	
	if (this.ctx.indx >= this.ctx.input.length && !this.ctx.parent)
		alert("Evaluting is finished!");
	
	if (this.stop_now) this.stop_now = false; // отключим если включено
	
	while (this.ctx.indx < this.ctx.input.length && !this.stop_now) {
		
		// breakpoints
		if (breakpoints_enabled && this.ctx.file in breakpoints 
		&& breakpoints[this.ctx.file][this.ctx.input[this.ctx.indx]]) {
			if (this.onerror) this.onerror({type: "debugger.breakpoint", debugger: this});
			throw "Breakpoint!";
		}
		
		this.ctx = this.ctx.interpret_list(false);
		
		for (var curr_ctx = ctx_saved;; curr_ctx = curr_ctx.parent) {
			if (this.ctx == curr_ctx) return; // stop ctx
			if (curr_ctx.parent) curr_ctx = curr_ctx.parent;
			else break;
		}
	}
}

Debugger.prototype.continue = function(skip_one_breakpoint) {

	if (this.ctx.indx >= this.ctx.input.length && !this.ctx.parent)
		alert("Evaluting is finished!");
	
	if (this.stop_now) this.stop_now = false; // отключим если включено
	
	if (this.ctx.input instanceof Array)
	while (this.ctx.indx < this.ctx.input.length && !this.stop_now) {
		
		// breakpoints
		if (this.ctx.file in breakpoints 
		&& breakpoints[this.ctx.file][this.ctx.input[this.ctx.indx]]
		&& !skip_one_breakpoint) {
			if (this.onerror) this.onerror({type: "debugger.breakpoint", debugger: this});
			throw "Breakpoint!";
		}
		
		try {
			this.ctx = this.ctx.interpret_list(true);
		} catch (ex) {
			if (this.onerror) this.onerror({type: "debugger.exception", debugger: this, ex: ex});
			alert(ex.stack || ex);
			console.info(ex.stack || ex);
			throw ex;
		}
		
		// сбрасываем подавление breakpoint на первом шаге, если надо
		if (skip_one_breakpoint) skip_one_breakpoint = false;
	}
	
	// если передали скаляр
	else try {
		this.ctx.input = [this.ctx.input];
		this.ctx.indx = 0;
		this.ctx.interpret_elem();
		this.ctx.result = this.ctx.result[0];
	}
	catch (ex) {
		console.info(ex.stack);
		
		if (!this.in_debuggers) 
			this.in_debuggers = debuggers.push(this), true;
		
		if (this.onerror) this.onerror({type: "debugger.exception", debugger: this, ex: ex});
		
		throw ex;
	}
	
	return this.ctx.result;
};

exports.littleLisp = {
	Context: Context,
	Debugger: Debugger,
	parse: parse,
	interpret: interpret,
	eval: function(source, scope, file) { return interpret(parse(source), scope || window, source, file); },
	debug: function(source, file, parent_ctx, onerror_callback) { return new Debugger(source, file, parent_ctx, window["littlelisp_ide_onclick"] ?  littlelisp_ide_onclick : onerror_callback); },
	breakpoints: breakpoints,
	debuggers: debuggers,
  	debuggersStopNow: function() { for (var i in debuggers) debuggers[i].stop_now = true; },
	gensymCounter: gensym_counter
};
})(typeof exports === 'undefined' ? this : exports);
