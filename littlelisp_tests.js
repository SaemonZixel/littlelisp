littleLisp = require("./littlelisp").littleLisp;

window = global;

/* ========== PARSE =========== */

console.log("\n\033[1m%s\033[0m\n", test = "(defun String.prototype.test1 (a1) (alert (this + a1)))\n(document.title.test1 \"123\")");
result = littleLisp.parse(test); // console.log(JSON.stringify(result));
expect = [[65541,491542,[2031618,1966084],[2359301,[3145729,2818052,3276802,2752523],2293779],55],[3768340,5111813,3670044],84];
console.assert(result == expect.toString(), result);

test = "(abc \n	(qwf";
console.log("\n\033[1m%s\033[0m\n", test);
try {
result = littleLisp.parse(test); // console.log(JSON.stringify(result));
} catch(ex) { result = ex; }
console.assert(result == "Error: No 2 closing brackets found at the end!", result);

// Parse "ambda"
test = "(setq test1 (lambda (x y) (alert x)))";
console.log("\n\033[1m%s\033[0m\n", test);
result = littleLisp.parse(test); // console.log(JSON.stringify(result));
expect = [65540,393221,[851974,[1376257,1507329,1310725],[1769477,2162689,1703945],786456],37];
console.assert(result == expect.toString(), result);

console.log("\n\033[1m%s\033[0m\n", test = "(1 - 2)");
result = littleLisp.parse(test); // console.log(JSON.stringify(result));
expect = [196609, 65537, 327681,7];
console.assert(result == expect.toString(), result);

console.log("\n\033[1m%s\033[0m\n", test = "(setq arg1 -1)");
result = littleLisp.parse(test); // console.log(JSON.stringify(result));
expect = [ 65540, 393220, 720898, 14 ];
console.assert(result == expect.toString(), result);

console.log("\n\033[1m%s\033[0m\n", test = "(1 / 0)");
result = littleLisp.parse(test); // console.log(JSON.stringify(result));
expect = [ 196609, 65537, 327681, 7 ];
console.assert(result == expect.toString(), result);

console.log("\n\033[1m%s\033[0m\n", test = "(num ++)");
result = littleLisp.parse(test); // console.log(JSON.stringify(result));
expect = [ 327682, 65539, 8 ];
console.assert(result == expect.toString(), result);

console.log("\n\033[1m%s\033[0m\n", test = "(num &num)");
result = littleLisp.parse(test); // console.log(JSON.stringify(result));
expect = [ 65539, 327684, 10 ];
console.assert(result == expect.toString(), result);

console.log("\n\033[1m%s\033[0m\n", test = "(curr_time > (parseInt gt_then)) || (curr_time_plus_12 < (parseInt gt_then))");
result = littleLisp.parse(test); console.log(JSON.stringify(result));
expect = [2162690,[720897,65545,[917512,1507335,851986],32],[3604481,2424849,[3801096,4390919,3735570],2359336],76];
console.assert(result == expect.toString(), result);

// TODO (++i)

/* ========== EVAL =========== */

console.log("\n\033[1m%s\033[0m\n", test = "(\"123\\n321\")");
arg1 = true;
result = littleLisp.eval(test);
console.assert(result[0] == "123\n321", result);

console.log("\n\033[1m%s\033[0m\n", test = "'(Шаблон_буквы_А \n	(состоит_из \"20 точек\")\n)\n\n'(Точка_1\n	(abc)\n)");
result = littleLisp.eval(test); // console.log(JSON.stringify(result));
expect = [["Шаблон_буквы_А",["состоит_из","20 точек"]],["Точка_1",["abc"]]];
console.assert(result == expect.toString(), result);


console.log("\n\033[1m%s\033[0m\n", test = "(\n		0 255 255 0\n		255 0 0 255\n		255 255 255 255\n		255 0 0 255)\n");
result = littleLisp.eval(test); // console.log(JSON.stringify(result));
expect = [0,255,255,0,255,0,0,255,255,255,255,255,255,0,0,255];
console.assert(result == expect.toString(), result);

console.log("\n\033[1m%s\033[0m\n", test = "`(abc 111 ,arg1 222 ,@arg2 \"str\" (:abc 333 ,arg1 444 ,@arg2 @arg1) 'cde ,arg2 arg1)\n");
arg1 = 999;
arg2 = [1, 2, 3];
result = littleLisp.eval(test); // console.log(JSON.stringify(result));
expect = ["abc", 111, 999, 222, 1, 2, 3, "str", ["abc", 333, 999, 444, 1, 2, 3, "arg1"], "cde", [1,2,3], "arg1"];
console.assert(result == expect.toString(), result);


console.log("\n\033[1m%s\033[0m\n", test = "(typeof true)");
result = littleLisp.eval(test); // console.log(JSON.stringify(result));
console.assert(result == "boolean", result);

console.log("\n\033[1m%s\033[0m\n", test = "(typeof false)");
console.assert(littleLisp.eval(test) == "boolean");

console.log("\n\033[1m%s\033[0m\n", test = "(typeof null)");
console.assert(littleLisp.eval(test) == "object");

console.log("\n\033[1m%s\033[0m\n", test = "(typeof :abc)");
console.assert(littleLisp.eval(test) == "atom");

console.log("\n\033[1m%s\033[0m\n", test = "(typeof (1 2 3))");
console.assert(littleLisp.eval(test) == "object");

// console.log("\n\033[1m%s\033[0m\n", test = "(typeof ++)");
// console.assert(littleLisp.eval(test) == "object");

console.log("\n\033[1m%s\033[0m\n", test = "(obj1.a)");
obj1 = {a: 1};
result = littleLisp.eval(test); // console.log(JSON.stringify(result));
console.assert(result == 1, result);

console.log("\n\033[1m%s\033[0m\n", test = "((gensym) (gensym 'tmp))");
result = littleLisp.eval(test); // console.log(JSON.stringify(result));
console.assert(result.toString() == "GTMP1,tmp2", result);

console.log("\n\033[1m%s\033[0m\n", test = "(\"word\" + (obj1.a))");
obj1 = {a: 1};
result = littleLisp.eval(test); // console.log(JSON.stringify(result));
console.assert(result == "word1", result);


console.log("\n\033[1m%s\033[0m\n", test = "(obj1.a - 1)");
obj1 = {a: 9};
result = littleLisp.eval(test); // console.log(JSON.stringify(result));
console.assert(result == 8, result);

console.log("\n\033[1m%s\033[0m\n", test = "(++ arg1 obj1.a arg2 arg2)");
obj1 = {a: 1};
arg1 = "5";
arg2 = 9;
result = littleLisp.eval(test);
console.assert(result == 11, result);
console.assert(obj1.a == 2, "obj1.a = %s", obj1.a);
console.assert(arg1 == 6, "arg1 = %s", arg1);
console.assert(arg2 == 11, "arg2 = %s", arg2);

console.log("\n\033[1m%s\033[0m\n", test = "(++ arg1 arg2 arg1 arg2)");
arg1 = "5";
arg2 = 9;
result = littleLisp.eval(test);
console.assert(result == 11, result);
console.assert(arg1 == 7, "arg1 = %s", arg1);
console.assert(arg2 == 11, "arg2 = %s", arg2);

console.log("\n\033[1m%s\033[0m\n", test = "(num ++)");
num = 9;
result = littleLisp.eval(test);
console.assert(result == 10, result);
console.assert(num == 10, "num = %s", num);

console.log("\n\033[1m%s\033[0m\n", test = "(num ++)");
num = 9.9;
result = littleLisp.eval(test);
console.assert(result == 10.9, result);
console.assert(num == 10.9, "num = %s", num);

console.log("\n\033[1m%s\033[0m\n", test = "(! arg1)");
arg1 = "abc";
result = littleLisp.eval(test);
console.assert(result == false, result);
console.assert(arg1 == "abc", "arg1 = %s", arg1);

console.log("\n\033[1m%s\033[0m\n", test = "(((curr_time > (parseInt gt_then)) || (curr_time_plus_12 < (parseInt gt_then))))");
curr_time = 2;
curr_time_plus_12 = 14;
gt_then = "21";
result = littleLisp.eval(test);
console.assert(result[0] == true, result);

console.log("\n\033[1m%s\033[0m\n", test = "(curr_time > (parseInt gt_then)) || (curr_time_plus_12 < (parseInt gt_then))");
curr_time = 2;
curr_time_plus_12 = 14;
gt_then = "21";
result = littleLisp.eval(test);
console.assert(result == true, result);

console.log("\n\033[1m%s\033[0m\n", test = "(setq arg1 7)");
arg1 = 5;
result = littleLisp.eval(test); // console.log(JSON.stringify(result));
console.assert(arg1 == 7, arg1);

console.log("\n\033[1m%s\033[0m\n", test = "(setq arg1 -1)");
arg1 = 5;
result = littleLisp.eval(test); // console.log(JSON.stringify(result));
console.assert(arg1 == -1, arg1);


console.log("\n\033[1m%s\033[0m\n", test = "(setq arg1 \"b\" obj1.a 123 obj1.@arg1 999)");
obj1 = {a: 1};
arg1 = 5;
arg2 = 9;
result = littleLisp.eval(test);
console.assert(result == 999, result);
console.assert(obj1.a == 123, "obj1.a = %s", obj1.a);
console.assert(arg1 == "b", "arg1 = %s", arg1);
console.assert(arg2 == 9, "arg2 = %s", arg2);
console.assert(obj1.b == 999, "obj1.b = %s", obj1.b);

console.log("\n\033[1m%s\033[0m\n", test = "'(arg1 \"b\" ,obj1.a 123 {a:[1, 3, 4]} ,obj1.@arg1 -999 -10.5)");
obj1 = {a: 1};
arg1 = 5;
arg2 = 9;
result = littleLisp.eval(test);
console.assert(result.toString() == "arg1,b,1,123,[object Object],,-999,-10.5", result.toString());

console.log("\n\033[1m%s\033[0m\n", test = "(setq arg1 arg2)");
arg1 = 5;
arg2 = 9;
result = littleLisp.eval(test);
console.assert(result == 9, result);
console.assert(arg1 == 9, "arg1 = %s", arg1);
console.assert(arg2 == 9, "arg2 = %s", arg2);


console.log("\n\033[1m%s\033[0m\n", test = "(get obj1.list 1)");
obj1 = {list: ["111", "222"]}
result = littleLisp.eval(test); // console.log(JSON.stringify(result));
console.assert(result == "222", result);

console.log("\n\033[1m%s\033[0m\n", test = "(get obj1.list (obj1.list.length - 1))");
obj1 = {list: ["111", "222", "333", "444"]}
result = littleLisp.eval(test); // console.log(JSON.stringify(result));
console.assert(result == "444", result);

console.log("\n\033[1m%s\033[0m\n", test = "(while arg1 (console.log arg1) (setq arg1 undefined))");
arg1 = true;
result = littleLisp.eval(test);
console.assert(result == undefined, result);

console.log("\n\033[1m%s\033[0m\n", test = "(while arg1 (setq arg1 123) (break))");
arg1 = true;
result = littleLisp.eval(test);
console.assert(result[2] == 123, result);

console.log("\n\033[1m%s\033[0m\n", test = "(while (arg1 < 10) (arg1 ++) (continue) (setq arg1 999))");
arg1 = 0;
result = littleLisp.eval(test);
console.assert(arg1 == 10, arg1);

console.log("\n\033[1m%s\033[0m\n", test = "(while (arg1 < 10) (arg1 ++) (arg1 + 1))");
arg1 = 0;
result = littleLisp.eval(test);
console.assert(result == 11, result);
console.assert(arg1 == 10, arg1);


// TODO
// console.log("\n\033[1m%s\033[0m\n", test = "(while (arg1) (console.log arg1) (setq arg1 undefined))");
// arg1 = true;
// result = littleLisp.eval(test);
// console.assert(result == undefined, result);

console.log("\n\033[1m%s\033[0m\n", test = "(if arg1 (setq arg1 'error) (setq arg1 false) :else (setq arg1 123))");
arg1 = true;
result = littleLisp.eval(test);
console.assert(result == false, result);

console.log("\n\033[1m%s\033[0m\n", test = "(if (! id) (+ id 123) (return))");
result = littleLisp.eval(test); // console.log(JSON.stringify(result));
console.assert(result === undefined, result);

console.log("\n\033[1m%s\033[0m\n", test = "(if (! id) (+ id 123) (return 999))");
result = littleLisp.eval(test); // console.log(JSON.stringify(result));
console.assert(result == 999, result);

console.log("\n\033[1m%s\033[0m\n", test = "(if (arg1 && (obj1.@arg1)) (123) :else (999))");
arg1 = undefined;
result = littleLisp.eval(test); // console.info(typeof result);
console.assert(result instanceof Array , result);
console.assert(result[0] == 999, result);

console.log("\n\033[1m%s\033[0m\n", test = "(if (curr_time > 12) (curr_time - 12) :else (curr_time + 12))");
curr_time = 2;
result = littleLisp.eval(test); // console.info(typeof result);
console.assert(result == 14, result);

console.log("\n\033[1m%s\033[0m\n", test = "(if (curr_time > 12) :else (curr_time + 12))");
curr_time = 2;
result = littleLisp.eval(test); // console.info(typeof result);
console.assert(result == 14, result);

console.log("\n\033[1m%s\033[0m\n", test = "(if (curr_time > 12) :elseif (curr_time == 2) 15 :else (curr_time + 12))");
curr_time = 2;
result = littleLisp.eval(test); // console.info(typeof result);
console.assert(result == 15, result);

console.log("\n\033[1m%s\033[0m\n", test = "(if (curr_time > 12) \n ; TODO \n)");
curr_time = 2;
result = littleLisp.eval(test); // console.info(result.toString());
console.assert(typeof result == "undefined", result);

console.log("\n\033[1m%s\033[0m\n", test = "(if (curr_time > 12) \n ; TODO \n:else \n ;TODO \n)");
curr_time = 2;
result = littleLisp.eval(test); // console.info(result.toString());
console.assert(typeof result == "undefined", result);

console.log("\n\033[1m%s\033[0m\n", test = "(if (curr_time > 12) \n ; TODO \n:elseif (curr_time == 2) \n ;TODO \n :else \n ;TODO \n)");
curr_time = 2;
result = littleLisp.eval(test); // console.info(result.toString());
console.assert(typeof result == "undefined", result);

console.log("\n\033[1m%s\033[0m\n", test = "(catch (a.b))");
result = littleLisp.eval(test); // console.info(typeof result);
console.assert(result == "Error: \"a\" is undefined!", result);

console.log("\n\033[1m%s\033[0m\n", test = "(throw 'exception1)");
try {
	result = littleLisp.eval(test); // console.info(typeof result);
} catch(ex) { 
	result = ex; 
}
console.assert(result == "exception1", result);


console.log("\n\033[1m%s\033[0m\n", test = "(unsetq obj1.a obj1.@arg1 arg1)");
obj1.a = 1;
obj1.b = 2;
arg1 = "b";
result = littleLisp.eval(test); // console.info(typeof result);
console.assert(result == undefined, result);
console.assert("a" in obj1 == false, obj1);
console.assert("b" in obj1 == false, obj1);
console.assert(typeof arg1 == "undefined", window["arg1"]);

console.log("\n\033[1m%s\033[0m\n", test = "(defun test1 (a) (return a)) (test1 0)");
delete test1;
result = littleLisp.eval(test); // console.info(result);
console.assert(result[1] == 0, result);

console.log("\n\033[1m%s\033[0m\n", test = "(defun test1 (a) (return 123 a)) (test1 0)");
delete test1;
result = littleLisp.eval(test); // console.info(result);
console.assert(result[1] == 123, result);

console.log("\n\033[1m%s\033[0m\n", test = "(defun test1 (a) 123 a) (test1 999)");
delete test1;
result = littleLisp.eval(test); // console.info(result);
console.assert(result.toString() == ",999", result);

// TCO test
console.log("\n\033[1m%s\033[0m\n", test = "(defun test1 (a) (if (a > 30) (throw 'break)) (test1 (a + 1))) (test1 0)");
delete test1;
var debugger1 = littleLisp.debug(test);
try {
	result = debugger1.continue(); 
} catch(ex) { /* skip */ }
// console.info(debugger1.ctx.type, debugger1.ctx.parent.type, debugger1.ctx.parent.parent.type, debugger1.ctx.parent.parent.parent.type, debugger1.ctx.parent.parent.parent.parent.type);
console.assert(debugger1.ctx.parent.parent.parent.parent.type == 32, debugger1.ctx.parent.parent.parent.parent);
console.assert(debugger1.ctx.parent.parent.type, debugger1.ctx.parent.parent);

console.log("\n\033[1m%s\033[0m\n", test = "(let ((a 123)) (setq window.arg1 a))");
delete arg1;
result = littleLisp.eval(test); // console.info(result);
console.assert(result == "123", result);
console.assert(arg1 == 123, result);

console.log("\n\033[1m%s\033[0m\n", test = "(setq window.arg1 [\"a\\\"bc\", 'a\\\'bc', 123, -1, {a:1, b:[1, 2, 3]}, /* function test ]}) */ function(){ alert({a:9}); }, null, undefined])");
delete arg1;
result = littleLisp.eval(test); // console.info(result.toString());
console.assert(result.toString() == "a\"bc,a'bc,123,-1,[object Object],function (){ alert({a:9}); },,", result);
console.assert(JSON.stringify(arg1) == '["a\\"bc","a\'bc",123,-1,{"a":1,"b":[1,2,3]},null,null,null]', JSON.stringify(arg1));

console.log("\n\033[1m%s\033[0m\n", test = "{}");
result = littleLisp.eval(test); // console.info(result.toString());
console.assert(JSON.stringify(result) == "{}", result);

console.log("\n\033[1m%s\033[0m\n", test = "[]");
result = littleLisp.eval(test); // console.info(result.toString());
console.assert(JSON.stringify(result) == "[]", result);

console.log("\n\033[1m%s\033[0m\n", test = "([])");
result = littleLisp.eval(test); // console.info(result.toString());
console.assert(JSON.stringify(result) == "[[]]", result);

console.log("\n\033[1m%s\033[0m\n", test = "({})");
result = littleLisp.eval(test); // console.info(result.toString());
console.assert(JSON.stringify(result) == "[{}]", result);

console.log("\n\033[1m%s\033[0m\n", test = "(defmacro macro1 (a b) `(,a ,b)) (macro1 arg1 123)");
delete macro1;
arg1 = 999;
result = littleLisp.eval(test); // console.info(JSON.stringify(result));
console.assert(JSON.stringify(result) == "[null,[999,123]]", result);
console.assert(arg1 == 999, "arg1 = %s", arg1||null);