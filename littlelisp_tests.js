littleLisp = require("./littlelisp").littleLisp;

window = global;

/* ========== PARSE =========== */

console.log("\n\033[1m%s\033[0m\n", test = "(defun String.prototype.test1 (a1) (alert (this + a1)))\n(document.title.test1 \"123\")");
result = littleLisp.parse(test); // console.log(JSON.stringify(result));
expect = [[65541,458774,[2031618,1966084],[2359301,[3145729,2818052,3276802,2752523],2293779],55],[3735572,5111813,3670044],84];
console.assert(result == expect.toString(), result);

test = "(abc \n	(qwf";
console.log("\n\033[1m%s\033[0m\n", test);
try {
result = littleLisp.parse(test); // console.log(JSON.stringify(result));
} catch(ex) { result = ex; }
console.assert(result == "Error: No 3 closing brackets found at the end!", result);

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

console.log("\n\033[1m%s\033[0m\n", test = "(obj1.a)");
obj1 = {a: 1};
result = littleLisp.eval(test); // console.log(JSON.stringify(result));
console.assert(result == 1, result);

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
arg1 = 5;
arg2 = 9;
result = littleLisp.eval(test);
console.assert(result == 11, result);
console.assert(obj1.a == 2, "obj1.a = %s", obj1.a);
console.assert(arg1 == 6, "arg1 = %s", arg1);
console.assert(arg2 == 11, "arg2 = %s", arg2);

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

console.log("\n\033[1m%s\033[0m\n", test = "(catch (a.b))");
result = littleLisp.eval(test); // console.info(typeof result);
console.assert(result == "Error: \"a\" is undefined!", result);