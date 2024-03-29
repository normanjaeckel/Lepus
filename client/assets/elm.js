(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.an.P === region.ay.P)
	{
		return 'on line ' + region.an.P;
	}
	return 'on lines ' + region.an.P + ' through ' + region.ay.P;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bl,
		impl.bF,
		impl.bB,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		B: func(record.B),
		ao: record.ao,
		ak: record.ak
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.B;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.ao;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.ak) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bl,
		impl.bF,
		impl.bB,
		function(sendToApp, initialModel) {
			var view = impl.bG;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bl,
		impl.bF,
		impl.bB,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.al && impl.al(sendToApp)
			var view = impl.bG;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.a8);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.bE) && (_VirtualDom_doc.title = title = doc.bE);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.bv;
	var onUrlRequest = impl.bw;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		al: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.aR === next.aR
							&& curr.aE === next.aE
							&& curr.aO.a === next.aO.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		bl: function(flags)
		{
			return A3(impl.bl, flags, _Browser_getUrl(), key);
		},
		bG: impl.bG,
		bF: impl.bF,
		bB: impl.bB
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { bj: 'hidden', ba: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { bj: 'mozHidden', ba: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { bj: 'msHidden', ba: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { bj: 'webkitHidden', ba: 'webkitvisibilitychange' }
		: { bj: 'hidden', ba: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		aX: _Browser_getScene(),
		a1: {
			a3: _Browser_window.pageXOffset,
			a4: _Browser_window.pageYOffset,
			a2: _Browser_doc.documentElement.clientWidth,
			aD: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		a2: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		aD: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			aX: {
				a2: node.scrollWidth,
				aD: node.scrollHeight
			},
			a1: {
				a3: node.scrollLeft,
				a4: node.scrollTop,
				a2: node.clientWidth,
				aD: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			aX: _Browser_getScene(),
			a1: {
				a3: x,
				a4: y,
				a2: _Browser_doc.documentElement.clientWidth,
				aD: _Browser_doc.documentElement.clientHeight
			},
			be: {
				a3: x + rect.left,
				a4: y + rect.top,
				a2: rect.width,
				aD: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// DECODER

var _File_decoder = _Json_decodePrim(function(value) {
	// NOTE: checks if `File` exists in case this is run on node
	return (typeof File !== 'undefined' && value instanceof File)
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FILE', value);
});


// METADATA

function _File_name(file) { return file.name; }
function _File_mime(file) { return file.type; }
function _File_size(file) { return file.size; }

function _File_lastModified(file)
{
	return $elm$time$Time$millisToPosix(file.lastModified);
}


// DOWNLOAD

var _File_downloadNode;

function _File_getDownloadNode()
{
	return _File_downloadNode || (_File_downloadNode = document.createElement('a'));
}

var _File_download = F3(function(name, mime, content)
{
	return _Scheduler_binding(function(callback)
	{
		var blob = new Blob([content], {type: mime});

		// for IE10+
		if (navigator.msSaveOrOpenBlob)
		{
			navigator.msSaveOrOpenBlob(blob, name);
			return;
		}

		// for HTML5
		var node = _File_getDownloadNode();
		var objectUrl = URL.createObjectURL(blob);
		node.href = objectUrl;
		node.download = name;
		_File_click(node);
		URL.revokeObjectURL(objectUrl);
	});
});

function _File_downloadUrl(href)
{
	return _Scheduler_binding(function(callback)
	{
		var node = _File_getDownloadNode();
		node.href = href;
		node.download = '';
		node.origin === location.origin || (node.target = '_blank');
		_File_click(node);
	});
}


// IE COMPATIBILITY

function _File_makeBytesSafeForInternetExplorer(bytes)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/10
	// all other browsers can just run `new Blob([bytes])` directly with no problem
	//
	return new Uint8Array(bytes.buffer, bytes.byteOffset, bytes.byteLength);
}

function _File_click(node)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/11
	// all other browsers have MouseEvent and do not need this conditional stuff
	//
	if (typeof MouseEvent === 'function')
	{
		node.dispatchEvent(new MouseEvent('click'));
	}
	else
	{
		var event = document.createEvent('MouseEvents');
		event.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
		document.body.appendChild(node);
		node.dispatchEvent(event);
		document.body.removeChild(node);
	}
}


// UPLOAD

var _File_node;

function _File_uploadOne(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			callback(_Scheduler_succeed(event.target.files[0]));
		});
		_File_click(_File_node);
	});
}

function _File_uploadOneOrMore(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.multiple = true;
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			var elmFiles = _List_fromArray(event.target.files);
			callback(_Scheduler_succeed(_Utils_Tuple2(elmFiles.a, elmFiles.b)));
		});
		_File_click(_File_node);
	});
}


// CONTENT

function _File_toString(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsText(blob);
		return function() { reader.abort(); };
	});
}

function _File_toBytes(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(new DataView(reader.result)));
		});
		reader.readAsArrayBuffer(blob);
		return function() { reader.abort(); };
	});
}

function _File_toUrl(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsDataURL(blob);
		return function() { reader.abort(); };
	});
}

var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$GT = 2;
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.b) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.x),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.x);
		} else {
			var treeLen = builder.b * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.d) : builder.d;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.b);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.x) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.x);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{d: nodeList, b: (len / $elm$core$Array$branchFactor) | 0, x: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {aA: fragment, aE: host, aM: path, aO: port_, aR: protocol, aS: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $author$project$Main$Model = F4(
	function (classes, events, pupils, assignment) {
		return {N: assignment, A: classes, h: events, r: pupils};
	});
var $author$project$Class$Model = F3(
	function (classes, formData, formInvalid) {
		return {A: classes, O: formData, J: formInvalid};
	});
var $author$project$Class$emptyFormData = '';
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Class$decoder = A2(
	$elm$json$Json$Decode$map,
	function (l) {
		return A3(
			$author$project$Class$Model,
			$elm$core$Set$fromList(l),
			$author$project$Class$emptyFormData,
			false);
	},
	$elm$json$Json$Decode$list($elm$json$Json$Decode$string));
var $author$project$Event$Disabled = {$: 0};
var $author$project$Event$Model = F4(
	function (events, formData, formInvalid, editMode) {
		return {v: editMode, h: events, O: formData, J: formInvalid};
	});
var $author$project$Event$Obj = F3(
	function (name, capacity, internalID) {
		return {s: capacity, af: internalID, aJ: name};
	});
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$Event$decoderEvent = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (n, c) {
			return A3($author$project$Event$Obj, n, c, 0);
		}),
	A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'capacity', $elm$json$Json$Decode$int));
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Dict$fromList,
		$elm$json$Json$Decode$keyValuePairs(decoder));
};
var $author$project$Event$emptyFormData = A3($author$project$Event$Obj, '', 1, 0);
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Event$decoder = A2(
	$elm$json$Json$Decode$map,
	function (d) {
		return A3(
			$author$project$Event$Model(
				$elm$core$Dict$fromList(
					A2(
						$elm$core$List$map,
						function (_v0) {
							var k = _v0.a;
							var v = _v0.b;
							return _Utils_Tuple2(
								A2(
									$elm$core$Maybe$withDefault,
									0,
									$elm$core$String$toInt(k)),
								v);
						},
						$elm$core$Dict$toList(d)))),
			$author$project$Event$emptyFormData,
			false,
			$author$project$Event$Disabled);
	},
	$elm$json$Json$Decode$dict($author$project$Event$decoderEvent));
var $author$project$Pupil$Model = F3(
	function (pupils, formData, formInvalid) {
		return {O: formData, J: formInvalid, r: pupils};
	});
var $author$project$Pupil$Obj = F3(
	function (name, _class, choices) {
		return {_: choices, aa: _class, aJ: name};
	});
var $author$project$Pupil$Green = 0;
var $author$project$Pupil$Red = 2;
var $author$project$Pupil$Yellow = 1;
var $elm$json$Json$Decode$fail = _Json_fail;
var $author$project$Pupil$decoderChoiceType = A2(
	$elm$json$Json$Decode$andThen,
	function (t) {
		return (t === 'green') ? $elm$json$Json$Decode$succeed(0) : ((t === 'yellow') ? $elm$json$Json$Decode$succeed(1) : ((t === 'red') ? $elm$json$Json$Decode$succeed(2) : $elm$json$Json$Decode$fail('invalid choice type')));
	},
	$elm$json$Json$Decode$string);
var $author$project$Pupil$FormData = F2(
	function (names, _class) {
		return {aa: _class, X: names};
	});
var $author$project$Pupil$emptyFormData = A2($author$project$Pupil$FormData, '', '');
var $elm$json$Json$Decode$map3 = _Json_map3;
var $author$project$Pupil$decoder = A2(
	$elm$json$Json$Decode$map,
	function (p) {
		return A3($author$project$Pupil$Model, p, $author$project$Pupil$emptyFormData, false);
	},
	$elm$json$Json$Decode$list(
		A4(
			$elm$json$Json$Decode$map3,
			F3(
				function (n, c, ch) {
					return A3(
						$author$project$Pupil$Obj,
						n,
						c,
						$elm$core$Dict$fromList(
							A2(
								$elm$core$List$map,
								function (_v0) {
									var k = _v0.a;
									var v = _v0.b;
									return _Utils_Tuple2(
										A2(
											$elm$core$Maybe$withDefault,
											0,
											$elm$core$String$toInt(k)),
										v);
								},
								$elm$core$Dict$toList(ch))));
				}),
			A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
			A2($elm$json$Json$Decode$field, 'class', $elm$json$Json$Decode$string),
			A2(
				$elm$json$Json$Decode$field,
				'choices',
				$elm$json$Json$Decode$dict($author$project$Pupil$decoderChoiceType)))));
var $author$project$Assignment$Hidden = 0;
var $author$project$Assignment$Model = F2(
	function (sortBy, visibility) {
		return {am: sortBy, ar: visibility};
	});
var $author$project$Assignment$NameSort = 0;
var $author$project$Assignment$init = A2($author$project$Assignment$Model, 0, 0);
var $author$project$Main$decoder = A4(
	$elm$json$Json$Decode$map3,
	F3(
		function (c, e, p) {
			return A4($author$project$Main$Model, c, e, p, $author$project$Assignment$init);
		}),
	A2($elm$json$Json$Decode$field, 'classes', $author$project$Class$decoder),
	A2($elm$json$Json$Decode$field, 'events', $author$project$Event$decoder),
	A2($elm$json$Json$Decode$field, 'pupils', $author$project$Pupil$decoder));
var $author$project$Class$init = A3($author$project$Class$Model, $elm$core$Set$empty, '', false);
var $author$project$Event$init = A4($author$project$Event$Model, $elm$core$Dict$empty, $author$project$Event$emptyFormData, false, $author$project$Event$Disabled);
var $author$project$Pupil$init = A3($author$project$Pupil$Model, _List_Nil, $author$project$Pupil$emptyFormData, false);
var $author$project$Main$init = function (flags) {
	var _v0 = A2($elm$json$Json$Decode$decodeString, $author$project$Main$decoder, flags.ab);
	if (!_v0.$) {
		var model = _v0.a;
		return model;
	} else {
		return {N: $author$project$Assignment$init, A: $author$project$Class$init, h: $author$project$Event$init, r: $author$project$Pupil$init};
	}
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Main$AssignmentMsg = function (a) {
	return {$: 3, a: a};
};
var $author$project$Main$Flags = function (data) {
	return {ab: data};
};
var $author$project$Main$ImportLoaded = function (a) {
	return {$: 8, a: a};
};
var $author$project$Main$ImportSelected = function (a) {
	return {$: 7, a: a};
};
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $elm$file$File$Select$file = F2(
	function (mimes, toMsg) {
		return A2(
			$elm$core$Task$perform,
			toMsg,
			_File_uploadOne(mimes));
	});
var $elm$core$Platform$Cmd$map = _Platform_map;
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Class$modelToJSON = function (model) {
	return A2(
		$elm$json$Json$Encode$list,
		$elm$json$Json$Encode$string,
		$elm$core$Set$toList(model.A));
};
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$json$Json$Encode$dict = F3(
	function (toKey, toValue, dictionary) {
		return _Json_wrap(
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (key, value, obj) {
						return A3(
							_Json_addField,
							toKey(key),
							toValue(value),
							obj);
					}),
				_Json_emptyObject(0),
				dictionary));
	});
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var $author$project$Event$eventToJSON = function (e) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'name',
				$elm$json$Json$Encode$string(e.aJ)),
				_Utils_Tuple2(
				'capacity',
				$elm$json$Json$Encode$int(e.s))
			]));
};
var $author$project$Event$modelToJSON = function (model) {
	return A3($elm$json$Json$Encode$dict, $elm$core$String$fromInt, $author$project$Event$eventToJSON, model.h);
};
var $author$project$Pupil$choiceTypeToString = function (c) {
	switch (c) {
		case 0:
			return 'green';
		case 1:
			return 'yellow';
		default:
			return 'red';
	}
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $author$project$Pupil$pupilToJSON = function (p) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'name',
				$elm$json$Json$Encode$string(p.aJ)),
				_Utils_Tuple2(
				'class',
				$elm$json$Json$Encode$string(p.aa)),
				_Utils_Tuple2(
				'choices',
				A3(
					$elm$json$Json$Encode$dict,
					$elm$core$String$fromInt,
					A2($elm$core$Basics$composeR, $author$project$Pupil$choiceTypeToString, $elm$json$Json$Encode$string),
					p._))
			]));
};
var $author$project$Pupil$modelToJSON = function (model) {
	return A2($elm$json$Json$Encode$list, $author$project$Pupil$pupilToJSON, model.r);
};
var $author$project$Main$modelToJSON = function (model) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'classes',
				$author$project$Class$modelToJSON(model.A)),
				_Utils_Tuple2(
				'events',
				$author$project$Event$modelToJSON(model.h)),
				_Utils_Tuple2(
				'pupils',
				$author$project$Pupil$modelToJSON(model.r))
			]));
};
var $author$project$Main$setStorage = _Platform_outgoingPort('setStorage', $elm$json$Json$Encode$string);
var $elm$file$File$Download$string = F3(
	function (name, mime, content) {
		return A2(
			$elm$core$Task$perform,
			$elm$core$Basics$never,
			A3(_File_download, name, mime, content));
	});
var $elm$file$File$toString = _File_toString;
var $author$project$Assignment$SetVisibility = function (a) {
	return {$: 1, a: a};
};
var $author$project$Assignment$Visible = 2;
var $elm$core$Process$sleep = _Process_sleep;
var $author$project$Assignment$update = F2(
	function (msg, model) {
		if (!msg.$) {
			var s = msg.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{am: s}),
				$elm$core$Platform$Cmd$none);
		} else {
			var v = msg.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{ar: v}),
				function () {
					switch (v) {
						case 0:
							return $elm$core$Platform$Cmd$none;
						case 1:
							return A2(
								$elm$core$Task$perform,
								$author$project$Assignment$SetVisibility,
								A2(
									$elm$core$Task$andThen,
									function (_v2) {
										return $elm$core$Task$succeed(2);
									},
									$elm$core$Process$sleep(50)));
						default:
							return $elm$core$Platform$Cmd$none;
					}
				}());
		}
	});
var $author$project$Helpers$DontSetStorage = 1;
var $author$project$Helpers$SetStorage = 0;
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === -1) {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === -1) {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Set$remove = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$remove, key, dict);
	});
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$member, key, dict);
	});
var $elm$core$String$trim = _String_trim;
var $author$project$Class$validate = function (model) {
	var c = $elm$core$String$trim(model.O);
	return ((c === '') || A2($elm$core$Set$member, c, model.A)) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(c);
};
var $author$project$Class$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var s = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{O: s, J: false}),
					1);
			case 1:
				var _v1 = $author$project$Class$validate(model);
				if (!_v1.$) {
					var c = _v1.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								A: A2($elm$core$Set$insert, c, model.A),
								O: $author$project$Class$emptyFormData,
								J: false
							}),
						0);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{J: true}),
						1);
				}
			default:
				var s = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							A: A2($elm$core$Set$remove, s, model.A),
							J: false
						}),
					0);
		}
	});
var $author$project$Event$Editing = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Event$Id = $elm$core$Basics$identity;
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $author$project$Event$idToInt = function (_v0) {
	var i = _v0;
	return i;
};
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (!_v0.$) {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $author$project$Event$updateFormdata = F2(
	function (msg, formData) {
		if (!msg.$) {
			var name = msg.a;
			return _Utils_update(
				formData,
				{aJ: name});
		} else {
			var capacity = msg.a;
			return _Utils_update(
				formData,
				{s: capacity});
		}
	});
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$Event$validate = F3(
	function (nameRaw, capacity, forbiddenNames) {
		var name = $elm$core$String$trim(nameRaw);
		return (A2(
			$elm$core$List$member,
			name,
			A2($elm$core$List$cons, '', forbiddenNames)) || (capacity <= 0)) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
			A3($author$project$Event$Obj, name, capacity, 0));
	});
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Event$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var data = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							O: A2($author$project$Event$updateFormdata, data, model.O),
							J: false
						}),
					1);
			case 1:
				var _v1 = A3(
					$author$project$Event$validate,
					model.O.aJ,
					model.O.s,
					A2(
						$elm$core$List$map,
						function ($) {
							return $.aJ;
						},
						$elm$core$Dict$values(model.h)));
				if (!_v1.$) {
					var newObj = _v1.a;
					var newId = 1 + A2(
						$elm$core$Maybe$withDefault,
						0,
						$elm$core$List$maximum(
							$elm$core$Dict$keys(model.h)));
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								h: A3(
									$elm$core$Dict$insert,
									$author$project$Event$idToInt(newId),
									newObj,
									model.h),
								O: $author$project$Event$emptyFormData,
								J: false
							}),
						0);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{J: true}),
						1);
				}
			case 2:
				var eId = msg.a;
				var _v2 = A2(
					$elm$core$Dict$get,
					$author$project$Event$idToInt(eId),
					model.h);
				if (!_v2.$) {
					var obj = _v2.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								v: A2($author$project$Event$Editing, eId, obj)
							}),
						1);
				} else {
					return _Utils_Tuple2(model, 1);
				}
			case 3:
				var data = msg.a;
				var _v3 = model.v;
				if (_v3.$ === 1) {
					var eId = _v3.a;
					var _new = _v3.b;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								v: A2(
									$author$project$Event$Editing,
									eId,
									A2($author$project$Event$updateFormdata, data, _new))
							}),
						1);
				} else {
					return _Utils_Tuple2(model, 1);
				}
			case 4:
				var _v4 = model.v;
				if (_v4.$ === 1) {
					var eId = _v4.a;
					var _new = _v4.b;
					var _v5 = A3(
						$author$project$Event$validate,
						_new.aJ,
						_new.s,
						A2(
							$elm$core$List$map,
							function ($) {
								return $.aJ;
							},
							$elm$core$Dict$values(
								A2(
									$elm$core$Dict$remove,
									$author$project$Event$idToInt(eId),
									model.h))));
					if (!_v5.$) {
						var updated = _v5.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									v: $author$project$Event$Disabled,
									h: A3(
										$elm$core$Dict$update,
										$author$project$Event$idToInt(eId),
										$elm$core$Basics$always(
											$elm$core$Maybe$Just(updated)),
										model.h)
								}),
							0);
					} else {
						return _Utils_Tuple2(model, 1);
					}
				} else {
					return _Utils_Tuple2(model, 1);
				}
			case 5:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{v: $author$project$Event$Disabled}),
					1);
			default:
				var eId = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							v: $author$project$Event$Disabled,
							h: A2(
								$elm$core$Dict$remove,
								$author$project$Event$idToInt(eId),
								model.h),
							J: false
						}),
					0);
		}
	});
var $author$project$Pupil$changeChoice = F4(
	function (pupils, pupil, event, newChoiceType) {
		return A2(
			$elm$core$List$map,
			function (p) {
				if (_Utils_eq(p, pupil)) {
					var newChoices = A3(
						$elm$core$Dict$update,
						$author$project$Event$idToInt(event),
						function (_v0) {
							return $elm$core$Maybe$Just(newChoiceType);
						},
						p._);
					return _Utils_update(
						p,
						{_: newChoices});
				} else {
					return p;
				}
			},
			pupils);
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $elm$core$Basics$not = _Basics_not;
var $author$project$Pupil$savePupils = F5(
	function (model, events, cls, namesRaw, classRaw) {
		var names = A2(
			$elm$core$List$filter,
			$elm$core$Basics$neq(''),
			A2(
				$elm$core$List$map,
				$elm$core$String$trim,
				A2($elm$core$String$split, '\n', namesRaw)));
		var cl = $elm$core$String$trim(classRaw);
		if ((cl === '') || ($elm$core$List$isEmpty(names) || (!A2($elm$core$Set$member, cl, cls)))) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = function () {
				var yellowEvents = A2(
					$elm$core$Dict$map,
					F2(
						function (_v2, _v3) {
							return 1;
						}),
					events);
				var fn = F2(
					function (name, _v1) {
						var currentPupils = _v1.a;
						var e = _v1.b;
						return (e || A2(
							$elm$core$List$any,
							function (p) {
								return _Utils_eq(p.aJ, name) && _Utils_eq(p.aa, cl);
							},
							currentPupils)) ? _Utils_Tuple2(_List_Nil, true) : _Utils_Tuple2(
							_Utils_ap(
								currentPupils,
								_List_fromArray(
									[
										A3($author$project$Pupil$Obj, name, cl, yellowEvents)
									])),
							false);
					});
				return A3(
					$elm$core$List$foldl,
					fn,
					_Utils_Tuple2(model.r, false),
					names);
			}();
			var pupils = _v0.a;
			var err = _v0.b;
			return err ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(pupils);
		}
	});
var $author$project$Pupil$updateFormdata = F2(
	function (msg, formData) {
		if (!msg.$) {
			var n = msg.a;
			return _Utils_update(
				formData,
				{X: n});
		} else {
			var c = msg.a;
			return _Utils_update(
				formData,
				{aa: c});
		}
	});
var $author$project$Pupil$update = F4(
	function (msg, model, events, classes) {
		switch (msg.$) {
			case 0:
				var data = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							O: A2($author$project$Pupil$updateFormdata, data, model.O),
							J: false
						}),
					1);
			case 1:
				var _v1 = A5($author$project$Pupil$savePupils, model, events, classes, model.O.X, model.O.aa);
				if (!_v1.$) {
					var pupils = _v1.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{O: $author$project$Pupil$emptyFormData, J: false, r: pupils}),
						0);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{J: true}),
						1);
				}
			case 2:
				var pupil = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							J: false,
							r: A2(
								$elm$core$List$filter,
								$elm$core$Basics$neq(pupil),
								model.r)
						}),
					0);
			default:
				var pupil = msg.a;
				var event = msg.b;
				var choice = msg.c;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							J: false,
							r: A4($author$project$Pupil$changeChoice, model.r, pupil, event, choice)
						}),
					0);
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $author$project$Pupil$updateEvents = F2(
	function (events, model) {
		var newChoices = function (current) {
			return A6(
				$elm$core$Dict$merge,
				F3(
					function (i, _v0, r) {
						return A3($elm$core$Dict$insert, i, 1, r);
					}),
				F4(
					function (i, _v1, c, r) {
						return A3($elm$core$Dict$insert, i, c, r);
					}),
				F3(
					function (_v2, _v3, r) {
						return r;
					}),
				events,
				current,
				$elm$core$Dict$empty);
		};
		return _Utils_update(
			model,
			{
				r: A2(
					$elm$core$List$map,
					function (p) {
						return _Utils_update(
							p,
							{
								_: newChoices(p._)
							});
					},
					model.r)
			});
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		var s = function (updatedModel) {
			return _Utils_Tuple2(
				updatedModel,
				$author$project$Main$setStorage(
					A2(
						$elm$json$Json$Encode$encode,
						0,
						$author$project$Main$modelToJSON(updatedModel))));
		};
		switch (msg.$) {
			case 0:
				var innerMsg = msg.a;
				var _v1 = A2($author$project$Class$update, innerMsg, model.A);
				var classesModel = _v1.a;
				var pers = _v1.b;
				if (pers === 1) {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{A: classesModel}),
						$elm$core$Platform$Cmd$none);
				} else {
					return s(
						_Utils_update(
							model,
							{A: classesModel}));
				}
			case 1:
				var innerMsg = msg.a;
				var _v3 = A2($author$project$Event$update, innerMsg, model.h);
				var eventsModel = _v3.a;
				var pers = _v3.b;
				if (pers === 1) {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{h: eventsModel}),
						$elm$core$Platform$Cmd$none);
				} else {
					return s(
						_Utils_update(
							model,
							{
								h: eventsModel,
								r: A2($author$project$Pupil$updateEvents, eventsModel.h, model.r)
							}));
				}
			case 2:
				var innerMsg = msg.a;
				var _v5 = A4($author$project$Pupil$update, innerMsg, model.r, model.h.h, model.A.A);
				var pupilsModel = _v5.a;
				var pers = _v5.b;
				if (pers === 1) {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{r: pupilsModel}),
						$elm$core$Platform$Cmd$none);
				} else {
					return s(
						_Utils_update(
							model,
							{r: pupilsModel}));
				}
			case 3:
				var innerMsg = msg.a;
				var _v7 = A2($author$project$Assignment$update, innerMsg, model.N);
				var assignmentModel = _v7.a;
				var cmd = _v7.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{N: assignmentModel}),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$AssignmentMsg, cmd));
			case 4:
				return s(
					$author$project$Main$init(
						$author$project$Main$Flags('')));
			case 5:
				return _Utils_Tuple2(
					model,
					A3(
						$elm$file$File$Download$string,
						'export.json',
						'application/json',
						A2(
							$elm$json$Json$Encode$encode,
							4,
							$author$project$Main$modelToJSON(model))));
			case 6:
				return _Utils_Tuple2(
					model,
					A2(
						$elm$file$File$Select$file,
						_List_fromArray(
							['application/json']),
						$author$project$Main$ImportSelected));
			case 7:
				var file = msg.a;
				return _Utils_Tuple2(
					model,
					A2(
						$elm$core$Task$perform,
						$author$project$Main$ImportLoaded,
						$elm$file$File$toString(file)));
			default:
				var content = msg.a;
				return s(
					$author$project$Main$init(
						$author$project$Main$Flags(content)));
		}
	});
var $author$project$Main$ClassMsg = function (a) {
	return {$: 0, a: a};
};
var $author$project$Main$EventMsg = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$PupilMsg = function (a) {
	return {$: 2, a: a};
};
var $author$project$Main$DeleteAll = {$: 4};
var $author$project$Main$Export = {$: 5};
var $author$project$Main$ImportRequested = {$: 6};
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$html$Html$Attributes$classList = function (classes) {
	return $elm$html$Html$Attributes$class(
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2($elm$core$List$filter, $elm$core$Tuple$second, classes))));
};
var $author$project$Helpers$classes = function (s) {
	var cl = A2(
		$elm$core$List$map,
		function (c) {
			return _Utils_Tuple2(c, true);
		},
		A2($elm$core$String$split, ' ', s));
	return $elm$html$Html$Attributes$classList(cl);
};
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $author$project$Main$admin = A2(
	$elm$html$Html$div,
	_List_Nil,
	_List_fromArray(
		[
			A2(
			$elm$html$Html$h2,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id('admin'),
					$elm$html$Html$Attributes$class('nav-anchor')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Administration')
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Hier kann man die eingegebenen Daten löschen oder exportieren. Beim Import werden alle bisherigen Eingaben überschrieben.')
				])),
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$author$project$Helpers$classes('btn btn-danger'),
					$elm$html$Html$Attributes$type_('button'),
					$elm$html$Html$Events$onClick($author$project$Main$DeleteAll)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Alle Daten löschen')
				])),
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$author$project$Helpers$classes('btn btn-secondary ms-2'),
					$elm$html$Html$Attributes$type_('button'),
					$elm$html$Html$Events$onClick($author$project$Main$Export)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Export')
				])),
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$author$project$Helpers$classes('btn btn-secondary ms-2'),
					$elm$html$Html$Attributes$type_('button'),
					$elm$html$Html$Events$onClick($author$project$Main$ImportRequested)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Import')
				]))
		]));
var $elm$virtual_dom$VirtualDom$lazy = _VirtualDom_lazy;
var $elm$html$Html$Lazy$lazy = $elm$virtual_dom$VirtualDom$lazy;
var $elm$virtual_dom$VirtualDom$lazy3 = _VirtualDom_lazy3;
var $elm$html$Html$Lazy$lazy3 = $elm$virtual_dom$VirtualDom$lazy3;
var $elm$virtual_dom$VirtualDom$lazy4 = _VirtualDom_lazy4;
var $elm$html$Html$Lazy$lazy4 = $elm$virtual_dom$VirtualDom$lazy4;
var $elm$html$Html$main_ = _VirtualDom_node('main');
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$html$Html$nav = _VirtualDom_node('nav');
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Main$navbar = A2(
	$elm$html$Html$nav,
	_List_fromArray(
		[
			$author$project$Helpers$classes('navbar navbar-expand-md navbar-dark fixed-top bg-dark')
		]),
	_List_fromArray(
		[
			A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('container-fluid')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('navbar-brand'),
							$elm$html$Html$Attributes$href('#')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Home')
						])),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('navbar-toggler'),
							$elm$html$Html$Attributes$type_('button'),
							A2($elm$html$Html$Attributes$attribute, 'data-bs-toggle', 'collapse'),
							A2($elm$html$Html$Attributes$attribute, 'data-bs-target', '#navbarCollapse'),
							A2($elm$html$Html$Attributes$attribute, 'aria-controls', 'navbarCollapse'),
							A2($elm$html$Html$Attributes$attribute, 'aria-expanded', 'false'),
							A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Toggle navigation')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('navbar-toggler-icon')
								]),
							_List_Nil)
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$author$project$Helpers$classes('collapse navbar-collapse'),
							$elm$html$Html$Attributes$id('navbarCollapse')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$ul,
							_List_fromArray(
								[
									$author$project$Helpers$classes('navbar-nav me-auto mb-2 mb-md-0')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$li,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('nav-item')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$a,
											_List_fromArray(
												[
													$author$project$Helpers$classes('nav-link'),
													$elm$html$Html$Attributes$href('#classes')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text('Klassen')
												]))
										])),
									A2(
									$elm$html$Html$li,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('nav-item')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$a,
											_List_fromArray(
												[
													$author$project$Helpers$classes('nav-link'),
													$elm$html$Html$Attributes$href('#events')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text('Projektgruppen')
												]))
										])),
									A2(
									$elm$html$Html$li,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('nav-item')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$a,
											_List_fromArray(
												[
													$author$project$Helpers$classes('nav-link'),
													$elm$html$Html$Attributes$href('#pupils')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text('Schüler/Schülerinnen')
												]))
										])),
									A2(
									$elm$html$Html$li,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('nav-item')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$a,
											_List_fromArray(
												[
													$author$project$Helpers$classes('nav-link'),
													$elm$html$Html$Attributes$href('#result')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text('Ergebnis')
												]))
										])),
									A2(
									$elm$html$Html$li,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('nav-item')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$a,
											_List_fromArray(
												[
													$author$project$Helpers$classes('nav-link'),
													$elm$html$Html$Attributes$href('#admin')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text('Administration')
												]))
										]))
								]))
						]))
				]))
		]));
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $author$project$Main$readme = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			$elm$html$Html$Attributes$class('mb-5')
		]),
	_List_fromArray(
		[
			A2(
			$elm$html$Html$h1,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Projektgruppenverteilung')
				])),
			A2(
			$elm$html$Html$p,
			_List_fromArray(
				[
					$author$project$Helpers$classes('fs-5 col-md-9')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Dieses Tool speichert die Eingaben im Local Storage des Browsers. Es werden keine eigegebenen Daten über das Internet gesendet.')
				]))
		]));
var $author$project$Assignment$Loading = 1;
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Dict$filter = F2(
	function (isGood, dict) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A2(isGood, k, v) ? A3($elm$core$Dict$insert, k, v, d) : d;
				}),
			$elm$core$Dict$empty,
			dict);
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Assignment$applyMatchingToRedState = F3(
	function (events, matching, pupils) {
		return A2(
			$elm$core$List$map,
			function (pupil) {
				var _v0 = $elm$core$List$head(
					A2(
						$elm$core$List$filter,
						function (_v1) {
							var p = _v1.a;
							return _Utils_eq(pupil, p);
						},
						matching));
				if (_v0.$ === 1) {
					return pupil;
				} else {
					var _v2 = _v0.a;
					var e = _v2.b;
					var eventIdInt = A2(
						$elm$core$Maybe$withDefault,
						0,
						$elm$core$List$head(
							$elm$core$Dict$keys(
								A2(
									$elm$core$Dict$filter,
									F2(
										function (_v3, v) {
											return _Utils_eq(v, e);
										}),
									events))));
					var newChoices = A3(
						$elm$core$Dict$update,
						eventIdInt,
						$elm$core$Maybe$andThen(
							$elm$core$Basics$always(
								$elm$core$Maybe$Just(2))),
						pupil._);
					return _Utils_update(
						pupil,
						{_: newChoices});
				}
			},
			pupils);
	});
var $author$project$Assignment$EventSort = 1;
var $author$project$Assignment$SortBy = function (a) {
	return {$: 0, a: a};
};
var $author$project$Pupil$eventGroup = F2(
	function (choiceType, pupil) {
		return $elm$core$Dict$keys(
			A2(
				$elm$core$Dict$filter,
				F2(
					function (_v0, c) {
						return _Utils_eq(c, choiceType);
					}),
				pupil._));
	});
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $elm$html$Html$h4 = _VirtualDom_node('h4');
var $elm$html$Html$ol = _VirtualDom_node('ol');
var $author$project$Assignment$onColor = F3(
	function (color, events, matching) {
		var fn = function (_v0) {
			var pupil = _v0.a;
			var event = _v0.b;
			return A2(
				$elm$core$List$any,
				function (_v1) {
					var i = _v1.a;
					var c = _v1.b;
					var _v2 = _Utils_Tuple2(c, color);
					_v2$2:
					while (true) {
						switch (_v2.a) {
							case 0:
								if (!_v2.b) {
									var _v3 = _v2.a;
									var _v4 = _v2.b;
									var _v5 = A2($elm$core$Dict$get, i, events);
									if (!_v5.$) {
										var ee = _v5.a;
										return _Utils_eq(
											_Utils_update(
												event,
												{af: 0}),
											ee);
									} else {
										return false;
									}
								} else {
									break _v2$2;
								}
							case 1:
								if (_v2.b === 1) {
									var _v6 = _v2.a;
									var _v7 = _v2.b;
									var _v8 = A2($elm$core$Dict$get, i, events);
									if (!_v8.$) {
										var ee = _v8.a;
										return _Utils_eq(
											_Utils_update(
												event,
												{af: 0}),
											ee);
									} else {
										return false;
									}
								} else {
									break _v2$2;
								}
							default:
								break _v2$2;
						}
					}
					return false;
				},
				$elm$core$Dict$toList(pupil._));
		};
		return A2(
			$elm$core$List$map,
			$elm$core$Tuple$first,
			A2($elm$core$List$filter, fn, matching));
	});
var $author$project$Pupil$pupilDisplay = function (pupil) {
	return pupil.aJ + (' (Klasse ' + (pupil.aa + ')'));
};
var $author$project$Pupil$pupilSorting = function (pupil) {
	return _Utils_ap(pupil.aa, pupil.aJ);
};
var $elm$html$Html$Attributes$scope = $elm$html$Html$Attributes$stringProperty('scope');
var $elm$core$List$sortBy = _List_sortBy;
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$Attributes$fillRule = _VirtualDom_attribute('fill-rule');
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $author$project$Helpers$svgIconSortAlphaDown = A2(
	$elm$svg$Svg$svg,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$width('16'),
			$elm$svg$Svg$Attributes$height('16'),
			$elm$svg$Svg$Attributes$fill('currentColor'),
			$elm$svg$Svg$Attributes$class('bi'),
			$elm$svg$Svg$Attributes$class('bi-sort-alpha-down'),
			$elm$svg$Svg$Attributes$viewBox('0 0 16 16')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$fillRule('evenodd'),
					$elm$svg$Svg$Attributes$d('M10.082 5.629 9.664 7H8.598l1.789-5.332h1.234L13.402 7h-1.12l-.419-1.371h-1.781zm1.57-.785L11 2.687h-.047l-.652 2.157h1.351z')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12.96 14H9.028v-.691l2.579-3.72v-.054H9.098v-.867h3.785v.691l-2.567 3.72v.054h2.645V14zM4.5 2.5a.5.5 0 0 0-1 0v9.793l-1.146-1.147a.5.5 0 0 0-.708.708l2 1.999.007.007a.497.497 0 0 0 .7-.006l2-2a.5.5 0 0 0-.707-.708L4.5 12.293V2.5z')
				]),
			_List_Nil)
		]));
var $elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$table = _VirtualDom_node('table');
var $elm$html$Html$tbody = _VirtualDom_node('tbody');
var $elm$html$Html$td = _VirtualDom_node('td');
var $elm$html$Html$th = _VirtualDom_node('th');
var $elm$html$Html$thead = _VirtualDom_node('thead');
var $elm$html$Html$Attributes$title = $elm$html$Html$Attributes$stringProperty('title');
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $author$project$Assignment$day = F5(
	function (num, model, events, matched, unmatched) {
		var tableRow = F2(
			function (p, e) {
				var isGreen = A2(
					$elm$core$List$any,
					function (green) {
						var _v3 = A2($elm$core$Dict$get, green, events);
						if (_v3.$ === 1) {
							return false;
						} else {
							var e2 = _v3.a;
							return _Utils_eq(e2, e);
						}
					},
					A2($author$project$Pupil$eventGroup, 0, p));
				return A2(
					$elm$html$Html$tr,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$td,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(
									$author$project$Pupil$pupilDisplay(p))
								])),
							A2(
							$elm$html$Html$td,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$author$project$Helpers$classes(
											isGreen ? 'badge text-bg-success' : 'badge text-bg-warning')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(e.aJ)
										]))
								]))
						]));
			});
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$author$project$Helpers$classes('col-md-10 mb-4')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h3,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							'Tag ' + $elm$core$String$fromInt(num))
						])),
					A2(
					$elm$html$Html$h4,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Zugeteilte Schüler/Schülerinnen')
						])),
					$elm$core$List$isEmpty(matched) ? A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('keine')
						])) : A2(
					$elm$html$Html$table,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('table')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$thead,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$tr,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											$elm$html$Html$th,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$scope('col')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text('Name und Klasse'),
													A2(
													$elm$html$Html$a,
													_List_fromArray(
														[
															$author$project$Helpers$classes('link-primary ms-2'),
															$elm$html$Html$Attributes$title('Nach Name und Klasse aufsteigend sortieren'),
															$elm$html$Html$Attributes$tabindex(0),
															A2($elm$html$Html$Attributes$attribute, 'role', 'button'),
															A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Nach Name und Klasse aufsteigend sortieren'),
															$elm$html$Html$Events$onClick(
															$author$project$Assignment$SortBy(0))
														]),
													_List_fromArray(
														[$author$project$Helpers$svgIconSortAlphaDown]))
												])),
											A2(
											$elm$html$Html$th,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$scope('col')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text('Gruppe'),
													A2(
													$elm$html$Html$a,
													_List_fromArray(
														[
															$author$project$Helpers$classes('link-primary ms-2'),
															$elm$html$Html$Attributes$title('Nach Gruppe aufsteigend sortieren'),
															$elm$html$Html$Attributes$tabindex(0),
															A2($elm$html$Html$Attributes$attribute, 'role', 'button'),
															A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Nach Gruppe aufsteigend sortieren'),
															$elm$html$Html$Events$onClick(
															$author$project$Assignment$SortBy(1))
														]),
													_List_fromArray(
														[$author$project$Helpers$svgIconSortAlphaDown]))
												]))
										]))
								])),
							A2(
							$elm$html$Html$tbody,
							_List_Nil,
							A2(
								$elm$core$List$map,
								function (_v2) {
									var p = _v2.a;
									var e = _v2.b;
									return A2(tableRow, p, e);
								},
								A2(
									$elm$core$List$sortBy,
									function (_v0) {
										var p = _v0.a;
										var e = _v0.b;
										var _v1 = model.am;
										if (!_v1) {
											return $author$project$Pupil$pupilSorting(p);
										} else {
											return e.aJ;
										}
									},
									matched)))
						])),
					A2(
					$elm$html$Html$h4,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Schüler/Schülerinnen ohne Platz')
						])),
					$elm$core$List$isEmpty(unmatched) ? A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Keine')
						])) : A2(
					$elm$html$Html$ol,
					_List_fromArray(
						[
							$author$project$Helpers$classes('list-group list-group-flush list-group-numbered')
						]),
					A2(
						$elm$core$List$map,
						function (p) {
							return A2(
								$elm$html$Html$li,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('list-group-item')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('ms-2')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												$author$project$Pupil$pupilDisplay(p))
											]))
									]));
						},
						A2($elm$core$List$sortBy, $author$project$Pupil$pupilSorting, unmatched))),
					A2(
					$elm$html$Html$h4,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Statistik')
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$author$project$Helpers$classes('badge text-bg-secondary me-2'),
									$elm$html$Html$Attributes$title('Gesamt')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									$elm$core$String$fromInt(
										$elm$core$List$length(matched) + $elm$core$List$length(unmatched)))
								])),
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('me-2')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('=')
								])),
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$author$project$Helpers$classes('badge text-bg-success me-2'),
									$elm$html$Html$Attributes$title('Grün')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									$elm$core$String$fromInt(
										$elm$core$List$length(
											A3($author$project$Assignment$onColor, 0, events, matched))))
								])),
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('me-2')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('+')
								])),
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$author$project$Helpers$classes('badge text-bg-warning me-2'),
									$elm$html$Html$Attributes$title('Gelb')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									$elm$core$String$fromInt(
										$elm$core$List$length(
											A3($author$project$Assignment$onColor, 1, events, matched))))
								])),
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('me-2')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('+')
								])),
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$author$project$Helpers$classes('badge text-bg-danger'),
									$elm$html$Html$Attributes$title('Rot')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									$elm$core$String$fromInt(
										$elm$core$List$length(unmatched)))
								]))
						]))
				]));
	});
var $author$project$Algo$VertexLeft = $elm$core$Basics$identity;
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $author$project$Algo$apply = F2(
	function (path, matching) {
		var splitFirstElem = function (l) {
			return A2(
				$elm$core$Maybe$andThen,
				function (h) {
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(
							h,
							A2($elm$core$List$drop, 1, l)));
				},
				$elm$core$List$head(l));
		};
		var right = path.p.b;
		var left = path.p.a;
		var newMatching = A2(
			$elm$core$List$cons,
			_Utils_Tuple2(left, right),
			A2(
				$elm$core$List$filter,
				function (_v2) {
					var l = _v2.a;
					return !_Utils_eq(l, left);
				},
				matching));
		var _v0 = splitFirstElem(path.x);
		if (_v0.$ === 1) {
			return newMatching;
		} else {
			var _v1 = _v0.a;
			var h = _v1.a;
			var t = _v1.b;
			return A2(
				$author$project$Algo$apply,
				{p: h, x: t},
				newMatching);
		}
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $author$project$Algo$getFromGraph = F2(
	function (left, graph) {
		return A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2(
				$elm$core$Maybe$andThen,
				A2($elm$core$Basics$composeL, $elm$core$Maybe$Just, $elm$core$Tuple$second),
				$elm$core$List$head(
					A2(
						$elm$core$List$filter,
						A2(
							$elm$core$Basics$composeR,
							$elm$core$Tuple$first,
							$elm$core$Basics$eq(left)),
						graph))));
	});
var $author$project$Algo$extend = F3(
	function (graph, path, acc) {
		var left = path.p;
		var newVertices = A2(
			$elm$core$List$filter,
			function (right) {
				return !A2($elm$core$List$member, right, acc.b);
			},
			A2($author$project$Algo$getFromGraph, left, graph));
		return _Utils_Tuple2(
			A2(
				$elm$core$List$append,
				acc.a,
				A2(
					$elm$core$List$map,
					function (right) {
						return {
							p: _Utils_Tuple2(left, right),
							x: path.x
						};
					},
					newVertices)),
			A2($elm$core$List$append, acc.b, newVertices));
	});
var $author$project$Algo$IntermediateUpdateResult = F2(
	function (paths, free) {
		return {ad: free, aj: paths};
	});
var $author$project$Algo$getFromMatchingRight = F2(
	function (right, matching) {
		return A2(
			$elm$core$Maybe$andThen,
			function (_v1) {
				var a = _v1.a;
				return $elm$core$Maybe$Just(a);
			},
			$elm$core$List$head(
				A2(
					$elm$core$List$filter,
					function (_v0) {
						var b = _v0.b;
						return _Utils_eq(right, b);
					},
					matching)));
	});
var $author$project$Algo$update = F2(
	function (matching, paths) {
		if (!paths.b) {
			return A2($author$project$Algo$IntermediateUpdateResult, _List_Nil, $elm$core$Maybe$Nothing);
		} else {
			var currentPath = paths.a;
			var remainingPaths = paths.b;
			var right = currentPath.p.b;
			var _v1 = A2($author$project$Algo$getFromMatchingRight, right, matching);
			if (_v1.$ === 1) {
				return A2(
					$author$project$Algo$IntermediateUpdateResult,
					_List_Nil,
					$elm$core$Maybe$Just(currentPath));
			} else {
				var left = _v1.a;
				var res = A2($author$project$Algo$update, matching, remainingPaths);
				var _v2 = res.ad;
				if (!_v2.$) {
					var path = _v2.a;
					return A2(
						$author$project$Algo$IntermediateUpdateResult,
						_List_Nil,
						$elm$core$Maybe$Just(path));
				} else {
					var updatedPath = {
						p: left,
						x: A2($elm$core$List$cons, currentPath.p, currentPath.x)
					};
					return A2(
						$author$project$Algo$IntermediateUpdateResult,
						A2($elm$core$List$cons, updatedPath, res.aj),
						$elm$core$Maybe$Nothing);
				}
			}
		}
	});
var $author$project$Algo$find = F3(
	function (graph, matching, _v0) {
		var paths = _v0.a;
		var visited = _v0.b;
		if ($elm$core$List$isEmpty(paths)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var res = A2($author$project$Algo$update, matching, paths);
			var _v1 = res.ad;
			if (!_v1.$) {
				var path = _v1.a;
				return $elm$core$Maybe$Just(path);
			} else {
				return A3(
					$author$project$Algo$find,
					graph,
					matching,
					A3(
						$elm$core$List$foldl,
						$author$project$Algo$extend(graph),
						_Utils_Tuple2(_List_Nil, visited),
						res.aj));
			}
		}
	});
var $author$project$Algo$getFromMatchingLeft = F2(
	function (left, matching) {
		return A2(
			$elm$core$Maybe$andThen,
			function (_v1) {
				var b = _v1.b;
				return $elm$core$Maybe$Just(b);
			},
			$elm$core$List$head(
				A2(
					$elm$core$List$filter,
					function (_v0) {
						var a = _v0.a;
						return _Utils_eq(left, a);
					},
					matching)));
	});
var $author$project$Algo$run = F2(
	function (graph, initialMatching) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (left, matching) {
					var newPath = function (l) {
						return {p: l, x: _List_Nil};
					};
					var _v1 = A3(
						$author$project$Algo$find,
						graph,
						matching,
						A3(
							$author$project$Algo$extend,
							graph,
							newPath(left),
							_Utils_Tuple2(_List_Nil, _List_Nil)));
					if (_v1.$ === 1) {
						return matching;
					} else {
						var path = _v1.a;
						return A2($author$project$Algo$apply, path, matching);
					}
				}),
			initialMatching,
			A2(
				$elm$core$List$filter,
				function (left) {
					var _v0 = A2($author$project$Algo$getFromMatchingLeft, left, initialMatching);
					if (_v0.$ === 1) {
						return true;
					} else {
						return false;
					}
				},
				A2($elm$core$List$map, $elm$core$Tuple$first, graph)));
	});
var $author$project$Algo$VertexRight = $elm$core$Basics$identity;
var $author$project$Event$Free = function (a) {
	return {$: 1, a: a};
};
var $author$project$Event$Reserved = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			if (dict.$ === -2) {
				return n;
			} else {
				var left = dict.d;
				var right = dict.e;
				var $temp$n = A2($elm$core$Dict$sizeHelp, n + 1, right),
					$temp$dict = left;
				n = $temp$n;
				dict = $temp$dict;
				continue sizeHelp;
			}
		}
	});
var $elm$core$Dict$size = function (dict) {
	return A2($elm$core$Dict$sizeHelp, 0, dict);
};
var $elm$core$Set$size = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$size(dict);
};
var $author$project$Event$extendToCapacityAndRestrictByClass = F3(
	function (event, cls, pupilsCl) {
		var num = (event.s / $elm$core$Set$size(cls)) | 0;
		var fn = F2(
			function (cl, _v2) {
				var i = _v2.a;
				var l = _v2.b;
				var newIndex = i + 1;
				return _Utils_Tuple2(
					(newIndex + num) - 1,
					_Utils_ap(
						l,
						A2(
							$elm$core$List$map,
							function (n) {
								return A2($author$project$Event$Reserved, n, cl);
							},
							A2($elm$core$List$range, newIndex, (newIndex + num) - 1))));
			});
		var seatsReserved = A3(
			$elm$core$List$foldl,
			fn,
			_Utils_Tuple2(0, _List_Nil),
			$elm$core$Set$toList(cls));
		var allSeats = _Utils_ap(
			seatsReserved.b,
			A2(
				$elm$core$List$map,
				$author$project$Event$Free,
				A2($elm$core$List$range, seatsReserved.a + 1, event.s)));
		return A2(
			$elm$core$List$map,
			function (s) {
				var i = function () {
					if (s.$ === 1) {
						var j = s.a;
						return j;
					} else {
						var j = s.a;
						return j;
					}
				}();
				return _Utils_update(
					event,
					{af: i});
			},
			A2(
				$elm$core$List$filter,
				function (s) {
					if (s.$ === 1) {
						return true;
					} else {
						var c = s.b;
						return _Utils_eq(c, pupilsCl);
					}
				},
				allSeats));
	});
var $author$project$Assignment$toGraphFromGreen = F3(
	function (pupils, cls, events) {
		var fn = F2(
			function (pupil, graph) {
				var v = A2(
					$elm$core$List$map,
					$elm$core$Basics$identity,
					A3(
						$elm$core$List$foldl,
						F2(
							function (e, l) {
								return _Utils_ap(
									A3($author$project$Event$extendToCapacityAndRestrictByClass, e, cls, pupil.aa),
									l);
							}),
						_List_Nil,
						A3(
							$elm$core$List$foldl,
							F2(
								function (eId, l) {
									var _v0 = A2($elm$core$Dict$get, eId, events);
									if (!_v0.$) {
										var e = _v0.a;
										return A2($elm$core$List$cons, e, l);
									} else {
										return l;
									}
								}),
							_List_Nil,
							A2($author$project$Pupil$eventGroup, 0, pupil))));
				var k = pupil;
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(k, v),
					graph);
			});
		var emptyGraph = _List_Nil;
		return A3($elm$core$List$foldl, fn, emptyGraph, pupils);
	});
var $author$project$Assignment$toGraphFromGreenAndYellow = F3(
	function (pupils, cls, events) {
		var fn = F2(
			function (pupil, graph) {
				var k = pupil;
				var combindEvents = A3(
					$elm$core$List$foldl,
					F2(
						function (eId, l) {
							var _v0 = A2($elm$core$Dict$get, eId, events);
							if (!_v0.$) {
								var e = _v0.a;
								return A2($elm$core$List$cons, e, l);
							} else {
								return l;
							}
						}),
					_List_Nil,
					_Utils_ap(
						A2($author$project$Pupil$eventGroup, 0, pupil),
						A2($author$project$Pupil$eventGroup, 1, pupil)));
				var v = A2(
					$elm$core$List$map,
					$elm$core$Basics$identity,
					A3(
						$elm$core$List$foldl,
						F2(
							function (e, l) {
								return _Utils_ap(
									A3($author$project$Event$extendToCapacityAndRestrictByClass, e, cls, pupil.aa),
									l);
							}),
						_List_Nil,
						combindEvents));
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(k, v),
					graph);
			});
		var emptyGraph = _List_Nil;
		return A3($elm$core$List$foldl, fn, emptyGraph, pupils);
	});
var $author$project$Assignment$toGraphFromYellowWithoutMatched = F4(
	function (pupils, cls, events, matching) {
		var onlyUnmatchedVertices = function (vertex) {
			return !A2(
				$elm$core$List$any,
				A2(
					$elm$core$Basics$composeR,
					$elm$core$Tuple$second,
					$elm$core$Basics$eq(vertex)),
				matching);
		};
		var onlyRemaining = function (pupil) {
			return !A2(
				$elm$core$List$any,
				function (_v1) {
					var p = _v1.a;
					return _Utils_eq(p, pupil);
				},
				matching);
		};
		var fn = F2(
			function (pupil, graph) {
				var v = A2(
					$elm$core$List$filter,
					onlyUnmatchedVertices,
					A2(
						$elm$core$List$map,
						$elm$core$Basics$identity,
						A3(
							$elm$core$List$foldl,
							F2(
								function (e, l) {
									return _Utils_ap(
										A3($author$project$Event$extendToCapacityAndRestrictByClass, e, cls, pupil.aa),
										l);
								}),
							_List_Nil,
							A3(
								$elm$core$List$foldl,
								F2(
									function (eId, l) {
										var _v0 = A2($elm$core$Dict$get, eId, events);
										if (!_v0.$) {
											var e = _v0.a;
											return A2($elm$core$List$cons, e, l);
										} else {
											return l;
										}
									}),
								_List_Nil,
								A2($author$project$Pupil$eventGroup, 1, pupil)))));
				var k = pupil;
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(k, v),
					graph);
			});
		var emptyGraph = _List_Nil;
		return A3(
			$elm$core$List$foldl,
			fn,
			emptyGraph,
			A2($elm$core$List$filter, onlyRemaining, pupils));
	});
var $author$project$Assignment$finalize = F3(
	function (pupils, cls, events) {
		var step3 = $author$project$Algo$run(
			A3($author$project$Assignment$toGraphFromGreenAndYellow, pupils, cls, events));
		var step1 = A2(
			$author$project$Algo$run,
			A3($author$project$Assignment$toGraphFromGreen, pupils, cls, events),
			_List_Nil);
		var step2 = A2(
			$author$project$Algo$run,
			A4($author$project$Assignment$toGraphFromYellowWithoutMatched, pupils, cls, events, step1),
			_List_Nil);
		return step3(
			_Utils_ap(step1, step2));
	});
var $author$project$Assignment$matchedAndUnmatchedPupils = F3(
	function (cls, events, pupils) {
		var matched = A3($author$project$Assignment$finalize, pupils, cls, events);
		var matchedTransformed = A2(
			$elm$core$List$map,
			function (_v1) {
				var p = _v1.a;
				var e = _v1.b;
				return _Utils_Tuple2(
					p,
					_Utils_update(
						e,
						{af: 0}));
			},
			matched);
		return _Utils_Tuple2(
			matchedTransformed,
			A2(
				$elm$core$List$filter,
				function (p) {
					var _v0 = A2($author$project$Algo$getFromMatchingLeft, p, matched);
					if (_v0.$ === 1) {
						return true;
					} else {
						return false;
					}
				},
				pupils));
	});
var $author$project$Assignment$innerView = F4(
	function (model, pupils, cls, events) {
		var _v0 = A3($author$project$Assignment$matchedAndUnmatchedPupils, cls, events, pupils);
		var matched = _v0.a;
		var unmatched = _v0.b;
		var _v1 = A3(
			$author$project$Assignment$matchedAndUnmatchedPupils,
			cls,
			events,
			A3($author$project$Assignment$applyMatchingToRedState, events, matched, pupils));
		var matched2 = _v1.a;
		var unmatched2 = _v1.b;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('col-md-9')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Das Ergebnis wird mit jeder Eingabe automatisch aktualisiert. Man kann es markieren, kopieren und anschließend in Excel, Word u. a. einfügen.')
						])),
					A5($author$project$Assignment$day, 1, model, events, matched, unmatched),
					A5($author$project$Assignment$day, 2, model, events, matched2, unmatched2)
				]));
	});
var $author$project$Assignment$view = F4(
	function (model, pupils, cls, events) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('mb-5')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h2,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('result'),
							$elm$html$Html$Attributes$class('nav-anchor')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Ergebnis')
						])),
					function () {
					var _v0 = model.ar;
					switch (_v0) {
						case 0:
							return A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$author$project$Helpers$classes('btn btn-primary'),
										$elm$html$Html$Events$onClick(
										$author$project$Assignment$SetVisibility(1))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Ergebnis berechnen und anzeigen')
									]));
						case 1:
							return A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$author$project$Helpers$classes('spinner-border text-primary'),
										A2($elm$html$Html$Attributes$attribute, 'role', 'status')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('visually-hidden')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Loading...')
											]))
									]));
						default:
							return A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$author$project$Helpers$classes('btn btn-primary mb-3'),
												$elm$html$Html$Events$onClick(
												$author$project$Assignment$SetVisibility(0))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Ergebnis ausblenden')
											])),
										A4($author$project$Assignment$innerView, model, pupils, cls, events)
									]));
					}
				}()
				]));
	});
var $author$project$Class$FormDataMsg = function (a) {
	return {$: 0, a: a};
};
var $author$project$Class$Save = {$: 1};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$hidden = $elm$html$Html$Attributes$boolProperty('hidden');
var $elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === -2) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Set$isEmpty = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$isEmpty(dict);
};
var $author$project$Class$Delete = function (a) {
	return {$: 2, a: a};
};
var $author$project$Helpers$svgIconXLg = A2(
	$elm$svg$Svg$svg,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$width('16'),
			$elm$svg$Svg$Attributes$height('16'),
			$elm$svg$Svg$Attributes$fill('currentColor'),
			$elm$svg$Svg$Attributes$class('bi'),
			$elm$svg$Svg$Attributes$class('bi-x-lg'),
			$elm$svg$Svg$Attributes$viewBox('0 0 16 16')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 .708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8 2.146 2.854Z')
				]),
			_List_Nil)
		]));
var $author$project$Class$oneClassLi = function (c) {
	return A2(
		$elm$html$Html$li,
		_List_fromArray(
			[
				$author$project$Helpers$classes('list-group-item d-flex justify-content-between align-items-start col-md-9 col-lg-7 col-xl-5')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$author$project$Helpers$classes('ms-2 me-auto')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(c)
					])),
				A2(
				$elm$html$Html$a,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('link-danger'),
						$elm$html$Html$Attributes$title('Löschen'),
						$elm$html$Html$Attributes$tabindex(0),
						A2($elm$html$Html$Attributes$attribute, 'role', 'button'),
						A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Löschen'),
						$elm$html$Html$Events$onClick(
						$author$project$Class$Delete(c))
					]),
				_List_fromArray(
					[$author$project$Helpers$svgIconXLg]))
			]));
};
var $elm$core$List$sort = function (xs) {
	return A2($elm$core$List$sortBy, $elm$core$Basics$identity, xs);
};
var $author$project$Class$allClasses = function (c) {
	return $elm$core$Set$isEmpty(c) ? A2(
		$elm$html$Html$p,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$hidden(true)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text('Noch keine Klassen angelegt')
			])) : A2(
		$elm$html$Html$ul,
		_List_fromArray(
			[
				$author$project$Helpers$classes('list-group list-group-flush')
			]),
		A2(
			$elm$core$List$map,
			$elm$html$Html$Lazy$lazy($author$project$Class$oneClassLi),
			$elm$core$List$sort(
				$elm$core$Set$toList(c))));
};
var $elm$html$Html$form = _VirtualDom_node('form');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$Events$alwaysPreventDefault = function (msg) {
	return _Utils_Tuple2(msg, true);
};
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 2, a: a};
};
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $elm$html$Html$Events$onSubmit = function (msg) {
	return A2(
		$elm$html$Html$Events$preventDefaultOn,
		'submit',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysPreventDefault,
			$elm$json$Json$Decode$succeed(msg)));
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$Attributes$required = $elm$html$Html$Attributes$boolProperty('required');
var $author$project$Helpers$tagWithInvalidFeedback = F5(
	function (tag, attrs, identifier, feedback, isInvalid) {
		if (isInvalid) {
			var i = identifier + 'Feedback';
			return _List_fromArray(
				[
					A2(
					tag,
					A2(
						$elm$core$List$cons,
						$elm$html$Html$Attributes$class('is-invalid'),
						A2(
							$elm$core$List$cons,
							A2($elm$html$Html$Attributes$attribute, 'aria-describedby', i),
							attrs)),
					_List_Nil),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id(i),
							$elm$svg$Svg$Attributes$class('invalid-feedback')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(feedback)
						]))
				]);
		} else {
			return _List_fromArray(
				[
					A2(tag, attrs, _List_Nil)
				]);
		}
	});
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Class$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('mb-5')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$h2,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('classes'),
						$elm$html$Html$Attributes$class('nav-anchor')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Klassen')
					])),
				A2(
				$elm$html$Html$form,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('mb-3'),
						$elm$html$Html$Events$onSubmit($author$project$Class$Save)
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h3,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$hidden(true)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Neue Klasse hinzufügen')
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$author$project$Helpers$classes('row g-3')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('col-md-3')
									]),
								A5(
									$author$project$Helpers$tagWithInvalidFeedback,
									$elm$html$Html$input,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('form-control'),
											$elm$html$Html$Attributes$type_('text'),
											$elm$html$Html$Attributes$placeholder('Bezeichner'),
											A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Bezeichner'),
											$elm$html$Html$Attributes$required(true),
											$elm$html$Html$Events$onInput($author$project$Class$FormDataMsg),
											$elm$html$Html$Attributes$value(model.O)
										]),
									'newClassName',
									'Klasse ist bereits vorhanden',
									model.J)),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('col-md-3')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$author$project$Helpers$classes('btn btn-primary'),
												$elm$html$Html$Attributes$type_('submit')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Hinzufügen')
											]))
									]))
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h3,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$hidden(true)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Alle Klassen')
							])),
						A2($elm$html$Html$Lazy$lazy, $author$project$Class$allClasses, model.A)
					]))
			]));
};
var $author$project$Event$Capacity = function (a) {
	return {$: 1, a: a};
};
var $author$project$Event$FormDataMsg = function (a) {
	return {$: 0, a: a};
};
var $author$project$Event$Name = function (a) {
	return {$: 0, a: a};
};
var $author$project$Event$SaveForm = {$: 1};
var $author$project$Event$CancelEdit = {$: 5};
var $author$project$Event$Delete = function (a) {
	return {$: 6, a: a};
};
var $author$project$Event$Edit = function (a) {
	return {$: 2, a: a};
};
var $author$project$Event$EditFormMsg = function (a) {
	return {$: 3, a: a};
};
var $author$project$Event$SaveEdit = {$: 4};
var $elm$html$Html$Attributes$min = $elm$html$Html$Attributes$stringProperty('min');
var $author$project$Helpers$svgIconCheckLg = A2(
	$elm$svg$Svg$svg,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$width('16'),
			$elm$svg$Svg$Attributes$height('16'),
			$elm$svg$Svg$Attributes$fill('currentColor'),
			$elm$svg$Svg$Attributes$class('bi'),
			$elm$svg$Svg$Attributes$class('bi-check-lg'),
			$elm$svg$Svg$Attributes$viewBox('0 0 16 16')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12.736 3.97a.733.733 0 0 1 1.047 0c.286.289.29.756.01 1.05L7.88 12.01a.733.733 0 0 1-1.065.02L3.217 8.384a.757.757 0 0 1 0-1.06.733.733 0 0 1 1.047 0l3.052 3.093 5.4-6.425a.247.247 0 0 1 .02-.022Z')
				]),
			_List_Nil)
		]));
var $author$project$Helpers$svgIconPencil = A2(
	$elm$svg$Svg$svg,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$width('16'),
			$elm$svg$Svg$Attributes$height('16'),
			$elm$svg$Svg$Attributes$fill('currentColor'),
			$elm$svg$Svg$Attributes$class('bi'),
			$elm$svg$Svg$Attributes$class('bi-pencil'),
			$elm$svg$Svg$Attributes$viewBox('0 0 16 16')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M12.146.146a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1 0 .708l-10 10a.5.5 0 0 1-.168.11l-5 2a.5.5 0 0 1-.65-.65l2-5a.5.5 0 0 1 .11-.168l10-10zM11.207 2.5 13.5 4.793 14.793 3.5 12.5 1.207 11.207 2.5zm1.586 3L10.5 3.207 4 9.707V10h.5a.5.5 0 0 1 .5.5v.5h.5a.5.5 0 0 1 .5.5v.5h.293l6.5-6.5zm-9.761 5.175-.106.106-1.528 3.821 3.821-1.528.106-.106A.5.5 0 0 1 5 12.5V12h-.5a.5.5 0 0 1-.5-.5V11h-.5a.5.5 0 0 1-.468-.325z')
				]),
			_List_Nil)
		]));
var $author$project$Event$oneEventLi = F2(
	function (editMode, _v0) {
		var eId = _v0.a;
		var event = _v0.b;
		var fixedLine = A2(
			$elm$html$Html$li,
			_List_fromArray(
				[
					$author$project$Helpers$classes('list-group-item d-flex justify-content-between align-items-start col-md-9 col-lg-7 col-xl-5')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$author$project$Helpers$classes('ms-2 me-auto')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(event.aJ),
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$author$project$Helpers$classes('ms-2 badge bg-primary rounded-pill'),
									$elm$html$Html$Attributes$title('Anzahl der Plätze'),
									A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Anzahl der Plätze')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									$elm$core$String$fromInt(event.s))
								]))
						])),
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('link-secondary'),
							$elm$html$Html$Attributes$title('Bearbeiten'),
							$elm$html$Html$Attributes$tabindex(0),
							A2($elm$html$Html$Attributes$attribute, 'role', 'button'),
							A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Bearbeiten'),
							$elm$html$Html$Events$onClick(
							$author$project$Event$Edit(eId))
						]),
					_List_fromArray(
						[$author$project$Helpers$svgIconPencil])),
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$author$project$Helpers$classes('ms-3 link-secondary'),
							$elm$html$Html$Attributes$title('Löschen'),
							$elm$html$Html$Attributes$tabindex(0),
							A2($elm$html$Html$Attributes$attribute, 'role', 'button'),
							A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Löschen'),
							$elm$html$Html$Events$onClick(
							$author$project$Event$Delete(eId))
						]),
					_List_fromArray(
						[$author$project$Helpers$svgIconXLg]))
				]));
		if (!editMode.$) {
			return fixedLine;
		} else {
			var editingEId = editMode.a;
			var _new = editMode.b;
			return (!_Utils_eq(editingEId, eId)) ? fixedLine : A2(
				$elm$html$Html$li,
				_List_fromArray(
					[
						$author$project$Helpers$classes('list-group-item d-flex justify-content-between align-items-start col-md-9 col-lg-7 col-xl-5')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$form,
						_List_fromArray(
							[
								$author$project$Helpers$classes('ms-2 row g-1'),
								$elm$html$Html$Events$onSubmit($author$project$Event$SaveEdit)
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('col-7')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$input,
										_List_fromArray(
											[
												$author$project$Helpers$classes('form-control form-control-sm'),
												$elm$html$Html$Attributes$type_('text'),
												$elm$html$Html$Attributes$placeholder('Name'),
												A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Name'),
												$elm$html$Html$Attributes$required(true),
												$elm$html$Html$Events$onInput(
												A2($elm$core$Basics$composeR, $author$project$Event$Name, $author$project$Event$EditFormMsg)),
												$elm$html$Html$Attributes$value(_new.aJ)
											]),
										_List_Nil)
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('col-3')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$input,
										_List_fromArray(
											[
												$author$project$Helpers$classes('form-control form-control-sm'),
												$elm$html$Html$Attributes$type_('number'),
												$elm$html$Html$Attributes$min('1'),
												A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Anzahl der Plätze'),
												$elm$html$Html$Events$onInput(
												A2(
													$elm$core$Basics$composeR,
													$elm$core$String$toInt,
													A2(
														$elm$core$Basics$composeR,
														$elm$core$Maybe$withDefault(0),
														A2($elm$core$Basics$composeR, $author$project$Event$Capacity, $author$project$Event$EditFormMsg)))),
												$elm$html$Html$Attributes$value(
												$elm$core$String$fromInt(_new.s))
											]),
										_List_Nil)
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('col-1')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$author$project$Helpers$classes('btn btn-outline-success btn-sm'),
												$elm$html$Html$Attributes$type_('submit'),
												$elm$html$Html$Attributes$title('Speichern'),
												A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Speichern')
											]),
										_List_fromArray(
											[$author$project$Helpers$svgIconCheckLg]))
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('col-1')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$author$project$Helpers$classes('btn btn-outline-danger btn-sm'),
												$elm$html$Html$Attributes$type_('button'),
												$elm$html$Html$Attributes$title('Abbrechen'),
												A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Abbrechen'),
												$elm$html$Html$Events$onClick($author$project$Event$CancelEdit)
											]),
										_List_fromArray(
											[$author$project$Helpers$svgIconXLg]))
									]))
							]))
					]));
		}
	});
var $author$project$Event$allEvents = F2(
	function (editMode, events) {
		return $elm$core$Dict$isEmpty(events) ? A2(
			$elm$html$Html$p,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$hidden(true)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Noch keine Gruppen angelegt')
				])) : A2(
			$elm$html$Html$ol,
			_List_fromArray(
				[
					$author$project$Helpers$classes('list-group list-group-flush list-group-numbered')
				]),
			A2(
				$elm$core$List$map,
				$elm$html$Html$Lazy$lazy(
					$author$project$Event$oneEventLi(editMode)),
				A2(
					$elm$core$List$sortBy,
					function (t) {
						return t.b.aJ;
					},
					A2(
						$elm$core$List$map,
						function (t) {
							return _Utils_Tuple2(t.a, t.b);
						},
						$elm$core$Dict$toList(events)))));
	});
var $elm$virtual_dom$VirtualDom$lazy2 = _VirtualDom_lazy2;
var $elm$html$Html$Lazy$lazy2 = $elm$virtual_dom$VirtualDom$lazy2;
var $author$project$Event$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('mb-5')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$h2,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('events'),
						$elm$html$Html$Attributes$class('nav-anchor')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Projektgruppen')
					])),
				A2(
				$elm$html$Html$p,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('col-md-9')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Abhängig von der Anzahl der Klassen und der Anzahl der Plätze in einer Gruppe gibt es feste Plätze\n                für Schüler und Schülerinnen aus bestimmten Klassen und freie Plätze. Es erfolgt eine ganzzahlige Teilung\n                der Plätze durch die Anzahl der Klassen. Der Rest gibt die freien Plätze an.')
					])),
				A2(
				$elm$html$Html$p,
				_List_fromArray(
					[
						$author$project$Helpers$classes('col-md-8 fst-italic pb-2')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Beispiel: Bei vier Klassen und 13 Plätzen gibt es 3 feste Plätze pro Klasse und einen freien\n                Platz, der von jedem beliebigen Schüler besetzt werden kann.')
					])),
				A2(
				$elm$html$Html$form,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('mb-3'),
						$elm$html$Html$Events$onSubmit($author$project$Event$SaveForm)
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h3,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$hidden(true)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Neue Gruppe hinzufügen')
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$author$project$Helpers$classes('row g-3')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('col-md-3')
									]),
								A5(
									$author$project$Helpers$tagWithInvalidFeedback,
									$elm$html$Html$input,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('form-control'),
											$elm$html$Html$Attributes$type_('text'),
											$elm$html$Html$Attributes$placeholder('Name'),
											A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Name'),
											$elm$html$Html$Attributes$required(true),
											$elm$html$Html$Events$onInput(
											A2($elm$core$Basics$composeR, $author$project$Event$Name, $author$project$Event$FormDataMsg)),
											$elm$html$Html$Attributes$value(model.O.aJ)
										]),
									'newGroupName',
									'Gruppe ist bereits vorhanden',
									model.J)),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('col-md-3')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$input,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('form-control'),
												$elm$html$Html$Attributes$type_('number'),
												$elm$html$Html$Attributes$min('1'),
												A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Anzahl der Plätze'),
												$elm$html$Html$Events$onInput(
												A2(
													$elm$core$Basics$composeR,
													$elm$core$String$toInt,
													A2(
														$elm$core$Basics$composeR,
														$elm$core$Maybe$withDefault(0),
														A2($elm$core$Basics$composeR, $author$project$Event$Capacity, $author$project$Event$FormDataMsg)))),
												$elm$html$Html$Attributes$value(
												$elm$core$String$fromInt(model.O.s))
											]),
										_List_Nil),
										A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('form-text')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Anzahl der Plätze')
											]))
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('col-md-3')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$author$project$Helpers$classes('btn btn-primary'),
												$elm$html$Html$Attributes$type_('submit')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Hinzufügen')
											]))
									]))
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h3,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$hidden(true)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Alle Gruppen')
							])),
						A3($elm$html$Html$Lazy$lazy2, $author$project$Event$allEvents, model.v, model.h)
					]))
			]));
};
var $author$project$Pupil$Class = function (a) {
	return {$: 1, a: a};
};
var $author$project$Pupil$FormDataMsg = function (a) {
	return {$: 0, a: a};
};
var $author$project$Pupil$Names = function (a) {
	return {$: 0, a: a};
};
var $author$project$Pupil$Save = {$: 1};
var $author$project$Pupil$Delete = function (a) {
	return {$: 2, a: a};
};
var $author$project$Pupil$ChangeChoice = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $elm$html$Html$Attributes$colspan = function (n) {
	return A2(
		_VirtualDom_attribute,
		'colspan',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$Attributes$for = $elm$html$Html$Attributes$stringProperty('htmlFor');
var $author$project$Event$intToId = $elm$core$Basics$identity;
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$html$Html$Attributes$name = $elm$html$Html$Attributes$stringProperty('name');
var $author$project$Pupil$innerTable = F2(
	function (events, pupil) {
		var getEventName = function (_v1) {
			var i = _v1.a;
			return A2(
				$elm$core$Maybe$withDefault,
				'---',
				A2(
					$elm$core$Maybe$andThen,
					A2(
						$elm$core$Basics$composeL,
						$elm$core$Maybe$Just,
						function ($) {
							return $.aJ;
						}),
					A2($elm$core$Dict$get, i, events)));
		};
		return $elm$core$Dict$isEmpty(events) ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2(
			$elm$html$Html$table,
			_List_fromArray(
				[
					$author$project$Helpers$classes('table table-striped')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$thead,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$tr,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$th,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$scope('col'),
											$elm$html$Html$Attributes$class('col-9')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Gruppe')
										])),
									A2(
									$elm$html$Html$th,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$scope('col'),
											$elm$html$Html$Attributes$colspan(3),
											$elm$html$Html$Attributes$class('col-3')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Farbe')
										]))
								]))
						])),
					A2(
					$elm$html$Html$tbody,
					_List_Nil,
					A2(
						$elm$core$List$map,
						function (_v0) {
							var i = _v0.a;
							var c = _v0.b;
							var radioGroup = _Utils_ap(
								$author$project$Pupil$pupilDisplay(pupil),
								$elm$core$String$fromInt(i));
							return A2(
								$elm$html$Html$tr,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$th,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$scope('row')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												getEventName(
													_Utils_Tuple2(i, c)))
											])),
										A2(
										$elm$html$Html$td,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$div,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('form-check form-check-inline')
													]),
												_List_fromArray(
													[
														A2(
														$elm$html$Html$input,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('form-check-input'),
																$elm$html$Html$Attributes$type_('radio'),
																$elm$html$Html$Attributes$name(radioGroup),
																$elm$html$Html$Attributes$id(radioGroup + 'Green'),
																$elm$html$Html$Attributes$checked(!c),
																$elm$html$Html$Events$onClick(
																A3(
																	$author$project$Pupil$ChangeChoice,
																	pupil,
																	$author$project$Event$intToId(i),
																	0))
															]),
														_List_Nil),
														A2(
														$elm$html$Html$label,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('form-check-label'),
																$elm$html$Html$Attributes$for(radioGroup + 'Green')
															]),
														_List_fromArray(
															[
																A2(
																$elm$html$Html$span,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$class('badge text-bg-success')
																	]),
																_List_fromArray(
																	[
																		$elm$html$Html$text('Grün')
																	]))
															]))
													]))
											])),
										A2(
										$elm$html$Html$td,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$div,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('form-check form-check-inline')
													]),
												_List_fromArray(
													[
														A2(
														$elm$html$Html$input,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('form-check-input'),
																$elm$html$Html$Attributes$type_('radio'),
																$elm$html$Html$Attributes$name(radioGroup),
																$elm$html$Html$Attributes$id(radioGroup + 'Yellow'),
																$elm$html$Html$Attributes$checked(c === 1),
																$elm$html$Html$Events$onClick(
																A3(
																	$author$project$Pupil$ChangeChoice,
																	pupil,
																	$author$project$Event$intToId(i),
																	1))
															]),
														_List_Nil),
														A2(
														$elm$html$Html$label,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('form-check-label'),
																$elm$html$Html$Attributes$for(radioGroup + 'Yellow')
															]),
														_List_fromArray(
															[
																A2(
																$elm$html$Html$span,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$class('badge text-bg-warning')
																	]),
																_List_fromArray(
																	[
																		$elm$html$Html$text('Gelb')
																	]))
															]))
													]))
											])),
										A2(
										$elm$html$Html$td,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$div,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('form-check form-check-inline')
													]),
												_List_fromArray(
													[
														A2(
														$elm$html$Html$input,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('form-check-input'),
																$elm$html$Html$Attributes$type_('radio'),
																$elm$html$Html$Attributes$name(radioGroup),
																$elm$html$Html$Attributes$id(radioGroup + 'Red'),
																$elm$html$Html$Attributes$checked(c === 2),
																$elm$html$Html$Events$onClick(
																A3(
																	$author$project$Pupil$ChangeChoice,
																	pupil,
																	$author$project$Event$intToId(i),
																	2))
															]),
														_List_Nil),
														A2(
														$elm$html$Html$label,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('form-check-label'),
																$elm$html$Html$Attributes$for(radioGroup + 'Red')
															]),
														_List_fromArray(
															[
																A2(
																$elm$html$Html$span,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$class('badge text-bg-danger')
																	]),
																_List_fromArray(
																	[
																		$elm$html$Html$text('Rot')
																	]))
															]))
													]))
											]))
									]));
						},
						A2(
							$elm$core$List$sortBy,
							getEventName,
							$elm$core$Dict$toList(pupil._))))
				]));
	});
var $author$project$Pupil$onePupilLi = F2(
	function (events, pupil) {
		return A2(
			$elm$html$Html$li,
			_List_fromArray(
				[
					$author$project$Helpers$classes('list-group-item d-flex justify-content-between align-items-start col-md-9 col-lg-7 col-xl-7 pt-4 pb-2')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$author$project$Helpers$classes('ms-2 w-100')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(
									$author$project$Pupil$pupilDisplay(pupil))
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('table-responsive')
								]),
							_List_fromArray(
								[
									A2($author$project$Pupil$innerTable, events, pupil)
								]))
						])),
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('link-danger'),
							$elm$html$Html$Attributes$title('Löschen'),
							$elm$html$Html$Attributes$tabindex(0),
							A2($elm$html$Html$Attributes$attribute, 'role', 'button'),
							A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Löschen'),
							$elm$html$Html$Events$onClick(
							$author$project$Pupil$Delete(pupil))
						]),
					_List_fromArray(
						[$author$project$Helpers$svgIconXLg]))
				]));
	});
var $author$project$Pupil$allPupils = F2(
	function (events, pupils) {
		return $elm$core$List$isEmpty(pupils) ? A2(
			$elm$html$Html$p,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$hidden(true)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Noch keine Schüler oder Schülerinnen angelegt')
				])) : A2(
			$elm$html$Html$ol,
			_List_fromArray(
				[
					$author$project$Helpers$classes('list-group list-group-flush list-group-numbered')
				]),
			A2(
				$elm$core$List$map,
				$elm$html$Html$Lazy$lazy(
					$author$project$Pupil$onePupilLi(events)),
				A2($elm$core$List$sortBy, $author$project$Pupil$pupilSorting, pupils)));
	});
var $elm$html$Html$option = _VirtualDom_node('option');
var $elm$html$Html$Attributes$rows = function (n) {
	return A2(
		_VirtualDom_attribute,
		'rows',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$select = _VirtualDom_node('select');
var $elm$html$Html$textarea = _VirtualDom_node('textarea');
var $author$project$Pupil$view = F3(
	function (model, cls, events) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('mb-5')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h2,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('pupils'),
							$elm$html$Html$Attributes$class('nav-anchor')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Schüler/Schülerinnen')
						])),
					A2(
					$elm$html$Html$form,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('mb-3'),
							$elm$html$Html$Events$onSubmit($author$project$Pupil$Save)
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$h3,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$hidden(true)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Schüler und Schülerinnen der gleichen Klasse hinzufügen')
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$author$project$Helpers$classes('row g-3')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('col-md-3')
										]),
									A5(
										$author$project$Helpers$tagWithInvalidFeedback,
										$elm$html$Html$textarea,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('form-control'),
												$elm$html$Html$Attributes$rows(1),
												$elm$html$Html$Attributes$placeholder('Namen (mit Zeilenumbrüchen getrennt)'),
												A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Namen (mit Zeilenumbrüchen getrennt)'),
												$elm$html$Html$Attributes$required(true),
												$elm$html$Html$Events$onInput(
												A2($elm$core$Basics$composeR, $author$project$Pupil$Names, $author$project$Pupil$FormDataMsg)),
												$elm$html$Html$Attributes$value(model.O.X)
											]),
										'newPupilNames',
										'Schüler/Schülerin ist in dieser Klasse bereits vorhanden oder unbekannte Klasse oder sonst ungültige Eingabe',
										model.J)),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('col-md-3')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$select,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('form-select'),
													A2($elm$html$Html$Attributes$attribute, 'aria-label', 'Klasse'),
													$elm$html$Html$Attributes$required(true),
													$elm$html$Html$Events$onInput(
													A2($elm$core$Basics$composeR, $author$project$Pupil$Class, $author$project$Pupil$FormDataMsg))
												]),
											A2(
												$elm$core$List$cons,
												A2(
													$elm$html$Html$option,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$value(''),
															$elm$html$Html$Attributes$hidden(true)
														]),
													_List_fromArray(
														[
															$elm$html$Html$text('Klasse bitte auswählen')
														])),
												A2(
													$elm$core$List$map,
													function (c) {
														return A2(
															$elm$html$Html$option,
															_List_fromArray(
																[
																	$elm$html$Html$Attributes$value(c)
																]),
															_List_fromArray(
																[
																	$elm$html$Html$text(c)
																]));
													},
													$elm$core$Set$toList(cls))))
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('col-md-3')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$button,
											_List_fromArray(
												[
													$author$project$Helpers$classes('btn btn-primary'),
													$elm$html$Html$Attributes$type_('submit')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text('Hinzufügen')
												]))
										]))
								]))
						])),
					A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$h3,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$hidden(true)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Alle Schüler/Schülerinnen')
								])),
							A2(
							$elm$html$Html$Lazy$lazy,
							$author$project$Pupil$allPupils(events),
							model.r)
						]))
				]));
	});
var $author$project$Main$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$author$project$Main$navbar,
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$author$project$Helpers$classes('container p-3 pb-5')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$main_,
						_List_Nil,
						_List_fromArray(
							[
								$author$project$Main$readme,
								A2(
								$elm$html$Html$map,
								$author$project$Main$ClassMsg,
								A2($elm$html$Html$Lazy$lazy, $author$project$Class$view, model.A)),
								A2(
								$elm$html$Html$map,
								$author$project$Main$EventMsg,
								A2($elm$html$Html$Lazy$lazy, $author$project$Event$view, model.h)),
								A2(
								$elm$html$Html$map,
								$author$project$Main$PupilMsg,
								A4($elm$html$Html$Lazy$lazy3, $author$project$Pupil$view, model.r, model.A.A, model.h.h)),
								A2(
								$elm$html$Html$map,
								$author$project$Main$AssignmentMsg,
								A5($elm$html$Html$Lazy$lazy4, $author$project$Assignment$view, model.N, model.r.r, model.A.A, model.h.h)),
								$author$project$Main$admin
							]))
					]))
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{
		bl: function (flags) {
			return _Utils_Tuple2(
				$author$project$Main$init(flags),
				$elm$core$Platform$Cmd$none);
		},
		bB: function (_v0) {
			return $elm$core$Platform$Sub$none;
		},
		bF: $author$project$Main$update,
		bG: $author$project$Main$view
	});
_Platform_export({'Main':{'init':$author$project$Main$main(
	A2(
		$elm$json$Json$Decode$andThen,
		function (data) {
			return $elm$json$Json$Decode$succeed(
				{ab: data});
		},
		A2($elm$json$Json$Decode$field, 'data', $elm$json$Json$Decode$string)))(0)}});}(this));