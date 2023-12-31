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
	if (region.aZ.ao === region.bb.ao)
	{
		return 'on line ' + region.aZ.ao;
	}
	return 'on lines ' + region.aZ.ao + ' through ' + region.bb.ao;
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
		impl.ca,
		impl.cD,
		impl.cy,
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
		L: func(record.L),
		a_: record.a_,
		aX: record.aX
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
		var message = !tag ? value : tag < 3 ? value.a : value.L;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.a_;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.aX) && event.preventDefault(),
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
		impl.ca,
		impl.cD,
		impl.cy,
		function(sendToApp, initialModel) {
			var view = impl.cE;
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
		impl.ca,
		impl.cD,
		impl.cy,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.aY && impl.aY(sendToApp)
			var view = impl.cE;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.bV);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.cB) && (_VirtualDom_doc.title = title = doc.cB);
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
	var onUrlChange = impl.cp;
	var onUrlRequest = impl.cq;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		aY: function(sendToApp)
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
							&& curr.by === next.by
							&& curr.bi === next.bi
							&& curr.bv.a === next.bv.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		ca: function(flags)
		{
			return A3(impl.ca, flags, _Browser_getUrl(), key);
		},
		cE: impl.cE,
		cD: impl.cD,
		cy: impl.cy
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
		? { b7: 'hidden', bY: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { b7: 'mozHidden', bY: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { b7: 'msHidden', bY: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { b7: 'webkitHidden', bY: 'webkitvisibilitychange' }
		: { b7: 'hidden', bY: 'visibilitychange' };
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
		bD: _Browser_getScene(),
		bL: {
			bO: _Browser_window.pageXOffset,
			bP: _Browser_window.pageYOffset,
			bN: _Browser_doc.documentElement.clientWidth,
			bh: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		bN: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		bh: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
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
			bD: {
				bN: node.scrollWidth,
				bh: node.scrollHeight
			},
			bL: {
				bO: node.scrollLeft,
				bP: node.scrollTop,
				bN: node.clientWidth,
				bh: node.clientHeight
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
			bD: _Browser_getScene(),
			bL: {
				bO: x,
				bP: y,
				bN: _Browser_doc.documentElement.clientWidth,
				bh: _Browser_doc.documentElement.clientHeight
			},
			b1: {
				bO: x + rect.left,
				bP: y + rect.top,
				bN: rect.width,
				bh: rect.height
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
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$Maybe$Nothing = {$: 1};
var $author$project$Main$defaultMaxStep = '5';
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $turboMaCk$any_set$Set$Any$AnySet = $elm$core$Basics$identity;
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $turboMaCk$any_dict$Dict$Any$AnyDict = $elm$core$Basics$identity;
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $turboMaCk$any_dict$Dict$Any$empty = function (toKey) {
	return {b: $elm$core$Dict$empty, w: toKey};
};
var $turboMaCk$any_set$Set$Any$empty = A2($elm$core$Basics$composeL, $elm$core$Basics$identity, $turboMaCk$any_dict$Dict$Any$empty);
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$core$Basics$append = _Utils_append;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropRight = F2(
	function (n, string) {
		return (n < 1) ? string : A3($elm$core$String$slice, 0, -n, string);
	});
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
var $elm$core$Basics$False = 1;
var $elm$core$Basics$True = 0;
var $elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === -2) {
		return true;
	} else {
		return false;
	}
};
var $turboMaCk$any_dict$Dict$Any$isEmpty = function (_v0) {
	var dict = _v0.b;
	return $elm$core$Dict$isEmpty(dict);
};
var $turboMaCk$any_set$Set$Any$isEmpty = function (_v0) {
	var dict = _v0;
	return $turboMaCk$any_dict$Dict$Any$isEmpty(dict);
};
var $author$project$Disjunct$isEmpty = $turboMaCk$any_set$Set$Any$isEmpty;
var $author$project$Language$printTerm = function (term) {
	switch (term.$) {
		case 0:
			var str = term.a;
			return str;
		case 1:
			var str = term.a;
			return str;
		default:
			var str = term.a;
			var terms = term.b;
			return str + ('(' + (A2(
				$elm$core$String$dropRight,
				2,
				A3(
					$elm$core$List$foldl,
					F2(
						function (x, acc) {
							return acc + ($author$project$Language$printTerm(x) + ', ');
						}),
					'',
					terms)) + ')'));
	}
};
var $author$project$Language$literalToString = function (literal) {
	if (!literal.$) {
		var p = literal.a;
		var terms = literal.b;
		var _v1 = A3(
			$elm$core$List$foldl,
			F2(
				function (x, acc) {
					return acc + ($author$project$Language$printTerm(x) + ', ');
				}),
			'',
			terms);
		if (_v1 === '') {
			return p;
		} else {
			var list = _v1;
			return p + ('(' + (A2($elm$core$String$dropRight, 2, list) + ')'));
		}
	} else {
		var p = literal.a;
		var terms = literal.b;
		var _v2 = A3(
			$elm$core$List$foldl,
			F2(
				function (x, acc) {
					return acc + ($author$project$Language$printTerm(x) + ', ');
				}),
			'',
			terms);
		if (_v2 === '') {
			return '¬' + p;
		} else {
			var list = _v2;
			return '¬' + (p + ('(' + (A2($elm$core$String$dropRight, 2, list) + ')')));
		}
	}
};
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
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
var $turboMaCk$any_dict$Dict$Any$toList = function (_v0) {
	var dict = _v0.b;
	return $elm$core$Dict$values(dict);
};
var $turboMaCk$any_dict$Dict$Any$keys = A2(
	$elm$core$Basics$composeL,
	$elm$core$List$map($elm$core$Tuple$first),
	$turboMaCk$any_dict$Dict$Any$toList);
var $turboMaCk$any_set$Set$Any$toList = function (_v0) {
	var dict = _v0;
	return $turboMaCk$any_dict$Dict$Any$keys(dict);
};
var $author$project$Disjunct$toString = function (d) {
	if ($author$project$Disjunct$isEmpty(d)) {
		return '■';
	} else {
		var listOfDisjunsts = $turboMaCk$any_set$Set$Any$toList(d);
		return '{' + (A2(
			$elm$core$String$dropRight,
			2,
			A3(
				$elm$core$List$foldl,
				F2(
					function (x, acc) {
						return acc + ($author$project$Language$literalToString(x) + ', ');
					}),
				'',
				listOfDisjunsts)) + '}');
	}
};
var $author$project$DisjunctSet$toComparable = A2($elm$core$Basics$composeR, $elm$core$Tuple$second, $author$project$Disjunct$toString);
var $author$project$DisjunctSet$empty = $turboMaCk$any_set$Set$Any$empty($author$project$DisjunctSet$toComparable);
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $author$project$Language$empty = {aL: $elm$core$Set$empty, aQ: $elm$core$Set$empty, aW: $elm$core$Set$empty, a1: $elm$core$Set$empty};
var $author$project$Main$init = {
	az: '',
	Z: $author$project$DisjunctSet$empty,
	aB: $elm$core$Maybe$Nothing,
	m: _List_fromArray(
		[
			_Utils_Tuple2('', '')
		]),
	F: _List_fromArray(
		[$elm$core$Maybe$Nothing]),
	aC: '',
	G: $author$project$Language$empty,
	an: $author$project$Language$empty,
	R: $author$project$Main$defaultMaxStep,
	aE: '',
	ar: $elm$core$Maybe$Nothing,
	ac: _List_Nil,
	ae: _Utils_Tuple2('', ''),
	V: _List_Nil,
	aK: ''
};
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
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
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
		if (!builder.h) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.j),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.j);
		} else {
			var treeLen = builder.h * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.k) : builder.k;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.h);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.j) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.j);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{k: nodeList, h: (len / $elm$core$Array$branchFactor) | 0, j: tail});
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
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
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
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {bd: fragment, bi: host, bt: path, bv: port_, by: protocol, bz: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
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
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$browser$Browser$sandbox = function (impl) {
	return _Browser_element(
		{
			ca: function (_v0) {
				return _Utils_Tuple2(impl.ca, $elm$core$Platform$Cmd$none);
			},
			cy: function (_v1) {
				return $elm$core$Platform$Sub$none;
			},
			cD: F2(
				function (msg, model) {
					return _Utils_Tuple2(
						A2(impl.cD, msg, model),
						$elm$core$Platform$Cmd$none);
				}),
			cE: impl.cE
		});
};
var $author$project$Main$UpdateFormula = F2(
	function (a, b) {
		return {$: 8, a: a, b: b};
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Language$And = 0;
var $author$project$Language$Operation = F3(
	function (a, b, c) {
		return {$: 2, a: a, b: b, c: c};
	});
var $author$project$Language$Or = 1;
var $author$project$Language$Quantification = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var $author$project$Language$Negation = function (a) {
	return {$: 1, a: a};
};
var $author$project$Transformations$negate = function (f) {
	if (f.$ === 1) {
		var f1 = f.a;
		return f1;
	} else {
		return $author$project$Language$Negation(f);
	}
};
var $author$project$Transformations$eliminateImplAndEqv = function (formula) {
	switch (formula.$) {
		case 2:
			var a = formula.a;
			var op = formula.b;
			var b = formula.c;
			var newB = $author$project$Transformations$eliminateImplAndEqv(b);
			var newA = $author$project$Transformations$eliminateImplAndEqv(a);
			switch (op) {
				case 2:
					return A3(
						$author$project$Language$Operation,
						$author$project$Transformations$negate(newA),
						1,
						newB);
				case 3:
					return A3(
						$author$project$Language$Operation,
						A3(
							$author$project$Language$Operation,
							$author$project$Transformations$negate(newA),
							1,
							newB),
						0,
						A3(
							$author$project$Language$Operation,
							newA,
							1,
							$author$project$Transformations$negate(newB)));
				default:
					return A3($author$project$Language$Operation, newA, op, newB);
			}
		case 1:
			var f = formula.a;
			return $author$project$Transformations$negate(
				$author$project$Transformations$eliminateImplAndEqv(f));
		case 3:
			var q = formula.a;
			var x = formula.b;
			var f = formula.c;
			return A3(
				$author$project$Language$Quantification,
				q,
				x,
				$author$project$Transformations$eliminateImplAndEqv(f));
		default:
			return formula;
	}
};
var $author$project$Parser$errToString = function (err) {
	switch (err.$) {
		case 0:
			return 'Unbalanced parentheses';
		case 1:
			var str = err.a;
			return 'Predicate ' + (str + ' cannot be in term');
		case 2:
			var str = err.a;
			return 'Unexpected symbol ' + str;
		case 3:
			var str = err.a;
			return str + ' must be a variable';
		default:
			return 'Unexpected end of input';
	}
};
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
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
var $author$project$DisjunctSet$isEmpty = $turboMaCk$any_set$Set$Any$isEmpty;
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $author$project$Language$Eqv = 3;
var $author$project$Language$Exists = 0;
var $author$project$Language$ForAll = 1;
var $author$project$Transformations$moveNegations = function (outerFormula) {
	moveNegations:
	while (true) {
		switch (outerFormula.$) {
			case 1:
				var formula = outerFormula.a;
				switch (formula.$) {
					case 0:
						return outerFormula;
					case 1:
						var f = formula.a;
						var $temp$outerFormula = f;
						outerFormula = $temp$outerFormula;
						continue moveNegations;
					case 2:
						var a = formula.a;
						var op = formula.b;
						var b = formula.c;
						switch (op) {
							case 0:
								return A3(
									$author$project$Language$Operation,
									$author$project$Transformations$moveNegations(
										$author$project$Transformations$negate(a)),
									1,
									$author$project$Transformations$moveNegations(
										$author$project$Transformations$negate(b)));
							case 1:
								return A3(
									$author$project$Language$Operation,
									$author$project$Transformations$moveNegations(
										$author$project$Transformations$negate(a)),
									0,
									$author$project$Transformations$moveNegations(
										$author$project$Transformations$negate(b)));
							case 2:
								return A3(
									$author$project$Language$Operation,
									$author$project$Transformations$moveNegations(a),
									0,
									$author$project$Transformations$moveNegations(
										$author$project$Transformations$negate(b)));
							default:
								return A3(
									$author$project$Language$Operation,
									$author$project$Transformations$moveNegations(
										$author$project$Transformations$negate(a)),
									3,
									$author$project$Transformations$moveNegations(b));
						}
					default:
						if (!formula.a) {
							var _v3 = formula.a;
							var x = formula.b;
							var f = formula.c;
							return A3(
								$author$project$Language$Quantification,
								1,
								x,
								$author$project$Transformations$moveNegations(
									$author$project$Transformations$negate(f)));
						} else {
							var _v4 = formula.a;
							var x = formula.b;
							var f = formula.c;
							return A3(
								$author$project$Language$Quantification,
								0,
								x,
								$author$project$Transformations$moveNegations(
									$author$project$Transformations$negate(f)));
						}
				}
			case 0:
				return outerFormula;
			case 2:
				var a = outerFormula.a;
				var op = outerFormula.b;
				var b = outerFormula.c;
				return A3(
					$author$project$Language$Operation,
					$author$project$Transformations$moveNegations(a),
					op,
					$author$project$Transformations$moveNegations(b));
			default:
				var q = outerFormula.a;
				var x = outerFormula.b;
				var f = outerFormula.c;
				return A3(
					$author$project$Language$Quantification,
					q,
					x,
					$author$project$Transformations$moveNegations(f));
		}
	}
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$Transformations$pNFtoCNF = function (formula) {
	var distribute = F2(
		function (f1_, f2_) {
			var _v0 = _Utils_Tuple2(f1_, f2_);
			if ((_v0.a.$ === 2) && (!_v0.a.b)) {
				var _v1 = _v0.a;
				var f11 = _v1.a;
				var _v2 = _v1.b;
				var f12 = _v1.c;
				var f2 = _v0.b;
				return A3(
					$author$project$Language$Operation,
					A2(distribute, f11, f2),
					0,
					A2(distribute, f12, f2));
			} else {
				if ((_v0.b.$ === 2) && (!_v0.b.b)) {
					var f1 = _v0.a;
					var _v3 = _v0.b;
					var f21 = _v3.a;
					var _v4 = _v3.b;
					var f22 = _v3.c;
					return A3(
						$author$project$Language$Operation,
						A2(distribute, f1, f21),
						0,
						A2(distribute, f1, f22));
				} else {
					var f1 = _v0.a;
					var f2 = _v0.b;
					return A3($author$project$Language$Operation, f1, 1, f2);
				}
			}
		});
	var noQuantorsToCNF = function (formula_) {
		_v5$2:
		while (true) {
			if (formula_.$ === 2) {
				switch (formula_.b) {
					case 0:
						var f1 = formula_.a;
						var _v6 = formula_.b;
						var f2 = formula_.c;
						return A3(
							$author$project$Language$Operation,
							noQuantorsToCNF(f1),
							0,
							noQuantorsToCNF(f2));
					case 1:
						var f1 = formula_.a;
						var _v7 = formula_.b;
						var f2 = formula_.c;
						return A2(
							distribute,
							noQuantorsToCNF(f1),
							noQuantorsToCNF(f2));
					default:
						break _v5$2;
				}
			} else {
				break _v5$2;
			}
		}
		return formula_;
	};
	if (formula.$ === 3) {
		var q = formula.a;
		var x = formula.b;
		var f = formula.c;
		return A3(
			$author$project$Language$Quantification,
			q,
			x,
			$author$project$Transformations$pNFtoCNF(f));
	} else {
		return noQuantorsToCNF(formula);
	}
};
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$Parser$UnexpectedToken = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Result$andThen = F2(
	function (callback, result) {
		if (!result.$) {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return $elm$core$Result$Err(msg);
		}
	});
var $author$project$Parser$ExpectedRightBracket = {$: 0};
var $author$project$Parser$ExpectedVariable = function (a) {
	return {$: 3, a: a};
};
var $author$project$Language$Impl = 2;
var $author$project$Language$Predicate = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Parser$UnexpectedEnd = {$: 4};
var $author$project$Language$Constant = function (a) {
	return {$: 0, a: a};
};
var $author$project$Language$Function = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Parser$PredicateInTerm = function (a) {
	return {$: 1, a: a};
};
var $author$project$Language$Variable = function (a) {
	return {$: 1, a: a};
};
var $author$project$Parser$toString = function (token) {
	switch (token.$) {
		case 0:
			var str = token.a;
			return 'Var: ' + str;
		case 1:
			var str = token.a;
			return 'Const: ' + str;
		case 2:
			var str = token.a;
			return 'Func: ' + str;
		case 3:
			var str = token.a;
			return 'Pred: ' + str;
		case 4:
			return '(';
		case 5:
			return ')';
		case 6:
			return '∀';
		case 7:
			return '∃';
		case 8:
			return '&';
		case 9:
			return '∨';
		case 11:
			return '⇔';
		case 10:
			return '⇒';
		case 12:
			return '¬';
		default:
			return ',';
	}
};
var $author$project$Parser$parseTerm = function (tokens) {
	_v6$4:
	while (true) {
		if (tokens.b) {
			switch (tokens.a.$) {
				case 1:
					var string = tokens.a.a;
					var rest = tokens.b;
					return $elm$core$Result$Ok(
						_Utils_Tuple2(
							$author$project$Language$Constant(string),
							rest));
				case 0:
					var string = tokens.a.a;
					var rest = tokens.b;
					return $elm$core$Result$Ok(
						_Utils_Tuple2(
							$author$project$Language$Variable(string),
							rest));
				case 2:
					if (tokens.b.b && (tokens.b.a.$ === 4)) {
						var string = tokens.a.a;
						var _v7 = tokens.b;
						var _v8 = _v7.a;
						var rest = _v7.b;
						var _v9 = $author$project$Parser$parseTermList(rest);
						if (!_v9.$) {
							var _v10 = _v9.a;
							var terms = _v10.a;
							var restTokens = _v10.b;
							if (restTokens.b && (restTokens.a.$ === 5)) {
								var _v12 = restTokens.a;
								var finalRest = restTokens.b;
								return $elm$core$Result$Ok(
									_Utils_Tuple2(
										A2($author$project$Language$Function, string, terms),
										finalRest));
							} else {
								return $elm$core$Result$Err($author$project$Parser$ExpectedRightBracket);
							}
						} else {
							var e = _v9.a;
							return $elm$core$Result$Err(e);
						}
					} else {
						break _v6$4;
					}
				case 3:
					var p = tokens.a.a;
					return $elm$core$Result$Err(
						$author$project$Parser$PredicateInTerm(p));
				default:
					break _v6$4;
			}
		} else {
			return $elm$core$Result$Err($author$project$Parser$UnexpectedEnd);
		}
	}
	var t = tokens.a;
	return $elm$core$Result$Err(
		$author$project$Parser$UnexpectedToken(
			$author$project$Parser$toString(t)));
};
var $author$project$Parser$parseTermList = function (tokens) {
	var _v0 = $author$project$Parser$parseTerm(tokens);
	if (!_v0.$) {
		var _v1 = _v0.a;
		var firstTerm = _v1.a;
		var rest = _v1.b;
		if (rest.b && (rest.a.$ === 13)) {
			var _v3 = rest.a;
			var restTokens = rest.b;
			var _v4 = $author$project$Parser$parseTermList(restTokens);
			if (!_v4.$) {
				var _v5 = _v4.a;
				var termList = _v5.a;
				var finalRest = _v5.b;
				return $elm$core$Result$Ok(
					_Utils_Tuple2(
						A2($elm$core$List$cons, firstTerm, termList),
						finalRest));
			} else {
				var e = _v4.a;
				return $elm$core$Result$Err(e);
			}
		} else {
			return $elm$core$Result$Ok(
				_Utils_Tuple2(
					_List_fromArray(
						[firstTerm]),
					rest));
		}
	} else {
		var e = _v0.a;
		return $elm$core$Result$Err(e);
	}
};
var $author$project$Parser$parseFormula = function (tokens) {
	var resultFirstFormula = function () {
		_v14$9:
		while (true) {
			if (!tokens.b) {
				return $elm$core$Result$Err($author$project$Parser$UnexpectedEnd);
			} else {
				switch (tokens.a.$) {
					case 4:
						var _v15 = tokens.a;
						var rest = tokens.b;
						var _v16 = $author$project$Parser$parseFormula(rest);
						if (!_v16.$) {
							var _v17 = _v16.a;
							var f = _v17.a;
							var restTokens = _v17.b;
							if (restTokens.b && (restTokens.a.$ === 5)) {
								var _v19 = restTokens.a;
								var finalRest = restTokens.b;
								return $elm$core$Result$Ok(
									_Utils_Tuple2(f, finalRest));
							} else {
								return $elm$core$Result$Err($author$project$Parser$ExpectedRightBracket);
							}
						} else {
							var e = _v16.a;
							return $elm$core$Result$Err(e);
						}
					case 12:
						var _v20 = tokens.a;
						var rest = tokens.b;
						var _v21 = $author$project$Parser$parseFormula(rest);
						if (!_v21.$) {
							if (_v21.a.a.$ === 2) {
								var _v22 = _v21.a;
								var _v23 = _v22.a;
								var f1 = _v23.a;
								var op = _v23.b;
								var f2 = _v23.c;
								var restTokens = _v22.b;
								return $elm$core$Result$Ok(
									_Utils_Tuple2(
										A3(
											$author$project$Language$Operation,
											$author$project$Language$Negation(f1),
											op,
											f2),
										restTokens));
							} else {
								var _v24 = _v21.a;
								var formula = _v24.a;
								var restTokens = _v24.b;
								return $elm$core$Result$Ok(
									_Utils_Tuple2(
										$author$project$Language$Negation(formula),
										restTokens));
							}
						} else {
							var e = _v21.a;
							return $elm$core$Result$Err(e);
						}
					case 3:
						if (tokens.b.b && (tokens.b.a.$ === 4)) {
							if (tokens.b.b.b && (tokens.b.b.a.$ === 5)) {
								var x = tokens.a.a;
								var _v25 = tokens.b;
								var _v26 = _v25.a;
								var _v27 = _v25.b;
								var _v28 = _v27.a;
								var rest = _v27.b;
								return $elm$core$Result$Ok(
									_Utils_Tuple2(
										A2($author$project$Language$Predicate, x, _List_Nil),
										rest));
							} else {
								var x = tokens.a.a;
								var _v29 = tokens.b;
								var _v30 = _v29.a;
								var rest = _v29.b;
								var _v31 = $author$project$Parser$parseTermList(rest);
								if (!_v31.$) {
									var _v32 = _v31.a;
									var terms = _v32.a;
									var restTokens = _v32.b;
									if (restTokens.b && (restTokens.a.$ === 5)) {
										var _v34 = restTokens.a;
										var finalRest = restTokens.b;
										return $elm$core$Result$Ok(
											_Utils_Tuple2(
												A2($author$project$Language$Predicate, x, terms),
												finalRest));
									} else {
										return $elm$core$Result$Err($author$project$Parser$ExpectedRightBracket);
									}
								} else {
									var e = _v31.a;
									return $elm$core$Result$Err(e);
								}
							}
						} else {
							break _v14$9;
						}
					case 6:
						if (tokens.b.b) {
							if (!tokens.b.a.$) {
								var _v35 = tokens.a;
								var _v36 = tokens.b;
								var x = _v36.a.a;
								var rest = _v36.b;
								var _v37 = $author$project$Parser$parseFormula(rest);
								if (!_v37.$) {
									var _v38 = _v37.a;
									var f = _v38.a;
									var restTokens = _v38.b;
									return $elm$core$Result$Ok(
										_Utils_Tuple2(
											A3($author$project$Language$Quantification, 1, x, f),
											restTokens));
								} else {
									var e = _v37.a;
									return $elm$core$Result$Err(e);
								}
							} else {
								var _v39 = tokens.a;
								var _v40 = tokens.b;
								var t = _v40.a;
								return $elm$core$Result$Err(
									$author$project$Parser$ExpectedVariable(
										$author$project$Parser$toString(t)));
							}
						} else {
							break _v14$9;
						}
					case 7:
						if (tokens.b.b) {
							if (!tokens.b.a.$) {
								var _v41 = tokens.a;
								var _v42 = tokens.b;
								var x = _v42.a.a;
								var rest = _v42.b;
								var _v43 = $author$project$Parser$parseFormula(rest);
								if (!_v43.$) {
									var _v44 = _v43.a;
									var f = _v44.a;
									var restTokens = _v44.b;
									return $elm$core$Result$Ok(
										_Utils_Tuple2(
											A3($author$project$Language$Quantification, 0, x, f),
											restTokens));
								} else {
									var e = _v43.a;
									return $elm$core$Result$Err(e);
								}
							} else {
								var _v45 = tokens.a;
								var _v46 = tokens.b;
								var t = _v46.a;
								return $elm$core$Result$Err(
									$author$project$Parser$ExpectedVariable(
										$author$project$Parser$toString(t)));
							}
						} else {
							break _v14$9;
						}
					default:
						break _v14$9;
				}
			}
		}
		var t = tokens.a;
		return $elm$core$Result$Err(
			$author$project$Parser$UnexpectedToken(
				$author$project$Parser$toString(t)));
	}();
	return A2(
		$elm$core$Result$andThen,
		function (_v0) {
			var firstFormula = _v0.a;
			var rest = _v0.b;
			_v1$4:
			while (true) {
				if (rest.b) {
					switch (rest.a.$) {
						case 8:
							var _v2 = rest.a;
							var restTokens = rest.b;
							var _v3 = $author$project$Parser$parseFormula(restTokens);
							if (!_v3.$) {
								var _v4 = _v3.a;
								var secondFormula = _v4.a;
								var finalTokens = _v4.b;
								return $elm$core$Result$Ok(
									_Utils_Tuple2(
										A3($author$project$Language$Operation, firstFormula, 0, secondFormula),
										finalTokens));
							} else {
								var e = _v3.a;
								return $elm$core$Result$Err(e);
							}
						case 9:
							var _v5 = rest.a;
							var restTokens = rest.b;
							var _v6 = $author$project$Parser$parseFormula(restTokens);
							if (!_v6.$) {
								var _v7 = _v6.a;
								var secondFormula = _v7.a;
								var finalTokens = _v7.b;
								return $elm$core$Result$Ok(
									_Utils_Tuple2(
										A3($author$project$Language$Operation, firstFormula, 1, secondFormula),
										finalTokens));
							} else {
								var e = _v6.a;
								return $elm$core$Result$Err(e);
							}
						case 10:
							var _v8 = rest.a;
							var restTokens = rest.b;
							var _v9 = $author$project$Parser$parseFormula(restTokens);
							if (!_v9.$) {
								var _v10 = _v9.a;
								var secondFormula = _v10.a;
								var finalTokens = _v10.b;
								return $elm$core$Result$Ok(
									_Utils_Tuple2(
										A3($author$project$Language$Operation, firstFormula, 2, secondFormula),
										finalTokens));
							} else {
								var e = _v9.a;
								return $elm$core$Result$Err(e);
							}
						case 11:
							var _v11 = rest.a;
							var restTokens = rest.b;
							var _v12 = $author$project$Parser$parseFormula(restTokens);
							if (!_v12.$) {
								var _v13 = _v12.a;
								var secondFormula = _v13.a;
								var finalTokens = _v13.b;
								return $elm$core$Result$Ok(
									_Utils_Tuple2(
										A3($author$project$Language$Operation, firstFormula, 3, secondFormula),
										finalTokens));
							} else {
								var e = _v12.a;
								return $elm$core$Result$Err(e);
							}
						default:
							break _v1$4;
					}
				} else {
					break _v1$4;
				}
			}
			return $elm$core$Result$Ok(
				_Utils_Tuple2(firstFormula, rest));
		},
		resultFirstFormula);
};
var $author$project$Parser$And = {$: 8};
var $author$project$Parser$Comma = {$: 13};
var $author$project$Parser$Const = function (a) {
	return {$: 1, a: a};
};
var $author$project$Parser$Eqv = {$: 11};
var $author$project$Parser$Exists = {$: 7};
var $author$project$Parser$ForAll = {$: 6};
var $author$project$Parser$Func = function (a) {
	return {$: 2, a: a};
};
var $author$project$Parser$Impl = {$: 10};
var $author$project$Parser$LeftBracket = {$: 4};
var $author$project$Parser$Not = {$: 12};
var $author$project$Parser$Or = {$: 9};
var $author$project$Parser$Pred = function (a) {
	return {$: 3, a: a};
};
var $author$project$Parser$RightBracket = {$: 5};
var $author$project$Parser$Var = function (a) {
	return {$: 0, a: a};
};
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$core$String$foldr = _String_foldr;
var $elm$core$Result$map2 = F3(
	function (func, ra, rb) {
		if (ra.$ === 1) {
			var x = ra.a;
			return $elm$core$Result$Err(x);
		} else {
			var a = ra.a;
			if (rb.$ === 1) {
				var x = rb.a;
				return $elm$core$Result$Err(x);
			} else {
				var b = rb.a;
				return $elm$core$Result$Ok(
					A2(func, a, b));
			}
		}
	});
var $author$project$ListHelperFunctions$listOfResultToResultList = function (listOfResults) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Result$map2($elm$core$List$cons),
		$elm$core$Result$Ok(_List_Nil),
		listOfResults);
};
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
var $elm$core$String$words = _String_words;
var $author$project$Parser$tokenizer = F2(
	function (lang, str) {
		var basicTokens = A3(
			$elm$core$String$foldr,
			F2(
				function (el, acc) {
					switch (el) {
						case '(':
							return A2($elm$core$List$cons, $author$project$Parser$LeftBracket, acc);
						case ')':
							return A2($elm$core$List$cons, $author$project$Parser$RightBracket, acc);
						case '∀':
							return A2($elm$core$List$cons, $author$project$Parser$ForAll, acc);
						case '∃':
							return A2($elm$core$List$cons, $author$project$Parser$Exists, acc);
						case '&':
							return A2($elm$core$List$cons, $author$project$Parser$And, acc);
						case '∨':
							return A2($elm$core$List$cons, $author$project$Parser$Or, acc);
						case '⇔':
							return A2($elm$core$List$cons, $author$project$Parser$Eqv, acc);
						case '⇒':
							return A2($elm$core$List$cons, $author$project$Parser$Impl, acc);
						case '¬':
							return A2($elm$core$List$cons, $author$project$Parser$Not, acc);
						case ',':
							return A2($elm$core$List$cons, $author$project$Parser$Comma, acc);
						default:
							var c = el;
							if (acc.b && (!acc.a.$)) {
								var restName = acc.a.a;
								var rest = acc.b;
								return A2(
									$elm$core$List$cons,
									$author$project$Parser$Var(
										$elm$core$String$concat(
											_List_fromArray(
												[
													$elm$core$String$fromChar(c),
													restName
												]))),
									rest);
							} else {
								return $elm$core$Char$isAlphaNum(c) ? A2(
									$elm$core$List$cons,
									$author$project$Parser$Var(
										$elm$core$String$fromChar(c)),
									acc) : acc;
							}
					}
				}),
			_List_Nil,
			str);
		return $author$project$ListHelperFunctions$listOfResultToResultList(
			A2(
				$elm$core$List$map,
				function (token) {
					if (!token.$) {
						var string = token.a;
						return A2($elm$core$Set$member, string, lang.aL) ? $elm$core$Result$Ok(
							$author$project$Parser$Const(string)) : (A2($elm$core$Set$member, string, lang.aQ) ? $elm$core$Result$Ok(
							$author$project$Parser$Func(string)) : (A2($elm$core$Set$member, string, lang.aW) ? $elm$core$Result$Ok(
							$author$project$Parser$Pred(string)) : (A2($elm$core$Set$member, string, lang.a1) ? $elm$core$Result$Ok(
							$author$project$Parser$Var(string)) : $elm$core$Result$Err(
							$author$project$Parser$UnexpectedToken(string)))));
					} else {
						return $elm$core$Result$Ok(token);
					}
				},
				A3(
					$elm$core$List$foldr,
					F2(
						function (token, acc) {
							if (!token.$) {
								var word = token.a;
								return A3(
									$elm$core$List$foldr,
									$elm$core$List$cons,
									acc,
									A2(
										$elm$core$List$map,
										$author$project$Parser$Var,
										$elm$core$String$words(word)));
							} else {
								return A2($elm$core$List$cons, token, acc);
							}
						}),
					_List_Nil,
					basicTokens)));
	});
var $author$project$Parser$parse = F2(
	function (lang, string) {
		var _v0 = A2(
			$elm$core$Result$andThen,
			$author$project$Parser$parseFormula,
			A2($author$project$Parser$tokenizer, lang, string));
		if (!_v0.$) {
			if (!_v0.a.b.b) {
				var _v1 = _v0.a;
				var formula = _v1.a;
				return $elm$core$Result$Ok(formula);
			} else {
				var _v2 = _v0.a;
				var _v3 = _v2.b;
				var x = _v3.a;
				return $elm$core$Result$Err(
					$author$project$Parser$UnexpectedToken(
						$author$project$Parser$toString(x)));
			}
		} else {
			var e = _v0.a;
			return $elm$core$Result$Err(e);
		}
	});
var $author$project$Language$printOp = function (op) {
	switch (op) {
		case 0:
			return '&';
		case 1:
			return '∨';
		case 2:
			return '⇒';
		default:
			return '⇔';
	}
};
var $author$project$Language$printFormula = function (f) {
	switch (f.$) {
		case 0:
			var p = f.a;
			var terms = f.b;
			var _v1 = A3(
				$elm$core$List$foldl,
				F2(
					function (x, acc) {
						return acc + ($author$project$Language$printTerm(x) + ', ');
					}),
				'',
				terms);
			if (_v1 === '') {
				return p;
			} else {
				var list = _v1;
				return p + ('(' + (A2($elm$core$String$dropRight, 2, list) + ')'));
			}
		case 1:
			var f1 = f.a;
			return '¬' + $author$project$Language$printFormula(f1);
		case 2:
			var f1 = f.a;
			var op = f.b;
			var f2 = f.c;
			return '(' + ($author$project$Language$printFormula(f1) + (' ' + ($author$project$Language$printOp(op) + (' ' + ($author$project$Language$printFormula(f2) + ')')))));
		default:
			if (!f.a) {
				var _v2 = f.a;
				var x = f.b;
				var f1 = f.c;
				return '∃' + (x + (' ' + $author$project$Language$printFormula(f1)));
			} else {
				var _v3 = f.a;
				var x = f.b;
				var f1 = f.c;
				return '∀' + (x + (' ' + $author$project$Language$printFormula(f1)));
			}
	}
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
var $elm$core$Set$foldl = F3(
	function (func, initialState, _v0) {
		var dict = _v0;
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (key, _v1, state) {
					return A2(func, key, state);
				}),
			initialState,
			dict);
	});
var $author$project$Language$printLanguage = function (l) {
	var printSet = function (set) {
		return '{' + (A2(
			$elm$core$String$dropRight,
			2,
			A3(
				$elm$core$Set$foldl,
				F2(
					function (x, acc) {
						return acc + (x + ', ');
					}),
				'',
				set)) + '}');
	};
	return '{Vars: ' + (printSet(l.a1) + (', Preds: ' + (printSet(l.aW) + (', Consts: ' + (printSet(l.aL) + (', Funcs: ' + (printSet(l.aQ) + '}')))))));
};
var $turboMaCk$any_dict$Dict$Any$foldl = F3(
	function (f, acc, _v0) {
		var dict = _v0.b;
		return A3(
			$elm$core$Dict$foldl,
			F2(
				function (_v1, _v2) {
					var k = _v2.a;
					var v = _v2.b;
					return A2(f, k, v);
				}),
			acc,
			dict);
	});
var $turboMaCk$any_dict$Dict$Any$any = F2(
	function (predicate, dict) {
		return A3(
			$turboMaCk$any_dict$Dict$Any$foldl,
			F3(
				function (k, v, acc) {
					return acc || A2(predicate, k, v);
				}),
			false,
			dict);
	});
var $turboMaCk$any_set$Set$Any$any = F2(
	function (predicate, _v0) {
		var dict = _v0;
		return A2(
			$turboMaCk$any_dict$Dict$Any$any,
			F2(
				function (k, _v1) {
					return predicate(k);
				}),
			dict);
	});
var $author$project$DisjunctSet$any = function (pred) {
	return $turboMaCk$any_set$Set$Any$any(
		function (_v0) {
			var d = _v0.b;
			return pred(d);
		});
};
var $author$project$Search$final = function (node) {
	return A2($author$project$DisjunctSet$any, $author$project$Disjunct$isEmpty, node.A);
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $author$project$ResolutionStep$Col = F3(
	function (a, b, c) {
		return {$: 1, a: a, b: b, c: c};
	});
var $turboMaCk$any_set$Set$Any$foldl = F3(
	function (f, init, _v0) {
		var dict = _v0;
		return A3(
			$turboMaCk$any_dict$Dict$Any$foldl,
			F3(
				function (x, _v1, acc) {
					return A2(f, x, acc);
				}),
			init,
			dict);
	});
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
var $turboMaCk$any_dict$Dict$Any$fromList = F2(
	function (f, xs) {
		return {
			b: $elm$core$Dict$fromList(
				A2(
					$elm$core$List$map,
					function (_v0) {
						var k = _v0.a;
						var v = _v0.b;
						return _Utils_Tuple2(
							f(k),
							_Utils_Tuple2(k, v));
					},
					xs)),
			w: f
		};
	});
var $turboMaCk$any_set$Set$Any$fromList = function (toComparable) {
	return A2(
		$elm$core$Basics$composeL,
		A2(
			$elm$core$Basics$composeL,
			$elm$core$Basics$identity,
			$turboMaCk$any_dict$Dict$Any$fromList(toComparable)),
		$elm$core$List$map(
			function (a) {
				return _Utils_Tuple2(a, 0);
			}));
};
var $turboMaCk$any_set$Set$Any$map = F2(
	function (toComparable, f) {
		return A2(
			$elm$core$Basics$composeL,
			$turboMaCk$any_set$Set$Any$fromList(toComparable),
			A2(
				$turboMaCk$any_set$Set$Any$foldl,
				F2(
					function (x, xs) {
						return A2(
							$elm$core$List$cons,
							f(x),
							xs);
					}),
				_List_Nil));
	});
var $author$project$Disjunct$map = F2(
	function (f, d) {
		return A3($turboMaCk$any_set$Set$Any$map, $author$project$Language$literalToString, f, d);
	});
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
var $turboMaCk$any_dict$Dict$Any$remove = F2(
	function (k, _v0) {
		var inner = _v0;
		return _Utils_update(
			inner,
			{
				b: A2(
					$elm$core$Dict$remove,
					inner.w(k),
					inner.b)
			});
	});
var $turboMaCk$any_set$Set$Any$remove = F2(
	function (a, _v0) {
		var dict = _v0;
		return A2($turboMaCk$any_dict$Dict$Any$remove, a, dict);
	});
var $author$project$Disjunct$remove = $turboMaCk$any_set$Set$Any$remove;
var $author$project$Language$NegativePredicate = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Language$PositivePredicate = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Transformations$replaceInTerm = F2(
	function (substitutions, term) {
		var replaceSingle = F2(
			function (_v0, t) {
				var varToRepl = _v0.a;
				var subTerm = _v0.b;
				switch (t.$) {
					case 1:
						var _var = t.a;
						return _Utils_eq(_var, varToRepl) ? subTerm : t;
					case 2:
						var f = t.a;
						var terms = t.b;
						return A2(
							$author$project$Language$Function,
							f,
							A2(
								$elm$core$List$map,
								replaceSingle(
									_Utils_Tuple2(varToRepl, subTerm)),
								terms));
					default:
						return t;
				}
			});
		return A3($elm$core$List$foldl, replaceSingle, term, substitutions);
	});
var $author$project$Unification$replaceInListTerms = F2(
	function (sub, list) {
		return A2(
			$elm$core$List$map,
			$author$project$Transformations$replaceInTerm(sub),
			list);
	});
var $author$project$Unification$replaceInLiteral = F2(
	function (sub, l) {
		if (!l.$) {
			var name = l.a;
			var terms = l.b;
			return A2(
				$author$project$Language$PositivePredicate,
				name,
				A2($author$project$Unification$replaceInListTerms, sub, terms));
		} else {
			var name = l.a;
			var terms = l.b;
			return A2(
				$author$project$Language$NegativePredicate,
				name,
				A2($author$project$Unification$replaceInListTerms, sub, terms));
		}
	});
var $author$project$ListHelperFunctions$subsetsOf2 = function (list) {
	if (!list.b) {
		return _List_Nil;
	} else {
		var x = list.a;
		var xs = list.b;
		return _Utils_ap(
			A2(
				$elm$core$List$map,
				function (y) {
					return _Utils_Tuple2(x, y);
				},
				xs),
			$author$project$ListHelperFunctions$subsetsOf2(xs));
	}
};
var $author$project$Disjunct$toList = $turboMaCk$any_set$Set$Any$toList;
var $author$project$Substitution$empty = _List_Nil;
var $author$project$Unification$DifferentSubTerms = function (a) {
	return {$: 1, a: a};
};
var $author$project$Unification$DifferentTerms = {$: 2};
var $author$project$Unification$Equal = {$: 0};
var $author$project$Unification$firstDifferenceInListOfTerms = F2(
	function (terms1, terms2) {
		firstDifferenceInListOfTerms:
		while (true) {
			var _v5 = _Utils_Tuple2(terms1, terms2);
			_v5$2:
			while (true) {
				if (!_v5.a.b) {
					if (!_v5.b.b) {
						return $author$project$Unification$Equal;
					} else {
						break _v5$2;
					}
				} else {
					if (_v5.b.b) {
						var _v6 = _v5.a;
						var x = _v6.a;
						var xs = _v6.b;
						var _v7 = _v5.b;
						var y = _v7.a;
						var ys = _v7.b;
						var _v8 = A2($author$project$Unification$firstDifferenceOfTerms, x, y);
						if (_v8.$ === 1) {
							var $temp$terms1 = xs,
								$temp$terms2 = ys;
							terms1 = $temp$terms1;
							terms2 = $temp$terms2;
							continue firstDifferenceInListOfTerms;
						} else {
							var _v9 = _v8.a;
							var t1 = _v9.a;
							var t2 = _v9.b;
							return $author$project$Unification$DifferentSubTerms(
								_Utils_Tuple2(t1, t2));
						}
					} else {
						break _v5$2;
					}
				}
			}
			return $author$project$Unification$DifferentTerms;
		}
	});
var $author$project$Unification$firstDifferenceOfTerms = F2(
	function (t1, t2) {
		var _v0 = _Utils_Tuple2(t1, t2);
		_v0$3:
		while (true) {
			switch (_v0.a.$) {
				case 1:
					if (_v0.b.$ === 1) {
						var x = _v0.a.a;
						var y = _v0.b.a;
						return _Utils_eq(x, y) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
							_Utils_Tuple2(t1, t2));
					} else {
						break _v0$3;
					}
				case 0:
					if (!_v0.b.$) {
						var x = _v0.a.a;
						var y = _v0.b.a;
						return _Utils_eq(x, y) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
							_Utils_Tuple2(t1, t2));
					} else {
						break _v0$3;
					}
				default:
					if (_v0.b.$ === 2) {
						var _v1 = _v0.a;
						var f = _v1.a;
						var terms1 = _v1.b;
						var _v2 = _v0.b;
						var g = _v2.a;
						var terms2 = _v2.b;
						if (!_Utils_eq(f, g)) {
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(t1, t2));
						} else {
							var _v3 = A2($author$project$Unification$firstDifferenceInListOfTerms, terms1, terms2);
							switch (_v3.$) {
								case 0:
									return $elm$core$Maybe$Nothing;
								case 1:
									var _v4 = _v3.a;
									var t11 = _v4.a;
									var t21 = _v4.b;
									return $elm$core$Maybe$Just(
										_Utils_Tuple2(t11, t21));
								default:
									return $elm$core$Maybe$Just(
										_Utils_Tuple2(t1, t2));
							}
						}
					} else {
						break _v0$3;
					}
			}
		}
		return $elm$core$Maybe$Just(
			_Utils_Tuple2(t1, t2));
	});
var $author$project$Substitution$insert = F3(
	function (x, t, sub) {
		return _Utils_ap(
			sub,
			_List_fromArray(
				[
					_Utils_Tuple2(x, t)
				]));
	});
var $elm$core$Basics$not = _Basics_not;
var $author$project$Substitution$singleton = F2(
	function (x, t) {
		return _List_fromArray(
			[
				_Utils_Tuple2(x, t)
			]);
	});
var $elm$core$Dict$singleton = F2(
	function (key, value) {
		return A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
	});
var $elm$core$Set$singleton = function (key) {
	return A2($elm$core$Dict$singleton, key, 0);
};
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$core$Set$union = F2(
	function (_v0, _v1) {
		var dict1 = _v0;
		var dict2 = _v1;
		return A2($elm$core$Dict$union, dict1, dict2);
	});
var $author$project$Language$varsInTerm = function (term) {
	switch (term.$) {
		case 0:
			return $elm$core$Set$empty;
		case 1:
			var v = term.a;
			return $elm$core$Set$singleton(v);
		default:
			var terms = term.b;
			return A3(
				$elm$core$List$foldr,
				F2(
					function (x, acc) {
						return A2(
							$elm$core$Set$union,
							$author$project$Language$varsInTerm(x),
							acc);
					}),
				$elm$core$Set$empty,
				terms);
	}
};
var $author$project$Unification$unification = F2(
	function (p, q) {
		var unificationHelper = F3(
			function (l1, l2, maybeSub) {
				return A2(
					$elm$core$Maybe$andThen,
					function (sub) {
						var _v0 = A2($author$project$Unification$firstDifferenceInListOfTerms, l1, l2);
						switch (_v0.$) {
							case 0:
								return $elm$core$Maybe$Just(sub);
							case 2:
								return $elm$core$Maybe$Nothing;
							default:
								var _v1 = _v0.a;
								var t1 = _v1.a;
								var t2 = _v1.b;
								var substituteAndCallRecursive = F2(
									function (x, t) {
										return A3(
											unificationHelper,
											A2(
												$author$project$Unification$replaceInListTerms,
												A2($author$project$Substitution$singleton, x, t),
												l1),
											A2(
												$author$project$Unification$replaceInListTerms,
												A2($author$project$Substitution$singleton, x, t),
												l2),
											$elm$core$Maybe$Just(
												A3($author$project$Substitution$insert, x, t, sub)));
									});
								var _v2 = _Utils_Tuple2(t1, t2);
								if (_v2.a.$ === 1) {
									if (_v2.b.$ === 1) {
										var x = _v2.a.a;
										return A2(substituteAndCallRecursive, x, t2);
									} else {
										var x = _v2.a.a;
										var t = _v2.b;
										return (!A2(
											$elm$core$Set$member,
											x,
											$author$project$Language$varsInTerm(t))) ? A2(substituteAndCallRecursive, x, t) : $elm$core$Maybe$Nothing;
									}
								} else {
									if (_v2.b.$ === 1) {
										var t = _v2.a;
										var x = _v2.b.a;
										return (!A2(
											$elm$core$Set$member,
											x,
											$author$project$Language$varsInTerm(t))) ? A2(substituteAndCallRecursive, x, t) : $elm$core$Maybe$Nothing;
									} else {
										return $elm$core$Maybe$Nothing;
									}
								}
						}
					},
					maybeSub);
			});
		var _v3 = _Utils_Tuple2(p, q);
		_v3$2:
		while (true) {
			if (!_v3.a.$) {
				if (_v3.b.$ === 1) {
					var _v4 = _v3.a;
					var pName = _v4.a;
					var list1 = _v4.b;
					var _v5 = _v3.b;
					var qName = _v5.a;
					var list2 = _v5.b;
					return (!_Utils_eq(pName, qName)) ? $elm$core$Maybe$Nothing : A3(
						unificationHelper,
						list1,
						list2,
						$elm$core$Maybe$Just($author$project$Substitution$empty));
				} else {
					break _v3$2;
				}
			} else {
				if (!_v3.b.$) {
					var _v6 = _v3.a;
					var pName = _v6.a;
					var list1 = _v6.b;
					var _v7 = _v3.b;
					var qName = _v7.a;
					var list2 = _v7.b;
					return (!_Utils_eq(pName, qName)) ? $elm$core$Maybe$Nothing : A3(
						unificationHelper,
						list1,
						list2,
						$elm$core$Maybe$Just($author$project$Substitution$empty));
				} else {
					break _v3$2;
				}
			}
		}
		return $elm$core$Maybe$Nothing;
	});
var $author$project$ResolutionStep$colapses = function (_v0) {
	var i = _v0.a;
	var d = _v0.b;
	var dList = $author$project$Disjunct$toList(d);
	return A3(
		$elm$core$List$foldr,
		F2(
			function (_v1, acc) {
				var l1 = _v1.a;
				var l2 = _v1.b;
				var _v2 = A2($author$project$Unification$unification, l1, l2);
				if (_v2.$ === 1) {
					return acc;
				} else {
					var sub = _v2.a;
					var newD = A2(
						$author$project$Disjunct$remove,
						l2,
						A2($author$project$Disjunct$remove, l1, d));
					var colapse = A2(
						$author$project$Disjunct$map,
						$author$project$Unification$replaceInLiteral(sub),
						newD);
					return A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							colapse,
							A3(
								$author$project$ResolutionStep$Col,
								_Utils_Tuple2(i, d),
								sub,
								colapse)),
						acc);
				}
			}),
		_List_Nil,
		$author$project$ListHelperFunctions$subsetsOf2(dList));
};
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
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
var $turboMaCk$any_dict$Dict$Any$insert = F3(
	function (k, v, _v0) {
		var inner = _v0;
		return _Utils_update(
			inner,
			{
				b: A3(
					$elm$core$Dict$insert,
					inner.w(k),
					_Utils_Tuple2(k, v),
					inner.b)
			});
	});
var $turboMaCk$any_set$Set$Any$insert = F2(
	function (a, _v0) {
		var dict = _v0;
		return A3($turboMaCk$any_dict$Dict$Any$insert, a, 0, dict);
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
var $turboMaCk$any_dict$Dict$Any$size = function (_v0) {
	var dict = _v0.b;
	return $elm$core$Dict$size(dict);
};
var $turboMaCk$any_set$Set$Any$size = function (_v0) {
	var dict = _v0;
	return $turboMaCk$any_dict$Dict$Any$size(dict);
};
var $author$project$DisjunctSet$insert = F2(
	function (d, ds) {
		return A2(
			$turboMaCk$any_set$Set$Any$insert,
			_Utils_Tuple2(
				$turboMaCk$any_set$Set$Any$size(ds),
				d),
			ds);
	});
var $author$project$Disjunct$indexToString = function (_v0) {
	var i = _v0.a;
	return 'D' + $elm$core$String$fromInt(i);
};
var $author$project$Unification$subToString = function (sub) {
	return '{' + (A2(
		$elm$core$String$dropRight,
		2,
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, acc) {
					var x = _v0.a;
					var t = _v0.b;
					return acc + (x + ('/' + ($author$project$Language$printTerm(t) + ', ')));
				}),
			'',
			sub)) + '}');
};
var $author$project$ResolutionStep$printLogEntry = F2(
	function (le, index) {
		if (!le.$) {
			var d1 = le.a;
			var d2 = le.b;
			var sub = le.c;
			var resD = le.d;
			return _Utils_Tuple2(
				$author$project$Unification$subToString(sub),
				'D' + ($elm$core$String$fromInt(index) + (' = Res(' + ($author$project$Disjunct$indexToString(d1) + (', ' + ($author$project$Disjunct$indexToString(d2) + (') = ' + $author$project$Disjunct$toString(resD))))))));
		} else {
			var d = le.a;
			var sub = le.b;
			var resD = le.c;
			return _Utils_Tuple2(
				$author$project$Unification$subToString(sub),
				'D' + ($elm$core$String$fromInt(index) + (' = Col(' + ($author$project$Disjunct$indexToString(d) + (') = ' + $author$project$Disjunct$toString(resD))))));
		}
	});
var $author$project$ResolutionStep$Res = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $author$project$ListHelperFunctions$allPairs = F2(
	function (list1, list2) {
		return A2(
			$elm$core$List$concatMap,
			function (x) {
				return A2(
					$elm$core$List$map,
					function (y) {
						return _Utils_Tuple2(x, y);
					},
					list2);
			},
			list1);
	});
var $turboMaCk$any_dict$Dict$Any$union = F2(
	function (_v0, _v1) {
		var inner = _v0;
		var dict = _v1.b;
		return _Utils_update(
			inner,
			{
				b: A2($elm$core$Dict$union, inner.b, dict)
			});
	});
var $turboMaCk$any_set$Set$Any$union = F2(
	function (_v0, _v1) {
		var d1 = _v0;
		var d2 = _v1;
		return A2($turboMaCk$any_dict$Dict$Any$union, d1, d2);
	});
var $author$project$Disjunct$union = $turboMaCk$any_set$Set$Any$union;
var $author$project$ResolutionStep$resolvents = F2(
	function (_v0, _v1) {
		var i1 = _v0.a;
		var d1 = _v0.b;
		var i2 = _v1.a;
		var d2 = _v1.b;
		var d2List = $author$project$Disjunct$toList(d2);
		var d1List = $author$project$Disjunct$toList(d1);
		return A3(
			$elm$core$List$foldr,
			F2(
				function (_v2, acc) {
					var l1 = _v2.a;
					var l2 = _v2.b;
					var _v3 = A2($author$project$Unification$unification, l1, l2);
					if (_v3.$ === 1) {
						return acc;
					} else {
						var sub = _v3.a;
						var newD2 = A2($author$project$Disjunct$remove, l2, d2);
						var newD1 = A2($author$project$Disjunct$remove, l1, d1);
						var resolvent = A2(
							$author$project$Disjunct$union,
							A2(
								$author$project$Disjunct$map,
								$author$project$Unification$replaceInLiteral(sub),
								newD1),
							A2(
								$author$project$Disjunct$map,
								$author$project$Unification$replaceInLiteral(sub),
								newD2));
						return A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								resolvent,
								A4(
									$author$project$ResolutionStep$Res,
									_Utils_Tuple2(i1, d1),
									_Utils_Tuple2(i2, d2),
									sub,
									resolvent)),
							acc);
					}
				}),
			_List_Nil,
			A2($author$project$ListHelperFunctions$allPairs, d1List, d2List));
	});
var $author$project$DisjunctSet$size = $turboMaCk$any_set$Set$Any$size;
var $elm$core$List$sortWith = _List_sortWith;
var $author$project$DisjunctSet$toIndexedList = A2(
	$elm$core$Basics$composeR,
	$turboMaCk$any_set$Set$Any$toList,
	$elm$core$List$sortWith(
		F2(
			function (_v0, _v1) {
				var i1 = _v0.a;
				var i2 = _v1.a;
				return A2($elm$core$Basics$compare, i1, i2);
			})));
var $author$project$Search$generateChildren = function (node) {
	var disjuncts = $author$project$DisjunctSet$toIndexedList(node.A);
	return A2(
		$elm$core$List$filter,
		function (child) {
			return !_Utils_eq(child.A, node.A);
		},
		A2(
			$elm$core$List$map,
			function (_v1) {
				var d = _v1.a;
				var logEntry = _v1.b;
				return {
					A: A2($author$project$DisjunctSet$insert, d, node.A),
					al: node.al + 1,
					ab: A2(
						$elm$core$List$append,
						node.ab,
						_List_fromArray(
							[
								A2(
								$author$project$ResolutionStep$printLogEntry,
								logEntry,
								$author$project$DisjunctSet$size(node.A))
							]))
				};
			},
			A2(
				$elm$core$List$append,
				A2(
					$elm$core$List$concatMap,
					function (d) {
						return $author$project$ResolutionStep$colapses(d);
					},
					disjuncts),
				A2(
					$elm$core$List$concatMap,
					function (_v0) {
						var d1 = _v0.a;
						var d2 = _v0.b;
						return A2($author$project$ResolutionStep$resolvents, d1, d2);
					},
					$author$project$ListHelperFunctions$subsetsOf2(disjuncts)))));
};
var $fifth_postulate$priority_queue$PriorityQueue$Kernel$head = function (_v0) {
	var tree = _v0.W;
	if (!tree.$) {
		return $elm$core$Maybe$Nothing;
	} else {
		var element = tree.b;
		return $elm$core$Maybe$Just(element);
	}
};
var $fifth_postulate$priority_queue$PriorityQueue$head = function (queue) {
	return $fifth_postulate$priority_queue$PriorityQueue$Kernel$head(queue);
};
var $fifth_postulate$priority_queue$PriorityQueue$Kernel$PriorityQueue = $elm$core$Basics$identity;
var $fifth_postulate$priority_queue$PriorityQueue$Kernel$Node = F4(
	function (a, b, c, d) {
		return {$: 1, a: a, b: b, c: c, d: d};
	});
var $elm$core$Basics$ge = _Utils_ge;
var $fifth_postulate$priority_queue$PriorityQueue$Kernel$rank = function (tree) {
	if (!tree.$) {
		return 0;
	} else {
		var r = tree.a;
		return r;
	}
};
var $fifth_postulate$priority_queue$PriorityQueue$Kernel$make = F3(
	function (element, a, b) {
		return (_Utils_cmp(
			$fifth_postulate$priority_queue$PriorityQueue$Kernel$rank(a),
			$fifth_postulate$priority_queue$PriorityQueue$Kernel$rank(b)) > -1) ? A4(
			$fifth_postulate$priority_queue$PriorityQueue$Kernel$Node,
			1 + $fifth_postulate$priority_queue$PriorityQueue$Kernel$rank(b),
			element,
			a,
			b) : A4(
			$fifth_postulate$priority_queue$PriorityQueue$Kernel$Node,
			1 + $fifth_postulate$priority_queue$PriorityQueue$Kernel$rank(a),
			element,
			b,
			a);
	});
var $fifth_postulate$priority_queue$PriorityQueue$Kernel$merge = F3(
	function (priority, left, right) {
		var _v0 = _Utils_Tuple2(left, right);
		if (!_v0.a.$) {
			var _v1 = _v0.a;
			return right;
		} else {
			if (!_v0.b.$) {
				var _v2 = _v0.b;
				return left;
			} else {
				var _v3 = _v0.a;
				var x = _v3.b;
				var a = _v3.c;
				var b = _v3.d;
				var _v4 = _v0.b;
				var y = _v4.b;
				var u = _v4.c;
				var v = _v4.d;
				return (_Utils_cmp(
					priority(x),
					priority(y)) < 1) ? A3(
					$fifth_postulate$priority_queue$PriorityQueue$Kernel$make,
					x,
					a,
					A3($fifth_postulate$priority_queue$PriorityQueue$Kernel$merge, priority, b, right)) : A3(
					$fifth_postulate$priority_queue$PriorityQueue$Kernel$make,
					y,
					u,
					A3($fifth_postulate$priority_queue$PriorityQueue$Kernel$merge, priority, left, v));
			}
		}
	});
var $fifth_postulate$priority_queue$PriorityQueue$Kernel$Empty = {$: 0};
var $fifth_postulate$priority_queue$PriorityQueue$Kernel$singleton = function (element) {
	return A4($fifth_postulate$priority_queue$PriorityQueue$Kernel$Node, 1, element, $fifth_postulate$priority_queue$PriorityQueue$Kernel$Empty, $fifth_postulate$priority_queue$PriorityQueue$Kernel$Empty);
};
var $fifth_postulate$priority_queue$PriorityQueue$Kernel$insert = F2(
	function (element, _v0) {
		var priority = _v0.aq;
		var tree = _v0.W;
		var a = $fifth_postulate$priority_queue$PriorityQueue$Kernel$singleton(element);
		var merged = A3($fifth_postulate$priority_queue$PriorityQueue$Kernel$merge, priority, a, tree);
		return {aq: priority, W: merged};
	});
var $fifth_postulate$priority_queue$PriorityQueue$insert = F2(
	function (element, queue) {
		return A2($fifth_postulate$priority_queue$PriorityQueue$Kernel$insert, element, queue);
	});
var $fifth_postulate$priority_queue$PriorityQueue$Kernel$tail = function (queue) {
	var priority = queue.aq;
	var tree = queue.W;
	if (!tree.$) {
		return queue;
	} else {
		var a = tree.c;
		var b = tree.d;
		var merged = A3($fifth_postulate$priority_queue$PriorityQueue$Kernel$merge, priority, a, b);
		return {aq: priority, W: merged};
	}
};
var $fifth_postulate$priority_queue$PriorityQueue$tail = function (queue) {
	return $fifth_postulate$priority_queue$PriorityQueue$Kernel$tail(queue);
};
var $author$project$Search$aStarLoop = F2(
	function (queue, maxDepth) {
		aStarLoop:
		while (true) {
			var _v0 = $fifth_postulate$priority_queue$PriorityQueue$head(queue);
			if (!_v0.$) {
				var current = _v0.a;
				if (_Utils_cmp(
					$elm$core$List$length(current.ab),
					maxDepth) > 0) {
					return $elm$core$Maybe$Nothing;
				} else {
					if ($author$project$Search$final(current)) {
						return $elm$core$Maybe$Just(current);
					} else {
						var restQueue = $fifth_postulate$priority_queue$PriorityQueue$tail(queue);
						var children = $author$project$Search$generateChildren(current);
						var newQueue = A3($elm$core$List$foldr, $fifth_postulate$priority_queue$PriorityQueue$insert, restQueue, children);
						var $temp$queue = newQueue,
							$temp$maxDepth = maxDepth;
						queue = $temp$queue;
						maxDepth = $temp$maxDepth;
						continue aStarLoop;
					}
				}
			} else {
				return $elm$core$Maybe$Nothing;
			}
		}
	});
var $fifth_postulate$priority_queue$PriorityQueue$Kernel$emptyTree = $fifth_postulate$priority_queue$PriorityQueue$Kernel$Empty;
var $fifth_postulate$priority_queue$PriorityQueue$Kernel$empty = function (priority) {
	return {aq: priority, W: $fifth_postulate$priority_queue$PriorityQueue$Kernel$emptyTree};
};
var $fifth_postulate$priority_queue$PriorityQueue$empty = function (priority) {
	return $fifth_postulate$priority_queue$PriorityQueue$Kernel$empty(priority);
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Disjunct$size = $turboMaCk$any_set$Set$Any$size;
var $author$project$DisjunctSet$toList = A2(
	$elm$core$Basics$composeR,
	$turboMaCk$any_set$Set$Any$toList,
	$elm$core$List$map($elm$core$Tuple$second));
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Search$hScore = F2(
	function (node, maxDepth) {
		return A2(
			$elm$core$Maybe$withDefault,
			maxDepth,
			$elm$core$List$minimum(
				A2(
					$elm$core$List$map,
					$author$project$Disjunct$size,
					$author$project$DisjunctSet$toList(node.A))));
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Search$resolutionMethod = F2(
	function (startingSet, maxDepth) {
		var startNode = {A: startingSet, al: 0, ab: _List_Nil};
		var emptyQueue = $fifth_postulate$priority_queue$PriorityQueue$empty(
			function (node) {
				return node.al + A2($author$project$Search$hScore, node, maxDepth);
			});
		var initialQueue = A2($fifth_postulate$priority_queue$PriorityQueue$insert, startNode, emptyQueue);
		return A2(
			$elm$core$Maybe$map,
			function (finalNode) {
				return finalNode.ab;
			},
			A2($author$project$Search$aStarLoop, initialQueue, maxDepth));
	});
var $author$project$Generator$createGenerator = F3(
	function (fc, ff, fv) {
		return {
			aN: fc,
			aO: ff,
			aP: fv,
			as: _List_fromArray(
				['a', 'b', 'c', 'd', 'e']),
			at: _List_fromArray(
				['f', 'g', 'h', 'k', 'l', 'i', 'j']),
			au: _List_fromArray(
				['x', 'y', 'z', 'u', 'v', 't', 'w'])
		};
	});
var $elm$core$Char$fromCode = _Char_fromCode;
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$String$right = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(
			$elm$core$String$slice,
			-n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Generator$getConst = function (generator) {
	getConst:
	while (true) {
		var newState = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			$elm$core$List$tail(generator.as));
		var _const = A2(
			$elm$core$Maybe$withDefault,
			'a',
			$elm$core$List$head(generator.as));
		var newConst = function () {
			var lastLetter = A2(
				$elm$core$Maybe$withDefault,
				'0',
				A2(
					$elm$core$Maybe$map,
					$elm$core$Tuple$first,
					$elm$core$String$uncons(
						A2($elm$core$String$right, 1, _const))));
			return ($elm$core$Char$isAlpha(lastLetter) || (lastLetter === '9')) ? (_const + '0') : _Utils_ap(
				A2($elm$core$String$dropRight, 1, _const),
				$elm$core$String$fromChar(
					$elm$core$Char$fromCode(
						$elm$core$Char$toCode(lastLetter) + 1)));
		}();
		var newGenerator = _Utils_update(
			generator,
			{
				as: A2(
					$elm$core$List$append,
					newState,
					_List_fromArray(
						[newConst]))
			});
		if (A2($elm$core$Set$member, _const, generator.aN)) {
			var $temp$generator = newGenerator;
			generator = $temp$generator;
			continue getConst;
		} else {
			return _Utils_Tuple2(_const, newGenerator);
		}
	}
};
var $author$project$Generator$getFunc = function (generator) {
	getFunc:
	while (true) {
		var newState = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			$elm$core$List$tail(generator.at));
		var func = A2(
			$elm$core$Maybe$withDefault,
			'f',
			$elm$core$List$head(generator.at));
		var newFunc = function () {
			var lastLetter = A2(
				$elm$core$Maybe$withDefault,
				'0',
				A2(
					$elm$core$Maybe$map,
					$elm$core$Tuple$first,
					$elm$core$String$uncons(
						A2($elm$core$String$right, 1, func))));
			return ($elm$core$Char$isAlpha(lastLetter) || (lastLetter === '9')) ? (func + '0') : _Utils_ap(
				A2($elm$core$String$dropRight, 1, func),
				$elm$core$String$fromChar(
					$elm$core$Char$fromCode(
						$elm$core$Char$toCode(lastLetter) + 1)));
		}();
		var newGenerator = _Utils_update(
			generator,
			{
				at: A2(
					$elm$core$List$append,
					newState,
					_List_fromArray(
						[newFunc]))
			});
		if (A2($elm$core$Set$member, func, generator.aO)) {
			var $temp$generator = newGenerator;
			generator = $temp$generator;
			continue getFunc;
		} else {
			return _Utils_Tuple2(func, newGenerator);
		}
	}
};
var $author$project$Transformations$skolemization = F2(
	function (lang, formula) {
		var skolemHelper = F5(
			function (l, gen, dependencies, substitutions, f) {
				switch (f.$) {
					case 0:
						var p = f.a;
						var terms = f.b;
						return _Utils_Tuple2(
							A2(
								$author$project$Language$Predicate,
								p,
								A2(
									$elm$core$List$map,
									$author$project$Transformations$replaceInTerm(substitutions),
									terms)),
							l);
					case 1:
						var f1 = f.a;
						var _v1 = A5(skolemHelper, l, gen, dependencies, substitutions, f1);
						var newF = _v1.a;
						var newL = _v1.b;
						return _Utils_Tuple2(
							$author$project$Transformations$negate(newF),
							newL);
					case 2:
						var f1 = f.a;
						var op = f.b;
						var f2 = f.c;
						var _v2 = A5(skolemHelper, l, gen, dependencies, substitutions, f2);
						var newF2 = _v2.a;
						var newL2 = _v2.b;
						var _v3 = A5(skolemHelper, l, gen, dependencies, substitutions, f1);
						var newF1 = _v3.a;
						var newL1 = _v3.b;
						return _Utils_Tuple2(
							A3($author$project$Language$Operation, newF1, op, newF2),
							_Utils_update(
								l,
								{
									aL: A2($elm$core$Set$union, newL1.aL, newL2.aL),
									aQ: A2($elm$core$Set$union, newL1.aQ, newL2.aQ)
								}));
					default:
						if (f.a === 1) {
							var _v4 = f.a;
							var x = f.b;
							var f1 = f.c;
							var _v5 = A5(
								skolemHelper,
								l,
								gen,
								A2($elm$core$List$cons, x, dependencies),
								substitutions,
								f1);
							var newF = _v5.a;
							var newL = _v5.b;
							return _Utils_Tuple2(
								A3($author$project$Language$Quantification, 1, x, newF),
								newL);
						} else {
							var _v6 = f.a;
							var x = f.b;
							var f1 = f.c;
							var _v7 = $elm$core$List$isEmpty(dependencies) ? A2(
								$elm$core$Tuple$mapFirst,
								$author$project$Language$Constant,
								$author$project$Generator$getConst(gen)) : A2(
								$elm$core$Tuple$mapFirst,
								function (funcName) {
									return A2(
										$author$project$Language$Function,
										funcName,
										A2($elm$core$List$map, $author$project$Language$Variable, dependencies));
								},
								$author$project$Generator$getFunc(gen));
							var newTerm = _v7.a;
							var newGen = _v7.b;
							var _v8 = A5(
								skolemHelper,
								l,
								newGen,
								dependencies,
								A3($author$project$Substitution$insert, x, newTerm, substitutions),
								f1);
							var newF = _v8.a;
							var newL = _v8.b;
							switch (newTerm.$) {
								case 0:
									var c = newTerm.a;
									return _Utils_Tuple2(
										newF,
										_Utils_update(
											newL,
											{
												aL: A2($elm$core$Set$insert, c, newL.aL)
											}));
								case 2:
									var func = newTerm.a;
									return _Utils_Tuple2(
										newF,
										_Utils_update(
											newL,
											{
												aQ: A2($elm$core$Set$insert, func, newL.aQ)
											}));
								default:
									return _Utils_Tuple2(newF, newL);
							}
						}
				}
			});
		return A5(
			skolemHelper,
			lang,
			A3($author$project$Generator$createGenerator, lang.aL, lang.aQ, lang.a1),
			_List_Nil,
			$author$project$Substitution$empty,
			formula);
	});
var $author$project$Disjunct$empty = $turboMaCk$any_set$Set$Any$empty($author$project$Language$literalToString);
var $author$project$DisjunctSet$fromList = A2(
	$elm$core$Basics$composeR,
	$elm$core$List$indexedMap($elm$core$Tuple$pair),
	$turboMaCk$any_set$Set$Any$fromList($author$project$DisjunctSet$toComparable));
var $turboMaCk$any_dict$Dict$Any$singleton = F3(
	function (k, v, f) {
		return A3(
			$turboMaCk$any_dict$Dict$Any$insert,
			k,
			v,
			$turboMaCk$any_dict$Dict$Any$empty(f));
	});
var $turboMaCk$any_set$Set$Any$singleton = function (a) {
	return A2(
		$elm$core$Basics$composeL,
		$elm$core$Basics$identity,
		A2($turboMaCk$any_dict$Dict$Any$singleton, a, 0));
};
var $author$project$Disjunct$singleton = function (l) {
	return A2($turboMaCk$any_set$Set$Any$singleton, l, $author$project$Language$literalToString);
};
var $author$project$DisjunctSet$foldl = function (f) {
	return $turboMaCk$any_set$Set$Any$foldl(
		F2(
			function (_v0, acc) {
				var d = _v0.b;
				return A2(f, d, acc);
			}));
};
var $author$project$DisjunctSet$union = $author$project$DisjunctSet$foldl($author$project$DisjunctSet$insert);
var $author$project$Transformations$toDisjunctSet = function (formula) {
	var removeQuantorPrefix = function (f) {
		removeQuantorPrefix:
		while (true) {
			if (f.$ === 3) {
				var f1 = f.c;
				var $temp$f = f1;
				f = $temp$f;
				continue removeQuantorPrefix;
			} else {
				return f;
			}
		}
	};
	var literalToDisjunct = function (f) {
		_v7$2:
		while (true) {
			switch (f.$) {
				case 0:
					var p = f.a;
					var terms = f.b;
					return $author$project$Disjunct$singleton(
						A2($author$project$Language$PositivePredicate, p, terms));
				case 1:
					if (!f.a.$) {
						var _v8 = f.a;
						var p = _v8.a;
						var terms = _v8.b;
						return $author$project$Disjunct$singleton(
							A2($author$project$Language$NegativePredicate, p, terms));
					} else {
						break _v7$2;
					}
				default:
					break _v7$2;
			}
		}
		return $author$project$Disjunct$empty;
	};
	var cnfToDisjunctSet = function (f) {
		_v1$2:
		while (true) {
			if (f.$ === 2) {
				switch (f.b) {
					case 0:
						var f1 = f.a;
						var _v2 = f.b;
						var f2 = f.c;
						return A2(
							$author$project$DisjunctSet$union,
							cnfToDisjunctSet(f1),
							cnfToDisjunctSet(f2));
					case 1:
						var f1 = f.a;
						var _v3 = f.b;
						var f2 = f.c;
						var _v4 = _Utils_Tuple2(
							$author$project$DisjunctSet$toList(
								cnfToDisjunctSet(f1)),
							$author$project$DisjunctSet$toList(
								cnfToDisjunctSet(f2)));
						if (((_v4.a.b && (!_v4.a.b.b)) && _v4.b.b) && (!_v4.b.b.b)) {
							var _v5 = _v4.a;
							var d1 = _v5.a;
							var _v6 = _v4.b;
							var d2 = _v6.a;
							return $author$project$DisjunctSet$fromList(
								_List_fromArray(
									[
										A2($author$project$Disjunct$union, d1, d2)
									]));
						} else {
							return $author$project$DisjunctSet$empty;
						}
					default:
						break _v1$2;
				}
			} else {
				break _v1$2;
			}
		}
		return $author$project$DisjunctSet$fromList(
			_List_fromArray(
				[
					literalToDisjunct(f)
				]));
	};
	return cnfToDisjunctSet(
		removeQuantorPrefix(formula));
};
var $author$project$Disjunct$indexedDisjunctToString = function (_v0) {
	var i = _v0.a;
	var d = _v0.b;
	return 'D' + ($elm$core$String$fromInt(i) + (' = ' + $author$project$Disjunct$toString(d)));
};
var $author$project$DisjunctSet$toIndexedString = function (ds) {
	var listOfDisjunsts = $author$project$DisjunctSet$toIndexedList(ds);
	return '{' + (A2(
		$elm$core$String$dropRight,
		2,
		A3(
			$elm$core$List$foldl,
			F2(
				function (x, acc) {
					return acc + ($author$project$Disjunct$indexedDisjunctToString(x) + ', ');
				}),
			'',
			listOfDisjunsts)) + '}');
};
var $author$project$Generator$getVar = function (generator) {
	getVar:
	while (true) {
		var _var = A2(
			$elm$core$Maybe$withDefault,
			'x',
			$elm$core$List$head(generator.au));
		var newVar = function () {
			var lastLetter = A2(
				$elm$core$Maybe$withDefault,
				'0',
				A2(
					$elm$core$Maybe$map,
					$elm$core$Tuple$first,
					$elm$core$String$uncons(
						A2($elm$core$String$right, 1, _var))));
			return ($elm$core$Char$isAlpha(lastLetter) || (lastLetter === '9')) ? (_var + '0') : _Utils_ap(
				A2($elm$core$String$dropRight, 1, _var),
				$elm$core$String$fromChar(
					$elm$core$Char$fromCode(
						$elm$core$Char$toCode(lastLetter) + 1)));
		}();
		var newState = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			$elm$core$List$tail(generator.au));
		var newGenerator = _Utils_update(
			generator,
			{
				au: A2(
					$elm$core$List$append,
					newState,
					_List_fromArray(
						[newVar]))
			});
		if (A2($elm$core$Set$member, _var, generator.aP)) {
			var $temp$generator = newGenerator;
			generator = $temp$generator;
			continue getVar;
		} else {
			return _Utils_Tuple2(_var, newGenerator);
		}
	}
};
var $author$project$Transformations$toPNF = F2(
	function (lang, formula) {
		var substituteVar = F3(
			function (_var, subVar, f) {
				switch (f.$) {
					case 0:
						var p = f.a;
						var terms = f.b;
						return A2(
							$author$project$Language$Predicate,
							p,
							A2(
								$elm$core$List$map,
								$author$project$Transformations$replaceInTerm(
									A2(
										$author$project$Substitution$singleton,
										_var,
										$author$project$Language$Variable(subVar))),
								terms));
					case 1:
						var f1 = f.a;
						return $author$project$Transformations$negate(
							A3(substituteVar, _var, subVar, f1));
					case 2:
						var f1 = f.a;
						var op = f.b;
						var f2 = f.c;
						return A3(
							$author$project$Language$Operation,
							A3(substituteVar, _var, subVar, f1),
							op,
							A3(substituteVar, _var, subVar, f2));
					default:
						var q = f.a;
						var x = f.b;
						var f1 = f.c;
						return _Utils_eq(x, _var) ? f : A3(
							$author$project$Language$Quantification,
							q,
							x,
							A3(substituteVar, _var, subVar, f1));
				}
			});
		var pullQuantors = function (initF) {
			var pullOnce = function (f) {
				switch (f.$) {
					case 1:
						if (f.a.$ === 3) {
							if (f.a.a === 1) {
								var _v2 = f.a;
								var _v3 = _v2.a;
								var x = _v2.b;
								var f1 = _v2.c;
								return A3(
									$author$project$Language$Quantification,
									0,
									x,
									$author$project$Language$Negation(
										pullQuantors(f1)));
							} else {
								var _v4 = f.a;
								var _v5 = _v4.a;
								var x = _v4.b;
								var f1 = _v4.c;
								return A3(
									$author$project$Language$Quantification,
									1,
									x,
									$author$project$Language$Negation(
										pullQuantors(f1)));
							}
						} else {
							var f1 = f.a;
							return $author$project$Language$Negation(
								pullQuantors(f1));
						}
					case 2:
						if (f.a.$ === 3) {
							if (f.c.$ === 3) {
								var _v6 = f.a;
								var q1 = _v6.a;
								var x1 = _v6.b;
								var f1 = _v6.c;
								var op = f.b;
								var _v7 = f.c;
								var q2 = _v7.a;
								var x2 = _v7.b;
								var f2 = _v7.c;
								return A3(
									$author$project$Language$Quantification,
									q1,
									x1,
									A3(
										$author$project$Language$Quantification,
										q2,
										x2,
										pullQuantors(
											A3(
												$author$project$Language$Operation,
												pullQuantors(f1),
												op,
												pullQuantors(f2)))));
							} else {
								var _v8 = f.a;
								var q1 = _v8.a;
								var x1 = _v8.b;
								var f1 = _v8.c;
								var op = f.b;
								var f2 = f.c;
								return A3(
									$author$project$Language$Quantification,
									q1,
									x1,
									pullQuantors(
										A3(
											$author$project$Language$Operation,
											pullQuantors(f1),
											op,
											pullQuantors(f2))));
							}
						} else {
							if (f.c.$ === 3) {
								var f1 = f.a;
								var op = f.b;
								var _v9 = f.c;
								var q2 = _v9.a;
								var x2 = _v9.b;
								var f2 = _v9.c;
								return A3(
									$author$project$Language$Quantification,
									q2,
									x2,
									pullQuantors(
										A3(
											$author$project$Language$Operation,
											pullQuantors(f1),
											op,
											pullQuantors(f2))));
							} else {
								var f1 = f.a;
								var op = f.b;
								var f2 = f.c;
								return A3(
									$author$project$Language$Operation,
									pullQuantors(f1),
									op,
									pullQuantors(f2));
							}
						}
					case 3:
						var q = f.a;
						var x = f.b;
						var f1 = f.c;
						return A3(
							$author$project$Language$Quantification,
							q,
							x,
							pullQuantors(f1));
					default:
						return f;
				}
			};
			var loop = function (f1) {
				loop:
				while (true) {
					var newF = pullOnce(f1);
					if (!_Utils_eq(newF, f1)) {
						var $temp$f1 = newF;
						f1 = $temp$f1;
						continue loop;
					} else {
						return newF;
					}
				}
			};
			return loop(initF);
		};
		var makeUniqueBounded = F4(
			function (l, gen, f, varsByFar) {
				switch (f.$) {
					case 0:
						var terms = f.b;
						var vars = A3(
							$elm$core$List$foldr,
							F2(
								function (x, acc) {
									return A2(
										$elm$core$Set$union,
										$author$project$Language$varsInTerm(x),
										acc);
								}),
							varsByFar,
							terms);
						return _Utils_Tuple3(
							f,
							vars,
							_Utils_Tuple2(l, gen));
					case 1:
						var f1 = f.a;
						var _v11 = A4(makeUniqueBounded, l, gen, f1, varsByFar);
						var newF1 = _v11.a;
						var newVars = _v11.b;
						var _v12 = _v11.c;
						var newL = _v12.a;
						var newGen = _v12.b;
						return _Utils_Tuple3(
							$author$project$Transformations$negate(newF1),
							newVars,
							_Utils_Tuple2(newL, newGen));
					case 2:
						var f1 = f.a;
						var op = f.b;
						var f2 = f.c;
						var _v13 = A4(makeUniqueBounded, l, gen, f1, varsByFar);
						var newF1 = _v13.a;
						var newVarsF1 = _v13.b;
						var _v14 = _v13.c;
						var newL1 = _v14.a;
						var newGen1 = _v14.b;
						var _v15 = A4(makeUniqueBounded, newL1, newGen1, f2, newVarsF1);
						var newF2 = _v15.a;
						var newVarsF2 = _v15.b;
						var _v16 = _v15.c;
						var newL2 = _v16.a;
						var newGen2 = _v16.b;
						return _Utils_Tuple3(
							A3($author$project$Language$Operation, newF1, op, newF2),
							newVarsF2,
							_Utils_Tuple2(newL2, newGen2));
					default:
						var q = f.a;
						var x = f.b;
						var f1 = f.c;
						if (A2($elm$core$Set$member, x, varsByFar)) {
							var _v17 = $author$project$Generator$getVar(gen);
							var newX = _v17.a;
							var newGen = _v17.b;
							var newF1 = A3(substituteVar, x, newX, f1);
							var newL = _Utils_update(
								l,
								{
									a1: A2($elm$core$Set$insert, newX, l.a1)
								});
							var _v18 = A4(
								makeUniqueBounded,
								newL,
								newGen,
								newF1,
								A2($elm$core$Set$insert, newX, varsByFar));
							var finalF1 = _v18.a;
							var finalVars = _v18.b;
							var _v19 = _v18.c;
							var finalL = _v19.a;
							var newGen2 = _v19.b;
							return _Utils_Tuple3(
								A3($author$project$Language$Quantification, q, newX, finalF1),
								finalVars,
								_Utils_Tuple2(finalL, newGen2));
						} else {
							var _v20 = A4(
								makeUniqueBounded,
								l,
								gen,
								f1,
								A2($elm$core$Set$insert, x, varsByFar));
							var finalF1 = _v20.a;
							var finalVars = _v20.b;
							var _v21 = _v20.c;
							var finalL = _v21.a;
							var newGen = _v21.b;
							return _Utils_Tuple3(
								A3($author$project$Language$Quantification, q, x, finalF1),
								finalVars,
								_Utils_Tuple2(finalL, newGen));
						}
				}
			});
		var _v22 = A4(
			makeUniqueBounded,
			lang,
			A3($author$project$Generator$createGenerator, lang.aL, lang.aQ, lang.a1),
			formula,
			$elm$core$Set$empty);
		var uniqueBoundedF = _v22.a;
		var _v23 = _v22.c;
		var modifiedL = _v23.a;
		return _Utils_Tuple2(
			pullQuantors(uniqueBoundedF),
			modifiedL);
	});
var $author$project$DisjunctSet$toString = function (d) {
	var listOfDisjunsts = $author$project$DisjunctSet$toList(d);
	return '{' + (A2(
		$elm$core$String$dropRight,
		2,
		A3(
			$elm$core$List$foldl,
			F2(
				function (x, acc) {
					return acc + ($author$project$Disjunct$toString(x) + ', ');
				}),
			'',
			listOfDisjunsts)) + '}');
};
var $elm$core$String$trim = _String_trim;
var $author$project$Main$update = F2(
	function (msg, model) {
		var updateAtIndex = F2(
			function (index, value) {
				return $elm$core$List$indexedMap(
					F2(
						function (i, el) {
							return _Utils_eq(i, index) ? value : el;
						}));
			});
		var updateAllFormulas = function (m) {
			return A3(
				$elm$core$List$foldl,
				F2(
					function (_v14, newModel) {
						var index = _v14.a;
						var formula = _v14.b;
						return A2(
							$author$project$Main$update,
							A2($author$project$Main$UpdateFormula, index, formula),
							newModel);
					}),
				m,
				A2(
					$elm$core$List$indexedMap,
					$elm$core$Tuple$pair,
					A2($elm$core$List$map, $elm$core$Tuple$first, m.m)));
		};
		var parseLanguageSet = function (str) {
			return $elm$core$Set$fromList(
				A2(
					$elm$core$List$map,
					$elm$core$String$trim,
					A2($elm$core$String$split, ',', str)));
		};
		var modelLanguage = model.G;
		switch (msg.$) {
			case 0:
				var value = msg.a;
				return updateAllFormulas(
					_Utils_update(
						model,
						{
							G: _Utils_update(
								modelLanguage,
								{
									a1: parseLanguageSet(value)
								}),
							aK: value
						}));
			case 1:
				var value = msg.a;
				return updateAllFormulas(
					_Utils_update(
						model,
						{
							G: _Utils_update(
								modelLanguage,
								{
									aW: parseLanguageSet(value)
								}),
							aE: value
						}));
			case 3:
				var value = msg.a;
				return updateAllFormulas(
					_Utils_update(
						model,
						{
							aC: value,
							G: _Utils_update(
								modelLanguage,
								{
									aQ: parseLanguageSet(value)
								})
						}));
			case 2:
				var value = msg.a;
				return updateAllFormulas(
					_Utils_update(
						model,
						{
							az: value,
							G: _Utils_update(
								modelLanguage,
								{
									aL: parseLanguageSet(value)
								})
						}));
			case 4:
				return _Utils_update(
					model,
					{
						m: _Utils_ap(
							model.m,
							_List_fromArray(
								[
									_Utils_Tuple2('', '')
								])),
						F: _Utils_ap(
							model.F,
							_List_fromArray(
								[$elm$core$Maybe$Nothing]))
					});
			case 5:
				var prependToLastNonEmpty = function (strings) {
					var prependToFirstNonEmpty = function (str) {
						if (!str.b) {
							return _List_Nil;
						} else {
							var _v2 = str.a;
							var head = _v2.a;
							var err = _v2.b;
							var tail = str.b;
							return $elm$core$String$isEmpty(head) ? A2(
								$elm$core$List$cons,
								_Utils_Tuple2(head, err),
								prependToFirstNonEmpty(tail)) : A2(
								$elm$core$List$cons,
								_Utils_Tuple2('¬' + head, err),
								tail);
						}
					};
					return $elm$core$List$reverse(
						prependToFirstNonEmpty(
							$elm$core$List$reverse(strings)));
				};
				return updateAllFormulas(
					_Utils_update(
						model,
						{
							m: prependToLastNonEmpty(model.m)
						}));
			case 6:
				var symbol = msg.a;
				var _v3 = model.aB;
				if (!_v3.$) {
					var index = _v3.a;
					return _Utils_update(
						model,
						{
							m: A2(
								$elm$core$List$indexedMap,
								F2(
									function (i, el) {
										return _Utils_eq(i, index) ? A2(
											$elm$core$Tuple$mapFirst,
											function (x) {
												return _Utils_ap(
													x,
													$elm$core$String$fromChar(symbol));
											},
											el) : el;
									}),
								model.m)
						});
				} else {
					return model;
				}
			case 7:
				var maybeInt = msg.a;
				return _Utils_update(
					model,
					{aB: maybeInt});
			case 8:
				var index = msg.a;
				var formulaString = msg.b;
				if ($elm$core$String$isEmpty(formulaString)) {
					return _Utils_update(
						model,
						{
							m: A3(
								updateAtIndex,
								index,
								_Utils_Tuple2(formulaString, ''),
								model.m),
							F: A3(updateAtIndex, index, $elm$core$Maybe$Nothing, model.F)
						});
				} else {
					var _v4 = A2($author$project$Parser$parse, model.G, formulaString);
					if (!_v4.$) {
						var formula = _v4.a;
						return _Utils_update(
							model,
							{
								m: A3(
									updateAtIndex,
									index,
									_Utils_Tuple2(formulaString, ''),
									model.m),
								F: A3(
									updateAtIndex,
									index,
									$elm$core$Maybe$Just(formula),
									model.F)
							});
					} else {
						var error = _v4.a;
						return _Utils_update(
							model,
							{
								m: A3(
									updateAtIndex,
									index,
									_Utils_Tuple2(
										formulaString,
										$author$project$Parser$errToString(error)),
									model.m)
							});
					}
				}
			case 9:
				var transform = F2(
					function (f, l) {
						var removeDuplicatesBySecondElement = function (dupList) {
							var member = F2(
								function (f11, listToSearch) {
									member:
									while (true) {
										if (!listToSearch.b) {
											return false;
										} else {
											var _v11 = listToSearch.a;
											var f21 = _v11.b;
											var ls = listToSearch.b;
											if (_Utils_eq(f11, f21)) {
												return true;
											} else {
												var $temp$f11 = f11,
													$temp$listToSearch = ls;
												f11 = $temp$f11;
												listToSearch = $temp$listToSearch;
												continue member;
											}
										}
									}
								});
							return A3(
								$elm$core$List$foldl,
								F2(
									function (item, acc) {
										return A2(member, item.b, acc) ? acc : A2($elm$core$List$cons, item, acc);
									}),
								_List_Nil,
								dupList);
						};
						var line1 = _Utils_Tuple2(
							'Original formula: ',
							$author$project$Language$printFormula(f));
						var lString = $author$project$Language$printLanguage(l);
						var f1 = $author$project$Transformations$eliminateImplAndEqv(f);
						var f2 = $author$project$Transformations$moveNegations(f1);
						var line3 = _Utils_Tuple2(
							'Moving negations inside: ',
							$author$project$Language$printFormula(f2));
						var line2 = _Utils_Tuple2(
							'Elimination of implications and equivalences: ',
							$author$project$Language$printFormula(f1));
						var _v8 = A2($author$project$Transformations$skolemization, l, f2);
						var f3 = _v8.a;
						var l1 = _v8.b;
						var line4 = _Utils_Tuple2(
							'Skolemization: ',
							$author$project$Language$printFormula(f3));
						var _v9 = A2($author$project$Transformations$toPNF, l1, f3);
						var f4 = _v9.a;
						var l2 = _v9.b;
						var f5 = $author$project$Transformations$pNFtoCNF(f4);
						var ds = $author$project$Transformations$toDisjunctSet(f5);
						var line7 = _Utils_Tuple2(
							'Formula disjunct set:',
							$author$project$DisjunctSet$toString(ds));
						var line6 = _Utils_Tuple2(
							'Converting to Conjunctive Normal Form: ',
							$author$project$Language$printFormula(f5));
						var line5 = _Utils_Tuple2(
							'Converting to Prenex Normal Form: ',
							$author$project$Language$printFormula(f4));
						var l2String = $author$project$Language$printLanguage(l2);
						return _Utils_Tuple3(
							ds,
							l2,
							_Utils_ap(
								$elm$core$List$reverse(
									removeDuplicatesBySecondElement(
										_List_fromArray(
											[line1, line2, line3, line4, line5, line5, line6]))),
								A2(
									$elm$core$List$cons,
									line7,
									(!_Utils_eq(lString, l2String)) ? _List_fromArray(
										[
											_Utils_Tuple2(' Language is updated to ', l2String)
										]) : _List_Nil)));
					});
				var filteredFormulas = A2($elm$core$List$filterMap, $elm$core$Basics$identity, model.F);
				var clearedModel = _Utils_update(
					model,
					{
						Z: $author$project$DisjunctSet$empty,
						an: model.G,
						ae: _Utils_Tuple2('', ''),
						V: _List_Nil
					});
				var _v5 = A3(
					$elm$core$List$foldl,
					F2(
						function (f, _v6) {
							var oldModel = _v6.a;
							var dss = _v6.b;
							var _v7 = A2(transform, f, oldModel.an);
							var ds = _v7.a;
							var newL = _v7.b;
							var lines = _v7.c;
							return _Utils_Tuple2(
								_Utils_update(
									oldModel,
									{
										an: newL,
										V: _Utils_ap(
											oldModel.V,
											_List_fromArray(
												[lines]))
									}),
								A2($author$project$DisjunctSet$union, dss, ds));
						}),
					_Utils_Tuple2(clearedModel, $author$project$DisjunctSet$empty),
					filteredFormulas);
				var newM = _v5.a;
				var finalDisjunctSet = _v5.b;
				return _Utils_update(
					newM,
					{
						Z: finalDisjunctSet,
						ae: $elm$core$List$isEmpty(newM.V) ? _Utils_Tuple2('', '') : _Utils_Tuple2(
							'Final Disjunct set:',
							$author$project$DisjunctSet$toIndexedString(finalDisjunctSet))
					});
			case 10:
				var str = msg.a;
				return _Utils_update(
					model,
					{R: str});
			default:
				if ($author$project$DisjunctSet$isEmpty(model.Z)) {
					return _Utils_update(
						model,
						{ac: _List_Nil});
				} else {
					var _v12 = A2(
						$elm$core$Maybe$andThen,
						function (num) {
							return (num <= 0) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(num);
						},
						$elm$core$String$toInt(model.R));
					if (_v12.$ === 1) {
						return _Utils_update(
							model,
							{R: $author$project$Main$defaultMaxStep});
					} else {
						var maxDepth = _v12.a;
						var _v13 = A2($author$project$Search$resolutionMethod, model.Z, maxDepth);
						if (!_v13.$) {
							var log = _v13.a;
							return _Utils_update(
								model,
								{ar: $elm$core$Maybe$Nothing, ac: log});
						} else {
							return _Utils_update(
								model,
								{
									ar: $elm$core$Maybe$Just('Unable to solve in under ' + (model.R + ' steps.')),
									ac: _List_Nil
								});
						}
					}
				}
		}
	});
var $author$project$Main$AddFormula = {$: 4};
var $author$project$Main$AddSymbol = function (a) {
	return {$: 6, a: a};
};
var $author$project$Main$FocusChanged = function (a) {
	return {$: 7, a: a};
};
var $author$project$Main$NegationOfLast = {$: 5};
var $author$project$Main$StartResolution = {$: 11};
var $author$project$Main$StartTransformations = {$: 9};
var $author$project$Main$UpdateConstants = function (a) {
	return {$: 2, a: a};
};
var $author$project$Main$UpdateFunctions = function (a) {
	return {$: 3, a: a};
};
var $author$project$Main$UpdateMaxDepth = function (a) {
	return {$: 10, a: a};
};
var $author$project$Main$UpdatePredicates = function (a) {
	return {$: 1, a: a};
};
var $author$project$Main$UpdateVariables = function (a) {
	return {$: 0, a: a};
};
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$html$Html$ol = _VirtualDom_node('ol');
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
var $elm$html$Html$Events$onFocus = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'focus',
		$elm$json$Json$Decode$succeed(msg));
};
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
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$string = _Json_decodeString;
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
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Main$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('app')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('language')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('label')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Language: ')
							])),
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('language-input'),
								$elm$html$Html$Attributes$placeholder('Variables'),
								$elm$html$Html$Attributes$value(model.aK),
								$elm$html$Html$Events$onInput($author$project$Main$UpdateVariables)
							]),
						_List_Nil),
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('language-input'),
								$elm$html$Html$Attributes$placeholder('Predicates'),
								$elm$html$Html$Attributes$value(model.aE),
								$elm$html$Html$Events$onInput($author$project$Main$UpdatePredicates)
							]),
						_List_Nil),
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('language-input'),
								$elm$html$Html$Attributes$placeholder('Functions'),
								$elm$html$Html$Attributes$value(model.aC),
								$elm$html$Html$Events$onInput($author$project$Main$UpdateFunctions)
							]),
						_List_Nil),
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('language-input'),
								$elm$html$Html$Attributes$placeholder('Constants'),
								$elm$html$Html$Attributes$value(model.az),
								$elm$html$Html$Events$onInput($author$project$Main$UpdateConstants)
							]),
						_List_Nil)
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('formulas-insert-container')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('formula-column')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('label')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Formulas:')
									])),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								$elm$core$List$concat(
									A2(
										$elm$core$List$indexedMap,
										F2(
											function (index, _v0) {
												var formula = _v0.a;
												var error = _v0.b;
												return _List_fromArray(
													[
														A2(
														$elm$html$Html$div,
														_List_Nil,
														_List_fromArray(
															[
																A2(
																$elm$html$Html$input,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$class('formula-input'),
																		$elm$html$Html$Attributes$placeholder('Formula'),
																		$elm$html$Html$Attributes$value(formula),
																		$elm$html$Html$Events$onInput(
																		$author$project$Main$UpdateFormula(index)),
																		$elm$html$Html$Events$onFocus(
																		$author$project$Main$FocusChanged(
																			$elm$core$Maybe$Just(index)))
																	]),
																_List_Nil),
																A2(
																$elm$html$Html$div,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$class('error')
																	]),
																_List_fromArray(
																	[
																		$elm$html$Html$text(error)
																	]))
															]))
													]);
											}),
										model.m))),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick($author$project$Main$AddFormula)
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Add Formula')
											])),
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick($author$project$Main$NegationOfLast)
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Negation of last formula')
											]))
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('insert-column')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('label')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Insert special character:')
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('insert-buttons')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick(
												$author$project$Main$AddSymbol('∀'))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('∀')
											])),
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick(
												$author$project$Main$AddSymbol('∃'))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('∃')
											])),
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick(
												$author$project$Main$AddSymbol('&'))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('&')
											])),
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick(
												$author$project$Main$AddSymbol('∨'))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('∨')
											])),
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick(
												$author$project$Main$AddSymbol('⇔'))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('⇔')
											])),
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick(
												$author$project$Main$AddSymbol('⇒'))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('⇒')
											])),
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick(
												$author$project$Main$AddSymbol('¬'))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('¬')
											]))
									]))
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('transformations')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Main$StartTransformations)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Make Transformations')
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('transformations-body')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_Nil,
								A2(
									$elm$core$List$indexedMap,
									F2(
										function (index, formulaLines) {
											return A2(
												$elm$html$Html$div,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('formula-transformation')
													]),
												_List_fromArray(
													[
														A2(
														$elm$html$Html$div,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('label')
															]),
														_List_fromArray(
															[
																$elm$html$Html$text(
																'Formula ' + $elm$core$String$fromInt(index + 1))
															])),
														A2(
														$elm$html$Html$div,
														_List_Nil,
														A2(
															$elm$core$List$map,
															function (_v1) {
																var step = _v1.a;
																var formula = _v1.b;
																return A2(
																	$elm$html$Html$div,
																	_List_fromArray(
																		[
																			$elm$html$Html$Attributes$class('single-transformation')
																		]),
																	_List_fromArray(
																		[
																			A2(
																			$elm$html$Html$span,
																			_List_Nil,
																			_List_fromArray(
																				[
																					$elm$html$Html$text(step)
																				])),
																			A2(
																			$elm$html$Html$span,
																			_List_fromArray(
																				[
																					$elm$html$Html$Attributes$class('formula')
																				]),
																			_List_fromArray(
																				[
																					$elm$html$Html$text(formula)
																				]))
																		]));
															},
															formulaLines))
													]));
										}),
									model.V)),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(model.ae.a)
											])),
										A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('formula')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(model.ae.b)
											]))
									]))
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('resolution')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('max-steps')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('label')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Max steps:')
									])),
								A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('steps-input'),
										$elm$html$Html$Attributes$type_('number'),
										$elm$html$Html$Attributes$value(model.R),
										$elm$html$Html$Events$onInput($author$project$Main$UpdateMaxDepth)
									]),
								_List_Nil)
							])),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Main$StartResolution)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Apply Resolution')
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('error')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(
								A2($elm$core$Maybe$withDefault, '', model.ar))
							])),
						A2(
						$elm$html$Html$ol,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('resolution-result')
							]),
						A2(
							$elm$core$List$map,
							function (_v2) {
								var sub = _v2.a;
								var step = _v2.b;
								return A2(
									$elm$html$Html$li,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('resolution-step')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$span,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(sub)
												])),
											A2(
											$elm$html$Html$span,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('res-step')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text(step)
												]))
										]));
							},
							model.ac))
					]))
			]));
};
var $author$project$Main$main = $elm$browser$Browser$sandbox(
	{ca: $author$project$Main$init, cD: $author$project$Main$update, cE: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));