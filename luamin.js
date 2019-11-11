/*! https://mths.be/luamin v1.0.4 by @mathias */
;(function(root) {

	// Detect free variables `exports`
	var freeExports = typeof exports == 'object' && exports;

	// Detect free variable `module`
	var freeModule = typeof module == 'object' && module &&
		module.exports == freeExports && module;

	// Detect free variable `global`, from Node.js or Browserified code,
	// and use it as `root`
	var freeGlobal = typeof global == 'object' && global;
	if (freeGlobal.global === freeGlobal || freeGlobal.window === freeGlobal) {
		root = freeGlobal;
	}

	/*--------------------------------------------------------------------------*/

	var luaparse = root.luaparse || require('luaparse');
	luaparse.defaultOptions.comments = false;
	luaparse.defaultOptions.scope = true;
	var parse = luaparse.parse;

	var regexAlphaUnderscore = /[a-zA-Z_]/;
	var regexAlphaNumUnderscore = /[a-zA-Z0-9_]/;
	var regexDigits = /[0-9]/;

	// http://www.lua.org/manual/5.2/manual.html#3.4.7
	// http://www.lua.org/source/5.2/lparser.c.html#priority
	var PRECEDENCE = {
		'or': 1,
		'and': 2,
		'<': 3, '>': 3, '<=': 3, '>=': 3, '~=': 3, '==': 3,
		'..': 5,
		'+': 6, '-': 6, // binary -
		'*': 7, '/': 7, '%': 7,
		'unarynot': 8, 'unary#': 8, 'unary-': 8, // unary -
		'^': 10
	};

	var IDENTIFIER_PARTS = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a',
		'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
		'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E',
		'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
		'U', 'V', 'W', 'X', 'Y', 'Z', '_'];
	var IDENTIFIER_PARTS_MAX = IDENTIFIER_PARTS.length - 1;

	var each = function(array, fn) {
		var index = -1;
		var length = array.length;
		var max = length - 1;
		while (++index < length) {
			fn(array[index], index < max);
		}
	};

	var indexOf = function(array, value) {
		var index = -1;
		var length = array.length;
		while (++index < length) {
			if (array[index] == value) {
				return index;
			}
		}
	};

	var hasOwnProperty = {}.hasOwnProperty;
	var extend = function(destination, source) {
		var key;
		if (source) {
			for (key in source) {
				if (hasOwnProperty.call(source, key)) {
					destination[key] = source[key];
				}
			}
		}
		return destination;
	};

	var generateZeroes = function(length) {
		var zero = '0';
		var result = '';
		if (length < 1) {
			return result;
		}
		if (length == 1) {
			return zero;
		}
		while (length) {
			if (length & 1) {
				result += zero;
			}
			if (length >>= 1) {
				zero += zero;
			}
		}
		return result;
	};

	// http://www.lua.org/manual/5.2/manual.html#3.1
	function isKeyword(id) {
		switch (id.length) {
			case 2:
				return 'do' == id || 'if' == id || 'in' == id || 'or' == id;
			case 3:
				return 'and' == id || 'end' == id || 'for' == id || 'nil' == id ||
					'not' == id;
			case 4:
				return 'else' == id || 'goto' == id || 'then' == id || 'true' == id;
			case 5:
				return 'break' == id || 'false' == id || 'local' == id ||
					'until' == id || 'while' == id;
			case 6:
				return 'elseif' == id || 'repeat' == id || 'return' == id;
			case 8:
				return 'function' == id;
		}
		return false;
	}

	var currentIdentifier;
	var identifierMap;
	var identifiersInUse;
	var generateIdentifier = function(originalName) {
		// Preserve `self` in methods
		if (originalName == 'self') {
			return originalName;
		}

		if (hasOwnProperty.call(identifierMap, originalName)) {
			return identifierMap[originalName];
		}
		var length = currentIdentifier.length;
		var position = length - 1;
		var character;
		var index;
		while (position >= 0) {
			character = currentIdentifier.charAt(position);
			index = indexOf(IDENTIFIER_PARTS, character);
			if (index != IDENTIFIER_PARTS_MAX) {
				currentIdentifier = currentIdentifier.substring(0, position) +
					IDENTIFIER_PARTS[index + 1] + generateZeroes(length - (position + 1));
				if (
					isKeyword(currentIdentifier) ||
					indexOf(identifiersInUse, currentIdentifier) > -1
				) {
					return generateIdentifier(originalName);
				}
				identifierMap[originalName] = currentIdentifier;
				return currentIdentifier;
			}
			--position;
		}
		currentIdentifier = 'a' + generateZeroes(length);
		if (indexOf(identifiersInUse, currentIdentifier) > -1) {
			return generateIdentifier(originalName);
		}
		identifierMap[originalName] = currentIdentifier;
		return currentIdentifier;
	};

	/*--------------------------------------------------------------------------*/

	var joinStatements = function(a, b, separator) {
		separator || (separator = ' ');

		var lastCharA = a.slice(-1);
		var firstCharB = b.charAt(0);

		if (lastCharA == '' || firstCharB == '') {
			return a + b;
		}
		if (regexAlphaUnderscore.test(lastCharA)) {
			if (regexAlphaNumUnderscore.test(firstCharB)) {
				// e.g. `while` + `1`
				// e.g. `local a` + `local b`
				return a + separator + b;
			} else {
				// e.g. `not` + `(2>3 or 3<2)`
				// e.g. `x` + `^`
				return a + b;
			}
		}
		if (regexDigits.test(lastCharA)) {
			if (
				firstCharB == '(' ||
				!(firstCharB == '.' ||
				regexAlphaUnderscore.test(firstCharB))
			) {
				// e.g. `1` + `+`
				// e.g. `1` + `==`
				return a + b;
			} else {
				// e.g. `1` + `..`
				// e.g. `1` + `and`
				return a + separator + b;
			}
		}
		if (lastCharA == firstCharB && lastCharA == '-') {
			// e.g. `1-` + `-2`
			return a + separator + b;
		}
		var secondLastCharA = a.slice(-2, -1);
		if (lastCharA == '.' && secondLastCharA != '.' && regexAlphaNumUnderscore.test(firstCharB)) {
			// e.g. `1.` + `print`
			return a + separator + b;
		}
		return a + b;
	};

	var formatBase = function(base, baseOptions) {
		var result = '';
		var type = base.type;
		var needsParens = base.inParens && (
			type == 'BinaryExpression' ||
			type == 'FunctionDeclaration' ||
			type == 'TableConstructorExpression' ||
			type == 'LogicalExpression' ||
			type == 'StringLiteral'
		);
		if (needsParens) {
			result += '(';
		}
		result += formatExpression(base, baseOptions);
		if (needsParens) {
			result += ')';
		}
		return result;
	};

	var formatExpression = function(expression, baseOptions) {

		var options = extend({
			'precedence': 0,
			'preserveIdentifiers': false,
			'forceGenerateIdentifiers': false
		}, baseOptions);

		var result = '';
		var currentPrecedence;
		var associativity;
		var operator;

		var expressionType = expression.type;

		if (expressionType == 'Identifier') {

			// forceGenerateIdentifiers has precedence over preserveIdentifiers
			result = options.forceGenerateIdentifiers ||
				(expression.isLocal && !options.preserveIdentifiers)
				? generateIdentifier(expression.name)
				: expression.name;

		} else if (
			expressionType == 'StringLiteral' ||
			expressionType == 'NumericLiteral' ||
			expressionType == 'BooleanLiteral' ||
			expressionType == 'NilLiteral' ||
			expressionType == 'VarargLiteral'
		) {

			result = expression.raw;

		} else if (
			expressionType == 'LogicalExpression' ||
			expressionType == 'BinaryExpression'
		) {

			// If an expression with precedence x
			// contains an expression with precedence < x,
			// the inner expression must be wrapped in parens.
			operator = expression.operator;
			currentPrecedence = PRECEDENCE[operator];
			associativity = 'left';

			result = formatExpression(expression.left, extend({
				'precedence': currentPrecedence,
				'direction': 'left',
				'parent': operator
			}, baseOptions));
			result = joinStatements(result, operator);
			result = joinStatements(result, formatExpression(expression.right, extend({
				'precedence': currentPrecedence,
				'direction': 'right',
				'parent': operator
			}, baseOptions)));

			if (operator == '^' || operator == '..') {
				associativity = "right";
			}

			if (
				currentPrecedence < options.precedence ||
				(
					currentPrecedence == options.precedence &&
					associativity != options.direction &&
					options.parent != '+' &&
					!(options.parent == '*' && (operator == '/' || operator == '*'))
				)
			) {
				// The most simple case here is that of
				// protecting the parentheses on the RHS of
				// `1 - (2 - 3)` but deleting them from `(1 - 2) - 3`.
				// This is generally the right thing to do. The
				// semantics of `+` are special however: `1 + (2 - 3)`
				// == `1 + 2 - 3`. `-` and `+` are the only two operators
				// who share their precedence level. `*` also can
				// commute in such a way with `/`, but not with `%`
				// (all three share a precedence). So we test for
				// all of these conditions and avoid emitting
				// parentheses in the cases where we don’t have to.
				result = '(' + result + ')';
			}

		} else if (expressionType == 'UnaryExpression') {

			operator = expression.operator;
			currentPrecedence = PRECEDENCE['unary' + operator];

			result = joinStatements(
				operator,
				formatExpression(expression.argument, extend({
					'precedence': currentPrecedence
				}, baseOptions))
			);

			if (
				currentPrecedence < options.precedence &&
				// In principle, we should parenthesize the RHS of an
				// expression like `3^-2`, because `^` has higher precedence
				// than unary `-` according to the manual. But that is
				// misleading on the RHS of `^`, since the parser will
				// always try to find a unary operator regardless of
				// precedence.
				!(
					(options.parent == '^') &&
					options.direction == 'right'
				)
			) {
				result = '(' + result + ')';
			}

		} else if (expressionType == 'CallExpression') {

			result = formatBase(expression.base, baseOptions) + '(';

			each(expression.arguments, function(argument, needsComma) {
				result += formatExpression(argument, baseOptions);
				if (needsComma) {
					result += ',';
				}
			});
			result += ')';

		} else if (expressionType == 'TableCallExpression') {

			result = formatExpression(expression.base, baseOptions) +
				formatExpression(expression.arguments, baseOptions);

		} else if (expressionType == 'StringCallExpression') {

			result = formatExpression(expression.base, baseOptions) +
				formatExpression(expression.argument, baseOptions);

		} else if (expressionType == 'IndexExpression') {

			result = formatBase(expression.base, baseOptions) + '[' +
				formatExpression(expression.index, baseOptions) + ']';

		} else if (expressionType == 'MemberExpression') {

			result = formatBase(expression.base, baseOptions) + expression.indexer +
				formatExpression(expression.identifier, extend({
					'preserveIdentifiers': true,
					'forceGenerateIdentifiers': baseOptions.minifyMemberNames &&
						!expression.identifier.name.startsWith("_")  // members accessed dynamically must start with "_"
				}, baseOptions));

		} else if (expressionType == 'FunctionDeclaration') {

			result = 'function(';
			if (expression.parameters.length) {
				each(expression.parameters, function(parameter, needsComma) {
					// `Identifier`s have a `name`, `VarargLiteral`s have a `value`
					result += parameter.name
						? generateIdentifier(parameter.name)
						: parameter.value;
					if (needsComma) {
						result += ',';
					}
				});
			}
			result += ')';
			result = joinStatements(result, formatStatementList(expression.body, baseOptions));
			result = joinStatements(result, 'end');

		} else if (expressionType == 'TableConstructorExpression') {

			result = '{';

			each(expression.fields, function(field, needsComma) {
				if (field.type == 'TableKey') {
					result += '[' + formatExpression(field.key, baseOptions) + ']=' +
						formatExpression(field.value, baseOptions);
				} else if (field.type == 'TableValue') {
					result += formatExpression(field.value, baseOptions);
				} else { // at this point, `field.type == 'TableKeyString'`
					// Aggressive minification note: this will only affect key strings without square brackets or quotes
					//   ex: t = { key1 = 1, key2 = 2 }
					// Strings inside square brackets are handled above, so you can use that to preserve
					//   key identifiers you intend to access dynamically via string, instead of starting them all with "_"
					//   ex: t = { ["key1_preserved"] = 1 }
					result += formatExpression(field.key, extend({
						// TODO: keep track of nested scopes (#18)
						'preserveIdentifiers': true,
						'forceGenerateIdentifiers': baseOptions.minifyTableKeyStrings  &&
							!field.key.name.startsWith("_")  // members accessed dynamically must start with "_"
					}, baseOptions)) + '=' + formatExpression(field.value, baseOptions);
				}
				if (needsComma) {
					result += ',';
				}
			});

			result += '}';

		} else {

			throw TypeError('Unknown expression type: `' + expressionType + '`');

		}

		return result;
	};

	var formatStatementList = function(body, baseOptions) {
		var result = '';
		each(body, function(statement) {
			var separator = baseOptions.newlineSeparator ? '\n' : ';';
			result = joinStatements(result, formatStatement(statement, baseOptions), separator);
		});
		return result;
	};

	var formatStatement = function(statement, baseOptions) {
		var result = '';
		var statementType = statement.type;

		if (statementType == 'AssignmentStatement') {

			// left-hand side
			each(statement.variables, function(variable, needsComma) {
				result += formatExpression(variable, baseOptions);
				if (needsComma) {
					result += ',';
				}
			});

			// right-hand side
			result += '=';
			each(statement.init, function(init, needsComma) {
				result += formatExpression(init, baseOptions);
				if (needsComma) {
					result += ',';
				}
			});

		} else if (statementType == 'LocalStatement') {

			result = 'local ';

			// left-hand side
			each(statement.variables, function(variable, needsComma) {
				// Variables in a `LocalStatement` are always local, duh
				result += generateIdentifier(variable.name);
				if (needsComma) {
					result += ',';
				}
			});

			// right-hand side
			if (statement.init.length) {
				result += '=';
				each(statement.init, function(init, needsComma) {
					result += formatExpression(init, baseOptions);
					if (needsComma) {
						result += ',';
					}
				});
			}

		} else if (statementType == 'CallStatement') {

			result = formatExpression(statement.expression, baseOptions);

		} else if (statementType == 'IfStatement') {

			result = joinStatements(
				'if',
				formatExpression(statement.clauses[0].condition, baseOptions)
			);
			result = joinStatements(result, 'then');
			result = joinStatements(
				result,
				formatStatementList(statement.clauses[0].body, baseOptions)
			);
			each(statement.clauses.slice(1), function(clause) {
				if (clause.condition) {
					result = joinStatements(result, 'elseif');
					result = joinStatements(result, formatExpression(clause.condition, baseOptions));
					result = joinStatements(result, 'then');
				} else {
					result = joinStatements(result, 'else');
				}
				result = joinStatements(result, formatStatementList(clause.body, baseOptions));
			});
			result = joinStatements(result, 'end');

		} else if (statementType == 'WhileStatement') {

			result = joinStatements('while', formatExpression(statement.condition, baseOptions));
			result = joinStatements(result, 'do');
			result = joinStatements(result, formatStatementList(statement.body, baseOptions));
			result = joinStatements(result, 'end');

		} else if (statementType == 'DoStatement') {

			result = joinStatements('do', formatStatementList(statement.body, baseOptions));
			result = joinStatements(result, 'end');

		} else if (statementType == 'ReturnStatement') {

			result = 'return';

			each(statement.arguments, function(argument, needsComma) {
				result = joinStatements(result, formatExpression(argument, baseOptions));
				if (needsComma) {
					result += ',';
				}
			});

		} else if (statementType == 'BreakStatement') {

			result = 'break';

		} else if (statementType == 'RepeatStatement') {

			result = joinStatements('repeat', formatStatementList(statement.body, baseOptions));
			result = joinStatements(result, 'until');
			result = joinStatements(result, formatExpression(statement.condition, baseOptions))

		} else if (statementType == 'FunctionDeclaration') {

			result = (statement.isLocal ? 'local ' : '') + 'function ';
			result += formatExpression(statement.identifier, baseOptions);
			result += '(';

			if (statement.parameters.length) {
				each(statement.parameters, function(parameter, needsComma) {
					// `Identifier`s have a `name`, `VarargLiteral`s have a `value`
					result += parameter.name
						? generateIdentifier(parameter.name)
						: parameter.value;
					if (needsComma) {
						result += ',';
					}
				});
			}

			result += ')';
			result = joinStatements(result, formatStatementList(statement.body, baseOptions));
			result = joinStatements(result, 'end');

		} else if (statementType == 'ForGenericStatement') {
			// see also `ForNumericStatement`

			result = 'for ';

			each(statement.variables, function(variable, needsComma) {
				// The variables in a `ForGenericStatement` are always local
				result += generateIdentifier(variable.name);
				if (needsComma) {
					result += ',';
				}
			});

			result += ' in';

			each(statement.iterators, function(iterator, needsComma) {
				result = joinStatements(result, formatExpression(iterator, baseOptions));
				if (needsComma) {
					result += ',';
				}
			});

			result = joinStatements(result, 'do');
			result = joinStatements(result, formatStatementList(statement.body, baseOptions));
			result = joinStatements(result, 'end');

		} else if (statementType == 'ForNumericStatement') {

			// The variables in a `ForNumericStatement` are always local
			result = 'for ' + generateIdentifier(statement.variable.name) + '=';
			result += formatExpression(statement.start, baseOptions) + ',' +
				formatExpression(statement.end, baseOptions);

			if (statement.step) {
				result += ',' + formatExpression(statement.step, baseOptions);
			}

			result = joinStatements(result, 'do');
			result = joinStatements(result, formatStatementList(statement.body, baseOptions));
			result = joinStatements(result, 'end');

		} else if (statementType == 'LabelStatement') {

			// The identifier names in a `LabelStatement` can safely be renamed
			result = '::' + generateIdentifier(statement.label.name) + '::';

		} else if (statementType == 'GotoStatement') {

			// The identifier names in a `GotoStatement` can safely be renamed
			result = 'goto ' + generateIdentifier(statement.label.name);

		} else {

			throw TypeError('Unknown statement type: `' + statementType + '`');

		}

		return result;
	};

	var minify = function(argument, baseOptions) {
		// `argument` can be a Lua code snippet (string)
		// or a luaparse-compatible AST (object)
		var ast = typeof argument == 'string'
			? parse(argument)
			: argument;

		// (Re)set temporary identifier values
		identifierMap = {};
		identifiersInUse = [];
		// This is a shortcut to help generate the first identifier (`a`) faster
		currentIdentifier = '9';

		// Make sure global variable names aren't renamed
		if (ast.globals) {
			each(ast.globals, function(object) {
				var name = object.name;
				identifierMap[name] = name;
				identifiersInUse.push(name);
			});
		} else {
			throw Error('Missing required AST property: `globals`');
		}

		return formatStatementList(ast.body, baseOptions);
	};

	/*--------------------------------------------------------------------------*/

	var luamin = {
		'version': '1.0.4',
		'minify': minify
	};

	// Some AMD build optimizers, like r.js, check for specific condition patterns
	// like the following:
	if (
		typeof define == 'function' &&
		typeof define.amd == 'object' &&
		define.amd
	) {
		define(function() {
			return luamin;
		});
	}	else if (freeExports && !freeExports.nodeType) {
		if (freeModule) { // in Node.js or RingoJS v0.8.0+
			freeModule.exports = luamin;
		} else { // in Narwhal or RingoJS v0.7.0-
			extend(freeExports, luamin);
		}
	} else { // in Rhino or a web browser
		root.luamin = luamin;
	}

}(this));
