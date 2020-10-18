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
	var shortenedGlobalIdentifiers;
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
					identifiersInUse.has(currentIdentifier)
				) {
					return generateIdentifier(originalName);
				}
				identifierMap[originalName] = currentIdentifier;
				return currentIdentifier;
			}
			--position;
		}
		currentIdentifier = 'a' + generateZeroes(length);
		if (identifiersInUse.has(currentIdentifier)) {
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

	var formatBase = function(base, preferences) {
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
		result += formatExpression(base, preferences);
		if (needsParens) {
			result += ')';
		}
		return result;
	};

	var formatExpression = function(expression, preferences, options) {

		options = extend({
			'precedence': 0,
			'preserveIdentifiers': false,
			'forceGenerateIdentifiers': false
		}, options);

		var result = '';
		var currentPrecedence;
		var associativity;
		var operator;

		var expressionType = expression.type;

		if (expressionType == 'Identifier') {

			// forceGenerateIdentifiers has precedence over the rest

			// Do not minify global variables, unless we force global variable minification and
			// they don't start with '_'.

			// When using minifyAllGlobalVars, all global vars not starting with '_' have already been minified
			// to pre-fill identifierMap and identifiersInUse, aSet() we always call generateIdentifier, but only to find the existing mapping.

			// When using minifyAssignedGlobalVars or minifyGlobalFunctions, we should only minify globals that have already been shortened previously
			// (we assume that all global assignments have been done previously in the code), so those registered in shortenedGlobalIdentifiers.

			// In both cases, the '_' check is optional as globals starting with _ have been protected during pre-pass on ast.globals in minify.
			result = options.forceGenerateIdentifiers ||
				(
					(
						expression.isLocal ||
						(
							preferences.minifyAllGlobalVars ||
							shortenedGlobalIdentifiers.has(expression.name)
						) && expression.name.substr(0, 1) != "_"
					) && !options.preserveIdentifiers
				)
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

			result = formatExpression(expression.left, preferences, {
				'precedence': currentPrecedence,
				'direction': 'left',
				'parent': operator
			});
			result = joinStatements(result, operator);
			result = joinStatements(result, formatExpression(expression.right, preferences, {
				'precedence': currentPrecedence,
				'direction': 'right',
				'parent': operator
			}));

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
				formatExpression(expression.argument, preferences, {
					'precedence': currentPrecedence
				})
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

			result = formatBase(expression.base, preferences) + '(';

			each(expression.arguments, function(argument, needsComma) {
				result += formatExpression(argument, preferences);
				if (needsComma) {
					result += ',';
				}
			});
			result += ')';

		} else if (expressionType == 'TableCallExpression') {

			result = formatExpression(expression.base, preferences) +
				formatExpression(expression.arguments, preferences);

		} else if (expressionType == 'StringCallExpression') {

			result = formatExpression(expression.base, preferences) +
				formatExpression(expression.argument, preferences);

		} else if (expressionType == 'IndexExpression') {

			result = formatBase(expression.base, preferences) + '[' +
				formatExpression(expression.index, preferences) + ']';

		} else if (expressionType == 'MemberExpression') {

			// Aggressive minification note: this will only affect key strings without square brackets or quotes
			//   ex: t = { key1 = 1, key2 = 2 }
			// Strings inside square brackets are handled above, so you can use that to preserve
			//   key identifiers you intend to access dynamically via string.
			// Alternatively, you can protect names from shortening by starting them with "_"
			//   ex: t = { ["key1_preserved"] = 1 }
			result = formatBase(expression.base, preferences) + expression.indexer +
				formatExpression(expression.identifier, preferences, {
					'preserveIdentifiers': true,
					'forceGenerateIdentifiers': preferences.minifyMemberNames &&
						expression.identifier.name.substr(0, 1) != "_"
				});

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
			result = joinStatements(result, formatStatementList(expression.body, preferences));
			result = joinStatements(result, 'end');

		} else if (expressionType == 'TableConstructorExpression') {

			result = '{';

			each(expression.fields, function(field, needsComma) {
				if (field.type == 'TableKey') {
					result += '[' + formatExpression(field.key, preferences) + ']=' +
						formatExpression(field.value, preferences);
				} else if (field.type == 'TableValue') {
					result += formatExpression(field.value, preferences);
				} else { // at this point, `field.type == 'TableKeyString'`
					// see MemberExpression case above for explanations about forceGenerateIdentifiers
					result += formatExpression(field.key, preferences, {
						// TODO: keep track of nested scopes (#18)
						'preserveIdentifiers': true,
						'forceGenerateIdentifiers': preferences.minifyTableKeyStrings  &&
							field.key.name.substr(0, 1) != "_"
					}) + '=' + formatExpression(field.value, preferences);
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

	var formatStatementList = function(body, preferences) {
		var result = '';
		each(body, function(statement) {
			var separator = preferences.newlineSeparator ? '\n' : ';';
			result = joinStatements(result, formatStatement(statement, preferences), separator);
		});
		return result;
	};

	var formatStatement = function(statement, preferences) {
		var result = '';
		var statementType = statement.type;

		if (statementType == 'AssignmentStatement') {

			// left-hand side
			each(statement.variables, function(variable, needsComma) {
				var expressionType = variable.type;

				// when using minifyAssignedGlobalVars, detect `global_var = value` patterns to register global_var as an assigned global identifier,
				// optionally, we can also generate the identifier now so formatExpression will just have to find the existing mapping
				//  (else formatExpression would generate it on its own anyway)
				// check that the expression type is Identifier, as we are not interested in global_table.member = value (as global_table would have
				//  been assigned previously, and member minification is done with another option, minifyMemberNames)
				// this, however, includes `global_a, global_b = value1, value2`` which is split into `global_a = value1`, `global_b = value2` on parsing
				// as usual, ignore variables starting with '_', but the check is optional as globals starting with _ have been protected during pre-pass
				//  on ast.globals in minify.
				if (preferences.minifyAssignedGlobalVars && expressionType == 'Identifier' && !variable.isLocal && variable.name.substr(0, 1) != "_") {
					shortenedGlobalIdentifiers.add(variable.name);
					generateIdentifier(variable.name);
				}

				result += formatExpression(variable, preferences);
				if (needsComma) {
					result += ',';
				}
			});

			// right-hand side
			result += '=';
			each(statement.init, function(init, needsComma) {
				result += formatExpression(init, preferences);
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
					result += formatExpression(init, preferences);
					if (needsComma) {
						result += ',';
					}
				});
			}

		} else if (statementType == 'CallStatement') {

			result = formatExpression(statement.expression, preferences);

		} else if (statementType == 'IfStatement') {

			result = joinStatements(
				'if',
				formatExpression(statement.clauses[0].condition, preferences)
			);
			result = joinStatements(result, 'then');
			result = joinStatements(
				result,
				formatStatementList(statement.clauses[0].body, preferences)
			);
			each(statement.clauses.slice(1), function(clause) {
				if (clause.condition) {
					result = joinStatements(result, 'elseif');
					result = joinStatements(result, formatExpression(clause.condition, preferences));
					result = joinStatements(result, 'then');
				} else {
					result = joinStatements(result, 'else');
				}
				result = joinStatements(result, formatStatementList(clause.body, preferences));
			});
			result = joinStatements(result, 'end');

		} else if (statementType == 'WhileStatement') {

			result = joinStatements('while', formatExpression(statement.condition, preferences));
			result = joinStatements(result, 'do');
			result = joinStatements(result, formatStatementList(statement.body, preferences));
			result = joinStatements(result, 'end');

		} else if (statementType == 'DoStatement') {

			result = joinStatements('do', formatStatementList(statement.body, preferences));
			result = joinStatements(result, 'end');

		} else if (statementType == 'ReturnStatement') {

			result = 'return';

			each(statement.arguments, function(argument, needsComma) {
				result = joinStatements(result, formatExpression(argument, preferences));
				if (needsComma) {
					result += ',';
				}
			});

		} else if (statementType == 'BreakStatement') {

			result = 'break';

		} else if (statementType == 'RepeatStatement') {

			result = joinStatements('repeat', formatStatementList(statement.body, preferences));
			result = joinStatements(result, 'until');
			result = joinStatements(result, formatExpression(statement.condition, preferences))

		} else if (statementType == 'FunctionDeclaration') {

			// global functions declared with assignment `foo = function() end` are automatically minified
			//   by minifyAssignedGlobalVars
			// however, minifyGlobalFunctions allows to minify the alternative writing `function foo() end`
			//   but `foo` would be parsed below in formatExpression as any global identifier,
			//   without knowing the context (i.e. we are not in an assignment but in an equivalent function declaration)
			// so, similarly to the AssignmentStatement case above with minifyAssignedGlobalVars, we handle this
			//  at the statement level before diving in formatExpression (and again, we can optionally generate the ID now)
			// as usual, ignore variables starting with '_', but the check is optional as globals starting with _ have been protected during pre-pass
			//  on ast.globals in minify.
			if (preferences.minifyGlobalFunctions && statement.identifier.type == 'Identifier' && !statement.isLocal && statement.identifier.name.substr(0, 1) != "_") {
				shortenedGlobalIdentifiers.add(statement.identifier.name);
				generateIdentifier(statement.identifier.name);
			}

			result = (statement.isLocal ? 'local ' : '') + 'function ';
			result += formatExpression(statement.identifier, preferences);
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
			result = joinStatements(result, formatStatementList(statement.body, preferences));
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
				result = joinStatements(result, formatExpression(iterator, preferences));
				if (needsComma) {
					result += ',';
				}
			});

			result = joinStatements(result, 'do');
			result = joinStatements(result, formatStatementList(statement.body, preferences));
			result = joinStatements(result, 'end');

		} else if (statementType == 'ForNumericStatement') {

			// The variables in a `ForNumericStatement` are always local
			result = 'for ' + generateIdentifier(statement.variable.name) + '=';
			result += formatExpression(statement.start, preferences) + ',' +
				formatExpression(statement.end, preferences);

			if (statement.step) {
				result += ',' + formatExpression(statement.step, preferences);
			}

			result = joinStatements(result, 'do');
			result = joinStatements(result, formatStatementList(statement.body, preferences));
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

	var minify = function(argument, preferences) {
		// define default preferences
		preferences = extend({
			'newlineSeparator': false,
			'minifyMemberNames': false,
			'minifyTableKeyStrings': false,
			'minifyAssignedGlobalVars': false,
			'minifyGlobalFunctions': false,
			'minifyAllGlobalVars': false,
		}, preferences);

		// `argument` can be a Lua code snippet (string)
		// or a luaparse-compatible AST (object)
		var ast = typeof argument == 'string'
			? parse(argument)
			: argument;

		// (Re)set temporary identifier values
		identifierMap = {};
		identifiersInUse = new Set();
		// Set of global identifiers that should be shortened on declaration and for any further usage
		// (only used with minifyAssignedGlobalVars and minifyGlobalFunctions; minifyAllGlobalVars doesn't need it and simply shortens all global identifiers)
		shortenedGlobalIdentifiers = new Set();
		// This is a shortcut to help generate the first identifier (`a`) faster
		currentIdentifier = '9';

		// Make sure global variable names aren't renamed
		// unless we want to minify global variables (assigned or all) and the name doesn't start with '_'.
		if (ast.globals) {
			each(ast.globals, function(object) {
				var name = object.name;
				if (!(preferences.minifyAssignedGlobalVars || preferences.minifyGlobalFunctions || preferences.minifyAllGlobalVars) || name.substr(0, 1) == "_") {
					identifierMap[name] = name;
					identifiersInUse.add(name);
				} else {
					// minifyAssignedGlobalVars should wait for assignment checking to see which global variables should be minified
					// minifyAllGlobalVars, however, knows we will minify them all, so we just generate the shortened identifiers now
					// (optional as they would be minified eventually during formatStatementList, but convenient to spot globals
					// as they will have the first minified names in the alphabetical order, namely 'a', 'b', etc.)
					if (preferences.minifyAllGlobalVars) {
						generateIdentifier(name);
					}
				}
			});
		} else {
			throw Error('Missing required AST property: `globals`');
		}

		return formatStatementList(ast.body, preferences);
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
