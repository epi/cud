/**
CTFEable C expression parser.

Authors: $(LINK2 https://github.com/epi, Adrian Matoga)
Copyright: © 2017 Adrian Matoga
License: $(LINK2 http://www.boost.org/users/license.html, BSL-1.0).
*/
module cud.parse;

import std.range : front, popFront, empty;

import cud.expr;
import cud.token;
import cud.type;

version(unittest) {
	import std.algorithm : map, filter, equal;
	import std.range : array;
	import std.exception : assertThrown, assertNotThrown;
	import std.stdio;
	import cud.test;
}

class Decl
{
	Location location;

	this(Location location)
	{
		this.location = location;
	}
}

class EnumConstDecl : Decl
{
	string name;
	long value;

	this(Location location, string name, long value)
	{
		super(location);
		this.name = name;
		this.value = value;
	}
}

class EnumDecl : Decl
{
	string tag;
	EnumConstDecl[] list;

	this(Location location, string tag, EnumConstDecl[] list)
	{
		super(location);
		this.tag = tag;
		this.list = list;
	}
}

struct Parser {
	private Target m_target;
	private const(Token)[] m_input;

	this(const(Token)[] input)
	{
		m_input = input;
		m_target = new Target;
	}

	@property Token current() pure nothrow const @nogc
	{
		return m_input[0];
	}

	Token match(string token_kinds)()
	{
		const tok = m_input.front;
		switch (tok.kind) {
		foreach (k; tks!token_kinds) {
		case k:
			m_input.popFront;
			return tok;
		}
		default:
			return Token.init;
		}
	}

	Token peek(token_kinds...)()
	{
		import std.meta : aliasSeqOf;
		foreach (i, ks; token_kinds) {
			const tok = m_input[i];
			switch (tok.kind) {
			foreach (k; tks!ks) {
			case k:
				return tok;
			}
			default:
				return Token.init;
			}
		}
	}

	Token expect(string token_kinds)()
	{
		if (const tok = match!token_kinds)
			return tok;
		error(m_input[0].location, "Expected " ~ token_kinds ~
			", not " ~ m_input[0].spelling);
		assert(0);
	}

	// returns true if the front token starts a type name
	bool peekTypeName()
	{
		import std.meta : AliasSeq;
		alias type_specifier_tokens = AliasSeq!(
			tks!`void char short int long float double signed unsigned`,
			tks!`_Bool _Complex`,
			tks!`_Atomic struct union enum`);

		const tok = m_input[0];
		switch (tok.kind) {
		foreach (k; type_specifier_tokens) {
		case k: return true;
		}
		case tk!`identifier`:
			return isType(tok.spelling);
		default:
			return false;
		}
	}

	/*
	primary-expression:
		identifier
		constant
		string-literal
		( expression )
		generic-selection

	postfix-expression:
		primary-expression
		postfix-expression [ expression ]
		postfix-expression ( argument-expression-listopt )
		postfix-expression . identifier
		postfix-expression -> identifier
		postfix-expression ++
		postfix-expression --
		( type-name ) { initializer-list }
		( type-name ) { initializer-list , }

	argument-expression-list:
		assignment-expression
		argument-expression-list , assignment-expression
	*/
	Expr parsePostfixExpr()
	{
		import std.meta : AliasSeq;
		Expr result = {
			foreach (typestr; AliasSeq!("uint", "int", "long", "ulong", "char")) {
				if (auto tok = match!(typestr ~ "const")) {
					return new IntConst(
						tok.location,
						mixin("m_target." ~ typestr ~ "_"),
						tok.longValue);
				}
			}
			if (match!`stringliteral`) {
				assert(0, "not implemented");
			} else if (match!`identifier`) {
				assert(0, "not implemented");
			} else {
				error(current.location,
					"Expected expression, not '%s'", current.spelling);
				assert(0);
			}
		}();
		for (;;) {
			if (auto tok = match!`[`) {
				auto e = parseExpr();
				expect!`]`;
				result = new IndexExpr(tok.location, result, e);
			} else if (auto tok = match!`(`) {
				Expr[] args;
				if (!match!`)`) {
					for (;;) {
						args ~= parseAssignExpr();
						if (match!`)`)
							break;
						expect!`,`;
					}
				}
				result = new CallExpr(tok.location, result, args);
			} else if (auto tok = match!`. ->`) {
				auto ident = expect!`identifier`;
				result = new MemberExpr(
					tok.location, result, ident.spelling, tok.kind == tk!`->`);
			} else if (auto tok = match!`++ --`) {
				result = new UnaryExpr(
					tok.location,
					tok.kind == tk!`++` ? tk!`post++` : tk!`post--`,
					result);
			} else {
				break;
			}
		}
		return result;
	}

	/*
	unary-expression:
		postfix-expression
		++ unary-expression
		-- unary-expression
		unary-operator cast-expression
		sizeof unary-expression
		sizeof ( type-name )
		_Alignof ( type-name )

	unary-operator: one of
		& * + - ~ !
	*/
	Expr parseUnaryExpr()
	{
		if (auto op = match!`++ --`) {
			return new UnaryExpr(op.location, op.kind, parseUnaryExpr());
		} else if (auto op = match!`& * + - ~ !`) {
			return new UnaryExpr(op.location, op.kind, parseCastExpr());
		} else if (auto op = match!`sizeof`) {
			if (auto lparen = match!`(`) {
				Type t = peekTypeName()
					? parseTypeName()
					: parseExpr().type;
				expect!`)`;
				return new TypeTraitExpr(
					op.location, m_target.size_t_, TypeTrait.sizeof_, t);
			} else {
				return new TypeTraitExpr(
					op.location, m_target.size_t_, TypeTrait.sizeof_,
					parseExpr().type);
			}
		} else if (auto op = match!`_Alignof`) {
			assert(0, "Not implemented");
		}
		return parsePostfixExpr();
	}

	bool isVariable(string ident)
	{
		// TODO
		return true;
	}

	bool isType(string ident)
	{
		// TODO
		return true;
	}

	/*
	cast-expression:
		unary-expression
		( type-name ) cast-expression
	*/
	Expr parseCastExpr()
	{
		if (auto lparen = match!`(`) {
			if (peekTypeName()) {
				Type t = parseTypeName();
				expect!`)`;
				// TODO: compound literal
				return new CastExpr(lparen.location, t, parseCastExpr());
			} else {
				Expr e = parseExpr();
				expect!`)`;
				return e;
			}
		} else {
			return parseUnaryExpr();
		}
	}

	Expr parseBinaryExpr(alias subparser, string tokenKinds)()
	{
		Expr e = subparser();
		for (;;) {
			if (auto tok = match!tokenKinds)
				e = new BinaryExpr(tok.location, tok.kind, e, subparser());
			else
				break;
		}
		return e;
	}

	Expr parseMulExpr()
	{
		return parseBinaryExpr!(parseCastExpr, `* / %`);
	}

	Expr parseAddExpr()
	{
		return parseBinaryExpr!(parseMulExpr, `+ -`);
	}

	Expr parseShiftExpr()
	{
		return parseBinaryExpr!(parseAddExpr, `<< >>`);
	}

	Expr parseRelExpr()
	{
		return parseBinaryExpr!(parseShiftExpr, `<= >= < >`);
	}

	Expr parseEqualityExpr()
	{
		return parseBinaryExpr!(parseRelExpr, `== !=`);
	}

	Expr parseAndExpr()
	{
		return parseBinaryExpr!(parseEqualityExpr, `&`);
	}

	Expr parseXorExpr()
	{
		return parseBinaryExpr!(parseAndExpr, `^`);
	}

	Expr parseOrExpr()
	{
		return parseBinaryExpr!(parseXorExpr, `|`);
	}

	Expr parseLogicalAndExpr()
	{
		return parseBinaryExpr!(parseOrExpr, `&&`);
	}

	Expr parseLogicalOrExpr()
	{
		return parseBinaryExpr!(parseLogicalAndExpr, `||`);
	}

	/*
	conditional-expression:
		logical-OR-expression
		logical-OR-expression ? expression : conditional-expression
	*/
	Expr parseCondExpr()
	{
		auto cond = parseLogicalOrExpr();
		auto qtok = match!`?`;
		if (!qtok)
			return cond;
		auto e1 = parseExpr();
		expect!`:`;
		auto e2 = parseCondExpr();
		return new CondExpr(qtok.location, cond, e1, e2);
	}

	/*
	assignment-expression:
		conditional-expression
		unary-expression assignment-operator assignment-expression

	assignment-operator: one of
		= *= /= %= += -= <<= >>= &= ^= |=

	expression:
		assignment-expression
		expression , assignment-expression

	constant-expression:
		conditional-expression
	*/
	Expr parseAssignExpr()
	{
		return parseBinaryExpr!(parseCondExpr, `= *= /= %= += -= <<= >>= &= ^= |=`);
	}

	Expr parseExpr()
	{
		return parseBinaryExpr!(parseAssignExpr, `,`);
	}

	alias parseConstantExpr = parseCondExpr;

	static immutable TokenKind[][tk!`max_keyword`] conflicts =
	{
		TokenKind[][tk!`max_keyword`] result;


		return result;
	}();

	Type typeFromTokens(ref Token[tk!`max_keyword`] tokens, bool longlong)
	{
		if (tokens[tk!`void`])
			return m_target.void_;
		if (tokens[tk!`_Bool`])
			return m_target.bool_;
		if (tokens[tk!`_Complex`]) {
			if (tokens[tk!`float`])
				return m_target.float_Complex_;
			if (tokens[tk!`double`])
				return tokens[tk!`long`] ? m_target.longdouble_Complex_
					: m_target.double_Complex_;
		}
		if (tokens[tk!`float`])
			return m_target.float_;
		if (tokens[tk!`double`])
			return tokens[tk!`long`] ? m_target.longdouble_
				: m_target.double_;
		if (tokens[tk!`char`])
			return tokens[tk!`signed`] ? m_target.schar_
				: tokens[tk!`unsigned`] ? m_target.uchar_
				: m_target.char_;
		if (tokens[tk!`short`])
			return tokens[tk!`signed`] ? m_target.sshort_
				: tokens[tk!`unsigned`] ? m_target.ushort_
				: m_target.short_;
		if (tokens[tk!`long`]) {
			if (longlong) {
				return tokens[tk!`signed`] ? m_target.slonglong_
					: tokens[tk!`unsigned`] ? m_target.ulonglong_
					: m_target.longlong_;
			} else {
				return tokens[tk!`signed`] ? m_target.slong_
					: tokens[tk!`unsigned`] ? m_target.ulong_
					: m_target.long_;
			}
		}
		return tokens[tk!`signed`] ? m_target.sint_
			: tokens[tk!`unsigned`] ? m_target.uint_
			: m_target.int_;
	}

	void parseSpecifiers(Ts...)(out Ts result)
	{
		import std.meta : staticIndexOf;
		enum itype = staticIndexOf!(Type, Ts);
		enum iqual = staticIndexOf!(Qualifiers, Ts);
		enum istc = staticIndexOf!(StorageClass, Ts);
		enum ifunc = staticIndexOf!(FunctionSpecifiers, Ts);
		enum ialign = staticIndexOf!(Expr, Ts);

		Token[tk!`max_keyword`] tokens;
		bool longlong;

		for (;;) {
			static if (iqual >= 0) {
				if (auto tok = match!`const restrict volatile _Atomic`) {
					tokens[tok.kind] = tok;
					static if (itype >= 0) {
						if (tok.kind == tk!`_Atomic` && match!`(`) {
							result[itype] =
								parseTypeName(); // wrap in AtomicType later
							expect!`)`;
						}
					}
					continue;
				}
			}

			static if (itype >= 0) {
				enum type_specifiers =
					`void char short int long float double signed unsigned ` ~
					`_Bool _Complex`;
				if (auto tok = match!type_specifiers) {
					// TODO: conflicts
					tokens[tok.kind] = tok;
					continue;
				} else if (auto tok = match!`enum`) {
					tokens[tok.kind] = tok;
					result[itype] = m_target.enum_(parseEnum(tok));
					continue;
				} else if (auto tok = match!`struct union`) {
					tokens[tok.kind] = tok;
					result[itype] = parseStructOrUnion(tok.kind);
					continue;
				} else if (auto tok = match!`identifier`) {
					// TODO: find typedef
					assert(0);
				}
			}

			static if (istc >= 0 && ifunc >= 0 && ialign >= 0) {
				enum storage_classes =
					`typedef extern static _Thread_local auto register ` ~
					`inline _Noreturn`;
				if (auto tok = match!storage_classes) {
					// TODO: conflicts
					tokens[tok.kind] = tok;
					continue;
				} else if (auto tok = match!`_Alignas`) {
					tokens[tok.kind] = tok;
					expect!`(`;
					result[ialign] = peekTypeName()
						? new TypeTraitExpr(
							tok.location,
							m_target.size_t_,
							TypeTrait.alignof_,
							parseTypeName())
						: parseConstantExpr();
					expect!`)`;
					continue;
				}
			}
			break;
		}

		static if (iqual >= 0) {
			result[iqual] = Qualifiers(
				!!tokens[tk!`const`],
				!!tokens[tk!`volatile`],
				!!tokens[tk!`restrict`]);
		}
		static if (itype >= 0) {
			result[itype] = typeFromTokens(tokens, longlong);
			if (tokens[tk!`_Atomic`])
				result[itype] = result[itype].atomic;
			static if (iqual >= 0) {
				if (result[iqual])
					result[itype] = result[itype].qualified(result[iqual]);
			}
		}
	}

	version(unittest)
	Type parseDeclSpecifiers()
	{
		Type t;
		Qualifiers q;
		StorageClass s;
		FunctionSpecifiers f;
		Expr e;
		parseSpecifiers(t, q, s, f, e);
		return t;
	}

	void skipParens()
	{
		int nest;
		do {
			if (match!`(`) {
				nest++;
			} else if (match!`)`) {
				nest--;
			} else if (auto tok = match!`eof ;`) {
				error(tok.location, "expected ')'");
				assert(0);
			} else {
				m_input = m_input[1 .. $];
			}
		} while (nest);
	}

	/*
	abstract-declarator:
		pointer
		pointer(opt) direct-abstract-declarator

	direct-abstract-declarator:
		( abstract-declarator )
		direct-abstract-declarator(opt) [ type-qualifier-list(opt)
				assignment-expression(opt) ]
		direct-abstract-declarator(opt) [ static type-qualifier-list(opt)
				assignment-expression ]
		direct-abstract-declarator(opt) [ type-qualifier-list static
				assignment-expression ]
		direct-abstract-declarator(opt) [ * ]
		direct-abstract-declarator(opt) ( parameter-type-list(opt) )
	*/
	Type parseAbstractDeclarator(Type t)
	{
		while (match!`*`) {
			t = m_target.pointer(t);
			Qualifiers q;
			parseSpecifiers(q);
			if (q)
				t = t.qualified(q);
		}
		const(Token)[] paren_decl;
		if (peek!(`(`, `( [ *`)) {
			paren_decl = m_input;
			skipParens();
		}
		for (;;) {
			if (match!`(`) {
				// TODO: parameter type list
				expect!`)`;
			} else if (match!`[`) {
				if (match!`*`) {
					// TODO: enforce(inside function prototype)
					t = t.variableArray(null);
				} else {
					Token static_ = match!`static`;
					Qualifiers q;
					parseSpecifiers(q);
					if (!static_)
						static_ = match!`static`;
					// TODO: if (static_) enforce(inside func proto)
					if (!peek!`]`) {
						Expr e = parseAssignExpr();
						t = t.constantArray(e.evaluate!ulong);
					} else {
						t = t.incompleteArray;
					}
				}
				expect!`]`;
			} else {
				break;
			}
		}

		if (paren_decl) {
			import std.algorithm : swap;
			swap(paren_decl, m_input);
			expect!`(`;
			t = parseAbstractDeclarator(t);
			expect!`)`;
			swap(paren_decl, m_input);
		}

		return t;
	}

	/*
	type-name:
		specifier-qualifier-list abstract-declarator(opt)
	*/
	Type parseTypeName()
	{
		Type result;
		Qualifiers q;
		parseSpecifiers(result, q);
		return parseAbstractDeclarator(result);
	}

	/*
	enum-specifier:
		enum identifieropt { enumerator-list }
		enum identifieropt { enumerator-list , }
		enum identifier

	enumerator-list:
		enumerator
		enumerator-list , enumerator

	enumerator:
		enumeration-constant
		enumeration-constant = constant-expression
	*/
	EnumDecl parseEnum(Token enum_tok)
	{
		string tag;
		EnumConstDecl[] list;

		if (auto tok = match!`identifier`)
			tag = tok.spelling;

		if (tag ? match!`{` : expect!`{`) {
			long value = -1;
			while (!match!`}`) {
				const idtok = expect!`identifier`;
				if (match!`=`)
					value = evaluate!long(parseConstantExpr());
				else
					value++;
				list ~= new EnumConstDecl(
					idtok.location, idtok.spelling, value);
				if (match!`}`)
					break;
				expect!`,`;
			}
			if (list.length == 0) {
				error(enum_tok.location, "empty enum declaration");
				assert(0);
			}
		}

		return new EnumDecl(enum_tok.location, tag, list);
	}

	version(unittest)
	auto parseEnum() { return parseEnum(expect!`enum`); }

	Type parseStructOrUnion(TokenKind k)
	{
		return null;
	}

	/+
	enum SpecifierSet
	{
		declarationSpecifiers,
		specifierQualifierList,
		typeQualifierList,
	}

	// 6.7. Decls
	// declaration-specifiers <
	//  ( storage-class-specifier / type-specifier / type-qualifier / function-specifier / alignment-specifier )+
	//
	// 6.7.2.1. Structure and union specifiers
	// 6.7.7. Type names
	// specifier-qualifier-list <
	//  ( type-specifier / type-qualifier )
	//
	// 6.7.6 Declarators
	// type-qualifier-list <
	//  type-qualifier+
	auto parseSpecifiers(SpecifierSet set)(ref const(Token)[] ref_input)
	{
		struct Specifiers
		{
			// 6.7.3 Type qualifiers
			Token const_;
			Token restrict_;
			Token volatile_;
			Token _Atomic_;

			@property bool anyQualifier() const pure nothrow @safe
			{
				return const_ || restrict_ || volatile_ || _Atomic_;
			}

			// 6.7.2 Type specifiers
			static if (set == SpecifierSet.specifierQualifierList || set == SpecifierSet.declarationSpecifiers) {
				Token void_;
				Token char_;
				Token short_;
				Token int_;
				Token long_;
				Token long_long_;
				Token float_;
				Token double_;
				Token signed_;
				Token unsigned_;
				Token _Bool_;
				Token _Complex_;
				Token other;
				Type type; // typedef, struct, union, enum, or atomic
			}

			// 6.7.1 Storage class specifiers
			// 6.7.4 Function specifiers
			// 6.7.5 Alignment specifier
			static if (set == SpecifierSet.declarationSpecifiers) {
				Token typedef_;
				Token extern_;
				Token static_;
				Token _Thread_local_;
				Token auto_;
				Token register_;

				Token inline_;
				Token _Noreturn_;

				Token _Alignas_;
				Expr[] alignment;
			}
		}

		Specifiers spec;

		void checkConflicts(Token checked_token, Token*[] conflicting_specifiers...)
		{
			foreach (pspec; conflicting_specifiers) {
				if (*pspec) {
					error(checked_token.location,
						"cannot combine '%s' with previous specifier '%s' at %s",
						checked_token.spelling, pspec.spelling, pspec.location);
					assert(0);
				}
			}
		}

		const(Token)[] input = ref_input;
		for (;;) {
			const tok = input.front;
			switch (tok.kind) {
				case tk!`const`:
					spec.const_ = tok;
					break;
				case tk!`restrict`:
					spec.restrict_ = tok;
					break;
				case tk!`volatile`:
					spec.volatile_ = tok;
					break;
				case tk!`_Atomic`:
					static if (set == SpecifierSet.typeQualifierList) {
						spec._Atomic_ = tok;
					} else static if (set == SpecifierSet.declarationSpecifiers
						|| set == SpecifierSet.specifierQualifierList)
					{
						if (input[1].kind != tk!`(`) {
							spec._Atomic_ = tok;
						} else {
							checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
								&spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
								&spec._Bool_, &spec._Complex_, &spec.other);
							input = input[2 .. $];
							spec.other = tok;
							spec.type = new AtomicType(
								() {
									if (auto t = parseTypeName(input))
										return t;
									error(input.front.location,
										"found '%s' when expecting type name in atomic type specifier",
										input.front.spelling);
									assert(0);
								}());
							if (input.front.kind != tk!`)`) {
								error(input.front.location,
									"found '%s' when expecting ')' closing atomic type specifier",
									input.front.spelling);
								assert(0);
							}
						}
					} else {
						static assert(0);
					}
					break;
				static if (set == SpecifierSet.specifierQualifierList
					|| set == SpecifierSet.declarationSpecifiers)
				{
				case tk!`void`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
						&spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
						&spec._Bool_, &spec._Complex_, &spec.other);
					spec.void_ = tok;
					break;
				case tk!`char`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
						&spec.float_, &spec.double_, &spec._Bool_, &spec._Complex_, &spec.other);
					spec.char_ = tok;
					break;
				case tk!`short`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.long_,
						&spec.float_, &spec.double_, &spec._Bool_, &spec._Complex_, &spec.other);
					spec.short_ = tok;
					break;
				case tk!`int`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.int_,
						&spec.float_, &spec.double_, &spec._Bool_, &spec._Complex_, &spec.other);
					spec.int_ = tok;
					break;
				case tk!`long`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.long_long_,
						&spec.float_, &spec.double_, &spec._Bool_, &spec._Complex_, &spec.other);
					(spec.long_ ? spec.long_long_ : spec.long_) = tok;
					break;
				case tk!`float`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
						&spec.long_long_, &spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
						&spec._Bool_, &spec.other);
					spec.float_ = tok;
					break;
				case tk!`double`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_,
						&spec.long_long_, &spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
						&spec._Bool_, &spec.other);
					spec.double_ = tok;
					break;
				case tk!`signed`:
					checkConflicts(tok, &spec.void_, &spec.float_, &spec.double_, &spec.unsigned_,
						&spec._Bool_, &spec.other);
					spec.signed_ = tok;
					break;
				case tk!`unsigned`:
					checkConflicts(tok, &spec.void_, &spec.float_, &spec.double_, &spec.signed_,
						&spec._Bool_, &spec.other);
					spec.unsigned_ = tok;
					break;
				case tk!`_Bool`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
						&spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
						&spec._Bool_, &spec._Complex_, &spec.other);
					spec._Bool_ = tok;
					break;
				case tk!`_Complex`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_,
						&spec.long_long_, &spec.signed_, &spec.unsigned_, &spec._Bool_);
					spec._Complex_ = tok;
					break;
				case tk!`enum`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
						&spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
						&spec._Bool_, &spec._Complex_, &spec.other);
					spec.other = tok;
						auto decl = parseEnumSpecifier(input);
						// type = decl.type;
					continue;
				case tk!`struct`:
				case tk!`union`:
					assert(0);
				}
				static if (set == SpecifierSet.declarationSpecifiers) {
				case tk!`typedef`:
					checkConflicts(tok, &spec.typedef_, &spec.static_, &spec.extern_,
						&spec._Thread_local_, &spec.auto_, &spec.register_);
					spec.typedef_ = tok;
					break;
				case tk!`extern`:
					checkConflicts(tok, &spec.typedef_, &spec.static_, &spec.extern_,
						&spec.auto_, &spec.register_);
					spec.extern_ = tok;
					break;
				case tk!`static`:
					checkConflicts(tok, &spec.typedef_, &spec.static_, &spec.extern_,
						&spec.auto_, &spec.register_);
					spec.static_ = tok;
					break;
				case tk!`_Thread_local`:
					checkConflicts(tok, &spec.typedef_, &spec.auto_, &spec.register_);
					spec._Thread_local_ = tok;
					break;
				case tk!`auto`:
					checkConflicts(tok, &spec.typedef_, &spec.static_, &spec.extern_,
						&spec._Thread_local_, &spec.auto_, &spec.register_);
					spec.auto_ = tok;
					break;
				case tk!`register`:
					checkConflicts(tok, &spec.typedef_, &spec.static_, &spec.extern_,
						&spec._Thread_local_, &spec.auto_, &spec.register_);
					spec.auto_ = tok;
					break;
				case tk!`inline`:
					spec.inline_ = tok;
					break;
				case tk!`_Noreturn`:
					spec._Noreturn_ = tok;
					break;
				case tk!`_Alignas`: {
					spec._Alignas_ = tok;
					input.popFront;
					if (input.front.kind != tk!`(`) {
						error(input.front.location, "expected '(' after _Alignas, not '%s'",
							input.front.spelling);
						assert(0);
					}
					input.popFront;
					Expr expr = parseConditionalExpr(input);
					if (!expr) {
						Type type = parseTypeName(input);
						if (!type) {
							error(input.front.location,
								"expected constant expression or type name");
							assert(0);
						}
						expr = new TypeTraitExpr(tok.location, TypeTrait.alignof_, type);
					}
					spec.alignment ~= expr;
					if (input.front.kind != tk!`)`) {
						error(input.front.location, "expected ')'");
						assert(0);
					}
					break;
				}
				}
				default:
					ref_input = input;
					return spec;
			}
			input.popFront;
		}
	}

	alias parseTypeQualifierList =
		parseSpecifiers!(SpecifierSet.typeQualifierList);

	Type specifiersToType(S)(ref S spec, bool implicit_int = false)
	{
		if (spec.long_long_) {
			if (spec.signed_)
				return m_target.get!`slonglong`;
			else if (spec.unsigned_)
				return m_target.get!`ulonglong`;
			else
				return m_target.get!`longlong`;
		} else if (spec.long_) {
			if (spec.signed_)
				return m_target.get!`slong`;
			else if (spec.unsigned_)
				return m_target.get!`ulong`;
			else
				return m_target.get!`long`;
		} else if (spec.short_) {
			if (spec.signed_)
				return m_target.get!`sshort`;
			else if (spec.unsigned_)
				return m_target.get!`ushort`;
			else
				return m_target.get!`ulong`;
		} else {
			if (spec.signed_)
				return m_target.get!`sint`;
			else if (spec.unsigned_)
				return m_target.get!`uint`;
			else if (spec.int_ || implicit_int)
				return m_target.get!`int`;
		}
		return null;

	}

	static struct Declarator
	{
		Type type;
		string identifier;
	}

	Type parsePointer(ref const(Token)[] ref_input, Type type)
	{
		while (ref_input.front.kind == tk!`*`) {
			type = new PointerType(type);
			ref_input.popFront;
			auto qualifiers = parseSpecifiers!(SpecifierSet.typeQualifierList)(ref_input);
			if (qualifiers.const_ || qualifiers.restrict_ || qualifiers.volatile_) {
				type = new QualifiedType(type,
					qualifiers.const_    ? Qualifiers.const_ : Qualifiers.none,
					qualifiers.restrict_ ? Qualifiers.restrict_ : Qualifiers.none,
					qualifiers.volatile_ ? Qualifiers.volatile_ : Qualifiers.none);
			}
			if (qualifiers._Atomic_)
				type = new AtomicType(type);
		}
		return type;
	}

	Type parseDeclaratorBrackets(ref const(Token)[] ref_input, Type type)
	{
	again:
		const tok = ref_input.front;
		// writeln(tok);
		switch (tok.kind) {
		case tk!`(`:
			assert(0);
		case tk!`[`: {
			bool static_;
			bool vla_;
			//writeln("braket");
			ref_input.popFront;
			if (ref_input.front.kind == tk!`static`) {
				ref_input.popFront;
				static_ = true;
			}
			auto qualifiers = parseTypeQualifierList(ref_input);
			if (qualifiers.anyQualifier && !static_ && ref_input.front.kind == tk!`static`) {
				ref_input.popFront;
				static_ = true;
			}
			Expr expr;
			if (!static_ && ref_input.front.kind == tk!`*`) {
				ref_input.popFront;
				vla_ = true;
			} else if (ref_input.front.kind != tk!`]`) {
				expr = parseAssignmentExpr(ref_input);
				if (!expr)
					error(ref_input.front.location,
						"Expected assignment expression");
			}
			if (ref_input.front.kind != tk!`]`)
				error(ref_input.front.location,
					"Expected ']'");
			ref_input.popFront;
			type = new ArrayType(type, expr);
			goto again;
		}
		default:
			return type;
		}
	}

	private const(Token)[] skipToParen(ref const(Token)[] ref_input)
	{
		ref_input.popFront;
		auto paren_decl = ref_input;
		int nest = 1;
		while (nest) {
			switch (ref_input.front.kind) {
			case tk!`(`:
				nest++;
				break;
			case tk!`)`:
				nest--;
				break;
			case tk!`eof`:
			case tk!`;`:
				error(ref_input.front.location, "expected ')'");
				assert(0);
			default:
			}
			ref_input.popFront;
		}
		return paren_decl;
	}

	Declarator parseDeclarator(ref const(Token)[] ref_input, Type type = null)
	{
		const(Token)[] paren_decl;
		const(Token)[] input = ref_input;
		string identifier;
		type = parsePointer(input, type);
		const tok = input.front;
		if (tok.kind == tk!`(`) {
			paren_decl = skipToParen(input);
		} else if (tok.kind == tk!`identifier`) {
			identifier = input.front.spelling;
			input.popFront;
		} else {
			error(tok.location, "expected identifier");
			assert(0);
		}

		type = parseDeclaratorBrackets(input, type);

		if (paren_decl) {
			auto result = parseDeclarator(paren_decl, type);
			if (paren_decl.front.kind != tk!`)`) {
				error(paren_decl.front.location,
					"expected ')' after declarator, not '%s'",
					paren_decl.front.spelling);
				assert(0);
			}
			type = result.type;
			identifier = result.identifier;
		}

		ref_input = input;
		return Declarator(type, identifier);
	}

	Type parseAbstractDeclarator(ref const(Token)[] ref_input, Type type = null)
	{
		const(Token)[] paren_decl;
		const(Token)[] input = ref_input;
		type = parsePointer(input, type);
		const tok = input.front;

		if (tok.kind == tk!`(`)
			paren_decl = skipToParen(input);

		type = parseDeclaratorBrackets(input, type);

		if (paren_decl) {
			type = parseAbstractDeclarator(paren_decl, type);
			if (paren_decl.front.kind != tk!`)`) {
				error(paren_decl.front.location,
					"expected ')' after abstract declarator, not '%s'",
					paren_decl.front.spelling);
				assert(0);
			}
		}
		ref_input = input;
		return type;
	}

	/+
	type-name:
		specifier-qualifier-list abstract-declarator?

	abstract-declarator:
		pointer
		pointer? direct-abstract-declarator

	direct-abstract-declarator:
		( abstract-declarator )
		direct-abstract-declarator? [ type-qualifier-list? assignment-expression? ]
		direct-abstract-declarator? [ static type-qualifier-list? assignment-expression ]
		direct-abstract-declarator? [ type-qualifier-list static assignment-expression ]
		direct-abstract-declarator? [ * ]
		direct-abstract-declarator? ( parameter-type-list opt )
	+/
	Type parseTypeName(ref const(Token)[] ref_input)
	{
		const(Token)[] input = ref_input;
		auto specifiers = parseSpecifiers!(SpecifierSet.specifierQualifierList)(input);

		auto type = parseAbstractDeclarator(input, specifiersToType(specifiers));

		ref_input = input;
		return type;
	}
	+/

}

version(unittest)
{
	Expr expr(string source)
	{
		return parse!"Expr"(source);
	}

	auto parse(string what)(string source, TokenKind[] remainder...)
	{
		import cud.lexer;
		import std.range : chain, only;
		const(Token)[] tokens =
			source
			.split
			.merge
			.tokenize
			.chain(only(Token(tk!`eof`)))
			.filter!(t => t.kind != tk!`space` && t.kind != tk!`newline`)
			.map!(t => t.ppTokenToToken)
			.array;
		auto parser = Parser(tokens);
		auto result = mixin("parser.parse" ~ what ~ "()");
		assertEqual(
			parser.m_input.map!(t => t.kind),
			chain(remainder, only(tk!`eof`)));
		if (!__ctfe) {
			import std.stdio;
			writeln(result.toString);
		}
		return result;
	}
}

unittest // primaryExpr
{
	crtest!("valid integer constants are parsed with types matching the specification",
		() {
			if (auto e = expr("0").as!IntConst) {
				assert(e.value == 0);
				assert(e.type.as!BuiltinType.ty == Ty.int_);
			}
			if (auto e = expr("42l").as!IntConst) {
				assert(e.value == 42);
				assert(e.type.as!BuiltinType.ty == Ty.long_);
			}
			if (auto e = expr("0777u").as!IntConst) {
				assert(e.value == 0x1ff);
				assert(e.type.as!BuiltinType.ty == Ty.uint_);
			}
			if (auto e = expr("1337ull").as!IntConst) {
				assert(e.value == 1337);
				assert(e.type.as!BuiltinType.ty == Ty.ulong_);
			}
		});
	crtest!("parenthesized constant",
		() {
			if (auto e = expr("(42)").as!IntConst) {
				assert(e.value == 42);
				assert(e.type.as!BuiltinType.ty == Ty.int_);
			}
		});
}

unittest
{
	crtest!("can nest postfix operators",
		() {
			auto e = expr("1[42]()(2, 3)->a.b++").as!UnaryExpr;
			assert(e.unaryOp == tk!`post++`);
			auto e1 = e.operand.as!MemberExpr;
			assert(e1.memberName == "b");
			assert(!e1.pointer);
			auto c0 = e1.composite.as!MemberExpr;
			assert(c0.memberName == "a");
			assert(c0.pointer);
			auto c1 = c0.composite.as!CallExpr;
			assert(c1.arguments.length == 2);
			assert(c1.arguments[0].as!IntConst.value == 2);
			assert(c1.arguments[1].as!IntConst.value == 3);
			auto c2 = c1.callee.as!CallExpr;
			assert(c2.arguments.length == 0);
			auto ind = c2.callee.as!IndexExpr;
			assert(ind.indexed.as!IntConst.value == 1);
			assert(ind.index.as!IntConst.value == 42);
		});

	crtest!("binary operators are left-associative",
		() {
			auto e = expr("1 + 2 * 3 % 4 / 5 + 6").as!BinaryExpr;
			assert(e.binaryOp == tk!`+`);
			assert(e.rhs.as!IntConst.value == 6);
			auto add = e.lhs.as!BinaryExpr;
			assert(add.binaryOp == tk!`+`);
			assert(add.lhs.as!IntConst.value == 1);
			auto div = add.rhs.as!BinaryExpr;
			assert(div.binaryOp == tk!`/`);
			assert(div.rhs.as!IntConst.value == 5);
			auto rem = div.lhs.as!BinaryExpr;
			assert(rem.binaryOp == tk!`%`);
			assert(rem.rhs.as!IntConst.value == 4);
			auto mul = rem.lhs.as!BinaryExpr;
			assert(mul.binaryOp == tk!`*`);
			assert(mul.rhs.as!IntConst.value == 3);
			assert(mul.lhs.as!IntConst.value == 2);
		});

	crtest!("prefix operators can be nested",
		() {
			auto c = expr("(int)--*&1").as!CastExpr;
			auto mm = c.operand.as!UnaryExpr;
			assert(mm.unaryOp == tk!`--`);
			auto ind = mm.operand.as!UnaryExpr;
			assert(ind.unaryOp == tk!`*`);
			auto ad = ind.operand.as!UnaryExpr;
			assert(ad.unaryOp == tk!`&`);
			assert(ad.operand.as!IntConst.value == 1);
		});

	crtest!("?: is parsed alone",
		() {
			auto e = expr("1 ? 2 : 3").as!CondExpr;
			assert(e.condition.as!IntConst.value == 1);
			assert(e.trueExpr.as!IntConst.value == 2);
			assert(e.falseExpr.as!IntConst.value == 3);
		});

	crtest!("?: has lower priority than ||",
		() {
			auto e = expr("1 || 2 ? 3 || 4 : 5 || 6").as!CondExpr;
			auto ce = e.condition.as!BinaryExpr;
			assert(ce.binaryOp == tk!`||`);
			assert(ce.lhs.as!IntConst.value == 1);
			assert(ce.rhs.as!IntConst.value == 2);
			auto te = e.trueExpr.as!BinaryExpr;
			assert(te.binaryOp == tk!`||`);
			assert(te.lhs.as!IntConst.value == 3);
			assert(te.rhs.as!IntConst.value == 4);
			auto fe = e.falseExpr.as!BinaryExpr;
			assert(fe.binaryOp == tk!`||`);
			assert(fe.lhs.as!IntConst.value == 5);
			assert(fe.rhs.as!IntConst.value == 6);
		});

	crtest!("?: is right-associative",
		() {
			auto e = expr("1 ? 2 : 3 ? 4 : 5 ? 6 : 7").as!CondExpr;
			assert(e.condition.as!IntConst.value == 1);
			assert(e.trueExpr.as!IntConst.value == 2);
			auto e1 = e.falseExpr.as!CondExpr;
			assert(e1.condition.as!IntConst.value == 3);
			assert(e1.trueExpr.as!IntConst.value == 4);
			auto e2 = e1.falseExpr.as!CondExpr;
			assert(e2.condition.as!IntConst.value == 5);
			assert(e2.trueExpr.as!IntConst.value == 6);
			assert(e2.falseExpr.as!IntConst.value == 7);
		});
}

unittest
{
	crtest!("enum without enumerator list",
		() {
			auto e = parse!"Enum"(`enum foo;`, tk!`;`).as!EnumDecl;
			assert(e.tag == "foo");
			assert(e.list.length == 0);
		});

	crtest!("enum with one constant with default value",
		() {
			foreach (src; [`enum foo { bar };`, `enum foo { bar, };`]) {
				auto e = parse!"Enum"(src, tk!`;`)
					.as!EnumDecl;
				assert(e.tag == "foo");
				assert(e.list.length == 1);
				assert(e.list[0].name == "bar");
				assert(e.list[0].value == 0);
			}
		});

	crtest!("enum with multiple constants",
		() {
			auto e =
				parse!"Enum"(
					`enum foo { bar, baz = 13, quux = 42, };`,
					tk!`;`)
				.as!EnumDecl;
			assert(e.tag == "foo");
			assert(e.list.length == 3);
			assert(e.list[0].name == "bar");
			assert(e.list[0].value == 0);
			assert(e.list[1].name == "baz");
			assert(e.list[1].value == 13);
			assert(e.list[2].name == "quux");
			assert(e.list[2].value == 42);
		});

	crtest!("malformed enum spec",
		() {
			foreach (src; [
					"enum",
					"enum {}",
					"enum { a",
					"enum {,}",
					"enum a {}",
					"enum a {b",
					"enum a {b,",
					"enum a {,}",
					"enum a {b=}",
					"enum a {b=,}",
				])
			{
				assertThrown(parse!"DeclSpecifiers"(src), src);
			}
		});
}

version(none)
unittest
{
	crtest!("parse type qualifiers",
		() {
			auto tq = parse!"Specifiers!(Parser.SpecifierSet.typeQualifierList)"(
				"const volatile restrict int", tk!`int`);
			assert(tq.const_);
			assert(tq.volatile_);
			assert(!tq._Atomic_);
			assert(tq.restrict_);

			tq = parse!"Specifiers!(Parser.SpecifierSet.typeQualifierList)"(
				"_Atomic restrict typedef", tk!`typedef`);
			assert(!tq.const_);
			assert(!tq.volatile_);
			assert(tq._Atomic_);
			assert(tq.restrict_);
		});

	crtest!("parse declaration-specifiers",
		() {
			auto sq = parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(
				"extern _Thread_local unsigned _Alignas(16) const _Alignas(int) *",
				tk!`*`);
			assert(sq.extern_);
			assert(sq._Thread_local_);
			assert(sq._Alignas_);
			assert(sq.const_);
			assert(sq.alignment.length == 2);
			assert(sq.alignment[0].as!IntConst.value == 16);
			auto tt = sq.alignment[1].as!TypeTraitExpr;
			assert(tt.trait == TypeTrait.alignof_);
			assert(tt.theType.as!BuiltinType.kind == BuiltinType.kind.int_);
			assert(!sq.restrict_);
			assert(!sq.volatile_);
			assert(!sq._Atomic_);
		});

	crtest!("parse _Atomic(type)",
		() {
			auto sq = parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(
				"const _Atomic(int)");
			assert(!sq.extern_);
			assert(!sq._Thread_local_);
			assert(!sq._Alignas_);
			assert(sq.const_);
			assert(!sq.alignment);
			assert(!sq.restrict_);
			assert(!sq.volatile_);
			assert(!sq._Atomic_);
		});

	crtest!("parse _Atomic _Atomic(type)",
		() {
			auto ds = parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(
				"extern const _Atomic _Atomic(int)");
			assert(ds.extern_);
			assert(ds.const_);
			assert(!ds.restrict_);
			assert(!ds.volatile_);
			assert(ds._Atomic_);
		});

	crtest!("parse specifier-qualifier-list",
		() {
			auto sq = parse!"Specifiers!(Parser.SpecifierSet.specifierQualifierList)"(
				"const int long unsigned long volatile restrict _Atomic");
			assert(sq.const_);
			assert(sq.int_);
			assert(sq.long_);
			assert(sq.long_long_);
			assert(sq.unsigned_);
			assert(sq.volatile_);
			assert(sq.restrict_);
			assert(sq._Atomic_);
			assert(!sq.other);
			assert(!sq.short_);
			assert(!sq.signed_);
		});

	crtest!("void combines with typedef",
		() {
			auto ds = parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(
				"void typedef *", tk!`*`);
			assert(ds.void_);
			assert(ds.typedef_);
		});

	crtest!("error if conflicting type specifiers are given",
		() {
			foreach (src; [
					"void int",
					"int void",
					"long _Atomic(int)",
					"_Atomic(int) long",
					"_Atomic(int) unsigned",
					"_Atomic(int) int",
					"float double",
					"double float",
					"long double int",
					"long int double",
					"double int long",
					"long float",
					"_Bool long",
					"_Bool int",
					"int _Bool",
					"short char",
					"short double",
					"long short",
					"short long",
					"char int",
					"unsigned signed",
					"signed unsigned",
					"_Complex int",
					"int _Complex",
					"enum { a } short",
					"signed enum { a }",
					"static typedef",
					"static static",
					"static extern",
					"extern typedef",
					"extern static",
					"extern extern",
					"typedef typedef",
					"typedef static",
					"typedef extern",
					"auto typedef",
					"auto static",
					"auto extern",
					"auto _Thread_local",
					"auto auto",
					"auto register",
					"register typedef",
					"register static",
					"register extern",
					"register _Thread_local",
					"register auto",
					"register register",
				])
			{
				assertThrown(parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(src), src);
			}
		});

	crtest!("malformed _Atomic()",
		() {
			foreach (src; [
					"_Atomic(",
					"_Atomic(",
					"_Atomic( foo)",
					"_Atomic(,)",
					"_Atomic(int",
					"_Atomic(,",
					"_Atomic()",
				])
			{
				assertThrown(parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(src), src);
			}
		});

	crtest!("malformed _Alignas",
		() {
			foreach (src; [
					"_Alignas int",
					"_Alignas *",
					"_Alignas foo",
					"_Alignas (",
					"_Alignas()",
					"_Alignas(foo)",
					"_Alignas(int",
					"_Alignas(int foo",
					"_Alignas(2",
					"_Alignas(2 foo",
				])
			{
				assertThrown(parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(src), src);
			}
		});
}

version(none)
unittest
{
	crtest!("parse declarators with pointers and qualifiers",
		() {
			auto decl = parse!"Declarator"(`foo`);
			assert(decl.identifier == "foo");

			decl = parse!"Declarator"(`*bar`);
			assert(decl.identifier == "bar");
			assert(decl.type.toString == "ptr(0)");

			decl = parse!"Declarator"(`*(bar)`);
			assert(decl.identifier == "bar");
			assert(decl.type.toString == "ptr(0)");

			decl = parse!"Declarator"(`*const baz`);
			assert(decl.identifier == "baz");
			assert(decl.type.toString == "const(ptr(0))");

			decl = parse!"Declarator"(`* const _Atomic * restrict ** volatile * const restrict volatile quux`);
			assert(decl.identifier == "quux");
			assert(decl.type.toString ==
				"const restrict volatile(ptr(volatile(ptr(ptr(restrict(ptr(_Atomic(const(ptr(0))))))))))");
		});

	crtest!("malformed declarator",
		() {
			foreach (src; [
					"* const",
					"*(",
					"((",
					"((dar)",
					"*(int)",
					"()",
					"*()",
					"*())",
					"(foo bar)",
					"(*foo bar)",
				])
			{
				assertThrown(parse!"Declarator"(src), src);
			}
		});
}

unittest
{
	crtest!("parse type name",
		() {
			foreach (src, type_str; [
					"int"           : "int",
					"int *"         : "ptr int",
					"int *[3]"      : "array[3] ptr int",
					"int (*)[42]"   : "ptr array[42] int",
					"int (*)[*]"    : "ptr array[*] int",
				/+	"int *()"       : "func(?:ptr(int))",
					"int (*)(void)" : "func(:int)",
					"int (*const [])(unsigned int, ...)"
					                : "array[?](const(ptr(func(uint,...:int))))"+/
				])
			{
				import std.conv : text;
				auto ts = parse!"TypeName"(src).toString;
				assert(ts == type_str, text("input: ", src, "  expected: ", type_str,
						"  actual: ", ts));
			}
		});
}
