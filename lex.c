const char *break_keyword;
const char *case_keyword;
const char *const_keyword;
const char *continue_keyword;
const char *default_keyword;
const char *do_keyword;
const char *else_keyword;
const char *enum_keyword;
const char *for_keyword;
const char *function_keyword;
const char *if_keyword;
const char *return_keyword;
const char *sizeof_keyword;
const char *struct_keyword;
const char *switch_keyword;
const char *typedef_keyword;
const char *union_keyword;
const char *var_keyword;
const char *while_keyword;

const char *first_keyword;
const char *last_keyword;
const char **keywords;

#define KEYWORD(name) name##_keyword = str_intern(#name); buf_push(keywords, name##_keyword)

void init_keywords() {
	static bool inited;
	if (inited) {
		return;
	}
	char *arena_end = str_arena.end;
	KEYWORD(break);
	KEYWORD(case);
	KEYWORD(const);
	KEYWORD(continue);
	KEYWORD(default);
	KEYWORD(do);
	KEYWORD(else);
	KEYWORD(enum);
	KEYWORD(for);
	KEYWORD(function);
	KEYWORD(if);
	KEYWORD(return);
	KEYWORD(sizeof);
	KEYWORD(struct);
	KEYWORD(switch);
	KEYWORD(typedef);
	KEYWORD(union);
	KEYWORD(var);
	KEYWORD(while);
	assert(str_arena.end == arena_end);
	first_keyword = break_keyword;
	last_keyword = while_keyword;
	inited = true;
}

#undef KEYWORD

bool is_keyword_str(const char *str) {
	return first_keyword <= str && str <= last_keyword;
}

typedef enum TokenType_t {
	TOKEN_EOF = 0,
	// Reserve first 128 values for single character tokens
	TOKEN_LAST_CHAR = 127,
	TOKEN_KEYWORD,
	TOKEN_INT,
	TOKEN_FLOAT,
	TOKEN_NAME,
	TOKEN_STR,
	TOKEN_COLON_ASSIGN,
	TOKEN_ADD_ASSIGN,
	TOKEN_FIRST_ASSIGN = TOKEN_ADD_ASSIGN,
	TOKEN_SUB_ASSIGN,
	TOKEN_MUL_ASSIGN,
	TOKEN_DIV_ASSIGN,
	TOKEN_MOD_ASSIGN,
	TOKEN_OR_ASSIGN,
	TOKEN_AND_ASSIGN,
	TOKEN_XOR_ASSIGN,
	TOKEN_LSHIFT_ASSIGN,
	TOKEN_RSHIFT_ASSIGN,
	TOKEN_LAST_ASSIGN = TOKEN_RSHIFT_ASSIGN,
	TOKEN_ARROW,
	TOKEN_INC,
	TOKEN_DEC,
	TOKEN_LSHIFT,
	TOKEN_RSHIFT,
	TOKEN_CMP_EQ,
	TOKEN_CMP_NE,
	TOKEN_CMP_LE,
	TOKEN_CMP_GE,
	TOKEN_LOG_AND,
	TOKEN_LOG_OR,

} TokenType;

typedef enum TokenMod_t {
	TOKMOD_NONE,
	TOKMOD_HEX,
	TOKMOD_BIN,
	TOKMOD_OCT,
	TOKMOD_CHAR,
} TokenMod;

const char *token_type_names[] = {
	[TOKEN_EOF] = "EOF",
	[TOKEN_INT] = "int",
	[TOKEN_FLOAT] = "float",
	[TOKEN_STR] = "string",
	[TOKEN_NAME] = "name",
	[TOKEN_LSHIFT] = "<<",
	[TOKEN_RSHIFT] = ">>",
	[TOKEN_CMP_EQ] = "==",
	[TOKEN_CMP_NE] = "!=",
	[TOKEN_CMP_LE] = "<=",
	[TOKEN_CMP_GE] = ">=",
	[TOKEN_LOG_AND] = "&&",
	[TOKEN_LOG_OR] = "||",
	[TOKEN_INC] = "++",
	[TOKEN_DEC] = "--",
	[TOKEN_COLON_ASSIGN] = ":=",
	[TOKEN_ADD_ASSIGN] = "+=",
	[TOKEN_SUB_ASSIGN] = "-=",
	[TOKEN_OR_ASSIGN] = "|=",
	[TOKEN_AND_ASSIGN] = "&=",
	[TOKEN_XOR_ASSIGN] = "^=",
	[TOKEN_MUL_ASSIGN] = "+=",
	[TOKEN_DIV_ASSIGN] = "/=",
	[TOKEN_MOD_ASSIGN] = "%=",
	[TOKEN_LSHIFT_ASSIGN] = "<<=",
	[TOKEN_RSHIFT_ASSIGN] = ">>=",
};

const char *token_type_name(TokenType type) {
	if (type < sizeof(token_type_names) / sizeof(*token_type_names)) {
		return token_type_names[type];
	}
	return NULL;
}

size_t str_token_copy(char *dest, size_t dest_size, TokenType type) {
	size_t n = 0;
	const char *name = token_type_name(type);
	if (name) {
		n = snprintf(dest, dest_size, "%s", name);
	}
	else if (type < 128 && isprint(type)) {
		n = snprintf(dest, dest_size, "%c", type);
	}
	else {
		n = snprintf(dest, dest_size, "<ASCII %d>", type);
	}
	return n;
}

const char *temp_str_token(TokenType type) {
	static char buf[256];
	size_t n = str_token_copy(buf, sizeof(buf), type);
	assert(n + 1 <= sizeof(buf));
	return buf;
}

typedef struct Token_t {
	TokenType type;
	TokenMod mod;
	const char *start;
	const char *end;
	union {
		uint64_t int_val;
		double float_val;
		const char *str_val;
		const char *name;
	};
} Token;

Token token;
const char *stream;

uint8_t char_to_digit[256] = {
	['0'] = 0,
	['1'] = 1,
	['2'] = 2,
	['3'] = 3,
	['4'] = 4,
	['5'] = 5,
	['6'] = 6,
	['7'] = 7,
	['8'] = 8,
	['9'] = 9,
	['a'] = 10,['A'] = 10,
	['b'] = 11,['B'] = 11,
	['c'] = 12,['C'] = 12,
	['d'] = 13,['D'] = 13,
	['e'] = 14,['E'] = 14,
	['f'] = 15,['F'] = 15,
};

char escape_to_char[256] = {
	['n'] = '\n',
	['r'] = '\r',
	['t'] = '\t',
	['v'] = '\v',
	['b'] = '\b',
	['a'] = '\a',
	['f'] = '\f',
	['\\'] = '\\',
	['\''] = '\'',
	['"'] = '\"',
	['0'] = 0,
};

void scan_int() {
	uint64_t base = 10;
	if (*stream == '0') {
		stream++;
		if (tolower(*stream) == 'x') {
			stream++;
			base = 16;
			token.mod = TOKMOD_HEX;
		}
		else if (tolower(*stream) == 'b') {
			stream++;
			base = 2;
			token.mod = TOKMOD_BIN;
		}
		else if (isdigit(*stream)) {
			base = 8;
			token.mod = TOKMOD_OCT;
		}
	}
	uint64_t val = 0;
	for (;;) {
		uint64_t digit = char_to_digit[*stream];
		if (digit == 0 && *stream != '0') {
			break;
		}
		if (digit >= base) {
			syntax_error("Digit '%c' out of range for base %" PRIu64, *stream, base);
			digit = 0;
		}
		if (val > (UINT64_MAX - digit) / base) {
			syntax_error("Integer literal overflow");
			while (isdigit(*stream)) stream++;
			val = 0;
		}
		val = base * val + digit;
		stream++;
	}
	token.type = TOKEN_INT;
	token.int_val = val;
}

void scan_float() {
	const char *start = stream;
	while (isdigit(*stream)) {
		stream++;
	}
	if (*stream == '.') {
		stream++;
	}
	while (isdigit(*stream)) {
		stream++;
	}
	if (tolower(*stream) == 'e') {
		stream++;
		if (*stream == '+' || *stream == '-') {
			stream++;
		}
		if (!isdigit(*stream)) {
			syntax_error("Expected digit after float literal exponent, got '%c'", *stream);
		}
		while (isdigit(*stream)) {
			stream++;
		}
	}
	const char *end = stream;
	double val = strtod(start, NULL);
	if (val == HUGE_VAL || val == -HUGE_VAL) {
		syntax_error("Float literal overflow");
	}
	token.type = TOKEN_FLOAT;
	token.float_val = val;
}

void scan_char() {
	assert(*stream == '\'');
	stream++;

	char val = 0;
	if (*stream == '\'') {
		syntax_error("Character literal cannot be empty");
		stream++;
	}
	else if (*stream == '\n') {
		syntax_error("Character literal cannot contain a newline");
		stream++;
	}
	else if (*stream == '\\') {
		stream++;
		val = escape_to_char[*stream];
		if (val == 0 && *stream != '0') {
			syntax_error("Invalid character escape '\\%c'", *stream);
		}
	}
	else {
		val = *stream;
	}
	stream++;
	if (*stream != '\'') {
		syntax_error("Missing closing quote for character literal, got '%c'", *stream);
	}
	else {
		stream++;
	}

	token.type = TOKEN_INT;
	token.int_val = val;
	token.mod = TOKMOD_CHAR;
}

void scan_str() {
	assert(*stream == '"');
	stream++;
	char *str = NULL;
	while (*stream && *stream != '"') {
		char val = *stream;
		if (val == '\n') {
			syntax_error("String literal cannot contain a newline");
		}
		else if (val == '\\') {
			stream++;
			val = escape_to_char[*stream];
			if (val == 0 && *stream != '0') {
				syntax_error("Invalid string literal escape '\\%c'", *stream);
			}
		}
		buf_push(str, val);
		stream++;
	}
	if (*stream) {
		assert(*stream == '"');
		stream++;
	}
	else {
		syntax_error("Reached EOF before the closing string quote");
	}
	buf_push(str, 0);
	token.type = TOKEN_STR;
	token.str_val = str;
}

#define CASE1(c, c1, t1) \
	case c: \
		token.type = *stream++; \
		if (*stream == c1) { \
			token.type = t1; \
			stream++; \
		} \
		break;
#define CASE2(c, c1, t1, c2, t2) \
	case c: \
		token.type = *stream++; \
		if (*stream == c1) { \
			token.type = t1; \
			stream++; \
		} \
		else if (*stream == c2) { \
			token.type = t2; \
			stream++; \
		} \
		break;
#define CASE3(c, c1, t1, c2, t2, c3, t3) \
	case c: \
		token.type = *stream++; \
		if (*stream == c1) { \
			token.type = t1; \
			stream++; \
		} \
		else if (*stream == c2) { \
			token.type = t2; \
			stream++; \
		} \
		else if (*stream == c3) { \
			token.type = t3; \
			stream++; \
		} \
		break;

void next_token() {
top:
	token.start = stream;
	token.mod = 0;
	switch (*stream) {
	case ' ': case '\n': case '\r': case '\t': case '\v':
		while (isspace(*stream)) {
			stream++;
		}
		goto top;
		break;

	case '\'':
		scan_char();
		break;

	case '"':
		scan_str();
		break;

	case '.':
		scan_float();
		break;

	case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': {
		while (isdigit(*stream)) {
			stream++;
		}
		char suffix = *stream;
		stream = token.start;
		if (suffix == '.' || tolower(suffix) == 'e') {
			scan_float();
		}
		else {
			scan_int();
		}
		break;
	}
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j':
	case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
	case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
	case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T':
	case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
	case '_':
		while (isalnum(*stream) || *stream == '_') {
			stream++;
		}
		token.name = str_intern_range(token.start, stream);
		token.type = is_keyword_str(token.name) ? TOKEN_KEYWORD : TOKEN_NAME;
		break;

	case '<':
		token.type = *stream++;
		if (*stream == '=') {
			token.type = TOKEN_CMP_LE;
			stream++;
		}
		else if (*stream == '<') {
			token.type = TOKEN_LSHIFT;
			stream++;
			if (*stream == '=') {
				token.type = TOKEN_LSHIFT_ASSIGN;
				stream++;
			}
		}
		break;
	case '>':
		token.type = *stream++;
		if (*stream == '=') {
			token.type = TOKEN_CMP_GE;
			stream++;
		}
		else if (*stream == '>') {
			token.type = TOKEN_RSHIFT;
			stream++;
			if (*stream == '=') {
				token.type = TOKEN_RSHIFT_ASSIGN;
				stream++;
			}
		}
		break;
	CASE2('+', '+', TOKEN_INC, '=', TOKEN_ADD_ASSIGN)
	CASE3('-', '-', TOKEN_DEC, '=', TOKEN_SUB_ASSIGN, '>', TOKEN_ARROW)
	CASE1(':', '=', TOKEN_COLON_ASSIGN)
	CASE2('|', '|', TOKEN_LOG_OR, '=', TOKEN_OR_ASSIGN)
	CASE2('&', '&', TOKEN_LOG_AND, '=', TOKEN_AND_ASSIGN)
	CASE1('^', '=', TOKEN_XOR_ASSIGN)
	CASE1('*', '=', TOKEN_MUL_ASSIGN)
	CASE1('/', '=', TOKEN_DIV_ASSIGN)
	CASE1('%', '=', TOKEN_MOD_ASSIGN)
	CASE1('=', '=', TOKEN_CMP_EQ)
	CASE1('!', '=', TOKEN_CMP_NE)

	default:
		token.type = *stream++;
		break;
	}
	token.end = stream;
}
#undef CASE1
#undef CASE2
#undef CASE3

void init_code(const char *str) {
	stream = str;
	next_token();
}

void print_token(Token token) {
	switch (token.type) {
	case TOKEN_INT:
		printf("Found INT token: %" PRIu64 "\n", token.int_val);
		break;
	case TOKEN_FLOAT:
		printf("Found FLOAT token: %f\n", token.float_val);
		break;
	case TOKEN_NAME:
		printf("Found NAME token: %.*s\n", (int)(token.end - token.start), token.start);
		break;
	default:
		printf("Found token: %c\n", token.type);
		break;
	}
}

bool is_token(TokenType type) {
	return token.type == type;
}

bool is_token_eof() {
	return token.type == TOKEN_EOF;
}

bool is_token_name(const char *name) {
	return token.type == TOKEN_NAME && token.name == name;
}

bool is_keyword(const char *name) {
	return is_token(TOKEN_KEYWORD) && token.name == name;
}

bool match_keyword(const char *name) {
	if (is_keyword(name)) {
		next_token();
		return true;
	}
	return false;
}

bool match_token(TokenType type) {
	if (is_token(type)) {
		next_token();
		return true;
	}
	return false;
}

bool expect_token(TokenType type) {
	if (is_token(type)) {
		next_token();
		return true;
	}
	char buf[256];
	str_token_copy(buf, sizeof(buf), type);
	fatal("Expected token %s, got %s", buf, temp_str_token(token.type));
	return false;
}

void keyword_test() {
	init_keywords();
	assert(is_keyword_str(first_keyword));
	assert(is_keyword_str(last_keyword));
	for (const char **it = keywords; it != buf_end(keywords); it++) {
		assert(is_keyword_str(*it));
	}
	assert(!is_keyword_str(str_intern("foo")));
}

#define assert_token(x) assert(match_token(x))
#define assert_token_name(x) assert(token.name == str_intern(x) && match_token(TOKEN_NAME))
#define assert_token_int(x) assert(token.int_val == (x) && match_token(TOKEN_INT))
#define assert_token_float(x) assert(token.float_val == (x) && match_token(TOKEN_FLOAT))
#define assert_token_str(x) assert(strcmp(token.str_val, (x)) == 0 && match_token(TOKEN_STR))
#define assert_token_eof() assert(is_token(0))

void lex_test() {
	keyword_test();

	// Integer literal tests
	init_code("18446744073709551615 0xffffffffffffffff 031 0b11001011 0");
	assert_token_int(18446744073709551615ull);
	assert(token.mod == TOKMOD_HEX);
	assert_token_int(0xffffffffffffffffull);
	assert(token.mod == TOKMOD_OCT);
	assert_token_int(031);
	assert(token.mod == TOKMOD_BIN);
	assert_token_int(0xCB);
	assert_token_int(0);
	assert_token_eof();

	// Float literal tests
	init_code("3.14 2.718281828459045 2.998e8 .1234 9876. 3e-10");
	assert_token_float(3.14);
	assert_token_float(2.718281828459045);
	assert_token_float(2.998e8);
	assert_token_float(.1234);
	assert_token_float(9876.);
	assert_token_float(3e-10);
	assert_token_eof();

	// Character literal tests
	init_code("'a' '0' '\\0' '\\\\' '\"'");
	assert_token_int('a');
	assert_token_int('0');
	assert_token_int(0);
	assert_token_int('\\');
	assert_token_int('"');
	assert_token_eof();

	// String literal tests
	init_code("\"Hello, World!\" \"name: string = \\\"John Smith\\\";\\nage := 23;\"");
	assert_token_str("Hello, World!");
	assert_token_str("name: string = \"John Smith\";\nage := 23;");
	assert_token_eof();

	// Operator tests
	init_code("()[]{}+-!~&*|^/% < <= << <<= >>= >> >= > = == != &&||?: :=+=-=*=/=%=&=|=^=++-- ->");
	assert_token('(');
	assert_token(')');
	assert_token('[');
	assert_token(']');
	assert_token('{');
	assert_token('}');
	assert_token('+');
	assert_token('-');
	assert_token('!');
	assert_token('~');
	assert_token('&');
	assert_token('*');
	assert_token('|');
	assert_token('^');
	assert_token('/');
	assert_token('%');
	assert_token('<');
	assert_token(TOKEN_CMP_LE);
	assert_token(TOKEN_LSHIFT);
	assert_token(TOKEN_LSHIFT_ASSIGN);
	assert_token(TOKEN_RSHIFT_ASSIGN);
	assert_token(TOKEN_RSHIFT);
	assert_token(TOKEN_CMP_GE);
	assert_token('>');
	assert_token('=');
	assert_token(TOKEN_CMP_EQ);
	assert_token(TOKEN_CMP_NE);
	assert_token(TOKEN_LOG_AND);
	assert_token(TOKEN_LOG_OR);
	assert_token('?');
	assert_token(':');
	assert_token(TOKEN_COLON_ASSIGN);
	assert_token(TOKEN_ADD_ASSIGN);
	assert_token(TOKEN_SUB_ASSIGN);
	assert_token(TOKEN_MUL_ASSIGN);
	assert_token(TOKEN_DIV_ASSIGN);
	assert_token(TOKEN_MOD_ASSIGN);
	assert_token(TOKEN_AND_ASSIGN);
	assert_token(TOKEN_OR_ASSIGN);
	assert_token(TOKEN_XOR_ASSIGN);
	assert_token(TOKEN_INC);
	assert_token(TOKEN_DEC);
	assert_token(TOKEN_ARROW);
	assert_token_eof();

	// General tests
	init_code("XY+(XY)_HELLO1,234+foo!994_");
	assert_token_name("XY");
	assert_token('+');
	assert_token('(');
	assert_token_name("XY");
	assert_token(')');
	assert_token_name("_HELLO1");
	assert_token(',');
	assert_token_int(234);
	assert_token('+');
	assert_token_name("foo");
	assert_token('!');
	assert_token_int(994);
	assert_token_name("_");
	assert_token_eof();
}

#undef assert_token
#undef assert_token_name
#undef assert_token_int
#undef assert_token_float
#undef assert_token_str
#undef assert_token_eof