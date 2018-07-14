Typespec *parse_type();
Decl *parse_decl();
Stmt *parse_stmt();
Expr *parse_expr();

const char *parse_name() {
	const char *name = token.name;
	expect_token(TOKEN_NAME);
	return name;
}

/* ------------------------------------------- */
/* --------- Type Specifier Parsing ---------- */
/* ------------------------------------------- */

Typespec *parse_func_type() {
	Typespec **args = NULL;
	expect_token('(');
	if (!is_token(')')) {
		buf_push(args, parse_type());
		while (match_token(',')) {
			buf_push(args, parse_type());
		}
	}
	expect_token(')');
	Typespec *ret_type = NULL;
	if (match_token(TOKEN_ARROW)) {
		ret_type = parse_type();
	}
	return typespec_func(ast_dup(args, buf_sizeof(args)), buf_len(args), ret_type);
}

Typespec *parse_base_type() {
	if (is_token(TOKEN_NAME)) {
		const char *name = parse_name();
		return typespec_name(name);
	}
	else if (match_keyword(function_keyword)) {
		return parse_func_type();
	}
	else if (match_token('(')) {
		return parse_type();
	}
	else {
		fatal_syntax_error("Unexpected token %s in type", temp_str_token(token.type));
		return NULL;
	}
}

Typespec *parse_type() {
	Typespec *type = parse_base_type();
	while (is_token('[') || is_token('*')) {
		if (match_token('[')) {
			Expr *expr = NULL;
			if (!is_token(']')) {
				expr = parse_expr();
			}
			expect_token(']');
			type = typespec_array(type, expr);
		}
		else {
			next_token();
			type = typespec_ptr(type);
		}
	}
	return type;
}

/* --------------------------------------- */
/* --------- Expression Parsing ---------- */
/* --------------------------------------- */

Expr *parse_expr_compound(Typespec *type) {
	expect_token('{');
	Expr **args = NULL;
	if (!is_token('}')) {
		buf_push(args, parse_expr());
		while (match_token(',')) {
			buf_push(args, parse_expr());
		}
	}
	expect_token('}');
	return expr_compound(type, ast_dup(args, buf_sizeof(args)), buf_len(args));
}

Expr *parse_expr_operand() {
	if (is_token(TOKEN_INT)) {
		uint64_t val = token.int_val;
		next_token();
		return expr_int(val);
	}
	else if (is_token(TOKEN_FLOAT)) {
		double val = token.float_val;
		next_token();
		return expr_float(val);
	}
	else if (is_token(TOKEN_STR)) {
		const char* val = token.str_val;
		next_token();
		return expr_str(val);
	}
	else if (is_token(TOKEN_NAME)) {
		const char* name = token.name;
		next_token();
		if (is_token('{')) {
			return parse_expr_compound(typespec_name(name));
		}
		else {
			return expr_name(name);
		}
	}
	else if (match_keyword(sizeof_keyword)) {
		expect_token('(');
		if (match_token(':')) {
			Typespec *type = parse_type();
			expect_token(')');
			return expr_sizeof_type(type);
		}
		else {
			Expr *expr = parse_expr();
			expect_token(')');
			return expr_sizeof_expr(expr);
		}
	}
	else if (is_token('{')) {
		return parse_expr_compound(NULL);
	}
	else if (match_token('(')) {
		if (is_token(':')) {
			Typespec *type = parse_type();
			expect_token(')');
			return parse_expr_compound(type);
		}
		else {
			Expr *expr = parse_expr();
			expect_token(')');
			return expr;
		}
	}
	else {
		fatal_syntax_error("Unexpected token %s in expression", temp_str_token(token.type));
		return NULL;
	}
}

Expr *parse_expr_base() {
	Expr *expr = parse_expr_operand();
	while (is_token('(') || is_token('[') || is_token('.')) {
		if (match_token('(')) {
			Expr **args = NULL;
			if (!is_token(')')) {
				buf_push(args, parse_expr());
				while (match_token(',')) {
					buf_push(args, parse_expr());
				}
			}
			expect_token(')');
			expr = expr_call(expr, ast_dup(args, buf_sizeof(args)), buf_len(args));
		}
		else if (match_token('[')) {
			Expr *index = parse_expr();
			expect_token(']');
			expr = expr_index(expr, index);
		}
		else if (match_token('.')) {
			const char *field = token.name;
			expect_token(TOKEN_NAME);
			expr = expr_field(expr, field);
		}
	}
	return expr;
}

bool is_unary_op() {
	return is_token('+') || is_token('-') || is_token('*') || is_token('&') || is_token('!') || is_token('~');
}

Expr *parse_expr_unary() {
	if (is_unary_op()) {
		TokenType op = token.type;
		next_token();
		return expr_unary(op, parse_expr_unary());
	}
	return parse_expr_base();
}

bool is_mul_op() {
	return is_token('*') || is_token('/') || is_token('%') || is_token('&') || is_token(TOKEN_LSHIFT) || is_token(TOKEN_RSHIFT);
}

Expr *parse_expr_mul() {
	Expr *expr = parse_expr_unary();
	while (is_mul_op()) {
		TokenType op = token.type;
		next_token();
		expr = expr_binary(op, expr, parse_expr_unary());
	}
	return expr;
}

bool is_add_op() {
	return is_token('+') || is_token('-') || is_token('|') || is_token('^');
}

Expr *parse_expr_add() {
	Expr *expr = parse_expr_mul();
	while (is_add_op()) {
		TokenType op = token.type;
		next_token();
		expr = expr_binary(op, expr, parse_expr_mul());
	}
	return expr;
}

bool is_cmp_op() {
	return is_token('<') || is_token('>') || is_token(TOKEN_CMP_EQ) || is_token(TOKEN_CMP_NE)
		|| is_token(TOKEN_CMP_GE) || is_token(TOKEN_CMP_LE);
}

Expr *parse_expr_cmp() {
	Expr *expr = parse_expr_add();
	while (is_cmp_op()) {
		TokenType op = token.type;
		next_token();
		expr = expr_binary(op, expr, parse_expr_add());
	}
	return expr;
}

Expr *parse_expr_and() {
	Expr *expr = parse_expr_cmp();
	while (match_token(TOKEN_LOG_AND)) {
		expr = expr_binary(TOKEN_LOG_AND, expr, parse_expr_cmp());
	}
	return expr;
}

Expr *parse_expr_or() {
	Expr *expr = parse_expr_and();
	while (match_token(TOKEN_LOG_OR)) {
		expr = expr_binary(TOKEN_LOG_OR, expr, parse_expr_and());
	}
	return expr;
}

Expr *parse_expr_ternary() {
	Expr *expr = parse_expr_or();
	if (match_token('?')) {
		Expr *then_expr = parse_expr_ternary();
		expect_token(':');
		Expr *else_expr = parse_expr_ternary();
		expr = expr_ternary(expr, then_expr, else_expr);
	}
	return expr;
}

Expr *parse_expr() {
	return parse_expr_ternary();
}

Expr *parse_paren_expr() {
	expect_token('(');
	Expr *expr = parse_expr();
	expect_token(')');
	return expr;
}

/* -------------------------------------- */
/* --------- Statement Parsing ---------- */
/* -------------------------------------- */

bool is_assign_op() {
	return TOKEN_FIRST_ASSIGN <= token.type && token.type <= TOKEN_LAST_ASSIGN;
}

Stmt *parse_base_stmt() {
	Expr *expr = parse_expr();
	Stmt *stmt;
	if (match_token(TOKEN_COLON_ASSIGN)) {
		if (expr->type != EXPR_NAME) {
			fatal_syntax_error("':=' must by preceded by a name/identifier");
		}
		stmt = stmt_init(expr->name, parse_expr());
	}
	else if (is_assign_op()) {
		TokenType op = token.type;
		next_token();
		stmt = stmt_assign(op, expr, parse_expr());
	}
	else if (is_token(TOKEN_INC) || is_token(TOKEN_DEC)) {
		TokenType op = token.type;
		next_token();
		stmt = stmt_assign(op, expr, NULL);
	}
	else {
		stmt = stmt_expr(expr);
	}
	return stmt;
}

StmtBlock parse_stmt_block() {
	expect_token('{');
	Stmt **stmts = NULL;
	while (!is_token('}')) {
		buf_push(stmts, parse_stmt());
	}
	expect_token('}');
	return (StmtBlock) { stmts, buf_len(stmts) };
}

Stmt *parse_stmt_if() {
	Expr* cond = parse_paren_expr();
	StmtBlock then_block = parse_stmt_block();
	ElseIf *elseifs = NULL;
	StmtBlock else_block = { 0 };
	while (match_keyword(else_keyword)) {
		if (!match_keyword(if_keyword)) {
			else_block = parse_stmt_block();
			break;
		}
		Expr *elseif_cond = parse_paren_expr();
		StmtBlock elseif_block = parse_stmt_block();
		buf_push(elseifs, (ElseIf) { elseif_cond, elseif_block });
	}
	return stmt_if(cond, then_block, ast_dup(elseifs, buf_sizeof(elseifs)), buf_len(elseifs), else_block);
}

Stmt *parse_stmt_while() {
	Expr *cond = parse_paren_expr();
	StmtBlock block = parse_stmt_block();
	return stmt_while(cond, block);
}

Stmt *parse_stmt_do_while() {
	StmtBlock block = parse_stmt_block();
	if (!match_keyword(while_keyword)) {
		fatal_syntax_error("Expected 'while' after 'do' block");
		return NULL;
	}
	Expr *cond = parse_paren_expr();
	expect_token(';');
	return stmt_do_while(block, cond);
}

Stmt *parse_stmt_for() {
	expect_token('(');
	Stmt *init = parse_stmt();
	expect_token(';');
	Expr *cond = parse_expr();
	expect_token(';');
	Stmt *update = parse_stmt();
	expect_token(')');
	StmtBlock block = parse_stmt_block();
	return stmt_for(init, cond, update, block);
}

SwitchCase parse_switch_case() {
	Expr **exprs = NULL;
	bool is_default = false;
	while (is_keyword(case_keyword) || is_keyword(default_keyword)) {
		if (match_keyword(case_keyword)) {
			buf_push(exprs, parse_expr());
		}
		else if (match_keyword(default_keyword)) {
			if (is_default) {
				syntax_error("Duplicate default labels in same switch clause");
			}
			is_default = true;
		}
		else {
			fatal_syntax_error("Expected 'case' or 'default' in switch clause, got %s", temp_str_token(token.type));
		}
		expect_token(':');
	}
	Stmt **stmts = NULL;
	while (!is_token_eof() && !is_token('}') && !is_keyword(case_keyword) && !is_keyword(default_keyword))
		buf_push(stmts, parse_stmt());
	StmtBlock block = { ast_dup(stmts, buf_sizeof(stmts)), buf_len(stmts) };
	return (SwitchCase) { ast_dup(exprs, buf_sizeof(exprs)), buf_len(exprs), is_default, block };
}

Stmt *parse_stmt_switch() {
	Expr *expr = parse_paren_expr();
	SwitchCase *cases = NULL;
	expect_token('{');
	while (!is_token_eof() && !is_token('}')) {
		buf_push(cases, parse_switch_case());
	}
	expect_token('}');
	return stmt_switch(expr, ast_dup(cases, buf_sizeof(cases)), buf_len(cases));
}

Stmt *parse_stmt_return() {
	Expr *expr = parse_expr();
	expect_token(';');
	return stmt_return(expr);
}

Stmt *parse_stmt_break() {
	expect_token(';');
	return stmt_break();
}

Stmt *parse_stmt_continue() {
	expect_token(';');
	return stmt_continue();
}

Stmt *parse_stmt() {
	if (match_keyword(if_keyword)) {
		return parse_stmt_if();
	}
	else if (match_keyword(while_keyword)) {
		return parse_stmt_while();
	}
	else if (match_keyword(do_keyword)) {
		return parse_stmt_do_while();
	}
	else if (match_keyword(for_keyword)) {
		return parse_stmt_for();
	}
	else if (match_keyword(switch_keyword)) {
		return parse_stmt_switch();
	}
	else if (match_keyword(return_keyword)) {
		return parse_stmt_return();
	}
	else if (match_keyword(break_keyword)) {
		return parse_stmt_break();
	}
	else if (match_keyword(continue_keyword)) {
		return parse_stmt_continue();
	}
	else if (is_token('{')) {
		return stmt_block(parse_stmt_block());
	}
	else {
		Stmt *stmt = parse_base_stmt();
		expect_token(';');
		return stmt;
	}
}

/* ---------------------------------------- */
/* --------- Declaration Parsing ---------- */
/* ---------------------------------------- */

EnumItem parse_enum_item() {
	const char *name = parse_name();
	Expr* expr = NULL;
	if (match_token('=')) {
		expr = parse_expr();
	}
	return (EnumItem) { name, expr };
}

Decl *parse_enum_decl() {
	const char *name = parse_name();
	expect_token('{');
	EnumItem *items = NULL;
	if (!is_token('}')) {
		buf_push(items, parse_enum_item());
		while (match_token(',')) {
			//if (is_token('}'))
			//	break;
			buf_push(items, parse_enum_item());
		}
	}
	expect_token('}');
	return decl_enum(name, ast_dup(items, buf_sizeof(items)), buf_len(items));
}

AggregateItem parse_aggregate_field() {
	const char **names = NULL;
	buf_push(names, parse_name());
	while (match_token(',')) {
		buf_push(names, parse_name());
	}
	expect_token(':');
	Typespec *type = parse_type();
	expect_token(';');
	return (AggregateItem) { names, buf_len(names), type };
}

Decl *parse_aggregate_decl(DeclType type) {
	const char *name = parse_name();
	expect_token('{');
	AggregateItem *items = NULL;
	while (!is_token('}')) {
		buf_push(items, parse_aggregate_field());
	}
	expect_token('}');
	return decl_aggregate(type, name, ast_dup(items, buf_sizeof(items)), buf_len(items));
}

Decl *parse_var_decl() {
	const char *name = parse_name();
	if (match_token('=')) {
		Expr *expr = parse_expr();
		return decl_var(name, NULL, expr);
	}
	else if (match_token(':')) {
		Typespec *type = parse_type();
		Expr *expr = NULL;
		if (match_token('=')) {
			expr = parse_expr();
		}
		return decl_var(name, type, expr);
	}
	else {
		fatal_syntax_error("Expected token ':' or '=', got %s", temp_str_token(token.type));
		return NULL;
	}
}

Decl *parse_const_decl() {
	const char *name = parse_name();
	expect_token('=');
	return decl_const(name, parse_expr());
}

Decl *parse_typedef_decl() {
	const char *name = parse_name();
	expect_token('=');
	return decl_typedef(name, parse_type());
}

FuncArg parse_func_arg() {
	const char *name = parse_name();
	expect_token(':');
	return (FuncArg) { name, parse_type() };
}

Decl *parse_func_decl() {
	const char *name = parse_name();
	expect_token('(');
	FuncArg *args = NULL;
	if (!is_token(')')) {
		buf_push(args, parse_func_arg());
		while (match_token(',')) {
			buf_push(args, parse_func_arg());
		}
	}
	expect_token(')');
	Typespec *ret_type = NULL;
	if (match_token(TOKEN_ARROW)) {
		ret_type = parse_type();
	}
	StmtBlock block = parse_stmt_block();
	return decl_func(name, ast_dup(args, buf_sizeof(args)), buf_len(args), ret_type, block);
}

Decl *parse_decl() {
	if (match_keyword(enum_keyword)) {
		return parse_enum_decl();
	}
	else if (match_keyword(struct_keyword)) {
		return parse_aggregate_decl(DECL_STRUCT);
	}
	else if (match_keyword(union_keyword)) {
		return parse_aggregate_decl(DECL_UNION);
	}
	else if (match_keyword(var_keyword)) {
		return parse_var_decl();
	}
	else if (match_keyword(const_keyword)) {
		return parse_const_decl();
	}
	else if (match_keyword(typedef_keyword)) {
		return parse_typedef_decl();
	}
	else if (is_token(TOKEN_NAME)) {
		return parse_func_decl();
	}
	else {
		fatal_syntax_error("Expected function declaration or keyword, got %s", temp_str_token(token.type));
		return NULL;
	}
}

void parse_test() {
	const char *tests[] = {
		"fact_rec(n: int) -> int { "	\
		"	if (n <= 1) {"				\
		"		return 1;"				\
		"	}"							\
		"	return n * fact_rec(n - 1);"\
		"}"
	};
	for (const char **it = tests; it != tests + sizeof(tests) / sizeof(*tests); it++) {
		init_code(*it);
		Decl *decl = parse_decl();
		print_decl(decl);
		printf("\n");
	}
}