Arena ast_arena;

void *ast_alloc(size_t size) {
	assert(size != 0);
	void *ptr = arena_alloc(&ast_arena, size);
	memset(ptr, 0, size);
	return ptr;
}

void *ast_dup(const void *src, size_t size) {
	if (size == 0)
		return NULL;
	void *ptr = arena_alloc(&ast_arena, size);
	memcpy(ptr, src, size);
	return ptr;
}

Typespec *typespec_new(TypespecKind kind) {
	Typespec *type = ast_alloc(sizeof(Typespec));
	type->kind = kind;
	return type;
}

Typespec *typespec_name(const char *name) {
	Typespec *type = typespec_new(TYPESPEC_NAME);
	type->name = name;
	return type;
}

Typespec *typespec_func(Typespec **args, size_t num_args, Typespec *ret) {
	Typespec *type = typespec_new(TYPESPEC_FUNC);
	type->func.args = args;
	type->func.num_args = num_args;
	type->func.ret = ret;
	return type;
}

Typespec *typespec_array(Typespec *elem, Expr *size) {
	Typespec *type = typespec_new(TYPESPEC_ARRAY);
	type->array.elem = elem;
	type->array.size = size;
	return type;
}

Typespec *typespec_ptr(Typespec *elem) {
	Typespec *type = typespec_new(TYPESPEC_PTR);
	type->ptr.elem = elem;
	return type;
}

Decl *decl_new(DeclType type, const char *name) {
	Decl *decl = ast_alloc(sizeof(Decl));
	decl->type = type;
	decl->name = name;
	return decl;
}

Decl *decl_enum(const char *name, EnumItem *items, size_t num_items) {
	Decl *decl = decl_new(DECL_ENUM, name);
	decl->enum_decl.items = items;
	decl->enum_decl.num_items = num_items;
	return decl;
}

Decl *decl_aggregate(DeclType type, const char *name, AggregateItem *items, size_t num_items) {
	assert(type == DECL_STRUCT || type == DECL_UNION);
	Decl *decl = decl_new(type, name);
	decl->aggregate_decl.items = items;
	decl->aggregate_decl.num_items = num_items;
	return decl;
}

Decl *decl_var(const char *name, Typespec *type, Expr *expr) {
	Decl *decl = decl_new(DECL_VAR, name);
	decl->var_decl.type = type;
	decl->var_decl.expr = expr;
	return decl;
}

Decl *decl_const(const char *name, Expr *expr) {
	Decl *decl = decl_new(DECL_CONST, name);
	decl->const_decl.expr = expr;
	return decl;
}

Decl *decl_typedef(const char *name, Typespec *type) {
	Decl *decl = decl_new(DECL_TYPEDEF, name);
	decl->typedef_decl.type = type;
	return decl;
}

Decl *decl_func(const char *name, FuncArg *args, size_t num_args, Typespec *ret, StmtBlock block) {
	Decl *decl = decl_new(DECL_FUNC, name);
	decl->func_decl.args = args;
	decl->func_decl.num_args = num_args;
	decl->func_decl.return_type = ret;
	decl->func_decl.block = block;
	return decl;
}

Stmt *stmt_new(StmtType type) {
	Stmt *stmt = ast_alloc(sizeof(Stmt));
	stmt->type = type;
	return stmt;
}

Stmt *stmt_return(Expr *expr) {
	Stmt *stmt = stmt_new(STMT_RETURN);
	stmt->return_stmt.expr = expr;
	return stmt;
}

Stmt *stmt_break() {
	return stmt_new(STMT_BREAK);
}

Stmt *stmt_continue() {
	return stmt_new(STMT_CONTINUE);
}

Stmt *stmt_block(StmtBlock block) {
	Stmt *stmt = stmt_new(STMT_BLOCK);
	stmt->block = block;
	return stmt;
}

Stmt *stmt_if(Expr *cond, StmtBlock then_block, ElseIf *elseifs, size_t num_elseifs, StmtBlock else_block) {
	Stmt *stmt = stmt_new(STMT_IF);
	stmt->if_stmt.cond = cond;
	stmt->if_stmt.then_block = then_block;
	stmt->if_stmt.elseifs = elseifs;
	stmt->if_stmt.num_elseifs = num_elseifs;
	stmt->if_stmt.else_block = else_block;
	return stmt;
}

Stmt *stmt_while(Expr *cond, StmtBlock block) {
	Stmt *stmt = stmt_new(STMT_WHILE);
	stmt->while_stmt.cond = cond;
	stmt->while_stmt.block = block;
	return stmt;
}

Stmt *stmt_do_while(StmtBlock block, Expr *cond) {
	Stmt *stmt = stmt_new(STMT_DO_WHILE);
	stmt->while_stmt.block = block;
	stmt->while_stmt.cond = cond;
	return stmt;
}

Stmt *stmt_for(Stmt *init, Expr *cond, Stmt *update, StmtBlock block) {
	Stmt *stmt = stmt_new(STMT_FOR);
	stmt->for_stmt.init = init;
	stmt->for_stmt.cond = cond;
	stmt->for_stmt.update = update;
	stmt->for_stmt.block = block;
	return stmt;
}

Stmt *stmt_switch(Expr *expr, SwitchCase *cases, size_t num_cases) {
	Stmt *stmt = stmt_new(STMT_SWITCH);
	stmt->switch_stmt.expr = expr;
	stmt->switch_stmt.cases = cases;
	stmt->switch_stmt.num_cases = num_cases;
	return stmt;
}

Stmt *stmt_assign(TokenType op, Expr *left, Expr *right) {
	Stmt *stmt = stmt_new(STMT_ASSIGN);
	stmt->assign_stmt.op = op;
	stmt->assign_stmt.left = left;
	stmt->assign_stmt.right = right;
	return stmt;
}

Stmt *stmt_init(const char *name, Expr *expr) {
	Stmt *stmt = stmt_new(STMT_INIT);
	stmt->init_stmt.name = name;
	stmt->init_stmt.expr = expr;
	return stmt;
}

Stmt *stmt_expr(Expr *expr) {
	Stmt *stmt = stmt_new(STMT_EXPR);
	stmt->expr = expr;
	return stmt;
}

Expr *expr_new(ExprType type) {
	Expr *expr = ast_alloc(sizeof(Expr));
	expr->type = type;
	return expr;
}

Expr *expr_sizeof_expr(Expr *expr) {
	Expr *e = expr_new(EXPR_SIZEOF);
	e->sizeof_expr.kind = SIZEOF_EXPR;
	e->sizeof_expr.expr = expr;
	return e;
}

Expr *expr_sizeof_type(Typespec *type) {
	Expr *e = expr_new(EXPR_SIZEOF);
	e->sizeof_expr.kind = SIZEOF_TYPE;
	e->sizeof_expr.type = type;
	return e;
}

Expr *expr_int(uint64_t int_val) {
	Expr *expr = expr_new(EXPR_INT);
	expr->int_val = int_val;
	return expr;
}

Expr *expr_float(double float_val) {
	Expr *expr = expr_new(EXPR_FLOAT);
	expr->float_val = float_val;
	return expr;
}

Expr *expr_str(const char *str_val) {
	Expr *expr = expr_new(EXPR_STRING);
	expr->str_val = str_val;
	return expr;
}

Expr *expr_name(const char *name) {
	Expr *expr = expr_new(EXPR_NAME);
	expr->name = name;
	return expr;
}

Expr *expr_compound(Typespec *type, Expr **args, size_t num_args) {
	Expr *expr = expr_new(EXPR_COMPOUND);
	expr->compound.type = type;
	expr->compound.args = args;
	expr->compound.num_args = num_args;
	return expr;
}

Expr *expr_cast(Typespec *cast_type, Expr *cast_expr) {
	Expr *expr = expr_new(EXPR_CAST);
	expr->cast.type = cast_type;
	expr->cast.expr = cast_expr;
	return expr;
}

Expr *expr_call(Expr *operand, Expr **args, size_t num_args) {
	Expr *expr = expr_new(EXPR_CALL);
	expr->call.expr = operand;
	expr->call.args = args;
	expr->call.num_args = num_args;
	return expr;
}
Expr *expr_index(Expr *operand, Expr* index) {
	Expr *expr = expr_new(EXPR_INDEX);
	expr->index.expr = operand;
	expr->index.index = index;
	return expr;
}
Expr *expr_field(Expr *operand, const char *field) {
	Expr *expr = expr_new(EXPR_FIELD);
	expr->field.expr = operand;
	expr->field.name = field;
	return expr;
}

Expr *expr_unary(TokenType op, Expr *operand) {
	Expr *expr = expr_new(EXPR_UNARY);
	expr->unary.op = op;
	expr->unary.expr = operand;
	return expr;
}

Expr *expr_binary(TokenType op, Expr *left, Expr *right) {
	Expr *expr = expr_new(EXPR_BINARY);
	expr->binary.op = op;
	expr->binary.left = left;
	expr->binary.right = right;
	return expr;
}

Expr *expr_ternary(Expr *cond, Expr *then_expr, Expr *else_expr) {
	Expr *expr = expr_new(EXPR_TERNARY);
	expr->ternary.cond = cond;
	expr->ternary.then_expr = then_expr;
	expr->ternary.else_expr = else_expr;
	return expr;
}