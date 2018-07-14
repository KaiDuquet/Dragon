void print_expr(Expr *expr);
void print_decl(Decl *decl);
void print_stmt(Stmt *stmt);

int indent;

void println() {
	printf("\n%.*s", 2 * indent, "                                                                      ");
}

void print_typespec(Typespec *type) {
	Typespec *t = type;
	switch (t->kind) {
	case TYPESPEC_NAME:
		printf("%s", t->name);
		break;
	case TYPESPEC_FUNC: {
		printf("(func (");
		for (Typespec **it = t->func.args; it != t->func.args + t->func.num_args; it++) {
			printf(" ");
			print_typespec(*it);
		}
		printf(")");
		print_typespec(t->func.ret);
		printf(")");
		break;
	}
	case TYPESPEC_ARRAY:
		printf("(array ");
		print_typespec(t->array.elem);
		printf(" ");
		print_expr(t->array.size);
		printf(")");
		break;
	case TYPESPEC_PTR:
		printf("(ptr ");
		print_typespec(t->ptr.elem);
		printf(")");
		break;
	default:
		assert(0);
		break;
	}
}

void print_expr(Expr *expr) {
	switch (expr->type) {
	case EXPR_INT:
		printf("%" PRIu64, expr->int_val);
		break;
	case EXPR_FLOAT:
		printf("%f", expr->float_val);
		break;
	case EXPR_STRING:
		printf("\"%s\"", expr->str_val);
		break;
	case EXPR_NAME:
		printf("%s", expr->name);
		break;
	case EXPR_SIZEOF:
		printf("(sizeof ");
		if (expr->sizeof_expr.kind == SIZEOF_EXPR) {
			print_expr(expr->sizeof_expr.expr);
		}
		else {
			assert(expr->sizeof_expr.kind == SIZEOF_TYPE);
			print_typespec(expr->sizeof_expr.type);
		}
		printf(")");
		break;
	case EXPR_CAST:
		printf("(cast ");
		print_typespec(expr->cast.type);
		printf(" ");
		print_expr(expr->cast.expr);
		printf(")");
		break;
	case EXPR_CALL:
		printf("(");
		print_expr(expr->call.expr);
		for (Expr **it = expr->call.args; it != expr->call.args + expr->call.num_args; it++) {
			printf(" ");
			print_expr(*it);
		}
		printf(")");
		break;
	case EXPR_INDEX:
		printf("(index ");
		print_expr(expr->index.expr);
		printf(" ");
		print_expr(expr->index.index);
		printf(")");
		break;
	case EXPR_FIELD:
		printf("(field ");
		print_expr(expr->field.expr);
		printf(" %s)", expr->field.name);
		break;
	case EXPR_COMPOUND:
		printf("(compound ");
		if (expr->compound.type) {
			print_typespec(expr->compound.type);
		}
		else {
			printf("null");
		}
		for (Expr **it = expr->compound.args; it != expr->compound.args + expr->compound.num_args; it++) {
			printf(" ");
			print_expr(*it);
		}
		printf(")");
		break;
	case EXPR_UNARY:
		printf("(%s ", temp_str_token(expr->unary.op));
		print_expr(expr->unary.expr);
		printf(")");
		break;
	case EXPR_BINARY:
		printf("(%s ", temp_str_token(expr->binary.op));
		print_expr(expr->binary.left);
		printf(" ");
		print_expr(expr->binary.right);
		printf(")");
		break;
	case EXPR_TERNARY:
		printf("(? ");
		print_expr(expr->ternary.cond);
		printf(" ");
		print_expr(expr->ternary.then_expr);
		printf(" ");
		print_expr(expr->ternary.else_expr);
		printf(")");
		break;
	default:
		assert(0);
		break;
	}
}

void print_stmt_block(StmtBlock block) {
	printf("(block ");
	indent++;
	for (Stmt **it = block.stmts; it != block.stmts + block.num_stmts; it++) {
		println();
		print_stmt(*it);
	}
	indent--;
	printf(")");
}

void print_stmt(Stmt *stmt) {
	Stmt *s = stmt;
	switch (s->type) {
	case STMT_RETURN:
		printf("(return ");
		print_expr(s->return_stmt.expr);
		printf(")");
		break;
	case STMT_BREAK:
		printf("(break)");
		break;
	case STMT_CONTINUE:
		printf("(continue)");
		break;
	case STMT_BLOCK:
		print_stmt_block(s->block);
		break;
	case STMT_IF:
		printf("(if ");
		print_expr(s->if_stmt.cond);
		indent++;
		println();
		print_stmt_block(s->if_stmt.then_block);
		for (ElseIf *it = s->if_stmt.elseifs; it != s->if_stmt.elseifs + s->if_stmt.num_elseifs; it++) {
			println();
			printf("elseif ");
			print_expr(it->cond);
			println();
			print_stmt_block(it->block);
		}
		if (s->if_stmt.else_block.num_stmts != 0) {
			println();
			printf("else ");
			println();
			print_stmt_block(s->if_stmt.else_block);
		}
		indent--;
		printf(")");
		break;
	case STMT_WHILE:
		printf("(while ");
		print_expr(s->while_stmt.cond);
		indent++;
		println();
		print_stmt_block(s->while_stmt.block);
		indent--;
		printf(")");
		break;
	case STMT_DO_WHILE:
		printf("(do-while ");
		print_expr(s->while_stmt.cond);
		indent++;
		println();
		print_stmt_block(s->while_stmt.block);
		indent--;
		printf(")");
		break;
	case STMT_FOR:
		printf("(for ");
		print_stmt(s->for_stmt.init);
		print_expr(s->for_stmt.cond);
		print_stmt(s->for_stmt.update);
		indent++;
		println();
		print_stmt_block(s->for_stmt.block);
		indent--;
		printf(")");
		break;
	case STMT_SWITCH:
		printf("(switch ");
		print_expr(s->switch_stmt.expr);
		indent++;
		for (SwitchCase *it = s->switch_stmt.cases; it != s->switch_stmt.cases + s->switch_stmt.num_cases; it++) {
			println();
			printf("(case (%s", it->is_default ? " default" : "");
			for (Expr **expr = it->exprs; expr != it->exprs + it->num_exprs; expr++) {
				printf(" ");
				print_expr(*expr);
			}
			printf(" ) ");
			indent++;
			println();
			print_stmt_block(it->block);
			indent--;
		}
		indent--;
		printf(")");
		break;
	case STMT_ASSIGN:
		printf("(%s ", token_type_names[s->assign_stmt.op]);
		print_expr(s->assign_stmt.left);
		if (s->assign_stmt.right) {
			printf(" ");
			print_expr(s->assign_stmt.right);
		}
		printf(")");
		break;
	case STMT_INIT:
		printf("(:= %s ", s->init_stmt.name);
		print_expr(s->init_stmt.expr);
		printf(")");
		break;
	case STMT_EXPR:
		print_expr(s->expr);
		break;
	default:
		assert(0);
		break;
	}
}

void print_aggregate_decl(Decl *decl) {
	Decl *d = decl;
	for (AggregateItem *it = d->aggregate_decl.items; it != d->aggregate_decl.items + d->aggregate_decl.num_items; it++) {
		println();
		printf("(");
		print_typespec(it->type);
		for (const char **name = it->names; name != it->names + it->num_names; name++) {
			printf(" %s", *name);
		}
		printf(")");
	}
}

void print_decl(Decl *decl) {
	Decl *d = decl;
	switch (d->type) {
	case DECL_ENUM:
		printf("(enum %s", d->name);
		indent++;
		for (EnumItem *it = d->enum_decl.items; it != d->enum_decl.items + d->enum_decl.num_items; it++) {
			println();
			printf("(%s ", it->name);
			if (it->init) {
				print_expr(it->init);
			}
			else {
				printf("null");
			}
			printf(")");
		}
		indent--;
		printf(")");
		break;
	case DECL_STRUCT:
		printf("(struct %s", d->name);
		indent++;
		print_aggregate_decl(d);
		indent--;
		printf(")");
		break;
	case DECL_UNION:
		printf("(union %s", d->name);
		indent++;
		print_aggregate_decl(d);
		indent--;
		printf(")");
		break;
	case DECL_VAR:
		printf("(var %s", d->name);
		if (d->var_decl.type) {
			print_typespec(d->var_decl.type);
		}
		else {
			printf("null");
		}
		printf(" ");
		print_expr(d->var_decl.expr);
		printf(")");
		break;
	case DECL_CONST:
		printf("(const %s ", d->name);
		print_expr(d->const_decl.expr);
		break;
	case DECL_TYPEDEF:
		printf("(typedef %s ", d->name);
		print_typespec(d->typedef_decl.type);
		printf(")");
		break;
	case DECL_FUNC:
		printf("(func %s ", d->name);
		printf("(");
		for (FuncArg *it = d->func_decl.args; it != d->func_decl.args + d->func_decl.num_args; it++) {
			printf(" %s ", it->name);
			print_typespec(it->type);
		}
		printf(") ");
		if (d->func_decl.return_type) {
			print_typespec(d->func_decl.return_type);
		}
		else {
			printf("null");
		}
		indent++;
		println();
		print_stmt_block(d->func_decl.block);
		indent--;
		printf(")");
		break;
	default:
		assert(0);
		break;
	}
}

void print_test() {
	// Expressions
	Expr *exprs[] = {
		expr_binary('+', expr_int(1), expr_int(2)),
		expr_unary('-', expr_float(3.14)),
		expr_ternary(expr_name("flag"), expr_str("true"), expr_str("false")),
		expr_field(expr_name("person"), "name"),
		expr_call(expr_name("fact"), (Expr*[]) { expr_int(42) }, 1),
		expr_index(expr_field(expr_name("person"), "siblings"), expr_int(3)),
		expr_cast(typespec_ptr(typespec_name("int")), expr_name("void_ptr")),
		expr_compound(typespec_name("Vector"), (Expr*[]) { expr_int(1), expr_unary('-', expr_int(1)) }, 2)
	};
	for (Expr **it = exprs; it != exprs + sizeof(exprs) / sizeof(*exprs); it++) {
		print_expr(*it);
		printf("\n");
	}

	// Statememts
	Stmt *stmts[] = {
		stmt_return(expr_int(42)),
		stmt_break(),
		stmt_continue(),
		stmt_block(
			(StmtBlock) {
				(Stmt*[]) {
					stmt_break(),
					stmt_continue()
				},
				2,
			}
		),
		stmt_expr(expr_call(expr_name("print"), (Expr*[]) { expr_int(1), expr_int(2) }, 2)),
		stmt_init("x", expr_int(42)),
		stmt_if(
			expr_name("flag1"),
			(StmtBlock) {
				(Stmt*[]) {
					stmt_return(expr_int(1))
				},
				1
			},
			(ElseIf[]) {
				expr_name("flag2"),
				(StmtBlock) {
					(Stmt*[]) {
						stmt_return(expr_int(2))
					},
					1
				}
			},
			1,
			(StmtBlock) {
				(Stmt*[]) {
					stmt_return(expr_int(3))
				},
				1
			}
		),
		stmt_while(
			expr_name("running"),
			(StmtBlock) {
				(Stmt*[]) {
					stmt_assign(TOKEN_ADD_ASSIGN, expr_name("i"), expr_int(16)),
				},
				1,
			}
		),
		stmt_switch(
			expr_name("val"),
			(SwitchCase[]) {
				{
					(Expr*[]) {	expr_int(3), expr_int(4) },
					2,
					false,
					(StmtBlock) {
						(Stmt*[]) {
							stmt_return(expr_name("val"))
						},
						1,
					},
				},
				{
					(Expr*[]){expr_int(1)},
					1,
					true,
					(StmtBlock) {
						(Stmt*[]) {
							stmt_return(expr_int(0))
						},
						1,
					},
				},
			},
			2
		),
	};
	for (Stmt **it = stmts; it != stmts + sizeof(stmts) / sizeof(*stmts); it++) {
		print_stmt(*it);
		printf("\n");
	}
}