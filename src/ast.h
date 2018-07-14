typedef struct Typespec_t Typespec;
typedef struct Decl_t Decl;
typedef struct Stmt_t Stmt;
typedef struct Expr_t Expr;

typedef struct StmtBlock_t {
	Stmt **stmts;
	size_t num_stmts;
} StmtBlock;

typedef enum TypespecKind_t {
	TYPESPEC_NONE,
	TYPESPEC_NAME,
	TYPESPEC_FUNC,
	TYPESPEC_ARRAY,
	TYPESPEC_PTR,
} TypespecKind;

typedef struct FuncTypespec_t {
	size_t num_args;
	Typespec **args;
	Typespec *ret;
} FuncTypespec;

typedef struct PtrTypespec_t {
	Typespec *elem;
} PtrTypespec;

typedef struct ArrayTypespec_t {
	Typespec *elem;
	Expr *size;
} ArrayTypespec;

struct Typespec_t {
	TypespecKind kind;
	union {
		const char *name;
		FuncTypespec func;
		PtrTypespec ptr;
		ArrayTypespec array;
	};
};

typedef enum DeclType_t {
	DECL_NONE,
	DECL_ENUM,
	DECL_STRUCT,
	DECL_UNION,
	DECL_VAR,
	DECL_CONST,
	DECL_TYPEDEF,
	DECL_FUNC
} DeclType;

typedef struct FuncArg_t {
	const char *name;
	Typespec *type;
} FuncArg;

typedef struct FuncDecl_t {
	FuncArg *args;
	size_t num_args;
	Typespec *return_type;
	StmtBlock block;
} FuncDecl;

typedef struct EnumItem_t {
	const char *name;
	Expr *init;
} EnumItem;

typedef struct EnumDecl_t {
	EnumItem *items;
	size_t num_items;
} EnumDecl;

typedef struct AggregateItem_t {
	const char **names;
	size_t num_names;
	Typespec *type;
} AggregateItem;

typedef struct AggregateDecl_t {
	AggregateItem *items;
	size_t num_items;
} AggregateDecl;

typedef struct TypedefDecl_t {
	Typespec *type;
} TypedefDecl;

typedef struct VarDecl_t {
	Typespec *type;
	Expr *expr;
} VarDecl;

typedef struct ConstDecl_t {
	Expr *expr;
} ConstDecl;

struct Decl_t {
	DeclType type;
	const char *name;
	union {
		EnumDecl enum_decl;
		AggregateDecl aggregate_decl;
		FuncDecl func_decl;
		TypedefDecl typedef_decl;
		VarDecl var_decl;
		ConstDecl const_decl;
	};
};

typedef enum ExprType_t {
	EXPR_NONE,
	EXPR_INT,
	EXPR_FLOAT,
	EXPR_STRING,
	EXPR_NAME,
	EXPR_CAST,
	EXPR_CALL,
	EXPR_INDEX,
	EXPR_FIELD,
	EXPR_COMPOUND,
	EXPR_UNARY,
	EXPR_BINARY,
	EXPR_TERNARY,
	EXPR_SIZEOF
} ExprType;

typedef enum SizeofKind_t {
	SIZEOF_EXPR,
	SIZEOF_TYPE
} SizeofKind;

typedef struct SizeofExpr_t {
	SizeofKind kind;
	union {
		Expr *expr;
		Typespec *type;
	};
} SizeofExpr;

typedef struct CompoundExpr_t {
	Typespec *type;
	Expr **args;
	size_t num_args;
} CompoundExpr;

typedef struct CastExpr_t {
	Typespec *type;
	Expr *expr;
} CastExpr;

typedef struct UnaryExpr_t {
	TokenType op;
	Expr *expr;
} UnaryExpr;

typedef struct BinaryExpr_t {
	TokenType op;
	Expr *left;
	Expr *right;
} BinaryExpr;

typedef struct TernaryExpr_t {
	TokenType op;
	Expr *cond;
	Expr *then_expr;
	Expr *else_expr;
} TernaryExpr;

typedef struct CallExpr_t {
	Expr *expr;
	Expr **args;
	size_t num_args;
} CallExpr;

typedef struct IndexExpr_t {
	Expr *expr;
	Expr *index;
} IndexExpr;

typedef struct FieldExpr_t {
	Expr *expr;
	const char *name;
} FieldExpr;

struct Expr_t {
	ExprType type;
	union {
		uint64_t int_val;
		double float_val;
		const char *str_val;
		const char *name;
		CompoundExpr compound;
		CastExpr cast;
		UnaryExpr unary;
		BinaryExpr binary;
		TernaryExpr ternary;
		CallExpr call;
		IndexExpr index;
		FieldExpr field;
		SizeofExpr sizeof_expr;
	};
};

typedef enum StmtType_t {
	STMT_NONE,
	STMT_IF,
	STMT_WHILE,
	STMT_FOR,
	STMT_DO_WHILE,
	STMT_SWITCH,
	STMT_RETURN,
	STMT_BREAK,
	STMT_CONTINUE,
	STMT_BLOCK,
	STMT_ASSIGN,
	STMT_INIT,
	STMT_EXPR,
} StmtType;

typedef struct ElseIf_t {
	Expr *cond;
	StmtBlock block;
} ElseIf;

typedef struct IfStmt_t {
	Expr *cond;
	StmtBlock then_block;
	ElseIf *elseifs;
	size_t num_elseifs;
	StmtBlock else_block;
} IfStmt;

typedef struct WhileStmt_t {
	Expr *cond;
	StmtBlock block;
} WhileStmt;

typedef struct ForStmt_t {
	Stmt *init;
	Expr *cond;
	Stmt *update;
	StmtBlock block;
} ForStmt;

typedef struct SwitchCase_t {
	Expr **exprs;
	size_t num_exprs;
	bool is_default;
	StmtBlock block;
} SwitchCase;

typedef struct SwitchStmt_t {
	Expr *expr;
	SwitchCase *cases;
	size_t num_cases;
} SwitchStmt;

typedef struct ReturnStmt_t {
	Expr *expr;
} ReturnStmt;

typedef struct AssignStmt_t {
	TokenType op;
	Expr *left;
	Expr *right;
} AssignStmt;

typedef struct InitStmt_t {
	const char *name;
	Expr *expr;
} InitStmt;

struct Stmt_t {
	StmtType type;
	union {
		IfStmt if_stmt;
		WhileStmt while_stmt;
		ForStmt for_stmt;
		SwitchStmt switch_stmt;
		StmtBlock block;
		AssignStmt assign_stmt;
		InitStmt init_stmt;
		ReturnStmt return_stmt;
		Expr *expr;
	};
};