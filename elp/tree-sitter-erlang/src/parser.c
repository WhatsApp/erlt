#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 11
#define STATE_COUNT 54
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 44
#define ALIAS_COUNT 0
#define TOKEN_COUNT 24
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 8
#define MAX_ALIAS_SEQUENCE_LENGTH 6

enum {
  sym__raw_atom = 1,
  anon_sym_DASH = 2,
  anon_sym_module = 3,
  anon_sym_LPAREN = 4,
  anon_sym_RPAREN = 5,
  anon_sym_DOT = 6,
  anon_sym_SEMI = 7,
  anon_sym_DASH_GT = 8,
  anon_sym_COMMA = 9,
  sym_wildcard = 10,
  sym_var = 11,
  aux_sym_integer_token1 = 12,
  aux_sym_integer_token2 = 13,
  sym_float = 14,
  anon_sym_DQUOTE = 15,
  aux_sym_string_token1 = 16,
  anon_sym_DOLLAR = 17,
  aux_sym_char_token1 = 18,
  anon_sym_SQUOTE = 19,
  aux_sym__quoted_atom_token1 = 20,
  anon_sym_SQUOTE2 = 21,
  sym__escape = 22,
  sym__comment = 23,
  sym_source_file = 24,
  sym__form = 25,
  sym_module_attribute = 26,
  sym_attribute = 27,
  sym_function = 28,
  sym_function_clause = 29,
  sym_arg_list = 30,
  sym_block = 31,
  sym__expr = 32,
  sym_integer = 33,
  sym_string = 34,
  sym_char = 35,
  sym_atom = 36,
  sym__quoted_atom = 37,
  aux_sym_source_file_repeat1 = 38,
  aux_sym_function_repeat1 = 39,
  aux_sym_arg_list_repeat1 = 40,
  aux_sym_block_repeat1 = 41,
  aux_sym_string_repeat1 = 42,
  aux_sym__quoted_atom_repeat1 = 43,
};

static const char *ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [sym__raw_atom] = "_raw_atom",
  [anon_sym_DASH] = "-",
  [anon_sym_module] = "module",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_DOT] = ".",
  [anon_sym_SEMI] = ";",
  [anon_sym_DASH_GT] = "->",
  [anon_sym_COMMA] = ",",
  [sym_wildcard] = "wildcard",
  [sym_var] = "var",
  [aux_sym_integer_token1] = "integer_token1",
  [aux_sym_integer_token2] = "integer_token2",
  [sym_float] = "float",
  [anon_sym_DQUOTE] = "\"",
  [aux_sym_string_token1] = "string_token1",
  [anon_sym_DOLLAR] = "$",
  [aux_sym_char_token1] = "char_token1",
  [anon_sym_SQUOTE] = "'",
  [aux_sym__quoted_atom_token1] = "_quoted_atom_token1",
  [anon_sym_SQUOTE2] = "'",
  [sym__escape] = "_escape",
  [sym__comment] = "_comment",
  [sym_source_file] = "source_file",
  [sym__form] = "_form",
  [sym_module_attribute] = "module_attribute",
  [sym_attribute] = "attribute",
  [sym_function] = "function",
  [sym_function_clause] = "function_clause",
  [sym_arg_list] = "arg_list",
  [sym_block] = "block",
  [sym__expr] = "_expr",
  [sym_integer] = "integer",
  [sym_string] = "string",
  [sym_char] = "char",
  [sym_atom] = "atom",
  [sym__quoted_atom] = "_quoted_atom",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_function_repeat1] = "function_repeat1",
  [aux_sym_arg_list_repeat1] = "arg_list_repeat1",
  [aux_sym_block_repeat1] = "block_repeat1",
  [aux_sym_string_repeat1] = "string_repeat1",
  [aux_sym__quoted_atom_repeat1] = "_quoted_atom_repeat1",
};

static TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [sym__raw_atom] = sym__raw_atom,
  [anon_sym_DASH] = anon_sym_DASH,
  [anon_sym_module] = anon_sym_module,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_DOT] = anon_sym_DOT,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_DASH_GT] = anon_sym_DASH_GT,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [sym_wildcard] = sym_wildcard,
  [sym_var] = sym_var,
  [aux_sym_integer_token1] = aux_sym_integer_token1,
  [aux_sym_integer_token2] = aux_sym_integer_token2,
  [sym_float] = sym_float,
  [anon_sym_DQUOTE] = anon_sym_DQUOTE,
  [aux_sym_string_token1] = aux_sym_string_token1,
  [anon_sym_DOLLAR] = anon_sym_DOLLAR,
  [aux_sym_char_token1] = aux_sym_char_token1,
  [anon_sym_SQUOTE] = anon_sym_SQUOTE,
  [aux_sym__quoted_atom_token1] = aux_sym__quoted_atom_token1,
  [anon_sym_SQUOTE2] = anon_sym_SQUOTE,
  [sym__escape] = sym__escape,
  [sym__comment] = sym__comment,
  [sym_source_file] = sym_source_file,
  [sym__form] = sym__form,
  [sym_module_attribute] = sym_module_attribute,
  [sym_attribute] = sym_attribute,
  [sym_function] = sym_function,
  [sym_function_clause] = sym_function_clause,
  [sym_arg_list] = sym_arg_list,
  [sym_block] = sym_block,
  [sym__expr] = sym__expr,
  [sym_integer] = sym_integer,
  [sym_string] = sym_string,
  [sym_char] = sym_char,
  [sym_atom] = sym_atom,
  [sym__quoted_atom] = sym__quoted_atom,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_function_repeat1] = aux_sym_function_repeat1,
  [aux_sym_arg_list_repeat1] = aux_sym_arg_list_repeat1,
  [aux_sym_block_repeat1] = aux_sym_block_repeat1,
  [aux_sym_string_repeat1] = aux_sym_string_repeat1,
  [aux_sym__quoted_atom_repeat1] = aux_sym__quoted_atom_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [sym__raw_atom] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_DASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_module] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [sym_wildcard] = {
    .visible = true,
    .named = true,
  },
  [sym_var] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_integer_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_integer_token2] = {
    .visible = false,
    .named = false,
  },
  [sym_float] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_DQUOTE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_string_token1] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_DOLLAR] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_char_token1] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_SQUOTE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym__quoted_atom_token1] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_SQUOTE2] = {
    .visible = true,
    .named = false,
  },
  [sym__escape] = {
    .visible = false,
    .named = true,
  },
  [sym__comment] = {
    .visible = false,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym__form] = {
    .visible = false,
    .named = true,
  },
  [sym_module_attribute] = {
    .visible = true,
    .named = true,
  },
  [sym_attribute] = {
    .visible = true,
    .named = true,
  },
  [sym_function] = {
    .visible = true,
    .named = true,
  },
  [sym_function_clause] = {
    .visible = true,
    .named = true,
  },
  [sym_arg_list] = {
    .visible = true,
    .named = true,
  },
  [sym_block] = {
    .visible = true,
    .named = true,
  },
  [sym__expr] = {
    .visible = false,
    .named = true,
  },
  [sym_integer] = {
    .visible = true,
    .named = true,
  },
  [sym_string] = {
    .visible = true,
    .named = true,
  },
  [sym_char] = {
    .visible = true,
    .named = true,
  },
  [sym_atom] = {
    .visible = true,
    .named = true,
  },
  [sym__quoted_atom] = {
    .visible = false,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_function_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_arg_list_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_block_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_string_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym__quoted_atom_repeat1] = {
    .visible = false,
    .named = false,
  },
};

enum {
  field_arg_list = 1,
  field_args = 2,
  field_body = 3,
  field_clauses = 4,
  field_exprs = 5,
  field_forms = 6,
  field_name = 7,
  field_value = 8,
};

static const char *ts_field_names[] = {
  [0] = NULL,
  [field_arg_list] = "arg_list",
  [field_args] = "args",
  [field_body] = "body",
  [field_clauses] = "clauses",
  [field_exprs] = "exprs",
  [field_forms] = "forms",
  [field_name] = "name",
  [field_value] = "value",
};

static const TSFieldMapSlice ts_field_map_slices[16] = {
  [1] = {.index = 0, .length = 1},
  [2] = {.index = 1, .length = 1},
  [3] = {.index = 2, .length = 1},
  [4] = {.index = 3, .length = 2},
  [5] = {.index = 5, .length = 2},
  [6] = {.index = 7, .length = 1},
  [7] = {.index = 8, .length = 3},
  [8] = {.index = 11, .length = 1},
  [9] = {.index = 12, .length = 2},
  [10] = {.index = 14, .length = 2},
  [11] = {.index = 16, .length = 2},
  [12] = {.index = 18, .length = 1},
  [13] = {.index = 19, .length = 2},
  [14] = {.index = 21, .length = 1},
  [15] = {.index = 22, .length = 2},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_forms, 0},
  [1] =
    {field_clauses, 0},
  [2] =
    {field_clauses, 1},
  [3] =
    {field_clauses, 0},
    {field_clauses, 1, .inherited = true},
  [5] =
    {field_clauses, 0, .inherited = true},
    {field_clauses, 1, .inherited = true},
  [7] =
    {field_args, 1},
  [8] =
    {field_arg_list, 1},
    {field_body, 3},
    {field_name, 0},
  [11] =
    {field_exprs, 0},
  [12] =
    {field_args, 1},
    {field_args, 2, .inherited = true},
  [14] =
    {field_args, 0, .inherited = true},
    {field_args, 1, .inherited = true},
  [16] =
    {field_exprs, 0},
    {field_exprs, 1, .inherited = true},
  [18] =
    {field_name, 3},
  [19] =
    {field_name, 1},
    {field_value, 3},
  [21] =
    {field_exprs, 1},
  [22] =
    {field_exprs, 0, .inherited = true},
    {field_exprs, 1, .inherited = true},
};

static TSSymbol ts_alias_sequences[16][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(19);
      if (lookahead == '"') ADVANCE(36);
      if (lookahead == '$') ADVANCE(39);
      if (lookahead == '%') ADVANCE(51);
      if (lookahead == '\'') ADVANCE(46);
      if (lookahead == '(') ADVANCE(22);
      if (lookahead == ')') ADVANCE(23);
      if (lookahead == ',') ADVANCE(27);
      if (lookahead == '-') ADVANCE(21);
      if (lookahead == '.') ADVANCE(24);
      if (lookahead == ';') ADVANCE(25);
      if (lookahead == '\\') ADVANCE(10);
      if (lookahead == '_') ADVANCE(28);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(32);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 222)) ADVANCE(29);
      if (('a' <= lookahead && lookahead <= 'z') ||
          (223 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(42);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(17)
      END_STATE();
    case 1:
      if (lookahead == '\n') SKIP(5)
      if (lookahead == '%') ADVANCE(45);
      if (lookahead == '\'') ADVANCE(46);
      if (lookahead == '\\') ADVANCE(10);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(44);
      if (lookahead != 0) ADVANCE(45);
      END_STATE();
    case 2:
      if (lookahead == '"') ADVANCE(36);
      if (lookahead == '%') ADVANCE(50);
      if (lookahead == '\\') ADVANCE(10);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(37);
      if (lookahead != 0) ADVANCE(38);
      END_STATE();
    case 3:
      if (lookahead == '%') ADVANCE(51);
      if (lookahead == '-') ADVANCE(6);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(3)
      END_STATE();
    case 4:
      if (lookahead == '%') ADVANCE(51);
      if (lookahead == '\\') ADVANCE(10);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(41);
      if (lookahead != 0) ADVANCE(40);
      END_STATE();
    case 5:
      if (lookahead == '%') ADVANCE(51);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(5)
      END_STATE();
    case 6:
      if (lookahead == '>') ADVANCE(26);
      END_STATE();
    case 7:
      if (lookahead == '{') ADVANCE(15);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(14);
      END_STATE();
    case 8:
      if (lookahead == '}') ADVANCE(47);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(8);
      END_STATE();
    case 9:
      if (lookahead == '+' ||
          lookahead == '-') ADVANCE(13);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(35);
      END_STATE();
    case 10:
      if (lookahead == '\n' ||
          lookahead == 'b' ||
          ('d' <= lookahead && lookahead <= 'f') ||
          lookahead == 'n' ||
          ('r' <= lookahead && lookahead <= 't') ||
          lookahead == 'v') ADVANCE(47);
      if (lookahead == 'x') ADVANCE(7);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(49);
      END_STATE();
    case 11:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(34);
      END_STATE();
    case 12:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(33);
      END_STATE();
    case 13:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(35);
      END_STATE();
    case 14:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(47);
      END_STATE();
    case 15:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(8);
      END_STATE();
    case 16:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(30);
      END_STATE();
    case 17:
      if (eof) ADVANCE(19);
      if (lookahead == '"') ADVANCE(36);
      if (lookahead == '$') ADVANCE(39);
      if (lookahead == '%') ADVANCE(51);
      if (lookahead == '\'') ADVANCE(43);
      if (lookahead == '(') ADVANCE(22);
      if (lookahead == ')') ADVANCE(23);
      if (lookahead == ',') ADVANCE(27);
      if (lookahead == '-') ADVANCE(21);
      if (lookahead == '.') ADVANCE(24);
      if (lookahead == ';') ADVANCE(25);
      if (lookahead == '_') ADVANCE(28);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(32);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 222)) ADVANCE(29);
      if (('a' <= lookahead && lookahead <= 'z') ||
          (223 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(42);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(17)
      END_STATE();
    case 18:
      if (eof) ADVANCE(19);
      if (lookahead == '"') ADVANCE(36);
      if (lookahead == '$') ADVANCE(39);
      if (lookahead == '%') ADVANCE(51);
      if (lookahead == '\'') ADVANCE(43);
      if (lookahead == ')') ADVANCE(23);
      if (lookahead == '-') ADVANCE(20);
      if (lookahead == '_') ADVANCE(28);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(32);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 222)) ADVANCE(29);
      if (('a' <= lookahead && lookahead <= 'z') ||
          (223 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(42);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(18)
      END_STATE();
    case 19:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(anon_sym_DASH);
      if (lookahead == '>') ADVANCE(26);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(anon_sym_DASH_GT);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(sym_wildcard);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(29);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(sym_var);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(29);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(aux_sym_integer_token1);
      if (lookahead == '_') ADVANCE(16);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(30);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(aux_sym_integer_token2);
      if (lookahead == '#') ADVANCE(16);
      if (lookahead == '.') ADVANCE(11);
      if (lookahead == '_') ADVANCE(12);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(33);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(aux_sym_integer_token2);
      if (lookahead == '#') ADVANCE(16);
      if (lookahead == '.') ADVANCE(11);
      if (lookahead == '_') ADVANCE(12);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(31);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(aux_sym_integer_token2);
      if (lookahead == '.') ADVANCE(11);
      if (lookahead == '_') ADVANCE(12);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(33);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(sym_float);
      if (lookahead == '_') ADVANCE(11);
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(9);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(34);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(sym_float);
      if (lookahead == '_') ADVANCE(13);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(35);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '%') ADVANCE(50);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(37);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(38);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(38);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(anon_sym_DOLLAR);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(aux_sym_char_token1);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(aux_sym_char_token1);
      if (lookahead == '%') ADVANCE(51);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(41);
      if (lookahead != 0 &&
          lookahead != '\\') ADVANCE(40);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(sym__raw_atom);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(42);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(anon_sym_SQUOTE);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(aux_sym__quoted_atom_token1);
      if (lookahead == '%') ADVANCE(45);
      if ((0 <= lookahead && lookahead <= '\t') ||
          (11 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(44);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(45);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(aux_sym__quoted_atom_token1);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(45);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_SQUOTE2);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(sym__escape);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(sym__escape);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(47);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(sym__escape);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(48);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(sym__comment);
      if (lookahead == '"' ||
          lookahead == '\\') ADVANCE(51);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(50);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(sym__comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(51);
      END_STATE();
    default:
      return false;
  }
}

static bool ts_lex_keywords(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (lookahead == 'm') ADVANCE(1);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(0)
      END_STATE();
    case 1:
      if (lookahead == 'o') ADVANCE(2);
      END_STATE();
    case 2:
      if (lookahead == 'd') ADVANCE(3);
      END_STATE();
    case 3:
      if (lookahead == 'u') ADVANCE(4);
      END_STATE();
    case 4:
      if (lookahead == 'l') ADVANCE(5);
      END_STATE();
    case 5:
      if (lookahead == 'e') ADVANCE(6);
      END_STATE();
    case 6:
      ACCEPT_TOKEN(anon_sym_module);
      END_STATE();
    default:
      return false;
  }
}

static TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 18},
  [2] = {.lex_state = 18},
  [3] = {.lex_state = 18},
  [4] = {.lex_state = 18},
  [5] = {.lex_state = 18},
  [6] = {.lex_state = 18},
  [7] = {.lex_state = 18},
  [8] = {.lex_state = 18},
  [9] = {.lex_state = 18},
  [10] = {.lex_state = 18},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 2},
  [14] = {.lex_state = 2},
  [15] = {.lex_state = 0},
  [16] = {.lex_state = 1},
  [17] = {.lex_state = 18},
  [18] = {.lex_state = 18},
  [19] = {.lex_state = 18},
  [20] = {.lex_state = 18},
  [21] = {.lex_state = 1},
  [22] = {.lex_state = 0},
  [23] = {.lex_state = 18},
  [24] = {.lex_state = 0},
  [25] = {.lex_state = 0},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 0},
  [28] = {.lex_state = 2},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 0},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 1},
  [35] = {.lex_state = 0},
  [36] = {.lex_state = 0},
  [37] = {.lex_state = 0},
  [38] = {.lex_state = 4},
  [39] = {.lex_state = 0},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 0},
  [42] = {.lex_state = 0},
  [43] = {.lex_state = 0},
  [44] = {.lex_state = 0},
  [45] = {.lex_state = 0},
  [46] = {.lex_state = 3},
  [47] = {.lex_state = 0},
  [48] = {.lex_state = 0},
  [49] = {.lex_state = 3},
  [50] = {.lex_state = 3},
  [51] = {.lex_state = 3},
  [52] = {.lex_state = 0},
  [53] = {.lex_state = 0},
};

static uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [sym__raw_atom] = ACTIONS(1),
    [anon_sym_DASH] = ACTIONS(1),
    [anon_sym_module] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_DASH_GT] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [sym_wildcard] = ACTIONS(1),
    [sym_var] = ACTIONS(1),
    [aux_sym_integer_token1] = ACTIONS(1),
    [aux_sym_integer_token2] = ACTIONS(1),
    [sym_float] = ACTIONS(1),
    [anon_sym_DQUOTE] = ACTIONS(1),
    [anon_sym_DOLLAR] = ACTIONS(1),
    [anon_sym_SQUOTE] = ACTIONS(1),
    [anon_sym_SQUOTE2] = ACTIONS(1),
    [sym__escape] = ACTIONS(1),
    [sym__comment] = ACTIONS(3),
  },
  [1] = {
    [sym_source_file] = STATE(48),
    [sym__form] = STATE(7),
    [sym_module_attribute] = STATE(7),
    [sym_attribute] = STATE(7),
    [sym_function] = STATE(7),
    [sym_function_clause] = STATE(35),
    [sym_atom] = STATE(42),
    [sym__quoted_atom] = STATE(11),
    [aux_sym_source_file_repeat1] = STATE(7),
    [ts_builtin_sym_end] = ACTIONS(5),
    [sym__raw_atom] = ACTIONS(7),
    [anon_sym_DASH] = ACTIONS(9),
    [anon_sym_SQUOTE] = ACTIONS(11),
    [sym__comment] = ACTIONS(3),
  },
};

static uint16_t ts_small_parse_table[] = {
  [0] = 12,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(15), 1,
      aux_sym_integer_token1,
    ACTIONS(17), 1,
      aux_sym_integer_token2,
    ACTIONS(19), 1,
      sym_float,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    ACTIONS(23), 1,
      anon_sym_DOLLAR,
    STATE(11), 1,
      sym__quoted_atom,
    STATE(40), 1,
      sym_block,
    ACTIONS(13), 2,
      sym_wildcard,
      sym_var,
    STATE(25), 5,
      sym__expr,
      sym_integer,
      sym_string,
      sym_char,
      sym_atom,
  [42] = 12,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(15), 1,
      aux_sym_integer_token1,
    ACTIONS(17), 1,
      aux_sym_integer_token2,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    ACTIONS(23), 1,
      anon_sym_DOLLAR,
    ACTIONS(25), 1,
      anon_sym_RPAREN,
    ACTIONS(29), 1,
      sym_float,
    STATE(11), 1,
      sym__quoted_atom,
    ACTIONS(27), 2,
      sym_wildcard,
      sym_var,
    STATE(33), 5,
      sym__expr,
      sym_integer,
      sym_string,
      sym_char,
      sym_atom,
  [84] = 11,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(15), 1,
      aux_sym_integer_token1,
    ACTIONS(17), 1,
      aux_sym_integer_token2,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    ACTIONS(23), 1,
      anon_sym_DOLLAR,
    ACTIONS(33), 1,
      sym_float,
    STATE(11), 1,
      sym__quoted_atom,
    ACTIONS(31), 2,
      sym_wildcard,
      sym_var,
    STATE(31), 5,
      sym__expr,
      sym_integer,
      sym_string,
      sym_char,
      sym_atom,
  [123] = 11,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(15), 1,
      aux_sym_integer_token1,
    ACTIONS(17), 1,
      aux_sym_integer_token2,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    ACTIONS(23), 1,
      anon_sym_DOLLAR,
    ACTIONS(37), 1,
      sym_float,
    STATE(11), 1,
      sym__quoted_atom,
    ACTIONS(35), 2,
      sym_wildcard,
      sym_var,
    STATE(41), 5,
      sym__expr,
      sym_integer,
      sym_string,
      sym_char,
      sym_atom,
  [162] = 11,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(15), 1,
      aux_sym_integer_token1,
    ACTIONS(17), 1,
      aux_sym_integer_token2,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    ACTIONS(23), 1,
      anon_sym_DOLLAR,
    ACTIONS(41), 1,
      sym_float,
    STATE(11), 1,
      sym__quoted_atom,
    ACTIONS(39), 2,
      sym_wildcard,
      sym_var,
    STATE(52), 5,
      sym__expr,
      sym_integer,
      sym_string,
      sym_char,
      sym_atom,
  [201] = 9,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(9), 1,
      anon_sym_DASH,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(43), 1,
      ts_builtin_sym_end,
    STATE(11), 1,
      sym__quoted_atom,
    STATE(35), 1,
      sym_function_clause,
    STATE(42), 1,
      sym_atom,
    STATE(8), 5,
      sym__form,
      sym_module_attribute,
      sym_attribute,
      sym_function,
      aux_sym_source_file_repeat1,
  [233] = 9,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(45), 1,
      ts_builtin_sym_end,
    ACTIONS(47), 1,
      sym__raw_atom,
    ACTIONS(50), 1,
      anon_sym_DASH,
    ACTIONS(53), 1,
      anon_sym_SQUOTE,
    STATE(11), 1,
      sym__quoted_atom,
    STATE(35), 1,
      sym_function_clause,
    STATE(42), 1,
      sym_atom,
    STATE(8), 5,
      sym__form,
      sym_module_attribute,
      sym_attribute,
      sym_function,
      aux_sym_source_file_repeat1,
  [265] = 6,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    STATE(11), 1,
      sym__quoted_atom,
    STATE(39), 1,
      sym_function_clause,
    STATE(42), 1,
      sym_atom,
  [284] = 6,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(56), 1,
      sym__raw_atom,
    ACTIONS(58), 1,
      anon_sym_module,
    STATE(11), 1,
      sym__quoted_atom,
    STATE(53), 1,
      sym_atom,
  [303] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(60), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [314] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(62), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [325] = 5,
    ACTIONS(64), 1,
      anon_sym_DQUOTE,
    ACTIONS(66), 1,
      aux_sym_string_token1,
    ACTIONS(68), 1,
      sym__escape,
    ACTIONS(70), 1,
      sym__comment,
    STATE(28), 1,
      aux_sym_string_repeat1,
  [341] = 5,
    ACTIONS(70), 1,
      sym__comment,
    ACTIONS(72), 1,
      anon_sym_DQUOTE,
    ACTIONS(74), 1,
      aux_sym_string_token1,
    ACTIONS(77), 1,
      sym__escape,
    STATE(14), 1,
      aux_sym_string_repeat1,
  [357] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(82), 1,
      anon_sym_COMMA,
    STATE(15), 1,
      aux_sym_block_repeat1,
    ACTIONS(80), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [371] = 4,
    ACTIONS(70), 1,
      sym__comment,
    ACTIONS(87), 1,
      anon_sym_SQUOTE2,
    STATE(21), 1,
      aux_sym__quoted_atom_repeat1,
    ACTIONS(85), 2,
      aux_sym__quoted_atom_token1,
      sym__escape,
  [385] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(89), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [395] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(91), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [405] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(93), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [415] = 5,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    STATE(11), 1,
      sym__quoted_atom,
    STATE(45), 1,
      sym_atom,
  [431] = 4,
    ACTIONS(70), 1,
      sym__comment,
    ACTIONS(98), 1,
      anon_sym_SQUOTE2,
    STATE(21), 1,
      aux_sym__quoted_atom_repeat1,
    ACTIONS(95), 2,
      aux_sym__quoted_atom_token1,
      sym__escape,
  [445] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(102), 1,
      anon_sym_COMMA,
    STATE(15), 1,
      aux_sym_block_repeat1,
    ACTIONS(100), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [459] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(104), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [469] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(106), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [479] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(102), 1,
      anon_sym_COMMA,
    STATE(22), 1,
      aux_sym_block_repeat1,
    ACTIONS(108), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [493] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(110), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [503] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(112), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [513] = 5,
    ACTIONS(70), 1,
      sym__comment,
    ACTIONS(114), 1,
      anon_sym_DQUOTE,
    ACTIONS(116), 1,
      aux_sym_string_token1,
    ACTIONS(118), 1,
      sym__escape,
    STATE(14), 1,
      aux_sym_string_repeat1,
  [529] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(120), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [539] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(122), 1,
      anon_sym_DOT,
    ACTIONS(124), 1,
      anon_sym_SEMI,
    STATE(30), 1,
      aux_sym_function_repeat1,
  [552] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(127), 3,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [561] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(129), 1,
      anon_sym_DOT,
    ACTIONS(131), 1,
      anon_sym_SEMI,
    STATE(30), 1,
      aux_sym_function_repeat1,
  [574] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(133), 1,
      anon_sym_RPAREN,
    ACTIONS(135), 1,
      anon_sym_COMMA,
    STATE(36), 1,
      aux_sym_arg_list_repeat1,
  [587] = 3,
    ACTIONS(70), 1,
      sym__comment,
    STATE(16), 1,
      aux_sym__quoted_atom_repeat1,
    ACTIONS(137), 2,
      aux_sym__quoted_atom_token1,
      sym__escape,
  [598] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(131), 1,
      anon_sym_SEMI,
    ACTIONS(139), 1,
      anon_sym_DOT,
    STATE(32), 1,
      aux_sym_function_repeat1,
  [611] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(135), 1,
      anon_sym_COMMA,
    ACTIONS(141), 1,
      anon_sym_RPAREN,
    STATE(37), 1,
      aux_sym_arg_list_repeat1,
  [624] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(143), 1,
      anon_sym_RPAREN,
    ACTIONS(145), 1,
      anon_sym_COMMA,
    STATE(37), 1,
      aux_sym_arg_list_repeat1,
  [637] = 3,
    ACTIONS(70), 1,
      sym__comment,
    ACTIONS(148), 1,
      aux_sym_char_token1,
    ACTIONS(150), 1,
      sym__escape,
  [647] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(152), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [655] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(154), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [663] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(156), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [671] = 3,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(158), 1,
      anon_sym_LPAREN,
    STATE(50), 1,
      sym_arg_list,
  [681] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(160), 1,
      anon_sym_DOT,
  [688] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(162), 1,
      anon_sym_LPAREN,
  [695] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(164), 1,
      anon_sym_RPAREN,
  [702] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(166), 1,
      anon_sym_DASH_GT,
  [709] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(168), 1,
      anon_sym_DOT,
  [716] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(170), 1,
      ts_builtin_sym_end,
  [723] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(172), 1,
      anon_sym_DASH_GT,
  [730] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(174), 1,
      anon_sym_DASH_GT,
  [737] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(176), 1,
      anon_sym_DASH_GT,
  [744] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(178), 1,
      anon_sym_RPAREN,
  [751] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(180), 1,
      anon_sym_LPAREN,
};

static uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 42,
  [SMALL_STATE(4)] = 84,
  [SMALL_STATE(5)] = 123,
  [SMALL_STATE(6)] = 162,
  [SMALL_STATE(7)] = 201,
  [SMALL_STATE(8)] = 233,
  [SMALL_STATE(9)] = 265,
  [SMALL_STATE(10)] = 284,
  [SMALL_STATE(11)] = 303,
  [SMALL_STATE(12)] = 314,
  [SMALL_STATE(13)] = 325,
  [SMALL_STATE(14)] = 341,
  [SMALL_STATE(15)] = 357,
  [SMALL_STATE(16)] = 371,
  [SMALL_STATE(17)] = 385,
  [SMALL_STATE(18)] = 395,
  [SMALL_STATE(19)] = 405,
  [SMALL_STATE(20)] = 415,
  [SMALL_STATE(21)] = 431,
  [SMALL_STATE(22)] = 445,
  [SMALL_STATE(23)] = 459,
  [SMALL_STATE(24)] = 469,
  [SMALL_STATE(25)] = 479,
  [SMALL_STATE(26)] = 493,
  [SMALL_STATE(27)] = 503,
  [SMALL_STATE(28)] = 513,
  [SMALL_STATE(29)] = 529,
  [SMALL_STATE(30)] = 539,
  [SMALL_STATE(31)] = 552,
  [SMALL_STATE(32)] = 561,
  [SMALL_STATE(33)] = 574,
  [SMALL_STATE(34)] = 587,
  [SMALL_STATE(35)] = 598,
  [SMALL_STATE(36)] = 611,
  [SMALL_STATE(37)] = 624,
  [SMALL_STATE(38)] = 637,
  [SMALL_STATE(39)] = 647,
  [SMALL_STATE(40)] = 655,
  [SMALL_STATE(41)] = 663,
  [SMALL_STATE(42)] = 671,
  [SMALL_STATE(43)] = 681,
  [SMALL_STATE(44)] = 688,
  [SMALL_STATE(45)] = 695,
  [SMALL_STATE(46)] = 702,
  [SMALL_STATE(47)] = 709,
  [SMALL_STATE(48)] = 716,
  [SMALL_STATE(49)] = 723,
  [SMALL_STATE(50)] = 730,
  [SMALL_STATE(51)] = 737,
  [SMALL_STATE(52)] = 744,
  [SMALL_STATE(53)] = 751,
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(25),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [17] = {.entry = {.count = 1, .reusable = false}}, SHIFT(26),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [27] = {.entry = {.count = 1, .reusable = false}}, SHIFT(33),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [31] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [35] = {.entry = {.count = 1, .reusable = false}}, SHIFT(41),
  [37] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [39] = {.entry = {.count = 1, .reusable = false}}, SHIFT(52),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [43] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1, .production_id = 1),
  [45] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [47] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(11),
  [50] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(10),
  [53] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(34),
  [56] = {.entry = {.count = 1, .reusable = false}}, SHIFT(11),
  [58] = {.entry = {.count = 1, .reusable = false}}, SHIFT(44),
  [60] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_atom, 1),
  [62] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__quoted_atom, 3),
  [64] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [66] = {.entry = {.count = 1, .reusable = false}}, SHIFT(28),
  [68] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [70] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [72] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_string_repeat1, 2),
  [74] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(14),
  [77] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(14),
  [80] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_block_repeat1, 2, .production_id = 15),
  [82] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_block_repeat1, 2, .production_id = 15), SHIFT_REPEAT(4),
  [85] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [87] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [89] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 2, .production_id = 2),
  [91] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_attribute, 6, .production_id = 13),
  [93] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_attribute, 6, .production_id = 12),
  [95] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__quoted_atom_repeat1, 2), SHIFT_REPEAT(21),
  [98] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__quoted_atom_repeat1, 2),
  [100] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_block, 2, .production_id = 11),
  [102] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [104] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 3, .production_id = 4),
  [106] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3),
  [108] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_block, 1, .production_id = 8),
  [110] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_integer, 1),
  [112] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_char, 2),
  [114] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [116] = {.entry = {.count = 1, .reusable = false}}, SHIFT(14),
  [118] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [120] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 2),
  [122] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2, .production_id = 5),
  [124] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2, .production_id = 5), SHIFT_REPEAT(9),
  [127] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_block_repeat1, 2, .production_id = 14),
  [129] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [131] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [133] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [135] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [137] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [139] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [141] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [143] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_arg_list_repeat1, 2, .production_id = 10),
  [145] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_arg_list_repeat1, 2, .production_id = 10), SHIFT_REPEAT(5),
  [148] = {.entry = {.count = 1, .reusable = false}}, SHIFT(27),
  [150] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [152] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2, .production_id = 3),
  [154] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause, 4, .production_id = 7),
  [156] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_arg_list_repeat1, 2, .production_id = 6),
  [158] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [160] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [162] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [164] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [166] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 4, .production_id = 9),
  [168] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [170] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [172] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 2),
  [174] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [176] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 3, .production_id = 6),
  [178] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [180] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_erlang_elp(void) {
  static TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .symbol_metadata = ts_symbol_metadata,
    .parse_table = (const unsigned short *)ts_parse_table,
    .small_parse_table = (const uint16_t *)ts_small_parse_table,
    .small_parse_table_map = (const uint32_t *)ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .lex_modes = ts_lex_modes,
    .symbol_names = ts_symbol_names,
    .public_symbol_map = ts_symbol_map,
    .alias_sequences = (const TSSymbol *)ts_alias_sequences,
    .field_count = FIELD_COUNT,
    .field_names = ts_field_names,
    .field_map_slices = (const TSFieldMapSlice *)ts_field_map_slices,
    .field_map_entries = (const TSFieldMapEntry *)ts_field_map_entries,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .lex_fn = ts_lex,
    .keyword_lex_fn = ts_lex_keywords,
    .keyword_capture_token = sym__raw_atom,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
