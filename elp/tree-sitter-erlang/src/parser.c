#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 12
#define STATE_COUNT 51
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 43
#define ALIAS_COUNT 0
#define TOKEN_COUNT 24
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 0
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
  aux_sym_string_repeat1 = 41,
  aux_sym__quoted_atom_repeat1 = 42,
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
  [aux_sym_string_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym__quoted_atom_repeat1] = {
    .visible = false,
    .named = false,
  },
};

static TSSymbol ts_alias_sequences[1][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static uint16_t ts_non_terminal_alias_map[] = {
  0,
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
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 0},
  [15] = {.lex_state = 18},
  [16] = {.lex_state = 1},
  [17] = {.lex_state = 18},
  [18] = {.lex_state = 0},
  [19] = {.lex_state = 18},
  [20] = {.lex_state = 18},
  [21] = {.lex_state = 1},
  [22] = {.lex_state = 2},
  [23] = {.lex_state = 18},
  [24] = {.lex_state = 0},
  [25] = {.lex_state = 0},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 2},
  [28] = {.lex_state = 2},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 1},
  [31] = {.lex_state = 0},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 0},
  [35] = {.lex_state = 0},
  [36] = {.lex_state = 4},
  [37] = {.lex_state = 0},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 0},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 0},
  [42] = {.lex_state = 0},
  [43] = {.lex_state = 3},
  [44] = {.lex_state = 0},
  [45] = {.lex_state = 0},
  [46] = {.lex_state = 3},
  [47] = {.lex_state = 0},
  [48] = {.lex_state = 0},
  [49] = {.lex_state = 3},
  [50] = {.lex_state = 3},
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
    [sym_source_file] = STATE(47),
    [sym__form] = STATE(6),
    [sym_module_attribute] = STATE(6),
    [sym_attribute] = STATE(6),
    [sym_function] = STATE(6),
    [sym_function_clause] = STATE(33),
    [sym_atom] = STATE(38),
    [sym__quoted_atom] = STATE(10),
    [aux_sym_source_file_repeat1] = STATE(6),
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
    STATE(10), 1,
      sym__quoted_atom,
    STATE(39), 1,
      sym_block,
    ACTIONS(13), 2,
      sym_wildcard,
      sym_var,
    STATE(18), 5,
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
    STATE(10), 1,
      sym__quoted_atom,
    ACTIONS(27), 2,
      sym_wildcard,
      sym_var,
    STATE(31), 5,
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
    STATE(10), 1,
      sym__quoted_atom,
    ACTIONS(31), 2,
      sym_wildcard,
      sym_var,
    STATE(29), 5,
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
    STATE(10), 1,
      sym__quoted_atom,
    ACTIONS(35), 2,
      sym_wildcard,
      sym_var,
    STATE(40), 5,
      sym__expr,
      sym_integer,
      sym_string,
      sym_char,
      sym_atom,
  [162] = 9,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(9), 1,
      anon_sym_DASH,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(39), 1,
      ts_builtin_sym_end,
    STATE(10), 1,
      sym__quoted_atom,
    STATE(33), 1,
      sym_function_clause,
    STATE(38), 1,
      sym_atom,
    STATE(7), 5,
      sym__form,
      sym_module_attribute,
      sym_attribute,
      sym_function,
      aux_sym_source_file_repeat1,
  [194] = 9,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(41), 1,
      ts_builtin_sym_end,
    ACTIONS(43), 1,
      sym__raw_atom,
    ACTIONS(46), 1,
      anon_sym_DASH,
    ACTIONS(49), 1,
      anon_sym_SQUOTE,
    STATE(10), 1,
      sym__quoted_atom,
    STATE(33), 1,
      sym_function_clause,
    STATE(38), 1,
      sym_atom,
    STATE(7), 5,
      sym__form,
      sym_module_attribute,
      sym_attribute,
      sym_function,
      aux_sym_source_file_repeat1,
  [226] = 6,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    STATE(10), 1,
      sym__quoted_atom,
    STATE(37), 1,
      sym_function_clause,
    STATE(38), 1,
      sym_atom,
  [245] = 6,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(52), 1,
      sym__raw_atom,
    ACTIONS(54), 1,
      anon_sym_module,
    STATE(10), 1,
      sym__quoted_atom,
    STATE(44), 1,
      sym_atom,
  [264] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(56), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [275] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(60), 1,
      anon_sym_COMMA,
    STATE(11), 1,
      aux_sym_arg_list_repeat1,
    ACTIONS(58), 3,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
  [290] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(63), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [301] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(65), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [311] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(67), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [321] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(69), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [331] = 4,
    ACTIONS(73), 1,
      anon_sym_SQUOTE2,
    ACTIONS(75), 1,
      sym__comment,
    STATE(21), 1,
      aux_sym__quoted_atom_repeat1,
    ACTIONS(71), 2,
      aux_sym__quoted_atom_token1,
      sym__escape,
  [345] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(77), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [355] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(81), 1,
      anon_sym_COMMA,
    STATE(25), 1,
      aux_sym_arg_list_repeat1,
    ACTIONS(79), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [369] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(83), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [379] = 5,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    STATE(10), 1,
      sym__quoted_atom,
    STATE(48), 1,
      sym_atom,
  [395] = 4,
    ACTIONS(75), 1,
      sym__comment,
    ACTIONS(88), 1,
      anon_sym_SQUOTE2,
    STATE(21), 1,
      aux_sym__quoted_atom_repeat1,
    ACTIONS(85), 2,
      aux_sym__quoted_atom_token1,
      sym__escape,
  [409] = 5,
    ACTIONS(75), 1,
      sym__comment,
    ACTIONS(90), 1,
      anon_sym_DQUOTE,
    ACTIONS(92), 1,
      aux_sym_string_token1,
    ACTIONS(94), 1,
      sym__escape,
    STATE(28), 1,
      aux_sym_string_repeat1,
  [425] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(96), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [435] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(98), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [445] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(81), 1,
      anon_sym_COMMA,
    STATE(11), 1,
      aux_sym_arg_list_repeat1,
    ACTIONS(100), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [459] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(102), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [469] = 5,
    ACTIONS(75), 1,
      sym__comment,
    ACTIONS(104), 1,
      anon_sym_DQUOTE,
    ACTIONS(106), 1,
      aux_sym_string_token1,
    ACTIONS(108), 1,
      sym__escape,
    STATE(22), 1,
      aux_sym_string_repeat1,
  [485] = 5,
    ACTIONS(75), 1,
      sym__comment,
    ACTIONS(110), 1,
      anon_sym_DQUOTE,
    ACTIONS(112), 1,
      aux_sym_string_token1,
    ACTIONS(115), 1,
      sym__escape,
    STATE(28), 1,
      aux_sym_string_repeat1,
  [501] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(58), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [511] = 3,
    ACTIONS(75), 1,
      sym__comment,
    STATE(16), 1,
      aux_sym__quoted_atom_repeat1,
    ACTIONS(118), 2,
      aux_sym__quoted_atom_token1,
      sym__escape,
  [522] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(81), 1,
      anon_sym_COMMA,
    ACTIONS(120), 1,
      anon_sym_RPAREN,
    STATE(34), 1,
      aux_sym_arg_list_repeat1,
  [535] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(122), 1,
      anon_sym_DOT,
    ACTIONS(124), 1,
      anon_sym_SEMI,
    STATE(32), 1,
      aux_sym_function_repeat1,
  [548] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(127), 1,
      anon_sym_DOT,
    ACTIONS(129), 1,
      anon_sym_SEMI,
    STATE(35), 1,
      aux_sym_function_repeat1,
  [561] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(81), 1,
      anon_sym_COMMA,
    ACTIONS(131), 1,
      anon_sym_RPAREN,
    STATE(11), 1,
      aux_sym_arg_list_repeat1,
  [574] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(129), 1,
      anon_sym_SEMI,
    ACTIONS(133), 1,
      anon_sym_DOT,
    STATE(32), 1,
      aux_sym_function_repeat1,
  [587] = 3,
    ACTIONS(75), 1,
      sym__comment,
    ACTIONS(135), 1,
      aux_sym_char_token1,
    ACTIONS(137), 1,
      sym__escape,
  [597] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(122), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [605] = 3,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(139), 1,
      anon_sym_LPAREN,
    STATE(50), 1,
      sym_arg_list,
  [615] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(141), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [623] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(143), 1,
      anon_sym_RPAREN,
  [630] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(145), 1,
      anon_sym_DOT,
  [637] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(147), 1,
      anon_sym_DOT,
  [644] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(149), 1,
      anon_sym_DASH_GT,
  [651] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(151), 1,
      anon_sym_LPAREN,
  [658] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
  [665] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(155), 1,
      anon_sym_DASH_GT,
  [672] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(157), 1,
      ts_builtin_sym_end,
  [679] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(159), 1,
      anon_sym_RPAREN,
  [686] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(161), 1,
      anon_sym_DASH_GT,
  [693] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(163), 1,
      anon_sym_DASH_GT,
};

static uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 42,
  [SMALL_STATE(4)] = 84,
  [SMALL_STATE(5)] = 123,
  [SMALL_STATE(6)] = 162,
  [SMALL_STATE(7)] = 194,
  [SMALL_STATE(8)] = 226,
  [SMALL_STATE(9)] = 245,
  [SMALL_STATE(10)] = 264,
  [SMALL_STATE(11)] = 275,
  [SMALL_STATE(12)] = 290,
  [SMALL_STATE(13)] = 301,
  [SMALL_STATE(14)] = 311,
  [SMALL_STATE(15)] = 321,
  [SMALL_STATE(16)] = 331,
  [SMALL_STATE(17)] = 345,
  [SMALL_STATE(18)] = 355,
  [SMALL_STATE(19)] = 369,
  [SMALL_STATE(20)] = 379,
  [SMALL_STATE(21)] = 395,
  [SMALL_STATE(22)] = 409,
  [SMALL_STATE(23)] = 425,
  [SMALL_STATE(24)] = 435,
  [SMALL_STATE(25)] = 445,
  [SMALL_STATE(26)] = 459,
  [SMALL_STATE(27)] = 469,
  [SMALL_STATE(28)] = 485,
  [SMALL_STATE(29)] = 501,
  [SMALL_STATE(30)] = 511,
  [SMALL_STATE(31)] = 522,
  [SMALL_STATE(32)] = 535,
  [SMALL_STATE(33)] = 548,
  [SMALL_STATE(34)] = 561,
  [SMALL_STATE(35)] = 574,
  [SMALL_STATE(36)] = 587,
  [SMALL_STATE(37)] = 597,
  [SMALL_STATE(38)] = 605,
  [SMALL_STATE(39)] = 615,
  [SMALL_STATE(40)] = 623,
  [SMALL_STATE(41)] = 630,
  [SMALL_STATE(42)] = 637,
  [SMALL_STATE(43)] = 644,
  [SMALL_STATE(44)] = 651,
  [SMALL_STATE(45)] = 658,
  [SMALL_STATE(46)] = 665,
  [SMALL_STATE(47)] = 672,
  [SMALL_STATE(48)] = 679,
  [SMALL_STATE(49)] = 686,
  [SMALL_STATE(50)] = 693,
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(18),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [17] = {.entry = {.count = 1, .reusable = false}}, SHIFT(14),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [27] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [31] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [35] = {.entry = {.count = 1, .reusable = false}}, SHIFT(40),
  [37] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [41] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [43] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(10),
  [46] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(9),
  [49] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(30),
  [52] = {.entry = {.count = 1, .reusable = false}}, SHIFT(10),
  [54] = {.entry = {.count = 1, .reusable = false}}, SHIFT(45),
  [56] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_atom, 1),
  [58] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_arg_list_repeat1, 2),
  [60] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_arg_list_repeat1, 2), SHIFT_REPEAT(4),
  [63] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__quoted_atom, 3),
  [65] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_char, 2),
  [67] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_integer, 1),
  [69] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_attribute, 6),
  [71] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [73] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [75] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [77] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 2),
  [79] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_block, 1),
  [81] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [83] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_attribute, 6),
  [85] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__quoted_atom_repeat1, 2), SHIFT_REPEAT(21),
  [88] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__quoted_atom_repeat1, 2),
  [90] = {.entry = {.count = 1, .reusable = false}}, SHIFT(26),
  [92] = {.entry = {.count = 1, .reusable = false}}, SHIFT(28),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [96] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 3),
  [98] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 2),
  [100] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_block, 2),
  [102] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3),
  [104] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [106] = {.entry = {.count = 1, .reusable = false}}, SHIFT(22),
  [108] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [110] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_string_repeat1, 2),
  [112] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(28),
  [115] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(28),
  [118] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [120] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [122] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2),
  [124] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2), SHIFT_REPEAT(8),
  [127] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [129] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [131] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [133] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [135] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [137] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [139] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [141] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause, 4),
  [143] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [145] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [147] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [149] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 3),
  [151] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [153] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [155] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 4),
  [157] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [159] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [161] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 2),
  [163] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
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
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .symbol_names = ts_symbol_names,
    .symbol_metadata = ts_symbol_metadata,
    .parse_table = (const uint16_t *)ts_parse_table,
    .parse_actions = ts_parse_actions,
    .lex_modes = ts_lex_modes,
    .alias_sequences = (const TSSymbol *)ts_alias_sequences,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .lex_fn = ts_lex,
    .keyword_lex_fn = ts_lex_keywords,
    .keyword_capture_token = sym__raw_atom,
    .field_count = FIELD_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .small_parse_table = (const uint16_t *)ts_small_parse_table,
    .small_parse_table_map = (const uint32_t *)ts_small_parse_table_map,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .state_count = STATE_COUNT,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
