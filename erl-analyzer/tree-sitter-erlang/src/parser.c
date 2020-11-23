#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 12
#define STATE_COUNT 52
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 41
#define ALIAS_COUNT 0
#define TOKEN_COUNT 22
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
  anon_sym_DQUOTE = 14,
  aux_sym_string_token1 = 15,
  anon_sym_DOLLAR = 16,
  aux_sym_char_token1 = 17,
  anon_sym_SQUOTE = 18,
  aux_sym__quoted_atom_token1 = 19,
  sym__escape = 20,
  sym__comment = 21,
  sym_source_file = 22,
  sym__form = 23,
  sym_module_attribute = 24,
  sym_attribute = 25,
  sym_function = 26,
  sym_function_clause = 27,
  sym_arg_list = 28,
  sym_block = 29,
  sym__expr = 30,
  sym_integer = 31,
  sym_string = 32,
  sym_char = 33,
  sym_atom = 34,
  sym__quoted_atom = 35,
  aux_sym_source_file_repeat1 = 36,
  aux_sym_function_repeat1 = 37,
  aux_sym_arg_list_repeat1 = 38,
  aux_sym_string_repeat1 = 39,
  aux_sym__quoted_atom_repeat1 = 40,
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
  [anon_sym_DQUOTE] = "\"",
  [aux_sym_string_token1] = "string_token1",
  [anon_sym_DOLLAR] = "$",
  [aux_sym_char_token1] = "char_token1",
  [anon_sym_SQUOTE] = "'",
  [aux_sym__quoted_atom_token1] = "_quoted_atom_token1",
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
  [anon_sym_DQUOTE] = anon_sym_DQUOTE,
  [aux_sym_string_token1] = aux_sym_string_token1,
  [anon_sym_DOLLAR] = anon_sym_DOLLAR,
  [aux_sym_char_token1] = aux_sym_char_token1,
  [anon_sym_SQUOTE] = anon_sym_SQUOTE,
  [aux_sym__quoted_atom_token1] = aux_sym__quoted_atom_token1,
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
      if (eof) ADVANCE(14);
      if (lookahead == '"') ADVANCE(29);
      if (lookahead == '$') ADVANCE(33);
      if (lookahead == '%') ADVANCE(45);
      if (lookahead == '\'') ADVANCE(38);
      if (lookahead == '(') ADVANCE(17);
      if (lookahead == ')') ADVANCE(18);
      if (lookahead == ',') ADVANCE(22);
      if (lookahead == '-') ADVANCE(16);
      if (lookahead == '.') ADVANCE(19);
      if (lookahead == ';') ADVANCE(20);
      if (lookahead == '\\') ADVANCE(8);
      if (lookahead == '_') ADVANCE(23);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(27);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 222)) ADVANCE(24);
      if (('a' <= lookahead && lookahead <= 'z') ||
          (223 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(37);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(0)
      END_STATE();
    case 1:
      if (lookahead == '"') ADVANCE(29);
      if (lookahead == '%') ADVANCE(30);
      if (lookahead == '\\') ADVANCE(8);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(31);
      if (lookahead != 0) ADVANCE(32);
      END_STATE();
    case 2:
      if (lookahead == '%') ADVANCE(45);
      if (lookahead == '-') ADVANCE(5);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(2)
      END_STATE();
    case 3:
      if (lookahead == '%') ADVANCE(39);
      if (lookahead == '\'') ADVANCE(38);
      if (lookahead == '\\') ADVANCE(8);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(40);
      if (lookahead != 0) ADVANCE(41);
      END_STATE();
    case 4:
      if (lookahead == '%') ADVANCE(36);
      if (lookahead == '\\') ADVANCE(8);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(35);
      if (lookahead != 0) ADVANCE(34);
      END_STATE();
    case 5:
      if (lookahead == '>') ADVANCE(21);
      END_STATE();
    case 6:
      if (lookahead == '{') ADVANCE(11);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(10);
      END_STATE();
    case 7:
      if (lookahead == '}') ADVANCE(42);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(7);
      END_STATE();
    case 8:
      if (lookahead == '\n' ||
          lookahead == 'b' ||
          ('d' <= lookahead && lookahead <= 'f') ||
          lookahead == 'n' ||
          ('r' <= lookahead && lookahead <= 't') ||
          lookahead == 'v') ADVANCE(42);
      if (lookahead == 'x') ADVANCE(6);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(44);
      END_STATE();
    case 9:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(28);
      END_STATE();
    case 10:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(42);
      END_STATE();
    case 11:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(7);
      END_STATE();
    case 12:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(25);
      END_STATE();
    case 13:
      if (eof) ADVANCE(14);
      if (lookahead == '%') ADVANCE(45);
      if (lookahead == '\'') ADVANCE(38);
      if (lookahead == '-') ADVANCE(15);
      if (('a' <= lookahead && lookahead <= 'z') ||
          (223 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(37);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(13)
      END_STATE();
    case 14:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(anon_sym_DASH);
      if (lookahead == '>') ADVANCE(21);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(anon_sym_DASH_GT);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(sym_wildcard);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(24);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(sym_var);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(24);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(aux_sym_integer_token1);
      if (lookahead == '_') ADVANCE(12);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(25);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(aux_sym_integer_token2);
      if (lookahead == '#') ADVANCE(12);
      if (lookahead == '_') ADVANCE(9);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(28);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(aux_sym_integer_token2);
      if (lookahead == '#') ADVANCE(12);
      if (lookahead == '_') ADVANCE(9);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(aux_sym_integer_token2);
      if (lookahead == '_') ADVANCE(9);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(28);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '\n') ADVANCE(32);
      if (lookahead == '"' ||
          lookahead == '\\') ADVANCE(45);
      if (lookahead != 0) ADVANCE(30);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '%') ADVANCE(30);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(31);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(32);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(32);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(anon_sym_DOLLAR);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(aux_sym_char_token1);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(aux_sym_char_token1);
      if (lookahead == '%') ADVANCE(36);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(35);
      if (lookahead != 0 &&
          lookahead != '\\') ADVANCE(34);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(aux_sym_char_token1);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(45);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(sym__raw_atom);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(37);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_SQUOTE);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(aux_sym__quoted_atom_token1);
      if (lookahead == '\n') ADVANCE(41);
      if (lookahead == '\'' ||
          lookahead == '\\') ADVANCE(45);
      if (lookahead != 0) ADVANCE(39);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(aux_sym__quoted_atom_token1);
      if (lookahead == '%') ADVANCE(39);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(40);
      if (lookahead != 0 &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(41);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(aux_sym__quoted_atom_token1);
      if (lookahead != 0 &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(41);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(sym__escape);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(sym__escape);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(42);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(sym__escape);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(43);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(sym__comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(45);
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
  [1] = {.lex_state = 13},
  [2] = {.lex_state = 0},
  [3] = {.lex_state = 0},
  [4] = {.lex_state = 0},
  [5] = {.lex_state = 0},
  [6] = {.lex_state = 13},
  [7] = {.lex_state = 13},
  [8] = {.lex_state = 0},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 0},
  [15] = {.lex_state = 3},
  [16] = {.lex_state = 13},
  [17] = {.lex_state = 1},
  [18] = {.lex_state = 0},
  [19] = {.lex_state = 0},
  [20] = {.lex_state = 13},
  [21] = {.lex_state = 0},
  [22] = {.lex_state = 3},
  [23] = {.lex_state = 0},
  [24] = {.lex_state = 13},
  [25] = {.lex_state = 13},
  [26] = {.lex_state = 3},
  [27] = {.lex_state = 0},
  [28] = {.lex_state = 1},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 1},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 0},
  [35] = {.lex_state = 0},
  [36] = {.lex_state = 0},
  [37] = {.lex_state = 4},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 0},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 0},
  [42] = {.lex_state = 0},
  [43] = {.lex_state = 2},
  [44] = {.lex_state = 0},
  [45] = {.lex_state = 2},
  [46] = {.lex_state = 0},
  [47] = {.lex_state = 0},
  [48] = {.lex_state = 2},
  [49] = {.lex_state = 2},
  [50] = {.lex_state = 0},
  [51] = {.lex_state = 0},
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
    [anon_sym_DQUOTE] = ACTIONS(1),
    [anon_sym_DOLLAR] = ACTIONS(1),
    [anon_sym_SQUOTE] = ACTIONS(1),
    [sym__escape] = ACTIONS(1),
    [sym__comment] = ACTIONS(3),
  },
  [1] = {
    [sym_source_file] = STATE(44),
    [sym__form] = STATE(6),
    [sym_module_attribute] = STATE(6),
    [sym_attribute] = STATE(6),
    [sym_function] = STATE(6),
    [sym_function_clause] = STATE(36),
    [sym_atom] = STATE(40),
    [sym__quoted_atom] = STATE(9),
    [aux_sym_source_file_repeat1] = STATE(6),
    [ts_builtin_sym_end] = ACTIONS(5),
    [sym__raw_atom] = ACTIONS(7),
    [anon_sym_DASH] = ACTIONS(9),
    [anon_sym_SQUOTE] = ACTIONS(11),
    [sym__comment] = ACTIONS(3),
  },
};

static uint16_t ts_small_parse_table[] = {
  [0] = 11,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_RPAREN,
    ACTIONS(17), 1,
      aux_sym_integer_token1,
    ACTIONS(19), 1,
      aux_sym_integer_token2,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    ACTIONS(23), 1,
      anon_sym_DOLLAR,
    STATE(9), 1,
      sym__quoted_atom,
    ACTIONS(15), 2,
      sym_wildcard,
      sym_var,
    STATE(33), 5,
      sym__expr,
      sym_integer,
      sym_string,
      sym_char,
      sym_atom,
  [39] = 11,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(17), 1,
      aux_sym_integer_token1,
    ACTIONS(19), 1,
      aux_sym_integer_token2,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    ACTIONS(23), 1,
      anon_sym_DOLLAR,
    STATE(9), 1,
      sym__quoted_atom,
    STATE(38), 1,
      sym_block,
    ACTIONS(25), 2,
      sym_wildcard,
      sym_var,
    STATE(23), 5,
      sym__expr,
      sym_integer,
      sym_string,
      sym_char,
      sym_atom,
  [78] = 10,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(17), 1,
      aux_sym_integer_token1,
    ACTIONS(19), 1,
      aux_sym_integer_token2,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    ACTIONS(23), 1,
      anon_sym_DOLLAR,
    STATE(9), 1,
      sym__quoted_atom,
    ACTIONS(27), 2,
      sym_wildcard,
      sym_var,
    STATE(29), 5,
      sym__expr,
      sym_integer,
      sym_string,
      sym_char,
      sym_atom,
  [114] = 10,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(17), 1,
      aux_sym_integer_token1,
    ACTIONS(19), 1,
      aux_sym_integer_token2,
    ACTIONS(21), 1,
      anon_sym_DQUOTE,
    ACTIONS(23), 1,
      anon_sym_DOLLAR,
    STATE(9), 1,
      sym__quoted_atom,
    ACTIONS(29), 2,
      sym_wildcard,
      sym_var,
    STATE(41), 5,
      sym__expr,
      sym_integer,
      sym_string,
      sym_char,
      sym_atom,
  [150] = 9,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(9), 1,
      anon_sym_DASH,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(31), 1,
      ts_builtin_sym_end,
    STATE(9), 1,
      sym__quoted_atom,
    STATE(36), 1,
      sym_function_clause,
    STATE(40), 1,
      sym_atom,
    STATE(7), 5,
      sym__form,
      sym_module_attribute,
      sym_attribute,
      sym_function,
      aux_sym_source_file_repeat1,
  [182] = 9,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(33), 1,
      ts_builtin_sym_end,
    ACTIONS(35), 1,
      sym__raw_atom,
    ACTIONS(38), 1,
      anon_sym_DASH,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    STATE(9), 1,
      sym__quoted_atom,
    STATE(36), 1,
      sym_function_clause,
    STATE(40), 1,
      sym_atom,
    STATE(7), 5,
      sym__form,
      sym_module_attribute,
      sym_attribute,
      sym_function,
      aux_sym_source_file_repeat1,
  [214] = 6,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    STATE(9), 1,
      sym__quoted_atom,
    STATE(39), 1,
      sym_function_clause,
    STATE(40), 1,
      sym_atom,
  [233] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(44), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [244] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(48), 1,
      anon_sym_COMMA,
    STATE(10), 1,
      aux_sym_arg_list_repeat1,
    ACTIONS(46), 3,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
  [259] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(51), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [270] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(53), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [281] = 6,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(55), 1,
      sym__raw_atom,
    ACTIONS(57), 1,
      anon_sym_module,
    STATE(9), 1,
      sym__quoted_atom,
    STATE(50), 1,
      sym_atom,
  [300] = 5,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    STATE(9), 1,
      sym__quoted_atom,
    STATE(42), 1,
      sym_atom,
  [316] = 4,
    ACTIONS(59), 1,
      anon_sym_SQUOTE,
    ACTIONS(63), 1,
      sym__comment,
    STATE(22), 1,
      aux_sym__quoted_atom_repeat1,
    ACTIONS(61), 2,
      aux_sym__quoted_atom_token1,
      sym__escape,
  [330] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(65), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [340] = 4,
    ACTIONS(63), 1,
      sym__comment,
    ACTIONS(67), 1,
      anon_sym_DQUOTE,
    STATE(17), 1,
      aux_sym_string_repeat1,
    ACTIONS(69), 2,
      aux_sym_string_token1,
      sym__escape,
  [354] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(72), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [364] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(76), 1,
      anon_sym_COMMA,
    STATE(10), 1,
      aux_sym_arg_list_repeat1,
    ACTIONS(74), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [378] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(78), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [388] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(80), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [398] = 4,
    ACTIONS(63), 1,
      sym__comment,
    ACTIONS(82), 1,
      anon_sym_SQUOTE,
    STATE(22), 1,
      aux_sym__quoted_atom_repeat1,
    ACTIONS(84), 2,
      aux_sym__quoted_atom_token1,
      sym__escape,
  [412] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(76), 1,
      anon_sym_COMMA,
    STATE(19), 1,
      aux_sym_arg_list_repeat1,
    ACTIONS(87), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [426] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(89), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [436] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(91), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [446] = 4,
    ACTIONS(63), 1,
      sym__comment,
    ACTIONS(93), 1,
      anon_sym_SQUOTE,
    STATE(15), 1,
      aux_sym__quoted_atom_repeat1,
    ACTIONS(95), 2,
      aux_sym__quoted_atom_token1,
      sym__escape,
  [460] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(97), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [470] = 4,
    ACTIONS(63), 1,
      sym__comment,
    ACTIONS(99), 1,
      anon_sym_DQUOTE,
    STATE(31), 1,
      aux_sym_string_repeat1,
    ACTIONS(101), 2,
      aux_sym_string_token1,
      sym__escape,
  [484] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(46), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [494] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(103), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [504] = 4,
    ACTIONS(63), 1,
      sym__comment,
    ACTIONS(105), 1,
      anon_sym_DQUOTE,
    STATE(17), 1,
      aux_sym_string_repeat1,
    ACTIONS(107), 2,
      aux_sym_string_token1,
      sym__escape,
  [518] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(109), 1,
      anon_sym_DOT,
    ACTIONS(111), 1,
      anon_sym_SEMI,
    STATE(32), 1,
      aux_sym_function_repeat1,
  [531] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(76), 1,
      anon_sym_COMMA,
    ACTIONS(114), 1,
      anon_sym_RPAREN,
    STATE(34), 1,
      aux_sym_arg_list_repeat1,
  [544] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(76), 1,
      anon_sym_COMMA,
    ACTIONS(116), 1,
      anon_sym_RPAREN,
    STATE(10), 1,
      aux_sym_arg_list_repeat1,
  [557] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(118), 1,
      anon_sym_DOT,
    ACTIONS(120), 1,
      anon_sym_SEMI,
    STATE(32), 1,
      aux_sym_function_repeat1,
  [570] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(120), 1,
      anon_sym_SEMI,
    ACTIONS(122), 1,
      anon_sym_DOT,
    STATE(35), 1,
      aux_sym_function_repeat1,
  [583] = 2,
    ACTIONS(63), 1,
      sym__comment,
    ACTIONS(124), 2,
      aux_sym_char_token1,
      sym__escape,
  [591] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(126), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [599] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(109), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [607] = 3,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(128), 1,
      anon_sym_LPAREN,
    STATE(48), 1,
      sym_arg_list,
  [617] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(130), 1,
      anon_sym_RPAREN,
  [624] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(132), 1,
      anon_sym_RPAREN,
  [631] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(134), 1,
      anon_sym_DASH_GT,
  [638] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(136), 1,
      ts_builtin_sym_end,
  [645] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(138), 1,
      anon_sym_DASH_GT,
  [652] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(140), 1,
      anon_sym_DOT,
  [659] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(142), 1,
      anon_sym_DOT,
  [666] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(144), 1,
      anon_sym_DASH_GT,
  [673] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(146), 1,
      anon_sym_DASH_GT,
  [680] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(148), 1,
      anon_sym_LPAREN,
  [687] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(150), 1,
      anon_sym_LPAREN,
};

static uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 39,
  [SMALL_STATE(4)] = 78,
  [SMALL_STATE(5)] = 114,
  [SMALL_STATE(6)] = 150,
  [SMALL_STATE(7)] = 182,
  [SMALL_STATE(8)] = 214,
  [SMALL_STATE(9)] = 233,
  [SMALL_STATE(10)] = 244,
  [SMALL_STATE(11)] = 259,
  [SMALL_STATE(12)] = 270,
  [SMALL_STATE(13)] = 281,
  [SMALL_STATE(14)] = 300,
  [SMALL_STATE(15)] = 316,
  [SMALL_STATE(16)] = 330,
  [SMALL_STATE(17)] = 340,
  [SMALL_STATE(18)] = 354,
  [SMALL_STATE(19)] = 364,
  [SMALL_STATE(20)] = 378,
  [SMALL_STATE(21)] = 388,
  [SMALL_STATE(22)] = 398,
  [SMALL_STATE(23)] = 412,
  [SMALL_STATE(24)] = 426,
  [SMALL_STATE(25)] = 436,
  [SMALL_STATE(26)] = 446,
  [SMALL_STATE(27)] = 460,
  [SMALL_STATE(28)] = 470,
  [SMALL_STATE(29)] = 484,
  [SMALL_STATE(30)] = 494,
  [SMALL_STATE(31)] = 504,
  [SMALL_STATE(32)] = 518,
  [SMALL_STATE(33)] = 531,
  [SMALL_STATE(34)] = 544,
  [SMALL_STATE(35)] = 557,
  [SMALL_STATE(36)] = 570,
  [SMALL_STATE(37)] = 583,
  [SMALL_STATE(38)] = 591,
  [SMALL_STATE(39)] = 599,
  [SMALL_STATE(40)] = 607,
  [SMALL_STATE(41)] = 617,
  [SMALL_STATE(42)] = 624,
  [SMALL_STATE(43)] = 631,
  [SMALL_STATE(44)] = 638,
  [SMALL_STATE(45)] = 645,
  [SMALL_STATE(46)] = 652,
  [SMALL_STATE(47)] = 659,
  [SMALL_STATE(48)] = 666,
  [SMALL_STATE(49)] = 673,
  [SMALL_STATE(50)] = 680,
  [SMALL_STATE(51)] = 687,
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [15] = {.entry = {.count = 1, .reusable = false}}, SHIFT(33),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [19] = {.entry = {.count = 1, .reusable = false}}, SHIFT(27),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [25] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [27] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [29] = {.entry = {.count = 1, .reusable = false}}, SHIFT(41),
  [31] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [33] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [35] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(9),
  [38] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(13),
  [41] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(26),
  [44] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_atom, 1),
  [46] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_arg_list_repeat1, 2),
  [48] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_arg_list_repeat1, 2), SHIFT_REPEAT(4),
  [51] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__quoted_atom, 3),
  [53] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__quoted_atom, 2),
  [55] = {.entry = {.count = 1, .reusable = false}}, SHIFT(9),
  [57] = {.entry = {.count = 1, .reusable = false}}, SHIFT(51),
  [59] = {.entry = {.count = 1, .reusable = false}}, SHIFT(11),
  [61] = {.entry = {.count = 1, .reusable = false}}, SHIFT(22),
  [63] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [65] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 2),
  [67] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_string_repeat1, 2),
  [69] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(17),
  [72] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3),
  [74] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_block, 2),
  [76] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [78] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_attribute, 6),
  [80] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_char, 2),
  [82] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym__quoted_atom_repeat1, 2),
  [84] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__quoted_atom_repeat1, 2), SHIFT_REPEAT(22),
  [87] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_block, 1),
  [89] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 3),
  [91] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_attribute, 6),
  [93] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [95] = {.entry = {.count = 1, .reusable = false}}, SHIFT(15),
  [97] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_integer, 1),
  [99] = {.entry = {.count = 1, .reusable = false}}, SHIFT(30),
  [101] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [103] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 2),
  [105] = {.entry = {.count = 1, .reusable = false}}, SHIFT(18),
  [107] = {.entry = {.count = 1, .reusable = false}}, SHIFT(17),
  [109] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2),
  [111] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2), SHIFT_REPEAT(8),
  [114] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [116] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [118] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [120] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [122] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [124] = {.entry = {.count = 1, .reusable = false}}, SHIFT(21),
  [126] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause, 4),
  [128] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [130] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [132] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [134] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 3),
  [136] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [138] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 2),
  [140] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [142] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [144] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [146] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 4),
  [148] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [150] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_erlang(void) {
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
