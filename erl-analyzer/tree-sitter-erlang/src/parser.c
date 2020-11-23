#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 12
#define STATE_COUNT 51
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 38
#define ALIAS_COUNT 0
#define TOKEN_COUNT 20
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
  anon_sym_DQUOTE = 12,
  aux_sym_string_token1 = 13,
  anon_sym_DOLLAR = 14,
  aux_sym_char_token1 = 15,
  anon_sym_SQUOTE = 16,
  aux_sym__quoted_atom_token1 = 17,
  sym__escape = 18,
  sym__comment = 19,
  sym_source_file = 20,
  sym__form = 21,
  sym_module_attribute = 22,
  sym_attribute = 23,
  sym_function = 24,
  sym_function_clause = 25,
  sym_arg_list = 26,
  sym_block = 27,
  sym__expr = 28,
  sym_atom = 29,
  sym_string = 30,
  sym_char = 31,
  sym__quoted_atom = 32,
  aux_sym_source_file_repeat1 = 33,
  aux_sym_function_repeat1 = 34,
  aux_sym_arg_list_repeat1 = 35,
  aux_sym_string_repeat1 = 36,
  aux_sym__quoted_atom_repeat1 = 37,
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
  [sym_atom] = "atom",
  [sym_string] = "string",
  [sym_char] = "char",
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
  [sym_atom] = sym_atom,
  [sym_string] = sym_string,
  [sym_char] = sym_char,
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
  [sym_atom] = {
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
      if (eof) ADVANCE(12);
      if (lookahead == '"') ADVANCE(23);
      if (lookahead == '$') ADVANCE(27);
      if (lookahead == '%') ADVANCE(39);
      if (lookahead == '\'') ADVANCE(32);
      if (lookahead == '(') ADVANCE(15);
      if (lookahead == ')') ADVANCE(16);
      if (lookahead == ',') ADVANCE(20);
      if (lookahead == '-') ADVANCE(14);
      if (lookahead == '.') ADVANCE(17);
      if (lookahead == ';') ADVANCE(18);
      if (lookahead == '\\') ADVANCE(8);
      if (lookahead == '_') ADVANCE(21);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 222)) ADVANCE(22);
      if (('a' <= lookahead && lookahead <= 'z') ||
          (223 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(31);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(0)
      END_STATE();
    case 1:
      if (lookahead == '"') ADVANCE(23);
      if (lookahead == '%') ADVANCE(24);
      if (lookahead == '\\') ADVANCE(8);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(25);
      if (lookahead != 0) ADVANCE(26);
      END_STATE();
    case 2:
      if (lookahead == '%') ADVANCE(39);
      if (lookahead == '-') ADVANCE(5);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(2)
      END_STATE();
    case 3:
      if (lookahead == '%') ADVANCE(33);
      if (lookahead == '\'') ADVANCE(32);
      if (lookahead == '\\') ADVANCE(8);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(34);
      if (lookahead != 0) ADVANCE(35);
      END_STATE();
    case 4:
      if (lookahead == '%') ADVANCE(30);
      if (lookahead == '\\') ADVANCE(8);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(29);
      if (lookahead != 0) ADVANCE(28);
      END_STATE();
    case 5:
      if (lookahead == '>') ADVANCE(19);
      END_STATE();
    case 6:
      if (lookahead == '{') ADVANCE(10);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(9);
      END_STATE();
    case 7:
      if (lookahead == '}') ADVANCE(36);
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
          lookahead == 'v') ADVANCE(36);
      if (lookahead == 'x') ADVANCE(6);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(38);
      END_STATE();
    case 9:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(36);
      END_STATE();
    case 10:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(7);
      END_STATE();
    case 11:
      if (eof) ADVANCE(12);
      if (lookahead == '%') ADVANCE(39);
      if (lookahead == '\'') ADVANCE(32);
      if (lookahead == '-') ADVANCE(13);
      if (('a' <= lookahead && lookahead <= 'z') ||
          (223 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(31);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(11)
      END_STATE();
    case 12:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 14:
      ACCEPT_TOKEN(anon_sym_DASH);
      if (lookahead == '>') ADVANCE(19);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(anon_sym_DASH_GT);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(sym_wildcard);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(22);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(sym_var);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(22);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '\n') ADVANCE(26);
      if (lookahead == '"' ||
          lookahead == '\\') ADVANCE(39);
      if (lookahead != 0) ADVANCE(24);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '%') ADVANCE(24);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(25);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(26);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(26);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(anon_sym_DOLLAR);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(aux_sym_char_token1);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(aux_sym_char_token1);
      if (lookahead == '%') ADVANCE(30);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(29);
      if (lookahead != 0 &&
          lookahead != '\\') ADVANCE(28);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(aux_sym_char_token1);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(39);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(sym__raw_atom);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(31);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(anon_sym_SQUOTE);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(aux_sym__quoted_atom_token1);
      if (lookahead == '\n') ADVANCE(35);
      if (lookahead == '\'' ||
          lookahead == '\\') ADVANCE(39);
      if (lookahead != 0) ADVANCE(33);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(aux_sym__quoted_atom_token1);
      if (lookahead == '%') ADVANCE(33);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(34);
      if (lookahead != 0 &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(35);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(aux_sym__quoted_atom_token1);
      if (lookahead != 0 &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(35);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(sym__escape);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(sym__escape);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(36);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(sym__escape);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(37);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(sym__comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(39);
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
  [1] = {.lex_state = 11},
  [2] = {.lex_state = 0},
  [3] = {.lex_state = 11},
  [4] = {.lex_state = 0},
  [5] = {.lex_state = 11},
  [6] = {.lex_state = 0},
  [7] = {.lex_state = 0},
  [8] = {.lex_state = 0},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 0},
  [15] = {.lex_state = 0},
  [16] = {.lex_state = 11},
  [17] = {.lex_state = 3},
  [18] = {.lex_state = 11},
  [19] = {.lex_state = 11},
  [20] = {.lex_state = 0},
  [21] = {.lex_state = 0},
  [22] = {.lex_state = 3},
  [23] = {.lex_state = 0},
  [24] = {.lex_state = 11},
  [25] = {.lex_state = 1},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 1},
  [28] = {.lex_state = 0},
  [29] = {.lex_state = 1},
  [30] = {.lex_state = 3},
  [31] = {.lex_state = 0},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 0},
  [35] = {.lex_state = 0},
  [36] = {.lex_state = 0},
  [37] = {.lex_state = 0},
  [38] = {.lex_state = 4},
  [39] = {.lex_state = 0},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 0},
  [42] = {.lex_state = 0},
  [43] = {.lex_state = 2},
  [44] = {.lex_state = 0},
  [45] = {.lex_state = 0},
  [46] = {.lex_state = 2},
  [47] = {.lex_state = 2},
  [48] = {.lex_state = 0},
  [49] = {.lex_state = 2},
  [50] = {.lex_state = 0},
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
    [anon_sym_DQUOTE] = ACTIONS(1),
    [anon_sym_DOLLAR] = ACTIONS(1),
    [anon_sym_SQUOTE] = ACTIONS(1),
    [sym__escape] = ACTIONS(1),
    [sym__comment] = ACTIONS(3),
  },
  [1] = {
    [sym_source_file] = STATE(48),
    [sym__form] = STATE(3),
    [sym_module_attribute] = STATE(3),
    [sym_attribute] = STATE(3),
    [sym_function] = STATE(3),
    [sym_function_clause] = STATE(32),
    [sym_atom] = STATE(36),
    [sym__quoted_atom] = STATE(10),
    [aux_sym_source_file_repeat1] = STATE(3),
    [ts_builtin_sym_end] = ACTIONS(5),
    [sym__raw_atom] = ACTIONS(7),
    [anon_sym_DASH] = ACTIONS(9),
    [anon_sym_SQUOTE] = ACTIONS(11),
    [sym__comment] = ACTIONS(3),
  },
};

static uint16_t ts_small_parse_table[] = {
  [0] = 9,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(17), 1,
      anon_sym_DOLLAR,
    STATE(10), 1,
      sym__quoted_atom,
    STATE(37), 1,
      sym_block,
    ACTIONS(13), 2,
      sym_wildcard,
      sym_var,
    STATE(28), 4,
      sym__expr,
      sym_atom,
      sym_string,
      sym_char,
  [32] = 9,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(9), 1,
      anon_sym_DASH,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(19), 1,
      ts_builtin_sym_end,
    STATE(10), 1,
      sym__quoted_atom,
    STATE(32), 1,
      sym_function_clause,
    STATE(36), 1,
      sym_atom,
    STATE(5), 5,
      sym__form,
      sym_module_attribute,
      sym_attribute,
      sym_function,
      aux_sym_source_file_repeat1,
  [64] = 9,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(17), 1,
      anon_sym_DOLLAR,
    ACTIONS(21), 1,
      anon_sym_RPAREN,
    STATE(10), 1,
      sym__quoted_atom,
    ACTIONS(23), 2,
      sym_wildcard,
      sym_var,
    STATE(31), 4,
      sym__expr,
      sym_atom,
      sym_string,
      sym_char,
  [96] = 9,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(25), 1,
      ts_builtin_sym_end,
    ACTIONS(27), 1,
      sym__raw_atom,
    ACTIONS(30), 1,
      anon_sym_DASH,
    ACTIONS(33), 1,
      anon_sym_SQUOTE,
    STATE(10), 1,
      sym__quoted_atom,
    STATE(32), 1,
      sym_function_clause,
    STATE(36), 1,
      sym_atom,
    STATE(5), 5,
      sym__form,
      sym_module_attribute,
      sym_attribute,
      sym_function,
      aux_sym_source_file_repeat1,
  [128] = 8,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(17), 1,
      anon_sym_DOLLAR,
    STATE(10), 1,
      sym__quoted_atom,
    ACTIONS(36), 2,
      sym_wildcard,
      sym_var,
    STATE(23), 4,
      sym__expr,
      sym_atom,
      sym_string,
      sym_char,
  [157] = 8,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(17), 1,
      anon_sym_DOLLAR,
    STATE(10), 1,
      sym__quoted_atom,
    ACTIONS(38), 2,
      sym_wildcard,
      sym_var,
    STATE(41), 4,
      sym__expr,
      sym_atom,
      sym_string,
      sym_char,
  [186] = 6,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    STATE(10), 1,
      sym__quoted_atom,
    STATE(36), 1,
      sym_atom,
    STATE(39), 1,
      sym_function_clause,
  [205] = 6,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(40), 1,
      sym__raw_atom,
    ACTIONS(42), 1,
      anon_sym_module,
    STATE(10), 1,
      sym__quoted_atom,
    STATE(50), 1,
      sym_atom,
  [224] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(44), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [235] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(48), 1,
      anon_sym_COMMA,
    STATE(11), 1,
      aux_sym_arg_list_repeat1,
    ACTIONS(46), 3,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
  [250] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(51), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [261] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(53), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [272] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(55), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [282] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(57), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [292] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(59), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [302] = 4,
    ACTIONS(61), 1,
      anon_sym_SQUOTE,
    ACTIONS(65), 1,
      sym__comment,
    STATE(22), 1,
      aux_sym__quoted_atom_repeat1,
    ACTIONS(63), 2,
      aux_sym__quoted_atom_token1,
      sym__escape,
  [316] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(67), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [326] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(69), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [336] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(73), 1,
      anon_sym_COMMA,
    STATE(11), 1,
      aux_sym_arg_list_repeat1,
    ACTIONS(71), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [350] = 5,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    STATE(10), 1,
      sym__quoted_atom,
    STATE(40), 1,
      sym_atom,
  [366] = 4,
    ACTIONS(65), 1,
      sym__comment,
    ACTIONS(75), 1,
      anon_sym_SQUOTE,
    STATE(22), 1,
      aux_sym__quoted_atom_repeat1,
    ACTIONS(77), 2,
      aux_sym__quoted_atom_token1,
      sym__escape,
  [380] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(46), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [390] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(80), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [400] = 4,
    ACTIONS(65), 1,
      sym__comment,
    ACTIONS(82), 1,
      anon_sym_DQUOTE,
    STATE(25), 1,
      aux_sym_string_repeat1,
    ACTIONS(84), 2,
      aux_sym_string_token1,
      sym__escape,
  [414] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(87), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [424] = 4,
    ACTIONS(65), 1,
      sym__comment,
    ACTIONS(89), 1,
      anon_sym_DQUOTE,
    STATE(29), 1,
      aux_sym_string_repeat1,
    ACTIONS(91), 2,
      aux_sym_string_token1,
      sym__escape,
  [438] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(73), 1,
      anon_sym_COMMA,
    STATE(20), 1,
      aux_sym_arg_list_repeat1,
    ACTIONS(93), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [452] = 4,
    ACTIONS(65), 1,
      sym__comment,
    ACTIONS(95), 1,
      anon_sym_DQUOTE,
    STATE(25), 1,
      aux_sym_string_repeat1,
    ACTIONS(97), 2,
      aux_sym_string_token1,
      sym__escape,
  [466] = 4,
    ACTIONS(65), 1,
      sym__comment,
    ACTIONS(99), 1,
      anon_sym_SQUOTE,
    STATE(17), 1,
      aux_sym__quoted_atom_repeat1,
    ACTIONS(101), 2,
      aux_sym__quoted_atom_token1,
      sym__escape,
  [480] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(73), 1,
      anon_sym_COMMA,
    ACTIONS(103), 1,
      anon_sym_RPAREN,
    STATE(33), 1,
      aux_sym_arg_list_repeat1,
  [493] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(105), 1,
      anon_sym_DOT,
    ACTIONS(107), 1,
      anon_sym_SEMI,
    STATE(35), 1,
      aux_sym_function_repeat1,
  [506] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(73), 1,
      anon_sym_COMMA,
    ACTIONS(109), 1,
      anon_sym_RPAREN,
    STATE(11), 1,
      aux_sym_arg_list_repeat1,
  [519] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(111), 1,
      anon_sym_DOT,
    ACTIONS(113), 1,
      anon_sym_SEMI,
    STATE(34), 1,
      aux_sym_function_repeat1,
  [532] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(107), 1,
      anon_sym_SEMI,
    ACTIONS(116), 1,
      anon_sym_DOT,
    STATE(34), 1,
      aux_sym_function_repeat1,
  [545] = 3,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(118), 1,
      anon_sym_LPAREN,
    STATE(49), 1,
      sym_arg_list,
  [555] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(120), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [563] = 2,
    ACTIONS(65), 1,
      sym__comment,
    ACTIONS(122), 2,
      aux_sym_char_token1,
      sym__escape,
  [571] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(111), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [579] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(124), 1,
      anon_sym_RPAREN,
  [586] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(126), 1,
      anon_sym_RPAREN,
  [593] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(128), 1,
      anon_sym_LPAREN,
  [600] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(130), 1,
      anon_sym_DASH_GT,
  [607] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(132), 1,
      anon_sym_DOT,
  [614] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(134), 1,
      anon_sym_DOT,
  [621] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(136), 1,
      anon_sym_DASH_GT,
  [628] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(138), 1,
      anon_sym_DASH_GT,
  [635] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(140), 1,
      ts_builtin_sym_end,
  [642] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(142), 1,
      anon_sym_DASH_GT,
  [649] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
};

static uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 32,
  [SMALL_STATE(4)] = 64,
  [SMALL_STATE(5)] = 96,
  [SMALL_STATE(6)] = 128,
  [SMALL_STATE(7)] = 157,
  [SMALL_STATE(8)] = 186,
  [SMALL_STATE(9)] = 205,
  [SMALL_STATE(10)] = 224,
  [SMALL_STATE(11)] = 235,
  [SMALL_STATE(12)] = 250,
  [SMALL_STATE(13)] = 261,
  [SMALL_STATE(14)] = 272,
  [SMALL_STATE(15)] = 282,
  [SMALL_STATE(16)] = 292,
  [SMALL_STATE(17)] = 302,
  [SMALL_STATE(18)] = 316,
  [SMALL_STATE(19)] = 326,
  [SMALL_STATE(20)] = 336,
  [SMALL_STATE(21)] = 350,
  [SMALL_STATE(22)] = 366,
  [SMALL_STATE(23)] = 380,
  [SMALL_STATE(24)] = 390,
  [SMALL_STATE(25)] = 400,
  [SMALL_STATE(26)] = 414,
  [SMALL_STATE(27)] = 424,
  [SMALL_STATE(28)] = 438,
  [SMALL_STATE(29)] = 452,
  [SMALL_STATE(30)] = 466,
  [SMALL_STATE(31)] = 480,
  [SMALL_STATE(32)] = 493,
  [SMALL_STATE(33)] = 506,
  [SMALL_STATE(34)] = 519,
  [SMALL_STATE(35)] = 532,
  [SMALL_STATE(36)] = 545,
  [SMALL_STATE(37)] = 555,
  [SMALL_STATE(38)] = 563,
  [SMALL_STATE(39)] = 571,
  [SMALL_STATE(40)] = 579,
  [SMALL_STATE(41)] = 586,
  [SMALL_STATE(42)] = 593,
  [SMALL_STATE(43)] = 600,
  [SMALL_STATE(44)] = 607,
  [SMALL_STATE(45)] = 614,
  [SMALL_STATE(46)] = 621,
  [SMALL_STATE(47)] = 628,
  [SMALL_STATE(48)] = 635,
  [SMALL_STATE(49)] = 642,
  [SMALL_STATE(50)] = 649,
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(28),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [19] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [23] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [25] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [27] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(10),
  [30] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(9),
  [33] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(30),
  [36] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [38] = {.entry = {.count = 1, .reusable = false}}, SHIFT(41),
  [40] = {.entry = {.count = 1, .reusable = false}}, SHIFT(10),
  [42] = {.entry = {.count = 1, .reusable = false}}, SHIFT(42),
  [44] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_atom, 1),
  [46] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_arg_list_repeat1, 2),
  [48] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_arg_list_repeat1, 2), SHIFT_REPEAT(6),
  [51] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__quoted_atom, 3),
  [53] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__quoted_atom, 2),
  [55] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_char, 2),
  [57] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 2),
  [59] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_attribute, 6),
  [61] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [63] = {.entry = {.count = 1, .reusable = false}}, SHIFT(22),
  [65] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [67] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 2),
  [69] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_attribute, 6),
  [71] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_block, 2),
  [73] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [75] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym__quoted_atom_repeat1, 2),
  [77] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__quoted_atom_repeat1, 2), SHIFT_REPEAT(22),
  [80] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 3),
  [82] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_string_repeat1, 2),
  [84] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(25),
  [87] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3),
  [89] = {.entry = {.count = 1, .reusable = false}}, SHIFT(15),
  [91] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [93] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_block, 1),
  [95] = {.entry = {.count = 1, .reusable = false}}, SHIFT(26),
  [97] = {.entry = {.count = 1, .reusable = false}}, SHIFT(25),
  [99] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [101] = {.entry = {.count = 1, .reusable = false}}, SHIFT(17),
  [103] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [105] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [107] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [109] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [111] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2),
  [113] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2), SHIFT_REPEAT(8),
  [116] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [118] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [120] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause, 4),
  [122] = {.entry = {.count = 1, .reusable = false}}, SHIFT(14),
  [124] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [126] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [128] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [130] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 3),
  [132] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [134] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [136] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 2),
  [138] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 4),
  [140] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [142] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [144] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
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
