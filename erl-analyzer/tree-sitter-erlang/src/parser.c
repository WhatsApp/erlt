#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 12
#define STATE_COUNT 42
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 29
#define ALIAS_COUNT 0
#define TOKEN_COUNT 15
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
  anon_sym_SQUOTE = 12,
  aux_sym__quoted_atom_token1 = 13,
  sym__comment = 14,
  sym_source_file = 15,
  sym__form = 16,
  sym_module_attribute = 17,
  sym_attribute = 18,
  sym_function = 19,
  sym_function_clause = 20,
  sym_arg_list = 21,
  sym_block = 22,
  sym__expr = 23,
  sym_atom = 24,
  sym__quoted_atom = 25,
  aux_sym_source_file_repeat1 = 26,
  aux_sym_function_repeat1 = 27,
  aux_sym_arg_list_repeat1 = 28,
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
  [anon_sym_SQUOTE] = "'",
  [aux_sym__quoted_atom_token1] = "_quoted_atom_token1",
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
  [sym__quoted_atom] = "_quoted_atom",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_function_repeat1] = "function_repeat1",
  [aux_sym_arg_list_repeat1] = "arg_list_repeat1",
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
  [anon_sym_SQUOTE] = anon_sym_SQUOTE,
  [aux_sym__quoted_atom_token1] = aux_sym__quoted_atom_token1,
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
  [sym__quoted_atom] = sym__quoted_atom,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_function_repeat1] = aux_sym_function_repeat1,
  [aux_sym_arg_list_repeat1] = aux_sym_arg_list_repeat1,
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
  [anon_sym_SQUOTE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym__quoted_atom_token1] = {
    .visible = false,
    .named = false,
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
      if (eof) ADVANCE(4);
      if (lookahead == '%') ADVANCE(20);
      if (lookahead == '\'') ADVANCE(16);
      if (lookahead == '(') ADVANCE(7);
      if (lookahead == ')') ADVANCE(8);
      if (lookahead == ',') ADVANCE(12);
      if (lookahead == '-') ADVANCE(6);
      if (lookahead == '.') ADVANCE(9);
      if (lookahead == ';') ADVANCE(10);
      if (lookahead == '_') ADVANCE(13);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 222)) ADVANCE(14);
      if (('a' <= lookahead && lookahead <= 'z') ||
          (223 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(15);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(0)
      END_STATE();
    case 1:
      if (lookahead == '%') ADVANCE(20);
      if (lookahead == '-') ADVANCE(2);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(1)
      END_STATE();
    case 2:
      if (lookahead == '>') ADVANCE(11);
      END_STATE();
    case 3:
      if (eof) ADVANCE(4);
      if (lookahead == '%') ADVANCE(20);
      if (lookahead == '\'') ADVANCE(16);
      if (lookahead == '-') ADVANCE(5);
      if (('a' <= lookahead && lookahead <= 'z') ||
          (223 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(15);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(3)
      END_STATE();
    case 4:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 5:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 6:
      ACCEPT_TOKEN(anon_sym_DASH);
      if (lookahead == '>') ADVANCE(11);
      END_STATE();
    case 7:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 8:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 9:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 10:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 11:
      ACCEPT_TOKEN(anon_sym_DASH_GT);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(sym_wildcard);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(14);
      END_STATE();
    case 14:
      ACCEPT_TOKEN(sym_var);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(14);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(sym__raw_atom);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(15);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(anon_sym_SQUOTE);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(aux_sym__quoted_atom_token1);
      if (lookahead == '\n') ADVANCE(19);
      if (lookahead == '\'' ||
          lookahead == '\\') ADVANCE(20);
      if (lookahead != 0) ADVANCE(17);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(aux_sym__quoted_atom_token1);
      if (lookahead == '%') ADVANCE(17);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(18);
      if (lookahead != 0 &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(19);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(aux_sym__quoted_atom_token1);
      if (lookahead != 0 &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(19);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(sym__comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(20);
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
  [1] = {.lex_state = 3},
  [2] = {.lex_state = 3},
  [3] = {.lex_state = 3},
  [4] = {.lex_state = 0},
  [5] = {.lex_state = 0},
  [6] = {.lex_state = 0},
  [7] = {.lex_state = 0},
  [8] = {.lex_state = 0},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 3},
  [15] = {.lex_state = 3},
  [16] = {.lex_state = 0},
  [17] = {.lex_state = 3},
  [18] = {.lex_state = 3},
  [19] = {.lex_state = 0},
  [20] = {.lex_state = 0},
  [21] = {.lex_state = 0},
  [22] = {.lex_state = 0},
  [23] = {.lex_state = 0},
  [24] = {.lex_state = 0},
  [25] = {.lex_state = 0},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 0},
  [28] = {.lex_state = 0},
  [29] = {.lex_state = 0},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 1},
  [32] = {.lex_state = 1},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 0},
  [35] = {.lex_state = 0},
  [36] = {.lex_state = 0},
  [37] = {.lex_state = 1},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 18},
  [40] = {.lex_state = 1},
  [41] = {.lex_state = 0},
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
    [anon_sym_SQUOTE] = ACTIONS(1),
    [sym__comment] = ACTIONS(3),
  },
  [1] = {
    [sym_source_file] = STATE(41),
    [sym__form] = STATE(2),
    [sym_module_attribute] = STATE(2),
    [sym_attribute] = STATE(2),
    [sym_function] = STATE(2),
    [sym_function_clause] = STATE(23),
    [sym_atom] = STATE(27),
    [sym__quoted_atom] = STATE(11),
    [aux_sym_source_file_repeat1] = STATE(2),
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
    ACTIONS(9), 1,
      anon_sym_DASH,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      ts_builtin_sym_end,
    STATE(11), 1,
      sym__quoted_atom,
    STATE(23), 1,
      sym_function_clause,
    STATE(27), 1,
      sym_atom,
    STATE(3), 5,
      sym__form,
      sym_module_attribute,
      sym_attribute,
      sym_function,
      aux_sym_source_file_repeat1,
  [32] = 9,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(15), 1,
      ts_builtin_sym_end,
    ACTIONS(17), 1,
      sym__raw_atom,
    ACTIONS(20), 1,
      anon_sym_DASH,
    ACTIONS(23), 1,
      anon_sym_SQUOTE,
    STATE(11), 1,
      sym__quoted_atom,
    STATE(23), 1,
      sym_function_clause,
    STATE(27), 1,
      sym_atom,
    STATE(3), 5,
      sym__form,
      sym_module_attribute,
      sym_attribute,
      sym_function,
      aux_sym_source_file_repeat1,
  [64] = 7,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    STATE(11), 1,
      sym__quoted_atom,
    STATE(28), 1,
      sym_block,
    ACTIONS(26), 2,
      sym_wildcard,
      sym_var,
    STATE(13), 2,
      sym__expr,
      sym_atom,
  [88] = 7,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(28), 1,
      anon_sym_RPAREN,
    STATE(11), 1,
      sym__quoted_atom,
    ACTIONS(30), 2,
      sym_wildcard,
      sym_var,
    STATE(25), 2,
      sym__expr,
      sym_atom,
  [112] = 6,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    STATE(11), 1,
      sym__quoted_atom,
    ACTIONS(32), 2,
      sym_wildcard,
      sym_var,
    STATE(20), 2,
      sym__expr,
      sym_atom,
  [133] = 6,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    STATE(11), 1,
      sym__quoted_atom,
    ACTIONS(34), 2,
      sym_wildcard,
      sym_var,
    STATE(29), 2,
      sym__expr,
      sym_atom,
  [154] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(36), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [165] = 6,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(38), 1,
      sym__raw_atom,
    ACTIONS(40), 1,
      anon_sym_module,
    STATE(11), 1,
      sym__quoted_atom,
    STATE(38), 1,
      sym_atom,
  [184] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(44), 1,
      anon_sym_COMMA,
    STATE(10), 1,
      aux_sym_arg_list_repeat1,
    ACTIONS(42), 3,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
  [199] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(47), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [210] = 6,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    STATE(11), 1,
      sym__quoted_atom,
    STATE(26), 1,
      sym_function_clause,
    STATE(27), 1,
      sym_atom,
  [229] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(51), 1,
      anon_sym_COMMA,
    STATE(19), 1,
      aux_sym_arg_list_repeat1,
    ACTIONS(49), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [243] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(53), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [253] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(55), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [263] = 5,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    STATE(11), 1,
      sym__quoted_atom,
    STATE(36), 1,
      sym_atom,
  [279] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(57), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [289] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(59), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [299] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(51), 1,
      anon_sym_COMMA,
    STATE(10), 1,
      aux_sym_arg_list_repeat1,
    ACTIONS(61), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [313] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(42), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [323] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(51), 1,
      anon_sym_COMMA,
    ACTIONS(63), 1,
      anon_sym_RPAREN,
    STATE(10), 1,
      aux_sym_arg_list_repeat1,
  [336] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(65), 1,
      anon_sym_DOT,
    ACTIONS(67), 1,
      anon_sym_SEMI,
    STATE(24), 1,
      aux_sym_function_repeat1,
  [349] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(67), 1,
      anon_sym_SEMI,
    ACTIONS(69), 1,
      anon_sym_DOT,
    STATE(22), 1,
      aux_sym_function_repeat1,
  [362] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(71), 1,
      anon_sym_DOT,
    ACTIONS(73), 1,
      anon_sym_SEMI,
    STATE(24), 1,
      aux_sym_function_repeat1,
  [375] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(51), 1,
      anon_sym_COMMA,
    ACTIONS(76), 1,
      anon_sym_RPAREN,
    STATE(21), 1,
      aux_sym_arg_list_repeat1,
  [388] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(71), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [396] = 3,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(78), 1,
      anon_sym_LPAREN,
    STATE(32), 1,
      sym_arg_list,
  [406] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(80), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [414] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(82), 1,
      anon_sym_RPAREN,
  [421] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(84), 1,
      anon_sym_LPAREN,
  [428] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(86), 1,
      anon_sym_DASH_GT,
  [435] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(88), 1,
      anon_sym_DASH_GT,
  [442] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(90), 1,
      anon_sym_SQUOTE,
  [449] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(92), 1,
      anon_sym_DOT,
  [456] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(94), 1,
      anon_sym_DOT,
  [463] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(96), 1,
      anon_sym_RPAREN,
  [470] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(98), 1,
      anon_sym_DASH_GT,
  [477] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(100), 1,
      anon_sym_LPAREN,
  [484] = 2,
    ACTIONS(102), 1,
      aux_sym__quoted_atom_token1,
    ACTIONS(104), 1,
      sym__comment,
  [491] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(106), 1,
      anon_sym_DASH_GT,
  [498] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(108), 1,
      ts_builtin_sym_end,
};

static uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 32,
  [SMALL_STATE(4)] = 64,
  [SMALL_STATE(5)] = 88,
  [SMALL_STATE(6)] = 112,
  [SMALL_STATE(7)] = 133,
  [SMALL_STATE(8)] = 154,
  [SMALL_STATE(9)] = 165,
  [SMALL_STATE(10)] = 184,
  [SMALL_STATE(11)] = 199,
  [SMALL_STATE(12)] = 210,
  [SMALL_STATE(13)] = 229,
  [SMALL_STATE(14)] = 243,
  [SMALL_STATE(15)] = 253,
  [SMALL_STATE(16)] = 263,
  [SMALL_STATE(17)] = 279,
  [SMALL_STATE(18)] = 289,
  [SMALL_STATE(19)] = 299,
  [SMALL_STATE(20)] = 313,
  [SMALL_STATE(21)] = 323,
  [SMALL_STATE(22)] = 336,
  [SMALL_STATE(23)] = 349,
  [SMALL_STATE(24)] = 362,
  [SMALL_STATE(25)] = 375,
  [SMALL_STATE(26)] = 388,
  [SMALL_STATE(27)] = 396,
  [SMALL_STATE(28)] = 406,
  [SMALL_STATE(29)] = 414,
  [SMALL_STATE(30)] = 421,
  [SMALL_STATE(31)] = 428,
  [SMALL_STATE(32)] = 435,
  [SMALL_STATE(33)] = 442,
  [SMALL_STATE(34)] = 449,
  [SMALL_STATE(35)] = 456,
  [SMALL_STATE(36)] = 463,
  [SMALL_STATE(37)] = 470,
  [SMALL_STATE(38)] = 477,
  [SMALL_STATE(39)] = 484,
  [SMALL_STATE(40)] = 491,
  [SMALL_STATE(41)] = 498,
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [13] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [15] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [17] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(11),
  [20] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(9),
  [23] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(39),
  [26] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [28] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [30] = {.entry = {.count = 1, .reusable = false}}, SHIFT(25),
  [32] = {.entry = {.count = 1, .reusable = false}}, SHIFT(20),
  [34] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [36] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__quoted_atom, 3),
  [38] = {.entry = {.count = 1, .reusable = false}}, SHIFT(11),
  [40] = {.entry = {.count = 1, .reusable = false}}, SHIFT(30),
  [42] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_arg_list_repeat1, 2),
  [44] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_arg_list_repeat1, 2), SHIFT_REPEAT(6),
  [47] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_atom, 1),
  [49] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_block, 1),
  [51] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [53] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 2),
  [55] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_attribute, 6),
  [57] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_attribute, 6),
  [59] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 3),
  [61] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_block, 2),
  [63] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [65] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [67] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [69] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [71] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2),
  [73] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2), SHIFT_REPEAT(12),
  [76] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [78] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [80] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause, 4),
  [82] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [84] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [86] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 3),
  [88] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [90] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [92] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [96] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [98] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 4),
  [100] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [102] = {.entry = {.count = 1, .reusable = false}}, SHIFT(33),
  [104] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [106] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 2),
  [108] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
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
