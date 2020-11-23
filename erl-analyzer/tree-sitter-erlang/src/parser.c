#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 12
#define STATE_COUNT 49
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 35
#define ALIAS_COUNT 0
#define TOKEN_COUNT 18
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
  anon_sym_SQUOTE = 14,
  aux_sym__quoted_atom_token1 = 15,
  sym__escape = 16,
  sym__comment = 17,
  sym_source_file = 18,
  sym__form = 19,
  sym_module_attribute = 20,
  sym_attribute = 21,
  sym_function = 22,
  sym_function_clause = 23,
  sym_arg_list = 24,
  sym_block = 25,
  sym__expr = 26,
  sym_atom = 27,
  sym_string = 28,
  sym__quoted_atom = 29,
  aux_sym_source_file_repeat1 = 30,
  aux_sym_function_repeat1 = 31,
  aux_sym_arg_list_repeat1 = 32,
  aux_sym_string_repeat1 = 33,
  aux_sym__quoted_atom_repeat1 = 34,
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
      if (eof) ADVANCE(11);
      if (lookahead == '"') ADVANCE(22);
      if (lookahead == '%') ADVANCE(34);
      if (lookahead == '\'') ADVANCE(27);
      if (lookahead == '(') ADVANCE(14);
      if (lookahead == ')') ADVANCE(15);
      if (lookahead == ',') ADVANCE(19);
      if (lookahead == '-') ADVANCE(13);
      if (lookahead == '.') ADVANCE(16);
      if (lookahead == ';') ADVANCE(17);
      if (lookahead == '\\') ADVANCE(7);
      if (lookahead == '_') ADVANCE(20);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 222)) ADVANCE(21);
      if (('a' <= lookahead && lookahead <= 'z') ||
          (223 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(26);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(0)
      END_STATE();
    case 1:
      if (lookahead == '"') ADVANCE(22);
      if (lookahead == '%') ADVANCE(23);
      if (lookahead == '\\') ADVANCE(7);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(24);
      if (lookahead != 0) ADVANCE(25);
      END_STATE();
    case 2:
      if (lookahead == '%') ADVANCE(34);
      if (lookahead == '-') ADVANCE(4);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(2)
      END_STATE();
    case 3:
      if (lookahead == '%') ADVANCE(28);
      if (lookahead == '\'') ADVANCE(27);
      if (lookahead == '\\') ADVANCE(7);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(29);
      if (lookahead != 0) ADVANCE(30);
      END_STATE();
    case 4:
      if (lookahead == '>') ADVANCE(18);
      END_STATE();
    case 5:
      if (lookahead == '{') ADVANCE(9);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(8);
      END_STATE();
    case 6:
      if (lookahead == '}') ADVANCE(31);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(6);
      END_STATE();
    case 7:
      if (lookahead == '\n' ||
          lookahead == 'b' ||
          ('d' <= lookahead && lookahead <= 'f') ||
          lookahead == 'n' ||
          ('r' <= lookahead && lookahead <= 't') ||
          lookahead == 'v') ADVANCE(31);
      if (lookahead == 'x') ADVANCE(5);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(33);
      END_STATE();
    case 8:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(31);
      END_STATE();
    case 9:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(6);
      END_STATE();
    case 10:
      if (eof) ADVANCE(11);
      if (lookahead == '%') ADVANCE(34);
      if (lookahead == '\'') ADVANCE(27);
      if (lookahead == '-') ADVANCE(12);
      if (('a' <= lookahead && lookahead <= 'z') ||
          (223 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(26);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) SKIP(10)
      END_STATE();
    case 11:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 12:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 13:
      ACCEPT_TOKEN(anon_sym_DASH);
      if (lookahead == '>') ADVANCE(18);
      END_STATE();
    case 14:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 15:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 16:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 17:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(anon_sym_DASH_GT);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(sym_wildcard);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(21);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(sym_var);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(21);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '\n') ADVANCE(25);
      if (lookahead == '"' ||
          lookahead == '\\') ADVANCE(34);
      if (lookahead != 0) ADVANCE(23);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '%') ADVANCE(23);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(24);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(25);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(25);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(sym__raw_atom);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('@' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z') ||
          (192 <= lookahead && lookahead <= 214) ||
          (216 <= lookahead && lookahead <= 246) ||
          (248 <= lookahead && lookahead <= 255)) ADVANCE(26);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(anon_sym_SQUOTE);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(aux_sym__quoted_atom_token1);
      if (lookahead == '\n') ADVANCE(30);
      if (lookahead == '\'' ||
          lookahead == '\\') ADVANCE(34);
      if (lookahead != 0) ADVANCE(28);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(aux_sym__quoted_atom_token1);
      if (lookahead == '%') ADVANCE(28);
      if ((0 <= lookahead && lookahead <= ' ') ||
          (128 <= lookahead && lookahead <= 160)) ADVANCE(29);
      if (lookahead != 0 &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(30);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(aux_sym__quoted_atom_token1);
      if (lookahead != 0 &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(30);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(sym__escape);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(sym__escape);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(31);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(sym__escape);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(32);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(sym__comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(34);
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
  [1] = {.lex_state = 10},
  [2] = {.lex_state = 10},
  [3] = {.lex_state = 10},
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
  [14] = {.lex_state = 10},
  [15] = {.lex_state = 1},
  [16] = {.lex_state = 10},
  [17] = {.lex_state = 10},
  [18] = {.lex_state = 3},
  [19] = {.lex_state = 10},
  [20] = {.lex_state = 0},
  [21] = {.lex_state = 0},
  [22] = {.lex_state = 0},
  [23] = {.lex_state = 3},
  [24] = {.lex_state = 0},
  [25] = {.lex_state = 1},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 1},
  [28] = {.lex_state = 0},
  [29] = {.lex_state = 3},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 0},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 0},
  [35] = {.lex_state = 0},
  [36] = {.lex_state = 0},
  [37] = {.lex_state = 0},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 0},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 2},
  [42] = {.lex_state = 0},
  [43] = {.lex_state = 0},
  [44] = {.lex_state = 2},
  [45] = {.lex_state = 0},
  [46] = {.lex_state = 2},
  [47] = {.lex_state = 2},
  [48] = {.lex_state = 0},
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
    [anon_sym_SQUOTE] = ACTIONS(1),
    [sym__escape] = ACTIONS(1),
    [sym__comment] = ACTIONS(3),
  },
  [1] = {
    [sym_source_file] = STATE(45),
    [sym__form] = STATE(2),
    [sym_module_attribute] = STATE(2),
    [sym_attribute] = STATE(2),
    [sym_function] = STATE(2),
    [sym_function_clause] = STATE(32),
    [sym_atom] = STATE(35),
    [sym__quoted_atom] = STATE(10),
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
    STATE(10), 1,
      sym__quoted_atom,
    STATE(32), 1,
      sym_function_clause,
    STATE(35), 1,
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
    STATE(10), 1,
      sym__quoted_atom,
    STATE(32), 1,
      sym_function_clause,
    STATE(35), 1,
      sym_atom,
    STATE(3), 5,
      sym__form,
      sym_module_attribute,
      sym_attribute,
      sym_function,
      aux_sym_source_file_repeat1,
  [64] = 8,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(28), 1,
      anon_sym_DQUOTE,
    STATE(10), 1,
      sym__quoted_atom,
    STATE(37), 1,
      sym_block,
    ACTIONS(26), 2,
      sym_wildcard,
      sym_var,
    STATE(24), 3,
      sym__expr,
      sym_atom,
      sym_string,
  [92] = 8,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(28), 1,
      anon_sym_DQUOTE,
    ACTIONS(30), 1,
      anon_sym_RPAREN,
    STATE(10), 1,
      sym__quoted_atom,
    ACTIONS(32), 2,
      sym_wildcard,
      sym_var,
    STATE(31), 3,
      sym__expr,
      sym_atom,
      sym_string,
  [120] = 7,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(28), 1,
      anon_sym_DQUOTE,
    STATE(10), 1,
      sym__quoted_atom,
    ACTIONS(34), 2,
      sym_wildcard,
      sym_var,
    STATE(26), 3,
      sym__expr,
      sym_atom,
      sym_string,
  [145] = 7,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(28), 1,
      anon_sym_DQUOTE,
    STATE(10), 1,
      sym__quoted_atom,
    ACTIONS(36), 2,
      sym_wildcard,
      sym_var,
    STATE(38), 3,
      sym__expr,
      sym_atom,
      sym_string,
  [170] = 6,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    STATE(10), 1,
      sym__quoted_atom,
    STATE(35), 1,
      sym_atom,
    STATE(36), 1,
      sym_function_clause,
  [189] = 6,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(38), 1,
      sym__raw_atom,
    ACTIONS(40), 1,
      anon_sym_module,
    STATE(10), 1,
      sym__quoted_atom,
    STATE(48), 1,
      sym_atom,
  [208] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(42), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [219] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(46), 1,
      anon_sym_COMMA,
    STATE(11), 1,
      aux_sym_arg_list_repeat1,
    ACTIONS(44), 3,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
  [234] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(49), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [245] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(51), 5,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [256] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(53), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [266] = 4,
    ACTIONS(55), 1,
      anon_sym_DQUOTE,
    ACTIONS(60), 1,
      sym__comment,
    STATE(15), 1,
      aux_sym_string_repeat1,
    ACTIONS(57), 2,
      aux_sym_string_token1,
      sym__escape,
  [280] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(62), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [290] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(64), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [300] = 4,
    ACTIONS(60), 1,
      sym__comment,
    ACTIONS(66), 1,
      anon_sym_SQUOTE,
    STATE(23), 1,
      aux_sym__quoted_atom_repeat1,
    ACTIONS(68), 2,
      aux_sym__quoted_atom_token1,
      sym__escape,
  [314] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(70), 4,
      ts_builtin_sym_end,
      anon_sym_DASH,
      sym__raw_atom,
      anon_sym_SQUOTE,
  [324] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(72), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [334] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(76), 1,
      anon_sym_COMMA,
    STATE(11), 1,
      aux_sym_arg_list_repeat1,
    ACTIONS(74), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [348] = 5,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(7), 1,
      sym__raw_atom,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    STATE(10), 1,
      sym__quoted_atom,
    STATE(43), 1,
      sym_atom,
  [364] = 4,
    ACTIONS(60), 1,
      sym__comment,
    ACTIONS(78), 1,
      anon_sym_SQUOTE,
    STATE(23), 1,
      aux_sym__quoted_atom_repeat1,
    ACTIONS(80), 2,
      aux_sym__quoted_atom_token1,
      sym__escape,
  [378] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(76), 1,
      anon_sym_COMMA,
    STATE(21), 1,
      aux_sym_arg_list_repeat1,
    ACTIONS(83), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [392] = 4,
    ACTIONS(60), 1,
      sym__comment,
    ACTIONS(85), 1,
      anon_sym_DQUOTE,
    STATE(15), 1,
      aux_sym_string_repeat1,
    ACTIONS(87), 2,
      aux_sym_string_token1,
      sym__escape,
  [406] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(44), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [416] = 4,
    ACTIONS(60), 1,
      sym__comment,
    ACTIONS(89), 1,
      anon_sym_DQUOTE,
    STATE(25), 1,
      aux_sym_string_repeat1,
    ACTIONS(91), 2,
      aux_sym_string_token1,
      sym__escape,
  [430] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(93), 4,
      anon_sym_RPAREN,
      anon_sym_DOT,
      anon_sym_SEMI,
      anon_sym_COMMA,
  [440] = 4,
    ACTIONS(60), 1,
      sym__comment,
    ACTIONS(95), 1,
      anon_sym_SQUOTE,
    STATE(18), 1,
      aux_sym__quoted_atom_repeat1,
    ACTIONS(97), 2,
      aux_sym__quoted_atom_token1,
      sym__escape,
  [454] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(99), 1,
      anon_sym_DOT,
    ACTIONS(101), 1,
      anon_sym_SEMI,
    STATE(34), 1,
      aux_sym_function_repeat1,
  [467] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(76), 1,
      anon_sym_COMMA,
    ACTIONS(103), 1,
      anon_sym_RPAREN,
    STATE(33), 1,
      aux_sym_arg_list_repeat1,
  [480] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(101), 1,
      anon_sym_SEMI,
    ACTIONS(105), 1,
      anon_sym_DOT,
    STATE(30), 1,
      aux_sym_function_repeat1,
  [493] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(76), 1,
      anon_sym_COMMA,
    ACTIONS(107), 1,
      anon_sym_RPAREN,
    STATE(11), 1,
      aux_sym_arg_list_repeat1,
  [506] = 4,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(109), 1,
      anon_sym_DOT,
    ACTIONS(111), 1,
      anon_sym_SEMI,
    STATE(34), 1,
      aux_sym_function_repeat1,
  [519] = 3,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(114), 1,
      anon_sym_LPAREN,
    STATE(47), 1,
      sym_arg_list,
  [529] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(109), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [537] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(116), 2,
      anon_sym_DOT,
      anon_sym_SEMI,
  [545] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(118), 1,
      anon_sym_RPAREN,
  [552] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(120), 1,
      anon_sym_DOT,
  [559] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(122), 1,
      anon_sym_DOT,
  [566] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(124), 1,
      anon_sym_DASH_GT,
  [573] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(126), 1,
      anon_sym_LPAREN,
  [580] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(128), 1,
      anon_sym_RPAREN,
  [587] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(130), 1,
      anon_sym_DASH_GT,
  [594] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(132), 1,
      ts_builtin_sym_end,
  [601] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(134), 1,
      anon_sym_DASH_GT,
  [608] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(136), 1,
      anon_sym_DASH_GT,
  [615] = 2,
    ACTIONS(3), 1,
      sym__comment,
    ACTIONS(138), 1,
      anon_sym_LPAREN,
};

static uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 32,
  [SMALL_STATE(4)] = 64,
  [SMALL_STATE(5)] = 92,
  [SMALL_STATE(6)] = 120,
  [SMALL_STATE(7)] = 145,
  [SMALL_STATE(8)] = 170,
  [SMALL_STATE(9)] = 189,
  [SMALL_STATE(10)] = 208,
  [SMALL_STATE(11)] = 219,
  [SMALL_STATE(12)] = 234,
  [SMALL_STATE(13)] = 245,
  [SMALL_STATE(14)] = 256,
  [SMALL_STATE(15)] = 266,
  [SMALL_STATE(16)] = 280,
  [SMALL_STATE(17)] = 290,
  [SMALL_STATE(18)] = 300,
  [SMALL_STATE(19)] = 314,
  [SMALL_STATE(20)] = 324,
  [SMALL_STATE(21)] = 334,
  [SMALL_STATE(22)] = 348,
  [SMALL_STATE(23)] = 364,
  [SMALL_STATE(24)] = 378,
  [SMALL_STATE(25)] = 392,
  [SMALL_STATE(26)] = 406,
  [SMALL_STATE(27)] = 416,
  [SMALL_STATE(28)] = 430,
  [SMALL_STATE(29)] = 440,
  [SMALL_STATE(30)] = 454,
  [SMALL_STATE(31)] = 467,
  [SMALL_STATE(32)] = 480,
  [SMALL_STATE(33)] = 493,
  [SMALL_STATE(34)] = 506,
  [SMALL_STATE(35)] = 519,
  [SMALL_STATE(36)] = 529,
  [SMALL_STATE(37)] = 537,
  [SMALL_STATE(38)] = 545,
  [SMALL_STATE(39)] = 552,
  [SMALL_STATE(40)] = 559,
  [SMALL_STATE(41)] = 566,
  [SMALL_STATE(42)] = 573,
  [SMALL_STATE(43)] = 580,
  [SMALL_STATE(44)] = 587,
  [SMALL_STATE(45)] = 594,
  [SMALL_STATE(46)] = 601,
  [SMALL_STATE(47)] = 608,
  [SMALL_STATE(48)] = 615,
};

static TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [13] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [15] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [17] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(10),
  [20] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(9),
  [23] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(29),
  [26] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [28] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [30] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [32] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [34] = {.entry = {.count = 1, .reusable = false}}, SHIFT(26),
  [36] = {.entry = {.count = 1, .reusable = false}}, SHIFT(38),
  [38] = {.entry = {.count = 1, .reusable = false}}, SHIFT(10),
  [40] = {.entry = {.count = 1, .reusable = false}}, SHIFT(42),
  [42] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_atom, 1),
  [44] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_arg_list_repeat1, 2),
  [46] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_arg_list_repeat1, 2), SHIFT_REPEAT(6),
  [49] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__quoted_atom, 3),
  [51] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__quoted_atom, 2),
  [53] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 3),
  [55] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_string_repeat1, 2),
  [57] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(15),
  [60] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [62] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_attribute, 6),
  [64] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_attribute, 6),
  [66] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [68] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [70] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function, 2),
  [72] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3),
  [74] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_block, 2),
  [76] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [78] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym__quoted_atom_repeat1, 2),
  [80] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__quoted_atom_repeat1, 2), SHIFT_REPEAT(23),
  [83] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_block, 1),
  [85] = {.entry = {.count = 1, .reusable = false}}, SHIFT(20),
  [87] = {.entry = {.count = 1, .reusable = false}}, SHIFT(15),
  [89] = {.entry = {.count = 1, .reusable = false}}, SHIFT(28),
  [91] = {.entry = {.count = 1, .reusable = false}}, SHIFT(25),
  [93] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 2),
  [95] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [97] = {.entry = {.count = 1, .reusable = false}}, SHIFT(18),
  [99] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [101] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [103] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [105] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [107] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [109] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2),
  [111] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_function_repeat1, 2), SHIFT_REPEAT(8),
  [114] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [116] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_function_clause, 4),
  [118] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [120] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [122] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [124] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 3),
  [126] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [128] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [130] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 4),
  [132] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [134] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_list, 2),
  [136] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [138] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
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
