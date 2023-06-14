/*
 *	Keywords list for mVim.
 *	This file is in the public domain.
 */

wchar_t *__cKeywords[] = {
	L"int", L"long", L"short", L"union", L"struct", L"typedef", L"if",
	L"for", L"do", L"while", L"void", L"auto", L"static", L"bool",
	L"_Bool", L"case", L"break", L"continue", L"extern", L"char",
	L"wchar_t", L"float", L"double", L"return", L"default", L"goto",
	L"inline", L"enum", L"const", L"else", L"NULL", L"unsigned",
	NULL
};

wchar_t *__luaKeywords[] = {
	L"function", L"local", L"for", L"in", L"return" ,L"if", L"else",
	L"elseif", L"do", L"end", L"then", L"break", L"goto", L"while",
	L"true", L"false", L"and", L"or", L"not", L"nil",
	NULL
};

wchar_t *__shellKeywords[] = {
	L"if", L"then", L"fi", L"exit", L"case", L"break", L"test",
	L"for", L"do", L"done", L"in", L"while", L"return", L"elif",
	L"else", L"esac",
	NULL
};

wchar_t *__cppKeywords[] = {
	L"int", L"long", L"short", L"union", L"struct", L"typedef", L"if",
	L"for", L"do", L"while", L"void", L"auto", L"static", L"bool",
	L"class", L"case", L"break", L"continue", L"extern", L"char",
	L"wchar_t", L"float", L"double", L"return", L"default", L"goto",
	L"inline", L"enum", L"const", L"else", L"NULL", L"unsigned",
	L"template", L"namespace", L"throw", L"noexcept", L"new", L"delete",
	L"class", L"operator", L"try", L"false", L"true",
	NULL
};

wchar_t *__rclangKeywords[] = {
	L"val", L"sal", L"s8", L"u8", L"s16", L"u16", L"s32", L"u32",
	L"s64", L"u64", L"for", L"fn", L"if", L"else", L"dcl", L"ret",
	L"break", L"export", L"szo", L"ptr",
	NULL
};

wchar_t *__golangKeywords[] = {
	L"go", L"func", L"int", L"uint", L"byte", L"int8", L"int16", L"int32",
	L"int64", L"uint8", L"uint16", L"uint32", L"uint64", L"string", L"map",
	L"make", L"len", L"struct", L"type", L"import", L"return", L"package",
	L"if", L"for", L"else", L"select", L"switch", L"chan", L"bool", L"true",
	L"false", L"continue", L"break", L"nil", L"case", L"var",
	NULL
};

wchar_t *__elmKeywords[] = {
	L"module", L"Cmd", L"Sub", L"List", L"type", L"alias", L"case", L"of",
	L"import", L"module", L"exposing", L"as", L"String", L"Int", L"if",
	L"then", L"else", L"Maybe", L"Result", L"Just", L"Nothing", L"True",
	L"False", L"Err", L"Ok", L"Bool",
	NULL
};

/*
 *	K[] must be defined as an array, which contains (char*, wchar**) pairs,
 *	representing the suffix of a type of file and keywords used in the
 *	type of file.
 */

Keyword_Class K[] = {
	{ ".c", __cKeywords },
	{ ".h", __cKeywords },
	{ ".lua", __luaKeywords },
	{ ".sh", __shellKeywords },
	{ ".cpp", __cppKeywords },
	{ ".hpp", __cppKeywords },
	{ ".rcs", __rclangKeywords },
	{ ".go", __golangKeywords },
	{ ".elm", __elmKeywords },
	{ NULL, NULL },
};
