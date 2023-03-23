/*
 *	Keywords list for mVim.
 *	This file is in the public domain.
 */

wchar_t *__cKeywords[] = {
	L"int", L"long", L"short", L"union", L"struct", L"typedef", L"if",
	L"for", L"do", L"while", L"void", L"auto", L"static", L"bool",
	L"_Bool", L"case", L"break", L"continue", L"extern", L"char",
	L"wchar_t", L"float", L"double", L"return", L"default", L"goto",
	L"inline", L"enum",
	NULL
};

wchar_t *__luaKeywords[] = {
	L"function", L"local", L"for", L"in", L"return" ,L"if", L"else",
	L"elseif", L"do", L"end", L"then", L"break", L"goto", NULL
};

wchar_t *__shellKeywords[] = {
	L"if", L"then", L"fi", L"exit", L"case", L"break", L"test",
	L"for", L"do", L"done", L"in", L"while", L"return", L"elif",
	L"else", NULL
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
	{ NULL, NULL },
};
