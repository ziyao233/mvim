--[[
	mvim
	File: /syntax/C.syntax.lua
	Date: 2023.03.22
	By MIT License.
	Copyright (c) 2023 Ziyao.
]]

return {
	suffix = ".c",
	keywords = {
		{},						-- Black
		{ "true", "false" },				-- Red
		{						-- Green
			"unsigned", "int", "char", "long",
			"double", "float", "typedef", "struct",
			"auto", "static", "void", "_Bool",
		},
		{						-- Yellow
			"do", "while", "for", "if", "else",
			"return", "goto", "case", "default",
		},
		{},						-- Blue
		{},						-- Mangenta
		{},						-- Cyan
		{},						-- White
	},
};
