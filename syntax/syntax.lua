#!/usr/bin/env lua

--[[
	mVim
	Compile Syntax File to C Representation
	File: /syntax/syntax.lua
	Date: 2023.03.19
	By MIT License.
	Copyright (c) 2023 Ziyao.
]]

local io			= require "io";
local string			= require "string";
local os			= require "os";
local utf8			= require "utf8";

local function build(words)
	local trie, count, list, revList, accept = {}, 2, {}, {}, {};
	list[1]		= trie;
	revList[trie]	= 1;
	for _, word in pairs(words)
	do
		local now = trie;
		for p, c in utf8.codes(word)
		do
			if not now[c]
			then
				now[c]		= {};
				list[count]	= now[c];
				revList[now[c]]	= count;
				count = count + 1;
			end
			now = now[c];
		end
		accept[now] = #word;
	end

	local fail, queue = { trie }, {};
	local head, tail = 1, 1;
	for c, follow in pairs(trie)
	do
		queue[tail] = follow;
		fail[revList[follow]] = trie;
		tail = tail + 1;
	end

	while tail > head
	do
		local this = queue[head];
		head = head + 1;

		local last = this;
		for c, follow in pairs(this)
		do
			repeat
				last = fail[revList[last]];
			until last == trie or last[c];

			fail[revList[follow]] = last[c] or trie;
			queue[tail] = follow;
			tail = tail + 1;
		end
	end

	return { fail = fail, list = list, revList = revList, accept = accept};
end

local function serialise(automata)
	local buf = { "{", "{0," };

	for i, v in ipairs(automata.fail)
	do
		table.insert(buf, ("%d,"):format(automata.revList[v]));
	end
	table.insert(buf, "},\n{");

	table.insert(buf, "{},\n");
	for i, v in ipairs(automata.list)
	do
		table.insert(buf, "{");
		for c, n in pairs(v)
		do
			table.insert(buf, ("{%d, %d}, "):
					  format(c, automata.revList[n]));
		end
		table.insert(buf, "{ 0, 0 }");
		if automata.accept[v]
		then
			table.insert(buf, (", %d"):format(automata.accept[v]));
		end
		table.insert(buf ,"},\n");
	end

	table.insert(buf, "}},\n");
	return table.concat(buf);
end

if #arg ~= 2
then
	io.stderr:write(("Usage: %s input output\n"):
			format(arg[0]));
	os.exit(-1);
end

local inputPath, outputPath = arg[1], arg[2];
local outputFile = assert(io.open(outputPath, "w"));

local function split(s)
	local l = {};
	for word in s:gmatch("(%S+)%s*")
	do
		table.insert(l, word);
	end
	return l;
end

local syntax = assert(loadfile(inputPath))();

outputFile:write(("{.suffix = \"%s\", automata = {\n"):
		 format(syntax.suffix));

local lines = {};
for _, words in ipairs(syntax.keywords)
do
	outputFile:write(#words == 0		and
		 	 "NULL,"			or
			 serialise(build(words)));
end

outputFile:write("}},\n");
outputFile:close();
