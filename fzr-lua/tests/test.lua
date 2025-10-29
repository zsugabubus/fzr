#!/bin/luajit
local function pretty(x, prefix)
	if type(x) == "table" then
		local s = ""
		for k, v in pairs(x) do
			s = s .. "\n" .. prefix .. "  [" .. pretty(k, "") .. "]=" .. pretty(v, prefix .. "  ") .. ","
		end
		return "{" .. s .. "\n" .. prefix .. "}"
	elseif type(x) == "string" then
		return ("%q"):format(x)
	else
		return tostring(x)
	end
end

local function assert_eq(left, right)
	left = pretty(left, "")
	right = pretty(right, "")
	return assert(left == right, ("assertion `left == right` failed\n  left: %s\n right: %s"):format(left, right))
end

local fzr = require("fzr")

do
	assert(not pcall(function()
		assert_eq(true, false)
	end))
	assert_eq(pretty({ "false", false }, ""), '{\n  [1]="false",\n  [2]=false,\n}')
end

do
	assert(fzr.create_needle("abc"))
	assert(fzr.create_needle(""))
	assert(fzr.create_needle(("a"):rep(1000)))
end

do
	assert(fzr.create_searcher())
	assert(fzr.create_searcher({}))
	assert(fzr.create_searcher({ "a", "b", "c" }))
	assert(fzr.create_searcher({ ("a"):rep(10000) }))
end

do
	assert(fzr.create_memory())
end

do
	assert_eq({ fzr.find({}) }, { false, "invalid needle" })
	assert_eq({ fzr.find(fzr.create_needle(""), {}) }, { false, "invalid searcher" })
	assert_eq({ fzr.find(fzr.create_needle(""), fzr.create_searcher({})) }, { false, "invalid memory" })
end

do
	local s = fzr.create_searcher({ "a", "b" })
	local m = fzr.create_memory()
	assert_eq(fzr.find(fzr.create_needle("a"), s, m), { "a" })
	assert_eq(fzr.find(fzr.create_needle("b"), s, m), { "b" })
	assert_eq(fzr.find(fzr.create_needle("c"), s, m), {})
	assert_eq(fzr.find(fzr.create_needle("a"), s, m), { "a" })
end

do
	assert_eq(
		fzr.find(fzr.create_needle("a"), fzr.create_searcher({ "a", "x", "á" }), fzr.create_memory()),
		{ "a", "á" }
	)
end

do
	local function test_find(needle, yes, no)
		local n = fzr.create_needle(needle)
		local s = fzr.create_searcher({ yes, no })
		local m = fzr.create_memory()
		assert_eq(fzr.find(fzr.create_needle(""), s, m), { yes, no })
		return assert_eq(fzr.find(n, s, m), { yes })
	end

	test_find("a", "Á", "b")
	test_find("ab", "xaxBx")
	test_find("aB", "xaxBx", "xaxbx")
	test_find("ao", "xÁxÓx")
	test_find("m", "[0m", "\x1b[0m")

	test_find("^a", "a", "xa")
	test_find("^ab", "abx", "axb")
	test_find("^ao", "ÁÓx")
	test_find("^ ab", "xaxbx")

	test_find("'a", "a", "b")
	test_find("'ab", "ab", "a")
	test_find("'ab", "xabx", "axb")
	test_find("'ao", "xÁÓx")
	test_find("' ab", "xaxbx")
end

do
	local s = fzr.create_searcher({ "aa", "a", "aaa" })
	local m = fzr.create_memory()

	local function test_limit_with_empty_needle(limit, output)
		return assert(fzr.find(fzr.create_needle(""), s, m, { limit = limit }), output)
	end
	test_limit_with_empty_needle(0, {})
	test_limit_with_empty_needle(1, { "aa" })
	test_limit_with_empty_needle(2, { "aa", "a" })
	test_limit_with_empty_needle(3, { "aa", "a", "aaa" })
	test_limit_with_empty_needle(4, { "aa", "a", "aaa" })
	test_limit_with_empty_needle(nil, { "aa", "a", "aaa" })

	local function test_limit_with_nonempty_needle(limit, output)
		return assert(fzr.find(fzr.create_needle("a"), s, m, { limit = limit }), output)
	end
	test_limit_with_nonempty_needle(0, {})
	test_limit_with_nonempty_needle(1, { "a" })
	test_limit_with_nonempty_needle(2, { "a", "aa" })
	test_limit_with_nonempty_needle(3, { "a", "aa", "aaa" })
	test_limit_with_nonempty_needle(4, { "a", "aa", "aaa" })
	test_limit_with_nonempty_needle(nil, { "a", "aa", "aaa" })
end

do
	local function test_result_syntax(result, error_msg)
		local n = fzr.create_needle("")
		local s = fzr.create_searcher({})
		local m = fzr.create_memory()
		return assert_eq({ fzr.find(n, s, m, { result = result }) }, { false, error_msg })
	end
	test_result_syntax("", "invalid result: empty")
	test_result_syntax("x", "invalid result: invalid character")
	test_result_syntax("ix", "invalid result: invalid character")
	test_result_syntax("{", "invalid result: expected '}' before end of string")
	test_result_syntax("{}", "invalid result: invalid '{}'")
	test_result_syntax("{i", "invalid result: expected '}' before end of string")
	test_result_syntax("{x", "invalid result: invalid character")
	test_result_syntax("{ix", "invalid result: invalid character")
	test_result_syntax("{x}", "invalid result: invalid character")
	test_result_syntax("{ix}", "invalid result: invalid character")
end

do
	local function test_result(result, output)
		local n = fzr.create_needle("aa")
		local s = fzr.create_searcher({ "aa", "x", "xaxá" })
		local m = fzr.create_memory()
		return assert_eq(fzr.find(n, s, m, { result = result }), output)
	end
	test_result(nil, { "aa", "xaxá" })
	test_result("i", { 1, 3 })
	test_result("iv", { 1, "aa", 3, "xaxá" })
	test_result("{vi{p}P}", {
		{ "aa", 1, { { 0, 1 }, { 1, 2 } }, 0, 1, 1, 2 },
		{ "xaxá", 3, { { 1, 2 }, { 3, 5 } }, 1, 2, 3, 5 },
	})
end
