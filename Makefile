install-lua :
	install -vD target/release/libfzr_lua.so /usr/local/lib/lua/5.1/fzr.so

update-unicode :
	fzr/unicode_variations.py > fzr/src/unicode.rs
	git commit -m 'feat: update generated Unicode variations' -- fzr/src/unicode.rs
