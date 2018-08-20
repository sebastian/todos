build:
	stack build --test --fast --haddock-deps

ghcid-test:
	stack exec -- ghcid --command="stack ghci todos:lib todos:test:spec --ghci-options=-fobject-code" --test "main"

watch:
	stack build --test --fast --haddock-deps --file-watch

gen-docs:
	stack hoogle -- generate --local

docs:
	stack hoogle -- server --local --port=8080 &
	open http://localhost:8080

support:
	# stack build --copy-compiler-tool hoogle weeder ghcid hlint stylish-haskell hdevtools
	stack install weeder hlint stylish-haskell hdevtools
