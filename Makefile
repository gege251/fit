lint:
	hlint src

format:
	fourmolu --mode inplace $(shell git ls-files '*.hs')
	
