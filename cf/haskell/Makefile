all : cf_parse

cf_lex.hs : cf_lex.x
	alex cf_lex.x

cf_parse.hs : cf_parse.y
	happy cf_parse.y

cf_parse : cf_lex.hs cf_parse.hs cf.hs
	ghc --make -XScopedTypeVariables cf_lex.hs cf_parse.hs cf.hs

clean :
	@rm -f cf_lex.h[si] cf_lex.o cf_parse.h[si] cf_parse.o cf.hi cf.o
