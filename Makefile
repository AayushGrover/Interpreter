evaluate : expression.cmx parser.cmx lexer.cmx evaluate.cmx interpreter.cmx
	ocamlopt -o evaluate expression.cmx interpreter.cmx parser.cmx lexer.cmx evaluate.cmx 

parser.ml : parser.mly
	ocamlyacc parser.mly

parser.mli : parser.mly
	ocamlyacc parser.mly

lexer.ml : lexer.mll parser.mli
	ocamllex lexer.mll

parser.cmx : expression.cmi parser.cmi parser.ml
	ocamlopt -c parser.ml

parser.cmi : expression.cmi parser.mli
	ocamlc -c parser.mli

lexer.cmx :  parser.cmi lexer.ml
	ocamlopt -c lexer.ml

expression.cmi : expression.mli
	ocamlc -c expression.mli

expression.cmx : expression.cmi expression.ml
	ocamlopt -c expression.ml

interpreter.cmi : expression.cmi interpreter.mli
	ocamlc -c interpreter.mli

interpreter.cmx : expression.cmi interpreter.cmi interpreter.ml
	ocamlopt -c interpreter.ml

evaluate.cmx :  parser.cmi lexer.cmx expression.cmi interpreter.cmi evaluate.ml
	ocamlopt -c evaluate.ml

clean:
	rm *.cmx *.cmi *.o evaluate lexer.ml parser.ml parser.mli
