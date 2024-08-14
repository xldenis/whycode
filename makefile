all:
	dune build
	mv -f _build/default/bin/main.exe extension/whycode2

clean:
	dune clean
