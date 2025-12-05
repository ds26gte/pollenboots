all:
	make clean
	raco pollen render index.ptree

tiny:
	make clean
	raco pollen render tiny-index.ptree

small:
	make clean
	raco pollen render small-index.ptree

clean:
	find . -name compiled -type d -exec rm -fr {} \; || true
	find . -name \*.html -delete || true
	raco pollen render styles.css.pp
