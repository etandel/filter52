BUILD_DIR=dist

DISTRIB=$(BUILD_DIR)/filter52

all: $(DISTRIB)

filter52:
	cabal build --builddir=$(BUILD_DIR)

$(DISTRIB): filter52 
	mkdir --parent $@

	cp "dist/build/filter52/filter52" $@
	cp "src/run-arch.sh" $@
	cp "README.md" $@

	tar -cvzf "$(BUILD_DIR)/filter52.tar.gz" $(DISTRIB)

clean:
	cabal clean
