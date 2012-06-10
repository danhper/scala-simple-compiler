SRC = src
OUT = bin
DOC = doc
SOURCES = $(shell ls $(SRC)/*.scala)
S = scala
SFLAGS = -cp $(OUT)
SC = scalac
SCFLAGS = -cp $(OUT) -d $(OUT)
SD = scaladoc
SDFLAGS = -d $(DOC)

all:
	@test -d $(OUT) || mkdir $(OUT) 
	@echo "Compiling scala files"
	@$(SC) $(SCFLAGS) $(SOURCES)

run:
	@$(S) $(SFLAGS) Main

.PHONY: doc clean

doc:
	@echo "Making doc"
	@$(SD) $(SDFLAGS) $(SOURCES)

clean:
	@$(RM) $(OUT)/*.class