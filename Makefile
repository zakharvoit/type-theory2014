# This makefile uses ocamlbuild to build all executables

SRC_DIR = src
EXECUTABLES = task1 task2 task3 task4
NATIVES = $(EXECUTABLES:%=$(SRC_DIR)/%.native)
INCLUDE_DIRS = src
GENERATE_TASKS = $(TEST_TASKS:%.test=%.generate)

all: build

build: $(NATIVES)

%.native:
	ocamlbuild -Is $(INCLUDE_DIRS) $@

clean:
	rm -rf _build
	rm -f *.native

.PHONY: all build clean generate test
