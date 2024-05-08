BUILD_DIR = _build/

ERLC_FLAGS ?= -Wall -Werror

all: compile

setup:
	mkdir -p $(BUILD_DIR)

compile: setup
	erlc $(ERLC_FLAGS) src/*.erl
	mv *.beam $(BUILD_DIR)

shell: compile
	erl -pa $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR)

.PHONY: clean
