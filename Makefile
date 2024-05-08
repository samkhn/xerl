BUILD_DIR=_build/

all: compile

setup:
	mkdir -p $(BUILD_DIR)

compile: setup
	erlc *.erl
	mv *.beam $(BUILD_DIR)

shell: compile 
	erl -pa $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR)

.PHONY: clean
