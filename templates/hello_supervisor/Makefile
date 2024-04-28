ERLC_OPTIONS ?= -Wall -Werror

ERL_SRC = $(wildcard src/*.erl)
ERL_OBJ = $(patsubst src/%.erl,ebin/%.beam,$(ERL_SRC))

RELEASE = hello

all: build

build: $(ERL_OBJ)

ebin/%.beam: src/%.erl
	erlc $(ERLC_OPTIONS) -o $(dir $@) $<

clean:
	$(RM) $(wildcard ebin/*.beam) $(RELEASE).boot $(RELEASE).script $(RELEASE).tar.gz

$(RELEASE).boot: $(RELEASE).rel ebin/$(RELEASE).app
	./generate-boot $(RELEASE)

release: $(RELEASE).tar.gz

$(RELEASE).tar.gz: $(ERL_OBJ) $(RELEASE).boot
	./generate-tarball $(RELEASE)

.PHONY: all build clean
