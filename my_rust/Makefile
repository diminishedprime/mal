#####################

SOURCES_BASE = src/eval.rs src/lisp_val.rs src/main.rs src/parser.rs src/read.rs
SOURCES_LISP = src/env.rs src/core.rs src/bin/stepA_mal.rs
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#####################

SRCS = step0_repl.rs step1_read_print.rs step2_eval.rs step3_env.rs \
			 step4_if_fn_do.rs step5_tco.rs step6_file.rs step7_quote.rs \
			 step8_macros.rs step9_try.rs stepA_mal.rs
BINS = $(SRCS:%.rs=target/release/%)

#####################

all: $(BINS)

dist: mal

mal: target/release/stepA_mal
	cp $< $@

# TODO: would be nice to build just the step requested
$(BINS): target/release/%: src/bin/%.rs $(wildcard src/*.rs)
	cargo build --release

clean:
	cargo clean
	rm -f mal

.PHONY: stats stats-lisp mal

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
