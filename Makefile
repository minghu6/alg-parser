
CARGO_TEST_FLAGS=--show-output

test-example-parser:
	cargo test --example parser_demo -- $(CARGO_TEST_FLAGS)

test-example-regex:
	cargo test --example regex_demo -- $(CARGO_TEST_FLAGS)

test-example: run-example-parser run-example-regex

test-algs:
	cargo test algs::gram -- $(CARGO_TEST_FLAGS)
