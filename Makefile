EBIN_DIR := ebin
SRC_DIR := src/server
TEST_DIR := test/server
INCLUDE_DIR := include
ERLC := erlc
ERLC_FLAGS := -W -I $(INCLUDE_DIR) -o $(EBIN_DIR)

all:
	@mkdir -p $(EBIN_DIR)
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/response.erl
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/otgeneral.erl
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/otins.erl
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/otnop.erl
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/otdel.erl
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/otrel.erl
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/otbrk.erl
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/protocol.erl
test: debug
	$(ERLC) $(ERLC_FLAGS) $(TEST_DIR)/protocol_SUITE.erl
runtest: test
	erl -noshell -pa $(EBIN_DIR) -eval "eunit:test(protocol_SUITE, [verbose])" -s init stop

clean:
	@rm -rf $(EBIN_DIR)/*
	@rm -f erl_crash.dump

debug:
	@mkdir -p $(EBIN_DIR)
	$(ERLC) -D log_debug $(ERLC_FLAGS) $(SRC_DIR)/response.erl
	$(ERLC) -D log_debug $(ERLC_FLAGS) $(SRC_DIR)/otgeneral.erl
	$(ERLC) -D log_debug $(ERLC_FLAGS) $(SRC_DIR)/otins.erl
	$(ERLC) -D log_debug $(ERLC_FLAGS) $(SRC_DIR)/otdel.erl
	$(ERLC) -D log_debug $(ERLC_FLAGS) $(SRC_DIR)/otnop.erl
	$(ERLC) -D log_debug $(ERLC_FLAGS) $(SRC_DIR)/otrel.erl
	$(ERLC) -D log_debug $(ERLC_FLAGS) $(SRC_DIR)/otbrk.erl
	$(ERLC) -D log_debug $(ERLC_FLAGS) $(SRC_DIR)/protocol.erl