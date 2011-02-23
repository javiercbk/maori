EBIN_DIR := ebin
SRC_DIR := src/server
INCLUDE_DIR := include
ERLC := erlc
ERLC_FLAGS := -W -I $(INCLUDE_DIR) -o $(EBIN_DIR)

all:
	@mkdir -p $(EBIN_DIR)
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/response.erl
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/otgeneral.erl
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/otins.erl
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/otdel.erl
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/otrel.erl
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/otbrk.erl
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/protocol.erl
#	@cp $(SRC_DIR)/server/misultin.app.src $(EBIN_DIR)/misultin.app	

clean:
	@rm -rf $(EBIN_DIR)/*
	@rm -f erl_crash.dump

debug:
	@mkdir -p $(EBIN_DIR)
	$(ERLC) -D log_debug $(ERLC_FLAGS) $(SRC_DIR)/*.erl
#	@cp $(SRC_DIR)/misultin.app.src $(EBIN_DIR)/misultin.app