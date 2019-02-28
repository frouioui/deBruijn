PROJECT_FOLDER	=	./deBruijn

PROJECT_NAME	=	deBruijn

SCRIPT_PATH	=	script

COMPILE_SCRIPT_FILENAME	=	compile.sh

all: give_script_right execute_script

give_script_right:
	chmod +x ./$(SCRIPT_PATH)/$(COMPILE_SCRIPT_FILENAME)

execute_script:
	cd $(SCRIPT_PATH) && ./$(COMPILE_SCRIPT_FILENAME)
	cd ..

tests_run:
	stack test

tests_run_coverage:
	stack test --coverage

clean:
	rm $(PROJECT_NAME) || true

fclean: clean

re: fclean all