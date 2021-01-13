##
## EPITECH PROJECT, 2018
## infinadd
## File description:
## Makefile
##
# package = my-project

executable_path  := $(shell stack path --local-install-root)

NAME	=	koak

all:
	stack build
	cp $(executable_path)/bin/$(NAME) .

clean:
	rm -rf src/*.o
	rm -rf src/*.hi
	rm -rf *.gc*
	rm -rf *vg*
	stack clean

tests_run: clean
	stack test
#		stack test --coverage

tests_bash: clean
	./tests/test.sh

tests_bash_chmod: clean
	chmod +x tests/test.sh && ./tests/test.sh

fclean:	clean

re:	fclean all

.PHONY: re clean fclean tests_run
