BINARY_PATH	:=	$(shell stack path --local-install-root)
NAME 				= glados

all:
	stack build --install-ghc
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	stack purge
	rm -f $(NAME)

tests_run:
	stack test --coverage
	# ./test/Functionnal/Functionnal.sh


re: fclean all

.PHONY: all build exec clean fclean tests_run re