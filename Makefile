BINARY_PATH	:=	$(shell stack path --local-install-root)
NAME 				= glados

all:
	stack build --install-ghc --allow-different-user
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	stack purge
	rm -f $(NAME)

tests_run:
	stack test --coverage --allow-different-user

re: fclean all

.PHONY: all build exec clean fclean tests_run re