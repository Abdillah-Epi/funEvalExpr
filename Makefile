##
## EPITECH PROJECT, 2020
## FUN_imageCompressor_2019
## File description:
## Makefile
##

BIN		=	funEvalExpr

all:
	stack build --copy-bins --local-bin-path .
	mv funEvalExpr-exe funEvalExpr

re:	fclean all

clean:
	stack clean --allow-different-user

fclean:	clean
	rm -f $(BIN)

.PHONY:	all clean fclean re
