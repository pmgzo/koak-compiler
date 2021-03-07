do a function that evaluate the depth in order to get the value of the escape block

escapeBlock :: Maybe Name

now each time we decide to ret we have to first call a function that takes:
Maybe Operand and check if the state as a escapeName

when launch another StateT we have to initialize the escape name
