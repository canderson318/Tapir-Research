#replace \ with /


string = input("enter string: ")
converted_string = string.replace('\\', "/")
converted_string = string.replace('\\\\', '/')

converted_string = string.replace("//", '/')
print("\n", converted_string, "\n")
