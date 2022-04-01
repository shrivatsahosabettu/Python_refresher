import basic_1

while True:
    text = input('Basic > ')
    result, error = basic_1.run('<stdin>', text)

    if error: print(error.as_string())
    else: print(result)