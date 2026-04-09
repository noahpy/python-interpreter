

def faculty(n):
    if n == 0:
        return 1
    elif n > 20:
        print("too big!")
        return 0
    else:
        return n * faculty(n - 1)


def many_faculties(n):
    res = []
    for i in range(n):
        res = res + [faculty(i)]
    return res


final_result = many_faculties(int(input("Enter a number: ")))
print(final_result)
