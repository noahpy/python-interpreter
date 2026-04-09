

def faculty(n):
    if n == 0:
        return 1
    elif n > 20:
        print("too big!")
        return 0
    else:
        return n * faculty(n - 1)


def many_faculties(n, f):
    res = []
    for i in range(n):
        res = res + [f(i)]
    return res

num_computations = int(input("Enter a number: "))
final_result = many_faculties(num_computations, faculty)
print(final_result)
