

def faculty(n):
    if n == 0:
        return 1
    else:
        return n * faculty(n - 1)


def many_faculties(n):
    res = []
    for i in range(n):
        res = res + [faculty(i)]
    return res


final_result = many_faculties(10)
print(final_result)
