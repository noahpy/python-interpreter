
# this comment is supported

def faculty(n):
    if n == 0:
        return 1
    elif n > 20:
        print("too big!")
        return 0
    else:
        return n * faculty(n - 1)


def many_faculties_memoize(n, f):
    res = []
    mem = {}
    for i in range(n):
        if i in mem:
            tmp = mem[i]
        else:
            tmp = f(i)
            mem[i] = tmp
        res = res + [f(i)]
    return res


num_computations = 10
final_result = many_faculties_memoize(num_computations, faculty)
print(final_result)
