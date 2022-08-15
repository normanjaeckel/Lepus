import json
import random


def main():
    pupils = []
    for i in range(1,81):
        k = "P"
        if i < 10:
            k += "0"
        k += str(i)
        v = []
        groups = set()
        for i in range(3):
            group = "Q"+str(random.randint(1,8))
            if group in groups:
                continue
            groups.add(group)
            for i in range(1,13):
                v.append(f"{group}-{i}")
        pupils.append((k,v))
    s = str(pupils)
    print(s.replace("'", '"'))



if __name__ == "__main__":
    main()
