def assoc(item, alist):
    for i in range(0, len(alist), 2):
        if alist[i] == item:
            return alist[i + 1]

    return None
