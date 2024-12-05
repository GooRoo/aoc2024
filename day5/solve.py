from pathlib import Path


def read_data(filePath: Path):
    with open(filePath, "r") as file:
        data = file.read()

    (rs, us) = data.split("\n\n")
    rules = []
    for rule in rs.split("\n"):
        (lhs, rhs) = rule.split("|")
        rules.append((int(lhs.strip()), int(rhs.strip())))

    updates = []
    for update in us.split("\n"):
        if update != "":
            updates.append([int(x.strip()) for x in update.split(",")])

    return rules, updates


def is_valid_order(update, rules):
    for i in range(len(update)):
        for j in range(i + 1, len(update)):
            if (update[j], update[i]) in rules:
                return False
    return True


def solve(filePath: Path):
    rules, updates = read_data(filePath)
    total = 0

    for update in updates:
        if is_valid_order(update, rules):
            middle = update[len(update) // 2]
            total += middle

    return total


def sort_update(update, rules):
    def compare_pages(a, b):
        if (a, b) in rules:
            return -1
        elif (b, a) in rules:
            return 1
        return 0

    from functools import cmp_to_key

    return sorted(update, key=cmp_to_key(compare_pages))


def solve2(filePath: Path):
    rules, updates = read_data(filePath)
    total = 0

    for update in updates:
        if not is_valid_order(update, rules):
            sorted_update = sort_update(update, rules)
            middle = sorted_update[len(sorted_update) // 2]
            total += middle

    return total


if __name__ == "__main__":
    print(solve(Path("data/task.example")))
    print(solve(Path("data/task.data")))
    print(solve2(Path("data/task.example")))
    print(solve2(Path("data/task.data")))
