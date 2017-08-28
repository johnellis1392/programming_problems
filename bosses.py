import unittest


# Given data like this, a mapping of employees
# to their respective bosses, construct a tree
# hierarchy to represent the structure of
# responsibilities.
# input_data = [
#     ("John Smith", None),
#     ("Ada Carpenter", "John Smith"),
#     ("Dekembe Metembo", "John Smith"),
#     ("Alaska Native", "Dekembe Metembo")
# ]


# Step 1)
# * Collect all employees into hashmap
# Step 2)
# * For each employee in map:
# ** Get employee's boss from map
# ** Assign employee to boss' understudies
# ** Assign boss as employee's boss
# ** Remove employee from map


class Employee:
    def __init__(self, name, boss = None):
        self.name = name
        self.boss = boss
        self.employees = []


def find(predicate, array):
    if type(array) == list:
        for i in array:
            if predicate(i):
                return i
        return None

    elif type(array) == dict:
        for i in array.items():
            if predicate(i):
                return i
        return None

    else:
        raise Exception("Expected array or map, found: %s" % type(array))


def get_employee_map(data):
    employees = {}
    for (employee, _) in data:
        employees[employee] = Employee(employee, None)

    return employees


def run(data):

    # Step 1
    employee_map = get_employee_map(data)

    # Step 2
    for (employee_name, boss_name) in data:
        if boss_name in employee_map:
            employee = employee_map[employee_name]
            boss = employee_map[boss_name]

            employee.boss = boss
            boss.employees.append(employee)

        else:
            # Employee not in map; skip
            continue

    return to_tree_map(find(lambda i: i[1].boss == None, employee_map)[1])



def to_tree_map(employee):
    # name = employee.name
    employees = employee.employees
    for i in employees:
        new_employees = to_tree_map(i)
        employees[i.name] = new_employees
    return employees


def pretty_print(employee, tab_offset = 0):
    print("%s %s" % ("*" * tab_offset, employee.name))
    for i in employee.employees:
        pretty_print(i, tab_offset + 1)



class RunTest(unittest.TestCase):

    def test_organizes_employees(self):
        test_data = [
            ("John Smith", None),
            ("Ada Carpenter", "John Smith"),
            ("Dekembe Metembo", "John Smith"),
            ("Alaska Native", "Dekembe Metembo")
        ]

        result = run(test_data)

        self.assertEqual(run(test_data), {'John Smith': [{'Ada Carpenter': []}, {'Dekembe Metembo': [{'Alaska Native': []}]}]})


if __name__ == '__main__':
    unittest.main()



# if __name__ == '__main__':
#     result = run(input_data)
#     pretty_print(result)
