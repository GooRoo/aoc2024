#include <iostream>
#include <numeric>
#include <sstream>
#include <stack>

import file;
import operations;

using namespace std;

int main()
{
	string filename = "data/task.data";
	const auto & [data, _] = readInputFile(filename);

	uint64_t result = 0;
	VariationsGenerator<Operation::Plus, Operation::Multiply, Operation::Concat> gen;

	// for (auto seq: gen.generate(3))
	// {
	// 	for (auto op: seq)
	// 		cout << op << ' ';
	// 	cout << endl;
	// }

	for (const auto & [key, values]: data)
	{
		for (const auto & sequence: gen.generate(values.size() - 1))
		{
			stack<Operation> ops{sequence};
			ostringstream oss;

			uint64_t acc = values.front();
			oss << key << " = " << acc;
			for (auto start = ++begin(values); start != end(values); ++start)
			{
				if (auto res = evaluate(acc, ops.top(), *start); res)
				{
					auto [newAcc, actualOp] = *res;
					oss << ' ' << actualOp << ' ' << *start;
					acc = newAcc;
					if (acc > key)
						break;
					ops.pop();
				}
				else
					break;
			}

			if (acc == key)
			{
				cout << oss.str() << endl;
				result += key;
				break;
			}
		}
	}

	cout << "Total: " << result << endl;

	return 0;
}
