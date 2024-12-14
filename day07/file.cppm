module;

#include <cassert>
#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

export module file;

using namespace std;

using key_type = uint64_t;
using value_type = vector<uint64_t>;

export using the_map = map<key_type, value_type>;

export pair<the_map, size_t> readInputFile(const string & filename)
{
	the_map data;
	ifstream inFile{filename};
	if (!inFile.is_open())
	{
		cerr << "Cannot open file " << filename << endl;
		return make_pair(data, 0);
	}

	clog << "Reading file " << filename << endl;

	size_t elementsCount = 0;
	for (string line; getline(inFile, line);)
	{
		istringstream iss{line};
		key_type key;
		char colon;
		iss >> key >> colon;
		assert(colon == ':');

		value_type values;
		int value;
		while (iss >> value)
		{
			values.push_back(value);
		}

		elementsCount = max(elementsCount, values.size());
		data[key] = values;
	}

	return make_pair(data, elementsCount);
}
