module;

#include <array>
#include <cmath>
#include <concepts>
#include <deque>
#include <expected>
#include <ostream>
#include <ranges>
#include <type_traits>
#include <vector>

export module operations;

namespace views = std::views;

export enum class Operation
{
	Plus,
	Multiply,
	Concat,
};

export std::ostream & operator<<(std::ostream & os, const Operation & op)
{
	switch (op)
	{
	case Operation::Plus:
		return os << '+';
	case Operation::Multiply:
		return os << '*';
	case Operation::Concat:
		return os << "|";
	}
	return os;
}

export template <Operation... Ops>
class VariationsGenerator
{
public:
	using Sequence = std::deque<Operation>;

	std::vector<Sequence> generate(size_t length) const
	{
		std::vector<Sequence> results;
		results.reserve(pow(length, _ops.size()));

		Sequence current(length, _ops[0]);
		generateRecursive(current, 0, results);
		return results;
	}

private:
	void generateRecursive(Sequence & current, size_t position, std::vector<Sequence> & results) const
	{
		if (position < current.size())
			for (const auto & op: _ops)
			{
				current[position] = op;
				generateRecursive(current, position + 1, results);
			}
		else
			results.push_back(current);
	}

	static constexpr std::array _ops = {Ops...};
};

export template <std::unsigned_integral T>
T concatenate(T a, T b)
{
	return a * pow(10, floor(log10(b) + 1)) + b;
}

export template <std::unsigned_integral T>
auto evaluate(T left, Operation op, T right) -> std::expected<std::pair<T, Operation>, bool>
{
	static constexpr auto overflow = std::unexpected(true);
	switch (op)
	{
	case Operation::Plus:
		if (left + right < left)
			return overflow;
		else
			return std::make_pair(left + right, Operation::Plus);
	case Operation::Multiply:
		if (left * right < left)
			return overflow;
		else
			return std::make_pair(left * right, Operation::Multiply);
	case Operation::Concat:
		auto result = concatenate(left, right);
		if (result < left || result < right)
			return overflow;
		else
			return std::make_pair(result, Operation::Concat);
	}
}
