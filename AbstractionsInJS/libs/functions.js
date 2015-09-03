(function()
{
	var Functions = {};

	function identity(a)
	{
		return a;
	}
	Functions.identity = identity;

	function append(a, b)
	{
		return a.concat(b);
	}
	Functions.append = append;

	function empty(m)
	{
		return m.empty ? m.empty() : m.constructor.empty();
	}
	Functions.empty = empty;

	function map(m, f)
	{
		if(m.map) return m.map(f);
		return flatMap(m, function(a)
		{
			return point(m, f(a));
		});
	}
	Functions.map = map;

	function flatMap(m, f)
	{
		return m.chain ? m.chain(f) : m.then(f);
	}
	Functions.flatMap = flatMap;

	function point(m, a)
	{
		return m.of ? m.of(a) : m.constructor.of(a);
	}
	Functions.point = point;

	function join(m)
	{
		return flatMap(m, identity);
	}
	Functions.join = join;

	function ap(a, f)
	{
		if(f.ap) return f.ap(a);
		return flatMap(f, function(f)
		{
			return map(a, f);
		});
	}
	Functions.ap = ap;

	function sequence(m, ms)
	{
		return ms.reduce(
			function(mr, mx)
			{
				return mr.chain(
					function(xs)
					{
						return mx.chain(
							function(x)
							{
								xs.push(x);
								return m.of(xs);
							}
						);
					}
				);
			},
			m.of([])
		);
	};
	Functions.sequence = sequence;


	function lift2(f, a, b)
	{
		return ap(b, map(a, function(a)
		{
			return function(b)
			{
				return f(a, b);
			};
		}));
	}
	Functions.lift2 = lift2;

	function lift3(f, a, b, c)
	{
		return ap(c, ap(b, map(a, function(a)
		{
			return function(b)
			{
				return function(c)
				{
					return f(a, b, c);
				};
			};
		})));
	}
	Functions.lift3 = lift3;

	function lift4(f, a, b, c, d)
	{
		return ap(d, ap(c, ap(b, map(a, function(a)
		{
			return function(b)
			{
				return function(c)
				{
					return function(d)
					{
						return f(a, b, c, d);
					};
				};
			};
		};
	))));
	}
	Functions.lift4 = lift4;
	window.Functions = Functions;
})()
