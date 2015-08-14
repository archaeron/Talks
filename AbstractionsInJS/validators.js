"use strict";

let Validation = require('data.validation')

const Normalizers =
{
	phoneNumber:
		function(number)
		{
			const ret = number
				.trim()
				.split('')
				.map(function(c){ return parseInt(c, 10); })
				.filter(function(c){ return !isNaN(c); });
			let n;

			if(ret.length > 2 && ret[0] == 0 && ret[1] == 0)
			{
				n = ['+'].concat(ret.slice(2));
			}
			else if(ret.length > 3 && ret[0] == 0 && ret[1] > 0)
			{
				n = ['+41'].concat(ret.slice(1));
			}
			else if(ret.length == '4179dddHHxx'.length && ret[0] == 4 && ret[1] == 1)
			{
				n = ['+'].concat(ret);
			}
			else
			{
				n = ret;
			}

			return n.join('')
		}
};

const predicate = function(pred, errorMessage)
{
	return function(value)
	{
		if(pred(value))
		{
			return Validation.Success(value)
		}
		else
		{
			return Validation.Failure([errorMessage])
		}
	};
};

const regexp = function(regexp, errorMessage)
{
	const pred = function(v)
	{
		return v && v.match && v.match(regexp)
	};
	return predicate(pred, errorMessage);
};

const profileName = function(value)
{
	const r = /^[a-zA-Z0-9\-]{5,30}$/;
	const e = "validation.invalid_profile_name";
	return regexp(r, e);
};

const email = regexp(/^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,4})$/, "validation.invalid_email");

const pin = function(v)
{
	if(typeof v !== "string")
	{
		return Validation.Failure(['"' + v + '" is not a string.']);
	}
	else if(v.length !== 4)
	{
		return Validation.Failure(['"' + v + '" needs to be exactly 4 characters long.']);
	}
	else if(!v.match(/^[0-9]{4}$/))
	{
		return Validation.Failure(['"' + v + '" can only contain numbers.']);
	}
	else
	{
		return Validation.Success(v);
	}
};

const phoneNumber = function(value)
{
	const r = /^\+?417[0-9]{8}$/;
	const normalized = Normalizers.phoneNumber(value);
	const e = 'validation.invalid_phone_number';
	return Validator.regexp(r, e)(normalized);
};

const empty = function(str)
{
	if(str.length == 0)
	{
		return Validation.Success(str)
	}
	else
	{
		return Validation.Failure(['not empty'])
	}
};

const or = function(a, b)
{
	return function(value)
	{
		return a(value).orElse(function(_) { return b(value); });
	}
};

const validateAll = function(validations)
{
	//errors = []
	//validations.forEach( func(validation) {
	//validation (->), (e) { errors.push(e)

	//if(errors.length == 0){ success()} else {error(errors)}
};

const Validator =
{
	predicate: predicate,
	regexp: regexp,
	profileName: profileName,
	email: email,
	pin: pin,
	phoneNumber: phoneNumber,
	empty: empty,
	or: or
};

module.exports = Validator;
