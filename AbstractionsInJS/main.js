"use strict";

let ops = require('core.operators');
let Validator = require('./validators.js');
let Pacta = require('pacta');

// pin Validator
window.isPin = Validator.pin;

// test if it is a pin
window.validPin = isPin('1234');
window.invalidPin = isPin('111111');

// test if a value is either a pin or an empty string
window.isPinOrEmpty = Validator.or(Validator.empty, Validator.pin)

// test if it is a pin or empty
window.invalidPinOrEmpty = isPinOrEmpty('243523');
window.emptyPinOrEmpty = isPinOrEmpty('');
window.pinPinOrEmpty = isPinOrEmpty('1234');

window.userValid = Functions.lift2(
	function(email, phoneNumber)
	{
		return { email: email, phoneNumber: phoneNumber };
	},
	Validator.email('abc@gmail.com'), Validator.phoneNumber('0793423464')
);

window.userInvalid1 = Functions.lift2(
	function(email, phoneNumber)
	{
		return { email: email, phoneNumber: phoneNumber };
	},
	Validator.email(''), Validator.phoneNumber('0793423464')
);

window.userInvalid2 = Functions.lift2(
	function(email, phoneNumber)
	{
		return { email: email, phoneNumber: phoneNumber };
	},
	Validator.email(''), Validator.phoneNumber(''));

window.rejectedTask = Pacta.reject(3);
window.resolvedTask = Pacta.of(3);

window.ajaxTask = function(url)
{
	let p = new Pacta();
	var oReq = new XMLHttpRequest();
	const resolve = function()
	{
		p.resolve(oReq.responseText);
	};
	const reject = function()
	{
		p.reject("Error");
	};

	oReq.addEventListener("load", resolve, false);
	oReq.addEventListener("error", reject, false);
	oReq.addEventListener("abort", reject, false);
	oReq.open("GET", url, true);
	oReq.send();
	return p;
};

window.logTask = function(t)
{
	t.fork(
		function(f) { console.log("Failure: ", f); },
		function(s) { console.log("Success: ", s); }
	);
};

window.starWarsTask = ajaxTask('http://swapi.co/api/starships/9/');
window.wookieeTask = ajaxTask('http://swapi.co/api/starships/9/?format=wookiee');
window.sequencedTasks = Functions.sequence(Pacta, [starWarsTask, wookieeTask]);
window.liftedTask = Functions.lift2(function(a, b){ return a + b; }, Pacta.of(3), Pacta.of(7))
