$input = $('#input')
$results = $('#results')


distinct = Rx.Observable.fromEvent(input, 'keyup')
	.map (e) ->
		e.target.value
	.filter (text) ->
		text.length > 2
	.throttle(500)
	.distinctUntilChanged()


searchWikipedia = (term) ->
	promise = $.ajax(
		url: 'http://en.wikipedia.org/w/api.php',
		dataType: 'jsonp',
		data:
			action: 'opensearch',
			format: 'json',
			search: encodeURI(term)
	).promise()
	Rx.Observable.fromPromise(promise)

suggestions = distinct
	.flatMapLatest (text) ->
		searchWikipedia(text)

success = (data) ->
	res = data[1]
	$results.empty();
	$.each res, (_, value) ->
		$('<li>' + value + '</li>').appendTo($results)

failure = (e) ->
	$results.empty()
	$('<li>Error: ' + error + '</li>').appendTo($results)


suggestions.subscribe success, failure
