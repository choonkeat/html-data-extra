test:
	npx elm-verify-examples -r

watch:
	while fswatch --one-event src; do elm-verify-examples -r; sleep 1; done