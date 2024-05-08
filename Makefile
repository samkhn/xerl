line_count:
	@find . -name "*.erl" -exec cat {} \; | wc -l

file_count:
	@find . -name "*.erl" -print | wc -l

.PHONE: line_count file_count
