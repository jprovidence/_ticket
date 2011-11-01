#!/bin/bash
#build _ticket

cd ../deps/qrly/src

erlc -pa . qrly.erl qrly_html.erl qrly_xml.erl
erlc qrly_lexer.xrl && erlc qrly_lexer.erl && rm qrly_lexer.erl
erlc qrly_parser.yrl && erlc qrly_parser.erl && rm qrly_parser.erl

mv ./*.beam ../../../ebin
cd ../../mochiweb/src

erlc *.erl

mv ./*.beam ../../../ebin
cd ../../../src

erlc *.erl
mv ./*.beam ../ebin

