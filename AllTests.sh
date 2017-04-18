echo "TESTING PERSONAL 1: Function Body test" > output.txt 
./mC "FunctionBodyTest.txt" >> output.txt
echo "TESTING PERSONAL 2: Summation Function" >> output.txt
./mC "SumFunction.txt" >> output.txt
echo "TESTING PERSONAL 3: Parser Failure" >> output.txt
 "Intentionally fails due to parse error" >> output.txt
./mC "FailParse.txt"  >>output.txt
echo "TESTING PERSONAL 4: Lexer Failure"  >>output.txt
 "Intentionally fails due to lexer error"  >>output.txt
./mC "LexFail.txt"  >>output.txt
echo "TESTING PERSONAL 5: Fibonacci Function"  >>output.txt
./mC "FibonacciTest.txt"  >>output.txt
echo "TESTING TEST 1: "  >>output.txt
./mC "test1.txt"  >>output.txt
echo "TESTING TEST 2: "  >>output.txt
./mC "test2.txt"  >>output.txt
echo "TESTING TEST 3: "  >>output.txt
./mC "test3.txt"  >>output.txt
echo "TESTING TEST 4: "  >>output.txt
./mC "test4.txt"  >>output.txt
echo "TESTING TEST 5: "  >>output.txt
./mC "test5.txt"  >>output.txt
echo "TESTING TEST 6: "  >>output.txt
./mC "test6.txt"  >>output.txt
echo "TESTING TEST 7: "  >>output.txt
./mC "test7.txt"  >>output.txt
echo "TESTING TEST 8: "  >>output.txt
./mC "test8.txt"  >>output.txt
echo "TESTING TEST 9: "  >>output.txt
./mC "test9.txt"  >>output.txt
echo "TESTING TEST 10: "  >>output.txt
./mC "test10.txt"  >>output.txt
echo "TESTING TEST 11: "  >>output.txt
./mC "test11.txt"  >>output.txt
echo "TESTING TEST 12: "  >>output.txt
./mC "test12.txt"  >>output.txt
echo "TESTING TEST 13: "  >>output.txt
./mC "test13.txt"  >>output.txt
echo "TESTING TEST 14: "  >>output.txt
./mC "test14.txt"  >>output.txt
echo "TESTING TEST 15: "  >>output.txt
./mC "test15.txt"  >>output.txt
echo "TESTING TEST 16: "  >>output.txt
./mC "test16.txt"  >>output.txt
echo "TESTING TEST 17: "  >>output.txt
./mC "test17.txt"  >>output.txt
echo "TESTING TEST 18: "  >>output.txt
./mC "test18.txt"  >>output.txt
echo "TESTING TEST 19: "  >>output.txt
./mC "test19.txt"  >>output.txt
echo "TESTING TEST 20: "  >>output.txt
./mC "test20.txt"  >>output.txt
