import sys
import tempfile
import subprocess

try:
    input = "struct foo{int bar;};";
    expectedOutput = "namespace aglio{template<>struct TypeDescriptorGen<foo>:MemberList<MemberDescriptor<&foo::bar,\"bar\">>{static constexpr std::string_view Name{\"foo\"};static constexpr std::string_view QualifiedName{\"foo\"};};}\n"

    generator = sys.argv[1]
    with tempfile.TemporaryDirectory() as tmpDirName:
        inputFileName = tmpDirName + "/Input.hpp"
        outputFileName =tmpDirName+"/TypeDescriptor_Input.hpp"
        with open(inputFileName, "w") as inputFile:
            inputFile.write(input);

        result = subprocess.run([generator, "-p", tmpDirName, "--output-directory=" + tmpDirName, inputFileName])
        if result.returncode != 0:
            print("generator: ", generator, " failed")
            exit(1)

        with open(outputFileName, "r") as outputFile:
            output = outputFile.read()
            if output != expectedOutput:
                print("wrong output")
                exit(1)
        exit(0)
except Exception as e:
    print(e)

exit(1)
