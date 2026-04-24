#!/bin/bash

# Command format: ./visualize.bash [Parser Rule Name] -gui
# e.g. If you want to see AST for the whole python program, use the following command:
#     ./visualize.bash file_input -gui
# Please ensure that you've already installed ANTLR4 to directory /usr/local/lib/antlr-4.13.1-complete.jar

export CLASSPATH=".:/usr/local/lib/antlr-4.13.1-complete.jar:$CLASSPATH"

# Check if Python3Demo.g4 file exists
if [ ! -f "Python3Demo.g4" ]; then
    echo "Error: Python3Demo.g4 not found."
    exit 1
fi

# Set working directory
output_dir="/tmp/Python3Demo"

# Create output directory if it doesn't exist
if [ ! -d "$output_dir" ]; then
    mkdir -p "$output_dir"
fi

# Check directory permissions, modify if insufficient
if [ ! -w "$output_dir" ]; then
    echo "Directory $output_dir is not writable. Changing permissions..."
    sudo chmod 777 -R "$output_dir"
    if [ $? -ne 0 ]; then
        echo "Error: Failed to change permissions for $output_dir."
        exit 1
    fi
    echo "Permissions for $output_dir have been changed."
fi

# Only run antlr4 if Java files don't exist in directory
if [ ! -f "$output_dir/Python3Demo*.java" ]; then
    echo "Running antlr4 to generate Java files..."
    java -Xmx500M -cp "/usr/local/lib/antlr-4.13.1-complete.jar:$CLASSPATH" org.antlr.v4.Tool -o "$output_dir" Python3Demo.g4
fi

# Run javac if Java files or .class files don't exist
if [ ! -f "$output_dir/Python3Demo.class" ]; then
    echo "Compiling Java files..."
    javac "$output_dir"/Python3Demo*.java
fi

# Switch to output directory, then run grun (java) command with all parameters
echo "Running grun with arguments: $@"
echo "All finished! Please type your code below. Type Ctrl+D to end. Enjoy coding~"
cd "$output_dir" && java -Xmx500M -cp "/usr/local/lib/antlr-4.13.1-complete.jar:$CLASSPATH" org.antlr.v4.gui.TestRig Python3Demo "$@"
