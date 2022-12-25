import os

def write_in_file(filename, min, max, average):
   
    f = open(filename, "a+")

    f.write(
        str(min) + "\n" +
        str(max) + "\n" +
        str(average)
    )

    f.close()

def read_file(file_path):
    
    # set the dataset file
    file = open(file_path, 'r')
    Lines = file.readlines()

    average = 0
    min = float(Lines[0])
    max = float(Lines[0])
    
    count = 0
    # Strips the newline character
    for line in Lines:
        line = float(line)
        # Lets add a variable to count the number of line readed
        count += 1
        average += line

        if(min > line): min = line
        if (max < line): max = line

    print(count)
    average = average / count

    return min, max, average


for path, subdirs, files in os.walk("./data"):
    for name in files:
        filename = os.path.join(path, name)
        print("> START --- " + os.path.join(path, name) + "---")
        min, max, average = read_file(filename)
        write_in_file(filename, min, max, average)
        print("> END ------ ")

