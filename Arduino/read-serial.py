# Code adapted from https://www.learnrobotics.org/blog/arduino-data-logger-csv/?utm_source=youtube&utm_medium=description&utm_campaign=arduino_CSV_data_logger
# and https://www.youtube.com/watch?v=vayAp84vea8
# Python 3.9
# Code run with Common Prompt with the line py read-serial.py
import serial

arduino_port = "COM5" #serial port of Arduino
baud = 9600 #arduino uno runs at 9600 baud
fileName="MP_W005_Arduino_100gCB_after.csv" #name of the CSV file generated

ser = serial.Serial(arduino_port, baud)
print("Connected to Arduino port:" + arduino_port)
file = open(fileName, "a")
print("Created file")

 #display the data to the terminal
getData=str(ser.readline(),'utf-8')
data=getData[0:][:-2]
print(data)

 #add the data to the file
file = open(fileName, "a") #append the data to the file
file.write(data + "\n") #write data with a newline

 #close out the file
file.close()

samples = 20 #how many samples to collect
print_labels = False
line = 0 #start at 0 because our header is 0 (not real data)
while line <= samples:
    # incoming = ser.read(9999)
    # if len(incoming) > 0:
    if print_labels:
        if line==0:
            print("Printing Column Headers")
        else:
            print("Line " + str(line,'utf-8') + ": writing...")
    getData=str(ser.readline(),'utf-8')
    data=getData[0:][:-2]
    print(data)

    file = open(fileName, "a")
    file.write(data + "\n") #write data with a newline
    line = line+1

print("Data collection complete!")
file.close()