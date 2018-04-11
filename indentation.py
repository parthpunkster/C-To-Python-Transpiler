def main():
	count = 0
	inputfile = raw_input("Enter the name of the .txt file: ")
	outputfile = raw_input("Enter the name of .py file in which you want the transpiled code: ")
	f = open(inputfile,'r')
	f1 = open(outputfile,'w')
	lines = f.readlines()
	# print lines
	for line in lines:
		if line == '{\n':
			count = count + 4
		elif line == '}\n':
			count = count - 4
		else:
			text = ''
			for i in range(0,count):
				text = text + ' '
			text = text + line
			f1.write(text)
	f.close()
	f1.close

if __name__ == '__main__':
	main()