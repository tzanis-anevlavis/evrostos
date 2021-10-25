import os

runs = 1
model_size = 10
runtimes = []
for k in range(10):
    time = 0.0
    for i in range(runs):
        os.system("python3 create_din_phil.py --n {} --k {} && (time ./evrostos input_spec.txt) &> term_out.txt".format(model_size, k))
        f = open("term_out.txt")
        lines = f.readlines()
        for line in lines:
            if not line=="\n":
                if line.split()[0]=="real":
                    time += float(line.split()[1].split('m')[0]) * 60 + float(line.split()[1].split('m')[1][:-1])

    print("For k={}, average: {}s".format(k, time / runs))
    runtimes.append(time / runs)

print(runtimes)