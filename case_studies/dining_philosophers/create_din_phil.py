import argparse
import os
import shutil
  
def main():
    # Parse arguments:
    parser = argparse.ArgumentParser(description="Scalability experiment for rLTL.")
    parser.add_argument("-n", "--n", type=int, help="Model size.", default=1)
    parser.add_argument("-k", "--k", type=int, help="Parameter for scalable formula.", default=1)
    args = parser.parse_args()
    
    # # Create n dining philosophers model:
    # model_path_og = os.path.join(os.getcwd(), "dining_philosophers.smv")
    model_path = os.path.join(os.getcwd(), "din_phil_temp.smv")
    # shutil.copyfile(model_path_og,model_path)
    
    # with open(model_path, "a") as f:
    #     f.write("MODULE main()\n")

    #     # VAR
    #     f.write("VAR\n")
    #     for i in range(args.n):
    #         f.write("\tfork{}: {{usable, unusable}};\n".format(i))
    #     for i in range(args.n):
    #         f.write("\tphil{}: process philosopher(fork{}, fork{});\n".format(i, i, i+1 % args.n-1))

    #     # ASSIGN
    #     f.write("ASSIGN\n")
    #     for i in range(args.n):
    #         f.write("\tinit(fork{}) := usable;\n".format(i))

    #     # DEFINE
    #     f.write("DEFINE\n")
    #     for i in range(args.n):
    #         f.write("\tp{}:= (phil{}.state = ready);\n".format(i,i))
    #         f.write("\tl{}:= (phil{}.state = lwait);\n".format(i,i))
    #         f.write("\tr{}:= (phil{}.state = rwait);\n".format(i,i))
    #         f.write("\te{}:= (phil{}.state = eat);\n".format(i,i))            

    # f.close()

    # Create specification file
    input_path = os.path.join(os.getcwd(), "input_spec.txt")
    with open(input_path, "w") as f:
        f.write("Model checker:\nNuSMV\n\n")
        f.write("Model name:\n{}\n\n".format(model_path))
        f.write("rLTLspecs:\n{}\n".format(1))

        spec = "( ( !(rG rF a0) | (rG rF d0))"
        for i in range(1, args.k):
            spec += " & ( !(rG rF a{}) | (rG rF d{}))".format(i,i)
        spec += ") => rF d0"
        # spec = "(rG rF d0) "
        # for i in range(1, args.k):
        #     spec += " & (rG rF  d{})".format(i)
        f.write("{}\n".format(spec))

        # f.write("flags:\n-coi -df -dcx -dynamic -n")
        f.write("flags:\n -coi -df -dcx -dynamic -disable_sexp2bdd_caching -n")
        # f.write("\nflags:\n -df -dcx -n")
        # -dcx :    Disables generation of counter-examples.
        #
        # -df :     Disable the computation of the set of reachable states. 
        #           This option is provided since NuSMV-2.4.0 to prevent the computation of reachable states 
        #           that are otherwise computed by default.
        #
        # -coi :    Enables cone of influence reduction. Sets to “1” the cone of influence environment variable. 
        #           We remark that, when cone of influence reduction is enabled, a counterexample trace for a property that 
        #           does not hold may not be a valid counter-example trace for the original model. 
        #           We remark that, the cone of influence reduction, as well as CTL/LTL model checking algorithms implemented 
        #           in NuSMV assume the concrete model to be deadlock free.
        #
        # -dynamic : 
        #           Enables dynamic reordering of variables. Add this to git: https://nusmv.fbk.eu/faq.html#014
        #
        # -disable_sexp2bdd_caching :
        #           If dynamic reordering is enabled it may be beneficial also to disable BDD caching. 
        #           During conversion of symbolic expressions to ADD and BDD representations the results of evaluations are cached.
        #           This avoids the construction of BDD for the same symbolic expression several times. 
        #           Dynamic reordering may modify all existing BDDs, and cleaning the cache thereby freeing the BDD may speed up the reordering.
    f.close()

if __name__ == '__main__':
    main()