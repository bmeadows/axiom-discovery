# -*- coding: utf-8 -*-
'''Python program to run experimental trials in the robot butler domain. 
Objectives:
1. Parse the output in a proper sorted format, so that it can be compared.
2. Prepare two ASP programs with and without axioms to be learnt.
3. Select static attributes for the objects in the domain. 
4. Select initial state for the objects in the domain. The state has to be valid. 
5. Run the experiment and count the number and proportion of plans in both the cases. 


Note: It is important that there are markers in the programs where the static attributes 
and initial state are to be inserted. For that purpose, we are using:
i) %% (*)(*)(*)   : A marker after which axioms will be inserted in the ASP with axioms program have to be inserted.
ii) %% <^^^>      : A marker after which static attributes has to be inserted. 
iii) %% #_#_#     : A marker after which initial state values has to be inserted.  
iv) %% $@$@$@$    : A marker after which number of steps value has to be inserted.

Note: It is important to modify this file in order to fit the requirements of our runs. 
The part that needs to be adjuted are, all parameters that need to be mofified are those below between markers #*******#
and the build_asp() function.
'''

import subprocess
import random, csv
from decimal import *
getcontext().prec = 3

number_steps_marker = '%% $@$@$@$'
attributes_marker = '%% <^^^>'
initial_state_marker = '%% #_#_#'
axioms_marker = '%% (*)(*)(*)'


#*******#
only_useful_runs = True  #only recording those runs in which the ASP program without axioms gives at least one plan as answer. 
number_of_runs = 100 #how many runs we want to record, each with random attribute values and random initial state
number_of_steps = 4 #the number of steps the ASP programs will give plans for.


axioms = []
#axioms.append('holds(is_labelled(X, true), I+1) :- occurs(serve(R,X,P),I),has_role(P,sales).')
#axioms.append('holds(status(X,damaged), I+1) :- occurs(putdown(R,X),I), has_surface(X,brittle).')
#axioms.append('-occurs(serve(R,X,P),I+1) :- holds(status(X,damaged),I), not has_role(P,engineer).')
#axioms.append('-occurs(affix_label(R,X),I+1) :- not has_surface(X,hard).')
#axioms.append('-occurs(pickup(R,X),I) :- holds(has_weight(X,heavy), I) , has_arm_type(R,electromagnetic).')
axioms.append('-occurs(affix_label(R,X),I) :- holds(status(X,damaged),I), has_arm_type(R,pneumatic).')



asp_inputfile_name = 'robotassist_preASP.txt'
results_file_name = 'ra_results'
#*******#


def build_asp():
	'''
	This function serves as the core module of our experimental trials. 
	It needs the description of the domain object and attributes. It:
	1. Reads the initial programs(without static attributes and initial state).
	2. Appends the static attributes. 
	3. Appends a valid initial state. 
	4. Writes the new programs into files and sends them for execution. 
	Returns the result to the caller function. 

	'''


        robots = ['rob1']
        persons = ['p0','p1','p2']
        entities = robots + persons

        items = ['cup1','book1' , 'prin1']
        furniture = []
        #furniture = ['shelf1', 'shelf2', 'desk1', 'tab1']
        objs = items + furniture
        
        things = objs + entities

	person_roles = ['sales','engineer','manager']
        robot_arm_types = ['pneumatic', 'electromagnetic']
        obj_weights = ['light', 'heavy']
        item_surfaces = ['hard', 'brittle']
        item_statuss = ['damaged', 'intact']
        item_is_labelled = ['true', 'false']
        thing_locations = ['office', 'library', 'kitchen', 'workshop']


        
        #Opening file with axioms and findind the marker for steps
	f1 = open(asp_inputfile_name, 'r')
	asp = f1.read() 
	f1.close()
	asp_split = asp.split('\n')
        index0 = asp_split.index(number_steps_marker)
        index0+=1
        asp_split.insert(index0, '#const numSteps = '+ str(number_of_steps)+'.')
        
        # finding marker for for static attribute insertion.

	index1 = asp_split.index(attributes_marker)

	static_attributes = []
        
	person_role_selected = []
        robot_arm_type_selected = []
        obj_weight_selected = []
        item_surface_selected = []
 
        
	# Assigning attributes to each object in the domain.
                

        for person in persons:
                #Assigning roles to a person and inserting the rule. 
                index1+=1
                person_role = random.choice(person_roles)
                person_role_selected.append(person_role)
                asp_split.insert(index1, 'has_role('+ person + ',' + person_role +').')
                static_attributes.append('has_role('+ person + ',' + person_role +').')               
                
	for robot in robots:
		#Assigning arm_type to each robot and inserting the rule.
		index1+=1
		robot_arm_type = random.choice(robot_arm_types)
		robot_arm_type_selected.append(robot_arm_type)
		asp_split.insert(index1,'has_arm_type('+robot+ ', ' +robot_arm_type + ').')
		static_attributes.append('has_arm_type('+robot+ ', ' +robot_arm_type + ').')

        for obj in objs:
                #Assigning weight to each object and inserting the rule.
                index1+=1
                obj_weight = random.choice(obj_weights)
                obj_weight_selected.append(obj_weight)
                asp_split.insert(index1,'has_weight('+obj+', '+obj_weight + ').')
                static_attributes.append('has_weight('+obj+', '+obj_weight +').')

        for item in items:
                #Assigning surface type to each item.
                index1+=1
                item_surface = random.choice(item_surfaces)
                item_surface_selected.append(item_surface)
                asp_split.insert(index1,'has_surface('+item+', '+item_surface +').')
                static_attributes.append('has_surface('+item+', '+item_surface +').')
                
        
        
	# Finding index of marker where initial state contents has to be inserted. 
	index2 = asp_split.index(initial_state_marker)

	initial_state = []
	thing_location_selected = []
        item_status_selected = []
        item_is_labelled_selected = []
        
        for item in items:
                # Randomly selecting status for each item.
                index2+=1
                item_status = random.choice(item_statuss)
                item_status_selected.append(item_status)
                asp_split.insert(index2,'holds(status('+item+ ', '+ item_status + '),0).')
                initial_state.append('holds(status('+item+ ', '+ item_status + '),0).')

                # Randomly selecting if each item is, or not, labelled.
                index2+=1
                item_is_labelled_choice = random.choice(item_is_labelled)
                item_is_labelled.append(item_is_labelled_choice)
                asp_split.insert(index2,'holds(is_labelled('+item+ ', '+ item_is_labelled_choice + '),0).')
                initial_state.append('holds(is_labelled('+item+ ', '+ item_is_labelled_choice+ '),0).')
                
        for thing in things:
                #Randomly selecting a location for each thing.
                index2+=1
                thing_location = random.choice(thing_locations)
                thing_location_selected.append(thing_location)
                asp_split.insert(index2,'holds(loc('+thing+', '+thing_location+'),0).')
                initial_state.append('holds(loc('+thing+', '+thing_location+'),0).')
            
                
	# Write the formed programs to respective files. 	
	asp = '\n'.join(asp_split)

        asp_with_axioms = asp
	asp_with_axioms_split = asp_with_axioms.split('\n')
	index = asp_with_axioms_split.index(axioms_marker)
        for axiom in axioms:
                index+=1
                asp_with_axioms_split.insert(index,axiom)
        asp_with_axioms = '\n'.join(asp_with_axioms_split)
        
	file1 = 'prg_with_axioms.sp'
        file2 = 'prg_without_axioms.sp'


	f1 = open(file1, 'w')
	f1.write(asp_with_axioms) 
	f1.close()
	f2 = open(file2, 'w')
	f2.write(asp) 
	f2.close()

	#Run the experiments and return the results, static attributes and initial state. 
	output1, output2 = run_experiment(file1, file2)
	return [static_attributes, initial_state, output1, output2]



def parse_output(text):
	'''
	A simple function to parse ASP output and sort the plans in sequential order.
	Returns a list of plans. 
	Each plan is a set of actions seperated by ','.
	Parameter: 
	text: Receives the ASP output.
	'''

	parsed_plans=[]
	plans = text.strip('\n').split('\n')
	plans = [plan for plan in plans if plan!='' and plan!='{}']
	for plan in plans:
		plan = plan.strip('}').strip('{').split(', ')
		plan = [action.split('),') for action in plan]
		plan.sort(key=lambda x:x[1])
		plan = ['),'.join(action) for action in plan]
		parsed_plans.append(plan)
	return parsed_plans

def run_experiment(file1, file2):
	'''
	Method to run two ASP files(with axioms and without), parse their outputs and return to
	calling function.
	Parameters:
	file1 - Name of sparc program with axioms.
	file2 - Name of sparc program without axioms. 
	Returns a list of outputs of both executions. 
	'''
	output1 = subprocess.check_output('java -jar sparc.jar '+file1+' -A ',shell=True)
 	parsed_output1 = parse_output(output1)
	output2 = subprocess.check_output('java -jar sparc.jar '+file2+' -A ',shell=True)
	parsed_output2 = parse_output(output2)
	return [parsed_output1, parsed_output2]

def overlapping_plans(set1, set2, fl):
	'''
	Counts the number of plans in set2(plans without domain axioms) 
	that are in set1(plans with domain axioms), i.e. finding proportion. 
	It also takes the file fl and writes the plans that are in set2, but not in set1. 
	'''
	counter=0
	for plan in set2:
		if plan in set1:
			counter+=1
		else:
			fl.write("\nExcluded Plan:\n "+str(plan))
	return counter



if __name__ == "__main__":
	'''
	The main function, creates two results file. 
	1. experimental_results.csv: A csv that just contains the number of plans generated in both the cases. 
	2. experimental_results.txt: A detailed text file that contains initial state, static attributes, 
								and the plans for each experiment. 
	This function also keeps track of 
	'''
	csvfile = open(results_file_name+'.csv', 'w')
	writer = csv.writer(csvfile)
	f = open(results_file_name+'.txt', 'w')
	counter = 0
        counting_useful_runs = 0 #non nule runs, where the number of plans without axioms is greater than 0.
        counting_nule_runs = 0 #where the number of plans without axioms is 0.
	totalPlansWithAxioms = 0
	totalPlansWithoutAxioms = 0
	writer.writerow(['Index', 'Plans with axioms', 'Plans without axioms', 'Overlapping plans', 'Average plans with axioms', 'Average plans without axioms' ])

        while counter < number_of_runs:
		static_attributes, initial_state, output1, output2 = build_asp()

                # this loop checks if the we get 0 answers in the run
                # if we only want 'useful' runs, it will go to the next run without recording the run with 0 answers
                # if the first 100 runs give 0 answers, it will stop running and print a message
                if len(output2) != 0:
                        counting_useful_runs+=1
                else:
                        counting_nule_runs+=1 
                        if counting_useful_runs == 0 and counting_nule_runs == 100:
                        	print "\n####################################\n all invalid runs!: "
			        writer.writerow( "\n###################################\n all invalid runs!: ")
                                break
                        if(only_useful_runs): continue
                
 		counter+=1
		l=[counter]
		print "\nRunning trial: "+str(counter)+' useful runs: '+str(counting_useful_runs)
                
     		#Writing to txt
		f.write(str(counter)+"\n")
		f.write("Initial state:\n")
		for state_value in initial_state:
			f.write(str(state_value)+"\n")	
		f.write("\nStatic attributes:\n")
		for attribute in static_attributes:
			f.write(str(attribute)+"\n")
		f.write('\nPlans with axioms: '+str(len(output1))+"\n")
		for plan in output1:
			f.write(str(plan)+"\n")
		f.write('\nPlans without axioms: '+str(len(output2))+"\n")
		for plan in output2:
			f.write(str(plan)+"\n")
                        

                totalPlansWithAxioms+=len(output1)
		totalPlansWithoutAxioms+=len(output2)                
		overlaping_number = overlapping_plans(output1, output2, f)
		l.append(len(output1))
		l.append(len(output2))
		l.append(str(overlaping_number))
                if counting_useful_runs != 0: 
  		        l.append(str(Decimal(totalPlansWithAxioms)/Decimal(counting_useful_runs)))
		        l.append(str(Decimal(totalPlansWithoutAxioms)/Decimal(counting_useful_runs)))
                else:
                        l.append(str(Decimal(totalPlansWithAxioms)))
		        l.append(str(Decimal(totalPlansWithoutAxioms)))              
		       

		f.write('\n--------------------------------\n\n')
		#Writing to csv
		writer.writerow(l)



		if overlaping_number != len(output1):
			print "\n########################################################################\nWarning!: "+str(counter)
			writer.writerow( "\n########################################################################\nWarning!: "+str(counter))

		csvfile.flush()


