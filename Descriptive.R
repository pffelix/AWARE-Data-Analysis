
  
#Smartphone N= 39::

# Estimate, how many minutes do you use your smartphone on an average day?: mean 119
sub <- subset(db, variable == "smartphone_usage_time")
describeBy(as.numeric(as.character(sub$value)))
as.numeric(as.character(sub$value))

# I normally have my smartphone with me wherever I go (0(5), +1(12),+2(22)):yes: mean 1.4, nearly no variance sd= 0.72
sub <- subset(db, variable == "smartphone_usage_affection")
describeBy(as.numeric(as.character(sub$value)))
as.numeric(as.character(sub$value))

# I often use my smartphone to delay unpopular tasks:middle: mean -0.13, sd=1.2
sub <- subset(db, variable == "smartphone_procrastination_absorption")
describeBy(as.numeric(as.character(sub$value)))
as.numeric(as.character(sub$value))

#I often use my smartphone to take my mind off from stressed situations:middle-no(Wrong):  mean -0.55, sd=1.27
sub <- subset(db, variable == "smartphone_stress_absorption")
describeBy(as.numeric(as.character(sub$value)))
as.numeric(as.character(sub$value))

# I often use my smartphone to overcome bored situations:middle-yes(correct):  mean 0.40, sd=1.32
sub <- subset(db, variable == "smartphone_boredom_absorption")
describeBy(as.numeric(as.character(sub$value)))
as.numeric(as.character(sub$value))


# Personalities N=40:

!# I like to share emotions with friends(-2(2),-1(6), 0(10), +1(12),+2(10)): mean = 0.55, sd=1.18
sub <- subset(db, variable == "personality_emotionsharing")
describeBy(as.numeric(as.character(sub$value)))
as.numeric(as.character(sub$value))

!# I often feel stressed (-2(1),-1(8); +1(15),+2(1)): mean = 0.18, sd=0.87
sub <- subset(db, variable == "personality_stress")
describeBy(as.numeric(as.character(sub$value)))
as.numeric(as.character(sub$value))

 
!# I often feel bored (-2(14),-1(13);+1(4),+2(3): mean = -0.78, sd=1.25
sub <- subset(db, variable == "personality_boredom")
describeBy(as.numeric(as.character(sub$value)))
as.numeric(as.character(sub$value))

!#I see myself as someone who is relaxed, handles stress well: mean=0.1, sd=092
sub <- subset(db, variable == "personality_neuroticism_1_r")
describeBy(as.numeric(as.character(sub$value)))
as.numeric(as.character(sub$value))


# week N=46:
!#Which of the following best describes your activities in the next 7 days?: stud: 39.5%, stud+work:34.9%, work:14%, free:11.6%
sub <- subset(db, variable == "week_workload_type")
freq(as.character(sub$value))


#Are you looking forward to your activities in the next 7 days(-2(2),-1(10);+1(15),+2(4): mean: 0.2, sd=1.04
sub <- subset(db, variable == "week_workload_valence")
describeBy(as.numeric(as.character(sub$value)))
as.numeric(as.character(sub$value))

!#Are you looking forward to your activities in the next 7 days(-2(2),-1(10);+1(15),+2(4): mean: 0.2, sd=1.04
sub <- subset(db, variable == "week_workload_valence")
describeBy(as.numeric(as.character(sub$value)))
as.numeric(as.character(sub$value))

#How busy are you in the next 7 days? mean:0.89, sd:0.99
sub <- subset(db, variable == "week_workload_time")
describeBy(as.numeric(as.character(sub$value)))
as.numeric(as.character(sub$value))




