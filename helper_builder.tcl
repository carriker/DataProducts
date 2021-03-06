cd "C:/DataScience/DataProducts"

set fi [open "census.csv" "r"]
gets $fi line
puts $line

set year {}
set age_group {}
set gender {}
set education_level {}
set samples {}
set income {}

set values 0
while {![eof $fi]} {
	gets $fi line
	if {$line != ""} {
		set fields [split $line ","]
		lappend year [lindex $fields 0]
		lappend age_group [lindex $fields 1]
		lappend gender [lindex $fields 2]
		lappend education_level [lindex $fields 3]
		lappend samples [lindex $fields 4]
		lappend income [lindex $fields 5]
		incr values
	}
}

close $fi

set fo [open "helper.R" "w"]
puts $fo "# helper.R"
puts $fo "# Wayne Carriker"
puts $fo "# October 14, 2015"
puts $fo "# if you can't read the data file, build the data frame \"by hand\""
puts $fo "# actually build it step-wise, but have the steps automatically"
puts $fo "# generated by \"helper_builder.tcl\""
puts $fo ""

for {set i 0} {$i < 27} {incr i} {
	puts -nonewline $fo "y$i <- c([lindex $year [expr $i * 100]]"
	for {set j 1} {$j < 100} {incr j} {
		puts -nonewline $fo ", [lindex $year [expr $i * 100 + $j]]"
	}
	puts $fo ")"
}
puts -nonewline $fo "Year <- c(y0"
for {set i 1} {$i < 27} {incr i} {
	puts -nonewline $fo ", y$i"
}
puts $fo ")"
puts $fo ""

for {set i 0} {$i < 27} {incr i} {
	puts -nonewline $fo "a$i <- c(\"[lindex $age_group [expr $i * 100]]\""
	for {set j 1} {$j < 100} {incr j} {
		puts -nonewline $fo ", \"[lindex $age_group [expr $i * 100 + $j]]\""
	}
	puts $fo ")"
}
puts -nonewline $fo "Age_Group <- as.factor(c(a0"
for {set i 1} {$i < 27} {incr i} {
	puts -nonewline $fo ", a$i"
}
puts $fo "))"
puts $fo ""

for {set i 0} {$i < 27} {incr i} {
	puts -nonewline $fo "g$i <- c(\"[lindex $gender [expr $i * 100]]\""
	for {set j 1} {$j < 100} {incr j} {
		puts -nonewline $fo ", \"[lindex $gender [expr $i * 100 + $j]]\""
	}
	puts $fo ")"
}
puts -nonewline $fo "Gender <- as.factor(c(g0"
for {set i 1} {$i < 27} {incr i} {
	puts -nonewline $fo ", g$i"
}
puts $fo "))"
puts $fo ""

for {set i 0} {$i < 27} {incr i} {
	puts -nonewline $fo "e$i <- c(\"[lindex $education_level [expr $i * 100]]\""
	for {set j 1} {$j < 100} {incr j} {
		puts -nonewline $fo ", \"[lindex $education_level [expr $i * 100 + $j]]\""
	}
	puts $fo ")"
}
puts -nonewline $fo "Education_Level <- as.factor(c(e0"
for {set i 1} {$i < 27} {incr i} {
	puts -nonewline $fo ", e$i"
}
puts $fo "))"
puts $fo ""

for {set i 0} {$i < 27} {incr i} {
	puts -nonewline $fo "s$i <- c([lindex $samples [expr $i * 100]]"
	for {set j 1} {$j < 100} {incr j} {
		puts -nonewline $fo ", [lindex $samples [expr $i * 100 + $j]]"
	}
	puts $fo ")"
}
puts -nonewline $fo "Samples <- c(s0"
for {set i 1} {$i < 27} {incr i} {
	puts -nonewline $fo ", s$i"
}
puts $fo ")"
puts $fo ""

for {set i 0} {$i < 27} {incr i} {
	puts -nonewline $fo "i$i <- c([lindex $income [expr $i * 100]]"
	for {set j 1} {$j < 100} {incr j} {
		puts -nonewline $fo ", [lindex $income [expr $i * 100 + $j]]"
	}
	puts $fo ")"
}
puts -nonewline $fo "Income <- c(i0"
for {set i 1} {$i < 27} {incr i} {
	puts -nonewline $fo ", i$i"
}
puts $fo ")"
puts $fo ""

puts $fo "data <- data.frame(Year, Age_Group, Gender, Education_Level, Samples, Income)"

close $fo
