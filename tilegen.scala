#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#
import scala.io.StdIn._

// write some text out to the user with Console.print
println("Observatory")

// readLine: lets you prompt the user and also read their command line input
val name = readLine("Year? ")

// readInt: read a simple Int
print("Year (1975 - 2015)? ")
val year = readInt()

// readInt: read a simple Int
print("Zoom (0 - 3)? ")
val zoom = readInt()




//
//
//// you can also print output with printf
//printf("Your name is %s and you are %d years old.\n", name, age)
//
//// you can also use the Java Scanner class, if desired
//val scanner = new java.util.Scanner(System.in)
//print("Where do you live? ")
//val input = scanner.nextLine()
//print("You live in " + input)
