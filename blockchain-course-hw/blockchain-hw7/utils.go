package main

import (
	"errors"
	"fmt"
	"strconv"
)

const filePrm = "--file"
const numBiletsPrm = "--numbilets"
const paramPrm = "--parameter"

func extractParams(args []string) (file string, numBilets uint8, param int, e error) {
	if len(args) != 6 {
		return incorrectInputMessage()
	}
	for i := 0; i < len(args); i++ {
		s := args[i]
		if s == filePrm {
			if i+1 >= len(args) {
				return incorrectInputMessage()
			}
			file = args[i+1]
			i += 1
		} else if s == numBiletsPrm {
			if i+1 >= len(args) {
				return incorrectInputMessage()
			}
			numBiletsInt, err := strconv.Atoi(args[i+1])
			if err != nil {
				return incorrectInputMessage()
			}
			if numBiletsInt <= 0 {
				return incorrectInputMessage("NumBilets can't be zero or less")
			}
			i += 1
			numBilets = uint8(numBiletsInt)
		} else if s == paramPrm {
			if i+1 >= len(args) {
				return incorrectInputMessage()
			}
			var err error
			param, err = strconv.Atoi(args[i+1])
			if err != nil {
				return incorrectInputMessage()
			}
			i += 1
		} else {
			return incorrectInputMessage()
		}
	}
	return file, numBilets, param, nil
}

func incorrectInputMessage(message ...string) (string, uint8, int, error) {
	m := "Incorrect set of params. Usage: --file <filename> --numbilets <num> --parameter <num>\n"
	if len(message) != 0 {
		m += message[0]
	}
	fmt.Println(m)
	return "", 0, -1, errors.New("incorrect input")
}
