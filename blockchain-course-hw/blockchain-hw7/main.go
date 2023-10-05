package main

import (
	"bufio"
	"crypto/sha1"
	"encoding/hex"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	fileName, numBilets, param, e := extractParams(os.Args[1:])
	if e != nil {
		return
	}
	f, err := os.Open(fileName)
	if err != nil {
		log.Fatal(err)
	}

	defer f.Close()

	scanner := bufio.NewScanner(f)

	for scanner.Scan() {
		str := scanner.Text()
		m := sha1.Sum([]byte(str + strconv.Itoa(param)))
		fmt.Println(str, getIntFromHashString(hex.EncodeToString(m[:]), numBilets))
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}

func getIntFromHashString(hash string, numBilets uint8) uint8 {
	var res uint8
	for i := 0; i < len(hash); i++ {
		res += hash[i]
	}
	return res%numBilets + 1
}
