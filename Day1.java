import java.nio.file.*;
import java.io.*;
import java.util.*;

public class Day1 {
    private String slurp(filename String) throws IOException {
        return Files.ReadString(Path.of(filename))
    }
    public static void main(String[] args) throws IOException {
        Integer silver = Arrays.stream(splurp("day1.txt").split("\n\n"))
            .map(elf -> Arrays.stream(elf.split("\n")))
            .map(elf -> elf.mapToInt(Integer::parseInt))
            .map(elf -> elf.sum())
            .max(Integer::compare).get();
        Integer gold = Arrays.stream(Files.readString(Path.of("day1.txt")).split("\n\n"))
            .map(elf -> Arrays.stream(elf.split("\n")))
            .map(elf -> elf.mapToInt(Integer::parseInt))
            .map(elf -> elf.sum())
            .sorted(Collections.reverseOrder())
            .limit(3)
            .reduce(Integer::sum).get();
        System.out.println("gold: " + gold + " silver: " + silver);
    }
}
