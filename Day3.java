import java.nio.file.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class Day3 {
    static private int intersect(String line) {
        int length = line.length();
        IntStream first = line.substring(0, length / 2).chars();
        List<Integer> second = line.substring(length / 2, length).chars().boxed().collect(Collectors.toList());
        return first.filter(second::contains).findFirst().orElseThrow();
    }
    static private int silver(String filename) throws IOException {
        return (int) Files.lines(Path.of(filename))
            .map(Day3::intersect)
            .reduce(Integer::sum)
            .orElseThrow();
    }
    static private int gold(String filename) throws IOException {
        AtomicInteger counter = new AtomicInteger();
        return Files.lines(Path.of(filename))
                .collect(Collectors.groupingBy(x -> counter.getAndIncrement() / 3)).values()
                .size();
    }
    public static void main(String[] args) throws IOException {
        System.out.printf("silver:\t%10d\ngold:\t%10d\n",
                          silver("/home/sendai/Downloads/new_big_30000_256-2048.txt"),
                          gold("day3.test.txt"));
    }
}
