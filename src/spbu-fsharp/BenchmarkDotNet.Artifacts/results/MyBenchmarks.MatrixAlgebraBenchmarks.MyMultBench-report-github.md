``` ini

BenchmarkDotNet=v0.13.4, OS=ubuntu 22.04
AMD Ryzen 5 4500U with Radeon Graphics, 1 CPU, 6 logical and 6 physical cores
.NET SDK=6.0.113
  [Host]     : .NET 6.0.13 (6.0.1322.60201), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 6.0.13 (6.0.1322.60201), X64 RyuJIT AVX2


```
| Method |       Mean |    Error |    StdDev | Ratio | RatioSD |
|------- |-----------:|---------:|----------:|------:|--------:|
|   Base | 2,078.9 ms |  6.39 ms |   5.66 ms |  1.00 |    0.00 |
| Level2 |   936.9 ms | 18.42 ms |  33.22 ms |  0.45 |    0.02 |
| Level4 |   989.4 ms | 63.86 ms | 184.24 ms |  0.42 |    0.05 |
