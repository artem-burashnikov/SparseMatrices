``` ini

BenchmarkDotNet=v0.13.4, OS=ubuntu 22.04
AMD Ryzen 5 4500U with Radeon Graphics, 1 CPU, 6 logical and 6 physical cores
.NET SDK=6.0.113
  [Host]     : .NET 6.0.13 (6.0.1322.60201), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 6.0.13 (6.0.1322.60201), X64 RyuJIT AVX2


```
| Method |     Mean |    Error |   StdDev | Ratio | RatioSD |
|------- |---------:|---------:|---------:|------:|--------:|
|   Base | 633.4 ms |  8.29 ms |  7.75 ms |  1.00 |    0.00 |
| Level2 | 568.0 ms | 11.10 ms | 16.27 ms |  0.89 |    0.03 |
| Level4 | 547.6 ms | 10.70 ms | 23.94 ms |  0.89 |    0.03 |
