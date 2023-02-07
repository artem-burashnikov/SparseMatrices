``` ini

BenchmarkDotNet=v0.13.4, OS=ubuntu 22.04
AMD Ryzen 5 4500U with Radeon Graphics, 1 CPU, 6 logical and 6 physical cores
.NET SDK=6.0.113
  [Host]     : .NET 6.0.13 (6.0.1322.60201), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 6.0.13 (6.0.1322.60201), X64 RyuJIT AVX2


```
| Method |       Mean |    Error |    StdDev | Ratio | RatioSD |
|------- |-----------:|---------:|----------:|------:|--------:|
|   Base | 2,203.8 ms | 26.42 ms |  24.72 ms |  1.00 |    0.00 |
| Level2 |   959.6 ms | 19.07 ms |  38.96 ms |  0.43 |    0.02 |
| Level4 |   935.6 ms | 43.98 ms | 128.99 ms |  0.42 |    0.05 |
