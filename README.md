# F# Programming Technologies course project

This project will be aiding its author in finishing the SPBU's F# course

---


## How to use
- After building a project on your system, open file explorer and navigate to
`2022-Programming-Technologies-F-sharp-course/src/spbu-fsharp`
- Open the terminal from inside the folder and execute the following code:
`dotnet run <function_name> <variable1> <variable2>`

[//]: # ()
[//]: # ()
[//]: # (GitHub Actions |)

[//]: # (:---: |)

[//]: # ([![GitHub Actions]&#40;https://github.com/artem-burashnikov/spbu-fsharp/workflows/Build%20main/badge.svg&#41;]&#40;https://github.com/artem-burashnikov/spbu-fsharp/actions?query=branch%3Amain&#41; |)

[//]: # ([![Build History]&#40;https://buildstats.info/github/chart/artem-burashnikov/spbu-fsharp&#41;]&#40;https://github.com/artem-burashnikov/spbu-fsharp/actions?query=branch%3Amain&#41; |)

[//]: # ()
[//]: # (## NuGet)

[//]: # ()
[//]: # (Package | Stable | Prerelease)

[//]: # (--- | --- | ---)

[//]: # (spbu-fsharp | [![NuGet Badge]&#40;https://buildstats.info/nuget/spbu-fsharp&#41;]&#40;https://www.nuget.org/packages/spbu-fsharp/&#41; | [![NuGet Badge]&#40;https://buildstats.info/nuget/spbu-fsharp?includePreReleases=true&#41;]&#40;https://www.nuget.org/packages/spbu-fsharp/&#41;)

[//]: # ()
[//]: # ()
[//]: # (---)


[//]: # (### Developing)

[//]: # ()
[//]: # ()
[//]: # (Make sure the following **requirements** are installed on your system:)

[//]: # ()
[//]: # ()
[//]: # (- [dotnet SDK]&#40;https://www.microsoft.com/net/download/core&#41; 3.0 or higher)

[//]: # ()
[//]: # (- [Mono]&#40;http://www.mono-project.com/&#41; if you're on Linux or macOS.)

[//]: # ()
[//]: # ()
[//]: # (or)

[//]: # ()
[//]: # ()
[//]: # (- [VSCode Dev Container]&#40;https://code.visualstudio.com/docs/remote/containers&#41;)



---

[//]: # ()
[//]: # (### Environment Variables)

[//]: # ()
[//]: # (- `CONFIGURATION` will set the [configuration]&#40;https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-build?tabs=netcore2x#options&#41; of the dotnet commands.  If not set, it will default to Release.)

[//]: # (  - `CONFIGURATION=Debug ./build.sh` will result in `-c` additions to commands such as in `dotnet build -c Debug`)

[//]: # (- `GITHUB_TOKEN` will be used to upload release notes and NuGet packages to GitHub.)

[//]: # (  - Be sure to set this before releasing)

[//]: # (- `DISABLE_COVERAGE` Will disable running code coverage metrics.  AltCover can have [severe performance degradation]&#40;https://github.com/SteveGilham/altcover/issues/57&#41; so it's worth disabling when looking to do a quicker feedback loop.)

[//]: # (  - `DISABLE_COVERAGE=1 ./build.sh`)

[//]: # ()
[//]: # ()
[//]: # (---)

[//]: # ()
[//]: # (### Building)

[//]: # ()
[//]: # ()
[//]: # ()
[//]: # (```sh)

[//]: # ()
[//]: # (> build.cmd <optional buildtarget> // on windows)

[//]: # ()
[//]: # ($ ./build.sh  <optional buildtarget>// on unix)

[//]: # ()
[//]: # (```)

[//]: # ()
[//]: # ()
[//]: # (---)


### List of available functions

- `pow [<base-number>] [<exponent-number>]`
- `q_pow [<base-number>] [<exponent-number>]`

[//]: # (- `Clean` - Cleans artifact and temp directories.)

[//]: # (- `DotnetRestore` - Runs [dotnet restore]&#40;https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-restore?tabs=netcore2x&#41; on the [solution file]&#40;https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019&#41;.)

[//]: # (- [`DotnetBuild`]&#40;#Building&#41; - Runs [dotnet build]&#40;https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-build?tabs=netcore2x&#41; on the [solution file]&#40;https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019&#41;.)

[//]: # (- `DotnetTest` - Runs [dotnet test]&#40;https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-test?tabs=netcore21&#41; on the [solution file]&#40;https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019.&#41;.)

[//]: # (- `GenerateCoverageReport` - Code coverage is run during `DotnetTest` and this generates a report via [ReportGenerator]&#40;https://github.com/danielpalme/ReportGenerator&#41;.)

[//]: # (- `WatchApp` - Runs [dotnet watch]&#40;https://docs.microsoft.com/en-us/aspnet/core/tutorials/dotnet-watch?view=aspnetcore-3.0&#41; on the application. Useful for rapid feedback loops.)

[//]: # (- `WatchTests` - Runs [dotnet watch]&#40;https://docs.microsoft.com/en-us/aspnet/core/tutorials/dotnet-watch?view=aspnetcore-3.0&#41; with the test projects. Useful for rapid feedback loops.)

[//]: # (- `GenerateAssemblyInfo` - Generates [AssemblyInfo]&#40;https://docs.microsoft.com/en-us/dotnet/api/microsoft.visualbasic.applicationservices.assemblyinfo?view=netframework-4.8&#41; for libraries.)

[//]: # (- `CreatePackages` - Runs the packaging task from [dotnet-packaging]&#40;https://github.com/qmfrederik/dotnet-packaging&#41;. This creates applications for `win-x64`, `osx-x64` and `linux-x64` - [Runtime Identifiers]&#40;https://docs.microsoft.com/en-us/dotnet/core/rid-catalog&#41;.  )

[//]: # (    - Bundles the `win-x64` application in a .zip file.)

[//]: # (    - Bundles the `osx-x64` application in a .tar.gz file.)

[//]: # (    - Bundles the `linux-x64` application in a .tar.gz file.)

[//]: # (- `GitRelease` - Creates a commit message with the [Release Notes]&#40;https://fake.build/apidocs/v5/fake-core-releasenotes.html&#41; and a git tag via the version in the `Release Notes`.)

[//]: # (- `GitHubRelease` - Publishes a [GitHub Release]&#40;https://help.github.com/en/articles/creating-releases&#41; with the Release Notes and any NuGet packages.)

[//]: # (- `FormatCode` - Runs [Fantomas]&#40;https://github.com/fsprojects/fantomas&#41; on the solution file.)

[//]: # (- [`Release`]&#40;#Releasing&#41; - Task that runs all release type tasks such as `GitRelease` and `GitHubRelease`. Make sure to read [Releasing]&#40;#Releasing&#41; to setup your environment correctly for releases.)

[//]: # ()
[//]: # (---)

[//]: # ()
[//]: # ()
[//]: # (### Releasing)

[//]: # ()
[//]: # (- [Start a git repo with a remote]&#40;https://help.github.com/articles/adding-an-existing-project-to-github-using-the-command-line/&#41;)

[//]: # ()
[//]: # (```sh)

[//]: # (git add .)

[//]: # (git commit -m "Scaffold")

[//]: # (git remote add origin https://github.com/user/MyCoolNewApp.git)

[//]: # (git push -u origin master)

[//]: # (```)

[//]: # ()
[//]: # (- [Create a GitHub OAuth Token]&#40;https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/&#41;)

[//]: # (  - You can then set the `GITHUB_TOKEN` to upload release notes and artifacts to github)

[//]: # (  - Otherwise it will fallback to username/password)

[//]: # ()
[//]: # (- Then update the `CHANGELOG.md` with an "Unreleased" section containing release notes for this version, in [KeepAChangelog]&#40;https://keepachangelog.com/en/1.1.0/&#41; format.)

[//]: # ()
[//]: # ()
[//]: # (NOTE: Its highly recommend to add a link to the Pull Request next to the release note that it affects. The reason for this is when the `RELEASE` target is run, it will add these new notes into the body of git commit. GitHub will notice the links and will update the Pull Request with what commit referenced it saying ["added a commit that referenced this pull request"]&#40;https://github.com/TheAngryByrd/MiniScaffold/pull/179#ref-commit-837ad59&#41;. Since the build script automates the commit message, it will say "Bump Version to x.y.z". The benefit of this is when users goto a Pull Request, it will be clear when and which version those code changes released. Also when reading the `CHANGELOG`, if someone is curious about how or why those changes were made, they can easily discover the work and discussions.)

[//]: # ()
[//]: # ()
[//]: # ()
[//]: # (Here's an example of adding an "Unreleased" section to a `CHANGELOG.md` with a `0.1.0` section already released.)

[//]: # ()
[//]: # (```markdown)

[//]: # (## [Unreleased])

[//]: # ()
[//]: # (### Added)

[//]: # (- Does cool stuff!)

[//]: # ()
[//]: # (### Fixed)

[//]: # (- Fixes that silly oversight)

[//]: # ()
[//]: # (## [0.1.0] - 2017-03-17)

[//]: # (First release)

[//]: # ()
[//]: # (### Added)

[//]: # (- This release already has lots of features)

[//]: # ()
[//]: # ([Unreleased]: https://github.com/user/MyCoolNewApp.git/compare/v0.1.0...HEAD)

[//]: # ([0.1.0]: https://github.com/user/MyCoolNewApp.git/releases/tag/v0.1.0)

[//]: # (```)

[//]: # ()
[//]: # (- You can then use the `Release` target, specifying the version number either in the `RELEASE_VERSION` environment)

[//]: # (  variable, or else as a parameter after the target name.  This will:)

[//]: # (  - update `CHANGELOG.md`, moving changes from the `Unreleased` section into a new `0.2.0` section)

[//]: # (    - if there were any prerelease versions of 0.2.0 in the changelog, it will also collect their changes into the final 0.2.0 entry)

[//]: # (  - make a commit bumping the version:  `Bump version to 0.2.0` and adds the new changelog section to the commit's body)

[//]: # (  - push a git tag)

[//]: # (  - create a GitHub release for that git tag)

[//]: # ()
[//]: # ()
[//]: # (macOS/Linux Parameter:)

[//]: # ()
[//]: # (```sh)

[//]: # (./build.sh Release 0.2.0)

[//]: # (```)

[//]: # ()
[//]: # (macOS/Linux Environment Variable:)

[//]: # ()
[//]: # (```sh)

[//]: # (RELEASE_VERSION=0.2.0 ./build.sh Release)

[//]: # (```)
