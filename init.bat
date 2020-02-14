cd ..
dotnet new sln -o Claw
cd Claw
dotnet new classlib -lang "F#" -o Claw.Core
dotnet add Claw.Core\Claw.Core.fsproj package FParsec
dotnet sln add Claw.Core\Claw.Core.fsproj
dotnet new console -lang "F#" -o Claw
dotnet add Claw\Calw.fsproj reference Claw.Core\Claw.Core.fsproj
dotnet sln add Claw\Claw.fsproj

dotnet new nunit -lang "F#" -o Claw.Test

dotnet add Claw.Test\Claw.Test.fsproj reference Claw.Core\Claw.Core.fsproj
dotnet add Claw.Test\Claw.Test.fsproj reference Claw\Claw.fsproj
dotnet sln add Claw.Test\Claw.Test.fsproj
cd Claw
dotnet run Hello World