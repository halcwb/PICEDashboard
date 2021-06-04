# PICEDashboard
This application is based on [SAFE Stack](https://safe-stack.github.io/). It was created using the dotnet [SAFE Template](https://safe-stack.github.io/docs/template-overview/). If you want to learn more about the template why not start with the [quick start](https://safe-stack.github.io/docs/quickstart/) guide?

## Install pre-requisites
You'll need to install the following pre-requisites in order to build SAFE applications

* The [.NET Core SDK](https://www.microsoft.com/net/download) 3.1 or higher.
* [npm](https://nodejs.org/en/download/) package manager.
* [Node LTS](https://nodejs.org/en/download/).

## Starting the application
Start the server:
```bash
cd src\Server\
dotnet run
```

Start the client:
```bash
dotnet fable watch src\Client --run webpack-dev-server
```

Open a browser to `http://localhost:8080` to view the site.

## Deploying the application

1. Bundle the client application in the root folder. This will build the client application according to the specs in the `webpack.config.js`, using the following commands:
```bash
# make sure that the client is build
dotnet fable src/Client/Client.fsproj
# bundles the client
npm run build
```

2. Build the server application using the following commands:
```bash
cd src/Server
dotnet publish -c release -o ../../deploy/Server
```

3. Copy the data folder to the root of application folder


The result is the following folder structure:
> deploy/<hr>
>   Client/<hr>
>       public/<hr>
>   Server/<hr>




## SAFE Stack Documentation
If you want to know more about the full Azure Stack and all of its components (including Azure) visit the official [SAFE documentation](https://safe-stack.github.io/docs/).

You will find more documentation about the used F# components at the following places:

* [Saturn](https://saturnframework.org/docs/)
* [Fable](https://fable.io/docs/)
* [Elmish](https://elmish.github.io/elmish/)

## Things to do
- [ ] Need to nuget the libs in the libs folder, i.e. `Informedica.PICE.Lib`
- [ ] Maybe make the data folder relative to the deploy folder (currently in the root of the repository)
