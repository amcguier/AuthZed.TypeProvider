namespace AuthZed.TypeProvider
module private AssemblyInfo =
    open System.Runtime.CompilerServices
    
    [<assembly:CompilerServices.TypeProviderAssembly("AuthZed.TypeProvider.DesignTime.dll")>]    
    [<assembly: InternalsVisibleTo("AuthZed.TypeProvider.Tests")>]
     do()
