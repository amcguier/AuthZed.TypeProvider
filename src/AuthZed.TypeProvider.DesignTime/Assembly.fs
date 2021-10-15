namespace AuthZed.TypeProvider.DesignTime

module private Assembly =
    open System.Runtime.CompilerServices
    
    [<assembly: InternalsVisibleTo("AuthZed.TypeProvider.Tests")>]
    [<assembly: InternalsVisibleTo("AuthZed.TypeProvider")>]
     do()
