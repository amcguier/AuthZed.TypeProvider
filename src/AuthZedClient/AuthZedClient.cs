using System.Net.Http;
using System.Threading.Tasks;
using Grpc.Net.Client;
using Grpc.Core.Interceptors;
using Grpc.Net.Client.Configuration;
using Authzed.Api.V1;
using Grpc.Core;

namespace AuthZed.Api
{
    public class AuthZedContext
    {
        private readonly string Address;
        private readonly string Token;

        public AuthZedContext(string address, string token)
        {
            Address = address;
            Token = token;
        }

        private GrpcChannel GetChannel()
        {
            /*var credentials = ChannelCredentials.Create(ChannelCredentials.Insecure, CallCredentials.FromInterceptor(
                (context, metadata) =>
                {
                    metadata.Add("Authorization", $"Bearer {Token}");
                    return Task.CompletedTask;
                }));
            */
            // , new GrpcChannelOptions { Credentials = credentials}
            return GrpcChannel.ForAddress(Address);
        }

        public PermissionsService.PermissionsServiceClient GetPermissionClient()
        {
            return new PermissionsService.PermissionsServiceClient(GetChannel());
        }

        public SchemaService.SchemaServiceClient GetSchemaClient()
        {
            return new SchemaService.SchemaServiceClient(GetChannel());
        }

        public WatchService.WatchServiceClient GetWatchclient()
        {
            return new WatchService.WatchServiceClient(GetChannel());
        }

    }

}

