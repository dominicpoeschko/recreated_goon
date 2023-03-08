#include "package.hpp"
#include "aglio/packager.hpp"
#include "aglio/remote_fmt.hpp"
#include "toxic_spokes/IP/Socket.hpp"
#include "CRC.h"
#include <span>
#include <cassert>

struct Crc {
    using type = std::uint16_t;
    static type calc(std::span<std::byte const> data){
        return CRC::Calculate(data.data(), data.size(),CRC::CRC_16_ARC());
    }
};

using packager = aglio::Packager<aglio::CrcConfig<Crc>>;

int main(){
    ts::UDP_ServerSocket server{2913};
    assert(server.is_valid());
    
    ts::UDP_ClientSocket client{"localhost", 2913};
    assert(client.is_valid());

    package::data d;
    d.number = 5;
    d.decimal = 13.2;
    d.name = "test123";

    std::vector<std::byte> txBuffer;
    packager::pack(txBuffer, d);
    client.send(txBuffer);

    std::vector<std::byte> rxBuffer;
    rxBuffer.resize(txBuffer.size());
    server.recv(std::span(rxBuffer));
    
    auto optionalPack = packager::unpack<package::data>(rxBuffer);
    if(optionalPack.has_value()){
        auto& pack = optionalPack.value();
        assert(pack.number == d.number);
        assert(pack.decimal == d.decimal);
        assert(pack.name == d.name);
    }
    return 0;
}