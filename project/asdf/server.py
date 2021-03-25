# references: 
# TA Kimmo Karkkainen's slides
# https://asyncio.readthedocs.io/en/latest/tcp_echo.html#tcp-echo-client
# https://aiohttp.readthedocs.io/en/stable/

import sys # for command line arguments
import asyncio
import time # for timing when messages are received/sent between the client and server
import aiohttp # for HTTP request to the Google Places API
import json # the Google Places API returns a JSON

# for bidirectional communication between servers 
server_connections = {
    'Goloman': ['Hands', 'Holiday', 'Wilkes'],
    'Hands': ['Goloman', 'Wilkes'],
    'Holiday': ['Goloman', 'Welsh', 'Wilkes'],
    'Welsh': ['Holiday'],
    'Wilkes': ['Goloman', 'Hands', 'Holiday']
}

server_ports = {
    'Goloman': 12372,
    'Hands': 12373,
    'Holiday': 12374,
    'Welsh': 12375,
    'Wilkes': 12376
}

# dictionary that maps a client ID to a list containing
# [Coordinates, Time client sent message, Time server received message, Server name]
clients = {}

API_KEY = 'hidden'

# input: coordinate string: +34.068930-118.445127
# output: (latitude, longitude) tuple: ('+34.068930', '-118.445127')
# return None if input is invalid
def separate_lat_and_long(coordinates):
    positions_of_signs = []
    for i in range(len(coordinates)):
        # if the current character is a sign
        if coordinates[i] == '+' or coordinates[i] == '-':
            positions_of_signs.append(i)

    # there should only be 2 signs
    if len(positions_of_signs) != 2:
        return None

    # one of the signs should be at the beginning, and no signs should be at the end
    if 0 not in positions_of_signs or (len(coordinates) - 1) in positions_of_signs:
        return None

    latitude = coordinates[positions_of_signs[0]:positions_of_signs[1]]
    longitude = coordinates[positions_of_signs[1]:]

    return (latitude,longitude)

# note: the message has been split at this point and is represented as an array
# returns a string representing the message type, or "Invalid" if the message isn't valid
def check_valid_message(message):
    if len(message) < 1:
        return "Invalid"

    command_name = message[0]

    # check that IAMAT messages have 4 tokens and valid coordinates
    # note: message[2] = coordinates
    if command_name == 'IAMAT':
        if len(message) == 4 and separate_lat_and_long(message[2]):
            return command_name

    # check that WHATSAT messages have 4 tokens, a valid radius, and a valid upper bound
    elif command_name == 'WHATSAT':
        if len(message) == 4:
            radius = int(message[2])
            upper_bound = int(message[3])
            if radius > 0 and radius <= 50 and upper_bound > 0 and upper_bound <= 20:
                return command_name

    # check that UPDATE_CLIENT messages have 6 tokens
    elif command_name == 'UPDATE_CLIENT':
        if len(message) == 6:
            return command_name

    return 'Invalid'

# format for UPDATE_CLIENT message:
# UPDATE_CLIENT [Client ID] [Updated coordinates] [Time sent] [Time received] [Server name]
# note: this message is only used for inter-server communication to flood messages
async def handle_UPDATE_CLIENT_message(message):
    client_ID = message[1]
    time_sent = message[3]

    if client_ID in clients:
        time_client_sent_message = clients[client_ID][1]
        # if the client hasn't received the message yet
        if time_sent > time_client_sent_message:
            clients[client_ID] = message[2:]
            flood_message = ' '.join(message)
            asyncio.ensure_future(flood(flood_message, server_name))
    else: # client_ID is NOT in clients -> flood message
        clients[client_ID] = message[2:]
        flood_message = ' '.join(message)
        asyncio.ensure_future(flood(flood_message, server_name))

# format for IAMAT message:
# IAMAT [Client ID] [Coordinates] [Timestamp (POSIX)]
# Ex: IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
# format for server response:
# AT [Server name] [Timestamp diff] [Copy of client data]
async def handle_IAMAT_message(message, time_received):
    client_ID = message[1]
    coordinates = message[2]
    time_sent = message[3]

    # update clients dictionary
    clients[client_ID] = [coordinates, time_sent, time_received, server_name]

    time_difference = str(float(time_received) - float(time_sent))

    # prepend a '+' if the difference is positive
    if time_difference[0] != '-':
        time_difference = '+' + time_difference
    
    # form server response
    copy_of_client_data = ' '.join(message[1:])
    server_response = ' '.join(['AT',server_name,time_difference,copy_of_client_data])

    # current server must inform other servers about the updated location
    # we flood an UPDATE_CLIENT message to all the other servers
    flood_message_list = message[1:]
    flood_message_list.append(time_received)
    flood_message_list.append(server_name)

    flood_message = "UPDATE_CLIENT " + ' '.join(flood_message_list)
    asyncio.ensure_future(flood(flood_message, server_name))

    return server_response

async def flood(message, server):
    for curr_server in server_connections[server]:
        log_file.write("Trying to connect server " + curr_server + " to port " + str(server_ports[curr_server]) + "\n")
        try:
            reader,writer = await asyncio.open_connection(host='127.0.0.1', port=server_ports[curr_server], loop=event_loop)
            log_file.write("Successfully connected.\n")
            writer.write(message.encode())
            await writer.drain()
            writer.write_eof()
            writer.close()
        except:
            log_file.write("Failed to connect.\n")
            pass

# format for WHATSAT message:
# WHATSAT [Name of another client] [Radius (km)] [Max # of results]
# Ex: WHATSAT kiwi.cs.ucla.edu 10 5
async def handle_WHATSAT_message(message):
    client_ID = message[1]
    if client_ID not in clients:
        server_response = "? " + ' '.join(message)
        return server_response

    radius = message[2]
    upper_bound = int(message[3])

    coordinates, time_client_sent, time_server_received, clients_server_name = clients[client_ID]

    latitude, longitude = separate_lat_and_long(coordinates)
    location = latitude + "," + longitude
    location = location.replace("+","")

    radius = str(int(radius) * 1000) # convert radius from km to m

    request_url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={0}&radius={1}&key={2}'.format(location,radius,API_KEY)

    time_difference = str(float(time_server_received) - float(time_client_sent))

    if time_difference[0] != '-':
        time_difference = '+' + time_difference

    server_response = ' '.join(['AT',clients_server_name,time_difference,client_ID,coordinates,time_client_sent])

    # request Google Places API for location data
    async with aiohttp.ClientSession() as session:
        places_json = await fetch(session, request_url)

    # enforce upper bound on amount of information to return
    places_json['results'] = places_json['results'][:upper_bound]

    formatted_places_json = json.dumps(places_json, indent=3)

    server_response += formatted_places_json + "\n"

    return server_response

async def fetch(session, url):
    async with session.get(url) as response:
        return await response.json()

# server implementation
async def handle_connection(reader, writer):
    
    # READLINE OR READ(1000)
    data = await reader.read(1000)
    original_received_message = data.decode()
    time_received = time.time()

    log_file.write("RECEIVED: " + original_received_message + "\n")

    received_message = original_received_message.strip() # remove leading and trailing spaces
    received_message = received_message.split() # convert message into a list of strings

    message_type = check_valid_message(received_message)
    
    server_response = ''

    if message_type == 'Invalid':
        server_response = "? " + original_received_message

    elif message_type == 'IAMAT':
        server_response = await handle_IAMAT_message(received_message, str(time_received))

    elif message_type == 'UPDATE_CLIENT':
        await handle_UPDATE_CLIENT_message(received_message)

    elif message_type == 'WHATSAT':
        server_response = await handle_WHATSAT_message(received_message)

    if message_type != 'UPDATE_CLIENT':
        log_file.write("SENDING: " + server_response + "\n")
        writer.write(server_response.encode())
        await writer.drain()
        writer.write_eof()
        writer.close()

def main():
    # command line argument checking
    if len(sys.argv) != 2:
        print("Error: incorrect number of arguments.\nUsage: python3 server.py [server_name]")
        exit()
    elif sys.argv[1] not in server_connections:
        print("Error: invalid server name.\nUsage: python3 server.py [server_name]")
        exit()

    global server_name
    server_name = sys.argv[1]
    
    log_file_name = server_name + "-log.txt"

    global log_file
    log_file = open(log_file_name, 'w+') # open for reading and appending
    log_file.truncate(0)

    # set up event loop
    global event_loop
    event_loop = asyncio.get_event_loop()
    coroutine = asyncio.start_server(handle_connection, host='127.0.0.1', port=server_ports[server_name], loop=event_loop)
    server = event_loop.run_until_complete(coroutine)

    # serve requests until Ctrl+C is pressed
    print('Serving on {}'.format(server.sockets[0].getsockname()))
    try:
        event_loop.run_forever()
    except KeyboardInterrupt:
        pass

    # close the server
    server.close()
    event_loop.run_until_complete(server.wait_closed())
    event_loop.close()
    log_file.close()

if __name__=='__main__':
    main()
