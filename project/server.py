import sys
import time
import asyncio
import json
import argparse
import logging
import signal
import aiohttp

API_KEY = 'AIzaSyCBQZmxboqM7LTptTtLAyosirxlU_Htojc'
server_port = {
        "Hill":      8000,#12350,
        "Jaquez":    8001,#12351,
        "Smith":     8002,#12352,
        "Campbell":  8003,#12353,
        "Singleton": 8004}#12354}
network = {
        "Hill":      ["Jaquez", "Smith"],
        "Jaquez":    ["Hill", "Singleton"],
        "Smith":     ["Hill", "Campbell", "Singleton"],
        "Campbell":  ["Singleton", "Smith"],
        "Singleton": ["Campbell", "Jaquez", "Smith"] }
client = {} #['AT', ORIGINAL_SERVER, TIME_DIFFERENCE, CLIENT, COORDINATES, CLIENT_TIME]
localhost = '127.0.0.1'

async def flood(server_name, message):
         for server in network[server_name]:
                  try:
                           reader, writer = await asyncio.open_connection(host=localhost, port=server_port[server])
                           writer.write(message.encode())
                           await writer.drain()
                           writer.close()
                           logging.info("Connected to server {}".format(server))
                  except:
                           logging.info("Can't connect to server {}".format(server))

def separate_coordinates(coordinates):
         sum_signs = coordinates.count('+') + coordinates.count('-')
         if sum_signs != 2:
                  return None
         if coordinates.endswith('+') or coordinates.endswith('-'):
                  return None
         if not(coordinates.startswith('+') or coordinates.startswith('-')):
                  return None

         i = coordinates.find('+', 1, len(coordinates) - 1)
         if i == -1:
                  i = coordinates.find('-', 1, len(coordinates) -1)
         latitude = coordinates[0:i]
         longitude = coordinates[i:]
         return latitude, longitude;

async def IAMAT(start_server, message):
         ID = message[1]
         coordinates = message[2]
         new_time = message[3]
         time_difference = str(time.time() - float(new_time))
         if time_difference[0] != '-':
                  time_difference = '+' + time_difference
         if ID not in client:
                  client[ID] = ['AT', start_server, time_difference, ID, coordinates, new_time]
                  flood_message = 'FLOOD ' + ' '.join(client[ID])
                  await flood(start_server, flood_message)
         else:
                  if float(new_time) > float(client[ID][5]):
                           client[ID] = ['AT', start_server, time_difference, ID, coordinates, new_time]
                           flood_message = 'FLOOD ' + ' '.join(client[ID])
                           await flood(start_server, flood_message)

async def WHATSAT(message):
         ID = message[1]
         radius = int(message[2])
         upper_bound = int(message[3])

         latitude, longitude = separate_coordinates(client[ID][4])
         coordinates = latitude + ',' + longitude
         coordinates = coordinates.replace('+', '')
         radius = str(radius * 1000)
         url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={}&radius={}&key={}'.format(coordinates, radius, API_KEY)
         async with aiohttp.ClientSession() as session:
                  raw_json = await fetch(session, url)
         raw_json['results'] = raw_json['results'][:upper_bound]
         logging.info("Successfully retrieved info from API")
         return json.dumps(raw_json, indent=4)

async def fetch(session, url):
         async with session.get(url) as response:
                  return await response.json()

async def handle_connection(reader, writer):
         data = await reader.readline()
         message = data.decode()
         message = message.strip()
         message = message.split()
         response = ''

         if not message:
                  logging.info("Received empty input")
                  pass
         elif len(message) != 4:
                  if message[0] == 'FLOOD':
                           ID = message[4]
                           if ID not in client:
                                    client[ID] = message[1:]
                                    logging.info("Received: {}".format(' '.join(message)))
                                    flood_message = ' '.join(message)
                                    await flood(start_server, flood_message)
                           else:
                                    if float(message[6]) > float(client[ID][5]):
                                             client[ID] = message[1:]
                                             logging.info("Received: {}".format(' '.join(message)))
                                             flood_message = ' '.join(message)
                                             await flood(start_server, flood_message)
                  else:
                           response = '? ' + ' '.join(message)
         elif message[0] == "IAMAT":
                  coordinates = message[2]
                  a, b = separate_coordinates(coordinates)
                  if a is None:
                           response = '? ' + ' '.join(message)
                  elif b is None:
                           response = '? ' + ' '.join(message)
                  elif not is_number(message[3]):
                           response = '? ' + ' '.join(message)
                  else:
                           await IAMAT(start_server, message)
                           response = ' '.join(client[message[1]])
         elif message[0] == "WHATSAT":
                  if not (is_number(message[2]) and is_number(message[3])):
                           response = '? ' + ' '.join(message)
                  elif int(message[2]) > 50 or int(message[2]) < 0:
                           response = '? ' + ' '.join(message)
                  elif int(message[3]) > 20 or int(message[3]) < 0:
                           response = '? ' + ' '.join(message)
                  elif message[1] not in client:
                           response = '? ' + ' '.join(message)
                  else:
                           locations = await WHATSAT(message)
                           locations = str(locations).strip('\n')
                           temp_response = ' '.join(client[message[1]])
                           response = "{}\n{}\n".format(temp_response, locations)
         else:
                  response = '? ' + ' '.join(message)

         if response:
                  logging.info(response)
                  writer.write(response.encode())
                  await writer.drain()
                  writer.write_eof()
                  writer.close()
def interupt_handler(signum, frame):
         logging.info("Shutting down server {}".format(start_server))
         sys.exit(-2)
def is_number(s):
         try:
                  float(s)
                  return True
         except ValueError:
                  return False

async def main():
         if len(sys.argv) != 2:
        	  print("./server.py 'server'")
        	  sys.exit(1)
         elif sys.argv[1] not in network:
        	  print("Invalid server name")
        	  sys.exit(1)
         signal.signal(signal.SIGINT, interupt_handler)
         global start_server
         start_server = sys.argv[1]
         logging.basicConfig(filename='server-{}.log'.format(start_server),
                             format='%(asctime)s %(levelname)s %(message)s',
                             filemode='w',
                             level=logging.INFO)
         server = await asyncio.start_server(handle_connection, host=localhost, port=server_port[start_server])
         logging.info("Starting server {}".format(start_server))
         await server.serve_forever()
         logging.info("Shutting down server {}".format(start_server))

if __name__ == '__main__':
         asyncio.run(main())
