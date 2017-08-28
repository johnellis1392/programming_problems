from flask import Flask

host = '127.0.0.1'
port = 3000

app = Flask(__name__)

@app.route('/')
def hello():
    return 'Hello, World!\n'

if __name__ == '__main__':
    app.run(host = host, port = port)
