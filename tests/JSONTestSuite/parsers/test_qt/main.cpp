#include <QCoreApplication>

#include <QFile>
#include <QJsonDocument>

int main(int argc, char *argv[]) {
    if (argc < 1) {
        return 1;
    }

    QFile file(argv[1]);
    if (! file.open(QIODevice::ReadOnly)) {
        return 1;
    }

    auto bytes = file.readAll();
    QJsonParseError parseError;
    QJsonDocument::fromJson(bytes, &parseError);

    if (parseError.error != QJsonParseError::NoError) {
        return 1;
    }

    return 0;
}
