Burroughs B5500 Utilities: Richard Cornwell


reader.card
    A compile deck for the OBJECT/READER utility program. This was
    reconstructed by inference from scanned PDF listings at
    http://bitsavers.org/pdf/burroughs/B5000_5500_5700/listing/.
    The program can be used to print a deck of cards, print a file from
    disk or tape, or copy a deck of cards to disk or tape. Its behavior
    is controlled by COMMON and file-equation control cards. See the
    comment in the source for details.

    The following example will copy a deck of cards to a disk file named
    CARDS/ONDISK, using the standard blocking for B5500 source files
    (10-word records, 150-word blocks). The file will also be printed:

        ?RUN OBJECT/READER; COMMON = 3
        ?FILE NEWTAPE = CARDS/ONDISK DISK SERIAL
        ?FILE LINE = LINE PRINT BACKUP DISK
        ?DATA CARD
            :
            : <deck of cards>
            :
        ?END

    This deck will compile the program to disk as OBJECT/READER, store
    its source as SYMBOL/READER on disk, and create a compile listing.

