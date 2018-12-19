# Author: Dainel Steinberg
# edited by Jingting Li
import utils; utils.init()

import argparse
import json
import os
import sys

from cStringIO import StringIO

from keras.layers import (
    Conv2D,
    Input,
    MaxPooling2D,
    UpSampling2D
)
from keras.models import Model

from init import INIT_ARGS_FILENAME

from utils import (
    COMBINE_METHODS,
    load_mnist,
    makedirs,
    redirect_stdout,
    separate_pair_generator,
)

TRAIN_SEPARATE_MODEL_DIRNAME = 'train_separate_models'
TRAIN_SEPARATE_KERAS_MODEL_FILENAME_PREFIX = 'keras_separate_model'

TRAIN_SEPARATE_MODEL_SUMMARY_FILENAME = 'train_separate_model_summary.txt'
TRAIN_SEPARATE_ARGS_FILENAME = 'train_separate_args.json'
TRAIN_SEPARATE_HISTORY_TABLE_FILENAME = 'train_separate_history_table.csv'
TRAIN_SEPARATE_HISTORY_PLOT_FILENAME = 'train_separate_history_plot.pdf'
TRAIN_SEPARATE_HISTORY_TABLE_BY_PAIR_DIRNAME = 'train_separate_history_table_by_pair'
TRAIN_SEPARATE_HISTORY_PLOT_BY_PAIR_DIRNAME = 'train_separate_history_plot_by_pair'

DIM_ORDERING = 'channels_first'
INPUT_SHAPE = (1, 28, 28)
OUTPUT_DIM = 10


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument('--batch-size', default=100)
    parser.add_argument('--steps-per-epoch', default=450)
    parser.add_argument('--combine-method',
                        choices=COMBINE_METHODS,
                        default='max')
    parser.add_argument('--epochs', default=200, type=int)
    parser.add_argument('--digits', default=range(OUTPUT_DIM), nargs='+', type=list)
    parser.add_argument('workspace')

    args = parser.parse_args(argv[1:])

    init_args_path = os.path.join(args.workspace, INIT_ARGS_FILENAME)
    with open(init_args_path, 'r') as f:
        init_args = json.load(f)

    # ******************************************
    # * Save training args
    # ******************************************

    train_args_path = os.path.join(args.workspace, TRAIN_SEPARATE_ARGS_FILENAME)
    with open(train_args_path, 'w') as f:
        json.dump(vars(args), f, indent=2)

    (X_train_mnist, T_train_mnist), _ = load_mnist()

    batch_size = args.batch_size
    steps_per_epoch = args.steps_per_epoch
    epochs = args.epochs

    # ******************************************
    # * Model Specification
    # ******************************************

    input = x = Input(shape=INPUT_SHAPE)

    x = Conv2D(32,
               kernel_size=(3, 3),
               activation='relu',
               padding='same',
               data_format=DIM_ORDERING,
               input_shape=INPUT_SHAPE)(x)
    x = MaxPooling2D((2, 2), padding='same', data_format=DIM_ORDERING)(x)

    x = Conv2D(64,
               kernel_size=(3, 3),
               activation='relu',
               padding='same',
               data_format=DIM_ORDERING)(x)
    x = MaxPooling2D((2, 2), padding='same', data_format=DIM_ORDERING)(x)

    x = Conv2D(64,
               kernel_size=(3, 3),
               activation='relu',
               padding='same',
               data_format=DIM_ORDERING)(x)
    x = UpSampling2D((2, 2), data_format=DIM_ORDERING)(x)

    x = Conv2D(32,
               kernel_size=(3, 3),
               activation='relu',
               padding='same',
               data_format=DIM_ORDERING)(x)
    x = UpSampling2D((2, 2), data_format=DIM_ORDERING)(x)

    x = Conv2D(1,
               kernel_size=(3, 3),
               activation='sigmoid',
               padding='same',
               data_format=DIM_ORDERING)(x)

    for digit in args.digits:
        print 'Digit: {}'.format(digit)

        train_generator = separate_pair_generator(
            X_train_mnist,
            T_train_mnist,
            digit,
            seed=0,
            batch_size=batch_size,
            combine_method=init_args['combine_method'])

        model = Model(inputs=input, outputs=x)

        summary_str_io = StringIO()
        with redirect_stdout(summary_str_io):
            model.summary()
        summary_str = summary_str_io.getvalue()
        print summary_str
        model_summary_path = os.path.join(
            args.workspace, TRAIN_SEPARATE_MODEL_SUMMARY_FILENAME)
        with open(model_summary_path, 'w') as f:
            f.write(summary_str)

        model.compile(optimizer='adam',
                      loss='binary_crossentropy')
        model.fit_generator(train_generator,
                            steps_per_epoch,
                            epochs=epochs)

        model_dir_path = os.path.join(args.workspace, TRAIN_SEPARATE_MODEL_DIRNAME)
        makedirs(model_dir_path)
        model_filename = '{}_{}.h5'.format(
            TRAIN_SEPARATE_KERAS_MODEL_FILENAME_PREFIX, digit)
        model_path = os.path.join(model_dir_path, model_filename)
        model.save(model_path)

    print 'Done!'

    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))
