FROM mcr.microsoft.com/devcontainers/javascript-node AS base

ENV LANG=C.UTF-8

ARG USERNAME=vscode
ARG STACK_VERSION=recommended
ARG HLS_VERSION=recommended

ENV DEBIAN_FRONTEND=noninteractive \
		USER_GID=1000 \
		USER_UID=1001 \
		USERNAME=${USERNAME} \
    STACK_VERSION=${STACK_VERSION} \
    HLS_VERSION=${HLS_VERSION}

FROM base AS haskell

RUN useradd -ms /bin/bash -K MAIL_DIR=/dev/null --uid ${USER_UID} --gid ${USER_GID} -m ${USERNAME}

USER ${USERNAME}

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
BOOTSTRAP_HASKELL_MINIMAL=1 \
BOOTSTRAP_HASKELL_NO_UPGRADE=1

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/home/${USERNAME}/.local/bin:/home/${USERNAME}/.ghcup/bin:$PATH"
RUN echo "export PATH=${PATH}" >> /home/${USERNAME}/.profile

RUN ghcup install stack ${STACK_VERSION} --set
RUN ghcup install hls ${HLS_VERSION} --set

ENV DEBIAN_FRONTEND=dialog
