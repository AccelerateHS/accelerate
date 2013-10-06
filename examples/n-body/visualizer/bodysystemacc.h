/*
 * Copyright 1993-2012 NVIDIA Corporation.  All rights reserved.
 *
 * Please refer to the NVIDIA end user license agreement (EULA) associated
 * with this source code for terms and conditions that govern your use of
 * this software. Any use, reproduction, disclosure, or distribution of
 * this software and related documentation outside the terms of the EULA
 * is strictly prohibited.
 *
 */

#ifndef __BODYSYSTEMACC_H__
#define __BODYSYSTEMACC_H__

#include <cuda.h>
#include "bodysystem.h"
#include "bodysystemcuda.h"
#include "AccFFI.h"
#include "Visualize_stub.h"

// Accelerate BodySystem: runs on the GPU via the Accelerate library.
class BodySystemAcc : public BodySystem<float>
{
    public:
        BodySystemAcc(unsigned int numBodies, unsigned int p, unsigned int q, bool usePBO, bool useSysMem = false);
        virtual ~BodySystemAcc();

        virtual void loadTipsyFile(const std::string &filename);

        virtual void update(float deltaTime);

        virtual void setSoftening(float softening);
        virtual void setDamping(float damping);

        virtual float *getArray(BodyArray array);
        virtual void   setArray(BodyArray array, const float *data);

        virtual unsigned int getCurrentReadBuffer() const
        {
            return m_pbo;
        }

        virtual unsigned int getNumBodies() const
        {
            return m_numBodies;
        }

    protected: // methods
        BodySystemAcc() {}

        virtual void _initialize(int numBodies);
        virtual void _finalize();

    protected: // data
        unsigned int m_numBodies;
        bool m_bInitialized;

        // Host data
        float *m_hPos[2];
        float *m_hVel;

        DeviceData<float> m_deviceData;

        CUcontext m_ctx;

        AccHandle m_hndl;
        AccProgram m_program;

        OutputArray m_prevResult;

        float* m_timestep;

        bool m_bUsePBO;
        bool m_bUseSysMem;
        unsigned int m_SMVersion;

        float m_damping;
        float* m_softening;

        unsigned int m_pbo;
        cudaGraphicsResource *m_pGRes;
        unsigned int m_currentRead;
        unsigned int m_currentWrite;

        unsigned int m_p;
        unsigned int m_q;
};

#include "bodysystemacc_impl.h"

#endif // __BODYSYSTEMACC_H__
