import {createEntityQueryEndpoints} from './createEntityQueryEndpoints';
import {createEventQueryEndpoints} from './createEventQueryEndpoints';
import {createCustomQueryEndpoints} from './customSubgraphQuery';
import {SubgraphSliceEndpointBuilder} from './subgraphSlice';

export const subgraphSliceAllEndpoints = {
    endpoints: (builder: SubgraphSliceEndpointBuilder) => ({
        ...createCustomQueryEndpoints(builder),
        ...createEntityQueryEndpoints(builder),
        ...createEventQueryEndpoints(builder),
    }),
};

export default subgraphSliceAllEndpoints;