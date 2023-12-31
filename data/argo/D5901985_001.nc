CDF   !   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2011-06-14T13:08:33Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:53:39Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;l   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ?�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @h   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  F�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  I   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    M   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    S   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    Y   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  _   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    _X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    _\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    _`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    _d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  _h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    _�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    _�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         _�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         _�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        _�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20110614130833  20161129232523  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  P7_97922_001                    2C  D   PROVOR                          09027                           5815A03                         841 @�긝��1   @��u� @1e�����d8�/��1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >���@�33A33A^ffA���A�  A���B33B��B0  BG33B]33Bn  B���B�ffB�  B�ffB�33B���B�33B�ffB�ffB�ffB癚B���B�33C33C��C�fC��C�3C��C   C&L�C*��C/�fC4�fC9� C>L�CC�CH  CR� C\L�Ce��CpffCz�C�@ C�L�C�  C�@ C��fC��3C���C��3C��3C�&fC��3C��C�Y�C�@ C�&fC�33C�@ C�L�C�@ C�ٚC��C�� C��C��3C��fD3D� DٚD  D��D�3D��D$�fD*�D/9�D3�3D9fD>3DB��DG� DL��DR�DVٚD[�3Da33Df�Dk33Dp&fDu33Dz&fD�@ D���D��fD��fD�Y�D�|�D��fD���D�33D�s3D���D�fD�L�D�p Dڳ3D��3D�VfD�3D��D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111>���@�33A33A^ffA���A�  A���B33B��B0  BG33B]33Bn  B���B�ffB�  B�ffB�33B���B�33B�ffB�ffB�ffB癚B���B�33C33C��C�fC��C�3C��C   C&L�C*��C/�fC4�fC9� C>L�CC�CH  CR� C\L�Ce��CpffCz�C�@ C�L�C�  C�@ C��fC��3C���C��3C��3C�&fC��3C��C�Y�C�@ C�&fC�33C�@ C�L�C�@ C�ٚC��C�� C��C��3C��fD3D� DٚD  D��D�3D��D$�fD*�D/9�D3�3D9fD>3DB��DG� DL��DR�DVٚD[�3Da33Df�Dk33Dp&fDu33Dz&fD�@ D���D��fD��fD�Y�D�|�D��fD���D�33D�s3D���D�fD�L�D�p Dڳ3D��3D�VfD�3D��D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��`A��A�ȴAѼjAѸRAѸRAџ�Aљ�Aћ�Aћ�Aљ�AѓuA�I�A�ȴA���A��Aˏ\A�I�A���A�M�A��A�"�A�VAǋDAƉ7AļjA�z�A���A�9XA��A��A�$�A��+A�hsA�;dA��HA��FA�p�A�-A�ffA�XA�7LA�|�A�
=A���A��Av�yAj5?AZffAM��AD�DA:��A5`BA*��A"ffA|�A=qA��A�A	��A�\A �!@��R@���@��D@�@�=q@�5?@�ȴ@�-@�V@��@ա�@�ff@͉7@ˍP@�  @�9X@��@�x�@�@�I�@��D@���@�=q@�E�@�C�@�1@���@��w@��9@�C�@��j@�M�@��F@�/@��@��y@|�@rn�@kdZ@b�@Z��@T�@JM�@B��@;�@4�@,�/@'�w@#�
@�@S�@v�@�@��22111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��`A��A�ȴAѼjAѸRAѸRAџ�Aљ�Aћ�Aћ�Aљ�AѓuA�I�A�ȴA���A��Aˏ\A�I�A���A�M�A��A�"�A�VAǋDAƉ7AļjA�z�A���A�9XA��A��A�$�A��+A�hsA�;dA��HA��FA�p�A�-A�ffA�XA�7LA�|�A�
=A���A��Av�yAj5?AZffAM��AD�DA:��A5`BA*��A"ffA|�A=qA��A�A	��A�\A �!@��R@���@��D@�@�=q@�5?@�ȴ@�-@�V@��@ա�@�ff@͉7@ˍP@�  @�9X@��@�x�@�@�I�@��D@���@�=q@�E�@�C�@�1@���@��w@��9@�C�@��j@�M�@��F@�/@��@��y@|�@rn�@kdZ@b�@Z��@T�@JM�@B��@;�@4�@,�/@'�w@#�
@�@S�@v�@�@��22111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�%B
�JB
�\B
�PB
�PB
�PB
�JB
�PB
�JB
�JB
�JB
�JB
��B$�B^5B�7B�qB�BbB;dB]/Bm�B�%B�hB�LB��B��B�5B�`B�BB33B�B@�B��B��B�?B{BC�B8RBXBD�B��B�/B�B
��B
cTB	��B	�VB	9XB	uB�B�
BB��B��B��B�VB�7B�B}�B�B�B�7B��B�RB��B�/B�BB�B��B��B	�B	33B	H�B	k�B	�7B	��B	��B	�9B	�XB	ĜB	��B	��B	�B	�#B	�HB	�fB	�B	�B	�B	��B
B
%B

=B
\B
uB
�B
#�B
(�B
/B
5?B
9XB
@�B
A�B
I�B
M�B
T�B
W
B
^5B
bNB
e`B
iyB
m�B
r�B
v�B
v�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�B
�vB
�PB
�PB
�PB
�JB
�PB
�JB
�JB
�JB
�JB
��B$�B^5B�7B�qB�BbB;dB]/Bm�B�%B�hB�LB��B��B�5B�`B�BB33B�B@�B��B��B�?B{BC�B8RBXBD�B��B�/B�B
��B
cTB	�B	�pB	9rB	�B��B�$BªB��B��B��B�pB�RB�9B}�B�-B�9B�7B��B�lB��B�IB�BB��B��B��B	�B	3MB	H�B	k�B	�7B	��B	��B	�9B	�XB	ĜB	��B	��B	�B	�#B	�HB	�fB	�B	�B	�B	�B
B
%B

XB
\B
uB
�B
#�B
)B
/B
5?B
9rB
@�B
A�B
I�B
M�B
T�B
W
B
^5B
bhB
ezB
iyB
m�B
r�B
v�B
v�44111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201307281649522013072816495220130728164952201608161346552016081613465520160816134655JA  ARFMdecpP7_b                                                                20110614130830  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20110614130833  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20110614130834  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20110614130839  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20110614130839  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20110614130839  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20110614130839  CF  PSAL            >���@�33?�                  JA  ARGQpump1.0                                                                 20110614130839  CF  TEMP            >���@�33?�                  JA  ARUP                                                                        20110614132207                      G�O�G�O�G�O�                JA  ARFMdecpP7_b                                                                20110616220707  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20110616221305  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20110616221306  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20110616221311  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20110616221311  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20110616221312  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20110616221312  CF  PSAL            >���@�33?�                  JA  ARGQpump1.0                                                                 20110616221312  CF  TEMP            >���@�33?�                  JA  ARUP                                                                        20110616222301                      G�O�G�O�G�O�                JA  ARFMdecpP7_e                                                                20111021064659  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20111021064819  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20111021064820  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20111021064825  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20111021064825  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20111021064825  QCP$                G�O�G�O�G�O�           10000JA  ARGQpump1.0                                                                 20111021064825  CF  PSAL            >���@�33?�                  JA  ARGQpump1.0                                                                 20111021064825  CF  TEMP            >���@�33?�                  JA  ARUP                                                                        20111021065123                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503002512                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728074952  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20130728074952  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816044655  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129232523                      G�O�G�O�G�O�                