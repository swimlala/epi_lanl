CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   G   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-20T16:46:56Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  ;   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       ;T   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  <p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       <�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       =�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  >�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ?8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  @T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       @�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       A�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  B�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       C   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  D8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       D�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  E�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    E�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    H�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    K�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  N�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    N�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    N�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    O    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    O   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  O   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    OH   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    OX   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    O\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         Ol   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         Op   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        Ot   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    OxArgo profile    3.1 1.2 19500101000000  20181120164656  20191218160210  5900947 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL              �A   AO  1287                            2C  D   APEX                            1910                            060602                          846 @׶�0*�"1   @׶���GR@2=�-V�c�&�x��1   ARGOS   Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   B   @���A33A�33A���B33BE��BlffB�  B�  B�33B�  B�33B�  C33CL�C��CffC)  C3� C=��CG��CQL�C[�Ce33Cn��Cy��C��3C��3C���C���C�� C��fC���C��3C�� C��fC��3C��fCǌ�C���C�� D	@ D�3D"S3D.ٚD;L�DG� DT33D`��DmS3Dy��D�#3D�p D��fD���D�&fD�\�D�� D��fD�&fD�ffD�� D��3D�,�D�i�Dک�D���D�&fD�Y�D�D���11111111111111111111111111111111111111111111111111111111111111111111111 @���A��A�  AᙚB��BB  Bh��B�33B�33B�ffB�33B�ffB�33C L�C
fgC�4C� C(�C2��C<�4CF�4CPfgCZ34CdL�Cm�gCx�4C�@ C�@ C��C��C�L�C�33C�Y�C�@ C�L�C�33C�@ C�33C��C��C��D	fD��D"�D.� D;3DG�fDS��D`� Dm�Dy�3D�fD�S3D���D�� D�	�D�@ D��3D�əD�	�D�I�D��3D��fD� D�L�Dڌ�D�� D�	�D�<�D�|�D�� 11111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�7LA�G�A��AӮAӥ�Aӣ�Aӝ�A�;dA�dZA�=qA�%AǮA��A�XA�O�A���A���A��uA�\)A�A�n�A�G�A���A���A~z�AlbNAZ�!AJ�yA?O�A9hsA0�A*�uA(I�A"bAI�A�AJAr�A\)@��#@�dZ@ҏ\@��/@��7@�-@��@�K�@��/@��@��^@�/@��@~@rM�@j�!@`Ĝ@WK�@P  @J�\@Ct�@<(�@6$�@1�7@*~�@'|�@"J@�@�@�D@��@�m11111111111111111111111111111111111111111111111111111111111111111111111 A�7LA�G�A��AӮAӥ�Aӣ�Aӝ�A�;dA�dZA�=qA�%AǮA��A�XA�O�A���A���A��uA�\)A�A�n�A�G�A���A���A~z�AlbNAZ�!AJ�yA?O�A9hsA0�A*�uA(I�A"bAI�A�AJAr�A\)@��#@�dZ@ҏ\@��/@��7@�-@��@�K�@��/@��@��^@�/@��@~@rM�@j�!@`Ĝ@WK�@P  @J�\@Ct�@<(�@6$�@1�7@*~�@'|�@"J@�@�@�D@��@�m11111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�7B
cTB
�B%�Bs�B�;Bz�BffBq�BH�B)�B0!B�%BuBM�B
��B
�B
P�B	��B	cTB	�B��B�B��B��B��B�B�sB��B��B��B��B��B	P�B	��B	��B	�-B	��B	�BB
B
�B
!�B
33B
;dB
I�B
Q�B
ZB
_;B
e`B
n�B
s�B
y�B
|�B
�B
�=B
�JB
�oB
��B
��B
��B
��B
��B
�B
�-11111111411111111111111111111111111111111111111111111111111111111111111 B
EB
EB
EB
EB
F'B
F#B
F&B
DG�O�B
g�B
��B.*B�~B4�B lB+�B�B�4B�]B@�B
�aB�B
��B
hLB
_B	��B	hB��B�EB��B�7B�\B�FB�B��B��B��B��B��B�nB	pB	`;B	f`B	n�B	�2B	��B	�lB	��B	�B	�rB	��B
�B
"B
SB
tB
!�B
*�B
/�B
6B
9 B
?FB
FnB
HvB
N�B
R�B
W�B
]�B
bB
e!B
i3B
nS11111111411111111111111111111111111111111111111111111111111111111111111 =�P=�P=�P=�P=�P=�P=�P=�PG�O�=�P=�P=�P=�P=�P=�P=�P=�P=�P=�P=�P=�P=�P=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�=t�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.9 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9983(+/-0.0009), vertically averaged dS =-0.067(+/-0.036) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201912181602102019121816021020191218160210  AO  ARCAADJP                                                                    20181120164656    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20181120164656    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181120164656  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181120164656  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20191218160210  IP                  G�O�G�O�G�O�                