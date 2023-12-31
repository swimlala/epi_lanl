CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:30Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191730  20181005191730  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$���1   @��%O��@6[dZ��d��+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @���A   A   A@  A`  A�  A�  A���A�  A�  A���A���A�  B   B  B  B  B   B(  B0  B8  B@  BHffBPffBX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C/�fC1�fC4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd�Cf  Ch  Cj  Cl  Cn  Co�fCr  Cs�fCv  Cx�Cz  C|  C~  C�  C�  C��3C�  C��C��C��C�  C��3C��3C��3C�  C��C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��C��C�  C��C��C��C�  C�  C�  C��3C�  C��C�  C��C��C�  C�  C��C��C�  C��3C��3C��C��C��C��C��C�  C��3C��C��C�  C�  C�  C��C��3C�  C��C��3C��C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��3C��3C�  C��3C�  C�  C�  C�  C��C�  C��C��3C��C��C�  C��C��3C��3C��3C�  C��3C�  C�  C�  C�  C�  C��3C��C��C��3C��C��C��3C��3C�  C�  C��3C�  C�  C�  C��C�  D   D � D  D� DfD� DfD� D  D�fD��D�fD��D� D��D� D  D� D��D	� D
fD
�fDfD� D  D�fD  D�fD  Dy�D  D�fDfD�fD  D� DfD�fD�D� D  D��D  D� D  Ds3D��D� D  D� DfD�fD  D� D��Dy�D  D� D  D� D  D� D  D� D   D �fD!  D!� D"fD"�fD#fD#�fD$  D$y�D%  D%� D%��D&� D'  D'�fD(  D(y�D)  D)� D*fD*� D+fD+�fD,  D,� D-fD-� D.  D.�fD/fD/� D0  D0� D0�3D1y�D2  D2�fD3  D3� D4fD4� D5  D5� D5��D6� D6��D7� D8fD8� D9fD9�fD:  D:� D;fD;�fD<fD<� D<��D=y�D>fD>� D>��D?� D@  D@� DAfDA� DB  DB� DC  DC� DDfDD�fDE  DE� DFfDF� DGfDG� DG��DH� DI  DI� DJ  DJ� DK  DK�fDLfDL�fDM�DM�fDN  DN� DOfDO� DPfDP�fDQ�DQ��DR�DR�fDS  DS� DT  DT� DU  DU� DVfDV�fDW  DW� DX  DXy�DY  DYy�DZ  DZ�fD[  D[� D\fD\� D]  D]� D]��D^� D_  D_� D_��D`y�Da  Da�fDb  Dby�Dc  Dc� Dd  Dd�fDefDey�De��Df�fDgfDg� Dg��Dh�fDi  Diy�DjfDj� Dj��Dk� DlfDl� DmfDm�fDn  Dn� Do  Dos3Do��Dpy�Dq  Dq�fDrfDry�Ds  Ds�fDtfDt�fDufDu�fDv�Dv� Dw  Dw� Dw��Dy��D�7�D��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @E�@�@\A�HA"�HAB�HAb�HA�p�A�p�A�=qA�p�A�p�A�=qA�=qA�p�B �RB�RB�RB�RB �RB(�RB0�RB8�RB@�RBI�BQ�BX�RB`�RBh�RBp�RBy�B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�(�B�(�B�(�B�(�B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
G�C.C.C.C.C.C.C.C.C.CzC .C".C$.C&.C(.C*.C,.C..C0zC2zC4.C6.C8.C:.C<zC>.C@.CB.CD.CF.CH.CJ.CL.CN.CP.CR.CT.CV.CX.CZ.C\.C^.C`.CbG�CdG�Cf.Ch.Cj.Cl.Cn.CpzCr.CtzCv.CxG�Cz.C|.C~.C�
C�
C�
=C�
C�#�C�#�C�#�C�
C�
=C�
=C�
=C�
C�#�C�
C�
C�#�C�
C�
C�
C�
C�
C�
=C�
C�
C�
C�#�C�
C�
=C�
C�
C�
C�
C�#�C�#�C�
C�#�C�#�C�#�C�
C�
C�
C�
=C�
C�#�C�
C�#�C�#�C�
C�
C�#�C�#�C�
C�
=C�
=C�#�C�#�C�#�C�#�C�#�C�
C�
=C�#�C�#�C�
C�
C�
C�#�C�
=C�
C�#�C�
=C�#�C�#�C�#�C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
C�
C�#�C�
=C�
=C�
C�
=C�
C�
C�
C�
C�#�C�
C�#�C�
=C�#�C�#�C�
C�#�C�
=C�
=C�
=C�
C�
=C�
C�
C�
C�
C�
C�
=C�#�C�#�C�
=C�#�C�#�C�
=C�
=C�
C�
C�
=C�
C�
C�
C�#�C�
D �D ��D�D��D�D��D�D��D�D��DD��DD��DD��D�D��D	D	��D
�D
��D�D��D�D��D�D��D�D�D�D��D�D��D�D��D�D��DRD��D�D�RD�D��D�D~�DD��D�D��D�D��D�D��DD�D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$�D%�D%��D&D&��D'�D'��D(�D(�D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D0��D1�D2�D2��D3�D3��D4�D4��D5�D5��D6D6��D7D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=D=�D>�D>��D?D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DHDH��DI�DI��DJ�DJ��DK�DK��DL�DL��DMRDM��DN�DN��DO�DO��DP�DP��DQRDQ�RDRRDR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX�DY�DY�DZ�DZ��D[�D[��D\�D\��D]�D]��D^D^��D_�D_��D`D`�Da�Da��Db�Db�Dc�Dc��Dd�Dd��De�De�DfDf��Dg�Dg��DhDh��Di�Di�Dj�Dj��DkDk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do~�DpDp�Dq�Dq��Dr�Dr�Ds�Ds��Dt�Dt��Du�Du��DvRDv��Dw�Dw��Dw�RDy�)D�=qD��>1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/A�(�A�&�A�+A�(�A�(�A�(�A�1'A�7LA�9XA�=qA�=qA�=qA�=qA�?}A�?}A�?}A�7LA�&�A�+A��A�oA��A�{A�{A�bA�VA�  A��A��A��yA��mA��TA��HA��/A��
A���A�ȴA�AμjAθRAκ^AθRAβ-Aΰ!AΧ�AΣ�AΣ�AΛ�A΍PA·+AΉ7AΏ\AΏ\A�l�A��A�~�AˍPAʅA�t�A�Q�A���Aƙ�A�G�A�/A��A�ĜA�n�A��hA�9XA���A�jA��-A�`BA��+A���A��\A���A��jA�9XA�A��PA�33A�/A�ĜA��hA��A�&�A�ȴA�A�E�A�A�"�A�^5A���A�VA��/A�XA�oA��A�hsA��uA��A�r�A���A��uA���A�x�A�
=A��A�1A�M�A��A�l�A�"�A�r�A�dZA��A�VA�&�A��A�M�A��A���A��A�dZA���A��hA��HA�t�A�K�A�^A~��A}��A{�TAzbAx�Av5?Ar�\Ap$�An��Amt�Ak\)AihsAhbNAg�wAghsAf�DAeG�Ac��Aa�TA`�yA_�A^ĜA^��A^ZA]S�A[hsAYp�AX  AV��AU�TAT{AR$�AQ33AP��AP=qAO�AO`BANAM%AJ��AG�7AEADA�AB�A@�!A>��A=
=A:��A8�+A8-A6�\A4=qA3S�A1��A/+A.z�A,��A+��A+�A)�
A(n�A'��A%��A%7LA#A#;dA"�!A"r�A"I�A"�A!33A�hA33A��AA�A��Av�A��AG�A
=A��AhsA�AVA��AVA�7A��A�AƨA��Al�A�mAl�Az�AbNA1'A
�A
A	��AA��A;dA�A�AA�A��AA�A|�A~�A��@��F@���@��\@�ff@�@���@���@��#@�x�@�p�@���@�5?@�ff@�33@��!@�-@�9X@�V@���@���@��@�@��`@���@��/@�  @�ƨ@�;d@ꗍ@�Ĝ@��#@�Z@�7L@���@�ff@�J@�O�@�&�@ؼj@���@ם�@�|�@�\)@֧�@���@�hs@�Z@�J@�?}@���@��;@��@�V@ˍP@�K�@��H@��T@���@Ɂ@�I�@�ff@ģ�@�b@��
@�I�@�ȴ@��@�z�@�=q@�O�@���@���@�ff@��7@�j@�1@��P@��!@�=q@��@��^@��`@���@�O�@� �@��@��h@��/@���@�j@�9X@��w@��y@�p�@��@�hs@�Ĝ@�9X@���@��y@���@���@��!@�ff@�@�bN@���@�C�@���@��@�M�@�V@���@���@�Ĝ@��F@��w@�z�@�z�@�z�@���@�C�@�
=@�v�@��h@��@��u@��@�~�@��@���@���@��@�/@�`B@�?}@�7L@��`@�1'@��@��
@��w@���@��@��P@���@���@��@�t�@�\)@��\@�^5@��#@�p�@�?}@�/@�&�@��D@��u@�A�@���@���@���@�l�@��@���@���@���@�M�@�J@���@��7@�p�@�hs@�`B@�`B@�?}@��@���@���@�9X@���@��m@�\)@�-@�@���@�p�@�&�@�Ĝ@�Z@��@���@��F@��P@�dZ@�@�ȴ@��@�dZ@�A�@���@���@�"�@��@�ȴ@���@�ff@��@���@��^@��@�G�@��@��u@�z�@�z�@�j@�1'@���@��w@���@�S�@�+@�@��@���@���@�^5@�x�@���@���@��9@�r�@� �@��m@��w@��@�;d@��!@�5?@���@���@��#@��7@�%@���@���@��@���@���@�Ĝ@��b@��@o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�/A�(�A�&�A�+A�(�A�(�A�(�A�1'A�7LA�9XA�=qA�=qA�=qA�=qA�?}A�?}A�?}A�7LA�&�A�+A��A�oA��A�{A�{A�bA�VA�  A��A��A��yA��mA��TA��HA��/A��
A���A�ȴA�AμjAθRAκ^AθRAβ-Aΰ!AΧ�AΣ�AΣ�AΛ�A΍PA·+AΉ7AΏ\AΏ\A�l�A��A�~�AˍPAʅA�t�A�Q�A���Aƙ�A�G�A�/A��A�ĜA�n�A��hA�9XA���A�jA��-A�`BA��+A���A��\A���A��jA�9XA�A��PA�33A�/A�ĜA��hA��A�&�A�ȴA�A�E�A�A�"�A�^5A���A�VA��/A�XA�oA��A�hsA��uA��A�r�A���A��uA���A�x�A�
=A��A�1A�M�A��A�l�A�"�A�r�A�dZA��A�VA�&�A��A�M�A��A���A��A�dZA���A��hA��HA�t�A�K�A�^A~��A}��A{�TAzbAx�Av5?Ar�\Ap$�An��Amt�Ak\)AihsAhbNAg�wAghsAf�DAeG�Ac��Aa�TA`�yA_�A^ĜA^��A^ZA]S�A[hsAYp�AX  AV��AU�TAT{AR$�AQ33AP��AP=qAO�AO`BANAM%AJ��AG�7AEADA�AB�A@�!A>��A=
=A:��A8�+A8-A6�\A4=qA3S�A1��A/+A.z�A,��A+��A+�A)�
A(n�A'��A%��A%7LA#A#;dA"�!A"r�A"I�A"�A!33A�hA33A��AA�A��Av�A��AG�A
=A��AhsA�AVA��AVA�7A��A�AƨA��Al�A�mAl�Az�AbNA1'A
�A
A	��AA��A;dA�A�AA�A��AA�A|�A~�A��@��F@���@��\@�ff@�@���@���@��#@�x�@�p�@���@�5?@�ff@�33@��!@�-@�9X@�V@���@���@��@�@��`@���@��/@�  @�ƨ@�;d@ꗍ@�Ĝ@��#@�Z@�7L@���@�ff@�J@�O�@�&�@ؼj@���@ם�@�|�@�\)@֧�@���@�hs@�Z@�J@�?}@���@��;@��@�V@ˍP@�K�@��H@��T@���@Ɂ@�I�@�ff@ģ�@�b@��
@�I�@�ȴ@��@�z�@�=q@�O�@���@���@�ff@��7@�j@�1@��P@��!@�=q@��@��^@��`@���@�O�@� �@��@��h@��/@���@�j@�9X@��w@��y@�p�@��@�hs@�Ĝ@�9X@���@��y@���@���@��!@�ff@�@�bN@���@�C�@���@��@�M�@�V@���@���@�Ĝ@��F@��w@�z�@�z�@�z�@���@�C�@�
=@�v�@��h@��@��u@��@�~�@��@���@���@��@�/@�`B@�?}@�7L@��`@�1'@��@��
@��w@���@��@��P@���@���@��@�t�@�\)@��\@�^5@��#@�p�@�?}@�/@�&�@��D@��u@�A�@���@���@���@�l�@��@���@���@���@�M�@�J@���@��7@�p�@�hs@�`B@�`B@�?}@��@���@���@�9X@���@��m@�\)@�-@�@���@�p�@�&�@�Ĝ@�Z@��@���@��F@��P@�dZ@�@�ȴ@��@�dZ@�A�@���@���@�"�@��@�ȴ@���@�ff@��@���@��^@��@�G�@��@��u@�z�@�z�@�j@�1'@���@��w@���@�S�@�+@�@��@���@���@�^5@�x�@���@���@��9@�r�@� �@��m@��w@��@�;d@��!@�5?@���@���@��#@��7@�%@���@���@��@���@���@�Ĝ@��b@��@o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�fB�mB�mB�mB�mB�fB�fB�fB�`B�fB�fB�fB�fB�fB�fB�mB�mB�mB�B�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B
=BoB&�B-B;dBJ�BW
BXBR�BYB_;BiyBt�B� B�7B�1B�1B�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�7B�Bo�B_;BS�BJ�B7LB!�BoBDB1BB��B�B�HB�B��BǮB�LB��B��B�{B�Bl�BYBA�B(�B�B%B
�B
�;B
�5B
�#B
��B
��B
�B
�B
��B
��B
��B
�oB
�JB
�=B
�B
~�B
v�B
hsB
[#B
L�B
>wB
&�B
�B
PB
B	��B	�yB	�NB	�/B	�B	��B	ɺB	�wB	�-B	��B	��B	��B	��B	��B	�VB	�B	y�B	o�B	gmB	aHB	VB	J�B	G�B	H�B	I�B	I�B	I�B	F�B	A�B	7LB	'�B	�B	�B	bB	%B��B��B�B�NB�;B�B��B��BǮBB�}B�jB�RB�FB�3B�B�B��B��B��B��B��B��B��B��B��B�oB�hB�VB�PB�DB�+B�B�B�B� B|�By�Bw�Bs�Bp�Bm�Bk�BiyBhsBgmBe`BbNB\)BYBYBW
BS�BP�BN�BO�BQ�BO�BO�BO�BO�BO�BN�BN�BK�BH�BH�BC�BC�BD�BE�BF�BD�BA�B@�B@�BB�BC�BK�BQ�B\)B[#BZBVBk�By�Bw�Bu�Bn�BiyBgmBgmBdZBe`BiyBjBjBe`BaHB[#BN�BH�BK�BO�BP�BP�BVBXBZB\)B]/B^5B]/B_;B`BB^5B_;B`BBbNBe`BffBffBe`Bk�Bn�Bm�BjBp�Bu�Bw�B{�B�B�+B�%B�%B�B�B�B�B�B�B�B� B� B~�B~�B~�B~�B~�B� B� B�B�%B�%B�1B�7B�=B�DB�PB�VB�bB�oB�oB�uB�oB�oB��B��B��B��B��B�?BĜBBĜB��B��B��B�)B�NB�B�B�B�B��B��B��B��B��B��B	B	B		7B	DB	VB	bB	oB	�B	�B	�B	!�B	�B	 �B	 �B	"�B	(�B	-B	/B	33B	6FB	9XB	=qB	@�B	C�B	I�B	O�B	VB	W
B	VB	VB	T�B	T�B	XB	ZB	ZB	[#B	^5B	_;B	cTB	ffB	hsB	hsB	hsB	hsB	hsB	hsB	k�B	l�B	q�B	r�B	s�B	s�B	u�B	w�B	y�B	|�B	~�B	�B	�B	�B	�+B	�+B	�1B	�7B	�DB	�VB	�VB	�VB	�bB	�hB	�uB	�uB	�{B	��B	��B	��B	��B	�B	�LB	�}B	ĜB	ŢB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�)B	�)B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�;B	�TB	�NB	�NB	�NB	�ZB	�`B	�fB	�yB	�B	�]B
�B
12222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B�fB�mB�mB�mB�mB�fB�fB�fB�`B�fB�fB�fB�fB�fB�fB�mB�mB�mB�B�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B
=BoB&�B-B;dBJ�BW
BXBR�BYB_;BiyBt�B� B�7B�1B�1B�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�7B�Bo�B_;BS�BJ�B7LB!�BoBDB1BB��B�B�HB�B��BǮB�LB��B��B�{B�Bl�BYBA�B(�B�B%B
�B
�;B
�5B
�#B
��B
��B
�B
�B
��B
��B
��B
�oB
�JB
�=B
�B
~�B
v�B
hsB
[#B
L�B
>wB
&�B
�B
PB
B	��B	�yB	�NB	�/B	�B	��B	ɺB	�wB	�-B	��B	��B	��B	��B	��B	�VB	�B	y�B	o�B	gmB	aHB	VB	J�B	G�B	H�B	I�B	I�B	I�B	F�B	A�B	7LB	'�B	�B	�B	bB	%B��B��B�B�NB�;B�B��B��BǮBB�}B�jB�RB�FB�3B�B�B��B��B��B��B��B��B��B��B��B�oB�hB�VB�PB�DB�+B�B�B�B� B|�By�Bw�Bs�Bp�Bm�Bk�BiyBhsBgmBe`BbNB\)BYBYBW
BS�BP�BN�BO�BQ�BO�BO�BO�BO�BO�BN�BN�BK�BH�BH�BC�BC�BD�BE�BF�BD�BA�B@�B@�BB�BC�BK�BQ�B\)B[#BZBVBk�By�Bw�Bu�Bn�BiyBgmBgmBdZBe`BiyBjBjBe`BaHB[#BN�BH�BK�BO�BP�BP�BVBXBZB\)B]/B^5B]/B_;B`BB^5B_;B`BBbNBe`BffBffBe`Bk�Bn�Bm�BjBp�Bu�Bw�B{�B�B�+B�%B�%B�B�B�B�B�B�B�B� B� B~�B~�B~�B~�B~�B� B� B�B�%B�%B�1B�7B�=B�DB�PB�VB�bB�oB�oB�uB�oB�oB��B��B��B��B��B�?BĜBBĜB��B��B��B�)B�NB�B�B�B�B��B��B��B��B��B��B	B	B		7B	DB	VB	bB	oB	�B	�B	�B	!�B	�B	 �B	 �B	"�B	(�B	-B	/B	33B	6FB	9XB	=qB	@�B	C�B	I�B	O�B	VB	W
B	VB	VB	T�B	T�B	XB	ZB	ZB	[#B	^5B	_;B	cTB	ffB	hsB	hsB	hsB	hsB	hsB	hsB	k�B	l�B	q�B	r�B	s�B	s�B	u�B	w�B	y�B	|�B	~�B	�B	�B	�B	�+B	�+B	�1B	�7B	�DB	�VB	�VB	�VB	�bB	�hB	�uB	�uB	�{B	��B	��B	��B	��B	�B	�LB	�}B	ĜB	ŢB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�)B	�)B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�;B	�TB	�NB	�NB	�NB	�ZB	�`B	�fB	�yB	�B	�]B
�B
12222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191730                              AO  ARCAADJP                                                                    20181005191730    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191730  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191730  QCF$                G�O�G�O�G�O�8000            