CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:05Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140805  20181024140805  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               	A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @ף:g��1   @ף�},&@4;"��`B�c��hr�!1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      	A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�33A�33A�33A�  A�  A�  B   B  B  B  B   B(  B0  B7��B?��BH  BPffBX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C�C�C  C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C��C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D	  D	� D
  D
� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)�fD*  D*�fD+fD+� D,  D,� D-  D-� D-��D.y�D/  D/� D0  D0� D1  D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D>��D?� D@  D@�fDA  DA� DB  DB� DC  DCy�DC��DD� DD��DE� DFfDF�fDG  DG� DHfDH� DI  DI�fDJfDJ� DK  DKy�DL  DL�fDMfDM� DN  DN� DO  DO� DP  DP�fDQfDQ� DR  DR� DS  DS� DTfDT� DT��DUy�DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D]��D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDufDu� Du��Dvy�Dv��Dwy�DwٚDy�{D�9�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@��HAp�A!p�AAp�Aap�A��RA��RA��A��A��AиRA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B7��B?��BH\)BPBX\)B`\)Bh\)Bp\)BxB�.B�.B�.B�.B�.B���B���B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB�aGB�.B�.B�.B�.B�.B�.B�.B�.B�aGB�.C 
C0�C0�C
C
C

C
C
C�pC
C
C
C
C
C
C
C 
C"0�C$
C&
C(
C*
C,
C.
C0
C2
C4
C6
C8
C:
C<
C=�pC@
CB
CD
CF
CH
CJ
CL
CN
CP
CR
CT
CV
CX
CZ
C\
C^
C`
Cb
Cd0�Cf
Ch
Cj
Cl
Cn
Cp
Cr
Cs�pCv
Cx
Cz
C|
C~
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C�RC��C��C��C��C��C��C��C��C�RC�RC��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C���C���C��C��C��C��C��C��C��C��C��C�RC��C��C�RC�RC��C�RC�RC��C���C��C��C��C��C���C��C��C���C��C�RC�RC�RC��C��C��C�RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C��C��C��C���C��C�RC�RC��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D�)D)D��D�D��D	�D	��D
�D
��D�D��D)D�)D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�)D�D��D�D��D�D��D�D��D�D��D�]D��D�D��D�D��D�D��D�D��D�D��D �D ��D �]D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D))D)�)D*�D*�)D+)D+��D,�D,��D-�D-��D-�]D.]D/�D/��D0�D0��D1�D1]D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=)D=��D>�D>��D>�]D?��D@�D@�)DA�DA��DB�DB��DC�DC]DC�]DD��DD�]DE��DF)DF�)DG�DG��DH)DH��DI�DI�)DJ)DJ��DK�DK]DL�DL�)DM)DM��DN�DN��DO�DO��DP�DP�)DQ)DQ��DR�DR��DS�DS��DT)DT��DT�]DU]DV�DV��DW�DW��DX�DX�)DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]]D]�]D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl]Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�)Du)Du��Du�]Dv]Dv�]Dw]Dw�]Dy�>D�<{D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�M�A�M�A�O�A�Q�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�VA�XA�\)A�\)A�\)A�\)A�ZA�ZA�^5A�`BA�bNA�bNA�dZA�ffA�hsA�jA�n�A�n�A�n�A�hsA�hsA�ffA�dZA�^5A�?}A��A���A�l�A�G�A��AǇ+A�7LA�  A��AŅA�-Ać+A�$�A�^5A��HA��A�VA��\A��RA�XA�ZA��A��hA��A��RA��A�{A�\)A�Q�A�~�A���A��A�r�A��PA�A�ffA���A�hsA�r�A�I�A��/A��+A���A���A�ĜA���A��A�  A�~�A�1A�  A�l�A��FA��A��FA�9XA��A���A���A�M�A��A�+A��A�Q�A�ƨA���A�;dA��-A�^5A�5?A�"�A��-A�A�=qA��A}\)A{�PAu�^Ap�HAm&�AjI�Ag��Ae7LAcK�Ab1A`�A]\)A\�+AZ��AY�AW\)AT�ASC�ARZAP�!AM�FAK�AG��AB��A@�+A@E�A?A>{A<�\A;��A;��A;\)A;VA:Q�A9ƨA9�FA9�^A9\)A8VA7�wA6M�A5p�A2�`A2bA0��A0A.1'A,�\A+p�A*jA)�-A'x�A&��A%
=A#|�A"��A!��A��AG�A1A�^A�`A��A�-AZAȴA(�AA��A|�A��A��A�7A�hA�hAx�AO�A/A�AK�A��A�;A��A�AJA��A%AbAt�A�A
�`A
�+A
jA	�;A	�-A	��A	x�A	33A�!A��A�FA�AI�A(�A 1'@���@��@���@�o@�~�@�dZ@�o@��@�@�hs@��`@�r�@�I�@� �@�\)@��@��@���@�X@�r�@��@��@��H@��@�h@�V@��@��m@��;@���@�@�"�@�\@�v�@�J@�@��@߶F@�V@�X@��@���@���@�Z@��;@�-@���@���@�&�@���@���@�z�@��;@�\)@�o@���@�^5@ѡ�@Ώ\@˥�@�Ĝ@�+@���@�5?@�@��@�
=@�=q@���@�?}@��u@��@��y@��D@�9X@�9X@���@��T@��@� �@��P@�"�@�=q@��-@�x�@�O�@�G�@�/@�V@��@���@�r�@�A�@��;@��@���@���@��@�K�@�33@���@�^5@�@�x�@�/@�Z@��;@�33@���@�n�@�V@�=q@�5?@��@��-@�Ĝ@�S�@�X@�(�@�33@��R@�=q@���@�`B@��`@��u@�z�@�b@��@���@��@���@���@�;d@�ff@�v�@�M�@�$�@�$�@���@�@��h@�/@���@��u@�b@��F@�C�@��y@���@�v�@�M�@�E�@�E�@�-@�$�@�-@�$�@�{@�@��@��-@���@��h@�x�@�X@�V@�z�@��;@���@��@�|�@�t�@�dZ@�S�@�;d@�+@�
=@��H@��!@��\@�^5@�$�@�J@���@��#@��^@���@��@�`B@�G�@�/@���@�(�@�t�@��@��!@��@�O�@���@�1@���@�33@��R@�{@���@�V@���@���@��u@���@���@��!@��!@���@���@��+@�~�@�v�@�n�@�^5@�^5@�$�@���@��^@��7@�p�@�O�@���@��/@���@�j@�9X@�1@���@���@�S�@�o@��@���@�ff@�{@���@��@��@��T@��#@��^@�/@��u@�bN@�I�@�9X@�(�@��@��@���@���@��@��m@��m@��;@��;@��;@�ƨ@�ƨ@��F@���@�|�@�K�@�o@���@��@��H@��H@�ȴ@�ȴ@���@���@p�U@^Z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�K�A�M�A�M�A�O�A�Q�A�S�A�S�A�S�A�S�A�S�A�S�A�S�A�VA�XA�\)A�\)A�\)A�\)A�ZA�ZA�^5A�`BA�bNA�bNA�dZA�ffA�hsA�jA�n�A�n�A�n�A�hsA�hsA�ffA�dZA�^5A�?}A��A���A�l�A�G�A��AǇ+A�7LA�  A��AŅA�-Ać+A�$�A�^5A��HA��A�VA��\A��RA�XA�ZA��A��hA��A��RA��A�{A�\)A�Q�A�~�A���A��A�r�A��PA�A�ffA���A�hsA�r�A�I�A��/A��+A���A���A�ĜA���A��A�  A�~�A�1A�  A�l�A��FA��A��FA�9XA��A���A���A�M�A��A�+A��A�Q�A�ƨA���A�;dA��-A�^5A�5?A�"�A��-A�A�=qA��A}\)A{�PAu�^Ap�HAm&�AjI�Ag��Ae7LAcK�Ab1A`�A]\)A\�+AZ��AY�AW\)AT�ASC�ARZAP�!AM�FAK�AG��AB��A@�+A@E�A?A>{A<�\A;��A;��A;\)A;VA:Q�A9ƨA9�FA9�^A9\)A8VA7�wA6M�A5p�A2�`A2bA0��A0A.1'A,�\A+p�A*jA)�-A'x�A&��A%
=A#|�A"��A!��A��AG�A1A�^A�`A��A�-AZAȴA(�AA��A|�A��A��A�7A�hA�hAx�AO�A/A�AK�A��A�;A��A�AJA��A%AbAt�A�A
�`A
�+A
jA	�;A	�-A	��A	x�A	33A�!A��A�FA�AI�A(�A 1'@���@��@���@�o@�~�@�dZ@�o@��@�@�hs@��`@�r�@�I�@� �@�\)@��@��@���@�X@�r�@��@��@��H@��@�h@�V@��@��m@��;@���@�@�"�@�\@�v�@�J@�@��@߶F@�V@�X@��@���@���@�Z@��;@�-@���@���@�&�@���@���@�z�@��;@�\)@�o@���@�^5@ѡ�@Ώ\@˥�@�Ĝ@�+@���@�5?@�@��@�
=@�=q@���@�?}@��u@��@��y@��D@�9X@�9X@���@��T@��@� �@��P@�"�@�=q@��-@�x�@�O�@�G�@�/@�V@��@���@�r�@�A�@��;@��@���@���@��@�K�@�33@���@�^5@�@�x�@�/@�Z@��;@�33@���@�n�@�V@�=q@�5?@��@��-@�Ĝ@�S�@�X@�(�@�33@��R@�=q@���@�`B@��`@��u@�z�@�b@��@���@��@���@���@�;d@�ff@�v�@�M�@�$�@�$�@���@�@��h@�/@���@��u@�b@��F@�C�@��y@���@�v�@�M�@�E�@�E�@�-@�$�@�-@�$�@�{@�@��@��-@���@��h@�x�@�X@�V@�z�@��;@���@��@�|�@�t�@�dZ@�S�@�;d@�+@�
=@��H@��!@��\@�^5@�$�@�J@���@��#@��^@���@��@�`B@�G�@�/@���@�(�@�t�@��@��!@��@�O�@���@�1@���@�33@��R@�{@���@�V@���@���@��u@���@���@��!@��!@���@���@��+@�~�@�v�@�n�@�^5@�^5@�$�@���@��^@��7@�p�@�O�@���@��/@���@�j@�9X@�1@���@���@�S�@�o@��@���@�ff@�{@���@��@��@��T@��#@��^@�/@��u@�bN@�I�@�9X@�(�@��@��@���@���@��@��m@��m@��;@��;@��;@�ƨ@�ƨ@��F@���@�|�@�K�@�o@���@��@��H@��H@�ȴ@�ȴ@���@���@p�U@^Z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BB%B%B+B+B%B1B+BJBDBJBVBVB
=B1B1B1B
=B�B�B�B�B�B�B�B#�B%�B%�B0!B2-B0!B2-B2-B1'B49B6FB:^B?}BN�BffBp�B�DB�uB��BŢBȴBÖB��B�^B�LB�-B�XB��B��B��B'�BVBdZBy�B�DB�DB�+B�B� B|�BhsBJ�BI�BS�BS�BP�BQ�BP�BO�BN�BM�BM�BK�BH�BC�B:^B2-B%�B{B��B��B��B�B�sB�5B��B��B�oBw�BiyB_;BT�BD�B49B{B
��B
�#B
�3B
}�B
S�B
B�B
A�B
@�B
;dB
1'B
'�B
oB	��B	�B	ŢB	��B	�DB	r�B	aHB	R�B	H�B	B�B	:^B	1'B	-B	(�B	!�B	�B	PB	%B	B��B�B�ZB�BɺBŢBŢBÖB��B�jB�^B�RB�LB�FB�?B�FB�FB�RB�^B�XB�RB�LB�9B�-B�!B�B��B��B��B��B�oB�VB�DB�\B��B�FB�jBB��BǮBǮBŢBŢBĜBŢB��B�^B�^B�wB�wB�}B��BBŢBǮB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBɺBȴBȴBǮBǮBǮBŢBĜBÖBB��B�wB�jB�^B�^B�^B�XB�RB�LB�XB�dB�dB�dB�dB�dB�dB�dB�dB�jB�jB�wB�wB�qB�jB�dB�^B�dB�^B�dB�wB�wB�wB�wB�wB�}B��B��BBÖBÖBŢBǮBɺBɺBɺB��B��B��B��B��B��B��B��B��B��B��B�B�
B�
B�
B�B�;B�`B�B�B�B��B��B��B	B	%B	1B		7B	DB	JB	PB	�B	�B	 �B	%�B	2-B	<jB	@�B	C�B	H�B	O�B	R�B	S�B	T�B	T�B	VB	W
B	ZB	ZB	[#B	\)B	^5B	_;B	_;B	_;B	_;B	`BB	_;B	aHB	aHB	cTB	dZB	e`B	iyB	k�B	o�B	s�B	u�B	v�B	w�B	w�B	x�B	y�B	~�B	�1B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�-B	�3B	�?B	�RB	�^B	�^B	�dB	�dB	�dB	�jB	�jB	�jB	�jB	�jB	�jB	�qB	�qB	�wB	�wB	�wB	�wB	�}B	B	ŢB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�;B	�NB	�ZB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B
PB
�B
&�B
7�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BB%B%B+B+B%B1B+BJBDBJBVBVB
=B1B1B1B
=B�B�B�B�B�B�B�B#�B%�B%�B0!B2-B0!B2-B2-B1'B49B6FB:^B?}BN�BffBp�B�DB�uB��BŢBȴBÖB��B�^B�LB�-B�XB��B��B��B'�BVBdZBy�B�DB�DB�+B�B� B|�BhsBJ�BI�BS�BS�BP�BQ�BP�BO�BN�BM�BM�BK�BH�BC�B:^B2-B%�B{B��B��B��B�B�sB�5B��B��B�oBw�BiyB_;BT�BD�B49B{B
��B
�#B
�3B
}�B
S�B
B�B
A�B
@�B
;dB
1'B
'�B
oB	��B	�B	ŢB	��B	�DB	r�B	aHB	R�B	H�B	B�B	:^B	1'B	-B	(�B	!�B	�B	PB	%B	B��B�B�ZB�BɺBŢBŢBÖB��B�jB�^B�RB�LB�FB�?B�FB�FB�RB�^B�XB�RB�LB�9B�-B�!B�B��B��B��B��B�oB�VB�DB�\B��B�FB�jBB��BǮBǮBŢBŢBĜBŢB��B�^B�^B�wB�wB�}B��BBŢBǮB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBɺBȴBȴBǮBǮBǮBŢBĜBÖBB��B�wB�jB�^B�^B�^B�XB�RB�LB�XB�dB�dB�dB�dB�dB�dB�dB�dB�jB�jB�wB�wB�qB�jB�dB�^B�dB�^B�dB�wB�wB�wB�wB�wB�}B��B��BBÖBÖBŢBǮBɺBɺBɺB��B��B��B��B��B��B��B��B��B��B��B�B�
B�
B�
B�B�;B�`B�B�B�B��B��B��B	B	%B	1B		7B	DB	JB	PB	�B	�B	 �B	%�B	2-B	<jB	@�B	C�B	H�B	O�B	R�B	S�B	T�B	T�B	VB	W
B	ZB	ZB	[#B	\)B	^5B	_;B	_;B	_;B	_;B	`BB	_;B	aHB	aHB	cTB	dZB	e`B	iyB	k�B	o�B	s�B	u�B	v�B	w�B	w�B	x�B	y�B	~�B	�1B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�-B	�3B	�?B	�RB	�^B	�^B	�dB	�dB	�dB	�jB	�jB	�jB	�jB	�jB	�jB	�qB	�qB	�wB	�wB	�wB	�wB	�}B	B	ŢB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�;B	�NB	�ZB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B
PB
�B
&�B
7�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140805                              AO  ARCAADJP                                                                    20181024140805    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140805  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140805  QCF$                G�O�G�O�G�O�0               