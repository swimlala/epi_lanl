CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:42Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140842  20181024140842  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��d��D1   @��eWM@5;�l�C��d333331   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C�fC
  C  C�fC  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*�C,�C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C��C��C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��D   D � D  Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D��Dy�D��Dy�D  D�fD  D� D  D� D  D� D  D�fD  D� D  D�fDfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D,��D-� D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DO��DP� DQ  DQ� DQ��DRy�DS  DS�fDT  DT� DU  DUy�DU��DV� DW  DW� DX  DXy�DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Db��Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dl��Dm� Dn  Dn� Do  Do�fDpfDp�fDq  Dq� Dr  Dr� Ds  Ds�fDtfDt� Du  Du� Dv  Dv� Dw  Dw� DwٚDy��D�6f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@��HAp�A!p�AAp�Aap�A��RA��RA��A��RA��RAиRA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BHBP\)BX\)B`\)Bg��Bp\)Bx\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB�.C 
C
C
C
C�pC

C
C�pC
C
C
C
C
C0�C
C
C 
C"
C$
C&
C(
C*0�C,0�C.
C0
C2
C4
C6
C8
C:0�C<
C>
C@
CB
CD
CF
CH
CI�pCL
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
Cd
Cf
Ch
Cj
Cl
Cn
Cp
Cr
Ct
Cv
Cx
Cz
C|
C~0�C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C���C��C�RC��C���C��C�RC�RC��C��C���C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC�RC�RC��C��C��C��C��C��C��C�RC�RC��C��C��C�RC�RC��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC�RC��C��C��C��C��C��C��C��C��C���C���C���C��C��C��C��C��C��C���C���C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C�RD �D ��D�D]D�D�)D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D�)D�D��D�D��D�D��D�D��D�D��D�]D]D�]D]D�D�)D�D��D�D��D�D��D�D�)D�D��D�D�)D)D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$�)D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D,�]D-��D.�D.��D/)D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO]DO�]DP��DQ�DQ��DQ�]DR]DS�DS�)DT�DT��DU�DU]DU�]DV��DW�DW��DX�DX]DX�]DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Db�]Dc]Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh�)Di�Di��Dj�Dj��Dk�Dk��Dl�Dl]Dl�]Dm��Dn�Dn��Do�Do�)Dp)Dp�)Dq�Dq��Dr�Dr��Ds�Ds�)Dt)Dt��Du�Du��Dv�Dv��Dw�Dw��Dw�]Dy�]D�9G111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A˲-A˴9A˴9A˲-A˸RA˼jA˴9A˾wA�ƨA���A˸RA˾wA˼jA˺^A�ĜA�ƨA�ȴA�ȴA�ƨA���A���A���A���A���A˾wA�l�A��Aʝ�Aə�A�O�A�(�A�VA��yA��A���A��AìA�VA�-A���A��A���A���A�1A�bA�$�A�E�A��A�~�A�1A�
=A��A��A���A��RA�I�A��A�A��FA�XA�?}A�/A��wA�{A��9A��^A��PA��A� �A��^A�+A��A�p�A�M�A�oA���A��7A�
=A�(�A��^A�/A�v�A��A��A�|�A�r�A�VA��TA���A�I�A�K�A�A��RA���A�jA���A�K�A�^5A�ĜA��`A�A�A�A��PA��A�^5A�dZAyp�Aw�Aw+Av-Au;dAr�Aq�An�Am�^AlVAh��Ae��Ac��Ab�9Ab �Ab$�Ab�Aa�
Aa�^Aa��Aa��A_�A\~�AZn�AV��ATffAS�AP�/AO;dAMx�AL�+AJQ�AF�\AEx�AE%AC�PA?�7A=�-A<-A;dZA;A:ĜA:�+A:A�A9`BA7�
A6�DA6�A4-A1�^A0^5A.��A-|�A,=qA*��A)�A)`BA(��A(1A&VA%��A%oA$M�A"�A"�A"$�A ��A
=A9XA��A"�A��A;dA�A�^Av�A7LA�A��A��A��A�wAO�A�uAx�A�/A�TA�9Av�A^5AA�A{A�^A�A�PA
��A
�A	�hA	�A��A�\AS�A$�Ax�@���@�+@�@�I�@�{@��@�v�@�@���@��@��m@�\@��@�`B@�bN@�C�@���@�o@�9X@�|�@�;d@�V@��`@�ƨ@�"�@�E�@�C�@ٙ�@�Ĝ@׮@��@�p�@���@�z�@�r�@� �@җ�@��@��#@��#@�@�x�@�/@��@��
@�x�@�V@ɲ-@ɲ-@��T@��@�{@��#@�-@ț�@��@���@�M�@Ł@�%@Ĵ9@�b@�dZ@��y@�ȴ@�ff@�V@�{@��-@���@��@�G�@�&�@��@�I�@��!@��@��@�@�j@�Q�@�Q�@�I�@�t�@�V@���@��@�bN@��@�-@���@��@��/@��j@��@��u@� �@��
@��w@�b@�?}@���@��@�=q@�x�@���@��#@��#@��@�5?@�5?@�5?@�=q@���@�dZ@��m@�r�@��D@���@���@��-@�=q@���@���@�ȴ@�~�@�E�@�E�@�n�@�~�@�n�@�^5@�=q@��T@���@��@��/@��
@�V@���@��^@���@�/@���@�  @�K�@�+@��@�v�@���@�7L@��@�r�@�1'@��@�
=@���@�v�@�v�@���@���@�9X@�  @��@��@��!@�~�@�$�@�@��#@��-@��7@�O�@�/@��@���@��@�r�@�A�@�  @�  @��@���@��@�t�@�S�@�
=@��R@�E�@�{@�@��@��-@��@�G�@�&�@�/@�&�@�&�@��@��@�9X@��F@�t�@�dZ@�;d@��@���@�ff@�V@�=q@�@���@��^@���@�p�@�7L@�%@��j@�j@�I�@�I�@�I�@�9X@�1'@��@��F@�S�@�
=@��H@���@���@�~�@�V@�@�G�@�`B@�?}@�/@�/@�&�@��j@�Q�@��@�ƨ@���@��@�dZ@�;d@�@���@���@��R@��+@�n�@�^5@�=q@�{@��@���@�x�@�O�@��@tM111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A˲-A˴9A˴9A˲-A˸RA˼jA˴9A˾wA�ƨA���A˸RA˾wA˼jA˺^A�ĜA�ƨA�ȴA�ȴA�ƨA���A���A���A���A���A˾wA�l�A��Aʝ�Aə�A�O�A�(�A�VA��yA��A���A��AìA�VA�-A���A��A���A���A�1A�bA�$�A�E�A��A�~�A�1A�
=A��A��A���A��RA�I�A��A�A��FA�XA�?}A�/A��wA�{A��9A��^A��PA��A� �A��^A�+A��A�p�A�M�A�oA���A��7A�
=A�(�A��^A�/A�v�A��A��A�|�A�r�A�VA��TA���A�I�A�K�A�A��RA���A�jA���A�K�A�^5A�ĜA��`A�A�A�A��PA��A�^5A�dZAyp�Aw�Aw+Av-Au;dAr�Aq�An�Am�^AlVAh��Ae��Ac��Ab�9Ab �Ab$�Ab�Aa�
Aa�^Aa��Aa��A_�A\~�AZn�AV��ATffAS�AP�/AO;dAMx�AL�+AJQ�AF�\AEx�AE%AC�PA?�7A=�-A<-A;dZA;A:ĜA:�+A:A�A9`BA7�
A6�DA6�A4-A1�^A0^5A.��A-|�A,=qA*��A)�A)`BA(��A(1A&VA%��A%oA$M�A"�A"�A"$�A ��A
=A9XA��A"�A��A;dA�A�^Av�A7LA�A��A��A��A�wAO�A�uAx�A�/A�TA�9Av�A^5AA�A{A�^A�A�PA
��A
�A	�hA	�A��A�\AS�A$�Ax�@���@�+@�@�I�@�{@��@�v�@�@���@��@��m@�\@��@�`B@�bN@�C�@���@�o@�9X@�|�@�;d@�V@��`@�ƨ@�"�@�E�@�C�@ٙ�@�Ĝ@׮@��@�p�@���@�z�@�r�@� �@җ�@��@��#@��#@�@�x�@�/@��@��
@�x�@�V@ɲ-@ɲ-@��T@��@�{@��#@�-@ț�@��@���@�M�@Ł@�%@Ĵ9@�b@�dZ@��y@�ȴ@�ff@�V@�{@��-@���@��@�G�@�&�@��@�I�@��!@��@��@�@�j@�Q�@�Q�@�I�@�t�@�V@���@��@�bN@��@�-@���@��@��/@��j@��@��u@� �@��
@��w@�b@�?}@���@��@�=q@�x�@���@��#@��#@��@�5?@�5?@�5?@�=q@���@�dZ@��m@�r�@��D@���@���@��-@�=q@���@���@�ȴ@�~�@�E�@�E�@�n�@�~�@�n�@�^5@�=q@��T@���@��@��/@��
@�V@���@��^@���@�/@���@�  @�K�@�+@��@�v�@���@�7L@��@�r�@�1'@��@�
=@���@�v�@�v�@���@���@�9X@�  @��@��@��!@�~�@�$�@�@��#@��-@��7@�O�@�/@��@���@��@�r�@�A�@�  @�  @��@���@��@�t�@�S�@�
=@��R@�E�@�{@�@��@��-@��@�G�@�&�@�/@�&�@�&�@��@��@�9X@��F@�t�@�dZ@�;d@��@���@�ff@�V@�=q@�@���@��^@���@�p�@�7L@�%@��j@�j@�I�@�I�@�I�@�9X@�1'@��@��F@�S�@�
=@��H@���@���@�~�@�V@�@�G�@�`B@�?}@�/@�/@�&�@��j@�Q�@��@�ƨ@���@��@�dZ@�;d@�@���@���@��R@��+@�n�@�^5@�=q@�{@��@���@�x�@�O�@��@tM111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�By�By�By�By�Bx�Bx�By�Bx�Bx�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�B}�B�hBŢB��B�B"�B5?BA�BE�BK�BO�BO�BN�BM�BO�BW
B]/B^5B`BBe`BgmBo�B{�B�%B��B��B��B��B�JBz�B� Bz�B~�B�B�7B�=B�JB��B��B��B��B��B��B��B�uB�BcTBG�BB�BA�BB�BA�B<jB-B#�B �B�BuBoBDBB  B��B��B��B�B�5B��B��B��BŢB�3B��B��B�\Bs�BW
BD�B{B
�B
��B
�{B
]/B
N�B
H�B
?}B
6FB
�B
hB	��B	��B	�yB	��B	�qB	�!B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	t�B	ffB	]/B	VB	H�B	=qB	2-B	(�B	�B	B��B��B�B�TB�HB�BB�BB�;B�;B�/B�B�B��B��BƨB�wB�?B�'B�B��B��B��B��B��B��B��B�oB�\B�VB�PB�DB�=B�+B�B� B}�B{�By�Bv�Bs�Bq�Bp�Bp�Bo�Bm�Bo�Bq�Bq�Bs�Bs�Bs�Bs�Br�Bq�Bu�Bv�Bv�Bw�Bv�Bu�Bt�Bu�Bw�Bx�By�Bz�By�By�Bx�Bq�BcTBYB]/BaHBe`BiyBq�Bt�Bw�Bx�B� B�+B�7B�7B�7B�\B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�LB�XB�dB�jB�dB�dBBĜBĜBĜBĜBĜBĜBĜBÖB�}B�jB�jB�}B��B��BÖBÖBŢB��B��B��B�B�B�B�B�B�NB�TB�ZB�`B�fB�yB�B�B�B�B�B�B�B�B��B��B	B	DB	DB	
=B		7B	
=B	
=B	1B��B�B�NB�ZB�ZB�`B�`B�fB�fB�mB�B�B�B��B	DB	�B	%�B	#�B	%�B	)�B	,B	,B	-B	/B	/B	/B	0!B	33B	9XB	=qB	A�B	C�B	D�B	H�B	M�B	R�B	W
B	]/B	ffB	l�B	m�B	m�B	q�B	s�B	t�B	t�B	u�B	v�B	u�B	u�B	s�B	q�B	o�B	o�B	o�B	p�B	p�B	q�B	t�B	w�B	x�B	y�B	{�B	� B	�B	�+B	�1B	�7B	�DB	�bB	�{B	��B	��B	��B	I�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�/B	�5B	�5B	�5B	�HB	�HB	�NB	�NB	�TB	�ZB	�fB	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
%B
%B
%B
%B
%B
+B
+B
1B
1B
	7B
	7B

=B
DB
PB
PB
VB
VB
\B
\B
\B
bB
hB
hB
hB
hB
uB
�B
!�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  By�By�By�By�Bx�Bx�By�Bx�Bx�By�By�By�By�By�By�By�By�By�By�By�By�By�By�By�B}�B�hBŢB��B�B"�B5?BA�BE�BK�BO�BO�BN�BM�BO�BW
B]/B^5B`BBe`BgmBo�B{�B�%B��B��B��B��B�JBz�B� Bz�B~�B�B�7B�=B�JB��B��B��B��B��B��B��B�uB�BcTBG�BB�BA�BB�BA�B<jB-B#�B �B�BuBoBDBB  B��B��B��B�B�5B��B��B��BŢB�3B��B��B�\Bs�BW
BD�B{B
�B
��B
�{B
]/B
N�B
H�B
?}B
6FB
�B
hB	��B	��B	�yB	��B	�qB	�!B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	t�B	ffB	]/B	VB	H�B	=qB	2-B	(�B	�B	B��B��B�B�TB�HB�BB�BB�;B�;B�/B�B�B��B��BƨB�wB�?B�'B�B��B��B��B��B��B��B��B�oB�\B�VB�PB�DB�=B�+B�B� B}�B{�By�Bv�Bs�Bq�Bp�Bp�Bo�Bm�Bo�Bq�Bq�Bs�Bs�Bs�Bs�Br�Bq�Bu�Bv�Bv�Bw�Bv�Bu�Bt�Bu�Bw�Bx�By�Bz�By�By�Bx�Bq�BcTBYB]/BaHBe`BiyBq�Bt�Bw�Bx�B� B�+B�7B�7B�7B�\B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�LB�XB�dB�jB�dB�dBBĜBĜBĜBĜBĜBĜBĜBÖB�}B�jB�jB�}B��B��BÖBÖBŢB��B��B��B�B�B�B�B�B�NB�TB�ZB�`B�fB�yB�B�B�B�B�B�B�B�B��B��B	B	DB	DB	
=B		7B	
=B	
=B	1B��B�B�NB�ZB�ZB�`B�`B�fB�fB�mB�B�B�B��B	DB	�B	%�B	#�B	%�B	)�B	,B	,B	-B	/B	/B	/B	0!B	33B	9XB	=qB	A�B	C�B	D�B	H�B	M�B	R�B	W
B	]/B	ffB	l�B	m�B	m�B	q�B	s�B	t�B	t�B	u�B	v�B	u�B	u�B	s�B	q�B	o�B	o�B	o�B	p�B	p�B	q�B	t�B	w�B	x�B	y�B	{�B	� B	�B	�+B	�1B	�7B	�DB	�bB	�{B	��B	��B	��B	I�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�/B	�5B	�5B	�5B	�HB	�HB	�NB	�NB	�TB	�ZB	�fB	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
%B
%B
%B
%B
%B
+B
+B
1B
1B
	7B
	7B

=B
DB
PB
PB
VB
VB
\B
\B
\B
bB
hB
hB
hB
hB
uB
�B
!�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140842                              AO  ARCAADJP                                                                    20181024140842    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140842  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181024140842  QCF$                G�O�G�O�G�O�4000            