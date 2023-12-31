CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:04Z creation      
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
_FillValue                 �  A0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  g�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  wp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140804  20181024140804  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @נFk��c1   @נF��e�@4,�C���c���"��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�  @�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C  C  C  C�C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCw�fCz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D#��D$y�D$��D%y�D&  D&� D'fD'�fD(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4y�D5  D5� D6  D6� D7  D7� D8  D8y�D8��D9� D:  D:� D;  D;� D<  D<� D=  D=� D>fD>� D?fD?�fD@  D@y�DA  DA� DB  DB� DC  DC� DDfDD� DD��DE� DFfDF� DG  DG� DHfDH� DI  DI�fDJ  DJ� DK  DK� DK��DL� DM  DM� DN  DNy�DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ� DZ��D[� D\fD\� D]  D]� D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dvy�Dw  Dw� Dy��D�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��H@��HAp�A!p�AAp�Aap�A��RA��RA��A��RA��RAиRA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BH\)BP\)BX\)B`\)Bh\)Bp\)Bx\)B�.B�.B���B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B���B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C 
C
C
C
C
C

C0�C
C
C
C
C
C
C
C
C0�C 
C"
C$
C%�pC(
C*
C,
C.
C0
C2
C4
C6
C8
C:
C;�pC>
C@
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
Cd
Cf
Ch
Cj
Cl
Cn
Cp
Cr
Ct
Cu�pCw�pCz
C|
C~
C��C��C��C��C�RC��C��C��C��C��C��C���C��C��C��C��C�RC��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC�RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D�)D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D)D��D�D��D�D��D�D��D�D��D)D�)D�D��D�D��D�D��D�D��D�D��D�D�)D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#]D#�]D$]D$�]D%]D&�D&��D')D'�)D()D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4]D5�D5��D6�D6��D7�D7��D8�D8]D8�]D9��D:�D:��D;�D;��D<�D<��D=�D=��D>)D>��D?)D?�)D@�D@]DA�DA��DB�DB��DC�DC��DD)DD��DD�]DE��DF)DF��DG�DG��DH)DH��DI�DI�)DJ�DJ��DK�DK��DK�]DL��DM�DM��DN�DN]DO�DO��DO�]DP��DQ�DQ��DR�DR��DS�DS��DT)DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ)DZ��DZ�]D[��D\)D\��D]�D]��D^�D^�)D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm]Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dq�]Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv]Dw�Dw��Dy��D�C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�ffA��A˲-A�jA�9XA��A�1A�%AʁA�/A��
A�/A�\)A˅A�~�A�XA�bA���A��/Aʝ�A�O�A�  Aɺ^A�n�A�E�A�7LA�C�A�Q�A�%A��A�/A�z�A��A���A���A���Aơ�A�jAƋDAƥ�A�A�+A�bAƉ7A��A���A�ĜAžwA��A�O�A�A�A�A���A��;A�7LA���A���A��DA�r�A���A�ZA�r�A�/A��mA�XA��uA�+A�E�A��A���A�jA�ĜA�;dA���A�M�A�-A��RA�VA��A�$�A�VA��PA�A��^A�ffA�A�A�jA���A��A�`BA�1A�oA��A�33A�ȴA��DA���A�hsA��-A�  A�A�A��-A���A���A�VA�
=A��^A��uA�v�A��A�jAƨA~�A|��A|bNA|  A{p�A{/Az�/AzjAy�Av�As�Ap��ApȴAo�-Ak�AfQ�AcoA`n�AZZAY%AV�DAS�hAQVAJ�HAEdZAD�9ACO�AAoA;��A;
=A9��A8��A7/A3�A1��A0�RA0$�A-oA(I�A'�PA'VA&JA$��A$Q�A#��A!S�A��A��AbNAx�AVA�/A�A&�A  A�An�A1'AA�A{AbNAƨAƨA�-AG�AbA��Ar�AJA�\A�A �A�hA	p�A=qAhsAG�A
=AĜA�DAQ�A�mA�TA�#AƨA/A�yA �AbNA�^A33A �+A A�@��
@�ȴ@�v�@�~�@���@�/@��/@�ƨ@��-@�(�@���@�ff@�&�@��H@�@�?}@�D@� �@��;@�t�@��H@�7L@��@��@���@���@�9X@�F@��@�ff@��@���@��;@�ff@�Q�@�\)@��H@��T@�G�@��;@���@�/@��@���@�Ĝ@Դ9@ԓu@�z�@�\)@�?}@�ƨ@�
=@�~�@���@��@�@��@�I�@��;@�dZ@�;d@��@�
=@�@��H@��@ʸR@ʏ\@�v�@�V@��#@�`B@�%@��`@ȴ9@ȃ@�9X@� �@�  @�|�@�33@�o@�v�@���@î@�l�@\@���@���@��u@�bN@��;@�S�@��@��P@��y@�5?@���@�ƨ@�@�~�@�J@��@���@�O�@���@���@�@�X@��/@��9@���@���@�9X@��w@�ȴ@��#@�@���@�`B@���@�z�@��
@�
=@���@��@�@���@��@�X@�7L@�%@��9@�1@��m@�ƨ@��@�S�@�
=@��H@��@�E�@��-@�`B@�7L@���@�Q�@�Z@�V@�p�@�G�@�%@���@���@�Ĝ@�r�@�ȴ@��T@��#@���@��h@�7L@���@���@��/@���@�j@��@�  @��@���@��@��@�ff@�=q@�{@�^5@�^5@�-@�=q@��@��@�@�/@��j@���@�r�@�j@�Z@�I�@�j@�1'@�  @�;d@��@��R@�ȴ@���@�J@��-@�O�@�j@��;@��H@�J@��@��j@�A�@�A�@��m@�\)@�+@���@�^5@�E�@�{@�p�@�%@���@���@�Q�@�1@��@�K�@��R@��@��-@���@��@�G�@�j@��@���@�$�@��T@���@���@�@��^@��^@���@���@�x�@�X@�`B@�7L@��@�ƨ@�\)@�+@��@��R@��\@�J@��T@��@���@���@��@�x�@�x�@��@��@�hs@�X@�X@�X@�O�@�/@�V@��@��@��@���@��@��/@���@��/@���@���@��@��`@��/@��/@��/@��/@��/@�Ĝ@��@l61111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�ffA��A˲-A�jA�9XA��A�1A�%AʁA�/A��
A�/A�\)A˅A�~�A�XA�bA���A��/Aʝ�A�O�A�  Aɺ^A�n�A�E�A�7LA�C�A�Q�A�%A��A�/A�z�A��A���A���A���Aơ�A�jAƋDAƥ�A�A�+A�bAƉ7A��A���A�ĜAžwA��A�O�A�A�A�A���A��;A�7LA���A���A��DA�r�A���A�ZA�r�A�/A��mA�XA��uA�+A�E�A��A���A�jA�ĜA�;dA���A�M�A�-A��RA�VA��A�$�A�VA��PA�A��^A�ffA�A�A�jA���A��A�`BA�1A�oA��A�33A�ȴA��DA���A�hsA��-A�  A�A�A��-A���A���A�VA�
=A��^A��uA�v�A��A�jAƨA~�A|��A|bNA|  A{p�A{/Az�/AzjAy�Av�As�Ap��ApȴAo�-Ak�AfQ�AcoA`n�AZZAY%AV�DAS�hAQVAJ�HAEdZAD�9ACO�AAoA;��A;
=A9��A8��A7/A3�A1��A0�RA0$�A-oA(I�A'�PA'VA&JA$��A$Q�A#��A!S�A��A��AbNAx�AVA�/A�A&�A  A�An�A1'AA�A{AbNAƨAƨA�-AG�AbA��Ar�AJA�\A�A �A�hA	p�A=qAhsAG�A
=AĜA�DAQ�A�mA�TA�#AƨA/A�yA �AbNA�^A33A �+A A�@��
@�ȴ@�v�@�~�@���@�/@��/@�ƨ@��-@�(�@���@�ff@�&�@��H@�@�?}@�D@� �@��;@�t�@��H@�7L@��@��@���@���@�9X@�F@��@�ff@��@���@��;@�ff@�Q�@�\)@��H@��T@�G�@��;@���@�/@��@���@�Ĝ@Դ9@ԓu@�z�@�\)@�?}@�ƨ@�
=@�~�@���@��@�@��@�I�@��;@�dZ@�;d@��@�
=@�@��H@��@ʸR@ʏ\@�v�@�V@��#@�`B@�%@��`@ȴ9@ȃ@�9X@� �@�  @�|�@�33@�o@�v�@���@î@�l�@\@���@���@��u@�bN@��;@�S�@��@��P@��y@�5?@���@�ƨ@�@�~�@�J@��@���@�O�@���@���@�@�X@��/@��9@���@���@�9X@��w@�ȴ@��#@�@���@�`B@���@�z�@��
@�
=@���@��@�@���@��@�X@�7L@�%@��9@�1@��m@�ƨ@��@�S�@�
=@��H@��@�E�@��-@�`B@�7L@���@�Q�@�Z@�V@�p�@�G�@�%@���@���@�Ĝ@�r�@�ȴ@��T@��#@���@��h@�7L@���@���@��/@���@�j@��@�  @��@���@��@��@�ff@�=q@�{@�^5@�^5@�-@�=q@��@��@�@�/@��j@���@�r�@�j@�Z@�I�@�j@�1'@�  @�;d@��@��R@�ȴ@���@�J@��-@�O�@�j@��;@��H@�J@��@��j@�A�@�A�@��m@�\)@�+@���@�^5@�E�@�{@�p�@�%@���@���@�Q�@�1@��@�K�@��R@��@��-@���@��@�G�@�j@��@���@�$�@��T@���@���@�@��^@��^@���@���@�x�@�X@�`B@�7L@��@�ƨ@�\)@�+@��@��R@��\@�J@��T@��@���@���@��@�x�@�x�@��@��@�hs@�X@�X@�X@�O�@�/@�V@��@��@��@���@��@��/@���@��/@���@���@��@��`@��/@��/@��/@��/@��/@�Ĝ@��@l61111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
2-B
-B
 �B
�B
hB
PB

=B

=B
{B
%B	��B
&�B
?}B
J�B
W
B
m�B
t�B
z�B
�7B
�bB
��B
��B
�B
�dB
ɺB
��B
�#B
�BPB$�B,B�BB%BB�B<jB?}B;dBQ�BcTB�B�{B�!BÖB�
B�NB�`B�yB��B�B�B�BuB�BB�BK�BO�BQ�BR�BW
B_;Bq�Bn�Bm�Br�Bx�Bz�Bk�B�B�%B|�By�Bz�B\)BL�BG�B9XB.B(�B"�B �B�BbB��B�mB��BĜB�-B�\Bx�Bo�B^5B>wB#�B\B	7B
��B
�NB
ƨB
�wB
�-B
�7B
T�B
D�B
@�B
>wB
;dB
:^B
7LB
33B
-B
&�B
�B
�B
{B
hB
VB
JB

=B
%B	��B	�B	�/B	��B	��B	��B	��B	�B	l�B	[#B	B�B	:^B	.B	!�B	{B��B�B�B�mB�BB�
B��B��B��BƨB��B�-B�'B��B��B�1B�B�B�B�B~�B{�By�Bx�Bz�Bz�B�B�1B�7B�7B�B�B�B�%B�1B�DB�PB�VB�PB�PB�VB�VB�\B�\B�VB�JB�JB�=B�+B�B�%B�+B�1B�7B�DB�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�9B�?B�?B�?B�9B�3B�9B�-B�-B�'B�!B�B�B�B�B�9B�FB�LB�RB�XB�LB�LB�FB�RB�^B�wB��B��BÖBĜBƨB��B��B��B��B��B��B��B��B��B��B�
B�/B�5B�5B�HB�sB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	%B		7B	
=B	
=B	VB	\B	hB	uB	bB	hB	�B	�B	�B	�B	 �B	 �B	!�B	!�B	%�B	0!B	0!B	1'B	2-B	1'B	33B	6FB	9XB	=qB	@�B	F�B	P�B	XB	[#B	\)B	^5B	aHB	cTB	dZB	e`B	jB	r�B	y�B	z�B	z�B	}�B	�%B	�%B	�=B	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�FB	�FB	�^B	��B	ĜB	ŢB	ŢB	ĜB	ŢB	ŢB	ŢB	��B	�}B	B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ȴB	ȴB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�)B	�#B	�B	�B	�B	�/B	�HB	�;B	�BB	�HB	�5B	�BB	�NB	�ZB	�ZB	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
%B
+B
+B
+B
+B
+B
+B
+B
1B
1B
1B
1B
	7B
DB
JB
PB
PB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
hB
hB
hB
hB
oB
oB
hB
hB
hB
�B
SB
,1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
2-B
-B
 �B
�B
hB
PB

=B

=B
{B
%B	��B
&�B
?}B
J�B
W
B
m�B
t�B
z�B
�7B
�bB
��B
��B
�B
�dB
ɺB
��B
�#B
�BPB$�B,B�BB%BB�B<jB?}B;dBQ�BcTB�B�{B�!BÖB�
B�NB�`B�yB��B�B�B�BuB�BB�BK�BO�BQ�BR�BW
B_;Bq�Bn�Bm�Br�Bx�Bz�Bk�B�B�%B|�By�Bz�B\)BL�BG�B9XB.B(�B"�B �B�BbB��B�mB��BĜB�-B�\Bx�Bo�B^5B>wB#�B\B	7B
��B
�NB
ƨB
�wB
�-B
�7B
T�B
D�B
@�B
>wB
;dB
:^B
7LB
33B
-B
&�B
�B
�B
{B
hB
VB
JB

=B
%B	��B	�B	�/B	��B	��B	��B	��B	�B	l�B	[#B	B�B	:^B	.B	!�B	{B��B�B�B�mB�BB�
B��B��B��BƨB��B�-B�'B��B��B�1B�B�B�B�B~�B{�By�Bx�Bz�Bz�B�B�1B�7B�7B�B�B�B�%B�1B�DB�PB�VB�PB�PB�VB�VB�\B�\B�VB�JB�JB�=B�+B�B�%B�+B�1B�7B�DB�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�9B�?B�?B�?B�9B�3B�9B�-B�-B�'B�!B�B�B�B�B�9B�FB�LB�RB�XB�LB�LB�FB�RB�^B�wB��B��BÖBĜBƨB��B��B��B��B��B��B��B��B��B��B�
B�/B�5B�5B�HB�sB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	%B		7B	
=B	
=B	VB	\B	hB	uB	bB	hB	�B	�B	�B	�B	 �B	 �B	!�B	!�B	%�B	0!B	0!B	1'B	2-B	1'B	33B	6FB	9XB	=qB	@�B	F�B	P�B	XB	[#B	\)B	^5B	aHB	cTB	dZB	e`B	jB	r�B	y�B	z�B	z�B	}�B	�%B	�%B	�=B	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�FB	�FB	�^B	��B	ĜB	ŢB	ŢB	ĜB	ŢB	ŢB	ŢB	��B	�}B	B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ȴB	ȴB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�)B	�#B	�B	�B	�B	�/B	�HB	�;B	�BB	�HB	�5B	�BB	�NB	�ZB	�ZB	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
%B
+B
+B
+B
+B
+B
+B
+B
1B
1B
1B
1B
	7B
DB
JB
PB
PB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
hB
hB
hB
hB
oB
oB
hB
hB
hB
�B
SB
,1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140804                              AO  ARCAADJP                                                                    20181024140804    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140804  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140804  QCF$                G�O�G�O�G�O�0               