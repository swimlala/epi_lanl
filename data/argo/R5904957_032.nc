CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:10Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140810  20181024140810  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL                A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׷��Mp�1   @׷�@yoL@3L1&�y�c��+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                       A   A   A   @�33@�  A   A   A@  A`  A~ffA�  A�  A�  A���A���A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C�C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC9�fC<  C>  C@  CB  CD  CF�CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCy�fC{�fC~  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  D   D � D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D��Dy�D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  Dy�D   D � D!  D!� D"  D"� D#  D#� D#��D$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DG��DH� DI  DIy�DJ  DJy�DJ��DK� DL  DL� DM  DM� DN  DN� DOfDO� DO��DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]fD]�fD^  D^� D_  D_� D`  D`� Da  Da�fDbfDb� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Duy�Dv  Dv�fDw  Dw� Dx  Dy��D�Eq11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�ffA33A#33AC33Ac33A���A���A���A���A�fgA�fgAᙚA�B ��BfgB��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��BxfgB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33CL�CL�C L�C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C8�C:�C<33C>33C@33CB33CD33CFL�CHL�CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx�Cz�C|�C~33C��C��C��C��C��C��C��C��C��C��C��C��C�&gC�&gC��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&gC�&gC��C��C��C��C��C��C��C�&gC��C��C��C�&gC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D�3D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	3D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��DgD�gDgD�gD�D��D�D��D�D��D�D��D�D�gD�D��D�D��D�D��D�D�gD �D ��D!�D!��D"�D"��D#�D#��D$gD$��D%�D%��D&�D&��D'gD'��D(�D(��D)�D)�3D*�D*��D+�D+��D,�D,��D-�D-��D.gD.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE�gDF�DF��DG�DG��DHgDH��DI�DI�gDJ�DJ�gDKgDK��DL�DL��DM�DM��DN�DN��DO3DO��DPgDP�gDQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV�3DW�DW��DX�DX��DY3DY��DZ�DZ��D[�D[��D\�D\��D]3D]�3D^�D^��D_�D_��D`�D`��Da�Da�3Db3Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh�3Di�Di��Dj�Dj��DkgDk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq3Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du�gDv�Dv�3Dw�Dw��Dx�Dy��D�K�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�l�A�l�A�l�A�ffA�`BA�+AօA�1A��A�A՟�AՏ\AՁAՁA�~�A�|�A�r�A�x�A�n�A�C�A��;Aԥ�A�x�A��A��A�x�A��A�O�A�ZA�ffA��A��yAЍPA�p�AθRA���A�z�A�=qA�JA�XA�G�AɾwA�n�AȅA�O�A��yA�VA�7LA�A��-A�%A��wA�x�A���A���A�G�A�1'A���A�bNA�bA�;dA�^5A� �A�1A���A���A�33A��A�?}A��yA�x�A�A�ȴA�r�A�VA�K�A��A�VA�Q�A�S�A�I�A�A�5?A�p�A�~�A�K�A��A��uA�r�A���A�;dA��mA�;dA�A��jA�~�A��A�n�A�%A�%A���A���A�r�A�C�A�ƨA���A��mA��A��jA�9XA�VA�\)A�/A�ĜA�ffA|�9Asp�Ak��Ag�-Ac�;Ab��Aa��Aa��A^�HA[�AX��AW�hAV�/AU�TAT�ASS�APĜAO�hANA�AK��AH{AF�/AFI�AE�TAE�AD �AB�jAA��A@1'A=�A<�/A;A:�HA9?}A8v�A6�A6(�A4=qA2��A1�mA1hsA0�!A/%A,�A+oA*ffA)��A%p�A"�/A"�\A"=qA!x�A ^5AS�A��AC�A��A(�A�yAQ�AE�A��A+AbA��A�`A~�A��A$�A�^Ap�A?}A5?A|�A+A�yAr�A��At�AdZA�DA�A��A33A�AbA&�A
��A	�TAȴAƨA�wAO�A-A
=A��A��A��Ap�A ��A �A ff@��#@��D@��;@��7@���@��@�t�@�x�@��@�J@�z�@�M�@�O�@�I�@��H@��@�G�@�%@�D@�@���@�5?@�@�/@�r�@ߍP@�\)@���@�$�@݁@ە�@���@�?}@��m@�V@�%@�Q�@Ӯ@җ�@��@�-@�J@�@щ7@�7L@���@Гu@�r�@��m@·+@���@���@�&�@�%@�Z@�9X@˝�@�l�@�S�@��H@�@ɩ�@�G�@��/@ȣ�@�r�@��@�dZ@��@�$�@�%@�Q�@�\)@+@��^@�?}@�j@�C�@�5?@��7@�p�@�?}@�1'@�\)@��@��@��@� �@��@��w@�E�@�$�@��@�ƨ@��@��@�ƨ@�t�@�
=@�t�@�ƨ@��@��y@�
=@��@�v�@�Z@�
=@��!@���@��\@�E�@�$�@��R@�{@��@��9@�bN@�Q�@�b@���@���@���@�/@�Ĝ@�Q�@��m@��@���@�=q@���@�x�@��@��/@�z�@�(�@�`B@��!@��R@���@���@���@�-@���@�hs@�G�@��@���@��F@�l�@�;d@��@�v�@�=q@�{@���@��@�%@��@�z�@�A�@���@�|�@���@�v�@�$�@���@��j@�r�@�1@��
@�  @��w@���@���@���@��P@�dZ@�@���@�ff@��@�@�`B@�V@���@��@�Q�@��@�"�@�ȴ@�ff@��@���@��T@��T@��T@��T@��#@���@��-@�/@���@���@�Z@�1@��
@�|�@��@��y@��R@��+@�M�@�5?@�-@�J@��#@���@��-@��h@�X@�7L@�/@�&�@��@��/@�  @�ƨ@���@�t�@�S�@�
=@��!@���@�V@���@��h@�`B@�7L@�V@���@��9@�bN@�b@��;@��
@���@���@�ƨ@��w@��@�\)@�o@��@�ȴ@��!@���@�v�@�V@�5?@���@��@��@�x�@�/@���@��/@��@���@�r�@��
@��@���@�t�@�33@��@��H@�^5@���@r�A11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�l�A�l�A�l�A�ffA�`BA�+AօA�1A��A�A՟�AՏ\AՁAՁA�~�A�|�A�r�A�x�A�n�A�C�A��;Aԥ�A�x�A��A��A�x�A��A�O�A�ZA�ffA��A��yAЍPA�p�AθRA���A�z�A�=qA�JA�XA�G�AɾwA�n�AȅA�O�A��yA�VA�7LA�A��-A�%A��wA�x�A���A���A�G�A�1'A���A�bNA�bA�;dA�^5A� �A�1A���A���A�33A��A�?}A��yA�x�A�A�ȴA�r�A�VA�K�A��A�VA�Q�A�S�A�I�A�A�5?A�p�A�~�A�K�A��A��uA�r�A���A�;dA��mA�;dA�A��jA�~�A��A�n�A�%A�%A���A���A�r�A�C�A�ƨA���A��mA��A��jA�9XA�VA�\)A�/A�ĜA�ffA|�9Asp�Ak��Ag�-Ac�;Ab��Aa��Aa��A^�HA[�AX��AW�hAV�/AU�TAT�ASS�APĜAO�hANA�AK��AH{AF�/AFI�AE�TAE�AD �AB�jAA��A@1'A=�A<�/A;A:�HA9?}A8v�A6�A6(�A4=qA2��A1�mA1hsA0�!A/%A,�A+oA*ffA)��A%p�A"�/A"�\A"=qA!x�A ^5AS�A��AC�A��A(�A�yAQ�AE�A��A+AbA��A�`A~�A��A$�A�^Ap�A?}A5?A|�A+A�yAr�A��At�AdZA�DA�A��A33A�AbA&�A
��A	�TAȴAƨA�wAO�A-A
=A��A��A��Ap�A ��A �A ff@��#@��D@��;@��7@���@��@�t�@�x�@��@�J@�z�@�M�@�O�@�I�@��H@��@�G�@�%@�D@�@���@�5?@�@�/@�r�@ߍP@�\)@���@�$�@݁@ە�@���@�?}@��m@�V@�%@�Q�@Ӯ@җ�@��@�-@�J@�@щ7@�7L@���@Гu@�r�@��m@·+@���@���@�&�@�%@�Z@�9X@˝�@�l�@�S�@��H@�@ɩ�@�G�@��/@ȣ�@�r�@��@�dZ@��@�$�@�%@�Q�@�\)@+@��^@�?}@�j@�C�@�5?@��7@�p�@�?}@�1'@�\)@��@��@��@� �@��@��w@�E�@�$�@��@�ƨ@��@��@�ƨ@�t�@�
=@�t�@�ƨ@��@��y@�
=@��@�v�@�Z@�
=@��!@���@��\@�E�@�$�@��R@�{@��@��9@�bN@�Q�@�b@���@���@���@�/@�Ĝ@�Q�@��m@��@���@�=q@���@�x�@��@��/@�z�@�(�@�`B@��!@��R@���@���@���@�-@���@�hs@�G�@��@���@��F@�l�@�;d@��@�v�@�=q@�{@���@��@�%@��@�z�@�A�@���@�|�@���@�v�@�$�@���@��j@�r�@�1@��
@�  @��w@���@���@���@��P@�dZ@�@���@�ff@��@�@�`B@�V@���@��@�Q�@��@�"�@�ȴ@�ff@��@���@��T@��T@��T@��T@��#@���@��-@�/@���@���@�Z@�1@��
@�|�@��@��y@��R@��+@�M�@�5?@�-@�J@��#@���@��-@��h@�X@�7L@�/@�&�@��@��/@�  @�ƨ@���@�t�@�S�@�
=@��!@���@�V@���@��h@�`B@�7L@�V@���@��9@�bN@�b@��;@��
@���@���@�ƨ@��w@��@�\)@�o@��@�ȴ@��!@���@�v�@�V@�5?@���@��@��@�x�@�/@���@��/@��@���@�r�@��
@��@���@�t�@�33@��@��H@�^5@���@r�A11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�)B
�)B
�)B
�#B
�#B
��B
��B
�HB
�B
��B
��B
��B
��B
��B  B  BBBBB1B
=BPB
=B	7BbB1'B,B6FBC�B^5Be`B|�B�B� B~�B{�Bx�B�B�PB��B�XBÖB�)B��B�B2-BG�BL�BXBaHBdZBcTBk�BgmBffB]/B\)BZB^5BiyBz�B� B}�B|�B|�B�oB��B��B��B��B�\Bz�BM�B6FB-B?}B?}BE�BH�BI�BG�BT�BbNBS�BXBZBZBcTB[#BM�BB�BhB�B��BB��B�sB��B�HB��By�Bk�Br�Bk�BO�B(�B
��B
��B
��B
��B
��B
�PB
q�B
XB
�B	��B	��B	�B	m�B	e`B	bNB	`BB	M�B	6FB	,B	.B	,B	&�B	!�B	�B	VB		7B	B��B�B�mB�ZB�HB�5B�B��B��BƨB�}B�dB�dB�RB�FB�?B�-B�!B�B��B��B��B��B��B��B��B��B�{B��B�uB�hB�bB�JB�=B�+B�B�B�B�1B�DB�=B�=B�1B�%B�+B�DB�DB�JB�PB�VB�VB�PB�\B�hB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�?B�LB�^B�wB��BƨBǮB��B��B�#B�#B�)B�/B�BB�HB�TB�ZB�`B�`B�fB�sB�B�B�B��B��B��B	B	B	%B	JB	oB	�B	�B	�B	�B	 �B	"�B	%�B	)�B	0!B	.B	$�B	%�B	)�B	'�B	+B	2-B	B�B	I�B	I�B	I�B	O�B	S�B	W
B	ZB	]/B	`BB	aHB	bNB	cTB	dZB	cTB	cTB	dZB	ffB	l�B	p�B	q�B	r�B	t�B	v�B	w�B	x�B	x�B	y�B	z�B	{�B	{�B	{�B	|�B	|�B	~�B	�B	�B	�%B	�+B	�=B	�bB	��B	��B	�B	�B	�B	�!B	�9B	�RB	�RB	�XB	�XB	�dB	�}B	��B	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�NB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
1B
1B
1B
	7B

=B

=B

=B

=B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
PB
PB
PB
PB
VB
VB
\B
\B
bB
bB
bB
bB
hB
hB
hB
hB
uB
oB
{B
,B
'�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�)B
�)B
�)B
�#B
�#B
��B
��B
�HB
�B
��B
��B
��B
��B
��B  B  BBBBB1B
=BPB
=B	7BbB1'B,B6FBC�B^5Be`B|�B�B� B~�B{�Bx�B�B�PB��B�XBÖB�)B��B�B2-BG�BL�BXBaHBdZBcTBk�BgmBffB]/B\)BZB^5BiyBz�B� B}�B|�B|�B�oB��B��B��B��B�\Bz�BM�B6FB-B?}B?}BE�BH�BI�BG�BT�BbNBS�BXBZBZBcTB[#BM�BB�BhB�B��BB��B�sB��B�HB��By�Bk�Br�Bk�BO�B(�B
��B
��B
��B
��B
��B
�PB
q�B
XB
�B	��B	��B	�B	m�B	e`B	bNB	`BB	M�B	6FB	,B	.B	,B	&�B	!�B	�B	VB		7B	B��B�B�mB�ZB�HB�5B�B��B��BƨB�}B�dB�dB�RB�FB�?B�-B�!B�B��B��B��B��B��B��B��B��B�{B��B�uB�hB�bB�JB�=B�+B�B�B�B�1B�DB�=B�=B�1B�%B�+B�DB�DB�JB�PB�VB�VB�PB�\B�hB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�?B�LB�^B�wB��BƨBǮB��B��B�#B�#B�)B�/B�BB�HB�TB�ZB�`B�`B�fB�sB�B�B�B��B��B��B	B	B	%B	JB	oB	�B	�B	�B	�B	 �B	"�B	%�B	)�B	0!B	.B	$�B	%�B	)�B	'�B	+B	2-B	B�B	I�B	I�B	I�B	O�B	S�B	W
B	ZB	]/B	`BB	aHB	bNB	cTB	dZB	cTB	cTB	dZB	ffB	l�B	p�B	q�B	r�B	t�B	v�B	w�B	x�B	x�B	y�B	z�B	{�B	{�B	{�B	|�B	|�B	~�B	�B	�B	�%B	�+B	�=B	�bB	��B	��B	�B	�B	�B	�!B	�9B	�RB	�RB	�XB	�XB	�dB	�}B	��B	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�NB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
%B
1B
1B
1B
	7B

=B

=B

=B

=B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
PB
PB
PB
PB
VB
VB
\B
\B
bB
bB
bB
bB
hB
hB
hB
hB
uB
oB
{B
,B
'�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140810                              AO  ARCAADJP                                                                    20181024140810    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140810  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140810  QCF$                G�O�G�O�G�O�0               