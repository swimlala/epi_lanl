CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-02-19T11:01:18Z creation      
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
resolution        =���   axis      Z        P  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  o   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20200219110118  20200219110118  5906156 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  7912                            2B  A   NAVIS_A                         1020                            170425                          863 @�����1   @�>���@7��`A�7�c��
=p�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�<�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��A�
A;�
A[�
A{�
A��A��A��A��A��A��A��A��B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C��C�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�C)�qC+�qC-�qC/�qC1�qC3�C5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC�޸C�޸C�޸C���C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C���C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸D o\D �\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D	o\D	�\D
o\D
�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D o\D �\D!o\D!�\D"o\D"�\D#o\D#�\D$o\D$�\D%o\D%�\D&o\D&�\D'o\D'�\D(o\D(�\D)o\D)�\D*o\D*�\D+o\D+�\D,o\D,�\D-o\D-�\D.o\D.�\D/o\D/�\D0o\D0�\D1o\D1�\D2o\D2�\D3o\D3�\D4o\D4�\D5o\D5�\D6o\D6�\D7o\D7�\D8o\D8�\D9o\D9�\D:o\D:�\D;o\D;�\D<o\D<�\D=o\D=�\D>o\D>�\D?o\D?�\D@o\D@�\DAo\DA�\DBo\DB�\DCo\DC�\DDo\DD�\DEo\DE�\DFo\DF�\DGo\DG�\DHo\DH�\DIo\DI�\DJo\DJ�\DKo\DK�\DLo\DL�\DMo\DM�\DNo\DN�\DOo\DO�\DPo\DP�\DQo\DQ�\DRo\DR�\DSo\DS�\DTo\DT�\DUo\DU�\DVo\DV�\DWo\DW�\DXo\DX�\DYo\DY�\DZo\DZ�\D[o\D[�\D\o\D\�\D]o\D]�\D^o\D^�\D_o\D_�\D`o\D`�\Dao\Da�\Dbo\Db�\Dco\Dc�\Ddo\Dd�\Deo\De�\Dfo\Df�\Dgo\Dg�\Dho\Dh�\Dio\Di�\Djo\Dj�\Dko\Dk�\Dlo\Dl�\Dmo\Dm�\Dno\Dn�\Doo\Do�\Dpo\Dp�\Dqo\Dq�\Dro\Dr�\Dso\Ds�\Dto\Dt�\Duo\Du�\Dvo\Dv�\Dwo\Dw�\Dxo\Dx�\Dyo\Dy�\Dzo\Dz�\D{o\D{�\D|o\D|�\D}o\D}�\D~o\D~�\Do\D�\D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�t{D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D·�D���D�7�D�w�D÷�D���D�7�D�w�Dķ�D���D�7�D�w�Dŷ�D���D�7�D�w�DƷ�D���D�7�D�w�DǷ�D���D�7�D�w�Dȷ�D���D�7�D�w�Dɷ�D���D�7�D�w�Dʷ�D���D�7�D�w�D˷�D���D�7�D�w�D̷�D���D�7�D�w�Dͷ�D���D�7�D�w�Dη�D���D�7�D�w�DϷ�D���D�7�D�w�Dз�D���D�7�D�w�Dѷ�D���D�7�D�w�Dҷ�D���D�7�D�w�Dӷ�D���D�7�D�w�DԷ�D���D�7�D�w�Dշ�D���D�7�D�w�Dַ�D���D�7�D�w�D׷�D���D�7�D�w�Dط�D���D�7�D�w�Dٷ�D���D�7�D�w�Dڷ�D���D�7�D�w�D۷�D���D�7�D�w�Dܷ�D���D�7�D�w�Dݷ�D���D�7�D�w�D޷�D���D�7�D�w�D߷�D���D�7�D�w�D෮D���D�7�D�w�DᷮD���D�7�D�w�DⷮD���D�7�D�w�D㷮D���D�7�D�w�D䷮D���D�7�D�w�D差D���D�7�D�w�D淮D���D�7�D�w�D緮D���D�7�D�w�D跮D���D�7�D�w�D鷮D���D�7�D�w�D귮D���D�7�D�w�D뷮D���D�7�D�w�D췮D���D�7�D�w�D���D���D�7�D�w�DD���D�7�D�w�D﷮D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D���D���D�4{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1A�1A�JA�bA�VA�bA��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A�1A���A��/A��A���A���A�~�A�^5A�K�A�G�A�C�A�?}A�?}A�=qA�;dA�7LA�$�A�v�A�p�A�/A���A�ZA���A�|�A���A���A�z�A��^A�dZA�ĜA��!A��FA���A���A�33A���A�ffA�%A���A�p�A���A�+A���A���A�A�S�A��FA�ffA�1A�ƨA�=qA��RA�  A���A�Q�A�9XA�=qA�&�A�~�A��A�|�A�`BA�?}A�9XA�VA���A��-A�O�A�JA�dZA��A��A�S�A�G�A�M�A���A��yA�bA���A�~�A�{A��A`BA}�7Az  Av��As�#Ao�-Ao
=AnffAmG�Ak?}Ah�Ad�jAb�jAa�AaAa?}A`��A`bNA_��A_�hA^ĜA]p�A\��A[�A[��A[t�AZ�!AY�^AY%AVjAT�jAShsAQt�AP��AP�!APffAO�mAN�HAM+AKS�AJ-AH�9AGt�AE�
AD�ADM�AA�;A?��A>5?A<��A<E�A;hsA8ĜA7�hA6��A4��A2��A2�A1S�A0��A0A�A/�;A/;dA.ĜA.�A+�A*�RA* �A)��A)\)A(��A(^5A&��A$�\A#�A#A#��A#C�A#"�A"ȴA!hsAA~�AjAA�A�RAt�A1A��AbNAAz�A�
A"�A�AQ�AJA�AĜAn�A(�A�-A&�AA�A?}A	�FA	S�A	
=A��AbNA�mA��A`BA"�A�yAffA�!AE�A{A�#A �H@�l�@�@��@�bN@��
@�ff@�bN@��@�^5@��@��@���@�@�r�@��@�-@���@홚@웦@�C�@ꗍ@�hs@��m@��y@�ff@��@�/@�9@� �@㝲@�33@�V@�7@�G�@��u@ߝ�@��@���@��@��#@�/@�|�@�-@�?}@�dZ@�V@�  @�"�@�/@�bN@�(�@�@ɩ�@�|�@���@Ƈ+@�{@��@��#@�p�@��@�%@ģ�@ēu@���@��@��@��/@�C�@�/@�|�@���@��@��T@�7L@��@��j@�z�@�-@�/@�bN@�S�@���@�v�@��@��h@��@��D@�Z@�1'@�(�@��
@�;d@��@��T@�hs@�r�@�;d@�"�@�l�@�ȴ@�-@�ff@�~�@�hs@�V@�E�@��@�dZ@�7L@��@�j@�A�@�Q�@�(�@��w@�ff@�E�@�-@�{@��#@�O�@���@�(�@�ƨ@��P@�o@��y@���@�5?@��@��-@��`@�z�@�(�@�t�@�@���@�=q@�E�@�ȴ@�~�@�~�@�ff@�J@�@��j@�bN@�  @���@�|�@�l�@�|�@��w@��;@��P@�S�@��H@���@���@��!@�ff@��@�@���@�Ĝ@�@�ȴ@�K�@�ƨ@�b@���@�1@��@�"�@� �@���@���@�ƨ@���@�bN@�  @�b@�z�@��D@�z�@��@��@��9@��`@��`@��`@��`@��@�1'@�1'@�K�@�-@�@�/@�hs@�J@�  @��D@�@�C�@��P@�33@���@�M�@�$�@�J@���@�V@�bN@���@��@���@��@�V@���@�ƨ@�  @�\)@�Q�@���@�@~�@~�y@~@�r�@��@���@~E�@|9X@{ƨ@{dZ@{o@z��@yx�@x�u@w��@w|�@w;d@u�@vff@wl�@w�P@w�P@w|�@wl�@w\)@v��@u��@u�-@u�h@up�@uV@tj@t1@s�m@s��@s��@s�@s@r�\@r=q@q�#@qG�@s@t�/@s�m@p�`@qG�@s�@u?}@t�D@tZ@s@r�@r��@rM�@qG�@o��@n5?@n$�@n5?@nff@nE�@n��@o�@n��@m`B@j��@i�@i��@ix�@hQ�@g�@g|�@g�P@h1'@i%@i%@g\)@f�@f��@f��@g�w@g;d@g;d@gK�@g\)@g\)@g\)@g+@g;d@gK�@g\)@g;d@f��@f�+@f{@e�@e?}@eV@d��@d(�@c��@c"�@b��@bn�@a�@a��@ax�@aG�@aG�@`��@`A�@`A�@` �@_�w@_�P@_|�@_l�@_;d@_+@^�y@^ȴ@^V@^$�@^5?@^V@^{@]�-@]�@]O�@\��@\��@[��@Z�@Zn�@ZJ@Y�#@Y�7@YG�@Y�@X�`@X��@Xr�@X1'@W�@W�w@Wl�@Wl�@Wl�@Wl�@W\)@W;d@W+@V�@Vȴ@V��@V{@U�T@U�-@U`B@T�/@T��@T�@Tj@S��@S"�@R��@Rn�@RM�@RJ@Q��@Q��@Q��@Q�#@Qx�@P�9@PbN@P �@O�w@O|�@O;d@N�@NV@N@M�h@Mp�@M?}@L�@L�@KdZ@J�H@J�\@I��@IG�@HbN@H �@H  @G�@G\)@G+@F�y@F�R@Fv�@FV@F$�@E/@DI�@Ct�@C33@B�!@B-@Ax�@@��@@�9@@�u@@r�@@bN@@Q�@@b@?��@?K�@>�@>v�@>ff@>V@>5?@>$�@>{@>@=��@=��@=�@=p�@=p�@=O�@=V@<��@<��@<�@<Z@<1@;��@;��@;��@;�m@;ƨ@;ƨ@;�F@;�F@;��@;S�@:��@9�^@9G�@97L@8�`@8Q�@7�@7��@7��@7��@7��@7�P@7\)@7K�@7;d@7�@6�y@6��@6v�@6V@6V@6E�@65?@6$�@5�@5`B@4��@4j@4Z@49X@4�@41@3�
@3��@3S�@3@2��@2~�@2�@1�^@1&�@0��@0Ĝ@0Ĝ@0�9@0�@0r�@0Q�@0A�@0  @/�@/�P@/�P@/�P@/�P@/��@/�@/�@/�@.ȴ@.�R@.��@.v�@.ff@.V@.$�@-@-?}@,��@,z�@,9X@+ƨ@+��@+��@+��@+��@+��@+S�@+S�@+C�@*��@*�!@*�!@*�!@*��@*~�@*=q@*-@)�@)X@)�@(��@( �@'�@'�@'�;@'�w@'K�@&��@&�R@&��@&�+@&5?@&{@%�@%�-@%?}@$��@$��@$�j@$��@$9X@#�@#o@"�@"��@"n�@"-@!�#@!��@!x�@!X@!X@!7L@ ��@ ��@ r�@ A�@ 1'@ b@   @�;@�w@|�@�@V@5?@5?@�T@�h@V@�@��@�D@j@I�@�@�
@�@C�@33@o@�H@��@�\@^5@�@��@�7@hs@G�@&�@��@�@1'@�;@;d@�@��@�+@ff@E�@$�@@�@��@@��@�h@�@p�@O�@O�@O�@V@�/@�j@��@z�@�m@��@t�@33@@��@�\@M�@�@�@�#@��@�7@G�@7L@7L@&�@�@��@�@ �@��@��@��@|�@K�@+@�@��@�@v�@E�@5?@5?@$�@�T@�h@�@O�@?}@/@V@�/@�j@Z@9X@(�@(�@��@�m@ƨ@��@��@�@�@t�@C�@33@o@
�@
�@
�@
��@
n�@
=q@	��@	�#@	��@	�^@	�7@	hs@	G�@	%@�`@�9@�u@�u@r�@  @�w@�P@|�@l�@K�@+@
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1A�1A�JA�bA�VA�bA��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A�1A���A��/A��A���A���A�~�A�^5A�K�A�G�A�C�A�?}A�?}A�=qA�;dA�7LA�$�A�v�A�p�A�/A���A�ZA���A�|�A���A���A�z�A��^A�dZA�ĜA��!A��FA���A���A�33A���A�ffA�%A���A�p�A���A�+A���A���A�A�S�A��FA�ffA�1A�ƨA�=qA��RA�  A���A�Q�A�9XA�=qA�&�A�~�A��A�|�A�`BA�?}A�9XA�VA���A��-A�O�A�JA�dZA��A��A�S�A�G�A�M�A���A��yA�bA���A�~�A�{A��A`BA}�7Az  Av��As�#Ao�-Ao
=AnffAmG�Ak?}Ah�Ad�jAb�jAa�AaAa?}A`��A`bNA_��A_�hA^ĜA]p�A\��A[�A[��A[t�AZ�!AY�^AY%AVjAT�jAShsAQt�AP��AP�!APffAO�mAN�HAM+AKS�AJ-AH�9AGt�AE�
AD�ADM�AA�;A?��A>5?A<��A<E�A;hsA8ĜA7�hA6��A4��A2��A2�A1S�A0��A0A�A/�;A/;dA.ĜA.�A+�A*�RA* �A)��A)\)A(��A(^5A&��A$�\A#�A#A#��A#C�A#"�A"ȴA!hsAA~�AjAA�A�RAt�A1A��AbNAAz�A�
A"�A�AQ�AJA�AĜAn�A(�A�-A&�AA�A?}A	�FA	S�A	
=A��AbNA�mA��A`BA"�A�yAffA�!AE�A{A�#A �H@�l�@�@��@�bN@��
@�ff@�bN@��@�^5@��@��@���@�@�r�@��@�-@���@홚@웦@�C�@ꗍ@�hs@��m@��y@�ff@��@�/@�9@� �@㝲@�33@�V@�7@�G�@��u@ߝ�@��@���@��@��#@�/@�|�@�-@�?}@�dZ@�V@�  @�"�@�/@�bN@�(�@�@ɩ�@�|�@���@Ƈ+@�{@��@��#@�p�@��@�%@ģ�@ēu@���@��@��@��/@�C�@�/@�|�@���@��@��T@�7L@��@��j@�z�@�-@�/@�bN@�S�@���@�v�@��@��h@��@��D@�Z@�1'@�(�@��
@�;d@��@��T@�hs@�r�@�;d@�"�@�l�@�ȴ@�-@�ff@�~�@�hs@�V@�E�@��@�dZ@�7L@��@�j@�A�@�Q�@�(�@��w@�ff@�E�@�-@�{@��#@�O�@���@�(�@�ƨ@��P@�o@��y@���@�5?@��@��-@��`@�z�@�(�@�t�@�@���@�=q@�E�@�ȴ@�~�@�~�@�ff@�J@�@��j@�bN@�  @���@�|�@�l�@�|�@��w@��;@��P@�S�@��H@���@���@��!@�ff@��@�@���@�Ĝ@�@�ȴ@�K�@�ƨ@�b@���@�1@��@�"�@� �@���@���@�ƨ@���@�bN@�  @�b@�z�@��D@�z�@��@��@��9@��`@��`@��`@��`@��@�1'@�1'@�K�@�-@�@�/@�hs@�J@�  @��D@�@�C�@��P@�33@���@�M�@�$�@�J@���@�V@�bN@���@��@���@��@�V@���@�ƨ@�  @�\)@�Q�@���@�@~�@~�y@~@�r�@��@���@~E�@|9X@{ƨ@{dZ@{o@z��@yx�@x�u@w��@w|�@w;d@u�@vff@wl�@w�P@w�P@w|�@wl�@w\)@v��@u��@u�-@u�h@up�@uV@tj@t1@s�m@s��@s��@s�@s@r�\@r=q@q�#@qG�@s@t�/@s�m@p�`@qG�@s�@u?}@t�D@tZ@s@r�@r��@rM�@qG�@o��@n5?@n$�@n5?@nff@nE�@n��@o�@n��@m`B@j��@i�@i��@ix�@hQ�@g�@g|�@g�P@h1'@i%@i%@g\)@f�@f��@f��@g�w@g;d@g;d@gK�@g\)@g\)@g\)@g+@g;d@gK�@g\)@g;d@f��@f�+@f{@e�@e?}@eV@d��@d(�@c��@c"�@b��@bn�@a�@a��@ax�@aG�@aG�@`��@`A�@`A�@` �@_�w@_�P@_|�@_l�@_;d@_+@^�y@^ȴ@^V@^$�@^5?@^V@^{@]�-@]�@]O�@\��@\��@[��@Z�@Zn�@ZJ@Y�#@Y�7@YG�@Y�@X�`@X��@Xr�@X1'@W�@W�w@Wl�@Wl�@Wl�@Wl�@W\)@W;d@W+@V�@Vȴ@V��@V{@U�T@U�-@U`B@T�/@T��@T�@Tj@S��@S"�@R��@Rn�@RM�@RJ@Q��@Q��@Q��@Q�#@Qx�@P�9@PbN@P �@O�w@O|�@O;d@N�@NV@N@M�h@Mp�@M?}@L�@L�@KdZ@J�H@J�\@I��@IG�@HbN@H �@H  @G�@G\)@G+@F�y@F�R@Fv�@FV@F$�@E/@DI�@Ct�@C33@B�!@B-@Ax�@@��@@�9@@�u@@r�@@bN@@Q�@@b@?��@?K�@>�@>v�@>ff@>V@>5?@>$�@>{@>@=��@=��@=�@=p�@=p�@=O�@=V@<��@<��@<�@<Z@<1@;��@;��@;��@;�m@;ƨ@;ƨ@;�F@;�F@;��@;S�@:��@9�^@9G�@97L@8�`@8Q�@7�@7��@7��@7��@7��@7�P@7\)@7K�@7;d@7�@6�y@6��@6v�@6V@6V@6E�@65?@6$�@5�@5`B@4��@4j@4Z@49X@4�@41@3�
@3��@3S�@3@2��@2~�@2�@1�^@1&�@0��@0Ĝ@0Ĝ@0�9@0�@0r�@0Q�@0A�@0  @/�@/�P@/�P@/�P@/�P@/��@/�@/�@/�@.ȴ@.�R@.��@.v�@.ff@.V@.$�@-@-?}@,��@,z�@,9X@+ƨ@+��@+��@+��@+��@+��@+S�@+S�@+C�@*��@*�!@*�!@*�!@*��@*~�@*=q@*-@)�@)X@)�@(��@( �@'�@'�@'�;@'�w@'K�@&��@&�R@&��@&�+@&5?@&{@%�@%�-@%?}@$��@$��@$�j@$��@$9X@#�@#o@"�@"��@"n�@"-@!�#@!��@!x�@!X@!X@!7L@ ��@ ��@ r�@ A�@ 1'@ b@   @�;@�w@|�@�@V@5?@5?@�T@�h@V@�@��@�D@j@I�@�@�
@�@C�@33@o@�H@��@�\@^5@�@��@�7@hs@G�@&�@��@�@1'@�;@;d@�@��@�+@ff@E�@$�@@�@��@@��@�h@�@p�@O�@O�@O�@V@�/@�j@��@z�@�m@��@t�@33@@��@�\@M�@�@�@�#@��@�7@G�@7L@7L@&�@�@��@�@ �@��@��@��@|�@K�@+@�@��@�@v�@E�@5?@5?@$�@�T@�h@�@O�@?}@/@V@�/@�j@Z@9X@(�@(�@��@�m@ƨ@��@��@�@�@t�@C�@33@o@
�@
�@
�@
��@
n�@
=q@	��@	�#@	��@	�^@	�7@	hs@	G�@	%@�`@�9@�u@�u@r�@  @�w@�P@|�@l�@K�@+@
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bm�Bl�Bm�Bn�Bp�Bq�Br�Br�Bt�Bv�Bx�By�By�By�Bx�Bx�By�Bx�Bx�By�B��B�HB��B
=BbB�B�B"�B'�B&�B(�B)�B,B5?BD�BG�BF�BE�BE�BF�BG�BH�BH�BI�BH�BR�BS�BQ�BI�BE�B?}B;dB9XB49B/B%�B"�B�B�B�B��B�B�`B�/B��B�dB�XB�FB�B�bB� B_;BS�B>wB+B!�BhB
��B
�B
�FB
��B
��B
��B
�{B
�B
r�B
ffB
VB
=qB
'�B
B	��B	��B	�B	�BB	��B	�^B	�B	��B	��B	��B	��B	��B	��B	��B	�oB	�+B	�B	}�B	y�B	w�B	t�B	l�B	gmB	]/B	M�B	G�B	>wB	8RB	8RB	8RB	:^B	5?B	/B	#�B	�B	�B	bB	+B��B��B�B�TB�B��B��B��B�qB�XB�?B�!B��B��B��B��B��B��B��B��B��B��B�\B�PB�DB�=B�1B�B�B{�Bw�Bx�Bx�Bw�Bu�Bu�Bq�Bo�Bk�BjBiyBe`B\)BT�BQ�BO�BK�BJ�BO�BO�BO�BL�BK�BL�BL�BK�BK�BJ�BI�BG�BC�BB�B@�B>wB>wB=qB=qB<jB=qB<jB;dB:^B<jB>wB=qB>wB>wB?}B@�BC�BH�BI�BJ�BI�BI�BH�BH�BK�BL�BL�BL�BL�BO�BQ�B_;Bk�Bk�BjBjBiyBk�BiyBhsBhsBiyBk�Bn�Bp�Bq�Br�Br�Bt�Bs�Bs�Bs�Bt�Bw�Bw�Bw�By�By�By�B{�Bz�B}�B|�B� B~�B~�B�B�B�B�B�B�B�B�B�%B�+B�+B�1B�+B�=B�VB�PB�\B�VB�VB�VB�bB�uB��B��B�B�B�B�9B�LB�^B��B��BĜBƨBǮBɺB��B��BɺBɺBɺBɺBȴBǮBŢBB��B��BŢB��B��B��B�B�B�
B�HB�B�B��B��B��B��B��B	B	B	JB	bB	hB	uB	�B	�B	�B	$�B	)�B	+B	2-B	6FB	8RB	=qB	A�B	C�B	J�B	M�B	N�B	N�B	N�B	O�B	P�B	R�B	ZB	`BB	bNB	dZB	ffB	ffB	k�B	m�B	n�B	n�B	n�B	n�B	o�B	q�B	u�B	y�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�B	}�B	}�B	� B	�B	�B	�B	iyB	ffB	gmB	m�B	o�B	s�B	w�B	w�B	~�B	�B	�1B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�LB	��B	ŢB	ƨB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ƨB	ǮB	��B	ɺB	�qB	�'B	�B	�'B	�?B	�FB	ÖB	��B	��B	ȴB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�BB	�fB	�B	�B	�yB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
B
B
%B
%B
%B
%B
1B

=B
JB
PB
VB
\B
\B
bB
bB
bB
hB
hB
oB
oB
oB
uB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
$�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
-B
-B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
<jB
=qB
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
L�B
L�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
Q�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
S�B
S�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
XB
XB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
]/B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
gmB
gmB
hsB
hsB
iyB
iyB
jB
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bm�Bl�Bm�Bn�Bp�Bq�Br�Br�Bt�Bv�Bx�By�By�By�Bx�Bx�By�Bx�Bx�By�B��B�HB��B
=BbB�B�B"�B'�B&�B(�B)�B,B5?BD�BG�BF�BE�BE�BF�BG�BH�BH�BI�BH�BR�BS�BQ�BI�BE�B?}B;dB9XB49B/B%�B"�B�B�B�B��B�B�`B�/B��B�dB�XB�FB�B�bB� B_;BS�B>wB+B!�BhB
��B
�B
�FB
��B
��B
��B
�{B
�B
r�B
ffB
VB
=qB
'�B
B	��B	��B	�B	�BB	��B	�^B	�B	��B	��B	��B	��B	��B	��B	��B	�oB	�+B	�B	}�B	y�B	w�B	t�B	l�B	gmB	]/B	M�B	G�B	>wB	8RB	8RB	8RB	:^B	5?B	/B	#�B	�B	�B	bB	+B��B��B�B�TB�B��B��B��B�qB�XB�?B�!B��B��B��B��B��B��B��B��B��B��B�\B�PB�DB�=B�1B�B�B{�Bw�Bx�Bx�Bw�Bu�Bu�Bq�Bo�Bk�BjBiyBe`B\)BT�BQ�BO�BK�BJ�BO�BO�BO�BL�BK�BL�BL�BK�BK�BJ�BI�BG�BC�BB�B@�B>wB>wB=qB=qB<jB=qB<jB;dB:^B<jB>wB=qB>wB>wB?}B@�BC�BH�BI�BJ�BI�BI�BH�BH�BK�BL�BL�BL�BL�BO�BQ�B_;Bk�Bk�BjBjBiyBk�BiyBhsBhsBiyBk�Bn�Bp�Bq�Br�Br�Bt�Bs�Bs�Bs�Bt�Bw�Bw�Bw�By�By�By�B{�Bz�B}�B|�B� B~�B~�B�B�B�B�B�B�B�B�B�%B�+B�+B�1B�+B�=B�VB�PB�\B�VB�VB�VB�bB�uB��B��B�B�B�B�9B�LB�^B��B��BĜBƨBǮBɺB��B��BɺBɺBɺBɺBȴBǮBŢBB��B��BŢB��B��B��B�B�B�
B�HB�B�B��B��B��B��B��B	B	B	JB	bB	hB	uB	�B	�B	�B	$�B	)�B	+B	2-B	6FB	8RB	=qB	A�B	C�B	J�B	M�B	N�B	N�B	N�B	O�B	P�B	R�B	ZB	`BB	bNB	dZB	ffB	ffB	k�B	m�B	n�B	n�B	n�B	n�B	o�B	q�B	u�B	y�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�B	}�B	}�B	� B	�B	�B	�B	iyB	ffB	gmB	m�B	o�B	s�B	w�B	w�B	~�B	�B	�1B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�LB	��B	ŢB	ƨB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ƨB	ǮB	��B	ɺB	�qB	�'B	�B	�'B	�?B	�FB	ÖB	��B	��B	ȴB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�BB	�fB	�B	�B	�yB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
B
B
%B
%B
%B
%B
1B

=B
JB
PB
VB
\B
\B
bB
bB
bB
hB
hB
oB
oB
oB
uB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
$�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
-B
-B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
<jB
=qB
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
L�B
L�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
Q�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
S�B
S�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
XB
XB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
]/B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
gmB
gmB
hsB
hsB
iyB
iyB
jB
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20200219110118                              AO  ARCAADJP                                                                    20200219110118    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200219110118  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200219110118  QCF$                G�O�G�O�G�O�0               