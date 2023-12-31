CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-02-11T18:41:56Z creation;2023-02-11T18:41:57Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230211184156  20230211185919  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               zA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @�#b:g�1   @�$E�=@;?|�hs�c�~��"�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @���A   A!��A@  A^ffA~ffA�  A�  A�33A�  A�  A���A���B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC�fC�fC  C  C�C  C  C   C!�fC$  C&  C(  C)�fC+�fC-�fC/�fC2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��3C�  C��C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��3C�  C��C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C��C�  C��3C�  C�  C��C��C�  C�  C��C��C��C�  C��3C��3C��3C�  C�  C��C�  C��3C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C��C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C��C��C��D   D � D  D� D��D� D  Dy�D  D� D��D� D  Dy�D  D� D��Dy�D	  D	�fD
  D
� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  Dy�D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-fD-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DH��DIy�DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DR��DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� DhfDh� Dh��Diy�Di��Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dy��Dz� D{  D{� D|fD|�fD}fD}� D~  D~� D  D� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�|�D���D�  D�@ D��3D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�C3D��3D�� D�  D�C3D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D���D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ D�|�Dʼ�D�  D�@ Dˀ D��3D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dσ3D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�<�D�|�D�� D�  D�@ DӀ D��3D�  D�@ DԀ D�� D�3D�@ D�|�D�� D�  D�@ Dր D�� D�  D�@ D׀ D��3D�  D�@ D؃3D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۃ3D��3D�  D�@ D܀ D�� D�  D�@ D݃3D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D��D���D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D��3D��3D�  D�#3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@\)@�z�@��A!p�A?�
A^=pA~=pA��A��A��A��A��A�RA�RA��B��B��B\)B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bx\)B��B���B���B���B���B���B���B���B�.B���B���B�.B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC��C��C��C�qC�qCC�qC�qC�qC!��C#�qC%�qC'�qC)��C+��C-��C/��C1�qC3�qC5�qC7�qC9�qC;�qC>C?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCPCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C��C���C���C��C���C���C���C���C��C���C���C���C���C��C���C���C��C���C���C��C���C���C���C���C���C��C���C���C���C���C��C��C���C���C��C��C��C���C���C���C���C���C���C��C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C��C���C���C��C���C���C���C���C��C���C���C���C���C���C���C��C��C��C��C��C���D \D �\D\D��D\D�\Dx�D�\D\D��D\D�\Dx�D�\D\D��Dx�D�\D	��D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D��D�\D\D�\D\D�\Dx�D�\D \D �\D!\D!�\D"\D"�\D#x�D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,��D-�D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1��D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH��DIx�DI�\DJ\DJ�\DK\DK�\DL��DL�\DM\DM�\DN\DN�\DO\DP�DP\DP�\DQ\DQ�\DR\DR��DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dh�Dh\Dh��Dix�Di��Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy��Dz\Dz�\D{\D|�D|��D}�D}\D}�\D~\D~�\D\D�\D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D��{D�?�D��D���D��D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��{D�<{D�|{D��{D���D�?�D���D���D���D�?�D��D���D��D�?�D��D���D���D�?�D��D���D���D�?�D�|{D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D���D�<{D�|{D���D���D�B�D���D���D���D�B�D��D���D��{D�<{D��D���D���D�?�D��D���D��{D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�?�D��D��{D��{D�?�D��D���D���D�?�D��D���D���D�<{D��D���D���D�?�D�|{D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��{D�?�D��D���D��{D�<{D��D���D���D�?�D��D���D���D�?�D��D���D��D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D��D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D�|{D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�?�D��D���D��{D�?�D��D���D���D�?�D��D���D���D�B�D��D��{D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D�|{Dʼ{D���D�?�D��D���D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�Dς�DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�<{D�|{Dҿ�D���D�?�D��D���D���D�?�D��DԿ�D��D�?�D�|{Dտ�D���D�?�D��Dֿ�D���D�?�D��D���D���D�?�D؂�Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�Dۂ�D���D���D�?�D��Dܿ�D���D�?�D݂�Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D�|{D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�B�D��D꿮D���D�?�D��D�{D��{D�?�D��D쿮D��{D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�<{D��D���D���D�?�D��D���D���D�<{D��D���D���D�?�D���D���D���D�"�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A���A���A��GA���A��A��MA���A��A��A��A�oA��A�
�A�PA�A�PA�"A�"A�	7A�%A��A��A�"hA�|�A�l"A���A��zA���A���A�YA�.A��A��A���A�� A�v+A�tTA�n/A�gmA�a|A�X�A�QNA�IRA�+A��A��A�qA��A�qA��A��	A�� A���A�4�A�!A�A���A�4�A�#�A���A��A��A�ĜA��6A��A��6A��\A�|PA�_A�h�A��A���A�~�A��A��A�g�A�8�A�/�A��A��[A��A��mA��wA�x8A��pA�<6A���A��A��TA�_pA���A�l"A�A�@�A���A�d&A��A��HA�_;A{`�Au��AqAoVAna|Am.�AlOAk��AkO�Aj�Ajs�AjJ#Ai��Ain/Ag�qAf�4Ae�$AcԕAc	�Ab��Aa�<A_��A\c�AZ��AX�AWA�AV��AV2�AU�&AT�AS�AS��AR�AQ��AP��AO��AN��AN*�ALAIںAHS&AG�AG�@AGq�AGMjAG/�AF��AE�AD�=ACA�AB�AB�NAB��AB!-AA8�A@�MA@�}A@O�A?�TA?��A?�A>��A=ƨA=�+A=XA=E�A=�A<qA:��A:`�A:�A8ԕA8'�A6_�A5}VA50�A4_A1�A0q�A/�nA.�A.CA-��A-�HA-z�A,��A,��A,��A,5?A+�}A+�1A*��A)�rA'ߤA&��A&qvA&X�A%��A$u�A"�A!�A�/ArGA��A	A��A��Af�A��A��A}VA�A�qA�gA�FA�A[�A,=A�A��A�A��A�FA��A
��A
OvA	ȴA	6�A��A�AȴAs�A��A��A��Ao�As�AA�AZ�A�eAیAK^A�A��A6zA ��A `�@���@�l�@���@��	@���@��K@��@�2a@�)_@��@��@���@��H@�:*@��A@��@�s�@�0�@�{�@�8�@��@�iD@��M@�M@軙@�Ft@�O@��@�w2@�:�@�^�@�/�@܈�@��#@�֡@� \@ףn@ֵ�@��@Ԫe@���@���@�2�@�qv@�ԕ@��@��|@��K@��@Ȼ�@�1'@�<6@ƈ�@�#:@ţn@�>�@��y@�ԕ@�/�@��]@�e�@�9�@��@�ں@���@���@��.@�!@��g@�y�@��s@�Mj@��X@�m]@�ѷ@���@�@�@��@��@���@�U�@��@�2�@��o@���@�)�@���@�7L@��6@��D@�C�@���@�@���@�e,@��@��z@���@��9@��@�s�@���@�^�@�&�@���@�m�@�Q�@�G@���@��z@�j�@�"h@�/@��j@�
�@�4@�Z�@� i@��@�W�@���@�@��^@���@���@��H@���@�s�@��f@��+@�b@��a@��6@�$�@�@�{J@�G@�iD@��@� \@��@���@��O@�	@���@�J�@��@���@���@�
=@�@���@���@���@��e@���@��.@�p;@�J�@��}@���@�Y�@�� @�z@��Y@��@���@���@�A @��@���@�ی@���@���@�$�@���@�n/@�X�@�N<@�:�@�/@�V@��2@���@�q@�-�@�y�@���@���@�=q@���@�Mj@�F�@�9�@�&�@��@�xl@�PH@�@���@��M@���@�b�@�O@��@���@�:*@��@��H@�X�@� \@��@��v@���@�n�@�&�@�\�@��@��6@���@�u�@�I�@�:*@�&�@��@�u�@��L@�	@~��@~d�@~0U@~�@}�j@}�=@}`B@}q@|��@{��@{�@{n/@{@z�<@z�x@y��@y%F@xbN@w�W@w��@w��@ws@wX�@w'�@v��@v�<@v�b@v�F@vc @v1�@u��@u��@t��@t�D@t�D@t�o@s@O@r�<@r:*@q��@q��@q��@q\�@q0�@q \@q�@pl"@ph�@pe�@p6@p>B@p@o�@n�2@m�@mL�@l��@lq@k�6@j�6@j
�@i4@h�/@h<�@g8@f	@e�N@e�z@e��@e�t@e��@e�X@e�S@e�"@ec@eo @eY�@e+@dɆ@d�.@d$@c�&@c�V@cdZ@cH�@c1�@c!-@cY@b�8@b҉@b��@b�1@b3�@a�#@`�@_�@_.I@^҉@^� @^c @^$�@]�H@]hs@];@\�9@\�@\�@\�@[�Q@[�F@[��@[�4@[qv@[\)@[K�@[4�@[6z@[8@['�@[Y@[(@[�@Z�@Z�@Z҉@Zȴ@Z�6@Zd�@ZJ@Y��@Y��@Y��@Yf�@Y(�@X�D@W�@V�<@U|@T֡@Toi@T>B@T,=@T	�@S�;@S��@S@O@RJ@Q^�@P�`@P�9@Pl"@P6@O��@O��@O|�@N�@N��@NZ�@N6�@M�.@M�-@M4@L��@K��@Ip�@H�$@HbN@H	�@G�w@G��@Gy�@G��@G~�@Gn/@G_p@GE9@G@O@G.I@G&@G�@G@F�8@F�@F~�@Fd�@F@�@E�Z@D�_@C��@C�{@Cs@CdZ@CRT@C@O@C,�@CY@B��@B�b@Bq�@BGE@B�@A��@A@A�-@A��@A|@Ae,@A \@@�_@?g�@?"�@>��@>�'@>Z�@>�@=��@=�M@=c@=|@=zx@=zx@=x�@=s�@=o @=IR@<�@<��@<r�@</�@;�A@;��@;�q@;g�@;;d@;�@:�X@:�6@:p;@:5?@:	@:@9�@9@8��@8�?@7�]@7��@7=@6�"@6�1@6
�@5ϫ@5��@5o @5Vm@5?}@5@4�v@4��@4<�@4�@3��@3l�@3U�@3Mj@3H�@3&@2�@2�F@2}V@2Q@2�@2J@1�Z@1��@1T�@1-w@1 \@1�@0�@0�@0�@/�V@/�P@/P�@/@O@/8@/8@/.I@.�@.}V@.L0@-��@-O�@-%@,��@+�@+RT@*��@*z@*?@)�j@)j@);@(�K@(��@(�D@'�@'��@'Z�@'4�@'�@'o@'�@&�@&�H@&�X@&�R@&��@&��@&v�@&W�@&8�@&$�@& �@%ϫ@%zx@%X@%B�@%%F@$�@$*�@#�]@#�@#�@#{J@#s@#P�@#@O@#!-@"�]@"q�@!�@!�@!u�@!IR@!A @!:�@!�@!;@ u�@y�@�@�@��@� @#:@_@�T@�X@+@�@��@q@2�@�@@��@�F@��@b�@)_@�c@�<@�+@E�@�@�.@�@�t@o @N<@0�@�E@A�@� @��@e�@9�@ i@��@�\@d�@ �@ԕ@��@k�@2a@�@�f@Ĝ@��@��@tT@Z@:�@M@��@�@��@o�@iD@]�@X�@U�@RT@F�@>�@=@8@/�@+@�@(@��@�+@l�@Z�@L0@@�@1�@#:@��@}�@�@��@�D@w�@>B@,=@�@�@�@�@��@��@t�@b�@l�@C�@@O@C�@H�@F�@J#@O@O@A�@!-@
=@�@�@��@��@�r@n�@p;@xl@xl@z@}V@~�@��@��@��@�\@��@� @� @�r@@�@�j@�H@�@�S@w2@f�@X@4@�	@�	@��@ی@��@��@~(@K^@G@��@t�@A�@6z@&@C@@
�c@
�c@
��@
�@
�y@
�]@
��@
�@
{�@
ff@
E�@
 �@	��@	=�@	+�@	#�@	q@	�@֡@�@6@*�@!@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A���A���A��GA���A��A��MA���A��A��A��A�oA��A�
�A�PA�A�PA�"A�"A�	7A�%A��A��A�"hA�|�A�l"A���A��zA���A���A�YA�.A��A��A���A�� A�v+A�tTA�n/A�gmA�a|A�X�A�QNA�IRA�+A��A��A�qA��A�qA��A��	A�� A���A�4�A�!A�A���A�4�A�#�A���A��A��A�ĜA��6A��A��6A��\A�|PA�_A�h�A��A���A�~�A��A��A�g�A�8�A�/�A��A��[A��A��mA��wA�x8A��pA�<6A���A��A��TA�_pA���A�l"A�A�@�A���A�d&A��A��HA�_;A{`�Au��AqAoVAna|Am.�AlOAk��AkO�Aj�Ajs�AjJ#Ai��Ain/Ag�qAf�4Ae�$AcԕAc	�Ab��Aa�<A_��A\c�AZ��AX�AWA�AV��AV2�AU�&AT�AS�AS��AR�AQ��AP��AO��AN��AN*�ALAIںAHS&AG�AG�@AGq�AGMjAG/�AF��AE�AD�=ACA�AB�AB�NAB��AB!-AA8�A@�MA@�}A@O�A?�TA?��A?�A>��A=ƨA=�+A=XA=E�A=�A<qA:��A:`�A:�A8ԕA8'�A6_�A5}VA50�A4_A1�A0q�A/�nA.�A.CA-��A-�HA-z�A,��A,��A,��A,5?A+�}A+�1A*��A)�rA'ߤA&��A&qvA&X�A%��A$u�A"�A!�A�/ArGA��A	A��A��Af�A��A��A}VA�A�qA�gA�FA�A[�A,=A�A��A�A��A�FA��A
��A
OvA	ȴA	6�A��A�AȴAs�A��A��A��Ao�As�AA�AZ�A�eAیAK^A�A��A6zA ��A `�@���@�l�@���@��	@���@��K@��@�2a@�)_@��@��@���@��H@�:*@��A@��@�s�@�0�@�{�@�8�@��@�iD@��M@�M@軙@�Ft@�O@��@�w2@�:�@�^�@�/�@܈�@��#@�֡@� \@ףn@ֵ�@��@Ԫe@���@���@�2�@�qv@�ԕ@��@��|@��K@��@Ȼ�@�1'@�<6@ƈ�@�#:@ţn@�>�@��y@�ԕ@�/�@��]@�e�@�9�@��@�ں@���@���@��.@�!@��g@�y�@��s@�Mj@��X@�m]@�ѷ@���@�@�@��@��@���@�U�@��@�2�@��o@���@�)�@���@�7L@��6@��D@�C�@���@�@���@�e,@��@��z@���@��9@��@�s�@���@�^�@�&�@���@�m�@�Q�@�G@���@��z@�j�@�"h@�/@��j@�
�@�4@�Z�@� i@��@�W�@���@�@��^@���@���@��H@���@�s�@��f@��+@�b@��a@��6@�$�@�@�{J@�G@�iD@��@� \@��@���@��O@�	@���@�J�@��@���@���@�
=@�@���@���@���@��e@���@��.@�p;@�J�@��}@���@�Y�@�� @�z@��Y@��@���@���@�A @��@���@�ی@���@���@�$�@���@�n/@�X�@�N<@�:�@�/@�V@��2@���@�q@�-�@�y�@���@���@�=q@���@�Mj@�F�@�9�@�&�@��@�xl@�PH@�@���@��M@���@�b�@�O@��@���@�:*@��@��H@�X�@� \@��@��v@���@�n�@�&�@�\�@��@��6@���@�u�@�I�@�:*@�&�@��@�u�@��L@�	@~��@~d�@~0U@~�@}�j@}�=@}`B@}q@|��@{��@{�@{n/@{@z�<@z�x@y��@y%F@xbN@w�W@w��@w��@ws@wX�@w'�@v��@v�<@v�b@v�F@vc @v1�@u��@u��@t��@t�D@t�D@t�o@s@O@r�<@r:*@q��@q��@q��@q\�@q0�@q \@q�@pl"@ph�@pe�@p6@p>B@p@o�@n�2@m�@mL�@l��@lq@k�6@j�6@j
�@i4@h�/@h<�@g8@f	@e�N@e�z@e��@e�t@e��@e�X@e�S@e�"@ec@eo @eY�@e+@dɆ@d�.@d$@c�&@c�V@cdZ@cH�@c1�@c!-@cY@b�8@b҉@b��@b�1@b3�@a�#@`�@_�@_.I@^҉@^� @^c @^$�@]�H@]hs@];@\�9@\�@\�@\�@[�Q@[�F@[��@[�4@[qv@[\)@[K�@[4�@[6z@[8@['�@[Y@[(@[�@Z�@Z�@Z҉@Zȴ@Z�6@Zd�@ZJ@Y��@Y��@Y��@Yf�@Y(�@X�D@W�@V�<@U|@T֡@Toi@T>B@T,=@T	�@S�;@S��@S@O@RJ@Q^�@P�`@P�9@Pl"@P6@O��@O��@O|�@N�@N��@NZ�@N6�@M�.@M�-@M4@L��@K��@Ip�@H�$@HbN@H	�@G�w@G��@Gy�@G��@G~�@Gn/@G_p@GE9@G@O@G.I@G&@G�@G@F�8@F�@F~�@Fd�@F@�@E�Z@D�_@C��@C�{@Cs@CdZ@CRT@C@O@C,�@CY@B��@B�b@Bq�@BGE@B�@A��@A@A�-@A��@A|@Ae,@A \@@�_@?g�@?"�@>��@>�'@>Z�@>�@=��@=�M@=c@=|@=zx@=zx@=x�@=s�@=o @=IR@<�@<��@<r�@</�@;�A@;��@;�q@;g�@;;d@;�@:�X@:�6@:p;@:5?@:	@:@9�@9@8��@8�?@7�]@7��@7=@6�"@6�1@6
�@5ϫ@5��@5o @5Vm@5?}@5@4�v@4��@4<�@4�@3��@3l�@3U�@3Mj@3H�@3&@2�@2�F@2}V@2Q@2�@2J@1�Z@1��@1T�@1-w@1 \@1�@0�@0�@0�@/�V@/�P@/P�@/@O@/8@/8@/.I@.�@.}V@.L0@-��@-O�@-%@,��@+�@+RT@*��@*z@*?@)�j@)j@);@(�K@(��@(�D@'�@'��@'Z�@'4�@'�@'o@'�@&�@&�H@&�X@&�R@&��@&��@&v�@&W�@&8�@&$�@& �@%ϫ@%zx@%X@%B�@%%F@$�@$*�@#�]@#�@#�@#{J@#s@#P�@#@O@#!-@"�]@"q�@!�@!�@!u�@!IR@!A @!:�@!�@!;@ u�@y�@�@�@��@� @#:@_@�T@�X@+@�@��@q@2�@�@@��@�F@��@b�@)_@�c@�<@�+@E�@�@�.@�@�t@o @N<@0�@�E@A�@� @��@e�@9�@ i@��@�\@d�@ �@ԕ@��@k�@2a@�@�f@Ĝ@��@��@tT@Z@:�@M@��@�@��@o�@iD@]�@X�@U�@RT@F�@>�@=@8@/�@+@�@(@��@�+@l�@Z�@L0@@�@1�@#:@��@}�@�@��@�D@w�@>B@,=@�@�@�@�@��@��@t�@b�@l�@C�@@O@C�@H�@F�@J#@O@O@A�@!-@
=@�@�@��@��@�r@n�@p;@xl@xl@z@}V@~�@��@��@��@�\@��@� @� @�r@@�@�j@�H@�@�S@w2@f�@X@4@�	@�	@��@ی@��@��@~(@K^@G@��@t�@A�@6z@&@C@@
�c@
�c@
��@
�@
�y@
�]@
��@
�@
{�@
ff@
E�@
 �@	��@	=�@	+�@	#�@	q@	�@֡@�@6@*�@!@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B*�B*�B*�B*eB*B*�B*�B*B)�B)yB)�B*0B*KB*B*B*�B*B*�B*eB*�B)�B%�B$&B�B.B��B�B�;B�VB��B��B՛B�@BуB�B�(B͟B̈́B��B��B�DB�=BɆB��B�B�+B��B�+B�YB��B�SB��BɺB�YB��B��B��B��B��B�B�XB�=BڠBخB�YB�7B�QB�mB��B�BY�B�LB�B��B�B��B��B��B�#B��B�4B��B}�Bx�Bq�BcB2|B�B��B��B��B��B��Bw2Bk�BVSB@�B&�BjB
��B
ѷB
�0B
�!B
�@B
��B
��B
��B
��B
~]B
{�B
z*B
y$B
vFB
tB
m�B
e,B
`�B
W$B
O�B
L�B
GzB
?�B
/OB
&�B
�B
�B
B
�B
B
	�B
B
B
 �B	��B	��B	��B	�B	�+B	�aB	�WB	�:B	�bB	ߤB	�B	�/B	�=B	�	B	�SB	бB	��B	��B	�B	��B	��B	�gB	�[B	� B	�B	�qB	��B	��B	��B	��B	��B	�tB	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�:B	��B	��B	�AB	}�B	|6B	y	B	w�B	w2B	v`B	t�B	raB	q�B	o�B	m]B	kkB	h�B	cB	^�B	TaB	R�B	Q�B	O�B	I�B	@OB	9�B	2�B	+�B	'�B	%,B	"�B	�B	1B	sB	 B	�B	�B	pB	~B		�B	�B	�B	�B	�B��B�	B��B��B�B�vB�UB�B��B�B�B��B��B�B��B��B�QB�0B�eB�B�8B�B�tB�B�NB�B�|B��B�BߤB�pB�jB��B�5BܬB�~B�xB�xB�]B��B�qB�=BڠB�QB�B�kBچBٴB�KB�eB�KB��B�1B�1B�sB�7B��B�	BیB�WB�#B��B�WB�)BݘB��B�jB޸B�B�VB�BB��B�B��B�B�,B��B�B��B�yB�kB�B�B�qB�B�B�B� B� B�;B�B�|B�|B�B�B��B��B�9B�B��B�	B��B	 �B	;B	�B	�B	�B	-B	�B	gB	9B	�B	�B	�B	�B	DB	B	�B	�B	�B	B	2B	mB	SB	B	B	B	�B	�B	�B	!-B	#B	#TB	%,B	%�B	%�B	&LB	-]B	1vB	3�B	8B	=VB	>�B	DB	L�B	N�B	P�B	R�B	SuB	T�B	U2B	U�B	VB	VSB	V�B	W�B	]B	aHB	c�B	e�B	g�B	m)B	n/B	m�B	nB	p�B	q�B	shB	y	B	z�B	{�B	|6B	� B	��B	�%B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�B	��B	��B	�+B	�KB	��B	��B	�hB	��B	�2B	�2B	��B	�qB	��B	�iB	�tB	�JB	�uB	ɠB	�JB	�B	�6B	͹B	��B	ΊB	ϫB	�B	�B	�[B	ؓB	�xB	�vB	�B	��B	�*B	�*B	�_B	��B	�B	�B	�5B	��B	�B	��B	�?B	�%B	��B	�zB	��B	��B	��B	��B
AB
�B
�B
�B
	7B
	�B
�B
\B
 B
�B
�B
�B
�B
B
SB

B
B
B
%B
'�B
(�B
)�B
)�B
*�B
+�B
,�B
-�B
.�B
2-B
2�B
3�B
5B
5�B
6B
:xB
<�B
?�B
@�B
A�B
A�B
BAB
BuB
CGB
C{B
D�B
EB
EB
E�B
E�B
FYB
GEB
H�B
J	B
K�B
LJB
Q�B
S&B
UgB
VB
W�B
X+B
X�B
YKB
Y�B
Y�B
\�B
\]B
\xB
]/B
]B
]�B
^5B
`'B
bNB
c�B
e`B
ezB
gB
j0B
k�B
n�B
o�B
q�B
t�B
xB
y	B
y$B
y>B
y�B
y�B
y�B
zDB
zxB
z�B
z�B
{JB
|PB
}VB
~B
�B
�iB
�;B
��B
�AB
��B
��B
�B
�aB
��B
��B
��B
��B
��B
�lB
�B
��B
��B
�bB
��B
��B
��B
��B
��B
��B
��B
�B
�YB
��B
�+B
��B
��B
��B
�KB
�eB
��B
��B
��B
�7B
�QB
��B
��B
��B
�#B
�WB
�qB
��B
��B
�~B
��B
�OB
��B
�!B
�VB
��B
��B
�,B
�sB
��B
�B
�kB
��B
��B
�"B
��B
��B
��B
�aB
��B
�B
��B
�%B
��B
�FB
��B
��B
�XB
��B
��B
�xB
�B
�6B
��B
��B
āB
��B
ƨB
�EB
�B
ȀB
ȴB
ȀB
ȴB
��B
�B
�7B
�RB
ɆB
ɆB
ɺB
ɺB
��B
�rB
��B
�)B
�DB
��B
�B
��B
�B
�4B
�hB
уB
ѷB
��B
��B
�TB
�B
�@B
өB
�B
ԯB
��B
��B
�2B
�MB
�gB
�B
��B
ٚB
��B
�B
�kB
�#B
��B
�)B
��B
��B
��B
��B
��B
��B
��B
��B
�IB
�5B
ޞB
��B
�VB
��B
��B
�'B
��B
�B
�bB
��B
��B
�B
��B
� B
� B
�B
��B
��B
�B
�LB
�B
�B
��B
�B
�B
�B
�B
��B
��B
�B
�QB
�B
�B
��B
�CB
��B
�B
�/B
�IB
�/B
�}B
�iB
�OB
�iB
�B
��B
��B
�B
��B
�'B
�B
�B
�B
��B
��B
�B
��B
�B
�TB
�B
�B
�TB
�nB
�tB
�tB
��B
��B
�LB
��B
�B
��B
��B
��B
�0B
�B
�6B
��B
�VB
�qB
��B
��B
��B
�B
��B
��B
��B B B 4B OB �B �B �B �BB;BoBoB�B�BuB�B�B�BBMBMB�BBB9BSBSBmB�B�BEBzB�BB1BBKB1B	7B
rBB)B)B�BJBdB~B�B�B�BBpB�B�B�BBvB�B�B.B}B�B BNB�B�B�B BoB�B�B&B�B�B�B�BMB�BB9BSB
B$BYB�BBB_B�B�B�BB1BeB�B�B7BkB�B�B�B�B�B�B�B�B�B	B	B	B	B#B�BB)B)B]B]BxBxB�BIBB�B�B�BB;B;BVBpBpB�B�B \B \B BB �B �B �B �B �B �B �B �B �B �B �B!HB!|B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B"NB#B"�B# B#TB#TB#�B#�B#�B$&B$&B$@B$@B$�B$�B$�B%,B%�B%�B&fB&�B&�B&�B&�B'B'B'B'B'B'B'8B'RB'�B'�B'�B'�B(sB(�B)yB)yB)�B)yB)�B)�B*eB*�B*�B*�B+444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B*�B*�B*�B*eB*B*�B*�B*B)�B)yB)�B*0B*KB*B*B*�B*B*�B*eB*�B)�B%�B$&B�B.B��B�B�;B�VB��B��B՛B�@BуB�B�(B͟B̈́B��B��B�DB�=BɆB��B�B�+B��B�+B�YB��B�SB��BɺB�YB��B��B��B��B��B�B�XB�=BڠBخB�YB�7B�QB�mB��B�BY�B�LB�B��B�B��B��B��B�#B��B�4B��B}�Bx�Bq�BcB2|B�B��B��B��B��B��Bw2Bk�BVSB@�B&�BjB
��B
ѷB
�0B
�!B
�@B
��B
��B
��B
��B
~]B
{�B
z*B
y$B
vFB
tB
m�B
e,B
`�B
W$B
O�B
L�B
GzB
?�B
/OB
&�B
�B
�B
B
�B
B
	�B
B
B
 �B	��B	��B	��B	�B	�+B	�aB	�WB	�:B	�bB	ߤB	�B	�/B	�=B	�	B	�SB	бB	��B	��B	�B	��B	��B	�gB	�[B	� B	�B	�qB	��B	��B	��B	��B	��B	�tB	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�:B	��B	��B	�AB	}�B	|6B	y	B	w�B	w2B	v`B	t�B	raB	q�B	o�B	m]B	kkB	h�B	cB	^�B	TaB	R�B	Q�B	O�B	I�B	@OB	9�B	2�B	+�B	'�B	%,B	"�B	�B	1B	sB	 B	�B	�B	pB	~B		�B	�B	�B	�B	�B��B�	B��B��B�B�vB�UB�B��B�B�B��B��B�B��B��B�QB�0B�eB�B�8B�B�tB�B�NB�B�|B��B�BߤB�pB�jB��B�5BܬB�~B�xB�xB�]B��B�qB�=BڠB�QB�B�kBچBٴB�KB�eB�KB��B�1B�1B�sB�7B��B�	BیB�WB�#B��B�WB�)BݘB��B�jB޸B�B�VB�BB��B�B��B�B�,B��B�B��B�yB�kB�B�B�qB�B�B�B� B� B�;B�B�|B�|B�B�B��B��B�9B�B��B�	B��B	 �B	;B	�B	�B	�B	-B	�B	gB	9B	�B	�B	�B	�B	DB	B	�B	�B	�B	B	2B	mB	SB	B	B	B	�B	�B	�B	!-B	#B	#TB	%,B	%�B	%�B	&LB	-]B	1vB	3�B	8B	=VB	>�B	DB	L�B	N�B	P�B	R�B	SuB	T�B	U2B	U�B	VB	VSB	V�B	W�B	]B	aHB	c�B	e�B	g�B	m)B	n/B	m�B	nB	p�B	q�B	shB	y	B	z�B	{�B	|6B	� B	��B	�%B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�B	��B	��B	�+B	�KB	��B	��B	�hB	��B	�2B	�2B	��B	�qB	��B	�iB	�tB	�JB	�uB	ɠB	�JB	�B	�6B	͹B	��B	ΊB	ϫB	�B	�B	�[B	ؓB	�xB	�vB	�B	��B	�*B	�*B	�_B	��B	�B	�B	�5B	��B	�B	��B	�?B	�%B	��B	�zB	��B	��B	��B	��B
AB
�B
�B
�B
	7B
	�B
�B
\B
 B
�B
�B
�B
�B
B
SB

B
B
B
%B
'�B
(�B
)�B
)�B
*�B
+�B
,�B
-�B
.�B
2-B
2�B
3�B
5B
5�B
6B
:xB
<�B
?�B
@�B
A�B
A�B
BAB
BuB
CGB
C{B
D�B
EB
EB
E�B
E�B
FYB
GEB
H�B
J	B
K�B
LJB
Q�B
S&B
UgB
VB
W�B
X+B
X�B
YKB
Y�B
Y�B
\�B
\]B
\xB
]/B
]B
]�B
^5B
`'B
bNB
c�B
e`B
ezB
gB
j0B
k�B
n�B
o�B
q�B
t�B
xB
y	B
y$B
y>B
y�B
y�B
y�B
zDB
zxB
z�B
z�B
{JB
|PB
}VB
~B
�B
�iB
�;B
��B
�AB
��B
��B
�B
�aB
��B
��B
��B
��B
��B
�lB
�B
��B
��B
�bB
��B
��B
��B
��B
��B
��B
��B
�B
�YB
��B
�+B
��B
��B
��B
�KB
�eB
��B
��B
��B
�7B
�QB
��B
��B
��B
�#B
�WB
�qB
��B
��B
�~B
��B
�OB
��B
�!B
�VB
��B
��B
�,B
�sB
��B
�B
�kB
��B
��B
�"B
��B
��B
��B
�aB
��B
�B
��B
�%B
��B
�FB
��B
��B
�XB
��B
��B
�xB
�B
�6B
��B
��B
āB
��B
ƨB
�EB
�B
ȀB
ȴB
ȀB
ȴB
��B
�B
�7B
�RB
ɆB
ɆB
ɺB
ɺB
��B
�rB
��B
�)B
�DB
��B
�B
��B
�B
�4B
�hB
уB
ѷB
��B
��B
�TB
�B
�@B
өB
�B
ԯB
��B
��B
�2B
�MB
�gB
�B
��B
ٚB
��B
�B
�kB
�#B
��B
�)B
��B
��B
��B
��B
��B
��B
��B
��B
�IB
�5B
ޞB
��B
�VB
��B
��B
�'B
��B
�B
�bB
��B
��B
�B
��B
� B
� B
�B
��B
��B
�B
�LB
�B
�B
��B
�B
�B
�B
�B
��B
��B
�B
�QB
�B
�B
��B
�CB
��B
�B
�/B
�IB
�/B
�}B
�iB
�OB
�iB
�B
��B
��B
�B
��B
�'B
�B
�B
�B
��B
��B
�B
��B
�B
�TB
�B
�B
�TB
�nB
�tB
�tB
��B
��B
�LB
��B
�B
��B
��B
��B
�0B
�B
�6B
��B
�VB
�qB
��B
��B
��B
�B
��B
��B
��B B B 4B OB �B �B �B �BB;BoBoB�B�BuB�B�B�BBMBMB�BBB9BSBSBmB�B�BEBzB�BB1BBKB1B	7B
rBB)B)B�BJBdB~B�B�B�BBpB�B�B�BBvB�B�B.B}B�B BNB�B�B�B BoB�B�B&B�B�B�B�BMB�BB9BSB
B$BYB�BBB_B�B�B�BB1BeB�B�B7BkB�B�B�B�B�B�B�B�B�B	B	B	B	B#B�BB)B)B]B]BxBxB�BIBB�B�B�BB;B;BVBpBpB�B�B \B \B BB �B �B �B �B �B �B �B �B �B �B �B!HB!|B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B"NB#B"�B# B#TB#TB#�B#�B#�B$&B$&B$@B$@B$�B$�B$�B%,B%�B%�B&fB&�B&�B&�B&�B'B'B'B'B'B'B'8B'RB'�B'�B'�B'�B(sB(�B)yB)yB)�B)yB)�B)�B*eB*�B*�B*�B+444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230211184155  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230211184156  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230211184156  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230211184157                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230211184157  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230211184157  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230211185919                      G�O�G�O�G�O�                