CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-06-21T07:03:04Z creation;2023-06-21T07:03:05Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230621070304  20230621071017  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 A.�~    9   @�4�כ�$@;��t�j�c�XbM�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @���A   A   AA��A`  A~ffA�  A�  A�  A���A�  A�33A�33B   B  B  B  B   B(ffB0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�33B�  B���B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C+�fC.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB�CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch�Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��C��C�  C��C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C��C��C�  C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D��D� D	  D	� D
  D
� D  D�fD  Dy�D  D� D  D� D  D�fD  D� D  D� D  D� DfD� D  D� D  D� D��D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#�fD$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D0��D1� D2  D2� D3  D3� D4fD4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>fD>� D?  D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO�fDP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DT��DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]�fD^fD^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dc��Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dx��Dy� Dz  Dz� D{  D{� D{��D|� D}  D}y�D}��D~� D  D� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D���D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�<�D�� D�� D�  D�<�D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�|�D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D���D�@ Dŀ D�� D�  D�@ Dƃ3D��3D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D��3D�  D�@ Dʀ D�� D�  D�@ Dˀ D˼�D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D�|�D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�<�DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր Dּ�D�  D�@ D׀ D�� D���D�@ D؀ D�� D�  D�@ Dـ D��3D�  D�@ Dڃ3D��3D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D޼�D���D�@ D�|�D�� D�  D�<�D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�|�D�� D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�&f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\@\)@�z�@��A�
AAp�A_�
A~=pA��A��A��A��RA��A��A��A��B��B��B��B��B(\)B0\)B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B�.B���B���B���B���B�.B�.B���B�ǮB���B�.B���B���B���B�ǮB���B���B���B���B���B���B���B�ǮB���B���B���B���B���C�qC�qC�qC�qC
C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!��C#�qC%�qC'�qC)�qC+��C-�qC0C1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCBCDCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCfChCi�qCk�qCm�qCo�qCq�qCtCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C��C��C���C��C���C���C���C��C��C��C���C���C���C���C���C���C���C���C��C���C��C���C���C���C���C���C���C���C���C���C���C���C���C��C��C��C��C���C��C��C���C���C���C���C���C���C���C���C���C��C���C���C���C���C��C���C���C���C���C���C���C���C���C��C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\Dx�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D��D\D�\D	\D	�\D
\D
�\D��D�\Dx�D�\D\D�\D\D�\D��D�\D\D�\D\D�\D\D�D\D�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D��D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D#�D#��D#�\D$\D$�\D%\D%��D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0��D1\D1�\D2\D2�\D3\D4�D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D>�D>\D>�\D?\D?�\D@\D@�\DAx�DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI��DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO��DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT��DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D]�D]��D^�D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dcx�Dc��Dd\Dd�\De\De�\Df\Df�\Dgx�Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dm�Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Ds�Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx��Dy\Dy�\Dz\Dz�\D{\D{��D|\D|�\D}x�D}��D~\D~�\D\D�\D�?�D��D���D���D�?�D���D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D���D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D���D���D���D�?�D��D���D��{D�?�D���D���D���D�?�D��D���D���D�?�D��D��{D��{D�<{D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�<{D�|{D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�<{D��D���D���D�<{D�|{D��{D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D�|{D���D���D�?�D���D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�?�D�|{D���D���D�?�D�|{D���D���D�?�D��D���D���D�?�D��D���D��{D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D��{D�?�D��Dſ�D���D�?�DƂ�D���D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��D���D���D�?�D��Dʿ�D���D�?�D��D˼{D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D�|{Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�<{D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dּ{D���D�?�D��D׿�D��{D�?�D��Dؿ�D���D�?�D��D���D���D�?�Dڂ�D���D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޼{D��{D�?�D�|{D߿�D���D�<{D�|{D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D���D��D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D�|{D鿮D���D�?�D�|{D꿮D���D�B�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�&111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�L�A�P�A�M6A�9�A�-wA�uA�A���A��A��RAʲ�Aʉ7A�L�A�d�A���A���A��>A��A��0A�l�A�J�A�h�A�\�A�A�A�A�FtA�G�A�U�A���A�ƨA�($A�IA���A�n/A�7�A��A��A�pA�_A�_pA�l�A�!�A��oA�
rA��SA��A��MA��2A�jKA��2A�"�A���A�/OA���A���A� �A�ZA��A��A�kQA��bA��!A���A��uA���A�یA���A�y	A��8A�x�A�6�A��A���A�K�A�A���A���A���A�{A�~(A���A�NA��A��2A�(XA��4A�A��yA�33A��A�FA�d�A�hsA��A�MA��LA���A���A��A�aHA��)A���A�LdA��A@OA}͟A|FtA{d�Az�-Ax�Ax+�Aw��Aw+�Av��Av��Av!�Au��At��As=qAq�XAq|�Ao�BAm�IAlCAj5�Ai�AhoiAg`BAezAd��Ac�Ab%A`�A^�A\^5AZ�6AZOAY�DAX��AW�OAWkQAV��AU��AR�kAQ�AO��AOR�ANɆAMtTAJ�'AI��AH��AF��AF.IAE�SAEDgAD�AD9XAD
=AA��AAs�AA9�AA,=AA�A?�A=��A=�A<�MA<�MA<�oA<�A<�A<��A;�jA:�mA:>�A8��A7�A6��A5��A4�;A4-�A3~�A1�	A1�rA1E�A1�A/sA.'RA-�FA,M�A+{A*T�A)�XA)h�A(tTA'��A'
=A&��A%ɆA$m�A#ݘA#[WA"�{A!��A jA��A��A��A�mA@A��Aj�AXA��A�;A"hA��A��A(A~�A�Ag8A�5A/�A�DA��A�Ar�Ah
A��A$�A
��A	��A	�A��A�hAuA�A�hA��A�RA9XA��A��A� AȴA �yA L�A 	�@���@�Ta@�%�@���@���@��W@�{�@�F@���@�*0@�R�@��@�/�@�Z�@됗@�S�@�C@��D@�e@��@�qv@�K^@�A�@�f�@�3�@�.�@���@ߔ�@��@��@ږ�@�Vm@�:�@�l"@՗$@Ԍ@�)�@�@�@���@��@̂A@�oi@Ȍ@ů�@��K@�9X@��@�ȴ@�C@���@�3�@�G�@�/�@��T@���@�PH@��@�33@��@���@�L0@���@���@���@�=q@��@�3�@�F@���@�6@���@�M�@��@���@��M@�=�@��F@�J�@��@��@�ی@�n�@���@��"@�s@�?}@���@�n�@��[@���@���@�O@�)_@���@��f@��]@�z�@�@���@�͟@���@�=q@���@� \@���@���@�M�@��@��@�|@�=@���@�oi@�6@�O@��g@�t�@��@��/@��/@���@��s@�Z�@���@�9�@���@���@�1'@��@��2@�l"@�;�@��W@���@�\�@�W?@�8@�@��M@���@��.@�y>@�q@�n�@�m�@�q�@�v�@�s�@�(�@��@���@�\�@�'�@���@�C-@�~@�~@��.@�V@�N�@�!�@��@���@�zx@�N<@�O@�=@��@��H@��U@���@�4n@�s�@�	l@���@��`@���@�n�@�I�@�C�@�+k@�%�@�4@��T@�ƨ@��V@�y�@�iD@�Vm@�<6@��"@��c@��@�h�@�_@�Q�@�~@��@y�@6z@~��@~�@}��@}�"@}a�@}Dg@|�@|e�@|4n@|@|G@{��@{J#@z�L@z�b@z��@z��@zQ@y�@y��@yG�@x�v@x�@x`�@w��@w�@wW?@w�@v�@v!�@u��@ua�@t�z@tXy@t1@s��@s�@r��@rO@q\�@q&�@p�|@p�`@p�E@pĜ@p��@pc�@pS�@pA�@p9X@p(�@p7@o��@oiD@n��@n��@nZ�@m��@m�~@m#�@l��@k�]@k8@j��@h�U@g�@g��@gX�@g.I@g"�@g�@g�@f��@fW�@f3�@f�@f�@f �@e�@e�@e�@d�4@dD�@d~@c��@c�f@c�@b�H@b��@b��@b��@b��@a�@a!�@`�@`M@`*�@`@`�@_��@_�[@_�:@_9�@^p;@^u@]�@]�-@]��@]u�@]4@\��@\��@\��@\A�@[�f@[�@[S@Z�R@Z{�@Za|@ZC�@Z@Y�d@X�@W�@W��@WMj@W,�@V�@V��@VGE@U��@UN<@TɆ@Tc�@TK^@S��@S�0@S&@R��@RZ�@Q�z@QB�@Q(�@Q�@P��@P7@O�6@O��@O��@O�*@O~�@Oa@N��@N��@NkQ@N8�@M�H@MX@L�9@L�@L��@L��@L�u@Lu�@K�+@J�r@J}V@J��@J��@JTa@Is�@H]d@G��@G&@G�@G�@GY@G�@GC@G�@G$t@G'�@G!-@GY@F�"@F�@F��@F��@FTa@FJ�@FJ�@FL0@FOv@F0U@F@E�@Eu�@E/@D��@C�@B҉@B��@B�@BYK@BV@B=q@B($@Be@B	@A@A�M@A`B@A4@A@@@��@@�O@@�@@e�@?�r@?˒@?K�@?S@>�@>��@>�A@>�@=�.@=��@=�@=�>@=�@=�^@=hs@<�|@<��@<C-@;g�@;o@:��@:��@9@9��@9��@9Y�@92a@8�	@8'R@7�@7�a@7��@7��@7�	@7�@6�@6}V@6�@5�#@5�@4�@4�[@4�?@4�@4�O@4�@4��@4~(@4Q�@41'@4�@4x@3��@2�@2��@2��@2M�@1��@1��@1�#@1�@1�z@1��@1�"@1^�@0�f@0�4@0q@0?�@0@/��@/{J@/O@/6z@/+@/
=@.�s@.�h@.0U@-�@-�@-Dg@-@@,�K@,��@,��@,�D@,>B@,  @+�}@+��@+.I@*�8@*��@*��@*W�@*O@) \@(�f@(�5@(�K@(�`@(��@(�E@(�[@(��@(��@(~@'|�@'8@&�@&J@%�)@%�-@%Y�@$��@$��@$�@$�@$m�@$]d@#�r@#��@#��@#g�@#C@# i@"�@"��@"��@"�h@"�\@"p;@"E�@"&�@!�@!�H@!�@!|@!Dg@ �@ ��@ r�@ 7�@�&@�}@ƨ@�@�[@��@y�@U�@C�@�@��@�@�@�@�"@��@A @�@��@%�@�@�W@�&@�g@��@�:@Z�@&@�M@͟@��@�@��@-w@�@�P@��@,=@��@v`@W?@F�@33@@҉@�@�@��@�3@��@�@u�@c�@�@�@_@1'@1'@4n@6@A�@D�@>B@�@�@��@E9@�@��@_�@�Z@�'@u�@Vm@F@!�@�f@ی@Ĝ@��@��@m�@�@�@�&@�0@�q@�*@�:@W?@.I@�8@�b@\�@u@��@��@��@|@`B@F@+@��@�E@��@~(@I�@M@�@�@�@�}@�F@�[@��@y�@S�@�@
�c@
�<@
ff@
 �@	�@	��@	��@	j@	N<@	(�@	@�K@�U@�_@�@l"@M@,=@7@�@�@��@�@@�k@��@��@n/@Mj@8@�@��@ں@�b@��@��@a|@5?@
�@�#@��@�-@�@�=@�~@zx@B�@�@��@h�@M@2�@�@@	�@�@��@�[@��@{J@b�@Z�@8@�@��@��@{�@Ta@E�@.�@ԕ@F@+@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�L�A�P�A�M6A�9�A�-wA�uA�A���A��A��RAʲ�Aʉ7A�L�A�d�A���A���A��>A��A��0A�l�A�J�A�h�A�\�A�A�A�A�FtA�G�A�U�A���A�ƨA�($A�IA���A�n/A�7�A��A��A�pA�_A�_pA�l�A�!�A��oA�
rA��SA��A��MA��2A�jKA��2A�"�A���A�/OA���A���A� �A�ZA��A��A�kQA��bA��!A���A��uA���A�یA���A�y	A��8A�x�A�6�A��A���A�K�A�A���A���A���A�{A�~(A���A�NA��A��2A�(XA��4A�A��yA�33A��A�FA�d�A�hsA��A�MA��LA���A���A��A�aHA��)A���A�LdA��A@OA}͟A|FtA{d�Az�-Ax�Ax+�Aw��Aw+�Av��Av��Av!�Au��At��As=qAq�XAq|�Ao�BAm�IAlCAj5�Ai�AhoiAg`BAezAd��Ac�Ab%A`�A^�A\^5AZ�6AZOAY�DAX��AW�OAWkQAV��AU��AR�kAQ�AO��AOR�ANɆAMtTAJ�'AI��AH��AF��AF.IAE�SAEDgAD�AD9XAD
=AA��AAs�AA9�AA,=AA�A?�A=��A=�A<�MA<�MA<�oA<�A<�A<��A;�jA:�mA:>�A8��A7�A6��A5��A4�;A4-�A3~�A1�	A1�rA1E�A1�A/sA.'RA-�FA,M�A+{A*T�A)�XA)h�A(tTA'��A'
=A&��A%ɆA$m�A#ݘA#[WA"�{A!��A jA��A��A��A�mA@A��Aj�AXA��A�;A"hA��A��A(A~�A�Ag8A�5A/�A�DA��A�Ar�Ah
A��A$�A
��A	��A	�A��A�hAuA�A�hA��A�RA9XA��A��A� AȴA �yA L�A 	�@���@�Ta@�%�@���@���@��W@�{�@�F@���@�*0@�R�@��@�/�@�Z�@됗@�S�@�C@��D@�e@��@�qv@�K^@�A�@�f�@�3�@�.�@���@ߔ�@��@��@ږ�@�Vm@�:�@�l"@՗$@Ԍ@�)�@�@�@���@��@̂A@�oi@Ȍ@ů�@��K@�9X@��@�ȴ@�C@���@�3�@�G�@�/�@��T@���@�PH@��@�33@��@���@�L0@���@���@���@�=q@��@�3�@�F@���@�6@���@�M�@��@���@��M@�=�@��F@�J�@��@��@�ی@�n�@���@��"@�s@�?}@���@�n�@��[@���@���@�O@�)_@���@��f@��]@�z�@�@���@�͟@���@�=q@���@� \@���@���@�M�@��@��@�|@�=@���@�oi@�6@�O@��g@�t�@��@��/@��/@���@��s@�Z�@���@�9�@���@���@�1'@��@��2@�l"@�;�@��W@���@�\�@�W?@�8@�@��M@���@��.@�y>@�q@�n�@�m�@�q�@�v�@�s�@�(�@��@���@�\�@�'�@���@�C-@�~@�~@��.@�V@�N�@�!�@��@���@�zx@�N<@�O@�=@��@��H@��U@���@�4n@�s�@�	l@���@��`@���@�n�@�I�@�C�@�+k@�%�@�4@��T@�ƨ@��V@�y�@�iD@�Vm@�<6@��"@��c@��@�h�@�_@�Q�@�~@��@y�@6z@~��@~�@}��@}�"@}a�@}Dg@|�@|e�@|4n@|@|G@{��@{J#@z�L@z�b@z��@z��@zQ@y�@y��@yG�@x�v@x�@x`�@w��@w�@wW?@w�@v�@v!�@u��@ua�@t�z@tXy@t1@s��@s�@r��@rO@q\�@q&�@p�|@p�`@p�E@pĜ@p��@pc�@pS�@pA�@p9X@p(�@p7@o��@oiD@n��@n��@nZ�@m��@m�~@m#�@l��@k�]@k8@j��@h�U@g�@g��@gX�@g.I@g"�@g�@g�@f��@fW�@f3�@f�@f�@f �@e�@e�@e�@d�4@dD�@d~@c��@c�f@c�@b�H@b��@b��@b��@b��@a�@a!�@`�@`M@`*�@`@`�@_��@_�[@_�:@_9�@^p;@^u@]�@]�-@]��@]u�@]4@\��@\��@\��@\A�@[�f@[�@[S@Z�R@Z{�@Za|@ZC�@Z@Y�d@X�@W�@W��@WMj@W,�@V�@V��@VGE@U��@UN<@TɆ@Tc�@TK^@S��@S�0@S&@R��@RZ�@Q�z@QB�@Q(�@Q�@P��@P7@O�6@O��@O��@O�*@O~�@Oa@N��@N��@NkQ@N8�@M�H@MX@L�9@L�@L��@L��@L�u@Lu�@K�+@J�r@J}V@J��@J��@JTa@Is�@H]d@G��@G&@G�@G�@GY@G�@GC@G�@G$t@G'�@G!-@GY@F�"@F�@F��@F��@FTa@FJ�@FJ�@FL0@FOv@F0U@F@E�@Eu�@E/@D��@C�@B҉@B��@B�@BYK@BV@B=q@B($@Be@B	@A@A�M@A`B@A4@A@@@��@@�O@@�@@e�@?�r@?˒@?K�@?S@>�@>��@>�A@>�@=�.@=��@=�@=�>@=�@=�^@=hs@<�|@<��@<C-@;g�@;o@:��@:��@9@9��@9��@9Y�@92a@8�	@8'R@7�@7�a@7��@7��@7�	@7�@6�@6}V@6�@5�#@5�@4�@4�[@4�?@4�@4�O@4�@4��@4~(@4Q�@41'@4�@4x@3��@2�@2��@2��@2M�@1��@1��@1�#@1�@1�z@1��@1�"@1^�@0�f@0�4@0q@0?�@0@/��@/{J@/O@/6z@/+@/
=@.�s@.�h@.0U@-�@-�@-Dg@-@@,�K@,��@,��@,�D@,>B@,  @+�}@+��@+.I@*�8@*��@*��@*W�@*O@) \@(�f@(�5@(�K@(�`@(��@(�E@(�[@(��@(��@(~@'|�@'8@&�@&J@%�)@%�-@%Y�@$��@$��@$�@$�@$m�@$]d@#�r@#��@#��@#g�@#C@# i@"�@"��@"��@"�h@"�\@"p;@"E�@"&�@!�@!�H@!�@!|@!Dg@ �@ ��@ r�@ 7�@�&@�}@ƨ@�@�[@��@y�@U�@C�@�@��@�@�@�@�"@��@A @�@��@%�@�@�W@�&@�g@��@�:@Z�@&@�M@͟@��@�@��@-w@�@�P@��@,=@��@v`@W?@F�@33@@҉@�@�@��@�3@��@�@u�@c�@�@�@_@1'@1'@4n@6@A�@D�@>B@�@�@��@E9@�@��@_�@�Z@�'@u�@Vm@F@!�@�f@ی@Ĝ@��@��@m�@�@�@�&@�0@�q@�*@�:@W?@.I@�8@�b@\�@u@��@��@��@|@`B@F@+@��@�E@��@~(@I�@M@�@�@�@�}@�F@�[@��@y�@S�@�@
�c@
�<@
ff@
 �@	�@	��@	��@	j@	N<@	(�@	@�K@�U@�_@�@l"@M@,=@7@�@�@��@�@@�k@��@��@n/@Mj@8@�@��@ں@�b@��@��@a|@5?@
�@�#@��@�-@�@�=@�~@zx@B�@�@��@h�@M@2�@�@@	�@�@��@�[@��@{J@b�@Z�@8@�@��@��@{�@Ta@E�@.�@ԕ@F@+@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B�B��B�B��B��B�hB�B�&B�`B�XB�B��B�B�B��B��B��B�MBðB�]B�XB�aB� B�$B��B��B��B�`B��B��B��B�:B�eB�2B�}B�vB��B�#B��B�B}�B{�Bw�Bv`Bq�Bp;BoiBjeBf�B_;BW�BN�BK)BG+B@�B9�B3�B/�B"�B�B��B� BÖB��B�2B� B��B�YB��B��B��B�OBxRBqABi_B\�BS�BG�B<�B0;B"�B�B��B��B�
B�(B��B�+B�B��Bw2Be�BU2BE�B8�B5�B-)B!�B�BVB�B�B �B
�VB
�TB
��B
��B
߾B
�B
͹B
�JB
�B
�%B
�aB
� B
�dB
��B
�OB
�,B
��B
�7B
��B
�B
|PB
tTB
p�B
mCB
a�B
]�B
X�B
PbB
CGB
<B
7LB
7�B
=qB
;�B
9	B
2�B
1'B
.}B
)�B
B
 B
�B
�B
�B	�$B	�yB	�B	�B	ؓB	��B	�
B	��B	҉B	�B	ˬB	�+B	�B	�B	�HB	�.B	��B	�[B	�)B	�WB	�B	�WB	��B	�QB	��B	��B	�&B	��B	�5B	��B	�:B	��B	�gB	�4B	~�B	uB	t�B	tB	s�B	s3B	j�B	h$B	dB	_B	\�B	[	B	YeB	W�B	T{B	O�B	M�B	KxB	DMB	CaB	B[B	A�B	=�B	:B	5ZB	1B	,�B	'B	&�B	!|B	B	qB	?B	�B	@B	�B	�B	vB	jB	6B	^B	�B	zB	�B��B��B�2B��B�B��B��B��B�>B�B��B�B�B�BB�;B�]BچB�B��B�KB�+B�KB�$B��B�{BԕB�B�B��B͟B̈́BʌB�7BȚB�BǔB�B��B�B�mB�B�gB�AB�BªB�{B��B�'B��B��B��B��B��B�B��B��B�xB�DB��B��B�$B��B��B��B�$B�8B��B��B��B��B�B��B��B�B�oB��B�GB�B�BɠBʦB��B̘B̘B�B�jB�B��B�4B��BؓBޞB�jB�B��B�B��B�B�)B��B�cB��B��B�%B��B��B��B�*B��B�B��B��B��B	+B	�B	aB	�B	mB	�B	�B	$@B	&�B	)B	+B	0UB	1B	2aB	6FB	88B	9$B	;�B	="B	?cB	@iB	BB	CB	E�B	HKB	I�B	J#B	L�B	P.B	TB	V�B	W�B	X_B	YB	^B	bNB	dB	g�B	j0B	lB	r-B	y�B	~�B	��B	��B	��B	�#B	��B	�^B	�VB	�.B	�B	��B	�B	�SB	��B	��B	�+B	��B	�sB	�xB	�VB	��B	��B	��B	�sB	��B	�iB	�5B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�GB	��B	�B	�tB	�zB	ɠB	ϫB	��B	�&B	өB	��B	՛B	ևB	�sB	�B	�7B	ߊB	��B	��B	�B	�B	�XB	��B	��B	�)B	�wB	�]B	�B	��B	�B	�3B	�tB	�`B	��B	�$B	�B	��B
 OB
;B
�B
�B
%B
_B
fB
�B
	B
�B
�B
�B
4B
�B
�B
B
B
mB
�B
qB
jB
�B
!HB
#nB
%�B
&LB
)�B
*�B
,"B
.�B
/�B
1AB
2�B
49B
5�B
6FB
8�B
:�B
<�B
=�B
>�B
?}B
@�B
A�B
B'B
B�B
CB
CaB
DgB
ESB
G�B
J�B
K�B
L~B
M�B
OvB
P�B
P�B
RoB
V�B
W�B
`�B
c�B
e,B
f2B
f�B
gRB
g�B
gRB
i�B
j�B
kkB
lB
l=B
l�B
l�B
ncB
p�B
q�B
sB
s�B
t9B
utB
w�B
xlB
x�B
y$B
y�B
y�B
{�B
~]B
�B
�B
�UB
�oB
��B
�'B
�[B
��B
�B
��B
��B
�fB
�B
�RB
�lB
�rB
�)B
�xB
�JB
��B
�.B
�B
�4B
��B
�@B
��B
��B
��B
��B
�sB
�QB
��B
�xB
��B
�dB
�5B
�;B
��B
��B
��B
��B
��B
��B
�fB
�>B
��B
�eB
�=B
��B
��B
��B
�iB
��B
��B
�vB
��B
��B
��B
�-B
�hB
�B
�TB
�%B
�FB
��B
��B
��B
��B
��B
��B
�	B
��B
��B
��B
��B
�VB
�B
��B
ªB
��B
�SB
�mB
�B
�SB
ňB
�mB
�SB
�mB
�SB
�B
ňB
��B
�B
�%B
�tB
�_B
ǔB
�B
�_B
�_B
�_B
ǔB
ȚB
�B
ɠB
�rB
�0B
�pB
�VB
�(B
�vB
�(B
ϫB
��B
��B
��B
�bB
�B
�hB
��B
ѝB
�oB
ҽB
��B
�&B
��B
�aB
ՁB
ՁB
�B
�mB
ּB
�sB
��B
��B
��B
�B
�+B
�EB
خB
��B
ںB
ںB
��B
�IB
�~B
�5B
��B
��B
��B
�\B
��B
�bB
��B
��B
�B
��B
�B
�B
��B
�B
��B
�2B
�B
�>B
�B
�sB
��B
��B
��B
��B
��B
�_B
��B
��B
�B
��B
��B
��B
�WB
�qB
�B
��B
��B
�B
��B
�B
��B
�IB
�IB
�iB
��B
��B
�oB
��B
�[B
�vB
��B
�-B
�-B
�B
�B
��B
��B
�B
��B
�ZB
��B
��B
�FB
�`B
�FB
��B
�LB
��B
��B
��B
�	B
�$B
�XB
��B
�*B
��B
�B
��B
��B
��B
��B
��B
�B
�B
�6B
��B
��B
�BB
�HB �B �B �B�B�BuB�B-BB-B�B3BMB�B�B�BmB�BmBmBB%B?BYB�BB�B_B�BKB�B�B	RB	�B	�B	�B
	B
#B
#B
rB
�B
�B
�B
�BxB~BB6B�B�B�B<B�BBBB\BvBBB�B.B}B�B BNB:B�B&B�B�B,B�BgB�B�BBBBmB
B�BBB�B_B�B�B1BKBBkB�BkBkBkBkB�B�B	B�B�B�BxB�B~BBjBjBjB�B!B;BVBpB�B�B \B vB �B �B �B �B!B!-B!�B!�B"NB"�B#TB#�B#�B#�B#�B$&B$tB$ZB$�B$�B%B%,B%�B&B&LB&B&LB&�B&�B&LB&�B&�B&�B'8B'�B'�B(XB(�B(�B(�B)DB)�B)�B*0B*eB*eB*�B+B+B+6B+6B+�B+�B+�B,B,"B,WB,WB,�B,�B,�B,�B-B-]B-�B-�B.IB./B-�B.cB.}B.�B/B.�B/5B/iB/iB/iB/iB/�B0UB0�B1'B1B1'B1vB1�B1�B1�B2-B1�B2-B2�B2�B2�B2�B2�B33B3�B4B4B4B3�B4�B5�B5�B6+444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B��B�B��B�B��B��B�hB�B�&B�`B�XB�B��B�B�B��B��B��B�MBðB�]B�XB�aB� B�$B��B��B��B�`B��B��B��B�:B�eB�2B�}B�vB��B�#B��B�B}�B{�Bw�Bv`Bq�Bp;BoiBjeBf�B_;BW�BN�BK)BG+B@�B9�B3�B/�B"�B�B��B� BÖB��B�2B� B��B�YB��B��B��B�OBxRBqABi_B\�BS�BG�B<�B0;B"�B�B��B��B�
B�(B��B�+B�B��Bw2Be�BU2BE�B8�B5�B-)B!�B�BVB�B�B �B
�VB
�TB
��B
��B
߾B
�B
͹B
�JB
�B
�%B
�aB
� B
�dB
��B
�OB
�,B
��B
�7B
��B
�B
|PB
tTB
p�B
mCB
a�B
]�B
X�B
PbB
CGB
<B
7LB
7�B
=qB
;�B
9	B
2�B
1'B
.}B
)�B
B
 B
�B
�B
�B	�$B	�yB	�B	�B	ؓB	��B	�
B	��B	҉B	�B	ˬB	�+B	�B	�B	�HB	�.B	��B	�[B	�)B	�WB	�B	�WB	��B	�QB	��B	��B	�&B	��B	�5B	��B	�:B	��B	�gB	�4B	~�B	uB	t�B	tB	s�B	s3B	j�B	h$B	dB	_B	\�B	[	B	YeB	W�B	T{B	O�B	M�B	KxB	DMB	CaB	B[B	A�B	=�B	:B	5ZB	1B	,�B	'B	&�B	!|B	B	qB	?B	�B	@B	�B	�B	vB	jB	6B	^B	�B	zB	�B��B��B�2B��B�B��B��B��B�>B�B��B�B�B�BB�;B�]BچB�B��B�KB�+B�KB�$B��B�{BԕB�B�B��B͟B̈́BʌB�7BȚB�BǔB�B��B�B�mB�B�gB�AB�BªB�{B��B�'B��B��B��B��B��B�B��B��B�xB�DB��B��B�$B��B��B��B�$B�8B��B��B��B��B�B��B��B�B�oB��B�GB�B�BɠBʦB��B̘B̘B�B�jB�B��B�4B��BؓBޞB�jB�B��B�B��B�B�)B��B�cB��B��B�%B��B��B��B�*B��B�B��B��B��B	+B	�B	aB	�B	mB	�B	�B	$@B	&�B	)B	+B	0UB	1B	2aB	6FB	88B	9$B	;�B	="B	?cB	@iB	BB	CB	E�B	HKB	I�B	J#B	L�B	P.B	TB	V�B	W�B	X_B	YB	^B	bNB	dB	g�B	j0B	lB	r-B	y�B	~�B	��B	��B	��B	�#B	��B	�^B	�VB	�.B	�B	��B	�B	�SB	��B	��B	�+B	��B	�sB	�xB	�VB	��B	��B	��B	�sB	��B	�iB	�5B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�GB	��B	�B	�tB	�zB	ɠB	ϫB	��B	�&B	өB	��B	՛B	ևB	�sB	�B	�7B	ߊB	��B	��B	�B	�B	�XB	��B	��B	�)B	�wB	�]B	�B	��B	�B	�3B	�tB	�`B	��B	�$B	�B	��B
 OB
;B
�B
�B
%B
_B
fB
�B
	B
�B
�B
�B
4B
�B
�B
B
B
mB
�B
qB
jB
�B
!HB
#nB
%�B
&LB
)�B
*�B
,"B
.�B
/�B
1AB
2�B
49B
5�B
6FB
8�B
:�B
<�B
=�B
>�B
?}B
@�B
A�B
B'B
B�B
CB
CaB
DgB
ESB
G�B
J�B
K�B
L~B
M�B
OvB
P�B
P�B
RoB
V�B
W�B
`�B
c�B
e,B
f2B
f�B
gRB
g�B
gRB
i�B
j�B
kkB
lB
l=B
l�B
l�B
ncB
p�B
q�B
sB
s�B
t9B
utB
w�B
xlB
x�B
y$B
y�B
y�B
{�B
~]B
�B
�B
�UB
�oB
��B
�'B
�[B
��B
�B
��B
��B
�fB
�B
�RB
�lB
�rB
�)B
�xB
�JB
��B
�.B
�B
�4B
��B
�@B
��B
��B
��B
��B
�sB
�QB
��B
�xB
��B
�dB
�5B
�;B
��B
��B
��B
��B
��B
��B
�fB
�>B
��B
�eB
�=B
��B
��B
��B
�iB
��B
��B
�vB
��B
��B
��B
�-B
�hB
�B
�TB
�%B
�FB
��B
��B
��B
��B
��B
��B
�	B
��B
��B
��B
��B
�VB
�B
��B
ªB
��B
�SB
�mB
�B
�SB
ňB
�mB
�SB
�mB
�SB
�B
ňB
��B
�B
�%B
�tB
�_B
ǔB
�B
�_B
�_B
�_B
ǔB
ȚB
�B
ɠB
�rB
�0B
�pB
�VB
�(B
�vB
�(B
ϫB
��B
��B
��B
�bB
�B
�hB
��B
ѝB
�oB
ҽB
��B
�&B
��B
�aB
ՁB
ՁB
�B
�mB
ּB
�sB
��B
��B
��B
�B
�+B
�EB
خB
��B
ںB
ںB
��B
�IB
�~B
�5B
��B
��B
��B
�\B
��B
�bB
��B
��B
�B
��B
�B
�B
��B
�B
��B
�2B
�B
�>B
�B
�sB
��B
��B
��B
��B
��B
�_B
��B
��B
�B
��B
��B
��B
�WB
�qB
�B
��B
��B
�B
��B
�B
��B
�IB
�IB
�iB
��B
��B
�oB
��B
�[B
�vB
��B
�-B
�-B
�B
�B
��B
��B
�B
��B
�ZB
��B
��B
�FB
�`B
�FB
��B
�LB
��B
��B
��B
�	B
�$B
�XB
��B
�*B
��B
�B
��B
��B
��B
��B
��B
�B
�B
�6B
��B
��B
�BB
�HB �B �B �B�B�BuB�B-BB-B�B3BMB�B�B�BmB�BmBmBB%B?BYB�BB�B_B�BKB�B�B	RB	�B	�B	�B
	B
#B
#B
rB
�B
�B
�B
�BxB~BB6B�B�B�B<B�BBBB\BvBBB�B.B}B�B BNB:B�B&B�B�B,B�BgB�B�BBBBmB
B�BBB�B_B�B�B1BKBBkB�BkBkBkBkB�B�B	B�B�B�BxB�B~BBjBjBjB�B!B;BVBpB�B�B \B vB �B �B �B �B!B!-B!�B!�B"NB"�B#TB#�B#�B#�B#�B$&B$tB$ZB$�B$�B%B%,B%�B&B&LB&B&LB&�B&�B&LB&�B&�B&�B'8B'�B'�B(XB(�B(�B(�B)DB)�B)�B*0B*eB*eB*�B+B+B+6B+6B+�B+�B+�B,B,"B,WB,WB,�B,�B,�B,�B-B-]B-�B-�B.IB./B-�B.cB.}B.�B/B.�B/5B/iB/iB/iB/iB/�B0UB0�B1'B1B1'B1vB1�B1�B1�B2-B1�B2-B2�B2�B2�B2�B2�B33B3�B4B4B4B3�B4�B5�B5�B6+444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230621070302  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230621070304  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230621070304  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230621070305                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230621070305  QCP$                G�O�G�O�G�O�         20DF37AJA  ARGQrqcpc3.6                                                                20230621070305  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230621071017                      G�O�G�O�G�O�                