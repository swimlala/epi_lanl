CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:41:44Z creation;2022-06-04T17:41:45Z conversion to V3.1      
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174144  20220610131508  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               jA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ٮ��L;*1   @ٮ�J�͏@.���R�cCn��P1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B ffB(  B0  B7��B?��BH  BQ33BW��B`  Bh  Bo��Bw��B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&L�C'�fC)�fC,  C.  C0  C2  C4  C5�fC7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz�fD{fD{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�<�Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@\)@�z@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��BB��B \)B'��B/��B7�]B?�]BG��BQ(�BW�]B_��Bg��Bo�]Bw�]B��B���B���B���B���B���B�.B�ǮB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�.B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC&J>C'��C)��C+�qC-�qC/�qC1�qC3�qC5��C7��C9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCLCNCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���D �D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D��D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz��D{�D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�<{D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D��{D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�{D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D�|{D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��[A˼�A��-A���A��A˽�A�kAʒoAɥA�6A��QA�@OA�IA�U�A�u�AʬAʤAʇ�A�]/A�?HA�'�A�)�A�J�A�خA��"Aɸ�A��A�خA�{�A�OvAţnAĬ�A��A��bA�U�A�=�A���A���A���A��uA��qA�CA���A�A���A�DA���A��A�~�A���A� �A��@A���A�.}A���A�A���A��!A�P}A�SA���A��NA��]A�M�A�FtA��A�-A��?A�d&A��A�AUA��A��A�T�A�L0A�q�A��4A�A�A��aA��A���A��gA�2�A���A�m�A{XAt<�Am��Al<6Ad��A`�kA]��AZ��AYGEAUƨAQ'RAM��AJ��AH�DAGZ�AEIRAC|�AA�A@4A<�A9�A8��A6�vA5�A4�DA4�A3˒A3:�A3�A2�A2��A0��A/��A/A.��A.x�A-�7A+]dA*J�A*qA*<�A)��A)� A(ݘA(�uA'�A'K^A&�NA&7LA%�6A%��A%�A$��A$w�A$l�A#��A#�"A#A A"'�A!�A!Q�A ��A �A�QA�SA�#A4�AB[A�.AT�A�kAxAMjA.IA4nA�A�vA�"A-wA3�AqA�"A�<A�A�"A�AAb�A��A��A�uA�\A{�A�AE�A0�A�:A4�Ad�AGA�sAt�A�A�DA��A �A��A?A�A
��A
��A
��A
��A
��A
ĜA
|�A
8�A
A	GEA҉AcA��A�QA<�AQ�AC�AH�A�?Ao AI�A�A�PA;AiDA�AxA�A�AP�Ap;APHA �A RT@��@���@�\�@�\�@�+k@�GE@�a�@��@�1�@�@��P@�@@��@�"@�C@�W�@��@��@���@�/�@�F@��@��@��@��D@�F@��W@�/@�˒@�"@�\�@�H@�y�@�z�@�:@�rG@��@ߎ�@�k�@�K�@���@�y>@�[W@��@܌�@۝�@ڑ�@�@��`@�C-@���@׽�@�Q�@ֺ�@��@�֡@��"@԰!@ӷ@�a@��@�6�@ц�@�!�@�1'@���@ϋ�@��@Ο�@�!@��@ͮ@�O@�m�@ˊ	@�V@�~�@��@�qv@���@�^5@���@Ǔ@�`B@�8@Ƴh@�j@�tT@�H@�@�b@��Q@�j@Ĭ@�~@Ç�@��@�@�˒@���@���@��S@�Q�@�S@���@�>B@���@��n@�u�@�'�@��@���@���@��r@�?@��C@�o@���@�B[@��@��S@�.I@�ߤ@���@�l"@�(�@�u@���@���@�Z�@��@���@��m@��@�m�@�$�@���@�;@�z�@�	@���@�hs@�%F@���@�}V@�Q@�E�@�:*@��@��H@���@�M@��@�.I@���@��<@��F@�p;@�:*@�O@���@�
=@���@��@��@�w2@���@�L0@��9@��@��E@���@�tT@�C-@��o@���@�{J@�33@��@���@�Q@� �@��h@��@��/@��}@�{�@��@���@�]�@�0�@��P@���@��@�Ft@� �@��A@���@��{@�X�@�2a@���@��@�Xy@�#:@��@��0@���@�c�@�"�@��!@�W�@��d@���@��@�-�@���@�@��@��C@���@�qv@�+@��P@��@��s@�ѷ@���@�S�@��@��@�˒@�!-@��,@��B@��.@��@�ԕ@�}�@�!-@�&�@�-w@�@�;@���@�Ft@��
@��@�o�@�=@��@�y>@�($@��3@��@���@��4@�u�@�Z@�D�@�,=@�~@�@��@��9@�j�@�$t@��@���@���@�6�@��@��n@�k�@�.I@�@���@��@���@��C@� i@���@�4n@�!�@��.@�ƨ@�o @�;d@�!-@��M@���@�� @�r�@�H@�3�@��@��@��&@�خ@��@��	@�RT@�!�@��y@��h@�w�@��@���@���@���@�e,@�IR@�+@��@��)@��.@�Q@�M@���@�Y�@�4�@��@��j@��z@�z@�3�@���@���@��X@��~@��@�`B@��@���@�x@��@l�@~�y@~�@}�@|�p@|r�@|4n@{�W@{��@{�@{خ@{|�@{8@{33@{�@z�6@z_�@z�@y�S@yA @x�|@x��@xI�@x�@wƨ@w=@v�+@v4@u�@uX@tɆ@s�+@s�@rW�@qhs@q�@p�@py>@py>@pU2@o�A@o�@n�@nn�@m�T@m�~@l�[@l1'@kH�@j��@j{�@i�@iJ�@i%F@i \@h��@h@g"�@f� @fV@f
�@e��@e@d�@d`�@dS�@cخ@b͟@b\�@a@`�@`U2@_خ@_l�@_\)@_K�@_1�@^��@^��@^p;@]B�@\Xy@\	�@[��@[x@[�@Z�@Z@�@Z�@Y��@Y��@Y�@YO�@X�	@X�@X�.@Xoi@X�@W�*@WH�@V��@V�c@V��@V� @VL0@U��@Us�@U!�@T�@T*�@So�@S9�@R�m@R��@R	@Q�=@Q\�@QA @Q+�@P��@PQ�@O��@O��@Oa@O�@Nں@N�L@N~�@N3�@M�@MIR@L��@L�z@LH@K�{@J�H@J��@J.�@I�@Ip�@I#�@H��@HĜ@H�e@H�@H@G��@GP�@G$t@Fȴ@FM�@F
�@E�@E��@E2a@D��@D��@D��@D��@D�/@D�@Doi@D%�@C�g@C�[@C|�@C�@B�2@B�@Bc @A�T@A��@ArG@AV@@��@@�U@@�@@:�@?��@?�@?S@>ں@>��@>�+@>5?@=��@=��@=�n@=4@=�@<�@<�9@<��@<]d@<%�@;�F@;A�@:�@:ߤ@:�L@:n�@:H�@:1�@:$�@9��@9�@9��@9`B@8�@8��@8K^@8/�@7�r@7ƨ@7�:@6��@6Q@6	@5ϫ@5�S@5?}@4��@4I�@3ݘ@3��@3g�@3+@2�s@2L0@2&�@1�.@1�>@1ԕ@1��@1��@1X@0��@0�9@0A�@0*�@0�@0b@/9�@.�@.�@.��@.\�@.�@-�@-*0@,�o@+�@+�P@+a@+"�@*��@*c @)�j@)�-@)�'@)X@)%F@(�`@(��@(K^@(@'��@'��@'��@'x@'\)@'9�@',�@&҉@&n�@&GE@%��@%e,@%B�@%+�@$��@$C-@#�W@#�0@#��@#t�@#C�@"�M@"��@"6�@!�)@!@!��@!��@!+@ �@ oi@ D�@ !@��@�a@b�@+@�@��@�1@.�@�@�X@N<@��@�@-�@�;@��@�@g�@dZ@9�@�@��@z@!�@�Z@�t@��@O�@�@ѷ@��@r�@h�@_@/�@��@�:@g�@H�@+@�@��@��@n�@;�@_@�@�@G�@/@!�@;@�v@��@��@�o@Ft@�+@خ@��@��@]�@�@�@�A@~�@q�@W�@�@�N@��@?}@��@�@I�@�@�@�@@{J@;d@�@��@ȴ@��@;�@@�j@�z@��@m]@N<@&�@�@�@�@��@_@-�@��@�A@�A@�g@�V@x@]�@)_@@
��@
��@
�x@
a|@
E�@
�@	ԕ@	��@	�C@	��@	L�@	�@	�@��@�@�4@��@��@��@�o@Z@(�@~@M@�r@�Q@خ@�g@�0@�@@�$@�$@��@v`@Z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��[A˼�A��-A���A��A˽�A�kAʒoAɥA�6A��QA�@OA�IA�U�A�u�AʬAʤAʇ�A�]/A�?HA�'�A�)�A�J�A�خA��"Aɸ�A��A�خA�{�A�OvAţnAĬ�A��A��bA�U�A�=�A���A���A���A��uA��qA�CA���A�A���A�DA���A��A�~�A���A� �A��@A���A�.}A���A�A���A��!A�P}A�SA���A��NA��]A�M�A�FtA��A�-A��?A�d&A��A�AUA��A��A�T�A�L0A�q�A��4A�A�A��aA��A���A��gA�2�A���A�m�A{XAt<�Am��Al<6Ad��A`�kA]��AZ��AYGEAUƨAQ'RAM��AJ��AH�DAGZ�AEIRAC|�AA�A@4A<�A9�A8��A6�vA5�A4�DA4�A3˒A3:�A3�A2�A2��A0��A/��A/A.��A.x�A-�7A+]dA*J�A*qA*<�A)��A)� A(ݘA(�uA'�A'K^A&�NA&7LA%�6A%��A%�A$��A$w�A$l�A#��A#�"A#A A"'�A!�A!Q�A ��A �A�QA�SA�#A4�AB[A�.AT�A�kAxAMjA.IA4nA�A�vA�"A-wA3�AqA�"A�<A�A�"A�AAb�A��A��A�uA�\A{�A�AE�A0�A�:A4�Ad�AGA�sAt�A�A�DA��A �A��A?A�A
��A
��A
��A
��A
��A
ĜA
|�A
8�A
A	GEA҉AcA��A�QA<�AQ�AC�AH�A�?Ao AI�A�A�PA;AiDA�AxA�A�AP�Ap;APHA �A RT@��@���@�\�@�\�@�+k@�GE@�a�@��@�1�@�@��P@�@@��@�"@�C@�W�@��@��@���@�/�@�F@��@��@��@��D@�F@��W@�/@�˒@�"@�\�@�H@�y�@�z�@�:@�rG@��@ߎ�@�k�@�K�@���@�y>@�[W@��@܌�@۝�@ڑ�@�@��`@�C-@���@׽�@�Q�@ֺ�@��@�֡@��"@԰!@ӷ@�a@��@�6�@ц�@�!�@�1'@���@ϋ�@��@Ο�@�!@��@ͮ@�O@�m�@ˊ	@�V@�~�@��@�qv@���@�^5@���@Ǔ@�`B@�8@Ƴh@�j@�tT@�H@�@�b@��Q@�j@Ĭ@�~@Ç�@��@�@�˒@���@���@��S@�Q�@�S@���@�>B@���@��n@�u�@�'�@��@���@���@��r@�?@��C@�o@���@�B[@��@��S@�.I@�ߤ@���@�l"@�(�@�u@���@���@�Z�@��@���@��m@��@�m�@�$�@���@�;@�z�@�	@���@�hs@�%F@���@�}V@�Q@�E�@�:*@��@��H@���@�M@��@�.I@���@��<@��F@�p;@�:*@�O@���@�
=@���@��@��@�w2@���@�L0@��9@��@��E@���@�tT@�C-@��o@���@�{J@�33@��@���@�Q@� �@��h@��@��/@��}@�{�@��@���@�]�@�0�@��P@���@��@�Ft@� �@��A@���@��{@�X�@�2a@���@��@�Xy@�#:@��@��0@���@�c�@�"�@��!@�W�@��d@���@��@�-�@���@�@��@��C@���@�qv@�+@��P@��@��s@�ѷ@���@�S�@��@��@�˒@�!-@��,@��B@��.@��@�ԕ@�}�@�!-@�&�@�-w@�@�;@���@�Ft@��
@��@�o�@�=@��@�y>@�($@��3@��@���@��4@�u�@�Z@�D�@�,=@�~@�@��@��9@�j�@�$t@��@���@���@�6�@��@��n@�k�@�.I@�@���@��@���@��C@� i@���@�4n@�!�@��.@�ƨ@�o @�;d@�!-@��M@���@�� @�r�@�H@�3�@��@��@��&@�خ@��@��	@�RT@�!�@��y@��h@�w�@��@���@���@���@�e,@�IR@�+@��@��)@��.@�Q@�M@���@�Y�@�4�@��@��j@��z@�z@�3�@���@���@��X@��~@��@�`B@��@���@�x@��@l�@~�y@~�@}�@|�p@|r�@|4n@{�W@{��@{�@{خ@{|�@{8@{33@{�@z�6@z_�@z�@y�S@yA @x�|@x��@xI�@x�@wƨ@w=@v�+@v4@u�@uX@tɆ@s�+@s�@rW�@qhs@q�@p�@py>@py>@pU2@o�A@o�@n�@nn�@m�T@m�~@l�[@l1'@kH�@j��@j{�@i�@iJ�@i%F@i \@h��@h@g"�@f� @fV@f
�@e��@e@d�@d`�@dS�@cخ@b͟@b\�@a@`�@`U2@_خ@_l�@_\)@_K�@_1�@^��@^��@^p;@]B�@\Xy@\	�@[��@[x@[�@Z�@Z@�@Z�@Y��@Y��@Y�@YO�@X�	@X�@X�.@Xoi@X�@W�*@WH�@V��@V�c@V��@V� @VL0@U��@Us�@U!�@T�@T*�@So�@S9�@R�m@R��@R	@Q�=@Q\�@QA @Q+�@P��@PQ�@O��@O��@Oa@O�@Nں@N�L@N~�@N3�@M�@MIR@L��@L�z@LH@K�{@J�H@J��@J.�@I�@Ip�@I#�@H��@HĜ@H�e@H�@H@G��@GP�@G$t@Fȴ@FM�@F
�@E�@E��@E2a@D��@D��@D��@D��@D�/@D�@Doi@D%�@C�g@C�[@C|�@C�@B�2@B�@Bc @A�T@A��@ArG@AV@@��@@�U@@�@@:�@?��@?�@?S@>ں@>��@>�+@>5?@=��@=��@=�n@=4@=�@<�@<�9@<��@<]d@<%�@;�F@;A�@:�@:ߤ@:�L@:n�@:H�@:1�@:$�@9��@9�@9��@9`B@8�@8��@8K^@8/�@7�r@7ƨ@7�:@6��@6Q@6	@5ϫ@5�S@5?}@4��@4I�@3ݘ@3��@3g�@3+@2�s@2L0@2&�@1�.@1�>@1ԕ@1��@1��@1X@0��@0�9@0A�@0*�@0�@0b@/9�@.�@.�@.��@.\�@.�@-�@-*0@,�o@+�@+�P@+a@+"�@*��@*c @)�j@)�-@)�'@)X@)%F@(�`@(��@(K^@(@'��@'��@'��@'x@'\)@'9�@',�@&҉@&n�@&GE@%��@%e,@%B�@%+�@$��@$C-@#�W@#�0@#��@#t�@#C�@"�M@"��@"6�@!�)@!@!��@!��@!+@ �@ oi@ D�@ !@��@�a@b�@+@�@��@�1@.�@�@�X@N<@��@�@-�@�;@��@�@g�@dZ@9�@�@��@z@!�@�Z@�t@��@O�@�@ѷ@��@r�@h�@_@/�@��@�:@g�@H�@+@�@��@��@n�@;�@_@�@�@G�@/@!�@;@�v@��@��@�o@Ft@�+@خ@��@��@]�@�@�@�A@~�@q�@W�@�@�N@��@?}@��@�@I�@�@�@�@@{J@;d@�@��@ȴ@��@;�@@�j@�z@��@m]@N<@&�@�@�@�@��@_@-�@��@�A@�A@�g@�V@x@]�@)_@@
��@
��@
�x@
a|@
E�@
�@	ԕ@	��@	�C@	��@	L�@	�@	�@��@�@�4@��@��@��@�o@Z@(�@~@M@�r@�Q@خ@�g@�0@�@@�$@�$@��@v`@Z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Br�BsBr�Br�BtBvzB��B��B�B	^B	:�B	T{B	WYB	\CB	g�B	~�B	��B	�B	��B	�}B	�HB	�B	��B
+�B8B�qB�9BB	�B
�B�B�BөB��B��B�-B��B��B�B�B��B
	B_BuB"4B�B	lB�BTBvB	�B�B�<B��B��B�0B�B�]BSB-B��B��B��B�zB��B��B�!B��B�	B�iBwBl�BdB\�BP}B1�B
�B
�"B
��B
�B
�DB
kQB
Q4B
;JB
$B	�xB	��B	�B	�(B	i�B	S[B	M�B	Q�B	`�B	SuB	?B	0B	#:B	�B	~B	/�B	K�B	OB	S&B	H�B	FB	MB	\B	\)B	]�B	a-B	rGB	}qB	�B	��B	�EB	�^B	��B	��B	�&B	��B	��B	��B	��B	�xB	�.B	�NB	�yB	��B	��B	�B	�)B	�B	��B	��B	�*B	�qB	��B
 4B
�B
	�B
^B
DB
zB
�B
B
�B
�B
	7B
�B
9B
[B	�}B	�B	��B	��B	�aB	�vB	�B	��B	��B	�hB	��B	�3B	�B	�0B	��B	��B	�B	��B	��B	�/B	�B	�)B	��B	��B
�B
�B
�B

rB
�B
�B
 �B
{B
xB
aB
�B
mB
�B
�B
�B
oB
 �B
�B
-B

�B
�B
�B
5B
 �B
!B
B
kB
�B
�B
�B
YB
!�B
%,B
!HB
B
�B
�B
$B
B
�B
�B
�B
4B
BB
�B
1B
VB
&B
�B
�B
�B
EB
B
aB
 �B	�>B	��B	�5B	�)B	�B	�B	��B	�:B	ߊB	ܒB	�B	�xB	��B	�B	�B	�B	�B	�/B	�xB	�)B	��B	�qB	ܒB	��B	�IB	��B	��B	��B	�&B	�B	��B	�BB	�B	�bB	�hB	�B	�@B	�&B	��B	�:B	�B	�B	�hB	�B	�HB	�B	�bB	�B	�4B	��B	��B	�hB	�B	��B	�_B	�sB	�8B	�B	��B	�B	�2B	��B	�B	�B	�LB	��B	�8B	�B	�B	�B	�0B	�B	��B	�eB	�B	�KB	�B	�KB	�B	�B	��B	�B	�wB	�iB	�vB	�aB	�B	�tB	�LB	��B	�2B	�+B	��B	�`B	��B	�TB	�B	�B	�9B	�B	��B	�+B	��B	�>B	�DB	�JB	��B	�6B	�6B	�PB	�jB	��B	��B	�JB	�B	�0B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�HB	�cB
 4B	��B	��B	�cB	�BB	��B	��B	��B	��B	�}B	�cB
 �B
B
 B
�B
aB
-B
�B
�B
aB
GB
-B
�B
�B
B
B
�B
-B
uB
�B
�B
;B
 �B	�B	�B	�B	��B	��B	�wB	�BB	��B	�wB	�.B
  B
 OB
 iB
 OB
 OB	��B
 iB
 �B
 �B
 iB
 4B
 �B
 B
;B
oB
uB
B
B
aB
aB
�B
MB
�B
B
9B
�B
�B
�B
�B
B
+B
_B
1B
	B
	RB
	�B
B
�B
6B
�B
jB
HB
4B
NB
NB
�B
TB
�B
,B
�B
�B
�B
B
9B
B
B
B
MB
�B
gB
B
�B
aB
�B
�B
SB
�B
?B
B
eB
�B
�B
)B
]B
�B
/B
IB
�B
VB
;B
;B
�B
�B
�B
�B
�B
�B
�B
 'B
 �B
 �B
!-B
!HB
!HB
!�B
"4B
!�B
"4B
"4B
"4B
"NB
#:B
#�B
$@B
%B
%�B
%�B
%�B
&2B
&�B
&�B
&�B
'�B
'�B
'�B
(
B
($B
(>B
($B
($B
(XB
(�B
(�B
(�B
)*B
)_B
)yB
)�B
)�B
*B
*�B
*�B
*�B
+B
+B
+6B
+QB
+QB
+�B
+�B
,B
,"B
,�B
-]B
-]B
-�B
.IB
.�B
.�B
/iB
/�B
/�B
/�B
/�B
/�B
/�B
0!B
0�B
0�B
1[B
1[B
1'B
1�B
2�B
2�B
3�B
3�B
4TB
4TB
4�B
5B
5�B
5�B
6B
5�B
5�B
5�B
6FB
6�B
6�B
7B
7B
7B
6�B
6�B
6�B
7fB
7�B
7�B
7�B
88B
8�B
9>B
9>B
9�B
9�B
:DB
:�B
;dB
;�B
;�B
;�B
<6B
<6B
<�B
<�B
<�B
=<B
>B
>BB
>]B
>�B
?B
?.B
?B
?HB
?�B
@iB
@�B
@�B
@�B
AB
A�B
A�B
A�B
A�B
BB
B�B
B�B
CGB
C�B
D3B
D�B
EB
D�B
EB
D�B
E9B
EB
E9B
FtB
F�B
F�B
F�B
GEB
G�B
H1B
H1B
HKB
H�B
H�B
H�B
H�B
IB
H�B
IRB
I7B
I�B
I�B
J=B
JrB
JXB
J�B
J�B
J�B
J�B
KDB
KxB
K�B
LB
L~B
LdB
L�B
L�B
M�B
M�B
M�B
NB
NB
N<B
N�B
N�B
OBB
OvB
O�B
O�B
O�B
O�B
O�B
PB
PHB
PbB
P�B
PbB
Q�B
Q�B
Q�B
RTB
RoB
R�B
R�B
SB
S@B
SuB
S@B
SB
S&B
S&B
SB
S&B
S[B
S�B
TFB
T�B
T{B
T�B
U�B
U�B
U�B
VB
VB
VmB
V�B
W
B
V�B
W
B
WsB
WYB
W�B
W�B
X+B
XEB
X�B
X�B
X�B
X�B
YB
YKB
YeB
Y�B
ZB
ZB
Z�B
ZQB
Z�B
Z�B
[	B
[#B
[=B
[�B
[�B
[�B
[�B
[�B
[�B
\xB
\�B
\�B
\�B
]/B
]IB
]dB
]�B
]~B
]�B
]�B
]�B
^B
^B
^OB
^OB
^OB
^�B
^�B
^�B
_;B
_�B
_�B
`'B
`BB
`BB
`vB
`�B
`�B
`�B
aB
aB
a-B
a|B
a�B
bB
a�B
b�B
b4B
b4B
b�B
c:B
c:B
dB
cTB
d@B
cnB
d�B
d�B
e`B
eB
d�B
ezB
e�B
fB
f�B
g8B
gRB
gmB
gRB
g�B
g�B
h$B
h>B
hXB
h�B
h�B
h�B
h�B
iDB
iyB
h�B
iyB
i�B
iyB
i�B
i�B
i�B
j0B
kB
kB
kB
lB
k�B
kQB
l"B
l�B
l�B
m)B
m)B
mCB
m�B
m�B
nB
nIB
n}B
n}B
n�B
n�B
oB
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q'B
q�B
q�B
q�B
rB
r-B
r�B
r�B
sB
sB
s�B
sMB
shB
s�B
s�B
s�B
s�B
tTB
tTB
tTB
tnB
t�B
t�B
uB
u�B
u�B
u�B
utB
u�B
v+B
vFB
vzB
vzB
vzB
v�B
v�B
w2B
w2B
wfB
w�B
w�B
w�B
xRB
x�B
x�B
x�B
y$B
yrB
yXB
yrB
y�B
y�B
y�B
zDB
z*B
zDB
z^B
z�B
z�B
z^B
z�B
z�B
z�B
{JB
{dB
{�B
|B
|PB
|jB
|�B
}B
}B
}"B
}VB
}�B
}�B
}�B
~(B
~wB
~�B
~�B
~�B
~�B
}B
}B
�B
�B
�B
�B
� B
�iB
��B
��B
��B
��B
�B
�B
�UB
�UB
��B
��B
��B
��B
�B
�[B
�uB
��B
�-B
�B
�-B
�aB
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�9B
�SB
�SB
�SB
��B
��B
��B
��B
�B
�?B
�tB
�YB
�?B
�tB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Br�BsBr�Br�BtBvzB��B��B�B	^B	:�B	T{B	WYB	\CB	g�B	~�B	��B	�B	��B	�}B	�HB	�B	��B
+�B8B�qB�9BB	�B
�B�B�BөB��B��B�-B��B��B�B�B��B
	B_BuB"4B�B	lB�BTBvB	�B�B�<B��B��B�0B�B�]BSB-B��B��B��B�zB��B��B�!B��B�	B�iBwBl�BdB\�BP}B1�B
�B
�"B
��B
�B
�DB
kQB
Q4B
;JB
$B	�xB	��B	�B	�(B	i�B	S[B	M�B	Q�B	`�B	SuB	?B	0B	#:B	�B	~B	/�B	K�B	OB	S&B	H�B	FB	MB	\B	\)B	]�B	a-B	rGB	}qB	�B	��B	�EB	�^B	��B	��B	�&B	��B	��B	��B	��B	�xB	�.B	�NB	�yB	��B	��B	�B	�)B	�B	��B	��B	�*B	�qB	��B
 4B
�B
	�B
^B
DB
zB
�B
B
�B
�B
	7B
�B
9B
[B	�}B	�B	��B	��B	�aB	�vB	�B	��B	��B	�hB	��B	�3B	�B	�0B	��B	��B	�B	��B	��B	�/B	�B	�)B	��B	��B
�B
�B
�B

rB
�B
�B
 �B
{B
xB
aB
�B
mB
�B
�B
�B
oB
 �B
�B
-B

�B
�B
�B
5B
 �B
!B
B
kB
�B
�B
�B
YB
!�B
%,B
!HB
B
�B
�B
$B
B
�B
�B
�B
4B
BB
�B
1B
VB
&B
�B
�B
�B
EB
B
aB
 �B	�>B	��B	�5B	�)B	�B	�B	��B	�:B	ߊB	ܒB	�B	�xB	��B	�B	�B	�B	�B	�/B	�xB	�)B	��B	�qB	ܒB	��B	�IB	��B	��B	��B	�&B	�B	��B	�BB	�B	�bB	�hB	�B	�@B	�&B	��B	�:B	�B	�B	�hB	�B	�HB	�B	�bB	�B	�4B	��B	��B	�hB	�B	��B	�_B	�sB	�8B	�B	��B	�B	�2B	��B	�B	�B	�LB	��B	�8B	�B	�B	�B	�0B	�B	��B	�eB	�B	�KB	�B	�KB	�B	�B	��B	�B	�wB	�iB	�vB	�aB	�B	�tB	�LB	��B	�2B	�+B	��B	�`B	��B	�TB	�B	�B	�9B	�B	��B	�+B	��B	�>B	�DB	�JB	��B	�6B	�6B	�PB	�jB	��B	��B	�JB	�B	�0B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�HB	�cB
 4B	��B	��B	�cB	�BB	��B	��B	��B	��B	�}B	�cB
 �B
B
 B
�B
aB
-B
�B
�B
aB
GB
-B
�B
�B
B
B
�B
-B
uB
�B
�B
;B
 �B	�B	�B	�B	��B	��B	�wB	�BB	��B	�wB	�.B
  B
 OB
 iB
 OB
 OB	��B
 iB
 �B
 �B
 iB
 4B
 �B
 B
;B
oB
uB
B
B
aB
aB
�B
MB
�B
B
9B
�B
�B
�B
�B
B
+B
_B
1B
	B
	RB
	�B
B
�B
6B
�B
jB
HB
4B
NB
NB
�B
TB
�B
,B
�B
�B
�B
B
9B
B
B
B
MB
�B
gB
B
�B
aB
�B
�B
SB
�B
?B
B
eB
�B
�B
)B
]B
�B
/B
IB
�B
VB
;B
;B
�B
�B
�B
�B
�B
�B
�B
 'B
 �B
 �B
!-B
!HB
!HB
!�B
"4B
!�B
"4B
"4B
"4B
"NB
#:B
#�B
$@B
%B
%�B
%�B
%�B
&2B
&�B
&�B
&�B
'�B
'�B
'�B
(
B
($B
(>B
($B
($B
(XB
(�B
(�B
(�B
)*B
)_B
)yB
)�B
)�B
*B
*�B
*�B
*�B
+B
+B
+6B
+QB
+QB
+�B
+�B
,B
,"B
,�B
-]B
-]B
-�B
.IB
.�B
.�B
/iB
/�B
/�B
/�B
/�B
/�B
/�B
0!B
0�B
0�B
1[B
1[B
1'B
1�B
2�B
2�B
3�B
3�B
4TB
4TB
4�B
5B
5�B
5�B
6B
5�B
5�B
5�B
6FB
6�B
6�B
7B
7B
7B
6�B
6�B
6�B
7fB
7�B
7�B
7�B
88B
8�B
9>B
9>B
9�B
9�B
:DB
:�B
;dB
;�B
;�B
;�B
<6B
<6B
<�B
<�B
<�B
=<B
>B
>BB
>]B
>�B
?B
?.B
?B
?HB
?�B
@iB
@�B
@�B
@�B
AB
A�B
A�B
A�B
A�B
BB
B�B
B�B
CGB
C�B
D3B
D�B
EB
D�B
EB
D�B
E9B
EB
E9B
FtB
F�B
F�B
F�B
GEB
G�B
H1B
H1B
HKB
H�B
H�B
H�B
H�B
IB
H�B
IRB
I7B
I�B
I�B
J=B
JrB
JXB
J�B
J�B
J�B
J�B
KDB
KxB
K�B
LB
L~B
LdB
L�B
L�B
M�B
M�B
M�B
NB
NB
N<B
N�B
N�B
OBB
OvB
O�B
O�B
O�B
O�B
O�B
PB
PHB
PbB
P�B
PbB
Q�B
Q�B
Q�B
RTB
RoB
R�B
R�B
SB
S@B
SuB
S@B
SB
S&B
S&B
SB
S&B
S[B
S�B
TFB
T�B
T{B
T�B
U�B
U�B
U�B
VB
VB
VmB
V�B
W
B
V�B
W
B
WsB
WYB
W�B
W�B
X+B
XEB
X�B
X�B
X�B
X�B
YB
YKB
YeB
Y�B
ZB
ZB
Z�B
ZQB
Z�B
Z�B
[	B
[#B
[=B
[�B
[�B
[�B
[�B
[�B
[�B
\xB
\�B
\�B
\�B
]/B
]IB
]dB
]�B
]~B
]�B
]�B
]�B
^B
^B
^OB
^OB
^OB
^�B
^�B
^�B
_;B
_�B
_�B
`'B
`BB
`BB
`vB
`�B
`�B
`�B
aB
aB
a-B
a|B
a�B
bB
a�B
b�B
b4B
b4B
b�B
c:B
c:B
dB
cTB
d@B
cnB
d�B
d�B
e`B
eB
d�B
ezB
e�B
fB
f�B
g8B
gRB
gmB
gRB
g�B
g�B
h$B
h>B
hXB
h�B
h�B
h�B
h�B
iDB
iyB
h�B
iyB
i�B
iyB
i�B
i�B
i�B
j0B
kB
kB
kB
lB
k�B
kQB
l"B
l�B
l�B
m)B
m)B
mCB
m�B
m�B
nB
nIB
n}B
n}B
n�B
n�B
oB
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q'B
q�B
q�B
q�B
rB
r-B
r�B
r�B
sB
sB
s�B
sMB
shB
s�B
s�B
s�B
s�B
tTB
tTB
tTB
tnB
t�B
t�B
uB
u�B
u�B
u�B
utB
u�B
v+B
vFB
vzB
vzB
vzB
v�B
v�B
w2B
w2B
wfB
w�B
w�B
w�B
xRB
x�B
x�B
x�B
y$B
yrB
yXB
yrB
y�B
y�B
y�B
zDB
z*B
zDB
z^B
z�B
z�B
z^B
z�B
z�B
z�B
{JB
{dB
{�B
|B
|PB
|jB
|�B
}B
}B
}"B
}VB
}�B
}�B
}�B
~(B
~wB
~�B
~�B
~�B
~�B
}B
}B
�B
�B
�B
�B
� B
�iB
��B
��B
��B
��B
�B
�B
�UB
�UB
��B
��B
��B
��B
�B
�[B
�uB
��B
�-B
�B
�-B
�aB
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�9B
�SB
�SB
�SB
��B
��B
��B
��B
�B
�?B
�tB
�YB
�?B
�tB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104927  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174144  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174145  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174145                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024152  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024152  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131508                      G�O�G�O�G�O�                