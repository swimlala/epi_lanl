CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-08-24T09:01:16Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20220824090116  20220824090116  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @��U41   @��U�@�,@*�-V�dbI�^51   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D���D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�0 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��G@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bx\)B�aGB��{B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�.B���B�ǮB���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCVCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D��{D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�/�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aݕ�Aݛ�Aݡ�Aݧ�Aݧ�Aݩ�AݬAݰ!AݬAݮAݮAݑhAݍPAݍPA�t�A�z�A�x�A�p�A�p�A�n�A�Q�A��A�bA���A��TA���A�|�A���Aة�A��A��Aԣ�A�hsA�(�A�`BA��A��^A��;A�^5A�5?A��9A���A�z�A�=qA�ZA�;dA��-A�-A�A��#A�A�VA���A�ZA��hA��A��A�`BA��#A�+A��9A��A�ĜA�M�A��jA�dZA�ZA��HA�&�A�JA�VA��!A~(�AzQ�Aw/Ap�uAk�AiO�AhbAf�HAc�TAa��A_/A];dA[S�AWx�AS�AN��AK��AGdZAD1ACK�AC�7AC&�AB�jAA�AA��AA�7A?A=��A<ZA:E�A9O�A8Q�A7G�A4A�A2ȴA2^5A1��A1A0��A/��A.ffA-t�A,��A,1'A,A+�TA+|�A*ĜA)��A)VA(bNA'hsA'&�A&��A%��A%l�A%"�A$�DA#��A#t�A"�A"(�A!�;A!�-A!l�A!�A ��A �A �DA 1'A�A�A�`AZA�mA�-Ap�A�AoA��A��A-A�^A��A�A~�A�A33A%A�HA�RAZA�mAhsA��A��A\)A�+A�
AO�AG�A?}At�A%A�A&�A"�A%A�!A-AƨA"�Ar�A5?AbAhsA�!A(�A��A�Ax�AC�A
�\A
-A
{A	��A	�TA
1A
1A	t�A�`A��An�AQ�A �A  A�7A�A�uAM�A�A��AoA�uAbNAr�A(�A�TA�7A;dA�/A�\A9XA�A�^A��A�AS�A �`A �\A   @�S�@��\@�@�Q�@�dZ@�ff@��@�x�@��/@��@�l�@�v�@�7L@�D@�l�@�~�@�h@�9X@�@�
=@�@���@���@��@�A�@�t�@��H@�\@�v�@�M�@��@�p�@��`@�u@�z�@�  @睲@�dZ@�S�@�33@��@�M�@��@�V@��@�K�@��@�5?@�^@��@߮@�l�@�+@ް!@�$�@ݑh@�hs@�X@�G�@���@�Ĝ@ܴ9@�Q�@���@��H@�M�@�-@�J@���@���@���@؛�@��;@�S�@�o@�J@�x�@�/@�r�@ӝ�@�"�@҇+@���@ѩ�@�`B@���@�9X@��y@�M�@�@́@̼j@�r�@�1@˕�@�ȴ@���@��T@���@���@ə�@��/@ǅ@�C�@�@���@Ɵ�@Ƈ+@�=q@��@���@�?}@�&�@ă@���@Å@�C�@�o@°!@�@�-@��#@�@�@���@���@�p�@�j@�|�@�o@��H@��R@�M�@�$�@��^@�hs@��@�  @���@��F@���@�|�@��@��y@�^5@��#@���@��/@�r�@�b@�dZ@�K�@�;d@���@�ȴ@��\@���@���@�X@��@�1'@��@���@���@�dZ@�C�@��@��H@�=q@��T@���@��@��@�A�@���@�C�@�ȴ@���@�=q@���@�?}@�1'@���@�"�@��+@�M�@�-@�@��7@�G�@���@�Ĝ@��9@���@�A�@��@�dZ@�@���@�ff@�J@���@�hs@��@��j@�Q�@� �@��
@�|�@�+@��R@�=q@�@��h@�7L@���@��@�  @�ƨ@�l�@�dZ@�C�@�o@�ȴ@���@��\@�v�@�V@��@��@��#@���@���@�@���@��7@�`B@�G�@���@�z�@�1'@�  @��;@��@��@�\)@�o@��H@�~�@�J@�hs@��/@��D@�Q�@�1'@� �@��@���@��;@���@�S�@��y@��R@�^5@�5?@���@���@��@�A�@�b@���@��@�C�@��@��H@�~�@�5?@�J@��@��#@��-@�x�@��@��`@���@�I�@���@�|�@�+@��y@��R@��+@�M�@���@���@���@�`B@���@��9@�j@�1@���@�\)@��y@���@��@�{@��@��^@��7@�7L@���@���@���@�I�@�1'@��;@�l�@�o@��R@�^5@�$�@�J@��^@�O�@�V@��`@�j@�b@���@�|�@�\)@�@���@�5?@��@���@���@��7@�X@���@���@�bN@�b@�1@�  @�  @l�@~�y@~ȴ@~�+@}@|��@{�
@{�F@{�F@{��@{o@zM�@y�@y��@yX@y&�@xĜ@xb@w�;@w��@wl�@v�y@v��@vv�@vE�@v{@u@u`B@u/@uV@t�@t�j@t�D@tZ@t9X@t1@s��@s�m@s�m@s�
@sƨ@s��@s33@s@r�@r�!@rn�@r^5@r=q@q��@qx�@q�@pr�@p  @o�@ol�@o\)@ol�@ol�@ol�@o+@n�R@n{@mp�@l�@k�m@kdZ@ko@k@k@k@j�@j��@j��@jM�@h�u@g�@g�P@fV@f@e�@e��@d�@dz�@d�@cdZ@b�H@b��@bn�@b�@aX@`Q�@^ȴ@^V@]�@]/@\��@\�@\Z@[��@[�
@[��@[C�@Z�H@Z^5@Y�^@Y7L@X  @Vȴ@Vff@VE�@V5?@V5?@V@U�h@U?}@UV@T��@Tz�@S�m@St�@SdZ@SdZ@SS�@R�H@R�\@Q��@Qhs@Q%@O��@O��@O�P@O|�@Ol�@OK�@O
=@N�@N�R@N�+@N$�@M@M��@Mp�@M?}@MV@L��@L�@L�/@L�/@Lz�@LZ@L(�@K��@KC�@J�@J��@J��@J�\@JM�@I�^@Ix�@HĜ@H��@Hr�@HbN@HQ�@H �@G�@G�;@G�P@F��@Fȴ@F��@FE�@E��@E�h@EO�@E/@E�@EV@D��@Dz�@D9X@C�
@C�@CS�@C33@C"�@C@B��@B��@B~�@BM�@B=q@B=q@B-@A�^@A�@@��@@Ĝ@@bN@@b@?�P@?\)@?+@?
=@?
=@>��@>�y@>�@>��@>V@>V@>{@=��@=�-@=p�@=O�@=�@=V@<��@<�@<I�@;�F@;��@;dZ@;dZ@;C�@;@:�H@:�!@:��@:n�@:J@9��@9hs@9�@8�9@81'@8b@7�@7��@7K�@6ȴ@6��@6�+@6�+@6�+@6�+@6V@5p�@4�j@41@3��@3S�@2�@2��@2~�@2M�@2-@1�@1��@1�^@1��@1X@0��@/��@/��@/��@/|�@/�@.�R@.5?@-�T@-�h@-p�@,��@,9X@+S�@*�H@*��@*��@*~�@*~�@*n�@*=q@*-@*�@)�#@)hs@)%@(�u@(A�@( �@(b@'�@'��@'K�@'
=@&�R@&V@&{@%�T@%�@%?}@%�@$�@$�j@$��@$Z@#��@#��@#�@#t�@#t�@#dZ@#C�@"�@"-@!��@!��@!��@!&�@ ��@ Ĝ@ �9@ �@ bN@   @��@\)@�@�R@v�@@�@��@��@��@z�@Z@(�@�m@�F@�@dZ@@��@~�@=q@-@��@�^@X@7L@%@Ĝ@�@A�@1'@1'@ �@��@�P@|�@l�@+@�y@ȴ@�R@��@V@@�-@�-@p�@?}@/@V@��@�D@9X@1@�F@dZ@@@�H@M�@�@��@�#@G�@�@��@�u@r�@bN@Q�@1'@b@�w@l�@K�@;d@�@�y@��@E�@5?@$�@�@��@�-@`B@O�@/@�/@�j@�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aݕ�Aݛ�Aݡ�Aݧ�Aݧ�Aݩ�AݬAݰ!AݬAݮAݮAݑhAݍPAݍPA�t�A�z�A�x�A�p�A�p�A�n�A�Q�A��A�bA���A��TA���A�|�A���Aة�A��A��Aԣ�A�hsA�(�A�`BA��A��^A��;A�^5A�5?A��9A���A�z�A�=qA�ZA�;dA��-A�-A�A��#A�A�VA���A�ZA��hA��A��A�`BA��#A�+A��9A��A�ĜA�M�A��jA�dZA�ZA��HA�&�A�JA�VA��!A~(�AzQ�Aw/Ap�uAk�AiO�AhbAf�HAc�TAa��A_/A];dA[S�AWx�AS�AN��AK��AGdZAD1ACK�AC�7AC&�AB�jAA�AA��AA�7A?A=��A<ZA:E�A9O�A8Q�A7G�A4A�A2ȴA2^5A1��A1A0��A/��A.ffA-t�A,��A,1'A,A+�TA+|�A*ĜA)��A)VA(bNA'hsA'&�A&��A%��A%l�A%"�A$�DA#��A#t�A"�A"(�A!�;A!�-A!l�A!�A ��A �A �DA 1'A�A�A�`AZA�mA�-Ap�A�AoA��A��A-A�^A��A�A~�A�A33A%A�HA�RAZA�mAhsA��A��A\)A�+A�
AO�AG�A?}At�A%A�A&�A"�A%A�!A-AƨA"�Ar�A5?AbAhsA�!A(�A��A�Ax�AC�A
�\A
-A
{A	��A	�TA
1A
1A	t�A�`A��An�AQ�A �A  A�7A�A�uAM�A�A��AoA�uAbNAr�A(�A�TA�7A;dA�/A�\A9XA�A�^A��A�AS�A �`A �\A   @�S�@��\@�@�Q�@�dZ@�ff@��@�x�@��/@��@�l�@�v�@�7L@�D@�l�@�~�@�h@�9X@�@�
=@�@���@���@��@�A�@�t�@��H@�\@�v�@�M�@��@�p�@��`@�u@�z�@�  @睲@�dZ@�S�@�33@��@�M�@��@�V@��@�K�@��@�5?@�^@��@߮@�l�@�+@ް!@�$�@ݑh@�hs@�X@�G�@���@�Ĝ@ܴ9@�Q�@���@��H@�M�@�-@�J@���@���@���@؛�@��;@�S�@�o@�J@�x�@�/@�r�@ӝ�@�"�@҇+@���@ѩ�@�`B@���@�9X@��y@�M�@�@́@̼j@�r�@�1@˕�@�ȴ@���@��T@���@���@ə�@��/@ǅ@�C�@�@���@Ɵ�@Ƈ+@�=q@��@���@�?}@�&�@ă@���@Å@�C�@�o@°!@�@�-@��#@�@�@���@���@�p�@�j@�|�@�o@��H@��R@�M�@�$�@��^@�hs@��@�  @���@��F@���@�|�@��@��y@�^5@��#@���@��/@�r�@�b@�dZ@�K�@�;d@���@�ȴ@��\@���@���@�X@��@�1'@��@���@���@�dZ@�C�@��@��H@�=q@��T@���@��@��@�A�@���@�C�@�ȴ@���@�=q@���@�?}@�1'@���@�"�@��+@�M�@�-@�@��7@�G�@���@�Ĝ@��9@���@�A�@��@�dZ@�@���@�ff@�J@���@�hs@��@��j@�Q�@� �@��
@�|�@�+@��R@�=q@�@��h@�7L@���@��@�  @�ƨ@�l�@�dZ@�C�@�o@�ȴ@���@��\@�v�@�V@��@��@��#@���@���@�@���@��7@�`B@�G�@���@�z�@�1'@�  @��;@��@��@�\)@�o@��H@�~�@�J@�hs@��/@��D@�Q�@�1'@� �@��@���@��;@���@�S�@��y@��R@�^5@�5?@���@���@��@�A�@�b@���@��@�C�@��@��H@�~�@�5?@�J@��@��#@��-@�x�@��@��`@���@�I�@���@�|�@�+@��y@��R@��+@�M�@���@���@���@�`B@���@��9@�j@�1@���@�\)@��y@���@��@�{@��@��^@��7@�7L@���@���@���@�I�@�1'@��;@�l�@�o@��R@�^5@�$�@�J@��^@�O�@�V@��`@�j@�b@���@�|�@�\)@�@���@�5?@��@���@���@��7@�X@���@���@�bN@�b@�1@�  @�  @l�@~�y@~ȴ@~�+@}@|��@{�
@{�F@{�F@{��@{o@zM�@y�@y��@yX@y&�@xĜ@xb@w�;@w��@wl�@v�y@v��@vv�@vE�@v{@u@u`B@u/@uV@t�@t�j@t�D@tZ@t9X@t1@s��@s�m@s�m@s�
@sƨ@s��@s33@s@r�@r�!@rn�@r^5@r=q@q��@qx�@q�@pr�@p  @o�@ol�@o\)@ol�@ol�@ol�@o+@n�R@n{@mp�@l�@k�m@kdZ@ko@k@k@k@j�@j��@j��@jM�@h�u@g�@g�P@fV@f@e�@e��@d�@dz�@d�@cdZ@b�H@b��@bn�@b�@aX@`Q�@^ȴ@^V@]�@]/@\��@\�@\Z@[��@[�
@[��@[C�@Z�H@Z^5@Y�^@Y7L@X  @Vȴ@Vff@VE�@V5?@V5?@V@U�h@U?}@UV@T��@Tz�@S�m@St�@SdZ@SdZ@SS�@R�H@R�\@Q��@Qhs@Q%@O��@O��@O�P@O|�@Ol�@OK�@O
=@N�@N�R@N�+@N$�@M@M��@Mp�@M?}@MV@L��@L�@L�/@L�/@Lz�@LZ@L(�@K��@KC�@J�@J��@J��@J�\@JM�@I�^@Ix�@HĜ@H��@Hr�@HbN@HQ�@H �@G�@G�;@G�P@F��@Fȴ@F��@FE�@E��@E�h@EO�@E/@E�@EV@D��@Dz�@D9X@C�
@C�@CS�@C33@C"�@C@B��@B��@B~�@BM�@B=q@B=q@B-@A�^@A�@@��@@Ĝ@@bN@@b@?�P@?\)@?+@?
=@?
=@>��@>�y@>�@>��@>V@>V@>{@=��@=�-@=p�@=O�@=�@=V@<��@<�@<I�@;�F@;��@;dZ@;dZ@;C�@;@:�H@:�!@:��@:n�@:J@9��@9hs@9�@8�9@81'@8b@7�@7��@7K�@6ȴ@6��@6�+@6�+@6�+@6�+@6V@5p�@4�j@41@3��@3S�@2�@2��@2~�@2M�@2-@1�@1��@1�^@1��@1X@0��@/��@/��@/��@/|�@/�@.�R@.5?@-�T@-�h@-p�@,��@,9X@+S�@*�H@*��@*��@*~�@*~�@*n�@*=q@*-@*�@)�#@)hs@)%@(�u@(A�@( �@(b@'�@'��@'K�@'
=@&�R@&V@&{@%�T@%�@%?}@%�@$�@$�j@$��@$Z@#��@#��@#�@#t�@#t�@#dZ@#C�@"�@"-@!��@!��@!��@!&�@ ��@ Ĝ@ �9@ �@ bN@   @��@\)@�@�R@v�@@�@��@��@��@z�@Z@(�@�m@�F@�@dZ@@��@~�@=q@-@��@�^@X@7L@%@Ĝ@�@A�@1'@1'@ �@��@�P@|�@l�@+@�y@ȴ@�R@��@V@@�-@�-@p�@?}@/@V@��@�D@9X@1@�F@dZ@@@�H@M�@�@��@�#@G�@�@��@�u@r�@bN@Q�@1'@b@�w@l�@K�@;d@�@�y@��@E�@5?@$�@�@��@�-@`B@O�@/@�/@�j@�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BO�BO�BO�BO�BO�BO�BO�BO�BO�BN�BM�BM�BM�BK�BL�BL�BK�BL�BK�BI�BF�BF�BB�B9XB7LB=qBL�BT�BS�BVBYB$�B�{BB�NB<jBR�B]/BVB_;Bo�Be`Be`Be`BN�BL�BH�B<jB0!B0!B/B�B�BB�NB�qB��B� Be`BQ�B2-B�B  B
B
�B
[#B
[#B
K�B
,B	��B	��B	�B	��B	��B	r�B	q�B	�+B	�VB	�DB	�B	�B	t�B	p�B	iyB	W
B	I�B	=qB	=qB	(�B	(�B	/B	T�B	x�B	�B	�B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�hB	��B	��B	��B	��B	��B	��B	��B	�}B	��B	�/B	�NB	�NB	�BB	�HB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B
+B
DB
VB
oB
�B
�B
�B
!�B
#�B
%�B
%�B
#�B
#�B
#�B
#�B
&�B
-B
/B
-B
(�B
 �B
/B
7LB
5?B
49B
8RB
49B
1'B
0!B
6FB
9XB
9XB
8RB
6FB
49B
33B
1'B
1'B
1'B
.B
0!B
33B
;dB
?}B
C�B
F�B
J�B
P�B
Q�B
P�B
N�B
K�B
H�B
B�B
@�B
B�B
B�B
<jB
6FB
5?B
8RB
8RB
5?B
7LB
6FB
9XB
<jB
;dB
>wB
A�B
B�B
>wB
=qB
?}B
@�B
@�B
@�B
E�B
E�B
B�B
B�B
B�B
E�B
F�B
@�B
>wB
@�B
D�B
C�B
A�B
A�B
@�B
?}B
?}B
>wB
=qB
<jB
=qB
>wB
<jB
:^B
9XB
8RB
8RB
7LB
6FB
2-B
33B
33B
49B
2-B
2-B
0!B
.B
,B
(�B
)�B
&�B
#�B
$�B
"�B
$�B
#�B
"�B
%�B
%�B
#�B
"�B
!�B
"�B
%�B
&�B
&�B
$�B
%�B
&�B
)�B
-B
,B
,B
-B
-B
,B
+B
)�B
&�B
%�B
!�B
 �B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
!�B
 �B
�B
�B
�B
�B
!�B
!�B
�B
 �B
�B
�B
�B
�B
"�B
"�B
"�B
!�B
�B
 �B
�B
�B
 �B
�B
�B
 �B
�B
"�B
#�B
"�B
"�B
"�B
 �B
!�B
"�B
"�B
�B
#�B
%�B
$�B
$�B
%�B
$�B
#�B
!�B
"�B
#�B
"�B
#�B
$�B
$�B
#�B
$�B
%�B
$�B
$�B
#�B
"�B
%�B
%�B
&�B
)�B
)�B
(�B
)�B
+B
)�B
+B
,B
+B
)�B
)�B
)�B
)�B
+B
)�B
)�B
+B
,B
,B
+B
,B
-B
-B
-B
-B
,B
-B
-B
.B
.B
.B
.B
1'B
1'B
0!B
2-B
2-B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
2-B
2-B
2-B
2-B
0!B
1'B
2-B
2-B
33B
2-B
33B
2-B
2-B
2-B
1'B
0!B
0!B
1'B
33B
49B
5?B
5?B
6FB
5?B
5?B
5?B
49B
33B
5?B
49B
49B
49B
33B
2-B
2-B
7LB
7LB
6FB
6FB
8RB
8RB
7LB
9XB
:^B
:^B
:^B
:^B
:^B
9XB
:^B
9XB
9XB
9XB
:^B
;dB
;dB
<jB
<jB
<jB
<jB
=qB
>wB
=qB
=qB
=qB
>wB
=qB
>wB
>wB
>wB
?}B
?}B
C�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
E�B
E�B
D�B
E�B
F�B
G�B
G�B
H�B
F�B
F�B
G�B
H�B
G�B
H�B
H�B
G�B
H�B
G�B
G�B
H�B
I�B
J�B
J�B
J�B
I�B
I�B
I�B
K�B
K�B
L�B
M�B
L�B
K�B
K�B
L�B
K�B
J�B
J�B
M�B
O�B
O�B
O�B
M�B
M�B
O�B
O�B
P�B
O�B
O�B
O�B
Q�B
Q�B
P�B
P�B
R�B
R�B
R�B
R�B
Q�B
Q�B
R�B
S�B
R�B
S�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
S�B
S�B
S�B
T�B
VB
T�B
T�B
VB
VB
T�B
T�B
T�B
T�B
VB
W
B
W
B
XB
XB
XB
XB
W
B
VB
T�B
T�B
VB
T�B
XB
YB
ZB
ZB
ZB
ZB
YB
XB
W
B
T�B
XB
ZB
XB
[#B
\)B
[#B
ZB
[#B
\)B
[#B
\)B
^5B
]/B
]/B
[#B
[#B
[#B
_;B
`BB
`BB
bNB
bNB
bNB
bNB
cTB
cTB
bNB
bNB
bNB
bNB
bNB
aHB
bNB
e`B
ffB
gmB
ffB
ffB
e`B
ffB
ffB
ffB
e`B
e`B
gmB
hsB
hsB
gmB
ffB
gmB
gmB
gmB
gmB
ffB
jB
k�B
jB
jB
jB
jB
jB
jB
iyB
iyB
iyB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
k�B
l�B
k�B
k�B
jB
l�B
m�B
m�B
l�B
l�B
k�B
l�B
l�B
n�B
n�B
o�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
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
r�B
r�B
r�B
s�B
s�B
r�B
q�B
q�B
r�B
s�B
r�B
r�B
r�B
s�B
t�B
t�B
u�B
t�B
t�B
t�B
t�B
s�B
u�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
u�B
u�B
u�B
t�B
u�B
v�B
u�B
w�B
w�B
w�B
w�B
w�B
x�B
y�B
y�B
x�B
x�B
w�B
u�B
v�B
w�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
y�B
x�B
w�B
y�B
y�B
y�B
x�B
x�B
x�B
y�B
y�B
y�B
x�B
x�B
x�B
z�B
|�B
}�B
}�B
~�B
~�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
� B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�%B
�+B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�DB
�JB
�JB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�bB
�bB
�\B
�\B
�\B
�\B
�bB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
��B
��B
�{B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BO�BO�BO�BO�BO�BO�BO�BO�BO�BN�BM�BM�BM�BK�BL�BL�BK�BL�BK�BI�BF�BF�BB�B9XB7LB=qBL�BT�BS�BVBYB$�B�{BB�NB<jBR�B]/BVB_;Bo�Be`Be`Be`BN�BL�BH�B<jB0!B0!B/B�B�BB�NB�qB��B� Be`BQ�B2-B�B  B
B
�B
[#B
[#B
K�B
,B	��B	��B	�B	��B	��B	r�B	q�B	�+B	�VB	�DB	�B	�B	t�B	p�B	iyB	W
B	I�B	=qB	=qB	(�B	(�B	/B	T�B	x�B	�B	�B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�hB	��B	��B	��B	��B	��B	��B	��B	�}B	��B	�/B	�NB	�NB	�BB	�HB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B
+B
DB
VB
oB
�B
�B
�B
!�B
#�B
%�B
%�B
#�B
#�B
#�B
#�B
&�B
-B
/B
-B
(�B
 �B
/B
7LB
5?B
49B
8RB
49B
1'B
0!B
6FB
9XB
9XB
8RB
6FB
49B
33B
1'B
1'B
1'B
.B
0!B
33B
;dB
?}B
C�B
F�B
J�B
P�B
Q�B
P�B
N�B
K�B
H�B
B�B
@�B
B�B
B�B
<jB
6FB
5?B
8RB
8RB
5?B
7LB
6FB
9XB
<jB
;dB
>wB
A�B
B�B
>wB
=qB
?}B
@�B
@�B
@�B
E�B
E�B
B�B
B�B
B�B
E�B
F�B
@�B
>wB
@�B
D�B
C�B
A�B
A�B
@�B
?}B
?}B
>wB
=qB
<jB
=qB
>wB
<jB
:^B
9XB
8RB
8RB
7LB
6FB
2-B
33B
33B
49B
2-B
2-B
0!B
.B
,B
(�B
)�B
&�B
#�B
$�B
"�B
$�B
#�B
"�B
%�B
%�B
#�B
"�B
!�B
"�B
%�B
&�B
&�B
$�B
%�B
&�B
)�B
-B
,B
,B
-B
-B
,B
+B
)�B
&�B
%�B
!�B
 �B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
!�B
 �B
�B
�B
�B
�B
!�B
!�B
�B
 �B
�B
�B
�B
�B
"�B
"�B
"�B
!�B
�B
 �B
�B
�B
 �B
�B
�B
 �B
�B
"�B
#�B
"�B
"�B
"�B
 �B
!�B
"�B
"�B
�B
#�B
%�B
$�B
$�B
%�B
$�B
#�B
!�B
"�B
#�B
"�B
#�B
$�B
$�B
#�B
$�B
%�B
$�B
$�B
#�B
"�B
%�B
%�B
&�B
)�B
)�B
(�B
)�B
+B
)�B
+B
,B
+B
)�B
)�B
)�B
)�B
+B
)�B
)�B
+B
,B
,B
+B
,B
-B
-B
-B
-B
,B
-B
-B
.B
.B
.B
.B
1'B
1'B
0!B
2-B
2-B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
2-B
2-B
2-B
2-B
0!B
1'B
2-B
2-B
33B
2-B
33B
2-B
2-B
2-B
1'B
0!B
0!B
1'B
33B
49B
5?B
5?B
6FB
5?B
5?B
5?B
49B
33B
5?B
49B
49B
49B
33B
2-B
2-B
7LB
7LB
6FB
6FB
8RB
8RB
7LB
9XB
:^B
:^B
:^B
:^B
:^B
9XB
:^B
9XB
9XB
9XB
:^B
;dB
;dB
<jB
<jB
<jB
<jB
=qB
>wB
=qB
=qB
=qB
>wB
=qB
>wB
>wB
>wB
?}B
?}B
C�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
E�B
E�B
D�B
E�B
F�B
G�B
G�B
H�B
F�B
F�B
G�B
H�B
G�B
H�B
H�B
G�B
H�B
G�B
G�B
H�B
I�B
J�B
J�B
J�B
I�B
I�B
I�B
K�B
K�B
L�B
M�B
L�B
K�B
K�B
L�B
K�B
J�B
J�B
M�B
O�B
O�B
O�B
M�B
M�B
O�B
O�B
P�B
O�B
O�B
O�B
Q�B
Q�B
P�B
P�B
R�B
R�B
R�B
R�B
Q�B
Q�B
R�B
S�B
R�B
S�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
S�B
S�B
S�B
T�B
VB
T�B
T�B
VB
VB
T�B
T�B
T�B
T�B
VB
W
B
W
B
XB
XB
XB
XB
W
B
VB
T�B
T�B
VB
T�B
XB
YB
ZB
ZB
ZB
ZB
YB
XB
W
B
T�B
XB
ZB
XB
[#B
\)B
[#B
ZB
[#B
\)B
[#B
\)B
^5B
]/B
]/B
[#B
[#B
[#B
_;B
`BB
`BB
bNB
bNB
bNB
bNB
cTB
cTB
bNB
bNB
bNB
bNB
bNB
aHB
bNB
e`B
ffB
gmB
ffB
ffB
e`B
ffB
ffB
ffB
e`B
e`B
gmB
hsB
hsB
gmB
ffB
gmB
gmB
gmB
gmB
ffB
jB
k�B
jB
jB
jB
jB
jB
jB
iyB
iyB
iyB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
k�B
l�B
k�B
k�B
jB
l�B
m�B
m�B
l�B
l�B
k�B
l�B
l�B
n�B
n�B
o�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
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
r�B
r�B
r�B
s�B
s�B
r�B
q�B
q�B
r�B
s�B
r�B
r�B
r�B
s�B
t�B
t�B
u�B
t�B
t�B
t�B
t�B
s�B
u�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
u�B
u�B
u�B
t�B
u�B
v�B
u�B
w�B
w�B
w�B
w�B
w�B
x�B
y�B
y�B
x�B
x�B
w�B
u�B
v�B
w�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
y�B
x�B
w�B
y�B
y�B
y�B
x�B
x�B
x�B
y�B
y�B
y�B
x�B
x�B
x�B
z�B
|�B
}�B
}�B
~�B
~�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
� B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�%B
�+B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�DB
�JB
�JB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�bB
�bB
�\B
�\B
�\B
�\B
�bB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
��B
��B
�{B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.01 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220824090116                              AO  ARCAADJP                                                                    20220824090116    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220824090116  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20220824090116  QCF$                G�O�G�O�G�O�0               