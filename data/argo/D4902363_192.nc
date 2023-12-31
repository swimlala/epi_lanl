CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-12-24T00:35:19Z creation;2017-12-24T00:35:23Z conversion to V3.1;2019-12-19T07:53:30Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171224003519  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_192                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�?68�w 1   @�?6�O� @;��!�R��dWC��%1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D?��D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�3D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @8��@\)@��H@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B\)B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�ǮB���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCz
C{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?��D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI��DJ\DJ�\DK\DK�\DL\DL�\DM\DN�DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg��Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D��{D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D��D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D�l{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�n�A�n�A�n�A�l�A�p�A�n�A�n�A�jA�l�A�hsA�n�A�n�A�p�A�n�A�r�A�p�A�jA�bNA�XA�ZA�ZA�K�A�I�A�G�A�E�A�E�A�E�A�E�A�C�A�C�A�C�A�C�A�E�A�C�A�C�A�A�A�=qA�9XA�33A�+A�$�A��A�{A�
=A���A�v�A��`A���A���A�A��FA���A��TA��A��A��A�~�A��A���A�`BA�1'A�S�A�+A���A���A��`A�C�A�5?A��A�1A�ƨA�A�A�`BA�E�A�ZA���A���A�t�A�dZA�A�A�A}C�A|v�A{�A{��Ay�hAx1Av�Au��At�Atz�Ar�yAr1'Aq�hAp�`Ao�^Am�Ak�Ak|�AkO�Ak%Aj�Aj�+Aj=qAi�mAiG�Ah�`Ah�AhJAg��Ag��Ag��Af�Ae�Ac��Ab�A`��A`$�A_x�A_�A^ĜA]�A\�A\�DA[��AY��AX��AW�AW
=AV��AVȴAU��AS�;AS33AR5?AQ��AQO�AO�AN�\AM�hALĜAK
=AI�AH��AHr�AHE�AG�AF�HAFn�AE��AD��AC��AB�ABn�AA��AA
=A@�A?t�A>ĜA>(�A<��A;�7A;?}A;oA:�A:ZA9��A9S�A9"�A8�/A8��A8VA7x�A5�wA5
=A4~�A3A2�DA1�;A1C�A0��A/C�A-�A-O�A,v�A*�9A*JA(�RA(n�A'�mA'7LA&�jA&A�A&  A%�#A%�7A%G�A$��A$��A$��A$=qA#��A"�jA!\)A��A��A�7A`BA�A(�AȴAhsA�TAQ�At�AbNAA+A�A��A�A7LA/A/A/A&�AoA��AM�A��A`BAĜAE�A�FA��A��A{A��A33A�FA	�;AC�A9XA�A?}A��A�uA%A @��R@�O�@�hs@�@��@��@��@�j@�r�@�1'@�l�@���@� �@���@�K�@��@�n�@�F@◍@�{@��#@��@�x�@�hs@��/@�C�@ݲ-@�A�@�"�@�n�@�&�@֏\@�V@�1'@�=q@Ο�@�I�@��/@��@�+@Ƈ+@�^5@�M�@��@�V@�Q�@+@�n�@�=q@��@��T@��h@�x�@�x�@�G�@�?}@�G�@��@���@�r�@��;@�\)@�l�@�K�@��!@�@��
@��@��@��7@���@��@�M�@��-@�bN@��F@�~�@�bN@��D@���@�7L@�Q�@�t�@���@�E�@�$�@���@��P@�+@�o@��y@���@�?}@��@��m@�l�@��\@��@�9X@�o@�M�@�&�@�r�@�b@��P@���@���@��R@�ff@�$�@��-@�G�@���@���@�Ĝ@��j@���@� �@��F@�33@��+@�{@���@��@�%@���@��/@��9@�j@�Q�@� �@�K�@��+@�{@��@��#@�@���@��7@�%@���@�bN@���@��@��/@���@���@�v�@�5?@�{@��@��T@��^@�/@��D@�(�@�1@�  @���@�  @���@�+@��\@�ff@���@���@�&�@��9@�Z@��;@�C�@�@��+@���@��@���@���@��@��u@�bN@�1'@�b@��m@�ƨ@���@�;d@�
=@��!@�M�@��@�@���@�`B@�%@��j@��@���@�z�@�A�@� �@|�@;d@+@
=@~��@~�y@~��@}@|��@|��@|��@|I�@{��@{ƨ@{��@{�@{@z��@z^5@zM�@zM�@zM�@z=q@z�@yhs@w�;@w�@v��@vff@v{@u�T@u�-@u�h@u�@uO�@u/@u/@u/@u?}@u�@uV@tj@s�m@st�@sC�@s33@s@r�\@rM�@rM�@r-@q�@q�#@q�#@q�^@qhs@qG�@pĜ@o�@nȴ@nff@nE�@n$�@mp�@m?}@mO�@mp�@m?}@l�/@lz�@l(�@lj@l9X@j�@k@k33@m?}@m��@m��@mp�@m`B@mO�@mV@l�@l(�@k�@kt�@kdZ@kS�@j��@j~�@jM�@jM�@i��@i�#@i�#@i�#@i��@iX@i7L@h�`@h �@g�@g�P@g;d@f�+@f5?@e�h@d��@dz�@c�m@c��@c�@c33@c@c@b�!@a�@ax�@a�@`�9@`bN@_�@_l�@^ȴ@^E�@^{@^@]@]��@]�h@]/@\��@[�m@[dZ@Z�H@Z�\@Y�@Yx�@Y7L@Y�@X��@XĜ@X�9@XbN@X �@W�;@W;d@VV@V5?@V$�@U�@U@U/@T�@T�@SS�@R�!@R�\@Rn�@R�@RJ@Q��@Q��@Q�@Q��@Qhs@Q�@Q�@P�`@P�9@P�u@P  @O|�@O;d@N��@N5?@N{@M�T@M��@M`B@M?}@MV@L�@L�@Kƨ@K��@K�@KS�@K"�@K@K@J�H@J��@J��@J��@J�\@J^5@Ix�@H�9@H�@HA�@G�;@G�w@G|�@Fȴ@F��@Fff@F$�@F@E�@E�T@E�@E�@D�/@D�D@Dj@DZ@DI�@D(�@D1@C��@Cƨ@C��@CC�@C"�@Bn�@@��@@r�@@b@?�@?;d@?
=@>��@>ȴ@>��@>��@>v�@>E�@>�+@>E�@=�T@=�h@=V@;�m@;dZ@:��@:~�@:n�@:^5@:n�@:�@9��@9�7@9hs@9X@9&�@8��@8�9@8r�@8 �@7��@7l�@7;d@7
=@6�@6��@6{@5�@5`B@4�@4z�@41@3�
@3�F@3��@3��@3��@3ƨ@3ƨ@3��@3t�@3"�@2�\@2�@1�#@1��@1G�@1&�@0Ĝ@0�@0bN@01'@01'@0A�@0A�@0A�@0  @/�w@/|�@/+@.�R@.v�@.V@.5?@-�@-/@,��@,�D@,I�@,�@+�m@+�F@+�@+t�@+S�@+C�@*~�@)��@)7L@)&�@)&�@)%@(�9@( �@(b@'��@'|�@'K�@&�y@&ff@&@%@%O�@%?}@%?}@$�@$��@$��@$9X@#ƨ@#�F@#��@#t�@#"�@"�H@"��@"�!@"�!@"��@"��@"�\@"~�@"n�@"^5@"^5@"M�@"=q@"�@"J@!�@!��@!��@!��@!X@!�@!%@ �`@ Ĝ@ �u@ bN@ A�@�;@��@��@��@��@��@�@|�@l�@
=@��@ff@5?@{@@�@�@�@�T@�-@�h@�@p�@O�@/@�@��@�j@z�@I�@�F@dZ@@n�@=q@-@�@��@�7@7L@�`@��@bN@��@�P@l�@K�@+@�@ff@E�@$�@@�@�@��@?}@V@�@z�@j@9X@1@�m@�
@��@dZ@o@��@�!@��@�\@�\@-@�7@�@��@Ĝ@��@��@�u@�@bN@A�@1'@ �@��@|�@\)@��@��@��@��@��@E�@5?@5?@$�@{@@�-@��@�h@`B@/@�@�j@�D@9X@�@1@��@��@�@�@dZ@S�@33@o@
��@
n�@
=q@	��@	��@	��@	x�@	7L@	&�@	%@��@ �@�;@�w@��@|�@\)@K�@K�@�@�y@��@E�@�@�T@@�-@�h@`B@/@��@��@�@��@�j@�@�@�D@9X@�@�
@��@dZ@C�@33@o@�@�H@�H@�!@J@��@��@�#@��@�7@G�@7L11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�n�A�n�A�n�A�l�A�p�A�n�A�n�A�jA�l�A�hsA�n�A�n�A�p�A�n�A�r�A�p�A�jA�bNA�XA�ZA�ZA�K�A�I�A�G�A�E�A�E�A�E�A�E�A�C�A�C�A�C�A�C�A�E�A�C�A�C�A�A�A�=qA�9XA�33A�+A�$�A��A�{A�
=A���A�v�A��`A���A���A�A��FA���A��TA��A��A��A�~�A��A���A�`BA�1'A�S�A�+A���A���A��`A�C�A�5?A��A�1A�ƨA�A�A�`BA�E�A�ZA���A���A�t�A�dZA�A�A�A}C�A|v�A{�A{��Ay�hAx1Av�Au��At�Atz�Ar�yAr1'Aq�hAp�`Ao�^Am�Ak�Ak|�AkO�Ak%Aj�Aj�+Aj=qAi�mAiG�Ah�`Ah�AhJAg��Ag��Ag��Af�Ae�Ac��Ab�A`��A`$�A_x�A_�A^ĜA]�A\�A\�DA[��AY��AX��AW�AW
=AV��AVȴAU��AS�;AS33AR5?AQ��AQO�AO�AN�\AM�hALĜAK
=AI�AH��AHr�AHE�AG�AF�HAFn�AE��AD��AC��AB�ABn�AA��AA
=A@�A?t�A>ĜA>(�A<��A;�7A;?}A;oA:�A:ZA9��A9S�A9"�A8�/A8��A8VA7x�A5�wA5
=A4~�A3A2�DA1�;A1C�A0��A/C�A-�A-O�A,v�A*�9A*JA(�RA(n�A'�mA'7LA&�jA&A�A&  A%�#A%�7A%G�A$��A$��A$��A$=qA#��A"�jA!\)A��A��A�7A`BA�A(�AȴAhsA�TAQ�At�AbNAA+A�A��A�A7LA/A/A/A&�AoA��AM�A��A`BAĜAE�A�FA��A��A{A��A33A�FA	�;AC�A9XA�A?}A��A�uA%A @��R@�O�@�hs@�@��@��@��@�j@�r�@�1'@�l�@���@� �@���@�K�@��@�n�@�F@◍@�{@��#@��@�x�@�hs@��/@�C�@ݲ-@�A�@�"�@�n�@�&�@֏\@�V@�1'@�=q@Ο�@�I�@��/@��@�+@Ƈ+@�^5@�M�@��@�V@�Q�@+@�n�@�=q@��@��T@��h@�x�@�x�@�G�@�?}@�G�@��@���@�r�@��;@�\)@�l�@�K�@��!@�@��
@��@��@��7@���@��@�M�@��-@�bN@��F@�~�@�bN@��D@���@�7L@�Q�@�t�@���@�E�@�$�@���@��P@�+@�o@��y@���@�?}@��@��m@�l�@��\@��@�9X@�o@�M�@�&�@�r�@�b@��P@���@���@��R@�ff@�$�@��-@�G�@���@���@�Ĝ@��j@���@� �@��F@�33@��+@�{@���@��@�%@���@��/@��9@�j@�Q�@� �@�K�@��+@�{@��@��#@�@���@��7@�%@���@�bN@���@��@��/@���@���@�v�@�5?@�{@��@��T@��^@�/@��D@�(�@�1@�  @���@�  @���@�+@��\@�ff@���@���@�&�@��9@�Z@��;@�C�@�@��+@���@��@���@���@��@��u@�bN@�1'@�b@��m@�ƨ@���@�;d@�
=@��!@�M�@��@�@���@�`B@�%@��j@��@���@�z�@�A�@� �@|�@;d@+@
=@~��@~�y@~��@}@|��@|��@|��@|I�@{��@{ƨ@{��@{�@{@z��@z^5@zM�@zM�@zM�@z=q@z�@yhs@w�;@w�@v��@vff@v{@u�T@u�-@u�h@u�@uO�@u/@u/@u/@u?}@u�@uV@tj@s�m@st�@sC�@s33@s@r�\@rM�@rM�@r-@q�@q�#@q�#@q�^@qhs@qG�@pĜ@o�@nȴ@nff@nE�@n$�@mp�@m?}@mO�@mp�@m?}@l�/@lz�@l(�@lj@l9X@j�@k@k33@m?}@m��@m��@mp�@m`B@mO�@mV@l�@l(�@k�@kt�@kdZ@kS�@j��@j~�@jM�@jM�@i��@i�#@i�#@i�#@i��@iX@i7L@h�`@h �@g�@g�P@g;d@f�+@f5?@e�h@d��@dz�@c�m@c��@c�@c33@c@c@b�!@a�@ax�@a�@`�9@`bN@_�@_l�@^ȴ@^E�@^{@^@]@]��@]�h@]/@\��@[�m@[dZ@Z�H@Z�\@Y�@Yx�@Y7L@Y�@X��@XĜ@X�9@XbN@X �@W�;@W;d@VV@V5?@V$�@U�@U@U/@T�@T�@SS�@R�!@R�\@Rn�@R�@RJ@Q��@Q��@Q�@Q��@Qhs@Q�@Q�@P�`@P�9@P�u@P  @O|�@O;d@N��@N5?@N{@M�T@M��@M`B@M?}@MV@L�@L�@Kƨ@K��@K�@KS�@K"�@K@K@J�H@J��@J��@J��@J�\@J^5@Ix�@H�9@H�@HA�@G�;@G�w@G|�@Fȴ@F��@Fff@F$�@F@E�@E�T@E�@E�@D�/@D�D@Dj@DZ@DI�@D(�@D1@C��@Cƨ@C��@CC�@C"�@Bn�@@��@@r�@@b@?�@?;d@?
=@>��@>ȴ@>��@>��@>v�@>E�@>�+@>E�@=�T@=�h@=V@;�m@;dZ@:��@:~�@:n�@:^5@:n�@:�@9��@9�7@9hs@9X@9&�@8��@8�9@8r�@8 �@7��@7l�@7;d@7
=@6�@6��@6{@5�@5`B@4�@4z�@41@3�
@3�F@3��@3��@3��@3ƨ@3ƨ@3��@3t�@3"�@2�\@2�@1�#@1��@1G�@1&�@0Ĝ@0�@0bN@01'@01'@0A�@0A�@0A�@0  @/�w@/|�@/+@.�R@.v�@.V@.5?@-�@-/@,��@,�D@,I�@,�@+�m@+�F@+�@+t�@+S�@+C�@*~�@)��@)7L@)&�@)&�@)%@(�9@( �@(b@'��@'|�@'K�@&�y@&ff@&@%@%O�@%?}@%?}@$�@$��@$��@$9X@#ƨ@#�F@#��@#t�@#"�@"�H@"��@"�!@"�!@"��@"��@"�\@"~�@"n�@"^5@"^5@"M�@"=q@"�@"J@!�@!��@!��@!��@!X@!�@!%@ �`@ Ĝ@ �u@ bN@ A�@�;@��@��@��@��@��@�@|�@l�@
=@��@ff@5?@{@@�@�@�@�T@�-@�h@�@p�@O�@/@�@��@�j@z�@I�@�F@dZ@@n�@=q@-@�@��@�7@7L@�`@��@bN@��@�P@l�@K�@+@�@ff@E�@$�@@�@�@��@?}@V@�@z�@j@9X@1@�m@�
@��@dZ@o@��@�!@��@�\@�\@-@�7@�@��@Ĝ@��@��@�u@�@bN@A�@1'@ �@��@|�@\)@��@��@��@��@��@E�@5?@5?@$�@{@@�-@��@�h@`B@/@�@�j@�D@9X@�@1@��@��@�@�@dZ@S�@33@o@
��@
n�@
=q@	��@	��@	��@	x�@	7L@	&�@	%@��@ �@�;@�w@��@|�@\)@K�@K�@�@�y@��@E�@�@�T@@�-@�h@`B@/@��@��@�@��@�j@�@�@�D@9X@�@�
@��@dZ@C�@33@o@�@�H@�H@�!@J@��@��@�#@��@�7@G�@7L11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B!�B!�B!�B!�B �B �B �B �B �B �B �B �B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BoB1B�yB�LB�BG�B	7B�9B�hB�Bm�B`BB>wB49B&�B�B\BVBhBDB
��B
��B
�B
�B
�B
�5B
��B
��B
�}B
�FB
�B
��B
��B
��B
��B
��B
�PB
�B
{�B
w�B
s�B
cTB
YB
T�B
N�B
J�B
G�B
=qB
9XB
5?B
.B
#�B
uB
oB
uB
{B
oB
\B
VB
JB
	7B
B
B
B	��B	��B	��B	��B	�B	�BB	�B	��B	��B	��B	ǮB	ƨB	B	�jB	�?B	�FB	�!B	��B	��B	��B	��B	��B	��B	�DB	~�B	}�B	y�B	t�B	o�B	cTB	\)B	W
B	R�B	E�B	A�B	A�B	A�B	A�B	<jB	33B	.B	)�B	#�B	�B	 �B	�B	�B	�B	bB	PB		7B	B��B��B��B��B��B��B��B��B��B��B�B�B�B�/B�BB�5B�B��B��B��BȴB�}B�^B�wB�XB�B�3B�B�-B�B��B��B��B��B��B��B��B��B��B��B��B�uB�PB�+B�B�bB�\B�VB�1B�Bx�Bs�Bo�Bl�Bo�Bm�Bo�Bn�Bo�Bo�BjBe`Bl�Bm�Bm�Bm�Bl�BjBhsBhsBdZBdZBcTBcTBaHBcTB_;B^5B\)BN�BG�BA�BI�BK�BL�BB�B@�B=qB>wB@�B<jB0!B7LB:^B6FB<jB:^B=qB<jB8RB1'B7LB9XB6FB-B,B-B49B8RB9XB9XB8RB7LB33B,B,B-B.B0!B+B%�B+B.B&�B �B&�B)�B9XB;dB=qB@�B@�B>wB:^B;dB9XBD�BD�BD�BC�BD�BF�BF�BF�BF�BF�BE�BC�BC�BB�BB�BF�BD�BA�B@�B9XBB�BI�BN�BO�BQ�BR�BP�BN�BP�BM�BL�BW
BQ�BG�BL�BO�BO�BT�BT�BQ�BK�B[#B^5B]/B\)BaHBbNBdZBjBk�Bk�BffBiyBm�Bo�Br�Bw�Bx�Bz�B� B�B�B�B�B�%B�7B�\B�bB�hB�bB�\B�bB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B��B�3B�XB�}BȴB��B��B��B��B��B��B��B�B�;B�BB�BB�HB�ZB�B�B��B�B��B��B��B��B��B��B	B	B	%B	VB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	&�B	+B	,B	,B	.B	5?B	9XB	;dB	<jB	<jB	=qB	>wB	>wB	B�B	C�B	C�B	C�B	D�B	C�B	C�B	E�B	K�B	K�B	L�B	L�B	M�B	N�B	N�B	N�B	P�B	R�B	T�B	VB	VB	VB	VB	W
B	XB	]/B	`BB	bNB	bNB	e`B	ffB	hsB	iyB	iyB	k�B	o�B	p�B	q�B	r�B	r�B	r�B	t�B	w�B	y�B	{�B	|�B	|�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�JB	�VB	�VB	�VB	�hB	�oB	�uB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�!B	�'B	�-B	�9B	�XB	�jB	�wB	�qB	��B	B	B	B	ŢB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�)B	�5B	�;B	�BB	�BB	�BB	�BB	�BB	�;B	�BB	�NB	�ZB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
1B
+B
+B
%B
+B

=B

=B

=B
DB
DB
DB
VB
VB
VB
VB
\B
\B
\B
bB
hB
oB
oB
uB
uB
uB
uB
uB
uB
uB
oB
oB
hB
bB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
%�B
&�B
$�B
$�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
+B
+B
,B
-B
,B
-B
,B
,B
,B
,B
-B
,B
-B
-B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
/B
/B
0!B
0!B
2-B
33B
2-B
2-B
1'B
2-B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
49B
5?B
6FB
8RB
8RB
8RB
8RB
7LB
9XB
9XB
8RB
8RB
8RB
8RB
:^B
;dB
:^B
<jB
<jB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
@�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
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
E�B
D�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
M�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
P�B
R�B
S�B
S�B
S�B
R�B
R�B
T�B
T�B
T�B
T�B
VB
T�B
T�B
VB
VB
W
B
W
B
W
B
W
B
XB
XB
W
B
XB
XB
XB
YB
ZB
ZB
ZB
XB
XB
YB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
^5B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B!�B!�B!�B!�B �B �B �B �B �B �B �B �B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B&B
=B��B��B��BO�B4B�"B�
B��BpUBb�BC{B6zB)�B!|BBBB�B
�HB
��B
��B
�B
�B
��B
͟B
�<B
��B
��B
�)B
�mB
�bB
��B
��B
�KB
�B
��B
|�B
x�B
t�B
e�B
Z�B
VmB
PHB
K�B
H�B
?.B
:DB
6+B
/5B
%�B
9B
�B
�B
�B
�B
�B
�B
�B
	�B
�B
�B
�B	�}B	�<B	�B	�DB	�'B	�4B	��B	ҽB	�6B	ˬB	ȀB	�+B	�-B	��B	�`B	��B	�AB	�ZB	�!B	��B	�1B	��B	�9B	��B	��B	~�B	z�B	utB	p�B	e,B	]�B	X_B	TaB	G�B	C-B	BuB	BB	A�B	="B	4nB	.�B	+B	%,B	 BB	!�B	 vB	�B	yB	�B	<B	
=B	9B��B�B�]B�HB�VB��B��B�8B�2B�+B�3B�[B��B�;B�-B�!B�=B�mB��B��B��B�UB�B�cB��B�!B�9B��B��B��B��B��B�zB�FB�&B�HB�BB�!B�B�B�KB�{B��B�B��B�}B��B��B�B�aBz�Bu�Bq�Bn}Bp�Bn�Bp�BoOBpBpBk�Bf2Bl�Bm�Bm�Bm�Bl�BkBi*BiBeFBeFBd@Bd&Bb4Bc�B`BB_B]IBQ4BJrBD�BKBL�BM�BD�BB[B?}B?�BA�B=�B2�B8�B;�B7�B=<B;0B=�B<�B9$B2�B8B9�B7B/ B-�B.�B4�B8�B9�B9�B8�B7�B3�B-CB-)B.B.�B0�B,=B'�B,"B/ B(�B# B(�B,=B9�B<B=�B@�B@�B>�B;0B<B:xBD�BD�BD�BC�BD�BF�BF�BF�BF�BF�BE�BC�BC�BCBB�BF�BD�BB'BAUB:�BC�BJ#BN�BO�BQ�BR�BQ�BO�BQ�BN�BN"BW$BR�BI7BM�BP}BP}BUgBUMBR�BMPB[WB^jB]�B]Ba�Bb�Bd�BkBl=Bl"Bg�BjKBnIBpoBs3Bx8By>B{JB�4B�;B�UB�[B��B��B��B�\B�}B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�B� B�vB�nB�DB�]B�/B�cB�OB�OB�wB�cB��B��B�B�B�*B�4B��B��B�B��B�B�"B�VB�oB�eB�VB�vB�\B�bB�B�B��B��B�3B�	B�2B�8B�JB�JB�PB	UB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 B	#B	$&B	'8B	+6B	,"B	,WB	.}B	5�B	9�B	;B	<�B	<�B	=�B	>�B	>�B	B�B	C�B	C�B	C�B	D�B	C�B	C�B	E�B	K�B	K�B	MB	MB	NB	N�B	N�B	OB	QB	SB	UB	VB	VB	V9B	V9B	WsB	X�B	]�B	`vB	bhB	b�B	ezB	f�B	h�B	i�B	i�B	k�B	o�B	p�B	q�B	r�B	r�B	r�B	t�B	xB	y�B	|B	}B	}"B	B	�B	� B	� B	�'B	�'B	�'B	�AB	�'B	�[B	�{B	��B	�~B	�pB	��B	��B	��B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�fB	��B	�CB	�OB	�;B	�;B	�UB	�[B	�aB	�nB	�XB	��B	�wB	��B	��B	ªB	��B	��B	ŢB	ƨB	��B	��B	��B	��B	��B	�B	�B	��B	�B	�(B	�B	�4B	�4B	� B	�&B	�B	�B	�B	�B	�+B	�_B	�_B	�QB	�kB	�WB	�WB	�qB	�]B	�xB	�OB	�VB	�\B	�\B	�\B	�\B	�vB	�pB	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	��B	�"B	�B	�B	�<B	�"B	��B	�(B	�B
 B
 B
 B
 B
'B
;B
AB
AB
3B
9B
?B
?B
?B
EB
+B
_B
EB
fB
1B
_B
_B
tB
zB

XB

XB

rB
^B
xB
xB
pB
pB
pB
pB
\B
�B
�B
�B
�B
�B
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
 �B
 �B
!�B
"�B
$B
#�B
#�B
#�B
$B
$�B
$�B
%B
$�B
%�B
'B
%�B
'G�O�B
%B
'B
'B
($B
($B
)B
)�B
)�B
+B
+B
,B
-B
,"B
-)B
,=B
,=B
,=B
,"B
-)B
,=B
-)B
-CB
/B
/5B
0;B
0!B
0!B
0!B
0;B
0;B
/5B
/5B
0;B
0UB
2GB
3hB
2GB
2GB
1vB
2aB
3hB
4TB
49B
5ZB
5ZB
5ZB
5ZB
5tB
5ZB
4�B
5�B
6�B
8RB
8RB
8�B
8lB
7�B
9rB
9rB
8lB
8lB
8�B
8�B
:xB
;B
:�B
<jB
<jB
<�B
=�B
=�B
=�B
>�B
?}B
?�B
?�B
?�B
@�B
B�B
B�B
B�B
BuB
C�B
C{B
C�B
C{B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
D�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
M�B
MB
L�B
M�B
M�B
M�B
M�B
M�B
M�B
MB
N�B
OB
OB
PB
Q B
Q B
QB
Q B
QB
Q B
RB
QB
QB
SB
TB
T,B
TB
SB
SB
T�B
UB
UB
T�B
VB
UB
UB
VB
VB
W$B
W$B
W$B
W$B
X+B
X+B
W$B
XEB
XEB
X+B
YB
ZG�O�B
Z7G�O�B
XEB
Y1B
[=B
[=B
\CB
\)B
\)B
\)B
\CB
\CB
]/B
]IB
\]B
\CB
]IB
]dB
]IB
^5B
^OB
^5B
^OB
_VB
_;B
_;B
_!B
_;B
^OB
`BB
`\B
`\B
`\B
`\B
`\B
abB
abB
bNB
bhB
bhB
bhB
bhB
cTB
cnB
cnB
cnB
cnB
cnB
cnB
cTB
dtB
ezB
e`B
ezB
ezB
ezB
ezB
e�B
e�B
f�B
g�B
g�B
h�B
h�B
iyB
iyB
i�B
i�B
i�B
i�B
iyB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
lqB
l�B
m�B
m�B
m�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712280032552017122800325520171228003255201806221235192018062212351920180622123519201804050431362018040504313620180405043136  JA  ARFMdecpA19c                                                                20171224093517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171224003519  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171224003522  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171224003522  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171224003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171224003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171224003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171224003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171224003523  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171224003523                      G�O�G�O�G�O�                JA  ARUP                                                                        20171224005510                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171224153524  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20171227153255  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171227153255  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20180104000000  CF  PSAL_ADJUSTED_QCD�  D߀ G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193136  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033519  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                