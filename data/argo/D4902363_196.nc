CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-05T00:35:19Z creation;2018-01-05T00:35:23Z conversion to V3.1;2019-12-19T07:52:33Z update;     
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180105003519  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_196                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�B5�JU�1   @�B6Q�n @;M�b���dVz���1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�<�Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�3D�0 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @8��@\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B�.B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!x�D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,��D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9x�D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DAx�DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�<{D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��D���D���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D���D���D��D�/�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�-A�1'A�1'A�1'A�1'A�1'A�1'A�/A�+A��A���A��A�`BA�E�A��A��!A���A��A�t�A�ffA�XA�;dA�-A���A�9XA���A�;dA�C�A�33A�G�A�%A���A�M�A�VA��A���A���A���A��A�C�A��!A�bA�r�A�1A�{A��#A�/A�\)A�v�A�
=A�p�A��A�n�A�`BA���A�ȴA���A��DA��jA���A���A� �A���A�;dA��;A�XA�-A��A��-A��A�/A�Q�A�p�A~�A~bA|��A{��A{�AzbNAyƨAy%Aw��Avr�Au33At�!As�;As/As�As
=Ar��Ar1Ap��Ao�mAn��Am"�Aj�HAjr�Ai��Ai��Ai`BAi?}Ai�Ah��Ah�HAhQ�Ae�TAd�Acx�AbA�AaAadZA`�RA_��A_C�A^��A^�jA^I�A]�7A\�!A[AW�AVjAU�#AUx�AU&�AT�HATn�ASt�ASAPȴAP �AO�7AO�AN��AN{AM��AM
=AK�AI��AH(�AG%AF~�AE��AD�\ACƨACO�AA�AAA@ĜA@5?A?��A?�A?��A?��A>��A<Q�A9�TA8ĜA7%A5�PA4jA3��A2��A2^5A2bA1t�A0��A0-A/�
A/hsA/7LA.�\A.{A-�
A-�A-p�A,��A+A+?}A*r�A*bA)��A(�\A'�mA'|�A&I�A%�TA$�\A#�^A#A"ffA!x�A �A ȴA ��A VA bA�A��A�-A��A?}AE�A��AbNA��A��A�A��AM�A?}AoA�Av�AJAM�A?}A�DAhsA��An�A�FA�A^5A��A��Ap�A7LAVA�9A�A&�AffA
�A	�^A	+A��A��A\)A�!AM�AA��A�-A ��A @��@�V@��7@��D@���@�;d@�
=@��@��R@�n�@�5?@��T@�/@�G�@�b@�V@�b@�!@�w@�u@�+@�ff@��@�-@�&�@��`@���@�D@��
@�o@�1'@�?}@޸R@ܣ�@��m@�t�@���@��@��@�I�@�v�@�Z@ύP@���@ΰ!@�^5@��T@ͺ^@�Q�@ʏ\@ɑh@ȃ@�33@�5?@Ų-@��@�=q@�&�@�r�@� �@�dZ@���@��T@�z�@�+@��H@���@��@��y@�ȴ@��!@��+@��-@�b@���@��@��7@�p�@�p�@�hs@�7L@���@��
@���@��@��#@��h@�%@�Q�@�b@�  @��
@���@�S�@���@�@��@���@�1'@��@�O�@��@�"�@�C�@�
=@���@��\@��@��T@��-@�`B@�&�@���@�bN@��m@��;@�l�@�{@���@�b@��;@�ƨ@��w@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@��w@���@�@��@��@��@��@��u@���@�@�ff@�$�@�J@�x�@�Z@��@��\@�v�@�V@�$�@���@��@�A�@�+@�~�@�V@�M�@�{@��h@�/@��/@�1@��;@�ƨ@���@���@���@�t�@�l�@�dZ@�S�@�S�@�;d@�C�@�@���@��@��/@�z�@�1@��
@��@�|�@�S�@�33@�o@���@�E�@�p�@���@�j@�1'@��@��m@��
@��@�"�@���@��\@�~�@�^5@�@��#@��^@���@�hs@�`B@�X@�G�@�&�@��@��9@��@�j@�I�@�(�@|�@~�y@~�R@~��@~��@~V@~$�@}��@}��@}�@}p�@}O�@}V@|j@|1@|(�@|9X@|Z@|I�@{�F@{dZ@{o@z=q@yhs@y7L@x�9@w�@w|�@w\)@v��@v��@vv�@vE�@v{@u�@u@t��@t9X@s�
@s�@s"�@r��@r��@r�!@rn�@rJ@qX@pb@oK�@o+@o
=@nv�@nff@nV@n5?@n{@n@m�T@m@m`B@l��@l�@l��@l�@mO�@m`B@m�@l��@l�D@l(�@l1@k��@k��@k�m@k��@k��@k��@l1@l1@l�@l�@l�@k��@j��@jn�@jn�@j~�@j^5@j-@i�#@i�7@iG�@hĜ@h��@h�u@h�`@h��@h�u@g�@g�P@g\)@g+@g�@f��@fȴ@fV@e��@d�/@d(�@c��@c33@c@b�H@b^5@a��@a�7@ahs@`Ĝ@`Q�@`  @_�@_;d@^�R@^v�@^V@]p�@\�@\�@[t�@[t�@[dZ@["�@Z�@Z��@ZJ@Y�7@Yx�@YG�@X�`@X �@W�w@W�@W|�@WK�@W
=@V��@VE�@V@U�-@U�@U`B@U?}@UV@T�@T�/@T�@Tz�@T9X@Sƨ@S��@SdZ@S33@S@R��@Rn�@Q��@Qhs@Qhs@QX@P�@P �@O�;@P  @Pb@O|�@O
=@N�@N�+@NV@M�@MO�@MV@L��@L��@L�@Kƨ@KS�@KS�@K33@Ko@K@J�@J�@J�H@J��@J�!@J�\@J~�@J~�@IX@H�@HA�@H1'@H  @G�@G�@Gl�@G
=@FV@E�@E@E`B@E�@EV@D�@D�@D(�@C�F@C33@B�H@A��@A��@A7L@@�`@@�`@@�`@@��@@�`@@�`@@��@@��@@��@@�@@r�@@Q�@@1'@@  @?��@?��@?\)@>ȴ@>��@>ff@>$�@=��@=�@=`B@=?}@=/@<��@<��@<�/@<(�@;"�@:�H@:�\@:=q@:J@9��@9�7@9�7@9�7@9X@8Ĝ@8�9@8�u@8�@8A�@7�w@7|�@7�@6�R@6E�@5�@5O�@4��@4I�@3��@3ƨ@3��@3t�@3C�@2�H@2��@2^5@1�@1hs@1�@0��@0bN@/�;@/�w@/|�@/;d@/+@/�@.��@.�@.�R@.��@.��@.�+@.v�@.V@.@-��@-p�@-V@,�@,Z@,I�@+��@+S�@*�@*�\@)�@)�^@)��@)�7@)7L@(��@(�9@(��@(��@(�u@(�@(Q�@(b@'l�@'\)@';d@&�y@&�@&�@&��@&�+@&V@&@%��@%�-@%�h@$�j@$j@$Z@$I�@$9X@$9X@$(�@#��@#�
@#ƨ@#C�@"�@"��@"��@"-@!��@!�7@!�7@!hs@!�@ ��@ ��@ ��@ ��@ ��@ �@ 1'@   @�;@�@�P@|�@\)@+@��@�@ȴ@��@ff@V@5?@@p�@��@�j@�D@��@dZ@33@"�@�H@��@��@�!@~�@J@�#@�^@��@hs@7L@%@�9@�u@�@bN@ �@  @�w@�P@\)@�R@ff@5?@$�@@��@p�@�/@�j@��@(�@dZ@C�@@�H@��@�\@n�@^5@=q@-@��@�#@�7@�@�`@Ĝ@�u@�@r�@bN@A�@1'@ �@b@�@�@|�@\)@
=@��@�@��@5?@$�@{@�@�@p�@/@�/@�D@�@�
@ƨ@�@C�@o@
�@
��@
�\@
M�@
�@
J@	�^@	�7@	G�@	X@	7L@	7L@	7L@	&�@Ĝ@�u@r�@Q�@  @�w@|�@\)@;d@��@�y@ȴ@�+@$�@�@@�@p�@`B@?}@�@��@�D@j@9X@(�@�@1@��@�
@ƨ@ƨ@ƨ@ƨ@ƨ@�F@��@��@�@t�@S�@"�@"�@"�@33@o@�H@��@��@�!@~�@n�@M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�-A�1'A�1'A�1'A�1'A�1'A�1'A�/A�+A��A���A��A�`BA�E�A��A��!A���A��A�t�A�ffA�XA�;dA�-A���A�9XA���A�;dA�C�A�33A�G�A�%A���A�M�A�VA��A���A���A���A��A�C�A��!A�bA�r�A�1A�{A��#A�/A�\)A�v�A�
=A�p�A��A�n�A�`BA���A�ȴA���A��DA��jA���A���A� �A���A�;dA��;A�XA�-A��A��-A��A�/A�Q�A�p�A~�A~bA|��A{��A{�AzbNAyƨAy%Aw��Avr�Au33At�!As�;As/As�As
=Ar��Ar1Ap��Ao�mAn��Am"�Aj�HAjr�Ai��Ai��Ai`BAi?}Ai�Ah��Ah�HAhQ�Ae�TAd�Acx�AbA�AaAadZA`�RA_��A_C�A^��A^�jA^I�A]�7A\�!A[AW�AVjAU�#AUx�AU&�AT�HATn�ASt�ASAPȴAP �AO�7AO�AN��AN{AM��AM
=AK�AI��AH(�AG%AF~�AE��AD�\ACƨACO�AA�AAA@ĜA@5?A?��A?�A?��A?��A>��A<Q�A9�TA8ĜA7%A5�PA4jA3��A2��A2^5A2bA1t�A0��A0-A/�
A/hsA/7LA.�\A.{A-�
A-�A-p�A,��A+A+?}A*r�A*bA)��A(�\A'�mA'|�A&I�A%�TA$�\A#�^A#A"ffA!x�A �A ȴA ��A VA bA�A��A�-A��A?}AE�A��AbNA��A��A�A��AM�A?}AoA�Av�AJAM�A?}A�DAhsA��An�A�FA�A^5A��A��Ap�A7LAVA�9A�A&�AffA
�A	�^A	+A��A��A\)A�!AM�AA��A�-A ��A @��@�V@��7@��D@���@�;d@�
=@��@��R@�n�@�5?@��T@�/@�G�@�b@�V@�b@�!@�w@�u@�+@�ff@��@�-@�&�@��`@���@�D@��
@�o@�1'@�?}@޸R@ܣ�@��m@�t�@���@��@��@�I�@�v�@�Z@ύP@���@ΰ!@�^5@��T@ͺ^@�Q�@ʏ\@ɑh@ȃ@�33@�5?@Ų-@��@�=q@�&�@�r�@� �@�dZ@���@��T@�z�@�+@��H@���@��@��y@�ȴ@��!@��+@��-@�b@���@��@��7@�p�@�p�@�hs@�7L@���@��
@���@��@��#@��h@�%@�Q�@�b@�  @��
@���@�S�@���@�@��@���@�1'@��@�O�@��@�"�@�C�@�
=@���@��\@��@��T@��-@�`B@�&�@���@�bN@��m@��;@�l�@�{@���@�b@��;@�ƨ@��w@�ƨ@�ƨ@�ƨ@�ƨ@�ƨ@��w@���@�@��@��@��@��@��u@���@�@�ff@�$�@�J@�x�@�Z@��@��\@�v�@�V@�$�@���@��@�A�@�+@�~�@�V@�M�@�{@��h@�/@��/@�1@��;@�ƨ@���@���@���@�t�@�l�@�dZ@�S�@�S�@�;d@�C�@�@���@��@��/@�z�@�1@��
@��@�|�@�S�@�33@�o@���@�E�@�p�@���@�j@�1'@��@��m@��
@��@�"�@���@��\@�~�@�^5@�@��#@��^@���@�hs@�`B@�X@�G�@�&�@��@��9@��@�j@�I�@�(�@|�@~�y@~�R@~��@~��@~V@~$�@}��@}��@}�@}p�@}O�@}V@|j@|1@|(�@|9X@|Z@|I�@{�F@{dZ@{o@z=q@yhs@y7L@x�9@w�@w|�@w\)@v��@v��@vv�@vE�@v{@u�@u@t��@t9X@s�
@s�@s"�@r��@r��@r�!@rn�@rJ@qX@pb@oK�@o+@o
=@nv�@nff@nV@n5?@n{@n@m�T@m@m`B@l��@l�@l��@l�@mO�@m`B@m�@l��@l�D@l(�@l1@k��@k��@k�m@k��@k��@k��@l1@l1@l�@l�@l�@k��@j��@jn�@jn�@j~�@j^5@j-@i�#@i�7@iG�@hĜ@h��@h�u@h�`@h��@h�u@g�@g�P@g\)@g+@g�@f��@fȴ@fV@e��@d�/@d(�@c��@c33@c@b�H@b^5@a��@a�7@ahs@`Ĝ@`Q�@`  @_�@_;d@^�R@^v�@^V@]p�@\�@\�@[t�@[t�@[dZ@["�@Z�@Z��@ZJ@Y�7@Yx�@YG�@X�`@X �@W�w@W�@W|�@WK�@W
=@V��@VE�@V@U�-@U�@U`B@U?}@UV@T�@T�/@T�@Tz�@T9X@Sƨ@S��@SdZ@S33@S@R��@Rn�@Q��@Qhs@Qhs@QX@P�@P �@O�;@P  @Pb@O|�@O
=@N�@N�+@NV@M�@MO�@MV@L��@L��@L�@Kƨ@KS�@KS�@K33@Ko@K@J�@J�@J�H@J��@J�!@J�\@J~�@J~�@IX@H�@HA�@H1'@H  @G�@G�@Gl�@G
=@FV@E�@E@E`B@E�@EV@D�@D�@D(�@C�F@C33@B�H@A��@A��@A7L@@�`@@�`@@�`@@��@@�`@@�`@@��@@��@@��@@�@@r�@@Q�@@1'@@  @?��@?��@?\)@>ȴ@>��@>ff@>$�@=��@=�@=`B@=?}@=/@<��@<��@<�/@<(�@;"�@:�H@:�\@:=q@:J@9��@9�7@9�7@9�7@9X@8Ĝ@8�9@8�u@8�@8A�@7�w@7|�@7�@6�R@6E�@5�@5O�@4��@4I�@3��@3ƨ@3��@3t�@3C�@2�H@2��@2^5@1�@1hs@1�@0��@0bN@/�;@/�w@/|�@/;d@/+@/�@.��@.�@.�R@.��@.��@.�+@.v�@.V@.@-��@-p�@-V@,�@,Z@,I�@+��@+S�@*�@*�\@)�@)�^@)��@)�7@)7L@(��@(�9@(��@(��@(�u@(�@(Q�@(b@'l�@'\)@';d@&�y@&�@&�@&��@&�+@&V@&@%��@%�-@%�h@$�j@$j@$Z@$I�@$9X@$9X@$(�@#��@#�
@#ƨ@#C�@"�@"��@"��@"-@!��@!�7@!�7@!hs@!�@ ��@ ��@ ��@ ��@ ��@ �@ 1'@   @�;@�@�P@|�@\)@+@��@�@ȴ@��@ff@V@5?@@p�@��@�j@�D@��@dZ@33@"�@�H@��@��@�!@~�@J@�#@�^@��@hs@7L@%@�9@�u@�@bN@ �@  @�w@�P@\)@�R@ff@5?@$�@@��@p�@�/@�j@��@(�@dZ@C�@@�H@��@�\@n�@^5@=q@-@��@�#@�7@�@�`@Ĝ@�u@�@r�@bN@A�@1'@ �@b@�@�@|�@\)@
=@��@�@��@5?@$�@{@�@�@p�@/@�/@�D@�@�
@ƨ@�@C�@o@
�@
��@
�\@
M�@
�@
J@	�^@	�7@	G�@	X@	7L@	7L@	7L@	&�@Ĝ@�u@r�@Q�@  @�w@|�@\)@;d@��@�y@ȴ@�+@$�@�@@�@p�@`B@?}@�@��@�D@j@9X@(�@�@1@��@�
@ƨ@ƨ@ƨ@ƨ@ƨ@�F@��@��@�@t�@S�@"�@"�@"�@33@o@�H@��@��@�!@~�@n�@M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BhBbBbBbBbB\BPB
=BB�B�
B��BPBJBVB{B{B{B�B{BoBbBPB  B�B�HBĜB�^B�B�B�B�sB�mB�B�B�B�mB�ZB�/B��B�qB��Bx�B7LBbB�BŢB��B�\B�Bz�Bm�BR�B/B�BuB1B
��B
��B
�B
�TB
�B
��B
��B
ŢB
B
�}B
�^B
�-B
��B
��B
�uB
�7B
�B
}�B
t�B
p�B
n�B
jB
dZB
\)B
S�B
K�B
F�B
B�B
=qB
<jB
;dB
8RB
33B
,B
+B
#�B
�B
bB
PB

=B
1B
%B
B
B
B	��B	��B	�B	�`B	�HB	�#B	�B	�B	��B	��B	��B	ȴB	ƨB	B	�jB	�?B	�B	��B	�{B	�oB	�hB	�\B	�JB	�1B	�B	}�B	s�B	r�B	o�B	m�B	jB	gmB	cTB	^5B	T�B	I�B	C�B	@�B	=qB	;dB	1'B	/B	-B	&�B	#�B	$�B	"�B	!�B	!�B	 �B	�B	�B	JB��B��B�B�B�mB�`B�HB�BB�5B�#B�B��B�B��B��B��B��B��B��BɺBĜB�}B�qB�jB�dB�RB�3B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�7B�%B�7B�7B�7B�7B�B�B}�B�B�B}�Bz�Bs�Bp�Bq�Bm�Bn�Bl�BjBhsBiyBhsBjBjBiyBhsBffBbNB`BB^5BYBW
BYBW
BW
BT�BR�BQ�BL�BB�B?}BF�BF�BH�BH�BH�BG�BG�BH�BH�BH�BH�BG�BG�BF�BD�BD�B@�B;dB7LB7LB49B2-B9XB;dB<jB=qB<jB<jB<jB;dB9XB6FB0!B)�B,B-B49B49B/B0!B0!B1'B2-B0!B7LB;dB>wB>wB>wB=qB9XB7LB:^B:^B:^B;dB;dB8RB/B"�B1'B49B33B2-B2-B33B7LBA�BG�BH�BH�BI�BJ�BI�BG�BG�BJ�BQ�BS�BVBVBVBT�BS�BQ�BR�BZB\)B\)B\)B]/B`BBaHBaHBaHBaHBaHBe`Bk�Bu�Bu�Bs�Bw�Bw�B� B�B�%B�7B�DB�JB�\B�bB�bB�hB�hB�bB�bB�bB�bB�JB�PB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�'B�?B�jB�}B�qB�dB�qBBŢBĜBÖBB��BBŢB��B��B��B��B��B�B�#B�/B�TB�ZB�`B�`B�fB�fB�mB�mB�mB�mB�sB�sB�mB�yB�yB�B��B��B��B��B��B	  B	B	B	B	B	B		7B	\B	oB	uB	uB	�B	�B	�B	�B	 �B	 �B	 �B	 �B	$�B	&�B	(�B	)�B	,B	-B	-B	.B	/B	2-B	49B	5?B	5?B	6FB	6FB	9XB	<jB	>wB	>wB	>wB	?}B	@�B	B�B	D�B	E�B	F�B	F�B	G�B	G�B	K�B	M�B	M�B	M�B	L�B	O�B	O�B	O�B	P�B	Q�B	Q�B	T�B	YB	[#B	[#B	\)B	^5B	_;B	`BB	`BB	aHB	bNB	iyB	l�B	n�B	p�B	q�B	r�B	r�B	r�B	s�B	t�B	u�B	x�B	{�B	{�B	|�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�DB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�'B	�9B	�?B	�?B	�?B	�?B	�?B	�?B	�LB	�LB	�RB	�dB	�}B	ÖB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�#B	�)B	�)B	�5B	�;B	�BB	�HB	�NB	�TB	�TB	�NB	�ZB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B
B
B
B
B
B
B
%B
+B
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
	7B
1B
	7B
1B
+B
	7B
PB
VB
VB
\B
\B
bB
bB
bB
oB
uB
uB
{B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
(�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
,B
,B
,B
.B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
33B
33B
33B
49B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
8RB
8RB
9XB
:^B
:^B
<jB
=qB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
@�B
@�B
@�B
@�B
A�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
@�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
C�B
D�B
D�B
D�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
J�B
K�B
K�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
N�B
O�B
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
P�B
P�B
Q�B
Q�B
R�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
VB
T�B
T�B
T�B
T�B
W
B
W
B
VB
VB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
]/B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
`BB
`BB
`BB
_;B
`BB
`BB
`BB
`BB
aHB
bNB
cTB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
hsB
iyB
jB
jB
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BNBbBbBbBbB\BjB
�BSB��B�B�tB�BB�B�B�B�B�B�B�B�B"B�B�tB��B�?B��B�B��B�/B�yB��B�B��B��B�B��B�B�hB�cB��B�B<PB�B�B�^B��B�}B��B|6BpBW�B49B"NBB
�BB
�	B
�B
�zB
ؓB
�bB
ˬB
��B
�B
�4B
�JB
��B
�*B
��B
��B
�^B
�YB
cB
u�B
q�B
o�B
kkB
ezB
]�B
U�B
MB
GzB
C{B
>(B
<�B
;�B
8�B
4TB
-�B
,WB
%�B
�B
�B
�B

�B
�B
tB
MB
GB
UB	�}B	�*B	�IB	�B	�B	�xB	ٴB	֡B	��B	��B	˒B	�B	�B	�GB	��B	��B	�B	��B	��B	�&B	��B	��B	��B	�B	�GB	~�B	vB	s�B	poB	n/B	k6B	h$B	d&B	_VB	V�B	L0B	ESB	A�B	>BB	<PB	2�B	0!B	-�B	(�B	$�B	%FB	#nB	"B	!�B	!B	jB	B	�B	 �B��B��B�iB��B�B�NB��B��B�B�B՛BևBӏB�uBбB̈́B�6B�JB�XBŢB��B�BB�qB�B�$B��B�B��B�eB��B��B��B��B��B��B�KB��B��B��B��B��B��B��B��B�4B��B��B�	B��B��B��B�B�B.B�aB�uB~�B{�Bu�Br-Br�Bo BoOBm�Bk�BiyBjKBi*Bj�Bj�Bi�Bh�BgBcnBa|B_�B[	BX�BY�BW�BW�BU�BS�BR�BN"BEmBA�BG�BG�BIlBIRBIlBHfBHKBIBH�BH�BH�BG�BG�BGBEBD�BA�B<�B9	B8�B6FB4B:DB<B<�B=�B<�B<�B<�B;�B:B7LB2-B,"B-�B.cB4�B4�B0�B1vB1�B2�B3�B1vB7�B;�B>�B>�B>�B=�B:xB8�B;0B;JB;JB<B<B9rB1vB%�B1�B4�B3�B2�B2�B49B8BA�BG�BH�BH�BI�BJ�BJ#BH�BH�BK�BRoBTFBVBVBV9BUMBT{BR�BS�BZkB\xB\xB\�B]�B`vBaHBa|Ba�Ba�Ba�BfBl"BvBv�Bu%Bx�Bx�B�4B�3B�tB��B�xB��B��B��B��B��B��B��B��B��B��B�jB�"B� B��B��B��B��B��B��B��B��B��B�B�\B��B�yB�wB�cB�UB��B��B��B��B��B�B�PB�BB��BżB��B��B��B�AB�GB�tB�6B�B�B�FB�gB�yBیB��B�nB�tB�zB�zB�B�B�B�B�B�B�B�B�B��B�B�qB�B�*B�"B�(B�.B	 4B	;B	;B	UB	�B	�B		�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	 �B	!B	%B	'B	)B	*0B	,"B	-)B	-)B	.IB	/OB	2aB	4�B	5ZB	5tB	6zB	6�B	9�B	<jB	>�B	>�B	>�B	?�B	@�B	B�B	D�B	E�B	F�B	F�B	G�B	G�B	K�B	M�B	M�B	NB	MB	PB	PB	PHB	Q4B	RB	R:B	UMB	YKB	[=B	[WB	\CB	^OB	_VB	`\B	`\B	a|B	b�B	i�B	l�B	n�B	p�B	q�B	r�B	r�B	r�B	s�B	uB	v+B	y$B	|B	|B	}"B	�B	�'B	�-B	�3B	�9B	�?B	�EB	�fB	�xB	�bB	��B	�uB	�B	��B	��B	�B	��B	�B	�B	�"B	�B	�)B	�B	�B	�B	�B	�!B	�'B	�'B	�GB	�aB	��B	�TB	�ZB	�?B	�ZB	�ZB	�ZB	�tB	�fB	��B	�lB	�dB	�cB	ðB	��B	��B	��B	�B	��B	�B	��B	��B	�B	�<B	�(B	� B	�&B	�2B	�B	�
B	�?B	�KB	�=B	�CB	�xB	�jB	�VB	�vB	�|B	�B	�nB	�nB	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�	B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�<B	�<B	�B	�B	��B	�BB	�B
 B
B
 B
 4B
AB
-B
3B
3B
MB
MB
?B
EB
zB
_B
KB
KB
1B
fB
KB
1B
1B
KB
KB
fB
	7B
KB
	RB
1B
�B
	�B
jB
pB
VB
vB
\B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
 B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%B
(�B
($B
(
B
(
B
($B
)B
)*B
)*B
)*B
*KB
,"B
,=B
,WB
./B
/5B
/5B
0;B
0;B
0;B
0;B
1AB
1vB
1[B
2GB
3MB
3hB
3hB
4TB
5ZB
5ZB
6FB
6`B
6`B
7fB
7fB
7�B
7LB
7LB
7fB
7fB
7�B
7fB
7�B
7�B
8lB
8lB
9�B
8�B
8�B
9�B
:�B
:�B
<�B
=qB
<�B
<�B
<�B
=�B
>wB
>wB
>�B
>�B
>�B
>�B
>�B
@�B
@�B
@�B
@�B
A�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
@�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
C�B
D�B
D�B
D�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
J�B
K�B
K�B
J�B
K�B
K�B
L�B
L�B
MB
MB
N�B
O�B
OB
O�B
O�B
O�B
O�B
P.B
Q B
Q B
QB
Q B
Q B
Q B
R B
RB
R�B
RB
RB
S&B
SB
S&B
S&B
S@B
TB
T�B
VB
UB
UB
U2B
UMB
W?B
W$B
V9B
VSB
YB
Y1B
Y1B
Y1B
YKB
ZB
ZB
Z7B
ZQB
Z7B
Z7B
Z7B
ZQB
[=B
\CB
\]B
\CB
\CB
]IB
\CB
]/B
]/B
]IB
]IB
]IB
]IB
]IB
]IB
^5B
^OB
^jB
^�B
`BB
`\B
`\B
_pB
`\B
`\B
`\B
`\B
a|B
bhB
cTB
bhB
bhB
cnB
cnB
cnB
cnB
dtB
d�B
dZB
dtB
e�B
e�B
ffB
f�B
f�B
f�B
e�B
ezB
f�B
f�B
f�B
f�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
h�B
i�B
j�B
j�B
j�B
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�1111111113111111111111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801090034212018010900342120180109003421201806221235522018062212355220180622123552201804050432152018040504321520180405043215  JA  ARFMdecpA19c                                                                20180105093518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180105003519  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180105003521  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180105003521  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180105003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180105003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180105003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180105003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180105003522  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180105003523                      G�O�G�O�G�O�                JA  ARUP                                                                        20180105005522                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180105153156  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180108153421  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180108153421  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20180109000000  CF  PSAL_ADJUSTED_QCA�  B�  G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193215  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033552  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                