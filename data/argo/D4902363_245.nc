CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-31T21:35:04Z creation;2018-05-31T21:35:08Z conversion to V3.1;2019-12-19T07:41:00Z update;     
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
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΄   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180531213504  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_245                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�f�Y�k 1   @�f�W; @:�t��dB�a��f1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�<�DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�ɚD�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�{@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ��DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dm�Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�<{D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D�{D���D�B�D��D忮D���D�?�D��D濮D���D�?�D��D翮D��D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D���D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�B�D��D�D���D�<{D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��HD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A��A��A��A�{A�%A���A��/A��^A���A�x�A�G�A��yA�C�A�dZA�+A��DA�K�A�A�7LA���A�E�A�33A���A�?}A��-A�n�A�(�A��/A��-A��hA�x�A�K�A�r�A���A�ȴA���A��TA�
=A�G�A�t�A���A��FA���A�S�A���A���A���A� �A��A��A��FA�I�A��TA��A�ȴA��TA��\A�C�A��A��A�|�A���A�A�A�jA�I�A���A�S�A��A��uA�ffA�Q�A�E�A��A�VA��A���A�r�A�A�=qA�p�A~v�A|Ay"�AvȴAvbNAv=qAu?}As
=Ar$�Ap�`Ao��An�Am�;Al5?AjM�Ai��Ag"�Af  Ad�AdVAb1'Aa�A`��A_p�A^bA\��A[��AZ�AY�wAX�RAWdZAV^5AU��AUAT��ATM�AT$�AS�^ARȴARI�AQG�AP�\AO��AO?}AN��AM�;AM/AK��AJ1'AIdZAH��AHv�AGl�AE��AD�!AD�+ADI�ADJACl�ABM�A@��A@bA>I�A<v�A;?}A;
=A:��A:��A:A8��A8��A7��A6ffA5x�A4�/A4r�A4Q�A4JA3�
A3��A3G�A2��A2��A2�DA1�A0��A0{A/�#A/��A/C�A.�A.A,��A+��A*��A)"�A(A'x�A'VA&��A&VA%�
A%�7A%;dA$�`A$ȴA$�uA#�FA"z�A"I�A ĜAp�AȴA�+A�A��AȴA5?A�AA�!A-A��Al�AAE�A7LA
=A��A�#A�A�A^5A��A/A�AffA$�A �A�mA�A
��A
(�A	��A	`BA��A�Ap�A�A�!AbA�FAO�A��Av�A1A �@�hs@���@��@��@��@���@��@���@�C�@�(�@��@��@�X@�r�@�b@�F@��@�x�@�z�@��@��@�u@��@��@���@�ƨ@��@߾w@�M�@�V@ܼj@� �@ۮ@��H@�`B@�Q�@�|�@�E�@��T@���@��@�O�@��@�C�@̣�@�t�@�M�@�z�@�  @ƸR@��`@�;d@�(�@�t�@�"�@��R@��@��@��;@�;d@�J@���@�K�@��\@��@��h@��/@�Z@�1@���@�hs@�bN@��;@�ff@��@�G�@�&�@��@��
@�ȴ@�-@���@���@�9X@���@��P@�t�@�@�E�@�?}@�  @��H@�J@�hs@�b@���@��@�%@��j@�j@� �@���@�"�@���@���@�V@�@�O�@��@�t�@��y@�ȴ@���@��\@��@���@��h@�&�@���@���@���@�j@� �@�(�@�1'@�9X@�(�@��@�ƨ@�"�@���@���@�V@�5?@��@�O�@���@��@�bN@�9X@�b@�  @���@�l�@�\)@�+@��@���@���@�O�@���@��u@�ƨ@��!@�@�7L@��@���@��m@�S�@�@��@���@�v�@�V@�M�@��@���@�X@��@��`@��@���@��;@��P@���@��R@�E�@��@�J@��@��@��@�Ĝ@��D@�Z@�I�@�1'@��@�;@�w@�P@|�@;d@~�y@~ȴ@~��@~v�@~5?@}�-@}?}@}V@|�/@|�@|�D@|Z@|9X@{��@{t�@{o@y��@y%@x�u@x�@x  @w|�@v�y@v��@vff@v{@u�-@t�/@s�
@sdZ@s33@r�\@q��@qx�@qX@q7L@p��@p��@p�`@pĜ@p��@p�@pr�@pbN@p �@pb@p  @o�;@o��@o��@ol�@o�@o
=@n��@n�@n��@nv�@nV@m�T@m�@m?}@l��@l�@lZ@kƨ@ko@j��@ix�@i&�@h��@h�`@h��@h�9@h�u@h�u@h��@h�9@h�9@hĜ@hĜ@h�9@h�9@h��@hQ�@g�@g��@g�P@gl�@g;d@g+@g
=@f�@f��@f�+@f�+@f@e��@ep�@eO�@e/@d��@c�
@co@b��@a��@a&�@a%@`�9@`�9@`�u@`  @_�P@_|�@_;d@^��@^��@^ff@^E�@]@]p�@]`B@]O�@]/@\�j@\Z@\�@[C�@[o@Z�@Z�@Z��@Zn�@Y�#@Y��@Y��@Yx�@Yx�@Yhs@YG�@Y&�@X�9@W�;@W|�@WK�@V�R@V5?@U�@U��@T�@TZ@S�m@R�@R=q@RJ@Q�#@Q��@P�9@P  @O��@O�w@O��@O\)@O+@N�y@N�+@N$�@N@M�-@MO�@M�@L��@L�@K�F@K�@KdZ@KC�@K33@J�H@J��@J=q@I�#@I7L@HĜ@H�9@H�@Hr�@Hr�@HQ�@H1'@Hb@G�@Gl�@G;d@F�y@Fȴ@F�R@F��@Fv�@FV@F{@E��@EO�@E/@E�@E�@E�@EV@D��@D�/@D�/@Dz�@D9X@CS�@B�@B��@B��@B=q@A��@A�@A��@A�#@A��@A��@Ax�@AG�@A&�@@�9@@ �@?�@?|�@?;d@?
=@?
=@>�y@>��@>�+@>ff@>$�@=p�@<��@<j@;��@;S�@;"�@;o@;@:��@:^5@:=q@:=q@:J@9��@9�@9x�@8��@8Q�@7��@6��@6ȴ@6��@6v�@6V@6$�@5�-@5��@5�h@5`B@5/@4�/@49X@3�
@3o@1�@1X@1�@0��@0�`@0�9@0bN@/��@/l�@/+@.ȴ@.�+@.$�@-��@-��@,9X@+ƨ@+��@+��@+dZ@+dZ@+33@+o@*��@*~�@*~�@)��@)G�@(Ĝ@'�;@'�w@'��@'�P@'\)@';d@&�+@&5?@%�-@$�@$(�@$1@#�
@#t�@#@"n�@"=q@"M�@"�@!�@!�7@!�7@!x�@!G�@ r�@ 1'@ bN@ Q�@ A�@  �@ 1'@ 1'@  �@  �@  �@�@��@K�@��@��@��@5?@5?@{@�@�@�@@p�@p�@`B@?}@��@I�@t�@�\@�^@�`@��@%@�@r�@�@�@�u@�u@��@�@�@bN@A�@ �@�@�w@K�@K�@+@��@v�@V@E�@5?@$�@5?@5?@5?@5?@5?@5?@5?@E�@5?@$�@$�@@��@@��@�h@�@��@��@Z@(�@�@��@�@��@x�@7L@��@��@�9@�u@bN@A�@ �@  @�@�;@�w@��@l�@;d@�y@ȴ@��@��@�+@v�@E�@5?@$�@{@@�T@�h@O�@/@�/@9X@(�@(�@�@�@1@��@��@��@�m@�m@�m@�m@�
@�
@ƨ@�F@�F@��@��@��@t�@t�@dZ@dZ@S�@S�@C�@"�@@@@
�@
��@
��@
��@
�!@
��@
��@
�\@
~�@
n�@
^5@
=q@
-@
�@
�@
�@
�@
-@
�@	�@	�^@	7L@��@�u@�@bN@ �@�@�w@�@��@|�@\)@;d@
=@�y@�y@�@�@ȴ@ȴ@��@��@v�@v�@v�@ff@v�@ff@V@ff@V@V@V@V@V@V@V@V@V@V@V@V@V@V@ff@V@ff@ff@ff@V@E�@E�@5?@5?@5?@@@V@I�@9X@9X@Z@(�@n�@��@��@��@J@J@�#@��@��@��@hs@%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A��A��A��A�{A�%A���A��/A��^A���A�x�A�G�A��yA�C�A�dZA�+A��DA�K�A�A�7LA���A�E�A�33A���A�?}A��-A�n�A�(�A��/A��-A��hA�x�A�K�A�r�A���A�ȴA���A��TA�
=A�G�A�t�A���A��FA���A�S�A���A���A���A� �A��A��A��FA�I�A��TA��A�ȴA��TA��\A�C�A��A��A�|�A���A�A�A�jA�I�A���A�S�A��A��uA�ffA�Q�A�E�A��A�VA��A���A�r�A�A�=qA�p�A~v�A|Ay"�AvȴAvbNAv=qAu?}As
=Ar$�Ap�`Ao��An�Am�;Al5?AjM�Ai��Ag"�Af  Ad�AdVAb1'Aa�A`��A_p�A^bA\��A[��AZ�AY�wAX�RAWdZAV^5AU��AUAT��ATM�AT$�AS�^ARȴARI�AQG�AP�\AO��AO?}AN��AM�;AM/AK��AJ1'AIdZAH��AHv�AGl�AE��AD�!AD�+ADI�ADJACl�ABM�A@��A@bA>I�A<v�A;?}A;
=A:��A:��A:A8��A8��A7��A6ffA5x�A4�/A4r�A4Q�A4JA3�
A3��A3G�A2��A2��A2�DA1�A0��A0{A/�#A/��A/C�A.�A.A,��A+��A*��A)"�A(A'x�A'VA&��A&VA%�
A%�7A%;dA$�`A$ȴA$�uA#�FA"z�A"I�A ĜAp�AȴA�+A�A��AȴA5?A�AA�!A-A��Al�AAE�A7LA
=A��A�#A�A�A^5A��A/A�AffA$�A �A�mA�A
��A
(�A	��A	`BA��A�Ap�A�A�!AbA�FAO�A��Av�A1A �@�hs@���@��@��@��@���@��@���@�C�@�(�@��@��@�X@�r�@�b@�F@��@�x�@�z�@��@��@�u@��@��@���@�ƨ@��@߾w@�M�@�V@ܼj@� �@ۮ@��H@�`B@�Q�@�|�@�E�@��T@���@��@�O�@��@�C�@̣�@�t�@�M�@�z�@�  @ƸR@��`@�;d@�(�@�t�@�"�@��R@��@��@��;@�;d@�J@���@�K�@��\@��@��h@��/@�Z@�1@���@�hs@�bN@��;@�ff@��@�G�@�&�@��@��
@�ȴ@�-@���@���@�9X@���@��P@�t�@�@�E�@�?}@�  @��H@�J@�hs@�b@���@��@�%@��j@�j@� �@���@�"�@���@���@�V@�@�O�@��@�t�@��y@�ȴ@���@��\@��@���@��h@�&�@���@���@���@�j@� �@�(�@�1'@�9X@�(�@��@�ƨ@�"�@���@���@�V@�5?@��@�O�@���@��@�bN@�9X@�b@�  @���@�l�@�\)@�+@��@���@���@�O�@���@��u@�ƨ@��!@�@�7L@��@���@��m@�S�@�@��@���@�v�@�V@�M�@��@���@�X@��@��`@��@���@��;@��P@���@��R@�E�@��@�J@��@��@��@�Ĝ@��D@�Z@�I�@�1'@��@�;@�w@�P@|�@;d@~�y@~ȴ@~��@~v�@~5?@}�-@}?}@}V@|�/@|�@|�D@|Z@|9X@{��@{t�@{o@y��@y%@x�u@x�@x  @w|�@v�y@v��@vff@v{@u�-@t�/@s�
@sdZ@s33@r�\@q��@qx�@qX@q7L@p��@p��@p�`@pĜ@p��@p�@pr�@pbN@p �@pb@p  @o�;@o��@o��@ol�@o�@o
=@n��@n�@n��@nv�@nV@m�T@m�@m?}@l��@l�@lZ@kƨ@koG�O�@ix�@i&�@h��@h�`@h��@h�9@h�u@h�u@h��@h�9@h�9@hĜ@hĜ@h�9@h�9@h��@hQ�@g�@g��@g�P@gl�@g;d@g+@g
=@f�@f��@f�+G�O�@f@e��@ep�@eO�G�O�@d��@c�
@coG�O�@a��@a&�@a%@`�9@`�9G�O�@`  @_�P@_|�@_;d@^��@^��@^ff@^E�@]@]p�@]`B@]O�@]/@\�j@\Z@\�@[C�@[o@Z�@Z�@Z��@Zn�@Y�#@Y��@Y��@Yx�@Yx�@Yhs@YG�@Y&�@X�9@W�;@W|�@WK�@V�R@V5?@U�@U��@T�@TZ@S�m@R�@R=q@RJ@Q�#G�O�@P�9@P  @O��@O�w@O��@O\)@O+@N�y@N�+@N$�@N@M�-@MO�@M�@L��@L�@K�F@K�@KdZ@KC�@K33@J�H@J��@J=q@I�#@I7L@HĜ@H�9@H�@Hr�@Hr�@HQ�@H1'@Hb@G�@Gl�@G;d@F�y@Fȴ@F�R@F��@Fv�@FV@F{@E��@EO�@E/@E�@E�@E�@EV@D��@D�/G�O�@Dz�@D9X@CS�@B�@B��@B��@B=q@A��@A�@A��@A�#@A��@A��@Ax�@AG�@A&�@@�9@@ �@?�@?|�@?;d@?
=@?
=@>�y@>��@>�+@>ffG�O�@=p�@<��@<j@;��@;S�@;"�@;o@;@:��@:^5@:=q@:=q@:J@9��G�O�@9x�@8��@8Q�@7��@6��@6ȴ@6��@6v�@6V@6$�@5�-@5��@5�h@5`B@5/G�O�@49X@3�
@3o@1�@1X@1�@0��@0�`@0�9@0bN@/��@/l�@/+@.ȴ@.�+@.$�@-��G�O�@,9X@+ƨ@+��@+��@+dZ@+dZ@+33@+o@*��@*~�G�O�@)��@)G�@(Ĝ@'�;@'�w@'��@'�P@'\)G�O�@&�+@&5?@%�-@$�@$(�@$1@#�
@#t�@#@"n�@"=qG�O�@"�@!�@!�7@!�7@!x�G�O�@ r�@ 1'G�O�@ Q�@ A�@  �@ 1'@ 1'@  �@  �@  �@�@��@K�@��@��@��@5?@5?@{@�@�@�@@p�@p�@`B@?}G�O�@I�@t�@�\@�^@�`@��G�O�@�@r�@�@�@�u@�u@��@�@�@bN@A�@ �@�@�w@K�@K�@+@��@v�@V@E�@5?@$�@5?@5?@5?@5?@5?@5?@5?@E�@5?@$�@$�@@��@@��@�hG�O�@��@��@ZG�O�@�@��@�@��@x�@7L@��@��@�9@�u@bN@A�@ �@  @�@�;@�w@��@l�@;d@�y@ȴ@��@��@�+@v�@E�@5?@$�@{@@�T@�h@O�@/@�/@9X@(�@(�@�@�@1@��@��@��@�m@�m@�m@�m@�
@�
@ƨ@�F@�F@��@��@��@t�@t�@dZ@dZ@S�@S�@C�@"�@@@@
�@
��@
��@
��@
�!@
��@
��@
�\@
~�@
n�@
^5@
=q@
-@
�@
�@
�@
�@
-@
�@	�@	�^@	7L@��@�u@�@bN@ �@�@�w@�@��@|�@\)@;d@
=@�y@�y@�@�@ȴ@ȴ@��@��@v�@v�@v�@ff@v�@ff@V@ff@V@V@V@V@V@V@V@V@V@V@V@V@V@V@ff@V@ff@ff@ff@V@E�@E�@5?@5?@5?@G�O�@V@I�@9X@9XG�O�G�O�@n�@��@��@��@J@J@�#@��@��@��@hs@%1111111111111111111131111111111111311111111111111111111111111111111131111113111111111111311111111131113111111111111111111111111111111111111111113111111111111111111111111111111111111111111111113111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111114111141114111114111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111411111111111111111111111111141111111111111141111111111111114111111111111111114111111111141111111141111111111141111141141111111111111111111111111411111141111111111111111111111111111111111111114111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111144111111111111 B�B�B�B�B�B�B�B�B�B�sB�fB�TB�#B��B�dB��B�B�dBĜB�3B�hB�1B��B��B�B�XB�!B�RB�3B�'B�-B�B��B��B�B{�BdZBP�B:^BhB��B��B�wB��B�LB�B��B��B�DB�B� Bq�Br�Be`BZBD�B&�B&�B6FB0!B/B%�B�B+BB
�B
��B
�mB
ÖB
��B
�B
��B
��B
��B
ŢB
�B
��B
��B
�DB
�+B
r�B
[#B
H�B
)�B
&�B
uB
49B
1'B
�B
B

=B
B	�B	��B	�sB	��B	B	��B	��B	�B	�B	�B	��B	�oB	��B	�JB	~�B	}�B	p�B	e`B	r�B	dZB	YB	W
B	\)B	YB	[#B	\)B	ZB	P�B	E�B	D�B	<jB	8RB	7LB	5?B	.B	'�B	�B	\B	JB	hB	�B	\B��B��B��B	DB	%B��B�B�ZB��B�;BƨBŢB��B�BB�5B�B��BƨB��B��B�9B�XB�}BBȴBĜBŢB��B�}B�qB�qB�LB�B��B��B�B�B��B��B�uB�7B� B�Bx�B}�B�1B�=B�7B�7B�B�+B�%B�B�B}�Bp�BcTBq�BYB`BBaHBk�BcTBZBcTBZBJ�BI�BJ�BW
B\)BS�BP�BK�B@�BR�BJ�B<jB$�B7LB?}B?}B33B'�B.B@�BD�B=qB2-B6FB7LB7LB0!B+B$�B"�BoB�B%�B-B)�B!�B(�B�B
=B��BbB�B$�B$�B �B�BVB��B��B	7B �B�B�B�B�B�BbB{B�B{B	7BPB�B�B+B+B'�B�B�B$�B"�B!�B�B�B�B�B�B �B�BJB"�B$�B�B\B!�B!�B�B%�B�B�B�BuB5?B8RB7LB2-B0!B(�B2-B-B,B-B8RB=qB;dB;dB<jB<jB5?B6FB;dBB�B<jBJ�BJ�BP�BM�BG�BO�BYB\)B\)B^5BffBhsBhsBe`BaHB_;BbNBdZBjBl�BffBhsBr�B� B�B�B�%B�%B�%B�JB�DB�DB�DB�1B�7B�JB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B�B�B�B�'B�B�B�-B�jB�qB�qB�wB�wB�qB�}BB��B��B��B�}BBɺBɺBȴB��B�B�BB�mB�fB�TB�B��B��B��B��B��B	  B��B	B	B	+B	+B	%B	{B	�B	{B	�B	�B	!�B	)�B	.B	-B	-B	33B	9XB	@�B	C�B	E�B	F�B	H�B	H�B	L�B	M�B	N�B	O�B	P�B	S�B	T�B	VB	T�B	T�B	XB	ZB	\)B	]/B	_;B	_;B	`BB	`BB	`BB	bNB	aHB	hsB	o�B	q�B	p�B	q�B	s�B	v�B	w�B	w�B	w�B	v�B	y�B	�B	�B	�B	�B	�DB	�PB	�VB	�VB	�bB	�hB	�hB	�hB	�oB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�3B	�9B	�?B	�?B	�FB	�LB	�RB	�XB	�^B	�^B	�dB	�dB	�jB	�jB	�dB	�jB	�wB	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ǮB	ǮB	ŢB	ǮB	ȴB	ɺB	ȴB	ƨB	ŢB	ǮB	��B	��B	��B	��B	��B	�
B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�5B	�;B	�5B	�/B	�5B	�BB	�;B	�`B	�fB	�fB	�`B	�ZB	�ZB	�yB	�yB	�B	�B	�yB	�yB	�sB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
  B
B
B
B
B
%B
+B
+B
+B
%B
%B
%B
+B
%B
	7B
JB
JB
PB
PB
PB
JB
JB
JB
PB
VB
VB
\B
bB
bB
\B
\B
\B
\B
hB
uB
uB
{B
uB
uB
uB
uB
uB
hB
hB
bB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
!�B
!�B
#�B
$�B
#�B
#�B
#�B
!�B
 �B
"�B
!�B
$�B
'�B
(�B
(�B
(�B
(�B
(�B
+B
+B
)�B
)�B
(�B
&�B
(�B
'�B
'�B
,B
/B
0!B
1'B
0!B
/B
.B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
/B
5?B
8RB
9XB
9XB
9XB
9XB
9XB
8RB
8RB
9XB
6FB
6FB
8RB
8RB
>wB
>wB
>wB
=qB
=qB
;dB
=qB
<jB
<jB
?}B
B�B
A�B
@�B
A�B
A�B
D�B
F�B
D�B
D�B
E�B
F�B
F�B
E�B
C�B
G�B
I�B
H�B
H�B
H�B
H�B
I�B
H�B
H�B
H�B
G�B
F�B
F�B
E�B
J�B
J�B
I�B
K�B
J�B
J�B
K�B
K�B
J�B
I�B
K�B
K�B
J�B
I�B
G�B
F�B
G�B
I�B
K�B
R�B
Q�B
O�B
R�B
S�B
S�B
S�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
R�B
Q�B
Q�B
S�B
R�B
Q�B
S�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
T�B
T�B
VB
VB
T�B
T�B
R�B
S�B
T�B
T�B
R�B
Q�B
W
B
W
B
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
]/B
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
`BB
_;B
_;B
_;B
_;B
_;B
`BB
`BB
_;B
^5B
_;B
_;B
_;B
^5B
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
ffB
ffB
e`B
dZB
dZB
cTB
dZB
gmB
hsB
gmB
gmB
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
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
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
l�B
m�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
jB
hsB
iyB
o�B
n�B
n�B
k�B
ffB
m�B
r�B
r�B
r�B
r�B
q�B
q�B
r�B
r�B
q�B
r�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B��B��B��B��B�B�B��B�B��B��B�wB��B�mB��B��B��B��B��B�UB�B�[B��B��B��B�|B�cB�eB��B�EB}�Bf�BS&B=VBB�jB��BBοB�B��B��B��B��B��B�;Bs3BsMBf�B[qBF�B)�B(�B6�B0�B/�B&�B�B�B�B
��B
�>B
��B
�tB
ҽB
ּB
�gB
�2B
�4B
�YB
��B
��B
��B
��B
�fB
t�B
]~B
K�B
-]B
)�B
B
4�B
1�B
!-B
�B
xB
�B	�aB	��B	��B	�B	��B	��B	�B	��B	�qB	��B	�B	��B	��B	��B	��B	cB	raB	gB	s3B	e�B	Z�B	XEB	\�B	Y�B	[�B	\�B	Z�B	Q�B	F�B	EmB	=�B	9XB	88B	6B	/ B	)B	 �B	hB	�B	oB	$B	HB��B��B��B	^B	�B�}B��B��B�@B�'B�B��B�6B�\BޞBڠB��B�B�jB��B��B�xB�4B�B�B�B��B��B��B��B��B��B�B�-B��B�WB��B��B��B��B��B��B��Bz�BcB��B��B��B��B��B��B��B��B��B~�Bq�Bd�Br-B[WBa�BbBk�Bd@B[qBc�B[#BL~BKxBLBW�B\�BT�BQ�BL�BA�BS&BK�B=�B'B8�B@OB@4B4�B)�B/�B@�BD�B>B3MB6�B8B7�B1B,B%�B#�BFB	B&�B-�B*�B"�B)_B�BB�RB�B�B%,B%FB!HB7BvB �B��B
XB �B BB=B 'BB9BhB2BB2B
�BpB�B_B*�B+B(�B�B�B%,B#TB"NBxB�BkBdB�B!-B�BB#nB%`B�BB"�B"�B�B&fB�B�B�B�B5�B8�B7�B2�B0�B*B2�B.B-B.B8�B=�B;�B<B<�B<�B6`B72B<6BC-B=�BKBKDBQBN<BH�BP�BY�B\�B\�B^�Bf�Bh�Bh�Be�Ba�B`'Bc:Be,BkBm)Bg�BiyBs�B�OB�aB�mB�tB�tB��B�~B��B��B��B��B�	B�B��B��B��B�B�B�B��B�B�:B�B�B�B�B�B�B�B�0B�8B�>B�fB�QB�OB�cB�[B�iB��B��B��B��B��B��B��B��B��BªB��B��B��B� B�B�	B�#B�lB̘BּB�B�B��B�B� B��B�B�B�(B�HB	 B�BB	UB	SB	_B	zB	�B	�B	�B	�B	B	�B	"4B	*B	./B	-CB	-�B	3�B	9�B	@�B	C�B	E�B	F�B	H�B	H�B	L�B	NB	N�B	O�B	QB	TB	UB	VB	U2B	U2B	XEB	Z7B	\CB	]/B	_VB	_pB	`\B	`vB	`�B	b�B	a�B	h�B	o�B	q�B	p�B	q�B	s�B	v�B	w�B	xB	xB	w2B	zDB	�'B	�3B	�aB	�mB	�^B	�jB	�pB	�pB	�HB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�&B	� B	�&B	�DG�O�B	�;B	�MB	�nB	�ZB	�?B	�`B	�LB	�RB	�XB	�^B	�^B	�dB	��B	�jB	��B	��B	��B	��B	��B	��B	ªB	ðB	ðB	ĜB	żB	��B	��G�O�B	��B	��B	��B	��G�O�B	�B	��B	�G�O�B	�4B	�B	�B	�$B	�9G�O�B	�9B	�1B	�+B	�+B	�+B	�KB	�7B	�KB	�CB	�OB	�pB	�OB	�dB	�jB	�vB	ߊB	�zB	�B	�fB	�zB	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�G�O�B	�B	��B	�B	�B	�B	�B	�B	�(B	�.B
 B
 4B
 4B
'B
;B
UB
3B
YB
EB
EB
EB
?B
?B
YB
_B
�B
	lB
JB
dB
PB
PB
jB
dB
dB
~B
jB
pB
�B
vB
}B
}B
�B
vB
�B
�B
�B
�B
�B
�B
uB
uB
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
 �B
!�B
"�B
#B
!�B
!�B
$B
$�B
#�B
#�B
$G�O�B
!B
#B
"B
%,B
(
B
)*B
)B
(�B
)B
)*B
+B
+B
*B
*B
)*G�O�B
)*B
(>B
(XB
,=B
/5B
0;B
1'B
0;B
/OB
.}B
1AB
1AB
1[B
2GB
2|B
2GB
2|G�O�B
5tB
8RB
9XB
9rB
9XB
9rB
9rB
8RB
8lB
9rG�O�B
6�B
8�B
8�B
>wB
>�B
>�B
=�B
=�G�O�B
=�B
<�B
<�B
?�B
B�B
A�B
@�B
A�B
A�B
D�B
F�G�O�B
D�B
E�B
F�B
F�B
E�G�O�B
G�B
I�G�O�B
H�B
H�B
H�B
I�B
H�B
H�B
H�B
G�B
F�B
F�B
E�B
J�B
J�B
I�B
K�B
J�B
J�B
K�B
K�B
J�B
I�B
K�B
K�B
J�B
I�G�O�B
F�B
G�B
J	B
LB
R�B
RG�O�B
R�B
TB
S�B
S�B
SB
R�B
SB
SB
SB
S&B
RB
SB
RB
R B
S�B
SB
R B
T,B
T�B
U�B
VB
U�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
UB
UB
VB
VB
UB
UG�O�B
T,B
UB
U2G�O�B
RTB
W$B
W?B
Z7B
Z7B
Z7B
[=B
[WB
\CB
\CB
\CB
]IB
]IB
]/B
]/B
]IB
]dB
]IB
]IB
]IB
^OB
^OB
`BB
_VB
_;B
_VB
_;B
_VB
`BB
`BB
_VB
^OB
_pB
_VB
_VB
^jB
cTB
c:B
cTB
cTB
cTB
cTB
cTB
cTB
c:B
cTB
dZB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
cTB
cnB
cnB
dZB
dZB
d@B
dtB
dZB
dZB
d�B
dtB
e`B
e`B
dtB
dtB
ezB
e`B
e`B
ezB
e`B
e`B
ezB
e`B
e`B
ezB
f�B
ffB
g�B
gmB
ffB
ffB
ezB
dtB
dtB
c�B
dtB
gmB
hsB
g�B
g�B
h�B
iyB
iyB
iyB
i�B
i�B
i�B
i�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
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
l�B
mwB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
j�G�O�B
i�B
o�B
n�B
n�G�O�G�O�B
m�B
r�B
r�B
r�B
r�B
q�B
q�B
r�B
r�B
q�B
r�B
u�1111111111111111111131111111111111311111111111111111111111111111111131111113111111111111311111111131113111111111111111111111111111111111111111113111111111111111111111111111111111111111111111113111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111114111141114111114111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111411111111111111111111111111141111111111111141111111111111114111111111111111114111111111141111111141111111111141111141141111111111111111111111111411111141111111111111111111111111111111111111114111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111144111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;oG�O�;o;o;oG�O�;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;oG�O�;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
G�O�<#�
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
G�O�<#�
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
G�O�<#�
<#�
<#�
<#�
<#�
G�O�<#�
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
G�O�<#�
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
G�O�<#�
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
G�O�<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806050037482018060500374820180605003748201806221242472018062212424720180622124247201806060023592018060600235920180606002359  JA  ARFMdecpA19c                                                                20180601063503  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180531213504  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180531213507  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180531213507  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180531213508  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180531213508  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180531213508  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180531213508  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180531213508  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180531213508                      G�O�G�O�G�O�                JA  ARUP                                                                        20180531215453                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180601153625  CV  JULD            G�O�G�O�F�7�                JM  ARSQJMQC2.0                                                                 20180604000000  CF  PSAL_ADJUSTED_QCB0  D�  G�O�                JM  ARSQJMQC2.0                                                                 20180604000000  CF  TEMP_ADJUSTED_QCB0  D�  G�O�                JM  ARCAJMQC2.0                                                                 20180604153748  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180604153748  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180605152359  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034247  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                