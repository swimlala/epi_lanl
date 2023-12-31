CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-02-09T00:35:24Z creation;2017-02-09T00:35:27Z conversion to V3.1;2019-12-19T08:15:26Z update;     
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
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ol   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  sD   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ˬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ۜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170209003524  20200116211516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               WA   JA  I2_0577_087                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��S� 1   @�ﶷ�Ԁ@4�A���d�&���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ D�|�D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�{@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B0\)B7��B?��BG��BO��BW��B_�\Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Db�Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D�|{Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D��{D�?�D��D�D���D�?�D��D���D���D�?�D��D���D��D�?�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�(�A�+A�(�A�$�A�$�A�$�A�&�A�&�A�&�A�(�A�+A�-A�-A�-A�-A�/A�/A�/A�1'A�/A�/A�/A�1'A�1'A�33A�33A�5?A�5?A�5?A�5?A�7LA�;dA�;dA�=qA�=qA�9XA�9XA�?}A�;dA�7LA�5?A�;dA�5?A�v�Aɲ-AɅA�x�AɋDA�ffA�A�A�E�A�-A���AǺ^A�?}A�O�A��A�XA\A�\)A��`A�C�A�?}A�
=A��PA�(�A���A��A��mA���A��A�A�%A��A��A�I�A�&�A�bA���A���A��mA�7LA���A��TA�
=A��A��PA�`BA��A�&�A�1'A���A�oA�A���A��+A�z�A�I�A��`A�%A�;dA�l�A�E�A� �A��uA��-A�|�A�dZA�%A��A��+A��A��A�A�l�A�|�A�x�A��^A��uA��A�
=A�C�A��A��At�AK�A7LA~��A~JA|E�Ay�7Aw��Av(�Au&�Atn�As�ApA�Al�HAlE�Al  Aj�DAg�Ae;dAd�9Act�Aa�mA`ĜA^��A\�RAW��AR��AQ\)AP�APQ�AO�hAMK�AK��AJ��AJ�!AI�AF��AD�+AB^5A?`BA=l�A:�`A8�!A77LA5�FA41A37LA2Q�A0��A.Q�A-"�A+oA)�wA)&�A(A�A'�hA'%A&�uA&bA#`BA"5?A!�A!XA �`A �A�wAC�A�A�HA��An�A�wAVAƨA�A�HAQ�A�AA�/A�RAjAJAƨA�hAO�A�AE�AC�A�9A�;A�7AG�AoA�!AQ�A��AVAG�AE�AƨA
��A
ZA	��A	dZA	�A��A�`A�A\)A9XA-A$�A$�A �A�mAl�A�AĜAv�A-A ��@�"�@��`@�"�@��@���@��@���@��+@��@�G�@��;@�E�@��@�z�@��@�;d@�%@��T@�1@���@���@��;@���@��@�"�@ޟ�@ݩ�@���@�r�@�;d@ڧ�@�v�@�=q@�J@��@��@���@ؼj@ج@�I�@�1@׮@�l�@ָR@�$�@�b@�|�@ҧ�@�M�@�J@�V@�1'@ϝ�@�S�@Ο�@�$�@�&�@�A�@���@��;@˝�@ʏ\@�@��@ȓu@ȃ@�b@��@�&�@�;d@���@�{@��@�"�@��R@�V@���@�7L@�?}@�?}@�hs@�x�@���@��-@��7@�`B@���@�?}@��j@�Q�@�b@�|�@�
=@���@���@�dZ@��y@���@��\@�{@��@�X@��9@�9X@�n�@��!@�=q@��@��h@�X@�7L@��@�%@�r�@���@�\)@��R@�~�@�E�@�$�@�-@�{@���@�/@���@�A�@�Q�@� �@��w@��@�
=@��@��\@��@���@�1'@�(�@��P@�@��R@�V@��@�p�@��@�hs@�x�@�`B@�?}@��/@���@��j@�Ĝ@���@�r�@��@��F@�dZ@��@��!@�V@�=q@��@���@��j@�9X@�ƨ@�;d@���@���@��7@�x�@�`B@�V@��j@���@�r�@��@���@�K�@�+@�
=@���@��@��@�ȴ@�ȴ@���@�ȴ@���@�M�@�=q@�5?@�5?@�-@�-@�5?@�$�@��@��T@��h@�&�@��u@�j@�bN@�Z@�Z@�Q�@�A�@��@�b@�  @���@�l�@��@��@���@���@�n�@�@���@��7@�&�@��@���@��u@�9X@� �@���@���@��@��P@�|�@��@�|�@�l�@��@��y@��!@�M�@��T@�`B@�&�@��@��u@�1@���@�"�@�ȴ@�ȴ@��R@��!@��\@�n�@�V@�5?@�{@��@��7@��@��j@��u@�Z@�b@�K�@�
=@��@��@���@���@�M�@�G�@���@��@��j@�j@�Z@�b@��F@�l�@���@���@�n�@�-@�{@�J@���@��@��T@��#@��-@�p�@�%@���@�Z@� �@�1@�  @�  @�@��@�P@K�@~��@~$�@}��@}O�@}/@}V@|�@|�@|1@{�m@{��@{�@{t�@{t�@{dZ@{S�@zn�@y��@x�9@xbN@xA�@x  @w��@wl�@wK�@v��@vE�@v{@u�T@u�h@up�@uV@r��@q�7@q%@o�@o�w@o��@o|�@ol�@o;d@o+@n�@n�+@n5?@n@m@m�-@m��@m`B@m?}@mV@l�@l�/@l�@l�@kt�@ko@j��@jn�@j^5@j=q@i��@i7L@h�9@h�@g�@g+@g
=@f��@ep�@d��@d�@d�D@dz�@dz�@dj@dj@dj@dj@c�
@c@b��@bM�@a��@a��@aX@`�u@_�P@_
=@^v�@^@]`B@]/@\��@\�/@\�/@\�/@\�@\I�@\1@[��@[C�@Z�!@Zn�@Y�#@Y7L@Y%@Y%@Y%@XĜ@X��@X �@W
=@V��@V5?@V$�@V$�@U�-@Up�@U?}@T��@Tz�@T�@T�@T�@T�@S�m@S�@R�H@R=q@Q�@Qhs@P�9@PA�@O�@N��@N��@NE�@N{@M@M��@M/@Lz�@K��@K�@KS�@KC�@K33@K"�@K@Ko@K@J�@J�@J�H@J�!@J-@IX@H�9@HbN@G��@G\)@G
=@F��@F�y@F�@Fȴ@F��@E�@E�@Ep�@E`B@D��@D9X@B�@B��@B^5@BJ@A��@A%@@��@@��@@�u@@Q�@@ �@@  @?�;@?�w@?�@?�P@?|�@?l�@?\)@?K�@?+@>ȴ@>{@=��@=�-@=p�@=/@<Z@;ƨ@;��@;o@:��@:��@:n�@:n�@:^5@:=q@:J@9��@9�#@9�^@9��@9��@9hs@8�9@8b@7�@7|�@7
=@6ȴ@65?@5��@5�-@5��@5��@5��@5�@5`B@5V@4I�@3�
@3C�@2�!@2^5@2J@1��@1x�@1&�@0�9@0bN@0A�@/�w@/;d@/
=@.�y@.�R@.��@.��@.ff@.5?@.{@-��@-�@-`B@-V@,�j@,��@,1@*�@*-@*J@)��@)x�@)X@)�@(��@(��@(1'@(  @'�;@'�@'K�@&�y@&�+@&ff@&V@&V@&E�@&5?@%�@%�-@%�@%V@$�j@$��@$�D@$j@#��@#�
@#�F@#��@#33@"�H@"�!@"��@"n�@"-@"-@"J@!��@!��@!hs@!X@ ��@  �@�@�;@�;@�@�;@�;@��@�@|�@|�@l�@l�@�@ȴ@��@��@v�@@�T@@@�h@`B@��@z�@��@��@S�@"�@n�@^5@^5@^5@^5@�@�^@��@�7@�7@x�@7L@&�@��@��@�@Q�@ �@  @��@|�@\)@+@��@�@��@v�@5?@�@�T@��@@��@p�@�@��@�/@��@�j@�@��@j@��@C�@"�@�@�H@�H@��@��@n�@-@J@��@�#@�^@��@��@x�@hs@hs@X@�@��@��@�9@�u@bN@b@�@�@�;@�w@�@�P@K�@+@+@�@��@�@�@�@�@��@v�@V@5?@@@��@�@p�@O�@?}@/@/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�(�A�+A�(�A�$�A�$�A�$�A�&�A�&�A�&�A�(�A�+A�-A�-A�-A�-A�/A�/A�/A�1'A�/A�/A�/A�1'A�1'A�33A�33A�5?A�5?A�5?A�5?A�7LA�;dA�;dA�=qA�=qA�9XA�9XA�?}A�;dA�7LA�5?A�;dA�5?A�v�Aɲ-AɅA�x�AɋDA�ffA�A�A�E�A�-A���AǺ^A�?}A�O�A��A�XA\A�\)A��`A�C�A�?}A�
=A��PA�(�A���A��A��mA���A��A�A�%A��A��A�I�A�&�A�bA���A���A��mA�7LA���A��TA�
=A��A��PA�`BA��A�&�A�1'A���A�oA�A���A��+A�z�A�I�A��`A�%A�;dA�l�A�E�A� �A��uA��-A�|�A�dZA�%A��A��+A��A��A�A�l�A�|�A�x�A��^A��uA��A�
=A�C�A��A��At�AK�A7LA~��A~JA|E�Ay�7Aw��Av(�Au&�Atn�As�ApA�Al�HAlE�Al  Aj�DAg�Ae;dAd�9Act�Aa�mA`ĜA^��A\�RAW��AR��AQ\)AP�APQ�AO�hAMK�AK��AJ��AJ�!AI�AF��AD�+AB^5A?`BA=l�A:�`A8�!A77LA5�FA41A37LA2Q�A0��A.Q�A-"�A+oA)�wA)&�A(A�A'�hA'%A&�uA&bA#`BA"5?A!�A!XA �`A �A�wAC�A�A�HA��An�A�wAVAƨA�A�HAQ�A�AA�/A�RAjAJAƨA�hAO�A�AE�AC�A�9A�;A�7AG�AoA�!AQ�A��AVAG�AE�AƨA
��A
ZA	��A	dZA	�A��A�`A�A\)A9XA-A$�A$�A �A�mAl�A�AĜAv�A-A ��@�"�@��`@�"�@��@���@��@���@��+@��@�G�@��;@�E�@��@�z�@��@�;d@�%@��T@�1@���@���@��;@���@��@�"�@ޟ�@ݩ�@���@�r�@�;d@ڧ�@�v�@�=q@�J@��@��@���@ؼj@ج@�I�@�1@׮@�l�@ָR@�$�@�b@�|�@ҧ�@�M�@�J@�V@�1'@ϝ�@�S�@Ο�@�$�@�&�@�A�@���@��;@˝�@ʏ\@�@��@ȓu@ȃ@�b@��@�&�@�;d@���@�{@��@�"�@��R@�V@���@�7L@�?}@�?}@�hs@�x�@���@��-@��7@�`B@���@�?}@��j@�Q�@�b@�|�@�
=@���@���@�dZ@��y@���@��\@�{@��@�X@��9@�9X@�n�@��!@�=q@��@��h@�X@�7L@��@�%@�r�@���@�\)@��R@�~�@�E�@�$�@�-@�{@���@�/@���@�A�@�Q�@� �@��w@��@�
=@��@��\@��@���@�1'@�(�@��P@�@��R@�V@��@�p�@��@�hs@�x�@�`B@�?}@��/@���@��j@�Ĝ@���@�r�@��@��F@�dZ@��@��!@�V@�=q@��@���@��j@�9X@�ƨ@�;d@���@���@��7@�x�@�`B@�V@��j@���@�r�@��@���@�K�@�+@�
=@���@��@��@�ȴ@�ȴ@���@�ȴ@���@�M�@�=q@�5?@�5?@�-@�-@�5?@�$�@��@��T@��h@�&�@��u@�j@�bN@�Z@�Z@�Q�@�A�@��@�b@�  @���@�l�@��@��@���@���@�n�@�@���@��7@�&�@��@���@��u@�9X@� �@���@���@��@��P@�|�@��@�|�@�l�@��@��y@��!@�M�@��T@�`B@�&�@��@��u@�1@���@�"�@�ȴ@�ȴ@��R@��!@��\@�n�@�V@�5?@�{@��@��7@��@��j@��u@�Z@�b@�K�@�
=@��@��@���@���@�M�@�G�@���@��@��j@�j@�Z@�b@��F@�l�@���@���@�n�@�-@�{@�J@���@��@��T@��#@��-@�p�@�%@���@�Z@� �@�1@�  @�  @�@��@�P@K�@~��@~$�@}��@}O�@}/@}V@|�@|�@|1@{�m@{��@{�@{t�@{t�@{dZ@{S�@zn�@y��@x�9@xbN@xA�@x  @w��@wl�@wK�@v��@vE�@v{@u�T@u�h@up�@uV@r��@q�7@q%@o�@o�w@o��@o|�@ol�@o;d@o+@n�@n�+@n5?@n@m@m�-@m��@m`B@m?}@mV@l�@l�/@l�@l�@kt�@ko@j��@jn�@j^5@j=q@i��@i7L@h�9@h�@g�@g+@g
=@f��@ep�@d��@d�@d�D@dz�@dz�@dj@dj@dj@dj@c�
@c@b��@bM�@a��@a��@aX@`�u@_�P@_
=@^v�@^@]`B@]/@\��@\�/@\�/@\�/@\�@\I�@\1@[��@[C�@Z�!@Zn�@Y�#@Y7L@Y%@Y%@Y%@XĜ@X��@X �@W
=@V��@V5?@V$�@V$�@U�-@Up�@U?}@T��@Tz�@T�@T�@T�@T�@S�m@S�@R�H@R=q@Q�@Qhs@P�9@PA�@O�@N��@N��@NE�@N{@M@M��@M/@Lz�@K��@K�@KS�@KC�@K33@K"�@K@Ko@K@J�@J�@J�H@J�!@J-@IX@H�9@HbN@G��@G\)@G
=@F��@F�y@F�@Fȴ@F��@E�@E�@Ep�@E`B@D��@D9X@B�@B��@B^5@BJ@A��@A%@@��@@��@@�u@@Q�@@ �@@  @?�;@?�w@?�@?�P@?|�@?l�@?\)@?K�@?+@>ȴ@>{@=��@=�-@=p�@=/@<Z@;ƨ@;��@;o@:��@:��@:n�@:n�@:^5@:=q@:J@9��@9�#@9�^@9��@9��@9hs@8�9@8b@7�@7|�@7
=@6ȴ@65?@5��@5�-@5��@5��@5��@5�@5`B@5V@4I�@3�
@3C�@2�!@2^5@2J@1��@1x�@1&�@0�9@0bN@0A�@/�w@/;d@/
=@.�y@.�R@.��@.��@.ff@.5?@.{@-��@-�@-`B@-V@,�j@,��@,1@*�@*-@*J@)��@)x�@)X@)�@(��@(��@(1'@(  @'�;@'�@'K�@&�y@&�+@&ff@&V@&V@&E�@&5?@%�@%�-@%�@%V@$�j@$��@$�D@$j@#��@#�
@#�F@#��@#33@"�H@"�!@"��@"n�@"-@"-@"J@!��@!��@!hs@!X@ ��@  �@�@�;@�;@�@�;@�;@��@�@|�@|�@l�@l�@�@ȴ@��@��@v�@@�T@@@�h@`B@��@z�@��@��@S�@"�@n�@^5@^5@^5@^5@�@�^@��@�7@�7@x�@7L@&�@��@��@�@Q�@ �@  @��@|�@\)@+@��@�@��@v�@5?@�@�T@��@@��@p�@�@��@�/@��@�j@�@��@j@��@C�@"�@�@�H@�H@��@��@n�@-@J@��@�#@�^@��@��@x�@hs@hs@X@�@��@��@�9@�u@bN@b@�@�@�;@�w@�@�P@K�@+@+@�@��@�@�@�@�@��@v�@V@5?@@@��@�@p�@O�@?}@/@/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�XB��B�;B�B�B��B�B33BC�BZBs�B��B�!B�-B�9B�LB�B�B�RB�wB�wBB�jB�jB�XB�RB�XB�FB�RB�?B�3B�-B�'B�FB��BǮB��B��B�B�B�B�B�B�B�B��B��B��B��BǮBÖB�qB��B�bB�%B~�Bw�BbNBW
BL�B;dB-B�B��B�B�TB��BǮB�dB�B��B�oB�%B{�BaHBE�B+B �B\B
�NB
��B
ȴB
�B
��B
��B
�VB
�JB
�DB
�=B
�7B
�1B
�B
u�B
cTB
VB
J�B
A�B
<jB
1'B
�B
B	��B	��B	�B	�/B	��B	ȴB	��B	�XB	�'B	��B	��B	� B	iyB	YB	W
B	R�B	N�B	G�B	;dB	6FB	49B	/B	"�B	�B	JB	  B��B�B�HB�/B�
B��B��BȴBŢB�jB�LB�3B�B�B��B��B��B��B��B��B�uB�bB�bB�\B�PB�JB�JB�7B�7B�7B�=B�DB�PB�bB�\B�bB�hB�oB�oB�oB�oB�oB�oB�oB�oB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�LBȴB��B��B��B��B��B��B��B�B�)B�/B�/B�5B�#B�B�#B�5B�5B�;B�5B�5B�5B�NB�sB�mB�mB�fB�`B�TB�)B�B�B�#B�;B�#B�#B�#B�#B�/B�5B�;B�TB�`B�fB�fB�sB�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	
=B	\B	bB	oB	�B	�B	�B	�B	�B	�B	!�B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	!�B	#�B	$�B	%�B	&�B	(�B	+B	0!B	7LB	?}B	@�B	B�B	F�B	G�B	G�B	F�B	@�B	>wB	>wB	>wB	>wB	>wB	?}B	>wB	=qB	;dB	=qB	<jB	<jB	<jB	=qB	=qB	=qB	>wB	@�B	A�B	C�B	F�B	G�B	H�B	I�B	L�B	K�B	L�B	R�B	XB	[#B	`BB	cTB	dZB	dZB	e`B	iyB	n�B	n�B	p�B	s�B	x�B	y�B	z�B	{�B	}�B	�B	�%B	�=B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�3B	�9B	�FB	�^B	�jB	�qB	�}B	��B	��B	��B	ÖB	ĜB	ĜB	ŢB	ǮB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�/B	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�ZB	�fB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
B
B
B
%B
%B
%B
1B
	7B

=B

=B

=B
	7B

=B
DB
JB
PB
PB
PB
PB
PB
PB
PB
VB
VB
\B
bB
hB
hB
hB
hB
hB
hB
hB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
!�B
"�B
"�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
.B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
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
33B
33B
33B
33B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
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
;dB
;dB
;dB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
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
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
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
W
B
XB
XB
YB
YB
YB
YB
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
[#B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
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
bNB
bNB
cTB
cTB
cTB
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
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
gmB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
jB
k�B
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
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
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
u�B
u�B
u�B
u�B
v�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�rB��B�;B�WB�B�B9B3hBC�BZBtB��B�B��B�tB�XB��B�cB��B��B��BĶB��B�qB��B�>B�0B�*B��B�tB��B��B��B�^B��B�#B�BBңB�QB�KB�QB�eB��B�B��B�B��B��B�"B�B�zB�aB�B�uB��B��B}�Be,BZ7BQ�B>�B1'B1B�.B�GB�BңB��B�B�CB�B��B��B��BfBIRB-]B%�BB
��B
�+B
�dB
��B
�bB
�9B
��B
��B
�xB
��B
��B
��B
��B
x�B
e`B
XB
LB
B�B
>�B
4�B
!�B
�B	��B	�0B	�-B	�;B	��B	�XB	�uB	�0B	��B	��B	�5B	�B	k6B	Y�B	XB	T{B	Q�B	I�B	<�B	72B	6�B	2-B	%�B	�B	�B	�B��B�;B�nB�;B��B�:B�VB�BȀB�BB��B��B�B�"B��B��B��B�'B��B��B�B�4B�4B�.B�B��B��B�lB��B�	B�DB�dB��B�4B��B�NB��B�B��B��B��B��B��B��B��B�B��B��B�yB��B�#B�B�B�IB�jB��B�;B��B� B��B� B�`B��B��B�kB�eB��B��B��B��B��B��B��B�B�BBϑB�oB�{BخB��B��B�BߤB�CB��BۦB��B�!B�'B��B޸B�VB�nB�*B�
B�
B�RB�B�,B�IBٴB�#B�/B��B�CB��BۦB��BݲB޸B�'B�B�B�B�B�B�B��B��B��B��B��B��B�B�ZB�zB�B�FB�lB�*B�>B��B�rB�>B�B�LB�jB��B	�B	
�B	vB	�B	&B	$B	#B	�B	�B	B	�B	$B	!�B	1B	KB	�B	B	�B	B	#B	�B	!�B	!�B	!�B	#�B	$�B	%�B	'B	)*B	+kB	0!B	7�B	?�B	@�B	CB	GB	H1B	H�B	HB	@�B	>�B	>�B	>�B	>�B	>�B	@B	?HB	>�B	=qB	=�B	<�B	<�B	<�B	=�B	=�B	=�B	>�B	AB	A�B	C�B	F�B	G�B	H�B	I�B	MB	LB	MPB	S[B	XEB	[=B	`vB	c�B	d�B	d�B	e�B	i�B	o�B	o B	p�B	s�B	y>B	zDB	{0B	|6B	~BB	�UB	�YB	�#B	�}B	��B	��B	��B	�B	��B	�B	�B	�DB	�DB	�$B	�DB	�KB	�KB	�0B	�KB	�0B	��B	�B	�B	�B	�kB	�B	��B	�)B	�/B	�/B	�oB	�aB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ÖB	ĜB	ĶB	��B	��B	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�(B	�SB	�~B	�\B	�HB	�HB	�bB	�bB	�bB	�bB	�hB	�hB	�hB	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	�B	�B	�B	�6B	�VB	�BB	�.B	�B	�HB	�cB
 OB
UB
AB
B
-B
3B
3B
3B
B
9B
9B
3B
�B
gB
gB
SB
mB
mB
�B
YB
9B
9B
9B
SB
�B
�B
tB
%B
YB
fB
	RB

rB

�B

�B
	�B

rB
xB
~B
jB
jB
jB
jB
jB
jB
�B
�B
�B
�B
�B
�B
�B
hB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
pB
"NB
#B
#B
$�B
%B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&B
'B
'B
&�B
'B
'B
'B
'B
($B
)*B
)*B
*B
*B
*B
*0B
*KB
+6B
+B
,=B
,=B
-)B
-CB
-]B
.cB
./B
/B
/B
/B
/B
/B
/B
/5B
/OB
/iB
0;B
0;B
0;B
0UB
0UB
0oB
1vB
1[B
1[B
2GB
2aB
2-B
3MB
3MB
3MB
33B
3MB
3MB
3MB
3MB
3�B
4�B
5ZB
5tB
5tB
6`B
6FB
6FB
6`B
6`B
6zB
7�B
8�B
8lB
9XB
9XB
9�B
9rB
9rB
9�B
:xB
:xB
;dB
;dB
;dB
:xB
;�B
;�B
;�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
>�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
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
F%B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
MB
MB
OB
N�B
OB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
Q B
O�B
P.B
QB
RB
R B
R B
RB
S&B
SB
SB
S�B
S�B
S�B
S�B
TB
T,B
TFB
U2B
U2B
U2B
VB
VB
VB
VB
W$B
W?B
W$B
W$B
W?B
XEB
X+B
Y1B
Y1B
YB
YKB
Y1B
Y1B
Y1B
Y1B
Y1B
Z7B
Z7B
Z7B
Z7B
ZkB
[�B
\xB
]IB
]IB
]IB
^OB
^5B
^OB
^OB
^jB
_VB
_VB
_VB
_pB
_�B
`vB
`\B
`BB
aHB
aHB
aHB
abB
abB
abB
a|B
b�B
bhB
cTB
c�B
cnB
cnB
cnB
cnB
cnB
dtB
dtB
dtB
dtB
e�B
e`B
ezB
ezB
ezB
ezB
ezB
e�B
f�B
f�B
gmB
ffB
fLB
g�B
gmB
gmB
g�B
g�B
gmB
g�B
g�B
g�B
g�B
g�B
hsB
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
jB
k�B
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
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
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
u�B
u�B
u�B
u�B
v�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201702130035372017021300353720170213003537201806221309022018062213090220180622130902201804050709482018040507094820180405070948  JA  ARFMdecpA19c                                                                20170209093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170209003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170209003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170209003525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170209003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170209003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170209003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170209003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170209003526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170209003527                      G�O�G�O�G�O�                JA  ARUP                                                                        20170209010225                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170209153315  CV  JULD            G�O�G�O�F�}�                JM  ARCAJMQC2.0                                                                 20170212153537  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170212153537  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220948  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040902  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211516                      G�O�G�O�G�O�                