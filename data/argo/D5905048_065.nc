CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-05T00:35:24Z creation;2016-12-05T00:35:26Z conversion to V3.1;2019-12-19T08:20:28Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20161205003524  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               AA   JA  I2_0577_065                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��5V��1   @��6`� @3�y���d��ߤ@1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ�fD[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv�fDw  Dw� Dx  Dx� Dy  Dy� Dz  Dz�fD{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�3D�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @2�\@\)@��@��A�
A?�
A_�
A��RA��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC
C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D��D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ��DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Dv�Dv��Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz��Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�?�D��D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D�|{D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D�|{D濮D���D�?�D��D翮D��D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�<{D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D��D�9H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A� �A��A��A�&�A�(�A�(�A�(�A�+A�+A�+A�/A�5?A�?}AӸRA��TA�ƨAӸRAӶFAӸRAӸRAӸRA���A��A�  A�VA��`AӅA�=qA��#A�dZA���A�1'A�hsA̋DA��AɮA�XAÃA�1'A��PA�1'A�"�A�K�A�n�A��!A��hA��A�-A�p�A��\A��A�|�A���A��yA�I�A���A�=qA���A�?}A��A���A��A�A�A�oA��A��uA��A�~�A��HA� �A��-A�
=A���A���A�ȴA��A�33A�M�A��\A�
=A��A���A��A�jA��^A� �A�O�A��A�33A���A��DA�JA�5?A���A��A��A�5?A�?}A�^5A�9XA��/A�+A��7A��A��A�p�A~Q�A{�Az�!Ay�Ax�Au�FAs�As�Ar�Ar��Ar(�Aq;dAp�uAn��Al�RAj��AiC�Ahv�AgS�Ae&�Ab1'AaoA`r�A_��A^�A]�A]�A\�9A[��AX-AW+AUXAR��AP�yAP��AO?}AI33AD�ACdZAB��AA��AA33A@$�A?\)A>�A>-A=`BA;��A9��A7�7A6��A4�A3�A1�A/��A/A.�A++A*5?A(��A'ƨA&�A%�hA#�#A!�A �A��A�+A��AK�A�+A��AdZA��A�9A��A�A��AhsAQ�AA+Al�AG�AbA�wAC�AE�AG�A5?A
r�A�yA�A;dA�\A�
A|�AS�At�AoA�AdZAȴAQ�A��A Q�@��@��j@�@���@��@�C�@�@�=q@���@��@�V@�9@�P@�!@�\@��@�7@�7L@�Z@�1@��
@�S�@޸R@�Ĝ@�  @�|�@ڸR@ش9@�r�@�(�@���@��
@��H@�J@���@�O�@���@ӍP@�?}@�j@��m@�
=@Η�@���@�G�@���@��@�+@��T@�x�@��@�b@�\)@�v�@ŉ7@Å@�n�@���@�&�@�1@�
=@�V@���@��@��m@���@�ff@�ƨ@���@��@�5?@�E�@�@�hs@��
@���@��!@���@���@��@�A�@�l�@�C�@�l�@�"�@�ȴ@�5?@��@��7@��`@�(�@�;d@��@�ȴ@���@�v�@�ff@���@�7L@�Ĝ@�bN@�9X@��w@�dZ@��@�J@��T@���@�@��-@��7@�`B@�?}@�V@�A�@�ƨ@�l�@��H@�ȴ@�n�@�-@�@��7@�G�@��@��j@��@�A�@��F@�|�@�;d@�C�@�K�@��@��\@�ff@�E�@�$�@��@�G�@�V@��`@��F@�dZ@�S�@�C�@�+@��H@��@�X@�7L@�&�@�%@�Z@��@���@� �@��m@�(�@��D@��@�K�@�^5@�5?@���@���@���@���@�J@��T@���@��D@�Q�@��@�n�@�=q@�-@�J@�J@���@��7@�7L@�7L@�V@�&�@�p�@�x�@��@���@��/@�V@�O�@�&�@�A�@��u@��@�@�V@�=q@��@���@��h@�x�@��-@�x�@���@��D@��F@�|�@�dZ@�C�@���@���@��!@��+@�@���@��;@�1@��9@���@�%@��/@��@��@�9X@��
@���@�o@���@�^5@��@��@�=q@�v�@�M�@�$�@���@��T@�/@�&�@��@�/@�7L@�&�@�%@��j@�9X@� �@��@���@��w@�+@��!@�n�@��@��@��@���@��-@�&�@�Ĝ@��@�Z@�Z@��@�z�@�bN@� �@��@��m@��
@���@���@��@��@��H@���@���@�n�@���@��^@��-@���@��@�G�@�%@��`@���@��u@�z�@�A�@���@��
@�ƨ@���@�+@�@��@��R@�E�@��^@��-@��h@�X@�7L@�&�@�V@�%@���@��D@�j@�Q�@�b@�@~�y@~@}��@}?}@}�@|�/@|z�@|�@{C�@z~�@y��@y�@x��@w��@v�R@v��@v�+@vV@v5?@u�T@u`B@t�j@tj@t�@s�m@s�F@r�!@r=q@rJ@q��@q��@q7L@p�@o��@o
=@n�@n�+@m�@l��@l�D@lZ@k��@j��@jJ@i�@i�7@iG�@i&�@h��@h��@hr�@g�@g�@gl�@f�y@f��@fV@e/@dj@c��@c33@c@b�!@b~�@b=q@a��@ahs@a%@`Ĝ@`bN@`b@`  @`  @_�;@_|�@_\)@_\)@^��@^�R@^V@^{@]��@]/@\I�@\�@[t�@[C�@[o@Z�H@Z�!@Z~�@Y��@YX@Y&�@X�`@X�@XA�@Xb@W�@W|�@W�@Vȴ@V$�@U�h@U`B@U?}@U/@U�@T�/@T�j@TZ@S�
@St�@SS�@S"�@R�H@R�@Q7L@P��@P��@P�u@PbN@P  @O��@O|�@Ol�@O;d@N�R@M@M?}@L�j@L��@L(�@K�
@KdZ@J~�@J=q@JJ@I��@I7L@H�`@Hr�@H �@G��@G�P@G�P@Gl�@F�y@Fv�@F$�@E��@E/@E�@D��@D�@D��@Dz�@D9X@D�@D�@C�
@C�@CS�@B�H@A�@A��@A��@A��@Ax�@A7L@A�@A%@@��@@Q�@@A�@@1'@?��@?\)@?;d@>��@>�+@>@=�-@=��@=��@=�@=`B@=?}@<��@<�@<��@<�D@<9X@;��@;t�@;33@;@9��@97L@9%@8�`@8Ĝ@8A�@7�@7��@7�w@7�P@7+@7
=@6�y@6ȴ@6��@6�+@6E�@5�-@5�@5O�@5/@4�@4�@4�D@4j@4Z@49X@4�@41@3S�@3@2�@2�@2�H@2��@2n�@2�@1hs@0Ĝ@/�;@/l�@/;d@/+@/+@/�@/
=@.ff@-��@-�h@-�h@-�h@-�@-`B@-V@,�D@,9X@,(�@,1@+�F@+dZ@+"�@*�H@*��@*=q@*�@)��@)�@)�@)�#@)�^@)��@)��@)�@(��@(bN@(b@'��@'��@'\)@&�@&��@&V@&5?@%�@%p�@%/@$�/@$�j@$�D@$1@#��@#dZ@#33@#33@#C�@#"�@"��@"~�@"M�@"=q@"-@!�#@!X@!%@ �9@ Q�@��@|�@K�@;d@;d@K�@�@
=@��@��@�y@�y@�@�R@�+@V@E�@{@��@�h@O�@/@V@�/@�/@�@��@z�@9X@��@�m@�m@ƨ@��@��@dZ@"�@o@�@�H@�H@�!@n�@M�@=q@�@�@��@�7@x�@hs@G�@��@Ĝ@r�@�;@�;@|�@;d@�@�@�+@V@{@{@�@�T@��@��@��@��@@�h@�@O�@?}@?}@V@�/@�j@�@�@z�@�@��@33@@�H@�!@�\@�\@~�@^5@J@�#@��@hs@G�@�`@�9@r�@A�@b@  @�@�;@�w@��@l�@K�@K�@+@�y@�@�R@��@��@V@5?@$�@5?@5?@$�@{@�h@p�@O�@V@��@�@�D@Z@I�@I�@1@�
@�F@��@t�@C�@
�H@
��@
~�@
~�@
~�@
n�@
M�@
=q@
-@
-@
-@
-@	�@	x�@	7L@�`@�`@�9@r�@ �@  @�;@�w@�@�@�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A� �A��A��A�&�A�(�A�(�A�(�A�+A�+A�+A�/A�5?A�?}AӸRA��TA�ƨAӸRAӶFAӸRAӸRAӸRA���A��A�  A�VA��`AӅA�=qA��#A�dZA���A�1'A�hsA̋DA��AɮA�XAÃA�1'A��PA�1'A�"�A�K�A�n�A��!A��hA��A�-A�p�A��\A��A�|�A���A��yA�I�A���A�=qA���A�?}A��A���A��A�A�A�oA��A��uA��A�~�A��HA� �A��-A�
=A���A���A�ȴA��A�33A�M�A��\A�
=A��A���A��A�jA��^A� �A�O�A��A�33A���A��DA�JA�5?A���A��A��A�5?A�?}A�^5A�9XA��/A�+A��7A��A��A�p�A~Q�A{�Az�!Ay�Ax�Au�FAs�As�Ar�Ar��Ar(�Aq;dAp�uAn��Al�RAj��AiC�Ahv�AgS�Ae&�Ab1'AaoA`r�A_��A^�A]�A]�A\�9A[��AX-AW+AUXAR��AP�yAP��AO?}AI33AD�ACdZAB��AA��AA33A@$�A?\)A>�A>-A=`BA;��A9��A7�7A6��A4�A3�A1�A/��A/A.�A++A*5?A(��A'ƨA&�A%�hA#�#A!�A �A��A�+A��AK�A�+A��AdZA��A�9A��A�A��AhsAQ�AA+Al�AG�AbA�wAC�AE�AG�A5?A
r�A�yA�A;dA�\A�
A|�AS�At�AoA�AdZAȴAQ�A��A Q�@��@��j@�@���@��@�C�@�@�=q@���@��@�V@�9@�P@�!@�\@��@�7@�7L@�Z@�1@��
@�S�@޸R@�Ĝ@�  @�|�@ڸR@ش9@�r�@�(�@���@��
@��H@�J@���@�O�@���@ӍP@�?}@�j@��m@�
=@Η�@���@�G�@���@��@�+@��T@�x�@��@�b@�\)@�v�@ŉ7@Å@�n�@���@�&�@�1@�
=@�V@���@��@��m@���@�ff@�ƨ@���@��@�5?@�E�@�@�hs@��
@���@��!@���@���@��@�A�@�l�@�C�@�l�@�"�@�ȴ@�5?@��@��7@��`@�(�@�;d@��@�ȴ@���@�v�@�ff@���@�7L@�Ĝ@�bN@�9X@��w@�dZ@��@�J@��T@���@�@��-@��7@�`B@�?}@�V@�A�@�ƨ@�l�@��H@�ȴ@�n�@�-@�@��7@�G�@��@��j@��@�A�@��F@�|�@�;d@�C�@�K�@��@��\@�ff@�E�@�$�@��@�G�@�V@��`@��F@�dZ@�S�@�C�@�+@��H@��@�X@�7L@�&�@�%@�Z@��@���@� �@��m@�(�@��D@��@�K�@�^5@�5?@���@���@���@���@�J@��T@���@��D@�Q�@��@�n�@�=q@�-@�J@�J@���@��7@�7L@�7L@�V@�&�@�p�@�x�@��@���@��/@�V@�O�@�&�@�A�@��u@��@�@�V@�=q@��@���@��h@�x�@��-@�x�@���@��D@��F@�|�@�dZ@�C�@���@���@��!@��+@�@���@��;@�1@��9@���@�%@��/@��@��@�9X@��
@���@�o@���@�^5@��@��@�=q@�v�@�M�@�$�@���@��T@�/@�&�@��@�/@�7L@�&�@�%@��j@�9X@� �@��@���@��w@�+@��!@�n�@��@��@��@���@��-@�&�@�Ĝ@��@�Z@�Z@��@�z�@�bN@� �@��@��m@��
@���@���@��@��@��H@���@���@�n�@���@��^@��-@���@��@�G�@�%@��`@���@��u@�z�@�A�@���@��
@�ƨ@���@�+@�@��@��R@�E�@��^@��-@��h@�X@�7L@�&�@�V@�%@���@��D@�j@�Q�@�b@�@~�y@~@}��@}?}@}�@|�/@|z�@|�@{C�@z~�@y��@y�@x��@w��@v�R@v��@v�+@vV@v5?@u�T@u`B@t�j@tj@t�@s�m@s�F@r�!@r=q@rJ@q��@q��@q7L@p�@o��@o
=@n�@n�+@m�@l��@l�D@lZ@k��@j��@jJ@i�@i�7@iG�@i&�@h��@h��@hr�@g�@g�@gl�@f�y@f��@fV@e/@dj@c��@c33@c@b�!@b~�@b=q@a��@ahs@a%@`Ĝ@`bN@`b@`  @`  @_�;@_|�@_\)@_\)@^��@^�R@^V@^{@]��@]/@\I�@\�@[t�@[C�@[o@Z�H@Z�!@Z~�@Y��@YX@Y&�@X�`@X�@XA�@Xb@W�@W|�@W�@Vȴ@V$�@U�h@U`B@U?}@U/@U�@T�/@T�j@TZ@S�
@St�@SS�@S"�@R�H@R�@Q7L@P��@P��@P�u@PbN@P  @O��@O|�@Ol�@O;d@N�R@M@M?}@L�j@L��@L(�@K�
@KdZ@J~�@J=q@JJ@I��@I7L@H�`@Hr�@H �@G��@G�P@G�P@Gl�@F�y@Fv�@F$�@E��@E/@E�@D��@D�@D��@Dz�@D9X@D�@D�@C�
@C�@CS�@B�H@A�@A��@A��@A��@Ax�@A7L@A�@A%@@��@@Q�@@A�@@1'@?��@?\)@?;d@>��@>�+@>@=�-@=��@=��@=�@=`B@=?}@<��@<�@<��@<�D@<9X@;��@;t�@;33@;@9��@97L@9%@8�`@8Ĝ@8A�@7�@7��@7�w@7�P@7+@7
=@6�y@6ȴ@6��@6�+@6E�@5�-@5�@5O�@5/@4�@4�@4�D@4j@4Z@49X@4�@41@3S�@3@2�@2�@2�H@2��@2n�@2�@1hs@0Ĝ@/�;@/l�@/;d@/+@/+@/�@/
=@.ff@-��@-�h@-�h@-�h@-�@-`B@-V@,�D@,9X@,(�@,1@+�F@+dZ@+"�@*�H@*��@*=q@*�@)��@)�@)�@)�#@)�^@)��@)��@)�@(��@(bN@(b@'��@'��@'\)@&�@&��@&V@&5?@%�@%p�@%/@$�/@$�j@$�D@$1@#��@#dZ@#33@#33@#C�@#"�@"��@"~�@"M�@"=q@"-@!�#@!X@!%@ �9@ Q�@��@|�@K�@;d@;d@K�@�@
=@��@��@�y@�y@�@�R@�+@V@E�@{@��@�h@O�@/@V@�/@�/@�@��@z�@9X@��@�m@�m@ƨ@��@��@dZ@"�@o@�@�H@�H@�!@n�@M�@=q@�@�@��@�7@x�@hs@G�@��@Ĝ@r�@�;@�;@|�@;d@�@�@�+@V@{@{@�@�T@��@��@��@��@@�h@�@O�@?}@?}@V@�/@�j@�@�@z�@�@��@33@@�H@�!@�\@�\@~�@^5@J@�#@��@hs@G�@�`@�9@r�@A�@b@  @�@�;@�w@��@l�@K�@K�@+@�y@�@�R@��@��@V@5?@$�@5?@5?@$�@{@�h@p�@O�@V@��@�@�D@Z@I�@I�@1@�
@�F@��@t�@C�@
�H@
��@
~�@
~�@
~�@
n�@
M�@
=q@
-@
-@
-@
-@	�@	x�@	7L@�`@�`@�9@r�@ �@  @�;@�w@�@�@�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�LB
�LB
�FB
�LB
�qB
B
��Bz�B�B�BB
=BVB{B�B2-BN�B]/Br�B�PB��B��B��B��B��B��B��B�\B�=B��B��B�/B�B�B�B�B��B��B�3B�!B�!B�B�B�B��B��B��B��B�{B�bB�VB�DB�%B�B�B�=B�JB�VB��B��B�hBn�B_;BR�BL�BI�BE�BA�B8RB,B&�B�BJBBB��B��B�B�NB��B�'B��B�\B�1B�B|�BbNB]/BZBR�B5?B#�B	7B
�B
�ZB
�B
ƨB
�?B
��B
��B
�+B
r�B
jB
aHB
XB
H�B
:^B
5?B
2-B
1'B
-B
%�B
!�B
{B
+B	��B	�B	�mB	�;B	��B	ĜB	�qB	�XB	�9B	�B	��B	��B	��B	��B	�=B	� B	v�B	e`B	W
B	Q�B	E�B	(�B	
=B	B��B��B��B�B�B�mB�`B�;B�#B��B��BȴB��B�dB�LB�'B�B�B��B��B��B��B��B��B�{B�hB�VB�VB�DB�1B�+B�%B�B� B� B�B�B�B�DB�bB�DB�1B�B{�Bp�Bn�Bm�Bn�Bn�Bl�BhsBffBjBhsBffBffBjBiyBjBo�Bp�Bt�Bs�Bt�B� B�B}�B~�Bu�BjBffBbNB]/B[#B\)B[#B_;B`BBe`BgmBgmBgmBgmBffBgmBjBjBk�Bk�Bl�Bp�Br�Bs�Bu�Bw�Bx�B{�B|�B|�B�B�%B�1B�7B�7B�JB�PB�\B�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�FB�LB�XB�^B�qB�wBBĜBȴB��B��B��B�#B�sB�B�B��B��B��B��B��B��B��B��B	B	  B	B	+B	
=B	JB	JB	PB	VB	bB	oB	�B	�B	�B	�B	�B	�B	 �B	&�B	)�B	-B	0!B	2-B	49B	6FB	9XB	>wB	?}B	?}B	@�B	@�B	A�B	C�B	C�B	D�B	J�B	L�B	N�B	S�B	T�B	XB	YB	]/B	^5B	_;B	`BB	cTB	dZB	e`B	gmB	iyB	k�B	k�B	k�B	m�B	r�B	t�B	u�B	v�B	w�B	|�B	|�B	|�B	�B	�B	�+B	�1B	�=B	�VB	�\B	�bB	�hB	�hB	�{B	��B	��B	��B	��B	��B	�B	�!B	�?B	�3B	�-B	�-B	�-B	�3B	�9B	�dB	�}B	��B	B	B	B	B	�qB	�qB	�jB	�qB	�}B	B	ÖB	ŢB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�BB	�ZB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
+B
+B
+B
1B

=B

=B
DB
DB
DB
DB
DB

=B

=B

=B

=B
JB
JB
DB
DB
DB
DB
	7B
1B
1B
1B
	7B

=B

=B
DB
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
\B
\B
\B
\B
bB
bB
hB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
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
+B
+B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
.B
/B
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
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
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
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
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
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
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
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
W
B
XB
XB
W
B
W
B
XB
XB
YB
YB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
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
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
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
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
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
gmB
hsB
hsB
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
jB
jB
jB
jB
jB
jB
k�B
k�B
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
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
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
q�B
r�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
w�B
w�B
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
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�LB
�LB
�FB
�LB
�VB
�[B
��By�BٴB�BB
#BVBaBsB1�BN�B]IBs3B��B�B�B��B�B��B�B��B��B��B��B�:B�bB�CBݲB��B�=B�sB�mB��B�9B��B�|B��B�]B�B��B��B�
B��B� B��B��B��B�?B�EB�B��B�\B�eB�)B�{BpUB`�BTBN"BJrBF�BC{B:^B.B)DB�BPB�B�B�BB��B��B��BΥB�B�/B�bB�7B��B�4Bc:B^jB\xBV�B88B(>BPB
�B
�B
ݲB
�lB
��B
��B
��B
��B
t9B
l"B
cTB
Z�B
J�B
;0B
5�B
2�B
1�B
.IB
'8B
$B

B
	�B	�xB	��B	�_B	�B	��B	�B	�]B	�xB	��B	�/B	��B	��B	��B	�BB	�B	��B	y�B	g�B	XyB	U2B	L~B	-wB	JB	3B��B�B�$B��B�wB�XB��B�BݲB�gB�VB��B�'B��B��B�|B� B�5B�RB��B�\B�)B��B��B�
B��B��B��B�dB�B��B�KB��B��B�OB�AB�{B�B�JB��B�JB��B��B~]Br-BoOBn�BpBp;BnIBj�BhXBk�BiyBgmBgRBj�Bi�Bj�BpoBq�Bu�Bt�Bu�B�;B��BB� Bw�Bl"Bh
BdB_B\�B]~B]IBabBabBf2Bg�Bg�Bg�Bg�Bf�Bh
Bj�Bj�BlBl=Bm�BqABs3Bt�Bv�BxBy$B|B}<B}�B��B��B��B��B�rB��B��B��B�B��B�+B�	B�/B�vB��B��B�_B�mB��B��B��B�B�vB�B��B��B�0B�(B�B�-B�SB�RB�^B��B�VB�#B�yB��B��B�8B��B��B�`B��B��B�2B	  B	�B	 �B	;B	+B	
�B	�B	�B	�B	�B	�B	B	B	�B	�B	�B	�B	�B	!HB	'8B	*eB	-]B	0oB	2�B	4�B	6�B	9�B	>�B	?�B	?�B	@�B	@�B	A�B	C�B	C�B	E9B	KB	MB	OBB	TB	UMB	XEB	YeB	]dB	^jB	_pB	`�B	c�B	d�B	e�B	g�B	i�B	k�B	k�B	k�B	m�B	r�B	t�B	u�B	wB	xRB	}"B	}<B	}�B	�UB	�9B	�EB	�fB	��B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�UB	�FB	��B	�aB	�aB	�aB	�B	�B	��B	��B	��B	�-B	��B	�-B	�-B	��B	��B	��B	��B	��B	��B	��B	ŢB	��B	ƨB	ȚB	��B	�,B	�B	�B	��B	�B	�kB	چB	��B	��B	��B	�kB	�B	��B	��B	�B	�B	�B	��B	�B	�B	�-B	��B	��B	��B	�B	��B	�	B	�$B	�RB	��B	�B	�oB	�|B	��B	��B	��B	��B	�B	�PB	�0B	�6B	�JB	�*B	�B	��B	��B	��B	��B
 B
-B
GB
AB
�B
%B
+B
B
+B
KB

rB

�B
�B
xB
DB
xB
xB

�B

�B

rB

rB
~B
dB
^B
xB
�B
�B
	RB
fB
1B
1B
	RB

XB

rB
^B
JB
dB
~B
dB
�B
�B
jB
jB
jB
�B
�B
vB
vB
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
 B
 B
!B
"B
!�B
"4B
# B
#�B
#�B
#�B
#�B
$B
%,B
%B
%�B
%�B
%�B
&B
&2B
'B
'B
'B
'B
'B
'8B
'8B
($B
(
B
(>B
(>B
)DB
)*B
)*B
)DB
*KB
*0B
*B
*0B
+B
+B
+B
+B
+QB
+6B
,"B
,=B
,=B
-)B
-CB
-�B
.cB
/iB
0UB
0;B
0;B
0;B
1AB
1AB
1vB
2GB
2GB
2GB
3MB
2GB
2-B
2GB
2GB
3MB
33B
3hB
4TB
4nB
4TB
4nB
4�B
5�B
5ZB
5tB
6zB
6zB
6zB
6zB
6zB
6�B
7fB
7fB
7fB
7�B
7fB
7fB
8lB
8lB
8�B
8�B
8�B
9�B
:xB
:xB
:^B
:xB
:xB
:xB
:�B
;�B
;B
;B
;B
;�B
;�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
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
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
IB
I�B
I�B
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
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
MB
L�B
L�B
M�B
M�B
M�B
NB
NB
N�B
N�B
OB
OBB
PB
Q B
Q B
Q B
QB
Q�B
RB
RB
RB
S&B
SB
SB
SB
SB
SB
T,B
T,B
TB
T�B
T�B
UB
U2B
UB
U2B
VB
VB
VB
VB
V9B
W$B
W
B
XB
XB
W$B
W?B
XEB
X_B
YKB
YeB
[=B
[=B
[#B
[#B
[=B
[=B
[qB
\]B
]/B
]/B
]IB
]IB
]IB
]IB
]~B
]IB
^OB
^OB
^OB
^OB
^OB
^OB
^OB
_VB
_VB
_VB
_;B
_;B
_;B
_VB
_VB
_VB
_pB
_VB
_;B
_VB
_VB
_pB
`\B
`vB
`\B
abB
abB
abB
a|B
abB
bhB
bhB
bhB
b�B
cnB
cnB
cnB
cTB
cTB
cnB
c�B
cnB
dtB
dtB
dtB
dtB
d�B
ezB
ezB
e�B
e�B
f�B
f�B
ffB
ffB
gmB
ffB
gmB
gmB
g�B
gmB
gmB
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
iyB
i�B
i�B
i�B
i�B
i�B
jB
jB
j�B
j�B
j�B
j�B
j�B
k�B
k�B
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
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
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
q�B
r�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
w�B
w�B
w�B
xB
x�B
x�B
x�B
x�B
x�B
x�B
y$B
zB
y�B
y�B
y�B
y�B
zB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
|B
{�B
{�B
|B
|B
|B
|B
{�B
{�B
{�B
}B
}"B
~(B
~B
~B
}�B
~B
~B
~B
~B
~�B
B
~�B
~�B
~�B
� B
� B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201612090037042016120900370420161209003704201806221305572018062213055720180622130557201804050706092018040507060920180405070609  JA  ARFMdecpA19c                                                                20161205093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161205003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161205003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161205003525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161205003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161205003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161205003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161205003526  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20161205003526                      G�O�G�O�G�O�                JA  ARUP                                                                        20161205013421                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161205153415  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161208153704  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161208153704  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220609  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040557  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                