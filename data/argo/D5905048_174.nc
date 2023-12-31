CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-10-28T00:35:25Z creation;2017-10-28T00:35:29Z conversion to V3.1;2019-12-19T07:54:01Z update;     
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  sh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171028003525  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_174                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�0�:7_ 1   @�0���� @4�I�^�d�V�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D��3D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�C3D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D�vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @8��@\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B0\)B7��B?��BG��BO��BW��B_��Bg��Bo��Bx\)B��B���B�ǮB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC
C�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DJ�DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj��Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��D���D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�B�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�B�D�v11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�/A�(�A�$�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�
=A�1A�1A�A���A���A���A��AבhA��AՁA�jA�&�A�VA˧�A�I�A�Q�A��`A��A�jAŶFA���A�XA��A�%A�{A��TA��DA��yA��A�A��HA���A�"�A��hA�$�A�S�A���A�"�A��A�  A�p�A�VA�dZA��RA�A�A��A�?}A��A�x�A�VA���A�ZA���A��
A��/A��FA�VA�~�A�?}A���A�/A�ZA��`A��TA�?}A�=qA��A���A��;A��-A��HA��9A�=qA�VA�ƨA�XA�n�A�$�A��A��#A�+A���A���A���A���A��A��;A�-A��PA~I�A|jA{��AwXAt��AsAqp�Ao�-An{Al��Ai�TAf=qAc��Aa��A^�!A\�uAZz�AX��AXbNAX(�AVbAT(�AR~�AP��APJAO+ANVAKAH��AEG�AB��AA�mAA�-AAx�A?�A<�!A<JA<  A;�A;;dA:A8ffA6��A5?}A4�DA3�FA3C�A2�9A2  A17LA09XA/��A/`BA-�A,ffA+C�A*�9A*=qA)+A(M�A&��A%dZA$^5A#G�A"^5A ��A\)A5?AoA�^A�\A�A1'A�A�AĜA~�A�#A��AAp�AA �A�RA;dA�`AĜA�9A��A=qA��A%A�Av�AbAS�A
ȴA	��A	�A	x�A	S�A��AZA��A�A�\A�A�TA��A|�AO�A �!@�C�@�~�@�@��u@��F@�x�@�o@�r�@�@�@�1'@�\@�J@�j@�1@�+@��@�j@ᙚ@��@�dZ@�ff@�7L@� �@�^5@ف@�/@��`@أ�@���@�+@֟�@�7L@�1'@�+@ҧ�@�5?@�$�@���@��m@�S�@�@�E�@� �@�33@���@ʗ�@�n�@�/@��/@�Q�@�33@�n�@�O�@ēu@�Q�@���@���@�5?@��#@�G�@��@��u@�|�@�;d@�^5@�$�@��7@��@���@��@��+@���@��j@�z�@��
@�;d@�v�@���@��^@��^@���@���@��@�A�@��@���@�K�@���@�v�@�n�@�-@��7@��@��9@�r�@�1'@�
=@�-@�hs@�/@��@�?}@��^@��@��@��@���@��/@���@�Z@��@�
=@��H@�ȴ@�~�@��T@���@�@�O�@��@��
@�;d@���@��m@��
@��F@���@���@��D@��9@�1'@���@��@���@�C�@�r�@�%@�V@�r�@���@��P@�33@��R@���@�v�@�^5@�^5@�n�@�$�@��T@�@���@�hs@�?}@��/@�z�@�I�@���@��
@�ƨ@��P@�
=@�ȴ@��+@�M�@��T@�hs@��@��/@�1'@��;@��;@��
@��F@��@�|�@�+@�"�@�
=@��!@�V@�J@���@�G�@�Ĝ@�z�@�(�@���@���@�Q�@�l�@��@�ȴ@���@�~�@�n�@�M�@�E�@��@���@�@�hs@�O�@�G�@�&�@���@��@���@��j@���@��D@�1'@���@��
@��F@��@�S�@�33@��@�
=@�@��H@���@���@�~�@�ff@��@��@�x�@�G�@�&�@��`@��D@�j@�j@�j@�j@�r�@�bN@�Q�@��@��w@�dZ@��@��R@���@�n�@�=q@�{@���@��h@��@�x�@�x�@�`B@�?}@��@��/@���@�I�@�b@��m@���@��;@��
@��w@���@�\)@�33@�
=@�
=@���@�J@���@�hs@���@��`@��u@�1'@��@�  @��;@�ƨ@��@���@��P@�t�@�\)@��y@��R@���@�~�@�=q@��@��T@���@��^@��7@��@�`B@�%@��`@���@�Q�@�1'@��@��
@��P@�\)@��@��y@��R@�~�@��@���@��h@��@�7L@���@��`@��`@��/@���@��/@�Ĝ@�9X@� �@� �@�1@�@�1@�;@l�@
=@~E�@}��@}?}@|��@|j@|1@{��@{33@z�H@z��@zn�@y��@yhs@y&�@x��@x��@x��@xA�@x1'@x  @w�;@w��@w;d@v��@u�@u�@u�@t�@t�@sƨ@sC�@r��@r-@q�#@q��@q�7@q�7@qx�@qX@pbN@n�@m@m��@mp�@mV@l�@lz�@l1@k��@j�H@j��@i��@ihs@h�`@h�u@h�u@hA�@h1'@g�@g�P@g�@h1'@g�;@h  @g�@g�@fȴ@fff@fV@f5?@f{@e�@e��@ep�@d��@d�@dZ@d(�@d1@c�m@c��@cS�@c33@b��@b�\@b�\@b~�@bn�@b^5@b�@ahs@a7L@`��@`A�@_��@^��@]@]p�@\�@\z�@\1@[dZ@[S�@[C�@[33@Z�H@Z��@Zn�@Z�@Y&�@X��@X��@XQ�@Xb@W�P@Vȴ@V��@V�+@V�+@Vv�@VV@U�-@T�@T�@T�@T1@S��@S�F@SS�@R��@RM�@Q��@QG�@P�u@O�@O�P@N��@N��@Nv�@NV@M@MV@L��@K�
@Kt�@K"�@K@J�!@I�^@I��@I�7@I7L@HĜ@G�;@G
=@F�@F�R@F��@F5?@F{@E�@E�T@E�h@D�/@D��@DZ@D(�@C��@C��@C33@C33@C@B�@B�@B�!@B~�@Bn�@B^5@B�@A�@Ax�@?�;@?��@?+@>��@>��@>{@=��@=��@=��@=@=�-@<�j@<�D@<j@<I�@<9X@<Z@<(�@;��@;dZ@:�@:~�@:-@:J@9��@9�#@9��@9�^@9��@9X@9�@8��@8��@8��@8��@8��@8bN@8 �@8b@8  @7��@7;d@6��@6�y@6�R@6��@6�+@6ff@6$�@5�@5�-@5�h@5�@5/@4�@4j@41@3��@3��@3��@3��@333@2��@2�\@2=q@2J@1�@1�7@1G�@0��@0�`@0Ĝ@0��@0�u@0r�@0bN@01'@/�;@/\)@/�@.ȴ@.��@.v�@.5?@-��@-?}@-V@,��@,��@,j@+�m@+�F@+�@+"�@*�H@*�!@*~�@*=q@)��@)x�@)G�@)�@)%@(�`@(�9@(bN@'�;@'�@'\)@&�y@&�R@&�+@&5?@&@%��@%�h@$�@$��@$z�@$j@$(�@#��@#�
@#��@#��@#t�@#"�@"�@"�!@"n�@!��@!�^@!��@!�7@!hs@!7L@ �`@ ��@ r�@ 1'@   @   @�@�@\)@;d@+@�y@ȴ@�+@E�@�@��@��@@@`B@?}@/@�/@�D@j@I�@1@�
@��@dZ@33@�H@��@�!@n�@-@�#@hs@7L@&�@��@Ĝ@�u@bN@A�@b@�w@�P@|�@K�@;d@�y@�R@v�@ff@�@�T@��@p�@?}@�@�@��@�/@�@�D@I�@1@1@1@��@�
@�F@�@33@"�@"�@"�@o@�H@��@��@��@�\@^5@=q@�@��@�7@�7@�7@x�@x�@hs@G�@%@Ĝ@��@r�@bN@1'@1'@1'@ �@  @�@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�/A�(�A�$�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�
=A�1A�1A�A���A���A���A��AבhA��AՁA�jA�&�A�VA˧�A�I�A�Q�A��`A��A�jAŶFA���A�XA��A�%A�{A��TA��DA��yA��A�A��HA���A�"�A��hA�$�A�S�A���A�"�A��A�  A�p�A�VA�dZA��RA�A�A��A�?}A��A�x�A�VA���A�ZA���A��
A��/A��FA�VA�~�A�?}A���A�/A�ZA��`A��TA�?}A�=qA��A���A��;A��-A��HA��9A�=qA�VA�ƨA�XA�n�A�$�A��A��#A�+A���A���A���A���A��A��;A�-A��PA~I�A|jA{��AwXAt��AsAqp�Ao�-An{Al��Ai�TAf=qAc��Aa��A^�!A\�uAZz�AX��AXbNAX(�AVbAT(�AR~�AP��APJAO+ANVAKAH��AEG�AB��AA�mAA�-AAx�A?�A<�!A<JA<  A;�A;;dA:A8ffA6��A5?}A4�DA3�FA3C�A2�9A2  A17LA09XA/��A/`BA-�A,ffA+C�A*�9A*=qA)+A(M�A&��A%dZA$^5A#G�A"^5A ��A\)A5?AoA�^A�\A�A1'A�A�AĜA~�A�#A��AAp�AA �A�RA;dA�`AĜA�9A��A=qA��A%A�Av�AbAS�A
ȴA	��A	�A	x�A	S�A��AZA��A�A�\A�A�TA��A|�AO�A �!@�C�@�~�@�@��u@��F@�x�@�o@�r�@�@�@�1'@�\@�J@�j@�1@�+@��@�j@ᙚ@��@�dZ@�ff@�7L@� �@�^5@ف@�/@��`@أ�@���@�+@֟�@�7L@�1'@�+@ҧ�@�5?@�$�@���@��m@�S�@�@�E�@� �@�33@���@ʗ�@�n�@�/@��/@�Q�@�33@�n�@�O�@ēu@�Q�@���@���@�5?@��#@�G�@��@��u@�|�@�;d@�^5@�$�@��7@��@���@��@��+@���@��j@�z�@��
@�;d@�v�@���@��^@��^@���@���@��@�A�@��@���@�K�@���@�v�@�n�@�-@��7@��@��9@�r�@�1'@�
=@�-@�hs@�/@��@�?}@��^@��@��@��@���@��/@���@�Z@��@�
=@��H@�ȴ@�~�@��T@���@�@�O�@��@��
@�;d@���@��m@��
@��F@���@���@��D@��9@�1'@���@��@���@�C�@�r�@�%@�V@�r�@���@��P@�33@��R@���@�v�@�^5@�^5@�n�@�$�@��T@�@���@�hs@�?}@��/@�z�@�I�@���@��
@�ƨ@��P@�
=@�ȴ@��+@�M�@��T@�hs@��@��/@�1'@��;@��;@��
@��F@��@�|�@�+@�"�@�
=@��!@�V@�J@���@�G�@�Ĝ@�z�@�(�@���@���@�Q�@�l�@��@�ȴ@���@�~�@�n�@�M�@�E�@��@���@�@�hs@�O�@�G�@�&�@���@��@���@��j@���@��D@�1'@���@��
@��F@��@�S�@�33@��@�
=@�@��H@���@���@�~�@�ff@��@��@�x�@�G�@�&�@��`@��D@�j@�j@�j@�j@�r�@�bN@�Q�@��@��w@�dZ@��@��R@���@�n�@�=q@�{@���@��h@��@�x�@�x�@�`B@�?}@��@��/@���@�I�@�b@��m@���@��;@��
@��w@���@�\)@�33@�
=@�
=@���@�J@���@�hs@���@��`@��u@�1'@��@�  @��;@�ƨ@��@���@��P@�t�@�\)@��y@��R@���@�~�@�=q@��@��T@���@��^@��7@��@�`B@�%@��`@���@�Q�@�1'@��@��
@��P@�\)@��@��y@��R@�~�@��@���@��h@��@�7L@���@��`@��`@��/@���@��/@�Ĝ@�9X@� �@� �@�1@�@�1@�;@l�@
=@~E�@}��@}?}@|��@|j@|1@{��@{33@z�H@z��@zn�@y��@yhs@y&�@x��@x��@x��@xA�@x1'@x  @w�;@w��@w;d@v��@u�@u�@u�@t�@t�@sƨ@sC�@r��@r-@q�#@q��@q�7@q�7@qx�@qX@pbN@n�@m@m��@mp�@mV@l�@lz�@l1@k��@j�H@j��@i��@ihs@h�`@h�u@h�u@hA�@h1'@g�@g�P@g�@h1'@g�;@h  @g�@g�@fȴ@fff@fV@f5?@f{@e�@e��@ep�@d��@d�@dZ@d(�@d1@c�m@c��@cS�@c33@b��@b�\@b�\@b~�@bn�@b^5@b�@ahs@a7L@`��@`A�@_��@^��@]@]p�@\�@\z�@\1@[dZ@[S�@[C�@[33@Z�H@Z��@Zn�@Z�@Y&�@X��@X��@XQ�@Xb@W�P@Vȴ@V��@V�+@V�+@Vv�@VV@U�-@T�@T�@T�@T1@S��@S�F@SS�@R��@RM�@Q��@QG�@P�u@O�@O�P@N��@N��@Nv�@NV@M@MV@L��@K�
@Kt�@K"�@K@J�!@I�^@I��@I�7@I7L@HĜ@G�;@G
=@F�@F�R@F��@F5?@F{@E�@E�T@E�h@D�/@D��@DZ@D(�@C��@C��@C33@C33@C@B�@B�@B�!@B~�@Bn�@B^5@B�@A�@Ax�@?�;@?��@?+@>��@>��@>{@=��@=��@=��@=@=�-@<�j@<�D@<j@<I�@<9X@<Z@<(�@;��@;dZ@:�@:~�@:-@:J@9��@9�#@9��@9�^@9��@9X@9�@8��@8��@8��@8��@8��@8bN@8 �@8b@8  @7��@7;d@6��@6�y@6�R@6��@6�+@6ff@6$�@5�@5�-@5�h@5�@5/@4�@4j@41@3��@3��@3��@3��@333@2��@2�\@2=q@2J@1�@1�7@1G�@0��@0�`@0Ĝ@0��@0�u@0r�@0bN@01'@/�;@/\)@/�@.ȴ@.��@.v�@.5?@-��@-?}@-V@,��@,��@,j@+�m@+�F@+�@+"�@*�H@*�!@*~�@*=q@)��@)x�@)G�@)�@)%@(�`@(�9@(bN@'�;@'�@'\)@&�y@&�R@&�+@&5?@&@%��@%�h@$�@$��@$z�@$j@$(�@#��@#�
@#��@#��@#t�@#"�@"�@"�!@"n�@!��@!�^@!��@!�7@!hs@!7L@ �`@ ��@ r�@ 1'@   @   @�@�@\)@;d@+@�y@ȴ@�+@E�@�@��@��@@@`B@?}@/@�/@�D@j@I�@1@�
@��@dZ@33@�H@��@�!@n�@-@�#@hs@7L@&�@��@Ĝ@�u@bN@A�@b@�w@�P@|�@K�@;d@�y@�R@v�@ff@�@�T@��@p�@?}@�@�@��@�/@�@�D@I�@1@1@1@��@�
@�F@�@33@"�@"�@"�@o@�H@��@��@��@�\@^5@=q@�@��@�7@�7@�7@x�@x�@hs@G�@%@Ĝ@��@r�@bN@1'@1'@1'@ �@  @�@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�mB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�yB�B�B�B�B�B�B�yB�`B�)B��B��B�BE�BC�B=qB^5BhsBe`Bk�Bk�BjBp�Bl�BffBp�Bt�Bo�Bt�Bq�Bw�B�DB�1B�B~�B~�Bw�B|�B{�B� B�B~�B|�Bw�BjBy�B}�Bu�Bq�Be`B[#BT�BVBQ�BB�B.B>wB5?B$�B1BVB	7B��B�B�;B�B��B�jB��B��B�{B�DBq�BXB8RB!�B�BB
�B
�B
�;B
��B
��B
ɺB
�RB
��B
�bB
� B
v�B
YB
?}B
49B
9XB
�B
hB
1B
B	�B	�yB	�;B	ĜB	�B	��B	�\B	|�B	o�B	gmB	ZB	]/B	XB	F�B	:^B	7LB	,B	)�B	$�B	�B	JB��B�`B�`B�sB�B�ZB�BB��B�#B�
B��BŢB�jB�dB�?B�qB�dB�jB�XB�?B�-B�B�B�B��B��B��B��B��B��B�{B�PB�JB�VB�DB�1B~�Bz�Bz�Bu�Bs�Bq�Bp�Bm�Bl�Bu�Bt�Bq�BjBffBaHBhsBdZB]/BXBXBffBiyBiyBgmBbNB`BB]/B`BBaHB^5B[#BZBXB^5BaHB^5BYBR�BP�BJ�B;dBD�BM�BXBYBVBP�BL�BQ�BR�BM�BN�BG�BF�BH�BO�BP�BN�BH�BN�BE�B49B=qBG�BJ�BC�BL�BT�BR�BS�BXBZBcTBiyBl�Bk�BhsBk�Bk�BhsBm�Bo�Bu�Bu�Bw�Br�Bs�Bx�Bz�Bw�Bs�B� B�%B�+B�+B�B�1B�+B�B�=B�JB�bB��B�{B�oB��B��B��B��B��B��B��B��B��B��B��B�B�!B�9B�3B�XB��B�}B��BĜBȴB��B��B��B��B��B�
B�#B�ZB�`B�mB�B�B�B�B�B�B��B��B�B��B	B	DB	VB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	#�B	#�B	+B	1'B	6FB	8RB	8RB	;dB	E�B	J�B	L�B	O�B	Q�B	T�B	[#B	^5B	^5B	aHB	bNB	XB	^5B	q�B	t�B	s�B	s�B	w�B	~�B	� B	� B	�%B	�1B	�1B	�7B	�DB	�PB	�bB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�'B	�LB	�dB	�dB	�^B	�^B	�^B	�^B	�jB	�jB	�dB	�wB	��B	�}B	�wB	�wB	�wB	�}B	ƨB	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�5B	�/B	�;B	�HB	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�`B	�mB	�mB	�mB	�mB	�fB	�fB	�fB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B

=B

=B

=B
	7B

=B

=B
1B

=B
DB
	7B

=B
DB

=B
DB
DB
DB
DB
DB
DB
DB
JB
PB
VB
PB
VB
bB
bB
bB
hB
bB
hB
bB
uB
{B
{B
{B
{B
uB
uB
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
 �B
 �B
"�B
&�B
$�B
&�B
%�B
%�B
'�B
'�B
)�B
(�B
)�B
)�B
)�B
(�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
,B
,B
,B
,B
,B
+B
)�B
+B
+B
(�B
'�B
&�B
'�B
)�B
)�B
)�B
)�B
)�B
-B
-B
-B
,B
-B
-B
,B
,B
.B
/B
.B
/B
.B
.B
1'B
1'B
2-B
2-B
1'B
0!B
/B
0!B
33B
33B
2-B
2-B
2-B
1'B
33B
33B
49B
49B
49B
5?B
5?B
7LB
8RB
8RB
7LB
7LB
8RB
8RB
:^B
;dB
;dB
;dB
:^B
=qB
=qB
<jB
<jB
;dB
=qB
@�B
@�B
@�B
@�B
A�B
A�B
A�B
@�B
@�B
B�B
B�B
B�B
C�B
B�B
B�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
D�B
C�B
C�B
@�B
F�B
F�B
G�B
G�B
G�B
H�B
J�B
I�B
I�B
H�B
F�B
I�B
J�B
J�B
K�B
K�B
K�B
J�B
J�B
J�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
M�B
M�B
N�B
N�B
N�B
M�B
M�B
M�B
O�B
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
P�B
P�B
Q�B
Q�B
P�B
P�B
P�B
Q�B
R�B
S�B
S�B
S�B
R�B
R�B
T�B
S�B
T�B
T�B
T�B
T�B
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
W
B
W
B
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
W
B
YB
YB
YB
YB
XB
ZB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
[#B
\)B
\)B
\)B
[#B
[#B
]/B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
aHB
aHB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
bNB
cTB
dZB
e`B
e`B
e`B
e`B
dZB
e`B
ffB
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
ffB
gmB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
m�B
n�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
o�B
o�B
n�B
o�B
p�B
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
r�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
r�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
s�B
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
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�sB�B�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�B�B�B�B�B�B�B�B��B�B��B��BܒB�BG�BG_BBB_�Bi�BgRBl�Bm)Bl=Br�Bo�BjKBr�Bw�Br�Bw�BtBzDB�6B��B�9B�OB�By�B~(B}qB�UB�AB�4B~(By�Bm�Bz�B~�BwLBshBh$B^OBWsBW?BS[BEmB1�B>�B6�B'mB�B�B
�B�.B��B�B��B�PB�cB��B��B�YB�Bv�B\CB>BB%B!�B	7B
�tB
�B
��B
՛B
�B
ʌB
�B
�VB
��B
��B
y>B
]dB
D�B
6�B
;0B
5B
aB

rB
B	��B	�B	�|B	�KB	�AB	��B	�:B	�iB	raB	i�B	\B	]�B	X�B	IRB	<�B	9XB	-�B	+QB	&LB	�B	�B��B�_B�
B�_B��B�FB�yB��B՛B�=B�sB�B�zB��B�qB�B�wB�jB�"B�DB�FB�MB�OB��B��B��B��B�B��B��B�#B��B�vB�B��B��B��B�;B|�B|�Bw�Bu�BsMBr-Bo5Bm�BvBu%BrGBk�Bg�Bb�BiBeFB^�BZBY�Bf�Bi�Bi�Bg�Bb�BaB^5B`�Ba�B^�B\)B[	BY1B^�Ba|B^�BY�BS�BQ�BLJB>�BFtBO(BX_BYeBV�BQ�BN"BR�BS�BN�BO�BIlBHfBJXBP�BQ�BPbBI�BO�BGB72B>�BH�BK�BE�BM�BU�BS�BT�BX�B[WBc�Bi�Bl�Bk�Bi*BlBl"BiyBncBpUBv+Bv+BxBs�BtnByXB{JBx�Bu%B��B�tB�zB�zB��B��B��B�B��B�B��B��B��B�@B�B��B�/B�B�;B�jB�,B�tB�>B��B��B�iB��B��B��B��B��B� B�B�B�B�B��B��B�4B�{BרBیB�tB��B��B�B�B��B�B� B��B�B�%B�B��B	�B	^B	pB	TB	sB	�B	�B	�B	�B	�B	�B	�B	5B	OB	#�B	$�B	$&B	$@B	+B	1[B	6�B	8�B	9$B	;�B	E�B	J�B	L�B	PB	Q�B	T�B	Z�B	^OB	^�B	a�B	cB	Y�B	^jB	p�B	tnB	s�B	t9B	xRB	B	�OB	�OB	�YB	�KB	�KB	�RB	�^B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�4B	�B	�B	�B	�DB	�]B	�UB	�vB	��B	��B	�dB	�B	�xB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�tB	��B	�6B	�XB	�B	��B	� B	�B	�B	�B	�B	�9B	�9B	�KB	�KB	�=B	�=B	�CB	�CB	�IB	�IB	�OB	�OB	�OB	�~B	�pB	�bB	�HB	�B	�B	�nB	�tB	�zB	�zB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�"B	�B	�B	�(B	�qB	�]B	�cB
 4B	�HB
'B
UB
UB
3B
9B
9B
9B
9B
9B
9B
3B
3B
{B
3B
9B
?B
YB
_B

#B

XB

XB
	lB

XB

XB
�B

XB
xB
	�B

=B
^B

XB
xB
xB
xB
xB
xB
xB
�B
~B
�B
pB
�B
�B
bB
bB
bB
�B
}B
�B
�B
�B
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
B
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
�B
�B
�B
�B
�B
�B
 �B
 �B
"�B
&�B
$�B
&�B
&B
&B
(
B
($B
*B
)B
*B
*B
*0B
)*B
($B
($B
(
B
)B
*B
*B
*0B
*B
*B
*0B
,"B
,B
,B
,"B
,"B
+B
*0B
+6B
+QB
)DB
(>B
'RB
(>B
*B
*KB
*0B
*0B
*0B
-B
-)B
-CB
,"B
-B
-)B
,=B
,qB
./B
/5B
.IB
/5B
./B
.cB
1'B
1AB
2-B
2aB
1AB
0oB
/iB
0UB
33B
33B
2GB
2aB
2aB
1vB
3hB
3�B
4�B
4�B
4�B
5tB
5tB
7fB
8�B
8�B
7�B
7�B
8�B
8�B
:�B
;�B
;B
;�B
:�B
=qB
=�B
<�B
<�B
;�B
=�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
@�B
@�B
B�B
B�B
B�B
C�B
B�B
B�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
D�B
C�B
C�B
AB
F�B
F�B
G�B
G�B
G�B
H�B
J�B
I�B
I�B
H�B
F�B
I�B
J�B
J�B
K�B
K�B
K�B
J�B
J�B
KB
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
M�B
M�B
N�B
N�B
N�B
M�B
M�B
M�B
O�B
N�B
O(B
N�B
O�B
P�B
Q B
Q B
Q B
Q B
Q B
Q B
Q B
RB
Q�B
Q B
Q B
QB
RB
SB
S�B
S�B
TB
S&B
S&B
UB
TB
UB
UB
UB
UB
VB
W$B
W$B
W$B
W
B
W$B
W$B
W$B
VB
V9B
W$B
W$B
XEB
X+B
W$B
W?B
W?B
Y1B
Y1B
Y1B
Y1B
XEB
Z7B
Y1B
YKB
Z7B
Z7B
Z7B
Z7B
ZQB
[=B
\CB
[=B
\)B
\CB
\CB
[WB
[qB
]IB
\CB
\CB
]IB
^OB
^OB
^OB
^OB
^OB
^OB
_;B
abB
aHB
`\B
abB
bhB
bhB
b�B
bhB
bhB
bhB
cnB
bhB
c�B
dtB
e`B
ezB
e�B
ezB
dtB
ezB
f�B
ezB
f�B
ffB
f�B
f�B
f�B
g�B
gmB
f�B
g�B
f�B
g�B
g�B
hsB
h�B
hsB
hsB
g�B
h�B
h�B
h�B
h�B
i�B
i�B
h�B
h�B
hsB
i�B
i�B
i�B
jeB
j�B
j�B
j�B
j�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
m�B
n�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
o�B
o�B
n�B
o�B
p�B
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
r�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
r�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
s�B
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
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711020044502017110200445020171102004450201806221321102018062213211020180622132110201804050723582018040507235820180405072358  JA  ARFMdecpA19c                                                                20171028093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171028003525  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171028003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171028003528  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171028003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171028003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171028003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171028003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171028003529  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171028003529                      G�O�G�O�G�O�                JA  ARUP                                                                        20171028005631                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171028153625  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20171101154450  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171101154450  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222358  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042110  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                