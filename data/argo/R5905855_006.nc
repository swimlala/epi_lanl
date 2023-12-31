CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:11:41Z creation;2022-06-04T19:11:41Z conversion to V3.1      
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604191141  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @شz�]L;1   @ش{[�S@0��-�dO�;dZ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B���B���B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C�C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@�CB  CD  CF  CH  CJ�CK�fCM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{�fD|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @\)@\)@��G@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B \)B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B��{B��{B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�.B�.B���B���B�ǮB�ǮB�ǮB���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qCC�qC�qC�qC�qCC�qC!�qC#�qC%��C'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC>C@CA�qCC�qCE�qCG�qCJCK��CM��CO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCnCpCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C��C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DEx�DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\D{�D{��D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D���D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D�|{D���D���D�&1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�33A��A��A�SA���A̫A̓A�33Aˠ�A�8�A��A���A���A��>A���Aʫ6A�I�A�~�AȔ�A�oA��pAǧRA�c�A�!�A��A���AƗ$A�v�A�c A�bA��A��|A��jA���A��AÏ�A�Z�A���A�A��A��A���A�'�A�m)A�@OA���A�U2A���A��:A��A��hA�A�8�A�+6A���A�^�A�A��+A��aA��^A��tA��#A��A���A�$A�l�A�c�A���A�ZQA�6FA��UA�ƨA���A�VmA�7A�@OA��[A�EA�hA��VA��A��4A��A���A���A��uA�3�A���A��A���A�l�A�A�(�A�AA}�Ay�Au�XAo�pAh�?Ae7LAc��AbSA\!AUuAQMAM��AI�AF��AC4A@DgA?�A=��A<XA;�|A=A;y>A8/�A6RTA5��A4�8A2�1A4v�A5H�A2�A.6zA,��A,��A.GEA.@A.K^A-�]A-�'A-M�A+�A%�A%�A$�A"�vA �wA1�A��A�	A�A�AK^AOvA.�A^5A�2AK^Aq�AoiA?}AsAخA/�AMA%�A"�Al�A�'A��A��A˒An/A&�A�bAo�Aa�A�rA|�Al�AOA%�AA�A|�A��A� AcAL0A��A��A�MA\)A!�A|A�A�A�A��A^�Az�A^5AA� A33AݘA҉A�vA�KA�rA�^Ax�A
�sA
2�A	�A	�TA	��A	QA	OA�0A%FAqvA �A�A�FA0�A�KAA�hA�*A��A��A`�AK^A��A]dA m�A 4@��'@��+@��:@�@O@���@���@��+@�YK@���@��@���@�,�@�Q�@�S@�6�@���@���@�Ov@�@�rG@��4@�2a@�Q@��@���@�K^@�@��@�͟@�9X@��a@�{J@� i@�oi@�q@�Ov@��m@��@ꀝ@� i@�@�Z�@��v@��@�H�@��
@�P@�_@�Vm@�@�@��@�(�@�z@�خ@��2@�`B@܆Y@��)@��@ڌ@�z@��@ً�@��@،@�?�@���@ףn@��@�6@ռ�@ԃ@��@�s�@�ی@�L0@��}@�Z�@��@�p;@��>@ϖS@ρ@�0�@��@�{�@�:�@��@͸�@�p�@�&�@��|@̚@�?�@��@�a�@���@ʗ�@�e�@�u�@��"@Ȯ}@�D�@Ǫ�@�%F@�Q@��D@���@�a@��?@Ě�@ď\@�l�@��@ÍP@���@+@�Ft@�#:@��d@�Dg@��@��O@��@�
�@�x@��@���@�c�@�$�@�J@��)@��@��h@�0�@���@�%�@�X@�#�@���@�r�@��@�Vm@��@�Z@�@�e�@��8@�Ov@��@��4@�7L@���@�Q@��@�g�@��@���@��}@�L0@���@�X@��v@���@���@�%�@�e�@��@���@�M�@���@�8�@��v@��9@��-@�4�@�[�@�@���@��
@��[@���@�?}@���@�|�@�@�@��@���@��@��~@��@�u%@�B[@�-@��9@�*0@��@���@��@�M�@���@�$t@���@�Ta@��*@�X@���@��I@���@�>B@��@��'@��"@�x�@�b�@�Dg@�!�@�V@��P@��v@���@���@��@�,=@�P�@���@�l"@��@��g@���@�x@�[W@���@���@�bN@�.�@���@��^@���@�U�@��2@���@�z�@�=q@�_@���@��@��@���@�@��@�L�@���@��}@�oi@�i�@�C�@��@��6@���@�&@���@��@��@@�b�@�͟@�l"@��
@���@��:@��@��B@���@�B[@�@��>@��^@��K@��@��
@���@���@�ԕ@��H@���@�c�@�T�@�q@���@�_�@�_@���@�c@�]�@�]�@�b�@�c�@�b�@�e,@�X@�.I@���@���@���@�V�@��Q@���@��$@�qv@�[W@�=�@�"�@�S@���@�kQ@�4@��K@��-@�p�@�*0@���@�ی@��@���@��O@�l"@�8�@�	@�@�ݘ@���@��@��@�]�@��@��@���@�c�@�@�@�"h@��@�J@���@���@�O�@�F�@�G�@�@O@��@���@�Q�@��@�G@�}@�f@=@~�c@~�R@~i�@}�.@}�@}\�@}0�@|Ɇ@|��@|�z@|�z@|w�@|PH@|1'@{�@{\)@zi�@y�M@x��@wg�@v��@vZ�@v	@u�#@uQ�@t��@tG@t	�@s�4@sS�@s@r��@r0U@q��@p�@py>@o�K@n�"@nn�@m�-@l�9@l@k��@kC@j��@j�+@j@iIR@hFt@gخ@g�@giD@g"�@gS@fE�@ex�@e	l@d��@db@cg�@cJ#@c.I@b�@bh
@a��@a��@a�@a�@a+�@a%@`��@`9X@_�]@_�@_��@_��@_s@_�@^�\@^W�@^)�@]�Z@]�N@]��@]zx@]J�@] \@\�E@\�@\y>@\1'@[�+@[��@[o�@Z��@Z�,@Z�B@Z��@Z��@ZC�@Z@�@Z;�@Y��@Ym]@Y`B@Y5�@XI�@W�@W�$@W�@V��@V͟@V�b@U�Z@U��@U<6@T��@Tz�@T?�@S��@S�$@S8@S�@R�R@R��@R?@Q�^@Qw2@Q�@P��@Pr�@O��@O��@Ox@O9�@O�@N�h@NTa@M�@M�@M�C@M�h@M\�@M�@L��@K�+@K\)@J��@J{�@J=q@J0U@J4@I�Z@I�Z@I�@I��@I��@H��@H��@H�u@HFt@H  @G��@G�A@G��@G��@G�k@G�4@GA�@F�"@F�@F�@F�}@F?@F	@E�@E�~@E@@D�[@Dw�@D�@C�@C4�@B�X@B�\@Bz@A��@A7L@@֡@@��@@1'@?��@?�q@?��@?�P@?l�@?@>��@>Z�@>4@=�Z@=zx@<�@<��@<Xy@<'R@<�@;�]@;�[@;�@;@:��@:)�@9�M@9IR@8�$@7��@7�V@7~�@7>�@7o@7�@6�"@6�c@6҉@6n�@6&�@5��@5��@5A @4�K@4�9@4�@4��@4q@4�@3�@@3=@2�]@2��@2q�@2)�@1��@1�C@1m]@1c�@1^�@1Vm@1�@0��@0e�@0"h@0�@/�@/j�@/F�@/�@.�m@.�A@.H�@-�"@,�@,�e@,u�@,[�@,6@,G@+��@+��@+RT@*�@*�@*��@*n�@*3�@)ԕ@)w2@)�@(�U@(�Y@'�]@'�	@'33@&�<@&GE@%�@%�~@%c�@%+@$֡@$�?@$�e@$�@#�@#�@#�k@#g�@#RT@#+@"�8@"~�@"�@!ϫ@!��@!��@!O�@!@ ��@ �@�0@]�@��@h
@($@�D@��@��@�'@�@~(@~@�@�$@x@a@K�@&@@�R@-@�@�h@�"@�~@�@m]@+@��@�u@g8@b@�6@�*@t�@b�@@��@��@�F@��@��@u%@u@��@��@�@�@B�@@�	@��@��@��@��@�@]d@Q�@�@�&@�F@Z�@,�@C@��@�@��@u%@0U@{@�n@Vm@+@��@��@��@oi@D�@�@�r@��@9�@�8@{�@c @d�@i�@l�@h
@.�@�@�t@��@o @T�@(�@�	@��@��@_@"h@�@��@�6@��@W?@�@Y@�@
�M@
ߤ@
�s@
��@
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�33A��A��A�SA���A̫A̓A�33Aˠ�A�8�A��A���A���A��>A���Aʫ6A�I�A�~�AȔ�A�oA��pAǧRA�c�A�!�A��A���AƗ$A�v�A�c A�bA��A��|A��jA���A��AÏ�A�Z�A���A�A��A��A���A�'�A�m)A�@OA���A�U2A���A��:A��A��hA�A�8�A�+6A���A�^�A�A��+A��aA��^A��tA��#A��A���A�$A�l�A�c�A���A�ZQA�6FA��UA�ƨA���A�VmA�7A�@OA��[A�EA�hA��VA��A��4A��A���A���A��uA�3�A���A��A���A�l�A�A�(�A�AA}�Ay�Au�XAo�pAh�?Ae7LAc��AbSA\!AUuAQMAM��AI�AF��AC4A@DgA?�A=��A<XA;�|A=A;y>A8/�A6RTA5��A4�8A2�1A4v�A5H�A2�A.6zA,��A,��A.GEA.@A.K^A-�]A-�'A-M�A+�A%�A%�A$�A"�vA �wA1�A��A�	A�A�AK^AOvA.�A^5A�2AK^Aq�AoiA?}AsAخA/�AMA%�A"�Al�A�'A��A��A˒An/A&�A�bAo�Aa�A�rA|�Al�AOA%�AA�A|�A��A� AcAL0A��A��A�MA\)A!�A|A�A�A�A��A^�Az�A^5AA� A33AݘA҉A�vA�KA�rA�^Ax�A
�sA
2�A	�A	�TA	��A	QA	OA�0A%FAqvA �A�A�FA0�A�KAA�hA�*A��A��A`�AK^A��A]dA m�A 4@��'@��+@��:@�@O@���@���@��+@�YK@���@��@���@�,�@�Q�@�S@�6�@���@���@�Ov@�@�rG@��4@�2a@�Q@��@���@�K^@�@��@�͟@�9X@��a@�{J@� i@�oi@�q@�Ov@��m@��@ꀝ@� i@�@�Z�@��v@��@�H�@��
@�P@�_@�Vm@�@�@��@�(�@�z@�خ@��2@�`B@܆Y@��)@��@ڌ@�z@��@ً�@��@،@�?�@���@ףn@��@�6@ռ�@ԃ@��@�s�@�ی@�L0@��}@�Z�@��@�p;@��>@ϖS@ρ@�0�@��@�{�@�:�@��@͸�@�p�@�&�@��|@̚@�?�@��@�a�@���@ʗ�@�e�@�u�@��"@Ȯ}@�D�@Ǫ�@�%F@�Q@��D@���@�a@��?@Ě�@ď\@�l�@��@ÍP@���@+@�Ft@�#:@��d@�Dg@��@��O@��@�
�@�x@��@���@�c�@�$�@�J@��)@��@��h@�0�@���@�%�@�X@�#�@���@�r�@��@�Vm@��@�Z@�@�e�@��8@�Ov@��@��4@�7L@���@�Q@��@�g�@��@���@��}@�L0@���@�X@��v@���@���@�%�@�e�@��@���@�M�@���@�8�@��v@��9@��-@�4�@�[�@�@���@��
@��[@���@�?}@���@�|�@�@�@��@���@��@��~@��@�u%@�B[@�-@��9@�*0@��@���@��@�M�@���@�$t@���@�Ta@��*@�X@���@��I@���@�>B@��@��'@��"@�x�@�b�@�Dg@�!�@�V@��P@��v@���@���@��@�,=@�P�@���@�l"@��@��g@���@�x@�[W@���@���@�bN@�.�@���@��^@���@�U�@��2@���@�z�@�=q@�_@���@��@��@���@�@��@�L�@���@��}@�oi@�i�@�C�@��@��6@���@�&@���@��@��@@�b�@�͟@�l"@��
@���@��:@��@��B@���@�B[@�@��>@��^@��K@��@��
@���@���@�ԕ@��H@���@�c�@�T�@�q@���@�_�@�_@���@�c@�]�@�]�@�b�@�c�@�b�@�e,@�X@�.I@���@���@���@�V�@��Q@���@��$@�qv@�[W@�=�@�"�@�S@���@�kQ@�4@��K@��-@�p�@�*0@���@�ی@��@���@��O@�l"@�8�@�	@�@�ݘ@���@��@��@�]�@��@��@���@�c�@�@�@�"h@��@�J@���@���@�O�@�F�@�G�@�@O@��@���@�Q�@��@�G@�}@�f@=@~�c@~�R@~i�@}�.@}�@}\�@}0�@|Ɇ@|��@|�z@|�z@|w�@|PH@|1'@{�@{\)@zi�@y�M@x��@wg�@v��@vZ�@v	@u�#@uQ�@t��@tG@t	�@s�4@sS�@s@r��@r0U@q��@p�@py>@o�K@n�"@nn�@m�-@l�9@l@k��@kC@j��@j�+@j@iIR@hFt@gخ@g�@giD@g"�@gS@fE�@ex�@e	l@d��@db@cg�@cJ#@c.I@b�@bh
@a��@a��@a�@a�@a+�@a%@`��@`9X@_�]@_�@_��@_��@_s@_�@^�\@^W�@^)�@]�Z@]�N@]��@]zx@]J�@] \@\�E@\�@\y>@\1'@[�+@[��@[o�@Z��@Z�,@Z�B@Z��@Z��@ZC�@Z@�@Z;�@Y��@Ym]@Y`B@Y5�@XI�@W�@W�$@W�@V��@V͟@V�b@U�Z@U��@U<6@T��@Tz�@T?�@S��@S�$@S8@S�@R�R@R��@R?@Q�^@Qw2@Q�@P��@Pr�@O��@O��@Ox@O9�@O�@N�h@NTa@M�@M�@M�C@M�h@M\�@M�@L��@K�+@K\)@J��@J{�@J=q@J0U@J4@I�Z@I�Z@I�@I��@I��@H��@H��@H�u@HFt@H  @G��@G�A@G��@G��@G�k@G�4@GA�@F�"@F�@F�@F�}@F?@F	@E�@E�~@E@@D�[@Dw�@D�@C�@C4�@B�X@B�\@Bz@A��@A7L@@֡@@��@@1'@?��@?�q@?��@?�P@?l�@?@>��@>Z�@>4@=�Z@=zx@<�@<��@<Xy@<'R@<�@;�]@;�[@;�@;@:��@:)�@9�M@9IR@8�$@7��@7�V@7~�@7>�@7o@7�@6�"@6�c@6҉@6n�@6&�@5��@5��@5A @4�K@4�9@4�@4��@4q@4�@3�@@3=@2�]@2��@2q�@2)�@1��@1�C@1m]@1c�@1^�@1Vm@1�@0��@0e�@0"h@0�@/�@/j�@/F�@/�@.�m@.�A@.H�@-�"@,�@,�e@,u�@,[�@,6@,G@+��@+��@+RT@*�@*�@*��@*n�@*3�@)ԕ@)w2@)�@(�U@(�Y@'�]@'�	@'33@&�<@&GE@%�@%�~@%c�@%+@$֡@$�?@$�e@$�@#�@#�@#�k@#g�@#RT@#+@"�8@"~�@"�@!ϫ@!��@!��@!O�@!@ ��@ �@�0@]�@��@h
@($@�D@��@��@�'@�@~(@~@�@�$@x@a@K�@&@@�R@-@�@�h@�"@�~@�@m]@+@��@�u@g8@b@�6@�*@t�@b�@@��@��@�F@��@��@u%@u@��@��@�@�@B�@@�	@��@��@��@��@�@]d@Q�@�@�&@�F@Z�@,�@C@��@�@��@u%@0U@{@�n@Vm@+@��@��@��@oi@D�@�@�r@��@9�@�8@{�@c @d�@i�@l�@h
@.�@�@�t@��@o @T�@(�@�	@��@��@_@"h@�@��@�6@��@W?@�@Y@�@
�M@
ߤ@
�s@
��@
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�[B�AB��B�UB�B�B�hB�$B	�B	/�B	R B	\�B	\�B	]IB	aB	g�B	o�B	�B	��B	�RB	�B	�B	�\B	ںB	�9B
�B
�B
&2B
AoB
X_B
�B
��B
�^B
ҽB�BBRoB^jB[	BW�BS�BP.BU�Bz�B�0B�B��B��B�aB�B�%B�B�BQB vB�B�BB�B1B�B��BؓB��B�wB��B�2Bn�B9	B#B;B�BB�BB{B
��B
��B
�B
�@B
οB
�sB
��B
�OB
�yB
��B
�`B
�1B
y�B
Q B
3�B
!HB
B
B	��B	��B	�jB	��B	_�B	C{B	5tB	-�B	�B��B�	B�vB�=B��B�B�}B��B��B��B��B	&fB	=�B	"�B	�B		lB	B��B	9�B	ZB	?cB	qB	�B	�B	@�B	KB	R�B	\�B	m�B	�OB	v�B	>�B	6zB	2�B	/B	
=B	4B	%,B	)yB	2|B	8�B	>�B	@ B	B'B	KDB	[	B	��B	��B	��B	��B	w�B	ezB	a�B	_!B	_B	_�B	h$B	p�B	��B	�jB	�jB	��B	��B	�~B	�jB	��B	�!B	�B	�-B	�aB	��B	�3B	�hB	��B	�B	�$B	�^B	��B	��B	��B	��B	�'B	��B	�B	�RB	�B	�'B	��B	�}B	�;B	�tB	�B	�oB	��B	��B	�9B	�`B	��B	�$B	��B	��B	�RB	��B	�B	�hB	�MB	�MB	��B	�hB	�aB	�'B	��B	��B	��B	��B	�9B	��B	�yB	��B	��B	��B	�jB	��B	��B	��B	�B	�B	� B	�B	��B	�iB	ªB	�{B	�zB	��B	��B	��B	׍B	��B	��B	��B	�LB	�sB	�_B	�_B	��B	�B	��B	�mB	��B	�B	�B	��B	�NB	�-B	��B	�VB	�;B	ߊB	�\B	�nB	��B	�`B	�zB	�@B	�B	�B	�vB	��B	��B	ߤB	�pB	�B	�B	ܬB	�_B	�_B	چB	�+B	��B	�+B	��B	�aB	��B	ҽB	ңB	�oB	�B	�[B	��B	өB	�aB	�2B	�B	�SB	��B	�mB	�B	��B	��B	�]B	�IB	��B	�~B	�B	�]B	��B	�B	�5B	�B	ބB	��B	�pB	߾B	�B	��B	�bB	�NB	�B	�B	��B	�B	�B	�B	�LB	�LB	��B	�B	�B	�B	��B	��B	�RB	�B	��B	�B	�RB	�8B	�B	�B	�RB	�
B	�B	��B	�B	��B	�yB	��B	�eB	��B	�B	�kB	�B	�WB	�WB	�qB	�qB	�WB	�WB	�WB	��B	�)B	�CB	��B	�B	�B	��B	�)B	��B	�B	�B	�B	�B	��B	��B	�B	��B	��B	�B	�AB	�[B	��B	��B	��B	��B	�B	�hB	�TB	�TB	�9B	��B	�B	��B	�B	�?B	��B	�?B	�B	��B	�tB	�tB	�+B	��B	��B	�$B	��B	��B	��B	��B	��B	�6B	�PB	�B	�B	��B	�B	��B	�dB	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�BB	�B	��B
 �B
 �B
�B
�B
�B
�B
-B
aB
�B
�B
�B
�B
�B
�B
�B
B
B
�B
aB
mB
%B
�B
B
+B
B
B
zB
zB
�B
	B
	�B
	�B

=B

=B

�B

�B
�B
�B
dB
�B
�B
BB
vB
vB
bB
�B
B
�B
�B
hB
�B
�B
�B
9B
�B
�B
sB
�B
�B
�B
�B
aB
�B
B
B
9B
B
�B
�B
$B
_B
�B
�B
�B
eB
QB
�B
B
�B
dB
dB
IB
/B
�B
~B
/B
�B
�B
�B
�B
;B
�B
 B
 'B
 \B
 �B
 �B
!HB
!bB
!-B
!�B
"B
"4B
"NB
"hB
"NB
"NB
"NB
"4B
"hB
"�B
#:B
#TB
#:B
#�B
$&B
$�B
$�B
$�B
$�B
$�B
%�B
&2B
&�B
'B
'�B
'�B
'�B
'�B
(�B
)DB
)DB
*0B
*eB
*�B
*�B
*�B
*�B
*�B
+�B
,B
,=B
+�B
+�B
,WB
-)B
-�B
-�B
-�B
.B
.B
.}B
.�B
.�B
/B
/OB
/iB
/�B
/�B
0�B
0�B
0oB
0UB
0�B
0�B
0�B
0�B
0�B
1�B
2GB
2�B
2�B
2�B
2�B
3MB
3MB
3MB
3�B
4�B
5�B
6�B
6�B
7B
7�B
7�B
88B
9�B
9�B
:*B
:xB
;B
;B
:�B
;B
;B
:�B
;JB
;B
;dB
;�B
<B
<jB
<�B
<�B
="B
="B
=�B
>B
=�B
=�B
=VB
<�B
<�B
<�B
<�B
=<B
?HB
?�B
@4B
@�B
A�B
A�B
A�B
A�B
A�B
B'B
B�B
B�B
BuB
BuB
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
CGB
DB
D3B
D�B
D�B
EB
E�B
E�B
F%B
F%B
F?B
F?B
FtB
F�B
FtB
G+B
F�B
F�B
F�B
G�B
G�B
HB
HfB
HfB
HfB
HKB
IB
I7B
IlB
I�B
I�B
I�B
J#B
JrB
J�B
J�B
K^B
K)B
K�B
L0B
L0B
L�B
LdB
L~B
LJB
L0B
LB
L0B
L~B
LdB
LB
K�B
KxB
KxB
K�B
LdB
L0B
L�B
L�B
L�B
LdB
L�B
MB
L�B
M6B
MB
MB
M6B
M6B
M6B
M�B
M�B
NB
NpB
N�B
N�B
N�B
N�B
N�B
N�B
OB
OB
OB
OB
O\B
PB
PHB
P�B
Q4B
Q4B
Q�B
QhB
Q�B
R:B
R�B
R�B
R�B
R�B
SB
S�B
T,B
S�B
S�B
S[B
S�B
TB
T,B
TFB
TFB
T�B
T�B
UB
T�B
T�B
UMB
U�B
U�B
VB
V9B
VB
V9B
VmB
W
B
W$B
W
B
W�B
X+B
XEB
X�B
YeB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z7B
Z�B
Z�B
Z�B
[	B
[WB
[qB
[�B
[qB
[�B
[�B
\)B
\xB
\�B
\�B
]B
]IB
]�B
]�B
^B
^B
^B
]�B
^B
^�B
^�B
_B
^�B
_;B
_pB
_�B
_�B
_�B
_�B
`B
`�B
aHB
a|B
a�B
a�B
a�B
a�B
b4B
bNB
bhB
b�B
b�B
b�B
b�B
c B
cnB
c�B
dB
d&B
d&B
d�B
d�B
d�B
eFB
e`B
e�B
e�B
fLB
f�B
f�B
f�B
f�B
f�B
f�B
gB
gB
g8B
g8B
gRB
g�B
g�B
h
B
hsB
h�B
h�B
h�B
h�B
i*B
i�B
i�B
jB
jeB
j�B
kB
kB
kQB
k6B
k6B
k�B
l=B
l�B
l�B
mB
m)B
mCB
mCB
m�B
m�B
m�B
m�B
m�B
n}B
n�B
n�B
n�B
n�B
o5B
oOB
o5B
oiB
o�B
o�B
o�B
p;B
p!B
p�B
q'B
q'B
q'B
q'B
q'B
q'B
q[B
qvB
qvB
q�B
q�B
r-B
r|B
r�B
s3B
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tTB
t�B
u%B
u?B
uZB
utB
u�B
u�B
u�B
u�B
v`B
vFB
vzB
v�B
v�B
v�B
wB
wB
wB
v�B
wB
wLB
wfB
xB
xB
xB
xB
xB
xB
x�B
x�B
y	B
y$B
y>B
yXB
y�B
y�B
y�B
y�B
zDB
z^B
z^B
z�B
z�B
z�B
{JB
{B
{B
{B
{�B
{�B
{�B
{�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�[B�AB��B�UB�B�B�hB�$B	�B	/�B	R B	\�B	\�B	]IB	aB	g�B	o�B	�B	��B	�RB	�B	�B	�\B	ںB	�9B
�B
�B
&2B
AoB
X_B
�B
��B
�^B
ҽB�BBRoB^jB[	BW�BS�BP.BU�Bz�B�0B�B��B��B�aB�B�%B�B�BQB vB�B�BB�B1B�B��BؓB��B�wB��B�2Bn�B9	B#B;B�BB�BB{B
��B
��B
�B
�@B
οB
�sB
��B
�OB
�yB
��B
�`B
�1B
y�B
Q B
3�B
!HB
B
B	��B	��B	�jB	��B	_�B	C{B	5tB	-�B	�B��B�	B�vB�=B��B�B�}B��B��B��B��B	&fB	=�B	"�B	�B		lB	B��B	9�B	ZB	?cB	qB	�B	�B	@�B	KB	R�B	\�B	m�B	�OB	v�B	>�B	6zB	2�B	/B	
=B	4B	%,B	)yB	2|B	8�B	>�B	@ B	B'B	KDB	[	B	��B	��B	��B	��B	w�B	ezB	a�B	_!B	_B	_�B	h$B	p�B	��B	�jB	�jB	��B	��B	�~B	�jB	��B	�!B	�B	�-B	�aB	��B	�3B	�hB	��B	�B	�$B	�^B	��B	��B	��B	��B	�'B	��B	�B	�RB	�B	�'B	��B	�}B	�;B	�tB	�B	�oB	��B	��B	�9B	�`B	��B	�$B	��B	��B	�RB	��B	�B	�hB	�MB	�MB	��B	�hB	�aB	�'B	��B	��B	��B	��B	�9B	��B	�yB	��B	��B	��B	�jB	��B	��B	��B	�B	�B	� B	�B	��B	�iB	ªB	�{B	�zB	��B	��B	��B	׍B	��B	��B	��B	�LB	�sB	�_B	�_B	��B	�B	��B	�mB	��B	�B	�B	��B	�NB	�-B	��B	�VB	�;B	ߊB	�\B	�nB	��B	�`B	�zB	�@B	�B	�B	�vB	��B	��B	ߤB	�pB	�B	�B	ܬB	�_B	�_B	چB	�+B	��B	�+B	��B	�aB	��B	ҽB	ңB	�oB	�B	�[B	��B	өB	�aB	�2B	�B	�SB	��B	�mB	�B	��B	��B	�]B	�IB	��B	�~B	�B	�]B	��B	�B	�5B	�B	ބB	��B	�pB	߾B	�B	��B	�bB	�NB	�B	�B	��B	�B	�B	�B	�LB	�LB	��B	�B	�B	�B	��B	��B	�RB	�B	��B	�B	�RB	�8B	�B	�B	�RB	�
B	�B	��B	�B	��B	�yB	��B	�eB	��B	�B	�kB	�B	�WB	�WB	�qB	�qB	�WB	�WB	�WB	��B	�)B	�CB	��B	�B	�B	��B	�)B	��B	�B	�B	�B	�B	��B	��B	�B	��B	��B	�B	�AB	�[B	��B	��B	��B	��B	�B	�hB	�TB	�TB	�9B	��B	�B	��B	�B	�?B	��B	�?B	�B	��B	�tB	�tB	�+B	��B	��B	�$B	��B	��B	��B	��B	��B	�6B	�PB	�B	�B	��B	�B	��B	�dB	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�BB	�B	��B
 �B
 �B
�B
�B
�B
�B
-B
aB
�B
�B
�B
�B
�B
�B
�B
B
B
�B
aB
mB
%B
�B
B
+B
B
B
zB
zB
�B
	B
	�B
	�B

=B

=B

�B

�B
�B
�B
dB
�B
�B
BB
vB
vB
bB
�B
B
�B
�B
hB
�B
�B
�B
9B
�B
�B
sB
�B
�B
�B
�B
aB
�B
B
B
9B
B
�B
�B
$B
_B
�B
�B
�B
eB
QB
�B
B
�B
dB
dB
IB
/B
�B
~B
/B
�B
�B
�B
�B
;B
�B
 B
 'B
 \B
 �B
 �B
!HB
!bB
!-B
!�B
"B
"4B
"NB
"hB
"NB
"NB
"NB
"4B
"hB
"�B
#:B
#TB
#:B
#�B
$&B
$�B
$�B
$�B
$�B
$�B
%�B
&2B
&�B
'B
'�B
'�B
'�B
'�B
(�B
)DB
)DB
*0B
*eB
*�B
*�B
*�B
*�B
*�B
+�B
,B
,=B
+�B
+�B
,WB
-)B
-�B
-�B
-�B
.B
.B
.}B
.�B
.�B
/B
/OB
/iB
/�B
/�B
0�B
0�B
0oB
0UB
0�B
0�B
0�B
0�B
0�B
1�B
2GB
2�B
2�B
2�B
2�B
3MB
3MB
3MB
3�B
4�B
5�B
6�B
6�B
7B
7�B
7�B
88B
9�B
9�B
:*B
:xB
;B
;B
:�B
;B
;B
:�B
;JB
;B
;dB
;�B
<B
<jB
<�B
<�B
="B
="B
=�B
>B
=�B
=�B
=VB
<�B
<�B
<�B
<�B
=<B
?HB
?�B
@4B
@�B
A�B
A�B
A�B
A�B
A�B
B'B
B�B
B�B
BuB
BuB
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
CGB
DB
D3B
D�B
D�B
EB
E�B
E�B
F%B
F%B
F?B
F?B
FtB
F�B
FtB
G+B
F�B
F�B
F�B
G�B
G�B
HB
HfB
HfB
HfB
HKB
IB
I7B
IlB
I�B
I�B
I�B
J#B
JrB
J�B
J�B
K^B
K)B
K�B
L0B
L0B
L�B
LdB
L~B
LJB
L0B
LB
L0B
L~B
LdB
LB
K�B
KxB
KxB
K�B
LdB
L0B
L�B
L�B
L�B
LdB
L�B
MB
L�B
M6B
MB
MB
M6B
M6B
M6B
M�B
M�B
NB
NpB
N�B
N�B
N�B
N�B
N�B
N�B
OB
OB
OB
OB
O\B
PB
PHB
P�B
Q4B
Q4B
Q�B
QhB
Q�B
R:B
R�B
R�B
R�B
R�B
SB
S�B
T,B
S�B
S�B
S[B
S�B
TB
T,B
TFB
TFB
T�B
T�B
UB
T�B
T�B
UMB
U�B
U�B
VB
V9B
VB
V9B
VmB
W
B
W$B
W
B
W�B
X+B
XEB
X�B
YeB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z7B
Z�B
Z�B
Z�B
[	B
[WB
[qB
[�B
[qB
[�B
[�B
\)B
\xB
\�B
\�B
]B
]IB
]�B
]�B
^B
^B
^B
]�B
^B
^�B
^�B
_B
^�B
_;B
_pB
_�B
_�B
_�B
_�B
`B
`�B
aHB
a|B
a�B
a�B
a�B
a�B
b4B
bNB
bhB
b�B
b�B
b�B
b�B
c B
cnB
c�B
dB
d&B
d&B
d�B
d�B
d�B
eFB
e`B
e�B
e�B
fLB
f�B
f�B
f�B
f�B
f�B
f�B
gB
gB
g8B
g8B
gRB
g�B
g�B
h
B
hsB
h�B
h�B
h�B
h�B
i*B
i�B
i�B
jB
jeB
j�B
kB
kB
kQB
k6B
k6B
k�B
l=B
l�B
l�B
mB
m)B
mCB
mCB
m�B
m�B
m�B
m�B
m�B
n}B
n�B
n�B
n�B
n�B
o5B
oOB
o5B
oiB
o�B
o�B
o�B
p;B
p!B
p�B
q'B
q'B
q'B
q'B
q'B
q'B
q[B
qvB
qvB
q�B
q�B
r-B
r|B
r�B
s3B
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tTB
t�B
u%B
u?B
uZB
utB
u�B
u�B
u�B
u�B
v`B
vFB
vzB
v�B
v�B
v�B
wB
wB
wB
v�B
wB
wLB
wfB
xB
xB
xB
xB
xB
xB
x�B
x�B
y	B
y$B
y>B
yXB
y�B
y�B
y�B
y�B
zDB
z^B
z^B
z�B
z�B
z�B
{JB
{B
{B
{B
{�B
{�B
{�B
{�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105227  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191141  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191141  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191141                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041149  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041149  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                