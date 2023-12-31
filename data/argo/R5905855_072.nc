CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:23:22Z creation;2022-06-04T19:23:22Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192322  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               HA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�Zk=�/h1   @�Zk�+<M@-r-V�c�9XbN1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A!��A@  A`  A�  A�  A�  A�33A�33A�  A�  A�  B   BffB  B  B ffB(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B���B���B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ Dܼ�D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@\)@��@��A!p�A?�
A_�
A�
A��A��A��A��A��A��A��A��B\)B��B��B \)B'��B/��B7��B?�]BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B�.B�.B���B���B���B���B���B��{B�ǮB���B��{B���B�ǮB���B���B���B���B���B���B���B���B��{B�ǮB�ǮB���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qCC C!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCTCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D!�D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D�|{D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܼ{D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�B�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D���D���D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��iA���A���A��(A��(A��(A��(A��.A� �A��A��A�uA��A�MA��A��A��A�SA��A�%A��A��A�
rA��A�:A��A��A��A�@A�hA��A��A�>�A�PA��OA��ZA���A��;A�?A�2aA���A�y>A�}�A�g�A���A���A��vA��VA�s�A���A��A��A�ŢA��qA�A��A�p�A��A��A���A�3�A��QA�o�A�u�A�U�A�\A��A�g�A�Q�A�҉A�S[A�{JA�ӏA�A���A�q�A�:A��BA�ɺA{��At�aAr_Ao#�Ak'�AgE9Ad@A`��AXL0ATh�AQ�UAM�AK�"AJy�AI�AIF�AG5�AED�AC�A?1'A=�jA5� A2�A/Q�A.|A-��A+|A)�A(��A(�CA'VA&>BA&+kA&rGA&|A&YKA%�8A%��A%��A%ffA$�rA$QA$a�A$>BA#یA"�A"j�A��A	A1'A-�A+AL�A�PA�A�A=qAL0A��A{AYA�xAg8A+A�A[�A�.A�AqA�A�MA��A�A��A�A��A�A]�A� A��AbNA
ȴA
�A	u�A��A:�A_A�A��A��A5?A�A�A�*AqAZ�AݘA��AbNA_A?}A�DA��A\�At�A��AVmAA ��A w�A <�@��P@�Ɇ@�	�@��@�"�@���@�Ta@��E@�I�@��@�hs@�8@��z@���@���@� �@��0@��@�M�@���@���@�	@��@�0�@�#:@��@�7@��)@��@�@��@�q@���@��#@�!-@�xl@�k�@�oi@��z@�N<@��@���@�@�w2@�F@�%�@��+@���@�o @�Ĝ@�6�@�=@��M@�*�@��@��@�\)@�S&@�=�@��B@�}V@�	�@�:�@ރ�@݋�@�*0@��@�@��E@ܶ�@ܑ�@��@۞�@�9�@���@�4n@�� @ٌ~@�4�@ئL@��K@��@և+@��@�x@���@�%�@��@�\�@�/�@��c@��)@Ң4@��@ќ@�w2@�L�@��@п�@А.@�YK@���@Ϝ�@�a@�~�@�Z�@��@�a|@�t�@��@��@ʞ@�m�@��N@Ƀ{@�=@���@ț�@�u@ǥ�@�4@ƃ�@�&�@�hs@�A�@��@�S@��@��B@�D�@��@�e�@�_@��H@�{J@�Vm@�$t@��@��<@�ff@���@�!-@���@��'@���@�[�@� �@��7@��6@��@��@�{J@���@��/@�6�@��@���@�j�@��@��@���@���@�#�@�V@��@���@��+@�Z@�D�@�4n@�@��@���@�~�@���@���@��4@�	l@�xl@��&@�[W@�'�@��9@�3�@�4@�=q@�@�@�2�@���@�*0@���@�ȴ@��o@�#:@���@�N<@��@��o@�p;@�{@��'@���@��,@���@��I@�~(@��@���@�u�@�+�@�(@���@��+@�_@�ƨ@���@�F�@�%@�ȴ@��m@��!@�PH@���@���@�J�@���@�:�@�ԕ@���@�X�@�ȴ@�H@��@�ƨ@�p�@���@���@�L0@�|@��@�L0@�1@��T@��3@�k�@��@��@��U@���@�W�@��>@���@�`B@�IR@�9�@�@�w�@�"h@���@���@���@�O@�)_@�@���@�l"@��o@���@�-w@���@��s@���@�,=@��@�ݘ@��3@��4@�G�@�o@�֡@���@��.@�l�@�J�@�1�@��Z@��K@�a@�@@���@�Z�@���@�J#@�C@��|@�ی@���@�kQ@�;�@�{@��A@��@��c@��O@���@�I�@��^@��V@��P@��M@�}V@�@�@�{@��T@��N@��q@�x@�>�@���@��z@�~�@�ff@�V@�7�@�e@��W@��"@�+@��@��s@�e�@���@��S@�l�@�Q�@�H�@�Dg@���@��9@�e�@�6�@���@���@�IR@��@��!@�~(@�Ta@�<�@���@�e,@�Y�@�O�@��@���@�a|@�$@��&@���@�W?@�1�@��p@��+@�	@���@���@��@�/�@��@���@�z�@�-�@�K@b�@J#@9�@~�b@~($@}ԕ@}�C@}��@}:�@|]d@|,=@{�K@z�@z��@z͟@{@z��@z{�@zZ�@y��@y`B@x�.@x@w1�@v��@u��@uk�@t��@t7@s�;@s�P@sY@rB[@q��@q��@q�@qx�@q�@p��@p?�@p"h@p!@p7@o˒@o�@n�!@n	@l��@l��@l/�@k�*@k�4@k@O@k4�@j�h@i�@i(�@hѷ@hS�@g�	@g1�@f�@f� @f6�@f	@e�@e�n@e!�@d�?@d��@dh�@cخ@b��@b�@a��@a�7@aL�@a/@`�@`��@`�O@`j@`�@_��@_F�@^�@^��@^�x@^��@]�D@]��@][W@]�@\w�@[��@[��@[g�@Z��@ZC�@Y�z@Y�S@Ys�@X�@X%�@W1�@Vں@V�@V��@V{�@U��@UX@T�O@T:�@S�@S�f@SRT@R�x@R=q@R{@Q�~@Q2a@P�9@P%�@P�@P  @O�r@O�@Oخ@O�k@N�,@NGE@N�@M��@M?}@L�U@Lg8@K\)@Jں@Jc @J@I@H�4@HG@G��@G�{@F�@F��@F�@FH�@F#:@Fu@E\�@E�@D�@D@C��@C��@Cqv@C;d@C
=@C�@C�@B��@B�H@B�@B	@A��@A8�@A�@@�@@y>@?��@?��@>��@>_�@=�@=��@=<6@<�f@<��@<�/@<�@<y>@<_@;� @;&@:��@:�b@:��@:J�@9��@9�"@9[W@9%@8�`@8m�@7�m@7�a@7�@7��@7\)@7e�@7l�@7v`@7>�@6�@6��@6�h@6($@5�@4��@4w�@4PH@4�@3��@3��@3C�@2�M@2��@2��@2s�@2V@2�@1�3@1�"@1-w@0��@0m�@0~@/�@/�g@/��@/�V@/�P@/g�@.�@.M�@.@-�@-zx@,�@,��@,V�@,7@+�]@+��@+�@*��@*{�@*q�@*h
@*M�@*#:@)�H@)��@)X@)8�@(�P@(ی@(��@(�?@(z�@(M@(%�@'�&@'��@'�f@'Mj@'8@'1�@')_@'&@'"�@'C@' i@&�+@&!�@%�3@%Dg@$��@$�5@$ی@$��@#�r@#�4@#.I@"�R@"kQ@"�@!��@!T�@!2a@!�@ ی@ ��@ �9@ ��@ �4@ �I@ ��@ �@ g8@ Z@ 'R@�P@H�@�@�h@[W@?}@*0@��@A�@4n@'R@� @��@b�@33@"�@@S@�@�@��@kQ@�@��@�z@��@k�@e,@N<@�	@�`@��@��@bN@'R@M@�]@�;@˒@��@�@��@y�@>�@�@�s@��@�x@v�@?@J@�@ϫ@�t@k�@5�@q@�@�/@Ĝ@��@��@V�@�&@��@l�@Mj@
=@�y@��@�b@�\@�+@J@ϫ@�h@Q�@V@�P@�@�@�v@�E@�p@�@��@bN@-�@�K@�f@P�@o@ں@�@��@i�@E�@E�@@�@5?@��@}�@o @ \@�E@�)@Ĝ@��@��@�m@��@s@H�@33@"�@@�@
�8@
�@
Q@
�@	�@	��@	�3@	�H@	�X@	T�@	%F@	@@	�@��@�@�@Ɇ@��@��@`�@�@�F@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��iA���A���A��(A��(A��(A��(A��.A� �A��A��A�uA��A�MA��A��A��A�SA��A�%A��A��A�
rA��A�:A��A��A��A�@A�hA��A��A�>�A�PA��OA��ZA���A��;A�?A�2aA���A�y>A�}�A�g�A���A���A��vA��VA�s�A���A��A��A�ŢA��qA�A��A�p�A��A��A���A�3�A��QA�o�A�u�A�U�A�\A��A�g�A�Q�A�҉A�S[A�{JA�ӏA�A���A�q�A�:A��BA�ɺA{��At�aAr_Ao#�Ak'�AgE9Ad@A`��AXL0ATh�AQ�UAM�AK�"AJy�AI�AIF�AG5�AED�AC�A?1'A=�jA5� A2�A/Q�A.|A-��A+|A)�A(��A(�CA'VA&>BA&+kA&rGA&|A&YKA%�8A%��A%��A%ffA$�rA$QA$a�A$>BA#یA"�A"j�A��A	A1'A-�A+AL�A�PA�A�A=qAL0A��A{AYA�xAg8A+A�A[�A�.A�AqA�A�MA��A�A��A�A��A�A]�A� A��AbNA
ȴA
�A	u�A��A:�A_A�A��A��A5?A�A�A�*AqAZ�AݘA��AbNA_A?}A�DA��A\�At�A��AVmAA ��A w�A <�@��P@�Ɇ@�	�@��@�"�@���@�Ta@��E@�I�@��@�hs@�8@��z@���@���@� �@��0@��@�M�@���@���@�	@��@�0�@�#:@��@�7@��)@��@�@��@�q@���@��#@�!-@�xl@�k�@�oi@��z@�N<@��@���@�@�w2@�F@�%�@��+@���@�o @�Ĝ@�6�@�=@��M@�*�@��@��@�\)@�S&@�=�@��B@�}V@�	�@�:�@ރ�@݋�@�*0@��@�@��E@ܶ�@ܑ�@��@۞�@�9�@���@�4n@�� @ٌ~@�4�@ئL@��K@��@և+@��@�x@���@�%�@��@�\�@�/�@��c@��)@Ң4@��@ќ@�w2@�L�@��@п�@А.@�YK@���@Ϝ�@�a@�~�@�Z�@��@�a|@�t�@��@��@ʞ@�m�@��N@Ƀ{@�=@���@ț�@�u@ǥ�@�4@ƃ�@�&�@�hs@�A�@��@�S@��@��B@�D�@��@�e�@�_@��H@�{J@�Vm@�$t@��@��<@�ff@���@�!-@���@��'@���@�[�@� �@��7@��6@��@��@�{J@���@��/@�6�@��@���@�j�@��@��@���@���@�#�@�V@��@���@��+@�Z@�D�@�4n@�@��@���@�~�@���@���@��4@�	l@�xl@��&@�[W@�'�@��9@�3�@�4@�=q@�@�@�2�@���@�*0@���@�ȴ@��o@�#:@���@�N<@��@��o@�p;@�{@��'@���@��,@���@��I@�~(@��@���@�u�@�+�@�(@���@��+@�_@�ƨ@���@�F�@�%@�ȴ@��m@��!@�PH@���@���@�J�@���@�:�@�ԕ@���@�X�@�ȴ@�H@��@�ƨ@�p�@���@���@�L0@�|@��@�L0@�1@��T@��3@�k�@��@��@��U@���@�W�@��>@���@�`B@�IR@�9�@�@�w�@�"h@���@���@���@�O@�)_@�@���@�l"@��o@���@�-w@���@��s@���@�,=@��@�ݘ@��3@��4@�G�@�o@�֡@���@��.@�l�@�J�@�1�@��Z@��K@�a@�@@���@�Z�@���@�J#@�C@��|@�ی@���@�kQ@�;�@�{@��A@��@��c@��O@���@�I�@��^@��V@��P@��M@�}V@�@�@�{@��T@��N@��q@�x@�>�@���@��z@�~�@�ff@�V@�7�@�e@��W@��"@�+@��@��s@�e�@���@��S@�l�@�Q�@�H�@�Dg@���@��9@�e�@�6�@���@���@�IR@��@��!@�~(@�Ta@�<�@���@�e,@�Y�@�O�@��@���@�a|@�$@��&@���@�W?@�1�@��p@��+@�	@���@���@��@�/�@��@���@�z�@�-�@�K@b�@J#@9�@~�b@~($@}ԕ@}�C@}��@}:�@|]d@|,=@{�K@z�@z��@z͟@{@z��@z{�@zZ�@y��@y`B@x�.@x@w1�@v��@u��@uk�@t��@t7@s�;@s�P@sY@rB[@q��@q��@q�@qx�@q�@p��@p?�@p"h@p!@p7@o˒@o�@n�!@n	@l��@l��@l/�@k�*@k�4@k@O@k4�@j�h@i�@i(�@hѷ@hS�@g�	@g1�@f�@f� @f6�@f	@e�@e�n@e!�@d�?@d��@dh�@cخ@b��@b�@a��@a�7@aL�@a/@`�@`��@`�O@`j@`�@_��@_F�@^�@^��@^�x@^��@]�D@]��@][W@]�@\w�@[��@[��@[g�@Z��@ZC�@Y�z@Y�S@Ys�@X�@X%�@W1�@Vں@V�@V��@V{�@U��@UX@T�O@T:�@S�@S�f@SRT@R�x@R=q@R{@Q�~@Q2a@P�9@P%�@P�@P  @O�r@O�@Oخ@O�k@N�,@NGE@N�@M��@M?}@L�U@Lg8@K\)@Jں@Jc @J@I@H�4@HG@G��@G�{@F�@F��@F�@FH�@F#:@Fu@E\�@E�@D�@D@C��@C��@Cqv@C;d@C
=@C�@C�@B��@B�H@B�@B	@A��@A8�@A�@@�@@y>@?��@?��@>��@>_�@=�@=��@=<6@<�f@<��@<�/@<�@<y>@<_@;� @;&@:��@:�b@:��@:J�@9��@9�"@9[W@9%@8�`@8m�@7�m@7�a@7�@7��@7\)@7e�@7l�@7v`@7>�@6�@6��@6�h@6($@5�@4��@4w�@4PH@4�@3��@3��@3C�@2�M@2��@2��@2s�@2V@2�@1�3@1�"@1-w@0��@0m�@0~@/�@/�g@/��@/�V@/�P@/g�@.�@.M�@.@-�@-zx@,�@,��@,V�@,7@+�]@+��@+�@*��@*{�@*q�@*h
@*M�@*#:@)�H@)��@)X@)8�@(�P@(ی@(��@(�?@(z�@(M@(%�@'�&@'��@'�f@'Mj@'8@'1�@')_@'&@'"�@'C@' i@&�+@&!�@%�3@%Dg@$��@$�5@$ی@$��@#�r@#�4@#.I@"�R@"kQ@"�@!��@!T�@!2a@!�@ ی@ ��@ �9@ ��@ �4@ �I@ ��@ �@ g8@ Z@ 'R@�P@H�@�@�h@[W@?}@*0@��@A�@4n@'R@� @��@b�@33@"�@@S@�@�@��@kQ@�@��@�z@��@k�@e,@N<@�	@�`@��@��@bN@'R@M@�]@�;@˒@��@�@��@y�@>�@�@�s@��@�x@v�@?@J@�@ϫ@�t@k�@5�@q@�@�/@Ĝ@��@��@V�@�&@��@l�@Mj@
=@�y@��@�b@�\@�+@J@ϫ@�h@Q�@V@�P@�@�@�v@�E@�p@�@��@bN@-�@�K@�f@P�@o@ں@�@��@i�@E�@E�@@�@5?@��@}�@o @ \@�E@�)@Ĝ@��@��@�m@��@s@H�@33@"�@@�@
�8@
�@
Q@
�@	�@	��@	�3@	�H@	�X@	T�@	%F@	@@	�@��@�@�@Ɇ@��@��@`�@�@�F@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B^5B^5B^�B^�B^jB^jB^B^B^B^B^B^B^B^B^5B^5B^5B^OB^OB^OB^OB^OB^OB^5B^B^B^B^B^5B^5B^5B]�B]�B�fB	�,B�BS@BT�BXBo B��B��B��B��B�6B��B�FB��B�eB�=B�B�'B�LB��B��B��B�B��B��B�zB��B~�Bj�B`vBR�B<�B+�BBhB<B	�B�B
�zB
�eB
�B
�'B
m�B
U�B
DMB
(�B	�B	бB	��B	��B	��B	k�B	[qB	M�B	<B	1�B	.IB	.}B	+B	(�B	'�B	%�B	$�B	�B	�B	xB	}B�1B��B��B�2B��B�|B�xB�iB�[B�(B�EB�vB�iB��B�vB��B�-B�MB��B��B	B	?B	�B	)�B	*B	2GB	*�B	&2B	!B	!HB	&fB	3hB	JXB	`�B	^�B	_!B	y>B	��B	��B	|�B	|�B	}<B	��B	��B	zDB	tnB	{0B	yXB	w�B	tB	|�B	�-B	��B	B	��B	��B	�9B	��B	��B	��B	��B	� B	�B	��B	��B	�-B	�zB	�fB	�B	��B	� B	��B	�-B	�{B	��B	�)B	�JB	͹B	�VB	ϑB	ѷB	уB	� B	��B	�B	οB	��B	̳B	�6B	�<B	�PB	��B	ѝB	�SB	ԯB	ӏB	ܒB	��B	�B	�B	��B	�IB	�/B	��B	��B	�QB	�WB	�WB	�B	�DB	�B	�FB	�tB	�ZB	�|B	�B	��B	��B	�>B	�B	�B	��B	�B	�,B	�B	�B	�4B	�B	��B	��B	�B	�4B	�B	�@B	�B	�tB	�ZB	�@B	�ZB	�@B	�ZB	�ZB	�B	�B	��B	�tB	�ZB	�&B	��B	�&B	�B	�B	�B	�`B	�B	�LB	�2B	��B	��B	��B	�zB	��B	��B	��B	�B	�`B	�,B	�2B	��B	�
B	�B	�B	�B	��B	�DB	�0B	�QB	�QB	�qB	�B	��B	��B	�B	�B	�B	�B	�B	��B	�CB	�WB	�qB	��B	�B	��B	�B	� B	�}B	�OB	�OB	��B	�B	�B	��B	��B	�'B	�AB	�B	�B	�[B	��B	�GB	�3B	��B	��B	��B	��B	�FB	��B	��B	��B	�2B	�fB	��B	�$B	�>B	�XB	�rB	��B	�*B	�B	�^B	��B	�DB	�DB	�DB	�DB	�*B	��B	�B	�0B	�B	�dB	��B	�B	��B	�B	�B	��B	��B	��B	�.B	��B
 iB
 iB
 iB
 OB
�B
-B
�B
�B
MB
�B
B
tB
�B
zB
B
�B

#B
�B
_B
�B

=B
	�B
	�B
B
�B
B
�B
�B
"B
<B
"B
<B
(B
vB
\B
BB
B
�B
�B
�B
}B
�B
}B
bB
�B
�B
B
hB
4B
NB
�B
�B
�B
oB
oB
 B
B
�B
B
�B
B
hB
B
 B
NB
NB
 B
B
�B
�B
�B
�B
�B
@B
uB
�B
uB
[B
uB
uB
@B
[B
uB
uB
�B
�B
FB
,B
{B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B

B
+B
�B
�B
B
B
�B
#B
)B
CB
xB
CB
�B
�B
�B
�B
�B
~B
~B
~B
IB
�B
�B
5B
B
B
B
5B
dB
�B
B
�B
�B
�B
�B
B
�B
VB
�B
pB
!B
�B
�B
�B
�B
!B
 �B
 �B
!HB
!|B
!bB
!�B
!�B
"4B
"hB
"hB
"NB
"�B
"�B
"�B
"NB
"hB
#B
"�B
"�B
"�B
#TB
#�B
$&B
$ZB
$tB
$tB
$ZB
$�B
%,B
%�B
%�B
%�B
&�B
'B
'�B
'�B
'�B
'�B
(
B
(XB
)*B
)B
(�B
)yB
)�B
*KB
*�B
*�B
*�B
+�B
+kB
+�B
+�B
,qB
,=B
,=B
,�B
-B
-)B
-�B
-�B
-�B
.IB
.cB
.IB
./B
.�B
.�B
.�B
.�B
.�B
.�B
/5B
/OB
/�B
0B
0;B
0�B
2�B
3B
33B
3MB
3�B
3�B
4B
4B
3�B
3�B
33B
3B
2�B
2�B
2�B
2�B
2�B
33B
3�B
5%B
6FB
72B
72B
6�B
7B
7fB
7fB
72B
7�B
8�B
8lB
8�B
8�B
8�B
9>B
9�B
:*B
:DB
:B
:�B
;dB
;dB
;dB
;dB
;B
;JB
;0B
;JB
;JB
;dB
;B
;�B
<PB
<jB
<PB
<jB
<�B
>]B
>�B
?HB
?cB
?�B
?�B
?�B
@ B
@ B
@OB
@�B
@�B
A�B
A�B
BB
A�B
A�B
B�B
B�B
B�B
B�B
C{B
C�B
C�B
C-B
C�B
DgB
D�B
D�B
D�B
EB
E�B
FtB
F�B
F�B
F�B
F�B
G_B
G�B
HKB
H�B
IB
IB
IB
I�B
J	B
J	B
J�B
J�B
KDB
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L~B
L�B
L�B
L�B
M6B
M�B
M�B
N�B
N�B
OB
O(B
O�B
O�B
P}B
P�B
Q B
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
R B
R:B
R:B
R�B
SB
S&B
S�B
TB
TFB
TFB
TFB
TaB
TFB
T�B
U2B
U�B
U�B
U�B
U�B
U�B
VSB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W
B
WYB
W?B
WYB
W�B
XyB
X�B
X�B
X�B
X�B
Y�B
Y�B
ZB
ZQB
Z7B
Z�B
Z�B
Z�B
Z�B
[	B
[WB
[�B
[�B
\CB
]B
]dB
]/B
]/B
]�B
^OB
^�B
_!B
_!B
_VB
_VB
_pB
_�B
`'B
`\B
`\B
`vB
`vB
`�B
`�B
aB
abB
a�B
a�B
bB
bNB
bhB
bhB
bhB
bhB
b�B
b�B
cTB
c�B
cnB
c�B
dZB
dZB
d�B
d�B
d�B
e,B
e`B
e�B
e�B
e�B
e�B
fB
fB
f�B
f�B
f�B
f�B
f�B
f�B
gB
f�B
gmB
gmB
g�B
g�B
h
B
h$B
hXB
hXB
hsB
hsB
hsB
hsB
hXB
hXB
h�B
iB
i_B
i�B
i�B
i�B
i�B
jB
j�B
kB
kQB
k�B
k�B
l"B
l�B
l�B
l�B
mB
mCB
m]B
mwB
m]B
mwB
mwB
mwB
mwB
mwB
m]B
m]B
m�B
oOB
o�B
o�B
o�B
pB
o�B
pB
p�B
p�B
p�B
p�B
qB
q[B
qvB
q�B
q�B
q�B
q�B
q�B
q�B
r-B
rGB
raB
r�B
r�B
r�B
r�B
sB
sMB
shB
s�B
s�B
s�B
tB
t9B
t9B
tTB
tnB
tnB
tnB
tnB
t�B
t�B
u?B
u?B
uZB
utB
utB
u�B
u�B
vB
v+B
v+B
v�B
v�B
v�B
wB
wB
w2B
wLB
wLB
w�B
xB
xRB
xlB
x�B
x�B
x�B
y$B
yXB
y>B
y>B
y�B
y�B
z*B
z^B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{JB
{�B
|B
|6B
|�B
|�B
|�B
|�B
}"B
}<B
}<B
}<B
}"B
}�B
~B
}�B
~BB
~�B
~�B
~�B
~�B
~�B
�B
�B
�B
�B
�B
�B
�4B
�4B
�4B
�B
�B
�UB
�oB
��B
�oB
�oB
�oB
�B
�B
�AB
�AB
�AB
�AB
�[B
�[B
�uB
��B
��B
�aB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B^5B^5B^�B^�B^jB^jB^B^B^B^B^B^B^B^B^5B^5B^5B^OB^OB^OB^OB^OB^OB^5B^B^B^B^B^5B^5B^5B]�B]�B�fB	�,B�BS@BT�BXBo B��B��B��B��B�6B��B�FB��B�eB�=B�B�'B�LB��B��B��B�B��B��B�zB��B~�Bj�B`vBR�B<�B+�BBhB<B	�B�B
�zB
�eB
�B
�'B
m�B
U�B
DMB
(�B	�B	бB	��B	��B	��B	k�B	[qB	M�B	<B	1�B	.IB	.}B	+B	(�B	'�B	%�B	$�B	�B	�B	xB	}B�1B��B��B�2B��B�|B�xB�iB�[B�(B�EB�vB�iB��B�vB��B�-B�MB��B��B	B	?B	�B	)�B	*B	2GB	*�B	&2B	!B	!HB	&fB	3hB	JXB	`�B	^�B	_!B	y>B	��B	��B	|�B	|�B	}<B	��B	��B	zDB	tnB	{0B	yXB	w�B	tB	|�B	�-B	��B	B	��B	��B	�9B	��B	��B	��B	��B	� B	�B	��B	��B	�-B	�zB	�fB	�B	��B	� B	��B	�-B	�{B	��B	�)B	�JB	͹B	�VB	ϑB	ѷB	уB	� B	��B	�B	οB	��B	̳B	�6B	�<B	�PB	��B	ѝB	�SB	ԯB	ӏB	ܒB	��B	�B	�B	��B	�IB	�/B	��B	��B	�QB	�WB	�WB	�B	�DB	�B	�FB	�tB	�ZB	�|B	�B	��B	��B	�>B	�B	�B	��B	�B	�,B	�B	�B	�4B	�B	��B	��B	�B	�4B	�B	�@B	�B	�tB	�ZB	�@B	�ZB	�@B	�ZB	�ZB	�B	�B	��B	�tB	�ZB	�&B	��B	�&B	�B	�B	�B	�`B	�B	�LB	�2B	��B	��B	��B	�zB	��B	��B	��B	�B	�`B	�,B	�2B	��B	�
B	�B	�B	�B	��B	�DB	�0B	�QB	�QB	�qB	�B	��B	��B	�B	�B	�B	�B	�B	��B	�CB	�WB	�qB	��B	�B	��B	�B	� B	�}B	�OB	�OB	��B	�B	�B	��B	��B	�'B	�AB	�B	�B	�[B	��B	�GB	�3B	��B	��B	��B	��B	�FB	��B	��B	��B	�2B	�fB	��B	�$B	�>B	�XB	�rB	��B	�*B	�B	�^B	��B	�DB	�DB	�DB	�DB	�*B	��B	�B	�0B	�B	�dB	��B	�B	��B	�B	�B	��B	��B	��B	�.B	��B
 iB
 iB
 iB
 OB
�B
-B
�B
�B
MB
�B
B
tB
�B
zB
B
�B

#B
�B
_B
�B

=B
	�B
	�B
B
�B
B
�B
�B
"B
<B
"B
<B
(B
vB
\B
BB
B
�B
�B
�B
}B
�B
}B
bB
�B
�B
B
hB
4B
NB
�B
�B
�B
oB
oB
 B
B
�B
B
�B
B
hB
B
 B
NB
NB
 B
B
�B
�B
�B
�B
�B
@B
uB
�B
uB
[B
uB
uB
@B
[B
uB
uB
�B
�B
FB
,B
{B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B

B
+B
�B
�B
B
B
�B
#B
)B
CB
xB
CB
�B
�B
�B
�B
�B
~B
~B
~B
IB
�B
�B
5B
B
B
B
5B
dB
�B
B
�B
�B
�B
�B
B
�B
VB
�B
pB
!B
�B
�B
�B
�B
!B
 �B
 �B
!HB
!|B
!bB
!�B
!�B
"4B
"hB
"hB
"NB
"�B
"�B
"�B
"NB
"hB
#B
"�B
"�B
"�B
#TB
#�B
$&B
$ZB
$tB
$tB
$ZB
$�B
%,B
%�B
%�B
%�B
&�B
'B
'�B
'�B
'�B
'�B
(
B
(XB
)*B
)B
(�B
)yB
)�B
*KB
*�B
*�B
*�B
+�B
+kB
+�B
+�B
,qB
,=B
,=B
,�B
-B
-)B
-�B
-�B
-�B
.IB
.cB
.IB
./B
.�B
.�B
.�B
.�B
.�B
.�B
/5B
/OB
/�B
0B
0;B
0�B
2�B
3B
33B
3MB
3�B
3�B
4B
4B
3�B
3�B
33B
3B
2�B
2�B
2�B
2�B
2�B
33B
3�B
5%B
6FB
72B
72B
6�B
7B
7fB
7fB
72B
7�B
8�B
8lB
8�B
8�B
8�B
9>B
9�B
:*B
:DB
:B
:�B
;dB
;dB
;dB
;dB
;B
;JB
;0B
;JB
;JB
;dB
;B
;�B
<PB
<jB
<PB
<jB
<�B
>]B
>�B
?HB
?cB
?�B
?�B
?�B
@ B
@ B
@OB
@�B
@�B
A�B
A�B
BB
A�B
A�B
B�B
B�B
B�B
B�B
C{B
C�B
C�B
C-B
C�B
DgB
D�B
D�B
D�B
EB
E�B
FtB
F�B
F�B
F�B
F�B
G_B
G�B
HKB
H�B
IB
IB
IB
I�B
J	B
J	B
J�B
J�B
KDB
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L~B
L�B
L�B
L�B
M6B
M�B
M�B
N�B
N�B
OB
O(B
O�B
O�B
P}B
P�B
Q B
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
R B
R:B
R:B
R�B
SB
S&B
S�B
TB
TFB
TFB
TFB
TaB
TFB
T�B
U2B
U�B
U�B
U�B
U�B
U�B
VSB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W
B
WYB
W?B
WYB
W�B
XyB
X�B
X�B
X�B
X�B
Y�B
Y�B
ZB
ZQB
Z7B
Z�B
Z�B
Z�B
Z�B
[	B
[WB
[�B
[�B
\CB
]B
]dB
]/B
]/B
]�B
^OB
^�B
_!B
_!B
_VB
_VB
_pB
_�B
`'B
`\B
`\B
`vB
`vB
`�B
`�B
aB
abB
a�B
a�B
bB
bNB
bhB
bhB
bhB
bhB
b�B
b�B
cTB
c�B
cnB
c�B
dZB
dZB
d�B
d�B
d�B
e,B
e`B
e�B
e�B
e�B
e�B
fB
fB
f�B
f�B
f�B
f�B
f�B
f�B
gB
f�B
gmB
gmB
g�B
g�B
h
B
h$B
hXB
hXB
hsB
hsB
hsB
hsB
hXB
hXB
h�B
iB
i_B
i�B
i�B
i�B
i�B
jB
j�B
kB
kQB
k�B
k�B
l"B
l�B
l�B
l�B
mB
mCB
m]B
mwB
m]B
mwB
mwB
mwB
mwB
mwB
m]B
m]B
m�B
oOB
o�B
o�B
o�B
pB
o�B
pB
p�B
p�B
p�B
p�B
qB
q[B
qvB
q�B
q�B
q�B
q�B
q�B
q�B
r-B
rGB
raB
r�B
r�B
r�B
r�B
sB
sMB
shB
s�B
s�B
s�B
tB
t9B
t9B
tTB
tnB
tnB
tnB
tnB
t�B
t�B
u?B
u?B
uZB
utB
utB
u�B
u�B
vB
v+B
v+B
v�B
v�B
v�B
wB
wB
w2B
wLB
wLB
w�B
xB
xRB
xlB
x�B
x�B
x�B
y$B
yXB
y>B
y>B
y�B
y�B
z*B
z^B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{JB
{�B
|B
|6B
|�B
|�B
|�B
|�B
}"B
}<B
}<B
}<B
}"B
}�B
~B
}�B
~BB
~�B
~�B
~�B
~�B
~�B
�B
�B
�B
�B
�B
�B
�4B
�4B
�4B
�B
�B
�UB
�oB
��B
�oB
�oB
�oB
�B
�B
�AB
�AB
�AB
�AB
�[B
�[B
�uB
��B
��B
�aB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105242  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192322  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192322  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192322                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042330  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042330  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                