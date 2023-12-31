CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:29:49Z creation;2022-06-04T17:29:49Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604172949  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               (A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��c�P1   @���^o�@0	��l�D�b����F1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�33A   AffA@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  BxffB33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�ffB�  B�  B�  B�ffB�  B�  B�  B�  B�  B�  C   C  C  C�C��C	�fC�fC�fC  C  C  C  C  C  C  C  C   C"�C#�fC%�fC'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV�CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@\)@��G@��A=pA?�
A_�
A��RA��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bh\)Bo��Bx\)B(�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�aGB���B�aGB���B���B���B�aGB���B���B���B���B���B���B���C�qC�qCC�>C	��C��C��C�qC�qC�qC�qC�qC�qC�qC�qC�qC"C#��C%��C'��C)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG��CI�qCK�qCM�qCO�qCQ�qCS�qCVCXCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%x�D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�<{D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��EA��KA��A��KA��>A���A��A���A���A��,A��A��A��A��A��QA��%A��GA��DA���A��A���A��2A��	A��DA���A��]A���A���A���A�A���A��VA��A��A�
�A�xA��A�\A��A�hA�.A��A�hA��A�
�A�	A�	A�%A��A��A��Aż�A���A�m�A�E�A��&A�ҽA�A�poA���A���A���A��jA��A��rA�RTA�A���A���A���A���A��xA��dA�GA���A��A� A�VA���A֡A}��Ay�[ArخAf�zA_��A[4AX>BAU��ARGEAQ
=ANi�AJ�FAH��AF��ACZ�A?�ZA>+kA:�KA9!�A8GA6�A4V�A3��A3�?A2��A1�A/��A.C�A-�TA-�A,h�A*��A)��A)~�A)A'�4A&ffA%+A$a|A#�7A#CA#.�A#�CA$B[A$��A%��A%�]A&�@A'�FA'�A'A&JA%�4A%A&;�A&MA&l�A&I�A%��A#�A"y�A"�A ��A�A��AoA�DA�)A�6A�1A�Ap�A�|Ad�A/�A!-Ax�A��A��A4�A�7AH�A)�A��AE�A��A�[A�RAxA	A��AI�AѷA�oAMA��A�DA��A��AC-A+A�A�_Ac�A[WA%�A�PA��A2�A�AbA��A�A�|A��An�A�A��Ac A�2A��AL�A�A��A��Aq�A#�A��A\)AuA
dZA	�"A	�DA	/A��A*�A�jAZ�AIRA4A
�AѷA\)A5�A�A�HAV�A��A4A�dA�Av�A-A��A�A��A]�A �>A >�@���@��@��u@�0U@��@���@���@�*�@��@�a�@�=@���@��O@�8�@���@�m�@��M@�C�@��U@�	�@��M@���@�,=@�/@�R�@�&�@���@�B�@��@�C�@��N@�t�@�f�@�U�@�;@��`@���@�r@��>@�H�@�@釔@�@@��X@��@��@��@�(�@�+�@�"h@�ی@�@�S&@�X@��8@�s�@���@�A�@��8@���@�@�@��@߼�@���@�1'@�x@��
@ݸ�@ݔ�@�J�@���@�m�@��@���@�S�@���@�$@��Q@�5�@ج@�C-@ש�@�P�@�<6@��@�z�@�4@Ԋr@Ӡ'@���@ҿ�@Ҁ�@�L0@���@�]�@�,�@�V@Ы6@�~@ϯ�@��@�z�@ͣn@��@̧�@�b@��z@��@�h�@��}@�[W@���@�?�@ǈf@�8@�!�@���@Ɠu@�K^@��W@ū�@�@@Ĭ�@č�@�?@Ç�@��@�l�@�&�@���@��@@�^�@��'@�oi@���@�G�@���@�~@�ƨ@�O@��@�~(@�"h@��w@�w2@�J�@��@�Ɇ@���@��@��@���@�L0@�@��}@��'@�Vm@�)_@���@��@�PH@��@�x@��@���@���@���@��f@�rG@�Y�@�F@��@��c@�u%@��]@���@�2a@��E@�Ft@��@�#�@��m@�u�@�/�@��~@�-w@�&@��@���@��@��,@���@���@��\@��@���@�&�@��@�;@�ی@�s�@��@�a@��,@�u�@�:�@���@���@�v`@�
=@���@�e�@�+k@���@�ƨ@��n@�Vm@��]@���@�3�@��@�]�@���@�W�@�6@���@�Y�@��@�z�@�GE@���@�1�@���@��@���@��4@�҉@���@�c�@�E�@�5?@��@�0�@���@�M@���@�s�@�e�@�Y@��R@�z@��x@��@��@�ѷ@���@�b@��Q@���@�`B@�Dg@��f@���@�ff@�~@���@���@���@���@�u�@�Y�@��@��@�	�@��7@�5�@���@��@��o@�e�@�U2@�K^@�7@��T@���@�IR@�!-@�ی@���@�m�@�YK@�'R@��.@��@�ݘ@��@�_p@�;@���@��'@�K^@��&@�p�@�]�@�P�@�A�@�1�@�&�@�C@��@�@���@��<@�z@�@�@���@���@��@���@���@�iD@��@��u@�q@�H@�x@�ݘ@��q@�|�@�Z�@��@��@��]@�n�@�.�@��W@��-@�|@�C�@�@��1@��@���@�F@��2@��@�xl@�;�@��D@���@�|�@�v`@�o @�L�@�(�@�@�%@�ߤ@���@���@�3�@��+@���@���@���@��@�p�@�,�@��M@���@���@��@��4@�$t@��@���@�"h@�F@��@=@~�s@~�\@~0U@}a�@|��@|�@|x@{�Q@{��@{RT@{,�@z��@z��@z{�@z)�@y@y�7@y8�@x��@x��@xl"@xK^@w�+@w��@wO@wC@v��@v��@v��@v��@vu%@vff@v�@u��@uc�@t��@tr�@tQ�@t@s��@s�6@s��@s�0@s�@s�	@sRT@s�@r�h@r��@r�A@rYK@q��@q�@p��@p~(@p/�@os@n��@n��@n{�@nTa@nE�@n0U@m��@ms�@m�@l@k��@ky�@kC�@j�F@i�T@i�n@i4@hg8@g��@g\)@f��@f�@e�n@e!�@e�@e@@e�@d��@dc�@d%�@c��@b��@b�A@b0U@a�T@`��@`!@_x@_X�@_)_@^�\@^5?@^@]�o@]��@]|@]x�@]<6@\�D@[�@[_p@[/�@Z�@Z҉@Z&�@Y��@Y^�@X�/@X�@W�@W�@V�@V�L@V�+@VTa@U�-@Uo @U�@T��@TZ@S�g@SJ#@R�@Ra|@R �@Q�3@QG�@P��@P��@P��@P�@P'R@O��@O��@O�$@Ot�@O6z@O�@N�m@N�@NOv@N.�@N4@M��@MF@M0�@M�@L��@Lr�@L:�@K�@KU�@Ko@J��@JO@Ihs@H�)@H��@HZ@H:�@H*�@H�@G��@G�;@G�0@G.I@G@Fں@F��@F!�@E��@E?}@D�K@Dy>@D	�@C��@C�@C�
@C˒@Cb�@C"�@CY@C�@B�,@B�@BM�@A�@Aa�@@�E@@`�@@~@?�&@?��@?�{@?Mj@>�y@>�@>i�@>�@=�'@=:�@<��@<�@<Xy@;ݘ@;9�@:��@:��@9��@9`B@9�@8��@8w�@8l"@8bN@8]d@8Xy@8PH@8:�@7�{@6��@6c @6#:@6 �@5��@5m]@4�f@4�@3��@3�a@3�a@3��@3��@3��@3l�@2��@2YK@1��@1�h@0��@0�Y@/�@/C�@.��@.�X@.�b@.u%@.Ov@-��@-�~@-:�@-/@-!�@,�f@,�K@,Ĝ@,�Y@+��@+�@+K�@+6z@+33@+)_@+S@*�R@*i�@*B[@*!�@*
�@* �@* �@)�.@)�T@)|@)8�@(�	@(��@(�U@(�z@(H@'��@'8@&�s@&�@&��@&��@&�L@&�F@&��@&i�@&($@%��@%�@%rG@%e,@%8�@%�@%�@$�5@$�@$e�@$�@#� @#�:@#6z@"�'@"�x@"�r@"��@"p;@"W�@"@�@!�@!�X@!��@!c�@!+@!�@ �`@ �o@ $@�m@�K@��@qv@o@��@��@Ov@#:@�-@c�@2a@;@�@��@r�@�@��@�k@P�@&@�@��@~�@J�@GE@H�@?@6�@!�@��@��@;@�K@�@��@�@tT@:�@�@��@�V@�:@x@W?@C@�2@��@YK@+k@4@�Z@�@�X@Vm@�	@��@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��EA��KA��A��KA��>A���A��A���A���A��,A��A��A��A��A��QA��%A��GA��DA���A��A���A��2A��	A��DA���A��]A���A���A���A�A���A��VA��A��A�
�A�xA��A�\A��A�hA�.A��A�hA��A�
�A�	A�	A�%A��A��A��Aż�A���A�m�A�E�A��&A�ҽA�A�poA���A���A���A��jA��A��rA�RTA�A���A���A���A���A��xA��dA�GA���A��A� A�VA���A֡A}��Ay�[ArخAf�zA_��A[4AX>BAU��ARGEAQ
=ANi�AJ�FAH��AF��ACZ�A?�ZA>+kA:�KA9!�A8GA6�A4V�A3��A3�?A2��A1�A/��A.C�A-�TA-�A,h�A*��A)��A)~�A)A'�4A&ffA%+A$a|A#�7A#CA#.�A#�CA$B[A$��A%��A%�]A&�@A'�FA'�A'A&JA%�4A%A&;�A&MA&l�A&I�A%��A#�A"y�A"�A ��A�A��AoA�DA�)A�6A�1A�Ap�A�|Ad�A/�A!-Ax�A��A��A4�A�7AH�A)�A��AE�A��A�[A�RAxA	A��AI�AѷA�oAMA��A�DA��A��AC-A+A�A�_Ac�A[WA%�A�PA��A2�A�AbA��A�A�|A��An�A�A��Ac A�2A��AL�A�A��A��Aq�A#�A��A\)AuA
dZA	�"A	�DA	/A��A*�A�jAZ�AIRA4A
�AѷA\)A5�A�A�HAV�A��A4A�dA�Av�A-A��A�A��A]�A �>A >�@���@��@��u@�0U@��@���@���@�*�@��@�a�@�=@���@��O@�8�@���@�m�@��M@�C�@��U@�	�@��M@���@�,=@�/@�R�@�&�@���@�B�@��@�C�@��N@�t�@�f�@�U�@�;@��`@���@�r@��>@�H�@�@釔@�@@��X@��@��@��@�(�@�+�@�"h@�ی@�@�S&@�X@��8@�s�@���@�A�@��8@���@�@�@��@߼�@���@�1'@�x@��
@ݸ�@ݔ�@�J�@���@�m�@��@���@�S�@���@�$@��Q@�5�@ج@�C-@ש�@�P�@�<6@��@�z�@�4@Ԋr@Ӡ'@���@ҿ�@Ҁ�@�L0@���@�]�@�,�@�V@Ы6@�~@ϯ�@��@�z�@ͣn@��@̧�@�b@��z@��@�h�@��}@�[W@���@�?�@ǈf@�8@�!�@���@Ɠu@�K^@��W@ū�@�@@Ĭ�@č�@�?@Ç�@��@�l�@�&�@���@��@@�^�@��'@�oi@���@�G�@���@�~@�ƨ@�O@��@�~(@�"h@��w@�w2@�J�@��@�Ɇ@���@��@��@���@�L0@�@��}@��'@�Vm@�)_@���@��@�PH@��@�x@��@���@���@���@��f@�rG@�Y�@�F@��@��c@�u%@��]@���@�2a@��E@�Ft@��@�#�@��m@�u�@�/�@��~@�-w@�&@��@���@��@��,@���@���@��\@��@���@�&�@��@�;@�ی@�s�@��@�a@��,@�u�@�:�@���@���@�v`@�
=@���@�e�@�+k@���@�ƨ@��n@�Vm@��]@���@�3�@��@�]�@���@�W�@�6@���@�Y�@��@�z�@�GE@���@�1�@���@��@���@��4@�҉@���@�c�@�E�@�5?@��@�0�@���@�M@���@�s�@�e�@�Y@��R@�z@��x@��@��@�ѷ@���@�b@��Q@���@�`B@�Dg@��f@���@�ff@�~@���@���@���@���@�u�@�Y�@��@��@�	�@��7@�5�@���@��@��o@�e�@�U2@�K^@�7@��T@���@�IR@�!-@�ی@���@�m�@�YK@�'R@��.@��@�ݘ@��@�_p@�;@���@��'@�K^@��&@�p�@�]�@�P�@�A�@�1�@�&�@�C@��@�@���@��<@�z@�@�@���@���@��@���@���@�iD@��@��u@�q@�H@�x@�ݘ@��q@�|�@�Z�@��@��@��]@�n�@�.�@��W@��-@�|@�C�@�@��1@��@���@�F@��2@��@�xl@�;�@��D@���@�|�@�v`@�o @�L�@�(�@�@�%@�ߤ@���@���@�3�@��+@���@���@���@��@�p�@�,�@��M@���@���@��@��4@�$t@��@���@�"h@�F@��@=@~�s@~�\@~0U@}a�@|��@|�@|x@{�Q@{��@{RT@{,�@z��@z��@z{�@z)�@y@y�7@y8�@x��@x��@xl"@xK^@w�+@w��@wO@wC@v��@v��@v��@v��@vu%@vff@v�@u��@uc�@t��@tr�@tQ�@t@s��@s�6@s��@s�0@s�@s�	@sRT@s�@r�h@r��@r�A@rYK@q��@q�@p��@p~(@p/�@os@n��@n��@n{�@nTa@nE�@n0U@m��@ms�@m�@l@k��@ky�@kC�@j�F@i�T@i�n@i4@hg8@g��@g\)@f��@f�@e�n@e!�@e�@e@@e�@d��@dc�@d%�@c��@b��@b�A@b0U@a�T@`��@`!@_x@_X�@_)_@^�\@^5?@^@]�o@]��@]|@]x�@]<6@\�D@[�@[_p@[/�@Z�@Z҉@Z&�@Y��@Y^�@X�/@X�@W�@W�@V�@V�L@V�+@VTa@U�-@Uo @U�@T��@TZ@S�g@SJ#@R�@Ra|@R �@Q�3@QG�@P��@P��@P��@P�@P'R@O��@O��@O�$@Ot�@O6z@O�@N�m@N�@NOv@N.�@N4@M��@MF@M0�@M�@L��@Lr�@L:�@K�@KU�@Ko@J��@JO@Ihs@H�)@H��@HZ@H:�@H*�@H�@G��@G�;@G�0@G.I@G@Fں@F��@F!�@E��@E?}@D�K@Dy>@D	�@C��@C�@C�
@C˒@Cb�@C"�@CY@C�@B�,@B�@BM�@A�@Aa�@@�E@@`�@@~@?�&@?��@?�{@?Mj@>�y@>�@>i�@>�@=�'@=:�@<��@<�@<Xy@;ݘ@;9�@:��@:��@9��@9`B@9�@8��@8w�@8l"@8bN@8]d@8Xy@8PH@8:�@7�{@6��@6c @6#:@6 �@5��@5m]@4�f@4�@3��@3�a@3�a@3��@3��@3��@3l�@2��@2YK@1��@1�h@0��@0�Y@/�@/C�@.��@.�X@.�b@.u%@.Ov@-��@-�~@-:�@-/@-!�@,�f@,�K@,Ĝ@,�Y@+��@+�@+K�@+6z@+33@+)_@+S@*�R@*i�@*B[@*!�@*
�@* �@* �@)�.@)�T@)|@)8�@(�	@(��@(�U@(�z@(H@'��@'8@&�s@&�@&��@&��@&�L@&�F@&��@&i�@&($@%��@%�@%rG@%e,@%8�@%�@%�@$�5@$�@$e�@$�@#� @#�:@#6z@"�'@"�x@"�r@"��@"p;@"W�@"@�@!�@!�X@!��@!c�@!+@!�@ �`@ �o@ $@�m@�K@��@qv@o@��@��@Ov@#:@�-@c�@2a@;@�@��@r�@�@��@�k@P�@&@�@��@~�@J�@GE@H�@?@6�@!�@��@��@;@�K@�@��@�@tT@:�@�@��@�V@�:@x@W?@C@�2@��@YK@+k@4@�Z@�@�X@Vm@�	@��@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�RB�8B�8B�RB�8B�mB�B�8B�8B�8B�B�8B�B�8B�RB��B�8B��B�B�B�B�B�B�B�B�8B�mB�8B�RB�8B�RB�mB�RB�B�B�B�8B�B�B�8B�B��B��B��B�_B�B��B�B�6B�B�B�B	utB
x�B
�B
�sB
�sB
�!B
�B/iB0UB-]B)yB�B
�!B
��B
�wB
�^B
Q4B
;0B
2�B
%�B
�B
�B	��B	�B	�B	��B	�RB	�
B	��B	��B	tTB	E�B	#TB	hB	B��B�B�B�+B	 �B�%B�fB��B��B�	B��BևB�\B�"B�jB̈́B�6B��B�B�jB�KB�B��B��B�tB�B��B	
#B	
	B		B	B	(B	�B	$�B	49B	J	B	aB	zB	��B	��B	��B	��B	�<B	��B	��B	��B	�B
DB
B
�B
"B
"4B
�B
�B
2B
�B
�B
�B
7B
�B
�B
"�B
$�B
"�B
 �B
 �B
�B
!�B
,qB
*eB
'RB
"�B
#TB
%zB
%zB
&2B
1�B
KDB
NVB
N�B
N�B
N�B
M�B
MB
M�B
N�B
P}B
P.B
MB
H�B
HB
K�B
MPB
MB
M�B
L0B
LJB
O�B
P�B
P�B
N�B
N�B
NVB
P�B
O�B
N"B
OvB
T{B
R�B
Q B
N�B
N"B
L�B
K�B
K�B
KDB
K^B
K^B
K)B
KxB
K�B
I�B
I�B
I�B
I�B
IB
HB
GEB
D�B
B�B
A�B
B�B
B�B
B'B
A�B
A�B
CGB
CaB
CB
A;B
@B
>BB
<�B
<�B
;�B
:�B
9>B
7fB
6`B
5�B
4nB
2�B
1�B
0�B
0!B
/�B
/�B
/5B
.�B
.B
-wB
-CB
,�B
,�B
,�B
+�B
+6B
)�B
(�B
(�B
(
B
&�B
%FB
%FB
#nB
"4B
!-B
 �B
�B
/B
�B
CB
CB
�B
B
�B
�B
pB
B
�B
�B
�B
�B
)B
�B
=B
�B
�B
4B
�B
:B
�B
gB
�B
�B
mB
YB
�B
�B
�B
�B
�B
�B
�B
gB
B
[B
�B
�B
aB
{B
B
gB
MB
B
�B
�B
B
NB
 B
.B
bB
�B
�B
 B
�B
HB
�B
�B
\B
�B
�B
vB
vB
(B
BB
(B
�B
�B
pB
VB
VB
"B
jB
6B
6B
�B
�B
dB
dB
B
�B
^B
DB

�B
	�B
	�B
	�B

=B

=B

	B

=B

=B

�B

�B

rB

#B
	�B
	�B
	�B

#B

=B

#B

#B
	�B
	�B

=B

XB

	B

XB
	�B

XB

#B
�B
�B
�B
�B
B
"B
<B
pB
�B
�B
�B
�B
.B
HB
bB
�B
�B
�B
NB
hB
�B
�B
�B
�B
:B
B
:B
TB
TB
TB
TB
:B
�B
�B
:B
oB
 B
�B
�B
�B
oB
�B
�B
&B
&B
B
&B
&B
B
B
�B
�B
�B
B
&B
�B
�B
�B
�B
@B
�B
�B
�B
hB
NB
B
 B
[B
�B
B
gB
�B
B
mB
SB
�B
B
SB
�B
�B
�B
�B
�B
�B
hB
B
�B
B
 B
�B
�B
�B
oB
 B
�B
�B
B
 B
�B
bB
�B
�B
6B
6B
<B
<B
B
�B
vB
.B
�B
9B
sB
B
�B
B
B
eB
�B
�B
=B
�B
�B
�B
B
B
)B
)B
)B
CB
�B
WB
�B
B
�B
=B
�B
�B
�B
�B
�B
�B
�B
�B
�B
/B
5B
�B
!B
;B
�B
�B
�B
�B
�B
 �B
 vB
 BB
 �B
 �B
!B
!�B
!�B
!�B
!�B
!�B
"B
"4B
"4B
"B
"�B
"�B
#�B
#�B
#�B
#�B
$&B
$@B
$�B
%B
%�B
&LB
&B
&fB
&LB
&�B
&�B
&�B
&�B
'B
&�B
'B
'mB
'�B
(
B
(>B
(�B
)B
)�B
*�B
+B
+kB
,B
,WB
,=B
,�B
-B
-wB
-�B
.B
.B
.B
.cB
.}B
.�B
.�B
/ B
/B
/OB
0�B
1[B
1�B
1�B
1�B
1�B
2�B
2�B
3B
3hB
3�B
4TB
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5B
5ZB
5tB
5�B
5�B
6B
6+B
6+B
6`B
6�B
6�B
6�B
72B
7LB
7LB
72B
7�B
7�B
7�B
8B
8RB
88B
8RB
8�B
8�B
9	B
9	B
9	B
9	B
9$B
9$B
9>B
9>B
9rB
9>B
9�B
:^B
:xB
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;B
;B
;dB
;�B
;�B
;�B
;�B
<6B
<�B
<�B
<�B
<�B
=qB
=�B
=�B
>B
>B
=�B
>B
>BB
>BB
>�B
?�B
?}B
?�B
?�B
@4B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
CB
B�B
CaB
C{B
C�B
C�B
C{B
D�B
D�B
D�B
D�B
EB
E�B
FYB
F�B
F�B
F�B
GB
F�B
G+B
G�B
HB
HfB
HfB
H�B
H�B
IB
I7B
IRB
I�B
JrB
J�B
KB
K^B
K^B
K^B
K^B
K�B
K�B
K�B
LdB
LdB
MB
M�B
M�B
N"B
NpB
N�B
OB
O(B
O(B
OBB
OvB
O�B
O�B
PB
PB
PB
PHB
PbB
P�B
P�B
Q B
P�B
P�B
QNB
Q4B
Q4B
Q4B
QNB
Q4B
QhB
QhB
RB
Q�B
RoB
RoB
S&B
S&B
SuB
SuB
SuB
SuB
SuB
SuB
SuB
S@B
S&B
SuB
S�B
T�B
T�B
T�B
T�B
UMB
VB
V�B
V�B
W$B
W$B
W?B
X_B
XyB
XyB
X�B
X�B
X�B
X�B
Y1B
YB
ZB
ZB
ZkB
Z�B
Z�B
Z�B
Z�B
[	B
[	B
[WB
[�B
[�B
\)B
\CB
\]B
\�B
\�B
]~B
]dB
]�B
^�B
^�B
^�B
_VB
_pB
_pB
_pB
_pB
_VB
_VB
_!B
_�B
`�B
`�B
`�B
`�B
aHB
aHB
a�B
bNB
b�B
bhB
bhB
bhB
bhB
b�B
bhB
cB
c:B
cnB
c�B
d&B
dtB
d�B
e`B
ezB
e�B
e�B
e�B
e�B
f2B
f�B
f�B
f�B
f�B
gB
f�B
f�B
gB
g�B
h
B
h$B
h>B
h$B
h$B
h>B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
iDB
iyB
i�B
i�B
i�B
i�B
j0B
jB
kB
k6B
kQB
kQB
kQB
kQB
kkB
kkB
kkB
k�B
k�B
lB
l=B
l=B
lWB
lqB
l�B
lqB
l�B
l�B
m)B
mwB
m�B
m�B
nIB
ncB
n}B
n�B
n�B
n�B
n�B
oB
o5B
oOB
oiB
o�B
o�B
o�B
p!B
poB
p�B
p�B
p�B
p�B
qB
qvB
q�B
q�B
q�B
rGB
r|B
r|B
r�B
r�B
r�B
sB
s�B
s�B
s�B
tTB
tnB
tnB
t�B
uB
uB
uB
uB
u%B
uB
u%B
u?B
u�B
v+B
vFB
v`B
v`B
v`B
v�B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
w�B
x8B
x�B
x�B
x�B
x�B
x�B
y	B
yrB
y�B
y�B
y�B
zx1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�RB�8B�8B�RB�8B�mB�B�8B�8B�8B�B�8B�B�8B�RB��B�8B��B�B�B�B�B�B�B�B�8B�mB�8B�RB�8B�RB�mB�RB�B�B�B�8B�B�B�8B�B��B��B��B�_B�B��B�B�6B�B�B�B	utB
x�B
�B
�sB
�sB
�!B
�B/iB0UB-]B)yB�B
�!B
��B
�wB
�^B
Q4B
;0B
2�B
%�B
�B
�B	��B	�B	�B	��B	�RB	�
B	��B	��B	tTB	E�B	#TB	hB	B��B�B�B�+B	 �B�%B�fB��B��B�	B��BևB�\B�"B�jB̈́B�6B��B�B�jB�KB�B��B��B�tB�B��B	
#B	
	B		B	B	(B	�B	$�B	49B	J	B	aB	zB	��B	��B	��B	��B	�<B	��B	��B	��B	�B
DB
B
�B
"B
"4B
�B
�B
2B
�B
�B
�B
7B
�B
�B
"�B
$�B
"�B
 �B
 �B
�B
!�B
,qB
*eB
'RB
"�B
#TB
%zB
%zB
&2B
1�B
KDB
NVB
N�B
N�B
N�B
M�B
MB
M�B
N�B
P}B
P.B
MB
H�B
HB
K�B
MPB
MB
M�B
L0B
LJB
O�B
P�B
P�B
N�B
N�B
NVB
P�B
O�B
N"B
OvB
T{B
R�B
Q B
N�B
N"B
L�B
K�B
K�B
KDB
K^B
K^B
K)B
KxB
K�B
I�B
I�B
I�B
I�B
IB
HB
GEB
D�B
B�B
A�B
B�B
B�B
B'B
A�B
A�B
CGB
CaB
CB
A;B
@B
>BB
<�B
<�B
;�B
:�B
9>B
7fB
6`B
5�B
4nB
2�B
1�B
0�B
0!B
/�B
/�B
/5B
.�B
.B
-wB
-CB
,�B
,�B
,�B
+�B
+6B
)�B
(�B
(�B
(
B
&�B
%FB
%FB
#nB
"4B
!-B
 �B
�B
/B
�B
CB
CB
�B
B
�B
�B
pB
B
�B
�B
�B
�B
)B
�B
=B
�B
�B
4B
�B
:B
�B
gB
�B
�B
mB
YB
�B
�B
�B
�B
�B
�B
�B
gB
B
[B
�B
�B
aB
{B
B
gB
MB
B
�B
�B
B
NB
 B
.B
bB
�B
�B
 B
�B
HB
�B
�B
\B
�B
�B
vB
vB
(B
BB
(B
�B
�B
pB
VB
VB
"B
jB
6B
6B
�B
�B
dB
dB
B
�B
^B
DB

�B
	�B
	�B
	�B

=B

=B

	B

=B

=B

�B

�B

rB

#B
	�B
	�B
	�B

#B

=B

#B

#B
	�B
	�B

=B

XB

	B

XB
	�B

XB

#B
�B
�B
�B
�B
B
"B
<B
pB
�B
�B
�B
�B
.B
HB
bB
�B
�B
�B
NB
hB
�B
�B
�B
�B
:B
B
:B
TB
TB
TB
TB
:B
�B
�B
:B
oB
 B
�B
�B
�B
oB
�B
�B
&B
&B
B
&B
&B
B
B
�B
�B
�B
B
&B
�B
�B
�B
�B
@B
�B
�B
�B
hB
NB
B
 B
[B
�B
B
gB
�B
B
mB
SB
�B
B
SB
�B
�B
�B
�B
�B
�B
hB
B
�B
B
 B
�B
�B
�B
oB
 B
�B
�B
B
 B
�B
bB
�B
�B
6B
6B
<B
<B
B
�B
vB
.B
�B
9B
sB
B
�B
B
B
eB
�B
�B
=B
�B
�B
�B
B
B
)B
)B
)B
CB
�B
WB
�B
B
�B
=B
�B
�B
�B
�B
�B
�B
�B
�B
�B
/B
5B
�B
!B
;B
�B
�B
�B
�B
�B
 �B
 vB
 BB
 �B
 �B
!B
!�B
!�B
!�B
!�B
!�B
"B
"4B
"4B
"B
"�B
"�B
#�B
#�B
#�B
#�B
$&B
$@B
$�B
%B
%�B
&LB
&B
&fB
&LB
&�B
&�B
&�B
&�B
'B
&�B
'B
'mB
'�B
(
B
(>B
(�B
)B
)�B
*�B
+B
+kB
,B
,WB
,=B
,�B
-B
-wB
-�B
.B
.B
.B
.cB
.}B
.�B
.�B
/ B
/B
/OB
0�B
1[B
1�B
1�B
1�B
1�B
2�B
2�B
3B
3hB
3�B
4TB
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5B
5ZB
5tB
5�B
5�B
6B
6+B
6+B
6`B
6�B
6�B
6�B
72B
7LB
7LB
72B
7�B
7�B
7�B
8B
8RB
88B
8RB
8�B
8�B
9	B
9	B
9	B
9	B
9$B
9$B
9>B
9>B
9rB
9>B
9�B
:^B
:xB
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;B
;B
;dB
;�B
;�B
;�B
;�B
<6B
<�B
<�B
<�B
<�B
=qB
=�B
=�B
>B
>B
=�B
>B
>BB
>BB
>�B
?�B
?}B
?�B
?�B
@4B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
CB
B�B
CaB
C{B
C�B
C�B
C{B
D�B
D�B
D�B
D�B
EB
E�B
FYB
F�B
F�B
F�B
GB
F�B
G+B
G�B
HB
HfB
HfB
H�B
H�B
IB
I7B
IRB
I�B
JrB
J�B
KB
K^B
K^B
K^B
K^B
K�B
K�B
K�B
LdB
LdB
MB
M�B
M�B
N"B
NpB
N�B
OB
O(B
O(B
OBB
OvB
O�B
O�B
PB
PB
PB
PHB
PbB
P�B
P�B
Q B
P�B
P�B
QNB
Q4B
Q4B
Q4B
QNB
Q4B
QhB
QhB
RB
Q�B
RoB
RoB
S&B
S&B
SuB
SuB
SuB
SuB
SuB
SuB
SuB
S@B
S&B
SuB
S�B
T�B
T�B
T�B
T�B
UMB
VB
V�B
V�B
W$B
W$B
W?B
X_B
XyB
XyB
X�B
X�B
X�B
X�B
Y1B
YB
ZB
ZB
ZkB
Z�B
Z�B
Z�B
Z�B
[	B
[	B
[WB
[�B
[�B
\)B
\CB
\]B
\�B
\�B
]~B
]dB
]�B
^�B
^�B
^�B
_VB
_pB
_pB
_pB
_pB
_VB
_VB
_!B
_�B
`�B
`�B
`�B
`�B
aHB
aHB
a�B
bNB
b�B
bhB
bhB
bhB
bhB
b�B
bhB
cB
c:B
cnB
c�B
d&B
dtB
d�B
e`B
ezB
e�B
e�B
e�B
e�B
f2B
f�B
f�B
f�B
f�B
gB
f�B
f�B
gB
g�B
h
B
h$B
h>B
h$B
h$B
h>B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
iDB
iyB
i�B
i�B
i�B
i�B
j0B
jB
kB
k6B
kQB
kQB
kQB
kQB
kkB
kkB
kkB
k�B
k�B
lB
l=B
l=B
lWB
lqB
l�B
lqB
l�B
l�B
m)B
mwB
m�B
m�B
nIB
ncB
n}B
n�B
n�B
n�B
n�B
oB
o5B
oOB
oiB
o�B
o�B
o�B
p!B
poB
p�B
p�B
p�B
p�B
qB
qvB
q�B
q�B
q�B
rGB
r|B
r|B
r�B
r�B
r�B
sB
s�B
s�B
s�B
tTB
tnB
tnB
t�B
uB
uB
uB
uB
u%B
uB
u%B
u?B
u�B
v+B
vFB
v`B
v`B
v`B
v�B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
w�B
x8B
x�B
x�B
x�B
x�B
x�B
y	B
yrB
y�B
y�B
y�B
zx1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104859  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172949  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172949  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172949                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022956  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022956  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                