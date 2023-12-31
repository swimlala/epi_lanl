CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-03-29T15:36:54Z creation;2018-03-29T15:37:00Z conversion to V3.1;2019-12-18T07:23:57Z update;2022-11-21T05:31:01Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        X  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ix   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  MP   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  �@   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180329153654  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_132                     2C  DdFNAVIS_A                         0397                            ARGO 011514                     863 @�W"Ϳ��1   @�W#�W @<LC,�zx�dFOv_�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��H@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\Dx�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX��DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D��D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D���D��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111ZA�?}A�A�A�A�A�;dA�1'A�oA���A�E�A���A���A�VA���A�jA��A��A�9XA��A�JA��A��HA���A�A��-A���A�l�A��A��mA���A�~�A�r�A�hsA�ffA�ZA�S�A�Q�A�O�A�9XA�&�A��A��+A�33A���A���A�?}A���A�dZA��-A�XA�"�A�%A��A���A�`BA�ZA�
=A�A�A�?}A�&�A�  A���A�ȴA�l�A�(�A��;A�O�A���A�/A��TA� �A�XA}C�A{�TA{33Az-Ax�Av�jAu�#AuK�At��At��At~�At�AsAsS�As&�Ar�HAr�9Aq��Ao`BAn$�AmƨAm�^Amx�AmVAln�AkdZAi�7Ah�\AhZAh �AhJAg�TAg�
Ag��Ag��AgS�AfĜAf-Ad��Ad5?Ac��Ac�hAc?}AbJA^�HA]S�A\�`A\ZA[��A[p�AZn�AY\)AX�AW�TAVZAU��AU\)AT��AS�;AS|�AR��AP�DAO�PAN1'AM7LAL{AJ�jAI�FAIl�AH��AHAG�AEVAD$�ACS�ABZAA&�A?A?�PA?\)A>��A>��A>�A=�hA=oA<�A;A:�A:�A9��A8z�A6��A6�A5�hA4ȴA49XA3��A2��A2bNA1��A1�PA0��A/�;A/�hA-��A,��A+p�A*n�A)hsA(5?A'��A'\)A&bNA%�wA%x�A%�A$�yA$�RA$��A$~�A$-A#ƨA#"�A"�+A �jA��A�A�AI�A�^A`BAK�A�A��AĜA�\AZA-A�A�A��A�A�A+A�TA�/AĜA�!A��AbNAI�AbA�TAM�A{AƨAdZA
��A��A��A�jAE�A�yAhsAM�A|�A`BA�A �HA j@���@���@�"�@�o@���@�~�@��`@��R@��@�C�@���@�  @�1'@��P@�"�@���@�A�@�^5@��@�V@@�V@���@�=q@�w@�  @�v�@�hs@�+@�5?@�?}@�Q�@ۮ@�
=@���@��@��@�V@�@�9X@�=q@�x�@�x�@�x�@�x�@�`B@�O�@�&�@ЋD@��;@�;d@�o@�ȴ@̋D@�bN@�  @���@�|�@���@ź^@�/@���@ă@�b@���@��@��@�X@���@���@���@���@�;d@���@�E�@��@��@�bN@��w@���@�@��@���@�^5@���@��@��j@��@�;d@���@���@��F@�C�@�ȴ@��+@�~�@�=q@�-@�-@��@��^@�%@�z�@�(�@��@�J@���@�9X@� �@��@��F@�33@���@���@�Z@��F@�\)@��y@�E�@��7@���@���@�r�@� �@��w@�K�@��@��@�M�@��@���@�%@�r�@�1@��@��@��y@���@�ȴ@��!@��\@��^@��F@��!@��@��u@�A�@��;@��P@�t�@�\)@�33@�
=@��H@��!@�n�@��@� �@�33@�v�@��#@���@��h@��h@��h@��h@��7@��h@��h@��@��@��@�hs@�X@�O�@�?}@�/@��@��@���@�1'@���@�ƨ@��P@�C�@��+@�V@�M�@�E�@�{@��#@��^@�O�@�V@�%@���@���@��u@�1'@��;@��P@�o@�~�@���@�?}@���@���@�;@|�@K�@;d@;d@�@~�y@~�R@~ff@~5?@}�T@}p�@}?}@}�@|�@|��@|z�@{�m@{��@{o@{o@{o@{o@zn�@y�^@y�7@yX@x��@w��@w+@w+@v��@v�R@v�R@vV@v{@u�@u�T@u��@u�h@t��@s�F@r�H@r��@rn�@r-@qG�@pA�@o�w@oK�@n��@nv�@nE�@n@m�T@m��@m@m@m�-@mO�@mV@l��@k�m@kC�@k@j�H@j�H@j�!@j~�@jn�@jM�@j=q@i�^@i&�@hQ�@g�;@g�P@gK�@g;d@g;d@g;d@f�@f$�@e`B@eV@d�j@dj@d9X@d�@c�m@c�F@c�F@c��@ct�@c33@b�H@b��@b=q@bJ@a�@a�^@a��@a��@ax�@aX@a%@`Ĝ@`�@`bN@` �@_��@_\)@_+@^�y@^$�@]O�@\��@\j@\1@[@Z�@Y��@Y�^@Y�7@Y�7@YX@Y7L@Y&�@Y%@X��@X�@XA�@W��@W�@V��@Vff@Vff@VV@V{@U�@U�T@U�T@U��@U�-@U��@Up�@UO�@U?}@U/@U/@T��@T9X@S�m@Sƨ@S��@S��@S��@S��@SdZ@S@R��@R��@R��@RM�@Q%@O�@O
=@Nff@N@M�@MO�@M?}@MV@LZ@K�m@K��@Kt�@K33@K@J��@Jn�@J^5@J=q@J-@JJ@JJ@JJ@I��@I��@I��@I�@I�@I�^@H�9@G��@Gl�@GK�@G+@F�y@Fȴ@E�h@D1@C��@CS�@C33@C@B��@B�!@B��@B�\@B�\@B�\@B~�@B^5@B=q@B=q@B-@B�@BJ@Ax�@A7L@A�@A�@@�`@@��@@�u@@A�@@b@?��@=@=�@=p�@=`B@=/@<��@<��@<�D@<(�@<1@;�
@;��@;��@;dZ@;@:�H@:��@:-@9��@8��@8��@8��@8bN@81'@8b@7�@7��@7K�@7
=@6�y@6ȴ@6�R@6v�@6V@65?@6{@5@5�-@5�h@5�@5�@5p�@5O�@5/@4�@49X@3�
@3��@3��@3dZ@333@2��@2~�@2M�@2�@1�#@1�^@1�7@1�@0��@0bN@01'@/�;@/K�@.�@.ȴ@.5?@-�-@-p�@-O�@-�@,�@,j@+ƨ@+t�@+C�@*�H@*�!@*�\@*~�@*n�@*n�@*^5@*M�@*=q@*-@*-@*-@)��@)�@)��@)x�@)x�@)x�@)�7@)�7@)x�@)&�@(�u@(b@'�@'�w@'�@'��@'|�@'l�@'+@'+@'�@&��@&��@%@$��@$��@$�D@$j@$Z@$9X@#�
@#�F@#��@#��@#�F@#��@#�@#�@#�@#t�@#S�@#33@#o@"��@"n�@"-@"�@"-@"J@!�#@!��@!x�@!hs@!7L@ �`@ 1'@�@�;@�w@�w@�w@�@��@��@|�@\)@�@�y@ȴ@v�@$�@�-@O�@V@��@z�@(�@��@@��@n�@^5@-@J@�@�#@��@x�@hs@hs@x�@x�@X@X@X@7L@�@%@Ĝ@�@1'@�@�w@�P@K�@v�@�h@`B@�@I�@t�@o@�H@��@�!@��@�\@~�@M�@�@�7@�@bN@A�@ �@  @  @�;@�@�P@|�@;d@
=@�@V@�T@�-@��@p�@O�@/@�@�/@��@j@9X@�
@�@t�@t�@dZ@33@"�@@@@@@
�H@
^5@
J@	x�@	x�@	hs@	G�@	G�@	&�@	�@	%@	%@��@Ĝ@�9@�u@�@Q�@1'@ �@�@��@�w@��@|�@;d@
=@
=@��@�y@�y@�@ȴ@�R@��@�R@��@ff@V@E�@{@�@�T@@�-@��@�-@�-@�-@�-@�-@��@�-@?}@�j@�j@�j@�j@�D@z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111ZA�?}A�A�A�A�A�;dA�1'A�oA���A�E�A���A���A�VA���A�jA��A��A�9XA��A�JA��A��HA���A�A��-A���A�l�A��A��mA���A�~�A�r�A�hsA�ffA�ZA�S�A�Q�A�O�A�9XA�&�A��A��+A�33A���A���A�?}A���A�dZA��-A�XA�"�A�%A��A���A�`BA�ZA�
=A�A�A�?}A�&�A�  A���A�ȴA�l�A�(�A��;A�O�A���A�/A��TA� �A�XA}C�A{�TA{33Az-Ax�Av�jAu�#AuK�At��At��At~�At�AsAsS�As&�Ar�HAr�9Aq��Ao`BAn$�AmƨAm�^Amx�AmVAln�AkdZAi�7Ah�\AhZAh �AhJAg�TAg�
Ag��Ag��AgS�AfĜAf-Ad��Ad5?Ac��Ac�hAc?}AbJA^�HA]S�A\�`A\ZA[��A[p�AZn�AY\)AX�AW�TAVZAU��AU\)AT��AS�;AS|�AR��AP�DAO�PAN1'AM7LAL{AJ�jAI�FAIl�AH��AHAG�AEVAD$�ACS�ABZAA&�A?A?�PA?\)A>��A>��A>�A=�hA=oA<�A;A:�A:�A9��A8z�A6��A6�A5�hA4ȴA49XA3��A2��A2bNA1��A1�PA0��A/�;A/�hA-��A,��A+p�A*n�A)hsA(5?A'��A'\)A&bNA%�wA%x�A%�A$�yA$�RA$��A$~�A$-A#ƨA#"�A"�+A �jA��A�A�AI�A�^A`BAK�A�A��AĜA�\AZA-A�A�A��A�A�A+A�TA�/AĜA�!A��AbNAI�AbA�TAM�A{AƨAdZA
��A��A��A�jAE�A�yAhsAM�A|�A`BA�A �HA j@���@���@�"�@�o@���@�~�@��`@��R@��@�C�@���@�  @�1'@��P@�"�@���@�A�@�^5@��@�V@@�V@���@�=q@�w@�  @�v�@�hs@�+@�5?@�?}@�Q�@ۮ@�
=@���@��@��@�V@�@�9X@�=q@�x�@�x�@�x�@�x�@�`B@�O�@�&�@ЋD@��;@�;d@�o@�ȴ@̋D@�bN@�  @���@�|�@���@ź^@�/@���@ă@�b@���@��@��@�X@���@���@���@���@�;d@���@�E�@��@��@�bN@��w@���@�@��@���@�^5@���@��@��j@��@�;d@���@���@��F@�C�@�ȴ@��+@�~�@�=q@�-@�-@��@��^@�%@�z�@�(�@��@�J@���@�9X@� �@��@��F@�33@���@���@�Z@��F@�\)@��y@�E�@��7@���@���@�r�@� �@��w@�K�@��@��@�M�@��@���@�%@�r�@�1@��@��@��y@���@�ȴ@��!@��\@��^@��F@��!@��@��u@�A�@��;@��P@�t�@�\)@�33@�
=@��H@��!@�n�@��@� �@�33@�v�@��#@���@��h@��h@��h@��h@��7@��h@��h@��@��@��@�hs@�X@�O�@�?}@�/@��@��@���@�1'@���@�ƨ@��P@�C�@��+@�V@�M�@�E�@�{@��#@��^@�O�@�V@�%@���@���@��u@�1'@��;@��P@�o@�~�@���@�?}@���@���@�;@|�@K�@;d@;d@�@~�y@~�R@~ff@~5?@}�T@}p�@}?}@}�@|�@|��@|z�@{�m@{��@{o@{o@{o@{o@zn�@y�^@y�7@yX@x��@w��@w+@w+@v��@v�R@v�R@vV@v{@u�@u�T@u��@u�h@t��@s�F@r�H@r��@rn�@r-@qG�@pA�@o�w@oK�@n��@nv�@nE�@n@m�T@m��@m@m@m�-@mO�@mV@l��@k�m@kC�@k@j�H@j�H@j�!@j~�@jn�@jM�@j=q@i�^@i&�@hQ�@g�;@g�P@gK�@g;d@g;d@g;d@f�@f$�@e`B@eV@d�j@dj@d9X@d�@c�m@c�F@c�F@c��@ct�@c33@b�H@b��@b=q@bJ@a�@a�^@a��@a��@ax�@aX@a%@`Ĝ@`�@`bN@` �@_��@_\)@_+@^�y@^$�@]O�@\��@\j@\1@[@Z�@Y��@Y�^@Y�7@Y�7@YX@Y7L@Y&�@Y%@X��@X�@XA�@W��@W�@V��@Vff@Vff@VV@V{@U�@U�T@U�T@U��@U�-@U��@Up�@UO�@U?}@U/@U/@T��@T9X@S�m@Sƨ@S��@S��@S��@S��@SdZ@S@R��@R��@R��@RM�@Q%@O�@O
=@Nff@N@M�@MO�@M?}@MV@LZ@K�m@K��@Kt�@K33@K@J��@Jn�@J^5@J=q@J-@JJ@JJ@JJ@I��@I��@I��@I�@I�@I�^@H�9@G��@Gl�@GK�@G+@F�y@Fȴ@E�h@D1@C��@CS�@C33@C@B��@B�!@B��@B�\@B�\@B�\@B~�@B^5@B=q@B=q@B-@B�@BJ@Ax�@A7L@A�@A�@@�`@@��@@�u@@A�@@b@?��@=@=�@=p�@=`B@=/@<��@<��@<�D@<(�@<1@;�
@;��@;��@;dZ@;@:�H@:��@:-@9��@8��@8��@8��@8bN@81'@8b@7�@7��@7K�@7
=@6�y@6ȴ@6�R@6v�@6V@65?@6{@5@5�-@5�h@5�@5�@5p�@5O�@5/@4�@49X@3�
@3��@3��@3dZ@333@2��@2~�@2M�@2�@1�#@1�^@1�7@1�@0��@0bN@01'@/�;@/K�@.�@.ȴ@.5?@-�-@-p�@-O�@-�@,�@,j@+ƨ@+t�@+C�@*�H@*�!@*�\@*~�@*n�@*n�@*^5@*M�@*=q@*-@*-@*-@)��@)�@)��@)x�@)x�@)x�@)�7@)�7@)x�@)&�@(�u@(b@'�@'�w@'�@'��@'|�@'l�@'+@'+@'�@&��@&��@%@$��@$��@$�D@$j@$Z@$9X@#�
@#�F@#��@#��@#�F@#��@#�@#�@#�@#t�@#S�@#33@#o@"��@"n�@"-@"�@"-@"J@!�#@!��@!x�@!hs@!7L@ �`@ 1'@�@�;@�w@�w@�w@�@��@��@|�@\)@�@�y@ȴ@v�@$�@�-@O�@V@��@z�@(�@��@@��@n�@^5@-@J@�@�#@��@x�@hs@hs@x�@x�@X@X@X@7L@�@%@Ĝ@�@1'@�@�w@�P@K�@v�@�h@`B@�@I�@t�@o@�H@��@�!@��@�\@~�@M�@�@�7@�@bN@A�@ �@  @  @�;@�@�P@|�@;d@
=@�@V@�T@�-@��@p�@O�@/@�@�/@��@j@9X@�
@�@t�@t�@dZ@33@"�@@@@@@
�H@
^5@
J@	x�@	x�@	hs@	G�@	G�@	&�@	�@	%@	%@��@Ĝ@�9@�u@�@Q�@1'@ �@�@��@�w@��@|�@;d@
=@
=@��@�y@�y@�@ȴ@�R@��@�R@��@ff@V@E�@{@�@�T@@�-@��@�-@�-@�-@�-@�-@��@�-@?}@�j@�j@�j@�j@�D@z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111n�B��B��B��B��B��BȴB�RB��B��B��B��B��B�VB�%B� By�Bw�Bw�Bv�Bu�Bu�Bu�Bt�Bt�Bt�Bu�Bt�Bo�Bm�Bm�Bm�Bm�Bn�Bo�Bp�Bp�Bq�Bo�BffBQ�B0!B�BDB�BǮB�3B��B��B��B��B��B�uB}�Bl�Be`BW
BD�B1'B�BPB
��B
��B
�B
�B
�NB
��B
��B
�-B
��B
��B
�+B
}�B
x�B
p�B
hsB
\)B
VB
R�B
P�B
O�B
M�B
J�B
H�B
E�B
D�B
B�B
@�B
:^B
-B
&�B
$�B
$�B
"�B
�B
�B
{B

=B
B
B
B
B	��B	��B	��B	��B	��B	��B	�B	�B	�mB	�`B	�TB	�BB	�B	ƨB	�}B	�jB	�XB	�LB	�3B	�B	��B	��B	��B	��B	�uB	�bB	�JB	�+B	�B	}�B	r�B	l�B	ffB	aHB	ZB	Q�B	L�B	J�B	H�B	C�B	?}B	5?B	0!B	,B	&�B	 �B	�B	�B	�B	�B	{B	uB	\B	JB	
=B	%B	B��B��B�B�B�mB�TB�;B�)B�#B�B�
B��B��B��BȴBƨB��B�dB�LB�3B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�oB�=B�B�B� B}�B}�B|�B|�B{�Bz�Bz�By�Bx�Bx�Bw�Bv�Bu�Bq�Bk�Be`BcTBbNBbNBaHBaHB`BB]/BYBVBT�BR�BQ�BN�BJ�BG�BE�BC�B@�B=qB;dB:^B:^B9XB8RB8RB7LB7LB6FB6FB5?B49B33B2-B0!B1'B1'B0!B0!B/B/B.B-B,B+B)�B)�B(�B'�B'�B&�B$�B$�B%�B%�B#�B"�B"�B"�B"�B#�B#�B!�B�B�B�B�B#�B&�B&�B'�B'�B'�B(�B(�B)�B+B)�B(�B-B6FB6FB6FB7LB6FB7LB8RB8RB7LB7LB9XB:^B;dB;dB;dB;dB<jB>wB?}B?}B>wB?}BA�B@�B@�BB�BC�BG�BK�BL�BL�BK�BJ�BI�BL�BM�BO�BQ�BR�BT�BT�BT�BW
BXB[#B\)B]/B]/B]/B]/B]/B_;BbNBcTBdZBcTBdZBdZBhsBjBn�Br�Bt�Bu�B{�B~�B�B�B�%B�1B�7B�=B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�LB�qB�}B��BBĜBĜBŢBƨBǮBȴBɺB��B��B�#B�TB�sB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	%B		7B	
=B	JB	JB	\B	oB	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	&�B	+B	/B	49B	7LB	8RB	=qB	A�B	C�B	C�B	C�B	D�B	E�B	E�B	G�B	H�B	I�B	K�B	L�B	M�B	M�B	N�B	O�B	Q�B	R�B	VB	VB	VB	T�B	XB	[#B	[#B	\)B	`BB	dZB	ffB	ffB	gmB	hsB	hsB	jB	k�B	k�B	l�B	l�B	m�B	p�B	t�B	w�B	x�B	y�B	y�B	}�B	�B	�B	�%B	�7B	�=B	�=B	�JB	�JB	�PB	�PB	�PB	�PB	�\B	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�9B	�?B	�?B	�FB	�FB	�LB	�LB	�RB	�XB	�^B	�jB	�jB	�qB	�qB	�wB	�wB	�wB	�}B	��B	��B	B	B	ÖB	ĜB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�BB	�NB	�NB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
	7B
	7B
	7B
	7B
	7B

=B
PB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
oB
oB
uB
oB
uB
uB
{B
�B
�B
�B
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
"�B
"�B
"�B
#�B
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
'�B
'�B
'�B
(�B
(�B
)�B
+B
+B
+B
+B
,B
-B
-B
-B
-B
.B
.B
.B
/B
0!B
0!B
0!B
1'B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
:^B
:^B
:^B
;dB
<jB
=qB
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
>wB
@�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
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
L�B
L�B
M�B
M�B
M�B
N�B
N�B
O�B
P�B
P�B
Q�B
Q�B
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
VB
XB
XB
XB
YB
ZB
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
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
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
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
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
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
m�B
n�B
m�B
n�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111n�B��B��B�B�VB�"B�JB��B��B�-B�-B��B�sB�\B��B��Bz*BxBxBv�Bu�Bu�Bu�BuBu?ButBvFBu?Bo�Bm�Bm�Bm�Bm�Bn�Bo�Bp�Bp�BrGBq[Bi�BV�B3MBdB�B�B�XB��B��B�NB�B�)B��B�9B�Bm�BgmBYBGEB3�B"hB\B
��B
��B
�B
�]B
�,B
�4B
�gB
�9B
�_B
��B
��B
~�B
zDB
raB
j�B
]/B
V�B
S[B
Q4B
PHB
NVB
KDB
I7B
E�B
EB
CGB
B'B
<�B
.cB
'RB
%B
%FB
#�B
 �B
/B
mB
DB
gB
aB
;B
;B	�B	�B	�]B	�qB	��B	��B	��B	�qB	��B	��B	�@B	�NB	�WB	�fB	�4B	�"B	�B	�8B	�nB	�IB	��B	�FB	�;B	�?B	�aB	�hB	�6B	�B	��B	�OB	t9B	n/B	g�B	b�B	[�B	SB	MjB	K�B	J	B	D�B	BB	6�B	1[B	-wB	(sB	"4B		B	B	B	
B	�B	�B	B	B	xB	EB	'B��B�xB�tB��B�>B�ZB�B�B�B��B��BөB� B��BɺBȴB�GB�B��B��B��B��B��B�CB��B�sB�mB�2B�,B�&B�&B�nB��B��B�!B�xB�B�dB�9B�B��B~wB~BB}<B}<B|PB{JB{0Bz*By	By$BxRBw�Bw�BtBo Bf�Bc�Bb�Bb�Ba�Ba�BabB_�BZ�BV�BU�BS�BS@BQ4BL~BIBF�BE�BBuB>�B<PB:�B:�B9�B9	B8�B7�B7�B6�B6�B5�B5tB4�B3MB1'B1B1B0;B0�B/�B0;B/5B./B,�B+�B+B+B*KB)�B)�B)*B%�B%�B'8B&�B$�B#�B#nB#nB#�B$�B%B"hB \B!B�B;B#�B'B'B($B($B(>B)yB)yB*B+�B*�B*�B/5B6�B6�B6�B7�B6�B7�B8�B8�B7�B88B9�B:�B;�B;�B;�B;�B<�B?B@ B@B?�BABB�BA;BAUBCGBD�BH�BLJBM�BM6BLJBKxBJXBM�BN�BP�BRTBS[BU2BUBU2BW$BX+B[qB\xB]�B]�B]�B]�B^OB`'Bb�Bc�Bd�Bc�Bd�BeFBiBkBoBsBuBv`B|jB}B�GB�SB�tB��B��B��B��B�	B�B�B�BB�4B�@B�2B�RB�*B�B�B�0B�eB�B�]B�B�lB��B��B��B��BĶB��B��B��B��B�B�#B�DB�(B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B��B��B��B�B��B��B�B�9B��B��B�$B�0B�qB	?B		RB	
XB	dB	~B	�B	�B	�B	�B	�B	�B	�B	B	B	 �B	#:B	'mB	+�B	/iB	4�B	7�B	8�B	=�B	A�B	C�B	C�B	C�B	D�B	E�B	E�B	G�B	H�B	I�B	K�B	L�B	M�B	M�B	OB	PB	R B	S&B	VB	VB	VB	UMB	XEB	[=B	[WB	\�B	`�B	dtB	f�B	f�B	g�B	h�B	h�B	j�B	k�B	k�B	l�B	l�B	m�B	q'B	uB	w�B	y	B	zB	zDB	~wB	�AB	�MB	�YB	�RB	�XB	�XB	�dB	�~B	�jB	��B	�jB	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	��B	�B	�B	�DB	�kB	�5B	�;B	�GB	�MB	�TB	�ZB	�ZB	�FB	�`B	�fB	�fB	�lB	�rB	��B	��B	��B	��B	��B	�wB	��B	��B	��B	��B	��B	��B	ªB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�NB	�FB	�
B	�+B	�+B	�1B	�1B	�QB	�7B	�7B	�7B	�WB	�]B	�dB	ބB	�vB	�hB	�NB	�NB	�hB	�nB	�ZB	�ZB	�tB	�tB	�tB	�zB	�zB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�/B	�B	��B	��B	�B	�	B	��B	�B	�B	�6B	�(B	�B
 B
 B
;B
 B
'B
'B
-B
-B
-B
B
B
B
B
B
3B
B
MB
�B
tB
KB
	RB
	RB
	RB
	lB
	�B

�B
�B
pB
VB
pB
pB
vB
\B
\B
\B
\B
vB
vB
}B
bB
}B
}B
}B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B

B
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
 B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
$B
$�B
$�B
%B
$�B
%�B
&B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'B
(
B
(
B
($B
)*B
)B
*B
+B
+B
+B
+B
,=B
-)B
-)B
-CB
-)B
./B
.IB
.IB
/5B
0;B
0UB
0oB
1[B
2GB
2aB
3hB
33B
4TB
4TB
4TB
5tB
5�B
7fB
7�B
7�B
8lB
8lB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9rB
:xB
:xB
:xB
;dB
;dB
;dB
:^B
:xB
:�B
;�B
<�B
=�B
=�B
=�B
=qB
=�B
>wB
>�B
>�B
>�B
>�B
>�B
>�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
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
LB
MB
L�B
NB
M�B
NB
N�B
N�B
PB
Q B
Q B
Q�B
RB
RB
R B
RB
RB
SB
R�B
R�B
R�B
SB
SB
R�B
R�B
SB
S&B
SB
SB
TB
T,B
TB
U2B
U2B
UMB
UMB
VSB
X+B
XEB
XyB
YeB
Z7B
[=B
[=B
\CB
\)B
\CB
\CB
\)B
\CB
]~B
]~B
_VB
_VB
_VB
_VB
`\B
`\B
`\B
`BB
`\B
`vB
abB
abB
a|B
bhB
bhB
cTB
cTB
cTB
cnB
cnB
cnB
cnB
dtB
dtB
dtB
ezB
ffB
ffB
ffB
f�B
ffB
f�B
gmB
gmB
gmB
gRB
g�B
g�B
h�B
h�B
iyB
i�B
i�B
i�B
i�B
iyB
iyB
iyB
iyB
i�B
iyB
i�B
jB
j�B
j�B
j�B
j�B
j�B
j�B
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
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
m�B
n�B
m�B
n�B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804090033492018040900334920180409003349202211182134122022111821341220221118213412201806041923312018060419233120180604192331  JA  ARFMdecpA19c                                                                20180330003533  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180329153654  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180329153658  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180329153658  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180329153659  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180329153659  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180329153659  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180329153659  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180329153659  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180329153700                      G�O�G�O�G�O�                JA  ARUP                                                                        20180329160008                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180329153326  CV  JULD            G�O�G�O�F¹                JM  ARCAJMQC2.0                                                                 20180408153349  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180408153349  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604102331  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171535                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123412  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                