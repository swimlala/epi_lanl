CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-09-23T09:01:12Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        x  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  `    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �     TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȑ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220923090112  20220923090112  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @���?%��1   @����-� @*Õ�$��ds�
=p�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D��Dy�D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B \)B'�]B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�.B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\Dx�D��Dx�D�\D\D�\D\D�D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D7�D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�dZA�dZA�hsA�dZA�`BA�`BA�bNA�ffA�l�A�p�A�r�A�p�A�r�A�r�A�r�A�r�A�t�A�t�A�S�A�`BAڣ�Aغ^A�{A�5?A�Q�A�&�AԍPA�A�\)AБhA�=qA�33A�=qAʉ7A�jA��A�bNA�ffA���AǋDA�C�A���A²-A��A���A��A�VA� �A��\A���A�-A�{A��!A�7LA�?}A��A��hA��A���A�?}A�ĜA�ZA��A��+A�VA�G�A��A��A�1A��A���A��mA��hA�C�A�  A�oA{��Ax�AtJAo7LAm�wAlVAi
=AbȴA`A�A^�A\A�AY�
AWl�AVI�ASƨAO�TALQ�AHv�AGO�AD��AB~�A@��A?�TA?p�A=��A;��A:VA8(�A6��A4n�A2�uA1"�A/��A.v�A.-A-�A-�A.bNA-�A,ZA+�^A+l�A+�A*�9A*(�A)l�A)�A(�RA(ffA'��A'�A'�hA'\)A&��A&n�A%C�A$��A$Q�A$(�A#��A!ƨA!�A �/A �RA �\A bNA E�A 1'A {A�;A�^A�A��AO�A�+AA�A�
A�hAK�A�A��AȴA�A��Ax�A/A�`Av�A�wA;dA1'A�TA�wA��A��At�A;dA��AM�AA�hA%AĜA�!A��AVAA�A�AJA�AK�A
=A�/A1'A�^A�A�A��AȴA�A�A�wAO�A;dA33AoAĜA�DA$�AAt�A`BAK�A�A
��A
�\A
�A
A�A	�#A	XA	/A	VA�jA��AjA9XA�A�7AdZAG�AAȴA��AZAJA�PA�A�\A$�A  A�#A�PAK�A��A��AZA-A�;AhsA�A �yA �jA �uA r�A �@�33@��y@�ff@��@��h@��@�A�@�\)@�ȴ@�M�@���@�`B@�&�@��/@���@�j@� �@�|�@��H@��@��@�p�@�?}@�z�@�C�@�@�h@�(�@��@�K�@���@�R@�+@�^5@�V@�M�@�=q@��-@�Ĝ@�bN@�b@땁@�;d@���@�M�@�-@�7@�hs@�V@�A�@�w@�l�@��@�X@��/@�(�@�dZ@�C�@���@�V@�hs@�z�@�I�@��m@��y@��T@��@ܼj@ܣ�@�bN@��@�33@�ȴ@�ff@�J@ٙ�@�z�@���@�C�@�E�@��@�7L@� �@Ӿw@�V@�hs@��/@�I�@϶F@�ȴ@�E�@���@͑h@̴9@���@��H@ʇ+@ʇ+@��@�x�@�7L@ȴ9@���@Ƨ�@�ff@�@ŉ7@�?}@�%@�Ĝ@�9X@���@�dZ@���@��H@��@§�@�^5@���@�/@�/@�V@��D@���@�n�@��T@�hs@�Z@�l�@��y@�{@��^@�%@���@�(�@��P@��!@��@�p�@�V@��/@��@�Z@�b@���@�"�@���@��@���@�O�@��@�I�@�(�@�1@��
@��F@�dZ@��@���@��\@�-@�x�@���@���@��9@���@�z�@�  @��F@�C�@�+@��@���@�~�@�n�@�M�@���@���@�G�@���@��`@��9@��D@�z�@�bN@�bN@�A�@��F@�+@��H@�ȴ@��!@��+@�E�@��#@�O�@���@�I�@�1'@��;@�dZ@�K�@�C�@�;d@�ȴ@�~�@�@��7@�/@���@��j@��@�(�@��m@���@�l�@�S�@�o@��\@�$�@��@���@�O�@��@�Ĝ@�A�@�ƨ@�;d@��!@�-@���@��h@��@���@� �@��@�~�@�J@���@���@�p�@��@���@�Z@� �@���@��F@�33@�@��H@��R@��+@�M�@��@��-@�O�@��/@��9@���@���@��@�r�@�A�@��
@���@��P@�\)@�o@�^5@�@���@��^@��h@�x�@�V@�Ĝ@��D@�(�@��@�ƨ@�l�@���@�E�@�=q@�5?@�{@��T@��^@�X@�%@���@�I�@���@�\)@�"�@��@���@�E�@��T@�@���@�&�@��`@��D@�(�@���@���@���@��@�33@�"�@�@��!@�V@�@��#@��-@��h@��@�`B@�V@���@��D@�j@�I�@�9X@�(�@�;@K�@~��@~��@~ff@~E�@~5?@~$�@}�@}��@}�@}V@|�@|�j@|j@|1@{C�@z�!@zn�@z=q@zJ@y�#@yhs@x�`@x�u@xQ�@w��@w��@w\)@w;d@v�y@vȴ@v��@u��@u`B@t�@t��@tj@t(�@s�m@sdZ@s"�@r�\@rJ@q��@q%@p�@p1'@ol�@o
=@n��@n@m�-@m`B@mV@l�/@l�D@lZ@l9X@k�m@k"�@j��@j�!@j~�@i�@i�@hĜ@h�u@g�;@gl�@gK�@g+@fȴ@f�+@fE�@e�@e?}@d�@d��@dz�@dZ@d1@c��@cdZ@b�@b~�@b�@a��@a�7@ahs@aX@aX@aG�@a7L@a&�@`�`@`�9@`��@`�u@`�@`1'@_��@_
=@^�@^��@^V@^@]`B@\��@\(�@[��@[��@[S�@["�@Z�!@ZM�@Y��@X��@XbN@W�@W�P@W;d@Vȴ@V$�@UV@T�@Tz�@T9X@St�@R�H@R=q@Q��@Q7L@P�9@P�u@P �@O|�@N�R@NV@NE�@N5?@N5?@M�@M/@MV@L�/@L��@Lz�@L9X@K��@K�F@Kt�@J��@J=q@J�@I�@I�^@Ihs@H�9@H �@G�@G�@G�@G��@G�P@G|�@GK�@F��@F�y@F�@F�+@F@E��@E?}@Dz�@D�D@D��@D�/@D��@Dz�@DZ@D1@Cƨ@CS�@B��@B��@Bn�@BM�@A�@A��@AG�@A%@@�u@@b@?�@?��@?�P@?�@>�@>V@>$�@=�T@=@=�@=O�@=/@=V@<��@<�@;ƨ@;t�@;dZ@:�@:�\@:n�@:^5@:M�@9��@9x�@9%@8Ĝ@8�u@8A�@8b@8  @7��@7�P@7\)@7K�@7
=@6�R@6�+@6ff@6{@5�T@5@5��@5�h@5�h@5�@5p�@5?}@4�/@4j@3t�@333@2�@2��@2��@2M�@2-@1�@17L@0r�@0bN@0A�@0 �@/�;@/l�@/�@.��@.@-��@-�h@-�@-O�@-V@-V@-V@,��@,��@,��@,z�@,Z@+�
@+dZ@+C�@+33@+"�@*��@*M�@*-@)�@)��@)��@)�7@)�7@)x�@)x�@)7L@(�9@(r�@(1'@'�;@'��@'�@'\)@'K�@';d@&�@&��@&{@%��@%�-@$�/@$�D@$j@$I�@#�m@#�@#�@#�@#33@#o@"�@"�H@"��@"��@"=q@!��@!��@!�7@!x�@!X@!7L@!7L@!&�@!%@ �`@ �`@ ��@ Ĝ@ ��@ �@ Q�@ b@�;@�w@�P@\)@��@�R@�R@��@v�@V@E�@5?@5?@$�@@��@�@/@z�@Z@Z@9X@�@��@��@�@"�@��@��@~�@n�@n�@^5@�^@hs@G�@7L@&�@%@��@�`@�9@�u@�@bN@ �@b@�;@��@l�@+@
=@�@�R@��@v�@E�@@@p�@O�@?}@/@��@�@�/@�@��@z�@�D@�D@Z@(�@�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�dZA�dZA�hsA�dZA�`BA�`BA�bNA�ffA�l�A�p�A�r�A�p�A�r�A�r�A�r�A�r�A�t�A�t�A�S�A�`BAڣ�Aغ^A�{A�5?A�Q�A�&�AԍPA�A�\)AБhA�=qA�33A�=qAʉ7A�jA��A�bNA�ffA���AǋDA�C�A���A²-A��A���A��A�VA� �A��\A���A�-A�{A��!A�7LA�?}A��A��hA��A���A�?}A�ĜA�ZA��A��+A�VA�G�A��A��A�1A��A���A��mA��hA�C�A�  A�oA{��Ax�AtJAo7LAm�wAlVAi
=AbȴA`A�A^�A\A�AY�
AWl�AVI�ASƨAO�TALQ�AHv�AGO�AD��AB~�A@��A?�TA?p�A=��A;��A:VA8(�A6��A4n�A2�uA1"�A/��A.v�A.-A-�A-�A.bNA-�A,ZA+�^A+l�A+�A*�9A*(�A)l�A)�A(�RA(ffA'��A'�A'�hA'\)A&��A&n�A%C�A$��A$Q�A$(�A#��A!ƨA!�A �/A �RA �\A bNA E�A 1'A {A�;A�^A�A��AO�A�+AA�A�
A�hAK�A�A��AȴA�A��Ax�A/A�`Av�A�wA;dA1'A�TA�wA��A��At�A;dA��AM�AA�hA%AĜA�!A��AVAA�A�AJA�AK�A
=A�/A1'A�^A�A�A��AȴA�A�A�wAO�A;dA33AoAĜA�DA$�AAt�A`BAK�A�A
��A
�\A
�A
A�A	�#A	XA	/A	VA�jA��AjA9XA�A�7AdZAG�AAȴA��AZAJA�PA�A�\A$�A  A�#A�PAK�A��A��AZA-A�;AhsA�A �yA �jA �uA r�A �@�33@��y@�ff@��@��h@��@�A�@�\)@�ȴ@�M�@���@�`B@�&�@��/@���@�j@� �@�|�@��H@��@��@�p�@�?}@�z�@�C�@�@�h@�(�@��@�K�@���@�R@�+@�^5@�V@�M�@�=q@��-@�Ĝ@�bN@�b@땁@�;d@���@�M�@�-@�7@�hs@�V@�A�@�w@�l�@��@�X@��/@�(�@�dZ@�C�@���@�V@�hs@�z�@�I�@��m@��y@��T@��@ܼj@ܣ�@�bN@��@�33@�ȴ@�ff@�J@ٙ�@�z�@���@�C�@�E�@��@�7L@� �@Ӿw@�V@�hs@��/@�I�@϶F@�ȴ@�E�@���@͑h@̴9@���@��H@ʇ+@ʇ+@��@�x�@�7L@ȴ9@���@Ƨ�@�ff@�@ŉ7@�?}@�%@�Ĝ@�9X@���@�dZ@���@��H@��@§�@�^5@���@�/@�/@�V@��D@���@�n�@��T@�hs@�Z@�l�@��y@�{@��^@�%@���@�(�@��P@��!@��@�p�@�V@��/@��@�Z@�b@���@�"�@���@��@���@�O�@��@�I�@�(�@�1@��
@��F@�dZ@��@���@��\@�-@�x�@���@���@��9@���@�z�@�  @��F@�C�@�+@��@���@�~�@�n�@�M�@���@���@�G�@���@��`@��9@��D@�z�@�bN@�bN@�A�@��F@�+@��H@�ȴ@��!@��+@�E�@��#@�O�@���@�I�@�1'@��;@�dZ@�K�@�C�@�;d@�ȴ@�~�@�@��7@�/@���@��j@��@�(�@��m@���@�l�@�S�@�o@��\@�$�@��@���@�O�@��@�Ĝ@�A�@�ƨ@�;d@��!@�-@���@��h@��@���@� �@��@�~�@�J@���@���@�p�@��@���@�Z@� �@���@��F@�33@�@��H@��R@��+@�M�@��@��-@�O�@��/@��9@���@���@��@�r�@�A�@��
@���@��P@�\)@�o@�^5@�@���@��^@��h@�x�@�V@�Ĝ@��D@�(�@��@�ƨ@�l�@���@�E�@�=q@�5?@�{@��T@��^@�X@�%@���@�I�@���@�\)@�"�@��@���@�E�@��T@�@���@�&�@��`@��D@�(�@���@���@���@��@�33@�"�@�@��!@�V@�@��#@��-@��h@��@�`B@�V@���@��D@�j@�I�@�9X@�(�@�;@K�@~��@~��@~ff@~E�@~5?@~$�@}�@}��@}�@}V@|�@|�j@|j@|1@{C�@z�!@zn�@z=q@zJ@y�#@yhs@x�`@x�u@xQ�@w��@w��@w\)@w;d@v�y@vȴ@v��@u��@u`B@t�@t��@tj@t(�@s�m@sdZ@s"�@r�\@rJ@q��@q%@p�@p1'@ol�@o
=@n��@n@m�-@m`B@mV@l�/@l�D@lZ@l9X@k�m@k"�@j��@j�!@j~�@i�@i�@hĜ@h�u@g�;@gl�@gK�@g+@fȴ@f�+@fE�@e�@e?}@d�@d��@dz�@dZ@d1@c��@cdZ@b�@b~�@b�@a��@a�7@ahs@aX@aX@aG�@a7L@a&�@`�`@`�9@`��@`�u@`�@`1'@_��@_
=@^�@^��@^V@^@]`B@\��@\(�@[��@[��@[S�@["�@Z�!@ZM�@Y��@X��@XbN@W�@W�P@W;d@Vȴ@V$�@UV@T�@Tz�@T9X@St�@R�H@R=q@Q��@Q7L@P�9@P�u@P �@O|�@N�R@NV@NE�@N5?@N5?@M�@M/@MV@L�/@L��@Lz�@L9X@K��@K�F@Kt�@J��@J=q@J�@I�@I�^@Ihs@H�9@H �@G�@G�@G�@G��@G�P@G|�@GK�@F��@F�y@F�@F�+@F@E��@E?}@Dz�@D�D@D��@D�/@D��@Dz�@DZ@D1@Cƨ@CS�@B��@B��@Bn�@BM�@A�@A��@AG�@A%@@�u@@b@?�@?��@?�P@?�@>�@>V@>$�@=�T@=@=�@=O�@=/@=V@<��@<�@;ƨ@;t�@;dZ@:�@:�\@:n�@:^5@:M�@9��@9x�@9%@8Ĝ@8�u@8A�@8b@8  @7��@7�P@7\)@7K�@7
=@6�R@6�+@6ff@6{@5�T@5@5��@5�h@5�h@5�@5p�@5?}@4�/@4j@3t�@333@2�@2��@2��@2M�@2-@1�@17L@0r�@0bN@0A�@0 �@/�;@/l�@/�@.��@.@-��@-�h@-�@-O�@-V@-V@-V@,��@,��@,��@,z�@,Z@+�
@+dZ@+C�@+33@+"�@*��@*M�@*-@)�@)��@)��@)�7@)�7@)x�@)x�@)7L@(�9@(r�@(1'@'�;@'��@'�@'\)@'K�@';d@&�@&��@&{@%��@%�-@$�/@$�D@$j@$I�@#�m@#�@#�@#�@#33@#o@"�@"�H@"��@"��@"=q@!��@!��@!�7@!x�@!X@!7L@!7L@!&�@!%@ �`@ �`@ ��@ Ĝ@ ��@ �@ Q�@ b@�;@�w@�P@\)@��@�R@�R@��@v�@V@E�@5?@5?@$�@@��@�@/@z�@Z@Z@9X@�@��@��@�@"�@��@��@~�@n�@n�@^5@�^@hs@G�@7L@&�@%@��@�`@�9@�u@�@bN@ �@b@�;@��@l�@+@
=@�@�R@��@v�@E�@@@p�@O�@?}@/@��@�@�/@�@��@z�@�D@�D@Z@(�@�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B	�B	�B
��B
��B
�TB
�B
�B
��B
�B
�`B+B'�B7LB1'BT�B~�B�XB�}BǮB�
B��B�dB��B��B��B��B��B�-B�RB�{B��B�uB��B��B`BB�%B|�B{�Bx�B� BhsB9XB0!B&�B33B9XB,B
�5B
�B
��B
�+B
bNB
?}B
�B
\B
B	�#B	�RB	�B	�+B	t�B	v�B	k�B	B�B	oB	,B	)�B	 �B	�B	JB	JB��B�B�HB�
B�fB�B��B	%B	hB	$�B	 �B	$�B	1'B	,B	>wB	9XB	L�B	YB	_;B	r�B	�DB	�uB	��B	�!B	�wB	��B	�B	��B	��B	��B	��B
B
PB
VB
hB
uB
�B
�B
�B
{B
oB
PB
�B
�B
�B
�B
PB
'�B
1'B
33B
49B
49B
6FB
6FB
6FB
5?B
6FB
7LB
5?B
2-B
.B
6FB
>wB
>wB
<jB
<jB
=qB
=qB
<jB
:^B
8RB
9XB
=qB
<jB
:^B
7LB
=qB
9XB
G�B
K�B
L�B
K�B
J�B
H�B
G�B
D�B
E�B
M�B
J�B
M�B
P�B
O�B
L�B
I�B
F�B
D�B
C�B
L�B
N�B
P�B
P�B
K�B
L�B
P�B
P�B
P�B
S�B
Q�B
O�B
O�B
P�B
T�B
VB
S�B
Q�B
Q�B
P�B
Q�B
R�B
VB
VB
S�B
Q�B
T�B
T�B
P�B
N�B
N�B
Q�B
Q�B
P�B
P�B
P�B
P�B
O�B
N�B
P�B
P�B
N�B
N�B
N�B
L�B
J�B
G�B
G�B
G�B
G�B
J�B
J�B
G�B
G�B
G�B
F�B
D�B
E�B
C�B
A�B
B�B
D�B
C�B
B�B
A�B
?}B
<jB
A�B
?}B
?}B
>wB
=qB
:^B
9XB
<jB
<jB
<jB
<jB
=qB
=qB
<jB
<jB
:^B
8RB
8RB
7LB
:^B
8RB
8RB
49B
0!B
33B
1'B
0!B
5?B
8RB
7LB
8RB
8RB
9XB
9XB
8RB
6FB
33B
0!B
33B
33B
2-B
2-B
33B
0!B
1'B
33B
2-B
0!B
-B
.B
-B
+B
$�B
,B
+B
+B
-B
,B
)�B
'�B
'�B
,B
)�B
%�B
(�B
)�B
-B
.B
-B
+B
(�B
)�B
+B
)�B
'�B
#�B
%�B
%�B
"�B
$�B
"�B
�B
"�B
�B
�B
 �B
 �B
�B
�B
"�B
#�B
!�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
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
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
!�B
!�B
�B
 �B
�B
 �B
 �B
!�B
"�B
�B
$�B
$�B
#�B
$�B
"�B
"�B
$�B
#�B
"�B
!�B
#�B
%�B
&�B
&�B
%�B
#�B
$�B
$�B
&�B
&�B
&�B
'�B
'�B
&�B
$�B
'�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
'�B
%�B
%�B
(�B
+B
)�B
(�B
'�B
&�B
&�B
&�B
)�B
,B
+B
)�B
-B
-B
-B
)�B
+B
)�B
+B
,B
.B
-B
-B
,B
-B
-B
.B
.B
-B
,B
-B
/B
.B
.B
.B
.B
-B
,B
,B
-B
-B
.B
/B
.B
-B
-B
)�B
/B
1'B
1'B
49B
33B
1'B
1'B
33B
5?B
49B
49B
33B
6FB
6FB
6FB
6FB
6FB
5?B
6FB
6FB
6FB
9XB
:^B
:^B
9XB
9XB
8RB
7LB
8RB
9XB
8RB
7LB
6FB
9XB
;dB
<jB
<jB
<jB
;dB
;dB
<jB
;dB
=qB
=qB
<jB
9XB
?}B
A�B
A�B
@�B
@�B
?}B
>wB
>wB
?}B
?}B
?}B
A�B
C�B
C�B
B�B
B�B
C�B
E�B
E�B
C�B
D�B
D�B
E�B
F�B
G�B
I�B
H�B
G�B
I�B
H�B
G�B
G�B
H�B
I�B
J�B
J�B
J�B
J�B
H�B
I�B
J�B
J�B
K�B
K�B
K�B
J�B
I�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
L�B
L�B
K�B
L�B
N�B
N�B
N�B
N�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
M�B
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
Q�B
R�B
Q�B
R�B
S�B
S�B
T�B
T�B
VB
VB
VB
W
B
VB
VB
T�B
W
B
W
B
W
B
VB
T�B
XB
XB
W
B
XB
ZB
ZB
YB
YB
YB
XB
W
B
W
B
ZB
ZB
ZB
ZB
YB
[#B
[#B
[#B
[#B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
]/B
]/B
]/B
^5B
^5B
]/B
\)B
[#B
\)B
\)B
\)B
[#B
[#B
YB
YB
ZB
[#B
\)B
\)B
]/B
\)B
\)B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
[#B
ZB
^5B
^5B
^5B
]/B
]/B
_;B
_;B
`BB
`BB
aHB
`BB
_;B
`BB
bNB
dZB
dZB
dZB
dZB
bNB
e`B
ffB
e`B
ffB
e`B
e`B
e`B
dZB
dZB
dZB
ffB
ffB
ffB
e`B
dZB
ffB
gmB
iyB
jB
iyB
iyB
iyB
hsB
hsB
iyB
iyB
hsB
hsB
iyB
jB
jB
l�B
n�B
n�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
o�B
p�B
p�B
p�B
o�B
p�B
p�B
q�B
q�B
r�B
q�B
q�B
q�B
q�B
p�B
p�B
q�B
q�B
r�B
q�B
q�B
r�B
r�B
q�B
o�B
q�B
q�B
q�B
r�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
s�B
s�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
r�B
u�B
v�B
v�B
v�B
v�B
v�B
u�B
s�B
s�B
v�B
v�B
u�B
u�B
t�B
t�B
t�B
u�B
v�B
x�B
x�B
w�B
x�B
y�B
y�B
y�B
y�B
x�B
y�B
y�B
y�B
y�B
{�B
{�B
{�B
z�B
z�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
|�B
{�B
|�B
|�B
|�B
}�B
}�B
|�B
~�B
}�B
|�B
}�B
{�B
}�B
}�B
|�B
~�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�B
�B
�B
�B
�B
�B
�+B
�+B
�+B
�+B
�+B
�B
�%B
�+B
�1B
�1B
�1B
�1B
�1B
�+B
�1B
�1B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�7B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�7B
�DB
�DB
�DB
�DB
�DB
�JB
�DB
�JB
�JB
�PB
�PB
�JB
�JB
�PB	G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B	�B	�B
��B
��B
�TB
�B
�B
��B
�B
�`B+B'�B7LB1'BT�B~�B�XB�}BǮB�
B��B�dB��B��B��B��B��B�-B�RB�{B��B�uB��B��B`BB�%B|�B{�Bx�B� BhsB9XB0!B&�B33B9XB,B
�5B
�B
��B
�+B
bNB
?}B
�B
\B
B	�#B	�RB	�B	�+B	t�B	v�B	k�B	B�B	oB	,B	)�B	 �B	�B	JB	JB��B�B�HB�
B�fB�B��B	%B	hB	$�B	 �B	$�B	1'B	,B	>wB	9XB	L�B	YB	_;B	r�B	�DB	�uB	��B	�!B	�wB	��B	�B	��B	��B	��B	��B
B
PB
VB
hB
uB
�B
�B
�B
{B
oB
PB
�B
�B
�B
�B
PB
'�B
1'B
33B
49B
49B
6FB
6FB
6FB
5?B
6FB
7LB
5?B
2-B
.B
6FB
>wB
>wB
<jB
<jB
=qB
=qB
<jB
:^B
8RB
9XB
=qB
<jB
:^B
7LB
=qB
9XB
G�B
K�B
L�B
K�B
J�B
H�B
G�B
D�B
E�B
M�B
J�B
M�B
P�B
O�B
L�B
I�B
F�B
D�B
C�B
L�B
N�B
P�B
P�B
K�B
L�B
P�B
P�B
P�B
S�B
Q�B
O�B
O�B
P�B
T�B
VB
S�B
Q�B
Q�B
P�B
Q�B
R�B
VB
VB
S�B
Q�B
T�B
T�B
P�B
N�B
N�B
Q�B
Q�B
P�B
P�B
P�B
P�B
O�B
N�B
P�B
P�B
N�B
N�B
N�B
L�B
J�B
G�B
G�B
G�B
G�B
J�B
J�B
G�B
G�B
G�B
F�B
D�B
E�B
C�B
A�B
B�B
D�B
C�B
B�B
A�B
?}B
<jB
A�B
?}B
?}B
>wB
=qB
:^B
9XB
<jB
<jB
<jB
<jB
=qB
=qB
<jB
<jB
:^B
8RB
8RB
7LB
:^B
8RB
8RB
49B
0!B
33B
1'B
0!B
5?B
8RB
7LB
8RB
8RB
9XB
9XB
8RB
6FB
33B
0!B
33B
33B
2-B
2-B
33B
0!B
1'B
33B
2-B
0!B
-B
.B
-B
+B
$�B
,B
+B
+B
-B
,B
)�B
'�B
'�B
,B
)�B
%�B
(�B
)�B
-B
.B
-B
+B
(�B
)�B
+B
)�B
'�B
#�B
%�B
%�B
"�B
$�B
"�B
�B
"�B
�B
�B
 �B
 �B
�B
�B
"�B
#�B
!�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
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
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
!�B
!�B
�B
 �B
�B
 �B
 �B
!�B
"�B
�B
$�B
$�B
#�B
$�B
"�B
"�B
$�B
#�B
"�B
!�B
#�B
%�B
&�B
&�B
%�B
#�B
$�B
$�B
&�B
&�B
&�B
'�B
'�B
&�B
$�B
'�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
'�B
%�B
%�B
(�B
+B
)�B
(�B
'�B
&�B
&�B
&�B
)�B
,B
+B
)�B
-B
-B
-B
)�B
+B
)�B
+B
,B
.B
-B
-B
,B
-B
-B
.B
.B
-B
,B
-B
/B
.B
.B
.B
.B
-B
,B
,B
-B
-B
.B
/B
.B
-B
-B
)�B
/B
1'B
1'B
49B
33B
1'B
1'B
33B
5?B
49B
49B
33B
6FB
6FB
6FB
6FB
6FB
5?B
6FB
6FB
6FB
9XB
:^B
:^B
9XB
9XB
8RB
7LB
8RB
9XB
8RB
7LB
6FB
9XB
;dB
<jB
<jB
<jB
;dB
;dB
<jB
;dB
=qB
=qB
<jB
9XB
?}B
A�B
A�B
@�B
@�B
?}B
>wB
>wB
?}B
?}B
?}B
A�B
C�B
C�B
B�B
B�B
C�B
E�B
E�B
C�B
D�B
D�B
E�B
F�B
G�B
I�B
H�B
G�B
I�B
H�B
G�B
G�B
H�B
I�B
J�B
J�B
J�B
J�B
H�B
I�B
J�B
J�B
K�B
K�B
K�B
J�B
I�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
L�B
L�B
K�B
L�B
N�B
N�B
N�B
N�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
M�B
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
Q�B
R�B
Q�B
R�B
S�B
S�B
T�B
T�B
VB
VB
VB
W
B
VB
VB
T�B
W
B
W
B
W
B
VB
T�B
XB
XB
W
B
XB
ZB
ZB
YB
YB
YB
XB
W
B
W
B
ZB
ZB
ZB
ZB
YB
[#B
[#B
[#B
[#B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
]/B
]/B
]/B
^5B
^5B
]/B
\)B
[#B
\)B
\)B
\)B
[#B
[#B
YB
YB
ZB
[#B
\)B
\)B
]/B
\)B
\)B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
[#B
ZB
^5B
^5B
^5B
]/B
]/B
_;B
_;B
`BB
`BB
aHB
`BB
_;B
`BB
bNB
dZB
dZB
dZB
dZB
bNB
e`B
ffB
e`B
ffB
e`B
e`B
e`B
dZB
dZB
dZB
ffB
ffB
ffB
e`B
dZB
ffB
gmB
iyB
jB
iyB
iyB
iyB
hsB
hsB
iyB
iyB
hsB
hsB
iyB
jB
jB
l�B
n�B
n�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
o�B
p�B
p�B
p�B
o�B
p�B
p�B
q�B
q�B
r�B
q�B
q�B
q�B
q�B
p�B
p�B
q�B
q�B
r�B
q�B
q�B
r�B
r�B
q�B
o�B
q�B
q�B
q�B
r�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
s�B
s�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
r�B
u�B
v�B
v�B
v�B
v�B
v�B
u�B
s�B
s�B
v�B
v�B
u�B
u�B
t�B
t�B
t�B
u�B
v�B
x�B
x�B
w�B
x�B
y�B
y�B
y�B
y�B
x�B
y�B
y�B
y�B
y�B
{�B
{�B
{�B
z�B
z�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
|�B
{�B
|�B
|�B
|�B
}�B
}�B
|�B
~�B
}�B
|�B
}�B
{�B
}�B
}�B
|�B
~�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�B
�B
�B
�B
�B
�B
�+B
�+B
�+B
�+B
�+B
�B
�%B
�+B
�1B
�1B
�1B
�1B
�1B
�+B
�1B
�1B
�1B
�+B
�1B
�1B
�1B
�1B
�1B
�7B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�7B
�DB
�DB
�DB
�DB
�DB
�JB
�DB
�JB
�JB
�PB
�PB
�JB
�JB
�PB	G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.01 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220923090112                              AO  ARCAADJP                                                                    20220923090112    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220923090112  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20220923090112  QCF$                G�O�G�O�G�O�4800            