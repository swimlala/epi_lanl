CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-07-12T18:40:24Z creation;2019-07-12T18:40:28Z conversion to V3.1;2022-11-21T05:28:40Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        |  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  M`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �t   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20190712184024  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_179                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @�̣hN�1   @�̤)�� @<,�1&��d_�+1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @,(�@\)@��@��A�
A?�
A_�
A�
A��RA��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B�.B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D���D��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AՉ7AՉ7AՍPAՋDAՋDAՑhAՑhAՓuAՍPA���A���A�VAĸRA�?}A��RA�G�A�O�A�p�A��A�1'A�%A�n�A��-A��#A�%A�&�A��-A��9A��HA�ƨA�&�A��HA��A��7A��FA���A���A�=qA���A�|�A�;dA�bA��!A�Q�A���A��A�\)A�%A��A�jA���A��A��TA���A�\)A�G�A��A�Q�A�r�A��mA���A���A���A��A��RA� �A��yA�l�A�  A���A�bA�p�A�
=A��TA�A�5?A�S�A���A�I�A��PA���A��/A��mA���A���A�`BA��A���A��/A�jA��A��uA�7LA�A}��A|��A|^5Ay��Ax  Avn�Ar��Aq7LAo�-AooAm�mAm33AlbNAk\)AiS�Ag33AdE�Ac&�AbjAb1A_A]K�A[��AZ�jAXVAVbAT��AS��ASS�ARĜAQ�AP��APVAPI�AN��AM��AM��AM�AL�DAKG�AJ(�AJAIAIVAH5?AG�AF^5AF{AE��AEoADVAC�^AC|�AB��AA?}A@1A?C�A>ffA>I�A>�A=XA<�+A<VA;��A:�yA:M�A9dZA8��A7&�A6�A5�#A5��A4�/A4�DA2�A2bNA2jA1��A1XA0-A.E�A-�FA-�A,��A+|�A*�!A*-A)��A(�`A'�#A&��A&�uA&�+A&��A&�+A%��A$��A$ffA#��A#�A"��A"I�A!ƨA!C�A ĜA JA�A;dA%A��A=qA;dA�A%A~�AQ�AdZA"�A{A�AE�A�AXA��An�AƨA�RA��A�hA��A�A�9AbNAJAƨA
��A	�A	\)AAC�A�Az�A�A�yA1Ax�A�A�jAE�Al�A Ĝ@��
@��T@�A�@�\)@���@�@�O�@�b@�=q@�X@��@�ƨ@�K�@@웦@�M�@�9X@�/@�+@�5?@�J@��@���@��`@�7L@�K�@�M�@��@�|�@�J@�/@�r�@ӥ�@�
=@җ�@�%@�l�@��@·+@͉7@��@�(�@�"�@��@���@�Ĝ@�^5@�dZ@�$�@�7L@���@�z�@��
@��@�X@�z�@�l�@�v�@�5?@���@��-@�V@�1@�C�@��@��R@��@�?}@�I�@���@�=q@�-@��\@�n�@���@���@�bN@�b@��;@��@�@�5?@�p�@�%@���@�Q�@��@��
@��w@���@�\)@�{@�1'@�@�E�@��@���@�I�@��;@�;d@�V@��^@���@�|�@�S�@���@��#@��7@�%@��/@���@���@�A�@���@�\)@���@�-@���@��@��@��D@��@�j@�(�@��w@�@�~�@��@���@���@�/@���@�1'@�1@���@���@��\@�-@��^@�hs@�G�@�%@��u@�I�@��m@�C�@�
=@��\@�E�@�-@�@���@��@�hs@�`B@�?}@���@���@�9X@��m@��P@�K�@�~�@�$�@�{@��@���@�X@��@��u@�(�@���@��;@���@�"�@���@�E�@�@���@��-@��^@��-@��h@�?}@���@�9X@��@�ƨ@�l�@��@�o@��y@�ȴ@��!@���@�~�@�^5@�=q@�@���@���@�hs@�O�@�/@��@��@�%@��9@��u@�z�@�bN@�;@l�@+@~��@~ȴ@}V@|��@|z�@|j@|Z@{�F@{��@{C�@z�\@y�^@y7L@x�`@x�9@x �@v��@v�@v�@vȴ@v��@v{@u�@u@u�h@u?}@t�@t�D@sƨ@s�@sC�@r�@r��@q�#@qX@q%@p��@p��@pr�@pQ�@p1'@o�;@o�@o|�@o
=@n�y@n�@n�R@n��@n��@nV@n$�@m�-@mp�@m/@l�D@l(�@k�m@k��@k�@k"�@j��@j�\@j^5@j�@i��@i��@ix�@i&�@h��@h�u@hA�@g�w@g�P@g
=@fȴ@fV@e�@e�-@e?}@d��@ct�@b�@b~�@a��@a7L@`��@`r�@`A�@`b@_�P@_�@_
=@^�y@^��@^ff@^{@]�@]@]�@]`B@]?}@\��@\��@\�D@\Z@\�@[ƨ@[t�@Z��@Z�@YG�@X��@X�@X �@W�;@W�w@W+@V�R@VE�@U�h@UO�@T�@TZ@T9X@T(�@T(�@T�@T1@T1@S��@SdZ@S@R�!@R=q@Q�^@Q7L@Q�@Q%@P�`@P�`@P��@PĜ@P�9@P�u@P1'@O+@N��@Nv�@NV@M�T@Mp�@M`B@M?}@L�j@K�m@KdZ@Ko@K@J��@J��@J��@J��@J�!@J��@J�\@J~�@Jn�@J^5@J=q@I��@I��@Ix�@I�@I%@H�`@H�9@Hr�@G��@GK�@G+@G
=@F��@F��@F��@F��@F��@F��@F��@F�y@F�R@F��@F�+@E@EO�@D�@D�@D�D@D9X@Cƨ@Bn�@B�@A�^@A�7@AX@AG�@A7L@A7L@A&�@@�`@@��@@�@@bN@@ �@?�@?l�@?K�@>�y@>V@=�@=��@=��@=@=�-@=��@=�@=`B@=�@<��@<1@;�
@;t�@;C�@;33@;o@;@:�@:�H@:��@:��@:~�@:^5@:=q@9��@9�#@9��@97L@8��@8�u@8Q�@81'@8b@8  @8  @7�;@7�;@7�@7K�@6�+@6$�@5�h@4I�@4(�@41@3�m@3�
@3��@3t�@3�@3�@3C�@3o@2�@2�\@2~�@2M�@2=q@1��@1�7@1X@1%@0��@0��@0�@0 �@/��@/K�@.�y@.v�@.V@.5?@.$�@.@-�@-�T@-�-@-�-@-�h@-�@-p�@-p�@-`B@-`B@-?}@-/@-V@,�@,��@,z�@,I�@+�F@+dZ@+"�@+@*��@*��@*~�@*=q@)�#@)�^@)��@)x�@)hs@)&�@(�9@(r�@(Q�@( �@'��@'�@&��@&�y@&��@&E�@%�T@%p�@%?}@%�@$�@#�F@#S�@#@"�!@"��@"�\@"n�@!�#@!��@!X@!%@ ��@ r�@ Q�@ b@�P@��@ȴ@�R@��@5?@5?@@�-@�@O�@��@�/@��@Z@��@�
@��@dZ@33@��@�!@^5@��@�^@�7@�7@x�@G�@G�@%@�`@��@Ĝ@��@bN@ �@b@b@b@�;@K�@;d@;d@+@�@�y@�+@E�@�T@��@@�-@�-@��@p�@`B@`B@O�@O�@?}@V@�/@�D@j@1@ƨ@�@S�@o@�H@��@�!@~�@n�@M�@�@J@��@��@��@��@x�@G�@7L@��@�`@Ĝ@Ĝ@�u@r�@bN@A�@1'@�@|�@\)@+@
=@�y@ȴ@�R@��@��@��@5?@��@�-@�@V@��@��@�D@�D@Z@9X@(�@�m@��@dZ@"�@@
�@
��@
��@
n�@
-@
J@	��@	�@	��@	��@	x�@	X@	&�@	%@��@��@��@�`@�9@��@�u@�@r�@ �@�@�;@��@��@��@��@|�@\)@\)@\)@\)@;d@�@
=@��@�y@�y@�@�R@��@ff@E�@5?@$�@@�@�T@@��@O�@?}@/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AՉ7AՉ7AՍPAՋDAՋDAՑhAՑhAՓuAՍPA���A���A�VAĸRA�?}A��RA�G�A�O�A�p�A��A�1'A�%A�n�A��-A��#A�%A�&�A��-A��9A��HA�ƨA�&�A��HA��A��7A��FA���A���A�=qA���A�|�A�;dA�bA��!A�Q�A���A��A�\)A�%A��A�jA���A��A��TA���A�\)A�G�A��A�Q�A�r�A��mA���A���A���A��A��RA� �A��yA�l�A�  A���A�bA�p�A�
=A��TA�A�5?A�S�A���A�I�A��PA���A��/A��mA���A���A�`BA��A���A��/A�jA��A��uA�7LA�A}��A|��A|^5Ay��Ax  Avn�Ar��Aq7LAo�-AooAm�mAm33AlbNAk\)AiS�Ag33AdE�Ac&�AbjAb1A_A]K�A[��AZ�jAXVAVbAT��AS��ASS�ARĜAQ�AP��APVAPI�AN��AM��AM��AM�AL�DAKG�AJ(�AJAIAIVAH5?AG�AF^5AF{AE��AEoADVAC�^AC|�AB��AA?}A@1A?C�A>ffA>I�A>�A=XA<�+A<VA;��A:�yA:M�A9dZA8��A7&�A6�A5�#A5��A4�/A4�DA2�A2bNA2jA1��A1XA0-A.E�A-�FA-�A,��A+|�A*�!A*-A)��A(�`A'�#A&��A&�uA&�+A&��A&�+A%��A$��A$ffA#��A#�A"��A"I�A!ƨA!C�A ĜA JA�A;dA%A��A=qA;dA�A%A~�AQ�AdZA"�A{A�AE�A�AXA��An�AƨA�RA��A�hA��A�A�9AbNAJAƨA
��A	�A	\)AAC�A�Az�A�A�yA1Ax�A�A�jAE�Al�A Ĝ@��
@��T@�A�@�\)@���@�@�O�@�b@�=q@�X@��@�ƨ@�K�@@웦@�M�@�9X@�/@�+@�5?@�J@��@���@��`@�7L@�K�@�M�@��@�|�@�J@�/@�r�@ӥ�@�
=@җ�@�%@�l�@��@·+@͉7@��@�(�@�"�@��@���@�Ĝ@�^5@�dZ@�$�@�7L@���@�z�@��
@��@�X@�z�@�l�@�v�@�5?@���@��-@�V@�1@�C�@��@��R@��@�?}@�I�@���@�=q@�-@��\@�n�@���@���@�bN@�b@��;@��@�@�5?@�p�@�%@���@�Q�@��@��
@��w@���@�\)@�{@�1'@�@�E�@��@���@�I�@��;@�;d@�V@��^@���@�|�@�S�@���@��#@��7@�%@��/@���@���@�A�@���@�\)@���@�-@���@��@��@��D@��@�j@�(�@��w@�@�~�@��@���@���@�/@���@�1'@�1@���@���@��\@�-@��^@�hs@�G�@�%@��u@�I�@��m@�C�@�
=@��\@�E�@�-@�@���@��@�hs@�`B@�?}@���@���@�9X@��m@��P@�K�@�~�@�$�@�{@��@���@�X@��@��u@�(�@���@��;@���@�"�@���@�E�@�@���@��-@��^@��-@��h@�?}@���@�9X@��@�ƨ@�l�@��@�o@��y@�ȴ@��!@���@�~�@�^5@�=q@�@���@���@�hs@�O�@�/@��@��@�%@��9@��u@�z�@�bN@�;@l�@+@~��@~ȴ@}V@|��@|z�@|j@|Z@{�F@{��@{C�@z�\@y�^@y7L@x�`@x�9@x �@v��@v�@v�@vȴ@v��@v{@u�@u@u�h@u?}@t�@t�D@sƨ@s�@sC�@r�@r��@q�#@qX@q%@p��@p��@pr�@pQ�@p1'@o�;@o�@o|�@o
=@n�y@n�@n�R@n��@n��@nV@n$�@m�-@mp�@m/@l�D@l(�@k�m@k��@k�@k"�@j��@j�\@j^5@j�@i��@i��@ix�@i&�@h��@h�u@hA�@g�w@g�P@g
=@fȴ@fV@e�@e�-@e?}@d��@ct�@b�@b~�@a��@a7L@`��@`r�@`A�@`b@_�P@_�@_
=@^�y@^��@^ff@^{@]�@]@]�@]`B@]?}@\��@\��@\�D@\Z@\�@[ƨ@[t�@Z��@Z�@YG�@X��@X�@X �@W�;@W�w@W+@V�R@VE�@U�h@UO�@T�@TZ@T9X@T(�@T(�@T�@T1@T1@S��@SdZ@S@R�!@R=q@Q�^@Q7L@Q�@Q%@P�`@P�`@P��@PĜ@P�9@P�u@P1'@O+@N��@Nv�@NV@M�T@Mp�@M`B@M?}@L�j@K�m@KdZ@Ko@K@J��@J��@J��@J��@J�!@J��@J�\@J~�@Jn�@J^5@J=q@I��@I��@Ix�@I�@I%@H�`@H�9@Hr�@G��@GK�@G+@G
=@F��@F��@F��@F��@F��@F��@F��@F�y@F�R@F��@F�+@E@EO�@D�@D�@D�D@D9X@Cƨ@Bn�@B�@A�^@A�7@AX@AG�@A7L@A7L@A&�@@�`@@��@@�@@bN@@ �@?�@?l�@?K�@>�y@>V@=�@=��@=��@=@=�-@=��@=�@=`B@=�@<��@<1@;�
@;t�@;C�@;33@;o@;@:�@:�H@:��@:��@:~�@:^5@:=q@9��@9�#@9��@97L@8��@8�u@8Q�@81'@8b@8  @8  @7�;@7�;@7�@7K�@6�+@6$�@5�h@4I�@4(�@41@3�m@3�
@3��@3t�@3�@3�@3C�@3o@2�@2�\@2~�@2M�@2=q@1��@1�7@1X@1%@0��@0��@0�@0 �@/��@/K�@.�y@.v�@.V@.5?@.$�@.@-�@-�T@-�-@-�-@-�h@-�@-p�@-p�@-`B@-`B@-?}@-/@-V@,�@,��@,z�@,I�@+�F@+dZ@+"�@+@*��@*��@*~�@*=q@)�#@)�^@)��@)x�@)hs@)&�@(�9@(r�@(Q�@( �@'��@'�@&��@&�y@&��@&E�@%�T@%p�@%?}@%�@$�@#�F@#S�@#@"�!@"��@"�\@"n�@!�#@!��@!X@!%@ ��@ r�@ Q�@ b@�P@��@ȴ@�R@��@5?@5?@@�-@�@O�@��@�/@��@Z@��@�
@��@dZ@33@��@�!@^5@��@�^@�7@�7@x�@G�@G�@%@�`@��@Ĝ@��@bN@ �@b@b@b@�;@K�@;d@;d@+@�@�y@�+@E�@�T@��@@�-@�-@��@p�@`B@`B@O�@O�@?}@V@�/@�D@j@1@ƨ@�@S�@o@�H@��@�!@~�@n�@M�@�@J@��@��@��@��@x�@G�@7L@��@�`@Ĝ@Ĝ@�u@r�@bN@A�@1'@�@|�@\)@+@
=@�y@ȴ@�R@��@��@��@5?@��@�-@�@V@��@��@�D@�D@Z@9X@(�@�m@��@dZ@"�@@
�@
��@
��@
n�@
-@
J@	��@	�@	��@	��@	x�@	X@	&�@	%@��@��@��@�`@�9@��@�u@�@r�@ �@�@�;@��@��@��@��@|�@\)@\)@\)@\)@;d@�@
=@��@�y@�y@�@�R@��@ff@E�@5?@$�@@�@�T@@��@O�@?}@/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BȴBȴBȴBȴBȴBɺBɺBȴBǮB�B�B��B��B�-B�B�HB�B��B��BɺBƨB��B��B�fB�NB�)B�B��B��BɺBƨBĜBB�dB�?B�B��B��B��B�VB�JB�DB�=B�7B�B|�Bt�Bq�Bo�Bl�BcTB]/B[#BW
BS�BQ�BK�B@�B0!B#�BPB��B�B�TB�HB�)B�B��B��BƨB�jB�B��B��B��B�oB� BbNBK�B?}B33BuB  B
�B
�/B
ǮB
��B
�^B
�B
��B
��B
��B
�\B
{�B
l�B
dZB
]/B
K�B
=qB
.B
{B
1B	��B	��B	�B	�sB	�HB	�B	��B	�jB	��B	��B	��B	�{B	�B	r�B	iyB	`BB	R�B	D�B	<jB	7LB	49B	2-B	1'B	2-B	2-B	49B	/B	-B	2-B	9XB	8RB	2-B	)�B	(�B	&�B	"�B	�B	�B	{B	uB	hB	VB	
=B	+B	
=B	%B��B��B�B�B�B�B�mB�sB�B�B�yB�sB�B�sB�ZB�HB�BB�;B�)B�B��BǮBɺBŢB�}B�^B�9B�-B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�\B�VB�DB�7B�%B�B�B�B~�B}�B{�Bz�By�Bv�Br�Bo�Bm�BiyBcTB`BB^5B[#BYBXBW
BXBW
BR�BN�BL�BJ�BG�BE�BD�BC�BA�B@�B>wB=qB>wB;dB<jB<jB9XB7LB6FB49B33B2-B0!B0!B/B-B)�B)�B'�B&�B%�B&�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B �B!�B$�B&�B$�B �B�B�B�B�B�B�B �B �B#�B'�B+B)�B)�B(�B(�B)�B,B,B-B1'B0!B1'B5?B7LB;dBA�BB�BC�BE�BG�BG�BG�BH�BH�BK�BL�BO�BP�BQ�BQ�BS�BZBZBYBZBaHBdZBe`Be`BffBffBgmBiyBl�Bn�Bq�Bw�Bw�Bz�B�B�B�%B�+B�+B�+B�7B�JB�\B�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�9B�?B�LB�dB�wB�}B��B��B��BÖBŢBƨBȴB��B��B��B��B��B��B�B�
B�B�B�B�B�#B�;B�TB�fB�yB�B�B�B�B��B��B��B	  B	B	B	B	%B	+B	1B		7B	DB	VB	\B	bB	hB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	$�B	&�B	'�B	+B	,B	/B	1'B	2-B	49B	5?B	49B	5?B	7LB	8RB	8RB	9XB	>wB	@�B	A�B	A�B	A�B	D�B	F�B	G�B	H�B	I�B	N�B	P�B	T�B	YB	]/B	`BB	aHB	bNB	dZB	iyB	jB	jB	jB	k�B	n�B	o�B	p�B	q�B	r�B	s�B	u�B	x�B	y�B	z�B	{�B	}�B	�B	�B	�B	�%B	�+B	�1B	�7B	�=B	�JB	�PB	�VB	�hB	�hB	�oB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�-B	�9B	�?B	�FB	�LB	�XB	�qB	�}B	��B	��B	ĜB	ŢB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�/B	�5B	�;B	�;B	�;B	�HB	�NB	�NB	�ZB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
JB
PB
PB
PB
PB
PB
\B
oB
oB
oB
oB
oB
oB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
"�B
#�B
$�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
33B
49B
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
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
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
XB
XB
XB
W
B
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
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
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
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
e`B
e`B
ffB
ffB
ffB
gmB
gmB
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
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
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
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BȴBȴBȴBȴB��B�	B��B�JB�FB{B�B�QB�B��BܒB��B�B�mB�aB�B�KBϑB��B�$B�B�VB��B՛B�B�B�zB��BĶB��B��B�UB��B��B��B��B��B�0B�DB��B��B~]Bu�BraBp�Bn/BdZB]�B[�BW�BT{BR�BMjBBuB1�B&�B�B��B��B�tB�hB��B�7B��B��B��B��B��B�sB��B�bB�B��BeFBM�BA�B7�BB�B
� B
��B
ȚB
��B
�6B
�OB
�B
��B
��B
�B
~B
m�B
e�B
`B
NB
?�B
1�B
�B
	�B	��B	�B	�B	�B	�B	��B	ΊB	�cB	�eB	��B	��B	�?B	��B	t�B	k�B	c B	U�B	FYB	=�B	7�B	5?B	3�B	2-B	2�B	2�B	5�B	0UB	-]B	2�B	:�B	9�B	3MB	*KB	)�B	'�B	$B	B	yB	�B	,B	:B	BB	B	�B	DB	1B�qB��B�B��B�B�B�B��B�wB�B�B��B��B�0B�zB��B��B�BB��BٴB�~B�BʦBƨB�;B�jB�%B��B�-B��B��B��B��B� B�B��B��B��B��B�B��B��B�YB�2B�uB��B�B�B�	B��B�B��B�oB}B~�B|�B|jB{�Bw�Bs�BpoBoOBlBd�BabB_�B[�BY�BX�BW�BY1BX_BS�BO�BM�BLJBIBF?BE9BDMBB�BA�B?�B?B?cB;�B=qB=qB:�B8lB7B4�B3�B3B1[B1AB0UB.cB+B*�B(�B'�B'B)�B!�B �B \BVBOBxBB=B7BB�B�B�B�BB�B�B�BSBgB�B�BQB=B=B=B=B�B�BB)BdB;B"�B!�B"�B%FB(
B&�B"�B �B vB BB!BpB�B!bB!�B$�B(�B+6B*KB*eB)�B)�B*�B,qB,�B-�B1�B0�B2B5�B7�B;dBA�BCBDMBFBG�BG�BHBI7BIlBLJBM6BP.BQ4BR BR:BT,BZQBZ�BZ7B[qBbBd�Be�BfBf�Bf�Bh
BjKBm)BoiBraBxBxlB{B�[B��B�YB�EB�zB��B��B��B��B��B��B�
B��B��B��B��B�B�!B�bB�FB�>B�KB�WB�wB��B�|B�nB��B��B��B��B��B��B��B��B��B��B�B�7B�B�<B�B�B�&B�FB�9B�$B�+B�EB�eB�kBۦBߊB�B��B�B��B��B��B�B�B�B�JB	 OB	MB	SB	mB	�B	�B	�B		�B	^B	VB	vB	}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	%B	'B	($B	+6B	,"B	/OB	1AB	2GB	4TB	5ZB	4nB	5tB	7fB	8lB	8�B	9�B	>�B	@�B	A�B	A�B	BB	D�B	F�B	G�B	H�B	J	B	N�B	QB	UMB	YeB	]dB	`vB	a|B	b�B	d�B	iyB	j�B	j�B	j�B	k�B	n�B	o�B	p�B	q�B	r�B	s�B	v+B	x�B	zB	{B	|B	~BB	�;B	�-B	�9B	�?B	�EB	�KB	�lB	�XB	�dB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�
B	�B	�B	�6B	�=B	�IB	�5B	�UB	�GB	�GB	�nB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	� B	� B	�B	�B	�&B	�B	�,B	�2B	�MB	�yB	�kB	�]B	�IB	�jB	�VB	�VB	�pB	�|B	�B	�B	�tB	�B	�B	�B	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	�B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�$B	�B	��B	��B	�B	�6B	�(B	�B
 B
 B
 B
 B
B
B
B
B
B
 B
 B
 B
AB
'B
-B
-B
3B
MB
9B
SB
mB
EB
KB
	RB
	RB

=B

#B

=B

=B

=B

=B

rB

XB
xB
^B
�B
�B
jB
�B
�B
�B
�B
�B
�B
�B
oB
�B
oB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
"�B
#�B
$�B
$�B
%�B
'B
'B
&�B
'�B
(
B
(
B
'�B
(>B
(>B
)*B
)DB
*KB
,"B
,"B
,"B
,"B
,"B
-CB
-B
-B
-)B
-)B
./B
.IB
/OB
/5B
/5B
/OB
0;B
0;B
0!B
1AB
1AB
1[B
2GB
2aB
3MB
4nB
5ZB
5ZB
6`B
6FB
6`B
6FB
6zB
6`B
72B
7fB
7fB
7LB
7LB
7LB
72B
7fB
7�B
8lB
8lB
8lB
8RB
9�B
9�B
:xB
:xB
;dB
;B
;B
;B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
?�B
?�B
?�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
MB
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
PB
O�B
Q B
Q B
RB
RB
RB
S&B
S&B
T,B
TB
TB
UB
UB
T�B
UB
UB
VB
VB
VB
V9B
W$B
W$B
W$B
W$B
XB
XB
XB
W$B
XEB
YB
YB
Y1B
Y1B
YKB
YKB
ZQB
Z7B
[#B
[#B
[#B
[#B
[=B
[=B
[#B
[#B
[=B
[#B
[=B
[=B
\CB
\CB
\CB
]IB
]IB
]dB
^OB
^OB
_;B
_!B
_VB
_pB
_pB
`\B
`\B
`BB
`BB
`BB
`\B
`\B
abB
abB
abB
abB
bNB
bhB
bNB
bhB
bhB
bNB
bhB
cnB
cnB
c�B
dtB
dtB
dtB
dtB
dtB
e`B
e`B
e`B
ezB
e�B
ezB
f�B
f�B
f�B
g�B
gmB
h�B
hsB
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
j�B
j�B
k�B
k�B
kkB
k�B
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
m�B
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
p�B
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
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<I��<�YK<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201907230032142019072300321420190723003214202211182139402022111821394020221118213940201907240016482019072400164820190724001648  JA  ARFMdecpA19c                                                                20190713033909  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190712184024  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190712184026  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190712184026  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190712184027  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190712184027  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190712184027  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190712184027  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190712184027  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190712184028                      G�O�G�O�G�O�                JA  ARUP                                                                        20190712185652                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190713153220  CV  JULD            G�O�G�O�F�e                JM  ARCAJMQC2.0                                                                 20190722153214  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190722153214  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190723151648  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123940  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                