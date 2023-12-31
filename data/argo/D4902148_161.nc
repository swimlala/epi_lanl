CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-01-13T18:37:51Z creation;2019-01-13T18:37:55Z conversion to V3.1;2019-12-18T07:17:39Z update;2022-11-21T05:29:33Z update;     
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
resolution        =���   axis      Z        |  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  M|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pT   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20190113183751  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_161                     2C  DdsNAVIS_A                         0397                            ARGO 011514                     863 @؟��`�1   @؟�s�� @;�n��O��ds�PH1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ D�|�D�� D�3D�C3Dۀ D�� D���D�<�D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@�{@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�.B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DM�DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D���D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D�|{Dڿ�D��D�B�D��Dۿ�D��{D�<{D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111LA� �A�"�A��A��A���A��hA�XA�p�A���A�ƨA�ĜA�ĜA�A��wA��RA��-A��9A��A���A���A��\A��DA��+A�|�A�x�A�n�A�ffA�ZA�M�A�=qA�A�A�;dA�7LA�(�A� �A� �A�{A�1A�
=A�1A�A���A��A�ƨA��hA�1A��\A�VA�=qA�A��
A�A�ȴA��\A�A�A�(�A�A�l�A��A��A��mA��A�  A��FA�  A���A�&�A���A�
=A�?}A���A���A�n�A�7LA�x�A��A��A��A�A%A~��A~1A}
=A{�mAz��Ay
=AuƨAo�AlȴAk|�AjffAi��Ail�Ah�/AhA�AfĜAeS�Ad1'Acx�Ab�+Aa�mAa��A`�uA_�A^5?A]ƨA]"�AZVAX�AX1AWp�AWhsAWO�AW;dAW�AV��AVv�AV1'AU�
ATĜAQƨAQ;dAP�RAO�AOx�AO�AN��ANA�AM�AMXAK�hAI�-AFffAE�AC�AB�HAB�AB��AB�\AB^5ABE�AB=qAB(�AB1AA�-AA�AA��A@�A?S�A>�/A>�9A>~�A=��A;��A:r�A9�A9x�A8�9A7/A5�-A5dZA5G�A4��A2�uA1G�A/x�A.��A.�RA.5?A-�7A-33A,��A,Q�A+�A+�
A+�^A+�A+x�A+7LA+oA(��A$�\A#�#A#�A"�A"$�A!A ȴA`BA�A�AO�AoA��A^5A9XA5?A��A7LA�yA�/A��A�A\)A�A�A��A^5A��Ar�AƨA�PAhsA�AI�A�-A?}A�9A^5A�AAp�A�A�9A�RAt�A
�/A
�A	��A��A$�A/A��AZA�yA�TA�PA �A (�@��@��#@��
@�^5@���@��@�K�@�@�/@�F@�\)@��H@�J@��`@��m@ꟾ@��@�@�o@���@�l�@�X@��/@߮@�C�@ج@�^5@�&�@�  @�|�@��H@��T@�z�@��@�@���@�p�@�O�@�/@��@���@��`@���@̴9@�r�@���@�l�@ʟ�@�/@�=q@��@��`@��@��@���@��7@���@�|�@�S�@�;d@�33@��\@�@���@�x�@��@���@��`@��j@�I�@��m@���@���@���@���@��7@��@�hs@�b@�
=@���@��!@�v�@��@��@���@��9@��@�j@���@�"�@���@�@�A�@���@�o@���@�v�@���@��@���@�r�@��w@�~�@��@�p�@��@��D@�K�@�n�@���@��F@���@���@��@�j@�  @�ƨ@�t�@�@�^5@��@��-@��@�X@���@�1'@�  @��w@��@���@���@��P@��@�dZ@�
=@��!@�v�@���@��h@��@���@�Z@��@�dZ@��#@���@�X@��@�V@���@� �@��P@��\@�-@�$�@�5?@�5?@�-@��-@�1'@��@��R@��@��@���@��@�V@���@��@���@�|�@�  @� �@�1@��;@���@�K�@�;d@�;d@�;d@�+@��y@�n�@�5?@�{@��^@��@�Q�@��w@��@�~�@�M�@�=q@�-@��@���@��@��#@���@��9@~��@~ȴ@}@|z�@zn�@y�7@y&�@xĜ@x�@w�;@vff@u�-@t�@tz�@t(�@s��@s�m@sƨ@s�@st�@sS�@s33@so@r�@r��@r��@r�!@r�\@rM�@rJ@q�^@q��@q&�@p��@p�u@pA�@o\)@m�T@m�-@m�@mp�@mO�@m?}@l�@l�j@k��@k@j~�@i�@i�^@ihs@h��@hĜ@hQ�@h �@g��@f�@e�@eV@d��@d�D@c�F@bJ@a�^@a�7@ahs@aX@aX@aX@aG�@a�@`��@`�9@`�9@`��@`��@`�u@`bN@`A�@_�@_�;@_�@_��@_��@_�P@_|�@_l�@_l�@_l�@_\)@_K�@_K�@_;d@_+@_+@_�@_
=@^��@^E�@^{@]�T@]��@]@]��@]O�@]V@\��@\�@\�@\�/@\��@\z�@\j@\j@\�@[�m@[ƨ@[t�@[t�@[C�@[o@[@Z�@Z�H@Z�H@Z�H@Z�H@Z��@Z^5@Z-@ZJ@Y��@Y�7@XĜ@XQ�@W��@V�R@U�-@U�@T�/@S��@S��@S��@St�@SS�@S33@R�H@R�\@Q��@Q�@P��@P��@Pr�@P �@O�w@Ol�@OK�@O;d@N�y@Nȴ@Nȴ@N��@Nff@M��@L�@L�j@L�j@L�@LZ@L1@Kƨ@K��@Kt�@KdZ@J��@I�@Ihs@H��@H��@H  @Gl�@G;d@G
=@F��@F�y@F�R@FV@F$�@E�T@E��@E?}@EV@D�@Dz�@D(�@C��@Cƨ@C�@Ct�@CC�@B�!@Bn�@B-@B-@A��@A��@A��@A��@A�7@Ax�@Ahs@Ahs@AX@AX@AG�@A7L@A%@@��@@Ĝ@@ �@?��@?\)@>�y@>v�@>5?@=��@=p�@=�@<�@<��@<�j@<�D@<z�@<1@;��@:�H@:M�@:=q@:-@9�^@8�`@8�@8�@8bN@8Q�@8Q�@8Q�@8A�@8A�@8A�@81'@8 �@8 �@8 �@8 �@81'@81'@81'@8A�@81'@8 �@7�@6��@6�+@6@5��@5/@5V@4��@4�@4�/@4�@4��@4�D@41@3ƨ@3��@3t�@3S�@3"�@2�!@2=q@1�#@1��@1�^@1��@1�7@1&�@0��@0�9@0��@0��@0�u@0�@0r�@0bN@0A�@0A�@01'@01'@0b@/�@/�@/��@/|�@/;d@.�y@.��@.��@.�+@.v�@.ff@.V@.E�@.5?@.{@.@-�T@-@-�-@-�h@-�h@-�@-�@-`B@-�@,9X@+��@+S�@*�@*M�@)�^@)��@)X@)7L@)�@( �@'�P@'+@&��@&ȴ@&��@&�+@%�T@%�@$�/@$�j@#��@#"�@"�!@"�\@"n�@"-@!��@!&�@ �9@ �u@ �@ �@ �@ �@ r�@ bN@ bN@ Q�@ A�@ 1'@ 1'@  �@ b@��@�P@l�@;d@+@
=@��@�@�R@v�@E�@{@��@�-@�-@�@V@�/@��@��@��@�@�D@z�@j@I�@�
@��@��@"�@��@��@M�@J@��@�7@hs@7L@%@Ĝ@r�@bN@Q�@1'@1'@�@�;@��@��@\)@+@+@+@�@
=@
=@ȴ@��@��@v�@ff@V@E�@5?@$�@$�@{@@p�@O�@?}@�@V@�/@�D@I�@��@ƨ@��@��@S�@�@�!@~�@-@��@�@��@��@r�@A�@ �@  @�@��@\)@;d@+@�@
=@
=@��@�@ȴ@ȴ@�R@�+@ff@V@5?@�T@@��@p�@O�@�@��@��@��@��@��@��@��@��@�D@j@Z@Z@Z@Z@9X@1@�F@��@t�@
�@
��@
�\@
-@	�@�@Q�@ �@�@�w@�P@�@��@�y@�@ȴ@��@��@��@�+@v�@ff@ff@V@E�@5?@5?@5?@$�@$�@@�@@�-@p�@��@�@��@�D@�D@Z@(�@�
@�F@�F@�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111LA� �A�"�A��A��A���A��hA�XA�p�A���A�ƨA�ĜA�ĜA�A��wA��RA��-A��9A��A���A���A��\A��DA��+A�|�A�x�A�n�A�ffA�ZA�M�A�=qA�A�A�;dA�7LA�(�A� �A� �A�{A�1A�
=A�1A�A���A��A�ƨA��hA�1A��\A�VA�=qA�A��
A�A�ȴA��\A�A�A�(�A�A�l�A��A��A��mA��A�  A��FA�  A���A�&�A���A�
=A�?}A���A���A�n�A�7LA�x�A��A��A��A�A%A~��A~1A}
=A{�mAz��Ay
=AuƨAo�AlȴAk|�AjffAi��Ail�Ah�/AhA�AfĜAeS�Ad1'Acx�Ab�+Aa�mAa��A`�uA_�A^5?A]ƨA]"�AZVAX�AX1AWp�AWhsAWO�AW;dAW�AV��AVv�AV1'AU�
ATĜAQƨAQ;dAP�RAO�AOx�AO�AN��ANA�AM�AMXAK�hAI�-AFffAE�AC�AB�HAB�AB��AB�\AB^5ABE�AB=qAB(�AB1AA�-AA�AA��A@�A?S�A>�/A>�9A>~�A=��A;��A:r�A9�A9x�A8�9A7/A5�-A5dZA5G�A4��A2�uA1G�A/x�A.��A.�RA.5?A-�7A-33A,��A,Q�A+�A+�
A+�^A+�A+x�A+7LA+oA(��A$�\A#�#A#�A"�A"$�A!A ȴA`BA�A�AO�AoA��A^5A9XA5?A��A7LA�yA�/A��A�A\)A�A�A��A^5A��Ar�AƨA�PAhsA�AI�A�-A?}A�9A^5A�AAp�A�A�9A�RAt�A
�/A
�A	��A��A$�A/A��AZA�yA�TA�PA �A (�@��@��#@��
@�^5@���@��@�K�@�@�/@�F@�\)@��H@�J@��`@��m@ꟾ@��@�@�o@���@�l�@�X@��/@߮@�C�@ج@�^5@�&�@�  @�|�@��H@��T@�z�@��@�@���@�p�@�O�@�/@��@���@��`@���@̴9@�r�@���@�l�@ʟ�@�/@�=q@��@��`@��@��@���@��7@���@�|�@�S�@�;d@�33@��\@�@���@�x�@��@���@��`@��j@�I�@��m@���@���@���@���@��7@��@�hs@�b@�
=@���@��!@�v�@��@��@���@��9@��@�j@���@�"�@���@�@�A�@���@�o@���@�v�@���@��@���@�r�@��w@�~�@��@�p�@��@��D@�K�@�n�@���@��F@���@���@��@�j@�  @�ƨ@�t�@�@�^5@��@��-@��@�X@���@�1'@�  @��w@��@���@���@��P@��@�dZ@�
=@��!@�v�@���@��h@��@���@�Z@��@�dZ@��#@���@�X@��@�V@���@� �@��P@��\@�-@�$�@�5?@�5?@�-@��-@�1'@��@��R@��@��@���@��@�V@���@��@���@�|�@�  @� �@�1@��;@���@�K�@�;d@�;d@�;d@�+@��y@�n�@�5?@�{@��^@��@�Q�@��w@��@�~�@�M�@�=q@�-@��@���@��@��#@���@��9@~��@~ȴ@}@|z�@zn�@y�7@y&�@xĜ@x�@w�;@vff@u�-@t�@tz�@t(�@s��@s�m@sƨ@s�@st�@sS�@s33@so@r�@r��@r��@r�!@r�\@rM�@rJ@q�^@q��@q&�@p��@p�u@pA�@o\)@m�T@m�-@m�@mp�@mO�@m?}@l�@l�j@k��@k@j~�@i�@i�^@ihs@h��@hĜ@hQ�@h �@g��@f�@e�@eV@d��@d�D@c�F@bJ@a�^@a�7@ahs@aX@aX@aX@aG�@a�@`��@`�9@`�9@`��@`��@`�u@`bN@`A�@_�@_�;@_�@_��@_��@_�P@_|�@_l�@_l�@_l�@_\)@_K�@_K�@_;d@_+@_+@_�@_
=@^��@^E�@^{@]�T@]��@]@]��@]O�@]V@\��@\�@\�@\�/@\��@\z�@\j@\j@\�@[�m@[ƨ@[t�@[t�@[C�@[o@[@Z�@Z�H@Z�H@Z�H@Z�H@Z��@Z^5@Z-@ZJ@Y��@Y�7@XĜ@XQ�@W��@V�R@U�-@U�@T�/@S��@S��@S��@St�@SS�@S33@R�H@R�\@Q��@Q�@P��@P��@Pr�@P �@O�w@Ol�@OK�@O;d@N�y@Nȴ@Nȴ@N��@Nff@M��@L�@L�j@L�j@L�@LZ@L1@Kƨ@K��@Kt�@KdZ@J��@I�@Ihs@H��@H��@H  @Gl�@G;d@G
=@F��@F�y@F�R@FV@F$�@E�T@E��@E?}@EV@D�@Dz�@D(�@C��@Cƨ@C�@Ct�@CC�@B�!@Bn�@B-@B-@A��@A��@A��@A��@A�7@Ax�@Ahs@Ahs@AX@AX@AG�@A7L@A%@@��@@Ĝ@@ �@?��@?\)@>�y@>v�@>5?@=��@=p�@=�@<�@<��@<�j@<�D@<z�@<1@;��@:�H@:M�@:=q@:-@9�^@8�`@8�@8�@8bN@8Q�@8Q�@8Q�@8A�@8A�@8A�@81'@8 �@8 �@8 �@8 �@81'@81'@81'@8A�@81'@8 �@7�@6��@6�+@6@5��@5/@5V@4��@4�@4�/@4�@4��@4�D@41@3ƨ@3��@3t�@3S�@3"�@2�!@2=q@1�#@1��@1�^@1��@1�7@1&�@0��@0�9@0��@0��@0�u@0�@0r�@0bN@0A�@0A�@01'@01'@0b@/�@/�@/��@/|�@/;d@.�y@.��@.��@.�+@.v�@.ff@.V@.E�@.5?@.{@.@-�T@-@-�-@-�h@-�h@-�@-�@-`B@-�@,9X@+��@+S�@*�@*M�@)�^@)��@)X@)7L@)�@( �@'�P@'+@&��@&ȴ@&��@&�+@%�T@%�@$�/@$�j@#��@#"�@"�!@"�\@"n�@"-@!��@!&�@ �9@ �u@ �@ �@ �@ �@ r�@ bN@ bN@ Q�@ A�@ 1'@ 1'@  �@ b@��@�P@l�@;d@+@
=@��@�@�R@v�@E�@{@��@�-@�-@�@V@�/@��@��@��@�@�D@z�@j@I�@�
@��@��@"�@��@��@M�@J@��@�7@hs@7L@%@Ĝ@r�@bN@Q�@1'@1'@�@�;@��@��@\)@+@+@+@�@
=@
=@ȴ@��@��@v�@ff@V@E�@5?@$�@$�@{@@p�@O�@?}@�@V@�/@�D@I�@��@ƨ@��@��@S�@�@�!@~�@-@��@�@��@��@r�@A�@ �@  @�@��@\)@;d@+@�@
=@
=@��@�@ȴ@ȴ@�R@�+@ff@V@5?@�T@@��@p�@O�@�@��@��@��@��@��@��@��@��@�D@j@Z@Z@Z@Z@9X@1@�F@��@t�@
�@
��@
�\@
-@	�@�@Q�@ �@�@�w@�P@�@��@�y@�@ȴ@��@��@��@�+@v�@ff@ff@V@E�@5?@5?@5?@$�@$�@@�@@�-@p�@��@�@��@�D@�D@Z@(�@�
@�F@�F@�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�BBBBB��BÖBÖB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�
B�
B�
B�
B�B�B�5B�mB�B�B�B�B�B�B�B�B��B��B��B��B��B�B��B��B��B��B��Bu�B@�BbB�ZB�;B�/B�#B�BɺB�B��Bq�BcTB`BBjBv�Bw�Br�BW
BG�BB�B0!BB
�yB
�jB
��B
�uB
�7B
�B
� B
|�B
v�B
n�B
e`B
[#B
K�B
0!B
B	�B	�fB	�;B	�B	�
B	��B	��B	ÖB	�XB	�9B	�B	��B	��B	��B	��B	�VB	�%B	�B	|�B	q�B	l�B	k�B	l�B	k�B	l�B	l�B	m�B	m�B	m�B	m�B	k�B	gmB	[#B	W
B	R�B	N�B	L�B	K�B	J�B	H�B	E�B	A�B	<jB	49B	)�B	#�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	{B	{B	oB	PB	1B	%B	B	B��B��B�B�B�B�B�B�fB�TB�NB�#B��B��B�^B�XB�RB�dB�dB�XB�FB�9B�3B�3B�3B�3B�3B�-B�B��B��B��B�oB�bB�VB�JB�1B�B� B~�B}�B|�B{�Bz�Bz�By�Bx�Bw�Bv�Bu�Bt�Bq�Bp�Bm�BjBiyBgmBe`BbNB`BB_;B^5B]/B[#BZBXBW
BVBT�BS�BR�BQ�BN�BJ�BG�BE�BC�BA�B>wB:^B7LB6FB5?B2-B1'B0!B/B-B,B+B(�B&�B$�B#�B#�B"�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B�B �B'�B'�B(�B'�B(�B)�B(�B(�B(�B(�B(�B)�B)�B)�B,B/B/B/B/B/B/B2-B5?B5?B6FB7LB9XB>wB?}B?}B?}BA�BB�BD�BF�BG�BJ�BN�BQ�BR�BS�BW
BZB[#B\)B`BBffBhsBjBk�Bl�Bo�Br�Bu�Bv�Bv�Bu�Bu�Bu�Bu�Bv�Bw�Bx�Bz�B|�B}�B~�B~�B�B�+B�JB�\B��B��B��B��B��B��B��B�B�B�B�B�!B�!B�9B�RB�jBƨBǮBɺB��B��B��B��B�B�/B�/B�5B�;B�;B�;B�;B�HB�TB�fB�mB�mB�mB�mB�B�B�B��B��B	B	%B		7B	
=B	DB	\B	bB	bB	bB	hB	uB	�B	�B	�B	�B	!�B	%�B	)�B	/B	49B	5?B	6FB	6FB	7LB	8RB	8RB	8RB	9XB	?}B	E�B	E�B	H�B	J�B	P�B	R�B	S�B	VB	W
B	YB	aHB	ffB	iyB	k�B	k�B	l�B	l�B	l�B	m�B	n�B	n�B	o�B	p�B	p�B	q�B	q�B	q�B	r�B	s�B	t�B	u�B	v�B	x�B	y�B	y�B	{�B	� B	�+B	�1B	�7B	�7B	�=B	�=B	�JB	�JB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�9B	�?B	�?B	�?B	�?B	�?B	�?B	�FB	�FB	�LB	�LB	�LB	�LB	�LB	�RB	�RB	�XB	�^B	�^B	�^B	�^B	�dB	�dB	�dB	�dB	�dB	�dB	�jB	�jB	�jB	�jB	�jB	�jB	�jB	�wB	�}B	��B	��B	��B	��B	��B	B	ÖB	ÖB	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�;B	�HB	�HB	�NB	�TB	�TB	�ZB	�`B	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
	7B
	7B

=B
DB
DB
JB
JB
JB
VB
VB
\B
\B
\B
\B
\B
\B
bB
hB
hB
hB
hB
hB
hB
hB
hB
oB
hB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
&�B
&�B
(�B
(�B
)�B
+B
+B
+B
+B
+B
+B
+B
,B
-B
-B
-B
.B
.B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
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
49B
49B
49B
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
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
:^B
;dB
;dB
<jB
=qB
>wB
>wB
?}B
?}B
>wB
@�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
E�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
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
L�B
M�B
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
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
T�B
T�B
T�B
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
XB
XB
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
ZB
ZB
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
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
bNB
bNB
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
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
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
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
m�B
n�B
n�B
n�B
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
q�B
q�B
q�B
q�B
q�B
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
u�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�BBªB��B�BBāB�mB��B��B��B��B��B��B�B��B��B�B�B�B�B�B�B�B�$B�$B�$B�$B�+B�7B�5B�mB�B��B�B�B��B��B�B��B��B��B�B��B�+B��B��B�nB��B��B��B|�BG�BB�zBߤB�B�/B�xB�B��B��BtBdtBa�Bk�BxBy�Bu�BX�BI7BE�B5ZBB
�5B
�B
��B
��B
��B
��B
��B
}�B
xB
p!B
gRB
^B
P}B
6�B
�B	�AB	�B	��B	ںB	��B	�B	ΥB	�SB	��B	�?B	�;B	��B	��B	�B	��B	��B	�B	��B	�B	s�B	mCB	l"B	l�B	k�B	l�B	l�B	m�B	nB	nB	n}B	m]B	jKB	\B	W�B	S�B	OvB	MjB	L~B	KDB	IlB	F�B	C�B	?.B	7�B	+�B	%�B	/B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	gB	 B��B�oB�B��B�iB�B��B��B�BݘB͟B�{B�B��B�$B�6B��B��B��B��B�hB�hB�hB��B��B�hB��B��B��B��B�@B�B�BB��B�	B��B��BcB~]B}qB|�B{0B{Bz^By�Bx8BwBv`Bu�Br�BrGBn�BkBjBh�Bf�Bc B`�B_�B_!B]�B[�BZ�BX�BW�BV�BU�BT�BS�BSBQ4BLdBH�BF�BD�BCaBA;B;�B7�B7�B7B3�B1�B1'B0!B.B-B,qB*KB(�B&�B$�B$�B#�B#�B"4B!HB vB �B�B�B�B�B=B�B#B�BEB�B$B?B�BsBsBBCBxB�B)BqBKB�B�B�B�B�B�B�B�B�B
B$BsBB�BWB�B�B vB!B!B vB!�B(
B($B)*B(sB)yB*0B)DB)DB)*B)*B)DB*eB*B*�B,�B/iB/5B/iB/OB/�B0B2�B5tB5tB6�B7�B:B>�B?�B?�B?�BBBCGBEBGzBH�BKDBOBBRTBS[BT�BW�BZ�B[qB\�BaBf�Bh�Bj�Bl"BmwBpoBs�Bv�BxBwfBvBu�BvBvBwBx8ByrB{JB}"B~(B.BcB��B�_B��B�vB��B��B��B��B��B�:B�>B�WB�}B��B�iB�oB��B��B��B�VB��B��B�	B�B�0B�pB�oBخB�~B�IB�OB�VB�pB��B�BB��B�B��B�B�B�B��B��B��B�B��B��B	B	?B		lB	
rB	xB	vB	�B	}B	�B	�B	�B	�B	�B	B	CB	"NB	&fB	*B	/�B	4TB	5ZB	6zB	6zB	7fB	8lB	8�B	8�B	:*B	@4B	E�B	F%B	IRB	K�B	Q4B	SB	T,B	V9B	WsB	Y�B	a�B	f�B	i�B	k�B	k�B	l�B	l�B	l�B	m�B	n�B	n�B	o�B	p�B	p�B	q�B	q�B	q�B	r�B	s�B	t�B	u�B	v�B	x�B	zB	zB	|PB	��B	�EB	�KB	�RB	�RB	�XB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�B	�&B	�
B	�*B	�_B	��B	�GB	�TB	�ZB	�?B	�?B	�?B	�ZB	�tB	�`B	�`B	�LB	�fB	�LB	��B	�fB	�lB	�lB	�rB	�xB	�xB	�^B	�xB	�B	�dB	�dB	�B	�B	�B	�jB	��B	��B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ªB	ÖB	ðB	ÖB	��B	ĶB	ĶB	żB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	οB	��B	��B	�B	� B	�B	�B	�:B	�:B	�,B	�MB	�mB	�_B	�=B	�xB	�~B	�VB	�bB	�bB	�hB	�nB	�B	�tB	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�*B	�0B	�"B	�(B	�B
 OB
;B
-B
3B
3B
3B
3B
9B
?B
?B
?B
EB
KB
fB
	lB
	RB

XB
^B
^B
dB
JB
�B
pB
pB
\B
vB
\B
vB
vB
vB
bB
hB
hB
hB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$B
$&B
'B
'B
)B
)DB
*B
+B
+B
+B
+B
+B
+B
+6B
,=B
-)B
-)B
-)B
./B
.IB
/iB
0;B
0!B
0;B
1AB
1AB
1[B
1AB
2-B
2-B
2GB
2GB
33B
33B
33B
3MB
33B
3MB
33B
3MB
49B
4TB
4nB
4TB
5ZB
5tB
6`B
6FB
6FB
6FB
6FB
6`B
7LB
7LB
7fB
7fB
7fB
7fB
8RB
8RB
8RB
8lB
8RB
8lB
8�B
8�B
:�B
;B
;�B
<�B
=�B
>�B
>�B
?}B
?�B
>�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
FB
G�B
H�B
H�B
H�B
I�B
J	B
J�B
K�B
K�B
K�B
K�B
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
L�B
NB
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
PB
O�B
O�B
O�B
PB
Q B
P�B
Q�B
Q�B
Q B
Q�B
Q�B
RB
RB
R:B
SB
SB
S&B
TB
UB
T�B
UB
V9B
VB
VB
W$B
W$B
W$B
W$B
XB
XB
X+B
XB
X+B
Y1B
Y1B
YB
YB
Y1B
ZB
ZB
ZB
ZB
ZB
Z7B
Z7B
ZB
Z7B
[#B
[#B
[#B
[#B
[=B
[	B
[WB
[=B
\CB
\]B
\)B
\CB
\CB
]/B
]/B
]IB
^5B
^OB
^OB
^OB
_VB
_VB
_pB
`\B
`vB
`�B
a|B
bhB
bhB
cTB
cTB
cTB
cnB
cnB
dZB
dtB
dtB
dZB
e`B
e`B
eFB
ezB
ezB
e`B
e`B
ezB
ezB
ffB
ffB
f�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
hsB
hsB
hsB
hXB
h�B
hsB
h�B
iyB
iyB
iyB
i_B
i�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
m�B
n�B
n�B
n�B
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
q�B
q�B
q�B
q�B
q�B
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
u�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901240035172019012400351720190124003517202211182137412022111821374120221118213741201901250020142019012500201420190125002014  JA  ARFMdecpA19c                                                                20190114033629  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190113183751  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190113183753  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190113183754  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190113183754  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190113183754  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190113183755  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190113183755  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190113183755  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190113183755                      G�O�G�O�G�O�                JA  ARUP                                                                        20190113185603                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190113153325  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20190123153517  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190123153517  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190124152014  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231517                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123741  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                