CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-11-19T18:35:52Z creation;2017-11-19T18:35:55Z conversion to V3.1;2019-12-18T07:26:45Z update;2022-11-21T05:31:40Z update;     
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
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171119183552  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               wA   JA  I1_0397_119                     2C  DdiNAVIS_A                         0397                            ARGO 011514                     863 @�6�)�� 1   @�6���� @:ΐ��$t�di�B��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�33@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)y�D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@fD@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��H@��H@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B�\B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�D\D�\D\D�\D	\D
�D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)x�D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D@�D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D�|{D��{D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�jA�l�A�n�A�p�A�n�A�n�A�ffA�dZA�dZA�n�A�z�A�v�A�jA�n�A�p�A�p�A�x�A�x�A�t�A�r�A�r�A�r�A�r�A�r�A�v�A�x�A�bNA�S�A�G�A�I�A�A�A�A�A��A��uA�A�A�;dA�A�hsA���A��FA�1'A���A���A�^5A��A�;dA��9A��A��\A�~�A��FA�jA���A�+A�`BA��A���A���A�n�A�"�A��+A���A�
=A��jA���A�?}A�VA��yA�^5A�"�A�ffA�ĜA�\)A�oA��mA�ȴA���A�I�A��RA�I�A�\)A�%A��A���A�A��9A��!A��PA��`A|A�Ax��Aw%Au
=AsK�Aq�;Aq"�Ao��AnJAmx�Am+Al��AlbAk&�Ajz�AiXAg�Af�jAe�7Ad�Ad1'Ac��AcO�Ab�AbM�Aa�A`�A`z�A_dZA^�+A]oA\I�A[�FAY&�AXJAW�
AW��AW�AV�AVv�AV^5AT��AT�AS�TAS/AR�+ARM�AR�AQ�AQ�AQ�;AQG�AP�AO;dAN��ANz�AM�AL9XAJĜAI�^AH�AG&�AG
=AFAD��AD�9AD�+AD$�AC|�AC�ABbNA@ȴA?�mA>�A=�FA<jA;p�A:~�A:1A9G�A7��A6��A5�^A5C�A5%A4��A4-A333A3A2��A2JA1XA1&�A1A0��A0�A01'A/��A/��A/VA.I�A-?}A,9XA,$�A+�A+XA*�+A(n�A&��A&Q�A%ƨA$�`A$1A"�HA!��A!/A �!AA�jA�TA�7AK�AZA&�Ax�A��A�DAjAI�A+A�A��A��A7LA��A(�A��A�#AK�A�9Ax�A7LA&�A�AoA
�yA
��A	��A	�A��A��AI�A��A�hA�AI�A  A��A��A�AĜAbNA$�A�;A33A 1@��@�t�@�`B@�9X@�E�@�@��@�  @�t�@�l�@�dZ@�K�@�@�1@�{@땁@���@�E�@�G�@�ƨ@���@�5?@噚@�j@�+@�1'@���@��/@�%@�I�@�33@թ�@�A�@�
=@�Z@͡�@�I�@˥�@�33@���@��@�/@ÍP@�`B@�b@�ff@�I�@��@���@�%@�r�@�bN@�Q�@��@���@�M�@���@��^@�x�@�&�@�j@��F@�
=@�M�@��h@���@�t�@�33@��7@��@�1'@�|�@�@�%@��@��@��@�ȴ@�ȴ@�ȴ@��R@���@���@�n�@�E�@���@��7@�`B@�/@��/@��@�K�@�+@��@��R@�M�@��@��@�p�@��`@��u@��@�o@��@�bN@�dZ@��@�^5@�{@���@�hs@��9@��w@�K�@�v�@��^@��7@�X@�&�@���@��/@�I�@�t�@�+@��!@�ff@��@���@�(�@�t�@�-@�hs@��9@�1'@��m@��w@�S�@�@��y@�ȴ@�~�@�=q@�J@���@�`B@��@���@��/@�j@���@���@�C�@��@�~�@�{@��@��@���@��-@��h@�/@�V@��`@�Ĝ@���@�r�@�Q�@�  @���@���@�|�@��@��R@���@�ff@��@�X@��@��`@��@�j@�;@�@~E�@}@}O�@}�@|��@|�@|�@|(�@{�
@{�F@{�@{o@zn�@z=q@y�@y��@yG�@y%@x�9@xr�@x �@w�@w�@wl�@w;d@vȴ@v5?@u�h@t�/@t�D@tI�@t(�@t�@s�
@sdZ@r�H@r�@q��@q��@qX@pĜ@o�;@o��@o�P@o|�@o�@nȴ@nff@nE�@m��@l��@k��@k�
@kt�@k"�@j�H@j��@j��@jM�@i�#@i�7@i7L@i%@h�`@h��@h��@h�@h�@h�@hbN@hQ�@hA�@hA�@hA�@hA�@hA�@hA�@h �@hb@hb@h  @g��@g�w@g�@g��@g|�@g|�@gl�@gl�@gl�@f�@eO�@dj@d�@c�F@cdZ@b=q@`1'@^�y@^�+@^@]@]O�@\I�@[�
@[��@[�@[�@[t�@[t�@[dZ@[@Z��@Z�!@Yx�@Y&�@X�`@X�9@X��@X�@X�u@X�@X�@X�@X�@Xr�@XQ�@X  @W�w@W��@W\)@W;d@W�@V��@V�@V�@Vȴ@Vff@VV@VV@VE�@V5?@U��@U�@Up�@UO�@U?}@T��@T��@T�j@T�D@TZ@S��@S@R�!@RJ@QX@P��@P1'@P1'@P1'@P  @Ol�@Nȴ@Nv�@M��@M/@L�/@L�D@Lj@LZ@L�@K��@J�!@H��@G;d@F�@F��@Fv�@Fff@F$�@F@E�@E�T@E@E@E�@EO�@E�@D�@C��@Ct�@Ct�@CS�@C33@B�H@B�@A��@A�7@AX@A&�@A�@A%@A%@@��@@�9@@�u@@�@@Q�@@ �@@  @?�@?�;@?�@?|�@?\)@?K�@?+@>ȴ@>@=�@<Z@<I�@<1@;dZ@:�\@9��@9X@9&�@9%@9%@8��@8��@8�`@8��@8��@8Ĝ@8A�@7�@7�w@7�@7+@6�+@6V@65?@6{@6@5�T@5�h@5/@4�/@4Z@41@3�
@3��@3o@2�H@2��@2n�@2n�@2^5@2=q@2�@1��@1�@1�@1�#@1�^@1��@1�7@1x�@1x�@1hs@1X@1�@0�`@0r�@0  @/�P@/;d@.ȴ@.v�@.@-�h@-�@,��@,I�@+�F@+dZ@+"�@*�H@*^5@*-@)�@)�^@)hs@)7L@)%@(��@(�u@(r�@(1'@'+@&��@&��@&@%�@%?}@%V@$�@$�@$z�@$j@$9X@$1@$1@$1@$1@#��@#��@#�m@#ƨ@#��@#S�@#@"^5@"J@!�#@!�^@!��@!�7@!hs@!X@!G�@!G�@!7L@!�@ ��@ Ĝ@ ��@ bN@K�@�@V@��@��@�@O�@�@�/@j@�@�
@�F@�@S�@�@�!@=q@�^@��@�7@X@�@%@%@��@�`@�9@1'@|�@K�@�@��@�y@�@�R@�+@v�@V@5?@$�@$�@{@�@�T@@��@�@p�@p�@�@�@p�@/@��@�@z�@9X@�@1@�m@�F@��@@~�@n�@^5@^5@^5@^5@-@��@7L@7L@&�@%@��@Ĝ@��@��@�u@�@Q�@�@�@|�@�y@v�@$�@{@{@�@�T@�T@�T@�@��@��@��@@�-@�h@O�@��@�@ƨ@
��@
^5@
-@	�^@	��@	��@	��@	��@	��@	�7@	�7@	�7@	x�@	x�@	G�@	&�@��@��@Ĝ@Ĝ@Ĝ@Ĝ@��@Ĝ@Ĝ@�9@�u@�u@�@bN@b@  @  @  @  @�;@��@��@�w@��@��@|�@\)@+@��@{@@��@��@�-@O�@�@V@��@�@�/@�@�D@I�@1@��@�
@��@�@S�@33@33@"�@"�@"�@"�@o@��@~�@�@��@��@�7@x�@hs@G�@G�@7L@7L@&�@&�@�@ ��@ ��@ �@ 1'@ b@   ?��w?�\)?��R?�V?��?���?�O�?�V?�V?�V?��?���?�j?�I�?�1?��m?�ƨ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�jA�l�A�n�A�p�A�n�A�n�A�ffA�dZA�dZA�n�A�z�A�v�A�jA�n�A�p�A�p�A�x�A�x�A�t�A�r�A�r�A�r�A�r�A�r�A�v�A�x�A�bNA�S�A�G�A�I�A�A�A�A�A��A��uA�A�A�;dA�A�hsA���A��FA�1'A���A���A�^5A��A�;dA��9A��A��\A�~�A��FA�jA���A�+A�`BA��A���A���A�n�A�"�A��+A���A�
=A��jA���A�?}A�VA��yA�^5A�"�A�ffA�ĜA�\)A�oA��mA�ȴA���A�I�A��RA�I�A�\)A�%A��A���A�A��9A��!A��PA��`A|A�Ax��Aw%Au
=AsK�Aq�;Aq"�Ao��AnJAmx�Am+Al��AlbAk&�Ajz�AiXAg�Af�jAe�7Ad�Ad1'Ac��AcO�Ab�AbM�Aa�A`�A`z�A_dZA^�+A]oA\I�A[�FAY&�AXJAW�
AW��AW�AV�AVv�AV^5AT��AT�AS�TAS/AR�+ARM�AR�AQ�AQ�AQ�;AQG�AP�AO;dAN��ANz�AM�AL9XAJĜAI�^AH�AG&�AG
=AFAD��AD�9AD�+AD$�AC|�AC�ABbNA@ȴA?�mA>�A=�FA<jA;p�A:~�A:1A9G�A7��A6��A5�^A5C�A5%A4��A4-A333A3A2��A2JA1XA1&�A1A0��A0�A01'A/��A/��A/VA.I�A-?}A,9XA,$�A+�A+XA*�+A(n�A&��A&Q�A%ƨA$�`A$1A"�HA!��A!/A �!AA�jA�TA�7AK�AZA&�Ax�A��A�DAjAI�A+A�A��A��A7LA��A(�A��A�#AK�A�9Ax�A7LA&�A�AoA
�yA
��A	��A	�A��A��AI�A��A�hA�AI�A  A��A��A�AĜAbNA$�A�;A33A 1@��@�t�@�`B@�9X@�E�@�@��@�  @�t�@�l�@�dZ@�K�@�@�1@�{@땁@���@�E�@�G�@�ƨ@���@�5?@噚@�j@�+@�1'@���@��/@�%@�I�@�33@թ�@�A�@�
=@�Z@͡�@�I�@˥�@�33@���@��@�/@ÍP@�`B@�b@�ff@�I�@��@���@�%@�r�@�bN@�Q�@��@���@�M�@���@��^@�x�@�&�@�j@��F@�
=@�M�@��h@���@�t�@�33@��7@��@�1'@�|�@�@�%@��@��@��@�ȴ@�ȴ@�ȴ@��R@���@���@�n�@�E�@���@��7@�`B@�/@��/@��@�K�@�+@��@��R@�M�@��@��@�p�@��`@��u@��@�o@��@�bN@�dZ@��@�^5@�{@���@�hs@��9@��w@�K�@�v�@��^@��7@�X@�&�@���@��/@�I�@�t�@�+@��!@�ff@��@���@�(�@�t�@�-@�hs@��9@�1'@��m@��w@�S�@�@��y@�ȴ@�~�@�=q@�J@���@�`B@��@���@��/@�j@���@���@�C�@��@�~�@�{@��@��@���@��-@��h@�/@�V@��`@�Ĝ@���@�r�@�Q�@�  @���@���@�|�@��@��R@���@�ff@��@�X@��@��`@��@�j@�;@�@~E�@}@}O�@}�@|��@|�@|�@|(�@{�
@{�F@{�@{o@zn�@z=q@y�@y��@yG�@y%@x�9@xr�@x �@w�@w�@wl�@w;d@vȴ@v5?@u�h@t�/@t�D@tI�@t(�@t�@s�
@sdZ@r�H@r�@q��@q��@qX@pĜ@o�;@o��@o�P@o|�@o�@nȴ@nff@nE�@m��@l��@k��@k�
@kt�@k"�@j�H@j��@j��@jM�@i�#@i�7@i7L@i%@h�`@h��@h��@h�@h�@h�@hbN@hQ�@hA�@hA�@hA�@hA�@hA�@hA�@h �@hb@hb@h  @g��@g�w@g�@g��@g|�@g|�@gl�@gl�@gl�@f�@eO�@dj@d�@c�F@cdZ@b=q@`1'@^�y@^�+@^@]@]O�@\I�@[�
@[��@[�@[�@[t�@[t�@[dZ@[@Z��@Z�!@Yx�@Y&�@X�`@X�9@X��@X�@X�u@X�@X�@X�@X�@Xr�@XQ�@X  @W�w@W��@W\)@W;d@W�@V��@V�@V�@Vȴ@Vff@VV@VV@VE�@V5?@U��@U�@Up�@UO�@U?}@T��@T��@T�j@T�D@TZ@S��@S@R�!@RJ@QX@P��@P1'@P1'@P1'@P  @Ol�@Nȴ@Nv�@M��@M/@L�/@L�D@Lj@LZ@L�@K��@J�!@H��@G;d@F�@F��@Fv�@Fff@F$�@F@E�@E�T@E@E@E�@EO�@E�@D�@C��@Ct�@Ct�@CS�@C33@B�H@B�@A��@A�7@AX@A&�@A�@A%@A%@@��@@�9@@�u@@�@@Q�@@ �@@  @?�@?�;@?�@?|�@?\)@?K�@?+@>ȴ@>@=�@<Z@<I�@<1@;dZ@:�\@9��@9X@9&�@9%@9%@8��@8��@8�`@8��@8��@8Ĝ@8A�@7�@7�w@7�@7+@6�+@6V@65?@6{@6@5�T@5�h@5/@4�/@4Z@41@3�
@3��@3o@2�H@2��@2n�@2n�@2^5@2=q@2�@1��@1�@1�@1�#@1�^@1��@1�7@1x�@1x�@1hs@1X@1�@0�`@0r�@0  @/�P@/;d@.ȴ@.v�@.@-�h@-�@,��@,I�@+�F@+dZ@+"�@*�H@*^5@*-@)�@)�^@)hs@)7L@)%@(��@(�u@(r�@(1'@'+@&��@&��@&@%�@%?}@%V@$�@$�@$z�@$j@$9X@$1@$1@$1@$1@#��@#��@#�m@#ƨ@#��@#S�@#@"^5@"J@!�#@!�^@!��@!�7@!hs@!X@!G�@!G�@!7L@!�@ ��@ Ĝ@ ��@ bN@K�@�@V@��@��@�@O�@�@�/@j@�@�
@�F@�@S�@�@�!@=q@�^@��@�7@X@�@%@%@��@�`@�9@1'@|�@K�@�@��@�y@�@�R@�+@v�@V@5?@$�@$�@{@�@�T@@��@�@p�@p�@�@�@p�@/@��@�@z�@9X@�@1@�m@�F@��@@~�@n�@^5@^5@^5@^5@-@��@7L@7L@&�@%@��@Ĝ@��@��@�u@�@Q�@�@�@|�@�y@v�@$�@{@{@�@�T@�T@�T@�@��@��@��@@�-@�h@O�@��@�@ƨ@
��@
^5@
-@	�^@	��@	��@	��@	��@	��@	�7@	�7@	�7@	x�@	x�@	G�@	&�@��@��@Ĝ@Ĝ@Ĝ@Ĝ@��@Ĝ@Ĝ@�9@�u@�u@�@bN@b@  @  @  @  @�;@��@��@�w@��@��@|�@\)@+@��@{@@��@��@�-@O�@�@V@��@�@�/@�@�D@I�@1@��@�
@��@�@S�@33@33@"�@"�@"�@"�@o@��@~�@�@��@��@�7@x�@hs@G�@G�@7L@7L@&�@&�@�@ ��@ ��@ �@ 1'@ b@   ?��w?�\)?��R?�V?��?���?�O�?�V?�V?�V?��?���?�j?�I�?�1?��m?�ƨ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBoBJB��B��B�sB��B��B^5BE�BD�B@�B<jB5?B0!B(�B!�B�B\B  B�yB�
B�B��B�B��B��BǮB�FB�B��B��B�7B|�Bt�B`BBN�B;dB-B!�B�BoBVBJB
=B+BB
��B
�B
�/B
ȴB
ǮB
ŢB
ĜB
ÖB
��B
�qB
�'B
�%B
iyB
\)B
N�B
D�B
<jB
6FB
+B
!�B
�B
�B
�B
uB
PB
	7B
B	��B	�B	�B	�mB	�TB	�BB	�5B	�#B	�
B	��B	��B	ȴB	��B	�^B	�B	�B	��B	��B	�oB	�bB	�\B	�VB	�7B	�B	�B	{�B	u�B	s�B	n�B	k�B	iyB	hsB	gmB	gmB	ffB	bNB	\)B	VB	S�B	P�B	K�B	D�B	=qB	7LB	33B	+B	(�B	%�B	 �B	 �B	�B	�B	�B	�B	�B	uB	VB	+B	B��B��B�B�B�B�B�mB�TB�HB�BB�;B�/B�#B�B�B�
B��B��B��B��B��BɺBǮBŢBĜB��B�dB�?B�?B�9B�!B�B��B��B��B��B��B��B��B��B��B��B�uB�bB�VB�JB�=B�+B�B~�B|�B{�Bz�Bx�Bv�Bs�Bq�Bn�Bk�BffBdZB`BB^5B\)BZBW
BW
BVBVBT�BT�BR�BP�BN�BM�BM�BM�BM�BK�BM�BK�BK�BJ�BI�BG�BE�BE�BE�BD�BB�B@�B?}B>wB;dB:^B9XB9XB8RB9XB8RB8RB8RB7LB6FB5?B33B33B1'B1'B0!B/B/B.B-B,B,B+B+B(�B,B)�B)�B)�B(�B'�B%�B&�B)�B,B,B,B,B1'B1'B33B33B49B7LB:^B:^B:^B;dB;dB:^B:^B:^B;dB<jB=qB>wB?}B@�BA�BB�BC�BD�BC�BC�BC�BE�BF�BI�BK�BK�BP�BQ�BVBVBVBVBVBVBVBW
BXBYB[#B\)B\)B\)B]/B`BBbNBcTBcTBdZBe`BffBgmBhsBjBk�Bk�Bn�Bp�Bu�Bx�Bz�B|�B}�B}�B� B�B�1B�7B�PB�hB�oB�oB�uB�{B�{B��B��B��B��B��B��B�B�'B�FB�wB��BŢBǮBɺB��B��B��B��B��B��B�B�B�B�)B�5B�;B�BB�TB�mB�yB�B�B�B��B��B��B��B��B��B��B��B��B��B��B	  B	  B	B	B	B	%B	
=B	JB	PB	\B	oB	�B	�B	�B	�B	�B	#�B	&�B	+B	-B	/B	0!B	1'B	1'B	2-B	49B	6FB	6FB	7LB	9XB	<jB	=qB	>wB	?}B	A�B	B�B	C�B	E�B	F�B	G�B	I�B	J�B	K�B	L�B	N�B	Q�B	VB	W
B	YB	YB	YB	ZB	\)B	_;B	bNB	dZB	e`B	ffB	jB	n�B	o�B	o�B	p�B	r�B	t�B	u�B	v�B	w�B	}�B	�B	�B	�B	�B	�%B	�%B	�+B	�1B	�DB	�JB	�VB	�\B	�\B	�bB	�hB	�hB	�oB	�oB	�oB	�uB	�uB	�uB	�uB	�uB	�uB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�LB	�jB	�qB	�}B	��B	B	ƨB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�/B	�/B	�/B	�/B	�/B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�TB	�ZB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
	7B

=B

=B

=B
DB
DB
PB
VB
VB
\B
\B
\B
\B
\B
bB
bB
bB
bB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
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
 �B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
&�B
'�B
'�B
'�B
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
+B
,B
,B
,B
,B
,B
,B
,B
-B
-B
.B
.B
/B
0!B
0!B
1'B
1'B
2-B
33B
49B
49B
5?B
5?B
6FB
6FB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
<jB
<jB
<jB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
H�B
I�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
VB
VB
VB
VB
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
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
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
_;B
`BB
`BB
`BB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
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
ffB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
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
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
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
r�B
r�B
r�B
r�B
r�B
s�B
s�B
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
u�B
u�B
u�B
v�B
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
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111oB�B�B�B�B�B�B�B�ByB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BFB,BBB�B�B� BּB��BezBIlBG�BBB>B6+B1vB*B#�B�B�B�B�BںBٴBӏB�YB��B��B��B�8B��B�B��B�xB.Bx�Bc�BR:B>B.�B# BBB�B�B
�B1B�B
��B
��B
ߤB
�7B
��B
��B
��B
��B
��B
��B
��B
�=B
k�B
^�B
P�B
F?B
=�B
8B
,�B
"�B
5B
]B
B
�B
pB

�B
�B	�	B	��B	�qB	�>B	�B	��B	��B	�B	�EB	�bB	̈́B	�	B	��B	�B	�;B	�]B	�yB	��B	��B	��B	��B	�(B	��B	��B	��B	|�B	vFB	t�B	oOB	k�B	i�B	h�B	g�B	g�B	gRB	c�B	]/B	V�B	T�B	R:B	M�B	FYB	>�B	8�B	4�B	+�B	*KB	'B	!HB	!-B	 \B	�B	~B	�B	qB	�B	�B	�B	�B�6B��B�B��B�B��B�B��B�B��B�'B�5BیBٴB�B��B�TB�.B�BB�BB�JB�=B�1BƎBżB��B��B��B��B�?B��B��B��B��B��B��B�B�#B��B��B�yB��B��B�hB��B�B��B��B�B�B}VB|PB{Bz^Bx8Bt�Bs3Bp�Bm�BhsBf2BabB_!B]/B[qBWsBW?BV9BV9BUgBU�BTBR BQ BN�BN�BN�BNVBL�BN�BL0BLBK^BJrBH1BF?BF%BFYBE�BDBA�BA�B?�B<jB;B9�B9�B9$B9�B8lB8�B8�B88B8B6�B4�B3�B1�B1�B1'B/�B/�B.�B.B-�B-�B,�B,=B+6B,�B*�B+B+B*0B)�B'�B'�B*B,�B,�B-�B.�B2�B2�B4TB4�B5�B8�B:�B:�B:�B;�B;�B:�B;0B:�B;�B<�B=�B>�B@BA BB'BC-BDMBE�BC�BDBD�BFtBGBJXBL~BMBQ�BR�BVBVBVBVBV9BV9BV9BW?BXEBYB[qB\]B\xB\�B^B`�BbhBc�Bc�Bd�Be�Bf�Bg�Bh�Bj�BlBlqBo�Bq�BvzBy>B{dB}<B~BB~wB��B��B��B��B��B��B��B��B��B��B��B�+B��B�)B�!B�HB��B�}B��B�B��B�B�%B��B��B�B�"B��B�B�4B�,B�9B�_B�QB�]B�OB�pB�B��B�B��B��B� B�B��B��B��B�B�B�$B�B�B�"B�(B�.B	 4B	 OB	[B	9B	SB	�B	
rB	~B	�B	�B	�B	�B	�B	�B	B	 BB	$&B	'RB	+6B	-CB	/5B	0;B	1AB	1[B	2|B	4nB	6`B	6`B	7�B	9�B	<�B	=�B	>�B	?�B	A�B	B�B	C�B	E�B	F�B	G�B	I�B	J�B	K�B	MB	OB	R:B	VB	W$B	Y1B	Y1B	YKB	ZQB	\xB	_�B	bhB	dtB	e�B	f�B	j�B	n�B	o�B	o�B	p�B	r�B	t�B	u�B	wB	x8B	~(B	� B	�AB	�GB	�9B	�YB	�?B	�_B	�fB	�^B	�dB	��B	�vB	�vB	�}B	��B	��B	�oB	��B	��B	�uB	�uB	�uB	�uB	�uB	�uB	��B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�B	�B	�>B	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ɺB	��B	��B	��B	��B	�0B	� B	�B	�B	�,B	�B	��B	�,B	�B	��B	�B	�B	�B	�B	�$B	�+B	�+B	�1B	�7B	�QB	�=B	�#B	�=B	�=B	�/B	�/B	�dB	�IB	�dB	�VB	�\B	�\B	�\B	�vB	�bB	�bB	�bB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�RB	��B	�]B
AB
-B
3B
3B
3B
9B
9B
9B
9B
%B
YB
?B
?B
_B
�B
	lB

=B

XB

XB
xB
�B
�B
pB
pB
vB
\B
\B
\B
vB
bB
}B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
	B
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
 �B
!�B
"�B
"�B
#�B
#�B
#�B
$B
%,B
%�B
&B
'B
(
B
(
B
($B
)B
*B
*0B
)�B
*B
+B
+B
+6B
+B
+B
+B
+B
,"B
,B
,"B
,B
,B
,"B
,"B
-)B
-CB
.IB
.IB
/OB
0UB
0UB
1[B
1[B
2aB
3hB
4TB
4nB
5ZB
5?B
6`B
6zB
7fB
7fB
8lB
8lB
9rB
9rB
9rB
9rB
:�B
:�B
:�B
<�B
<�B
<�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
GB
H�B
I�B
J�B
K�B
K�B
K�B
K�B
L�B
MB
M�B
NB
N�B
N�B
N�B
OB
O�B
P.B
QB
R B
RB
RB
SB
SB
R�B
R�B
SB
T,B
T,B
T,B
U2B
VB
VB
VB
VB
VB
V9B
VB
VB
W
B
W
B
W
B
W$B
W$B
W
B
W$B
W$B
X+B
XB
W�B
XB
X+B
X+B
X+B
X+B
Y1B
YKB
YKB
Z7B
Z7B
Z7B
Z7B
Z7B
ZQB
[WB
\CB
\)B
\CB
\)B
\)B
\CB
\]B
]IB
^5B
^5B
^OB
^jB
^OB
_;B
_;B
_;B
_VB
_VB
_VB
`\B
`vB
`vB
a�B
bhB
cTB
cTB
cnB
cTB
cnB
cTB
cTB
cTB
cnB
cTB
cTB
cnB
cnB
c�B
dtB
d�B
d�B
f�B
h�B
h�B
i�B
iyB
jB
jB
jeB
j�B
jB
jB
jB
j�B
jB
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
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
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
r�B
r�B
r�B
r�B
r�B
s�B
s�B
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
u�B
u�B
u�B
v�B
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
y�B
y�B
y�B
y�B
y�B
zB
y�B
y�B
y�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<P�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711300032562017113000325620171130003256202211182132372022111821323720221118213237201804031938112018040319381120180403193811  JA  ARFMdecpA19c                                                                20171120033513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171119183552  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171119183553  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171119183554  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171119183554  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171119183554  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171119183555  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171119183555  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171119183555  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171119183555                      G�O�G�O�G�O�                JA  ARUP                                                                        20171119185626                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171120153313  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20171129153256  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171129153256  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103811  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171537                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123237  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                