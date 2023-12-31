CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-09-30T15:37:32Z creation;2019-09-30T15:37:38Z conversion to V3.1;2022-11-21T05:28:15Z update;     
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
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ],   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190930153732  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_187                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @���#�1   @�࣡/h�@;�����db�?1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?�fD@fD@� DA  DA� DB  DBy�DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��D�#31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B\)B��B��B'��B/��B7��B?�\BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[��C]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?��D@�D@\D@�\DA\DA�\DBx�DB�\DC\DC�\DD\DE�DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�<{D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D�|{D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D�{D�"�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1'A�33A�5?A�=qA�=qA�;dA�;dA�=qA�?}A�A�A�A�A�A�A�?}A�C�A�C�A�E�A��A�jAΕ�A�1A��;A� �A��A��A�p�A�-A�G�A�9XA�+A��`A���A��A��!A��
A��FA���A��A��A�A�C�A��/A��+A�v�A�jA�-A��!A��A���A���A� �A��PA�;dA��yA��A���A��jA��A��A�9XA�\)A���A���A�&�A�7LA�ĜA�z�A��A�VA�7LA�v�A�VA��A���A���A�dZA�bNA��A��A���A���A��\A�p�A�E�A�^5A�ƨA�5?A��#A��9A�\)A���A�A�A���A��A��A�wA|�+Ay��Ay�AwAw�Av��Au�Au�7At��At�!Ar��Aq?}Am�
Alr�AkXAj~�Ai��Ah�9AhI�Af��Ae/Ac�^Ab�/Ab�!AbjAa�mAa33A`�!A_�A^n�A]ƨA]|�A]C�A\��A\A�A[hsAY��AX��AW33AT��AR��AQ�AQ�AP~�APn�APZAP1'AO�7ANbNAN1AMl�AL�RAL5?AJ�`AI��AH��AHM�AH{AG�PAF��AFA�AE�FAD��AC�AC��AC/AB�AA�A@�uA?��A>ffA>  A=��A<�A<Q�A;�-A:bNA9��A9�A8�!A7�PA5��A4��A3��A3"�A2��A1�
A0�A/��A.�/A-�7A+A*�\A*n�A*M�A*  A)��A(�A(A�A'�FA'A&=qA%�A%��A%?}A#�mA!�^A!K�A!VA ��A ��A �DA �+A n�A�A��A �A�A�A33A�uAZA$�A`BA�A�hA{A��A��A\)A��A5?A�PAoA�`AQ�A��AG�AVA�`A�9AjAdZA��A�AjAI�AI�A �Al�A�A=qA�mA��A+A	A�-A  A�7A�\A/A%A�/AĜA�\A?}A bN@��@��@���@���@�7L@�Q�@��@��F@�l�@��`@��@�v�@�r�@�J@��@��@�33@�+@�@���@�w@�o@�\@��@�^@�%@�bN@ߕ�@�b@��@�hs@�A�@�K�@�"�@�"�@��H@�ff@���@��;@ϥ�@�+@�@̣�@��@�"�@��T@���@�1'@��y@�{@��@��@Ĵ9@�|�@�@�7L@��@��@��7@��@�o@�M�@�+@��H@�v�@�/@��;@��P@���@��@�A�@�dZ@�M�@���@�I�@��@�n�@���@��h@��u@�1'@�C�@���@�=q@�@��@�?}@���@�I�@��;@�\)@��@��H@���@��\@�n�@�E�@��@���@�O�@��@��@��@���@�I�@�
=@�V@��7@��@�Ĝ@�1@�dZ@�@���@�@��h@��j@��@��w@���@�dZ@��@�-@���@��@�%@�(�@��P@��y@�~�@�5?@�hs@��@���@��D@�(�@���@���@�"�@��@��R@�~�@��@���@�V@��/@��9@��@�j@�1'@�  @���@��@��@�v�@��-@��j@�I�@���@��;@��;@���@�S�@�@�ȴ@�E�@�J@��@���@��@���@��`@��j@��j@�r�@�9X@��@���@�\)@��@��@��!@�~�@�V@�=q@�5?@�-@�@���@�G�@���@�A�@��m@�l�@���@��+@�v�@�V@�-@��T@��7@�/@��/@��9@��@���@�r�@��@�1@��@\)@;d@~��@}�@|�@|�@|Z@|I�@|I�@|1@z�@z�\@y��@yX@x��@x�u@x1'@x  @w�@w�;@w��@w|�@v��@up�@u/@t�@tz�@tj@t1@s��@s33@sS�@sS�@so@r��@rn�@r�@q�#@p�`@pQ�@p �@pb@o��@o�P@o\)@nȴ@n5?@m�@m/@l�@l�j@l��@l�@kdZ@j�H@jn�@i�^@i�@h�9@hr�@g�@f��@f��@f�+@fv�@fff@fE�@e�T@d�@dz�@cƨ@ct�@ct�@b�@b�\@b~�@b^5@bM�@b=q@b-@a��@a�^@a��@a��@`Ĝ@_�@^�@^��@^E�@^{@]�T@]�-@]�@]V@\z�@[��@[33@[@Z��@Z�\@Z~�@Z�@Yhs@X��@Xr�@X1'@W�;@W��@W\)@W�@W
=@V��@V�@V��@VV@U�h@UO�@Up�@U��@U`B@UV@T��@T��@T��@T�/@T�D@TI�@T�@S�
@S��@St�@SC�@S33@S"�@S"�@R�@R��@R��@R��@RJ@Q��@Q&�@P�`@P�@Pb@O�@O\)@O+@N�y@N�R@N5?@M@M�@M/@L�/@L�D@LZ@L9X@L�@K�m@Kt�@KS�@KC�@K"�@Ko@J�@J��@J^5@I��@I��@Ix�@Ix�@Ihs@Ihs@I7L@H��@Hb@G��@Gl�@G;d@F��@F��@F��@F��@F�y@F��@F��@F�+@Fv�@FE�@F{@E�T@D�@D(�@Cƨ@Ct�@CS�@B�H@BM�@A�@A��@A�7@Ax�@A�@@�`@@��@@ �@?�@?�;@?�P@>�y@>�y@>ȴ@>�R@>�R@>ff@=��@=�h@=p�@=O�@=V@<�@<�j@<j@;��@;��@;t�@;o@9��@9��@9X@9�@8��@8�@8Q�@8 �@8  @7�@7K�@7�@6�y@6�@6��@6V@6@5�T@5��@5��@5`B@5�@4�/@4�j@4�D@3��@3�@2�H@2�\@2=q@1��@1&�@0��@0�u@01'@/�;@/��@/\)@/;d@.��@.V@.{@-�T@-�h@-/@-V@,�j@,��@,z�@,(�@+S�@*�@*�!@*��@*�\@*^5@*�@)��@)X@)%@(�@'��@'�@'��@'�P@'�P@'|�@'\)@'+@&��@&ȴ@&�+@&E�@%@%��@%�@%`B@%V@$�@$j@$Z@$9X@$1@#ƨ@#t�@#o@"��@"��@"�!@"~�@"M�@"�@!��@!�^@!�@ ��@ ��@ ��@ ��@ bN@ A�@  �@�;@|�@+@�y@ȴ@��@v�@V@$�@@�T@@�-@��@�h@`B@/@V@��@(�@ƨ@ƨ@��@dZ@S�@C�@"�@�!@M�@�@��@�@�7@7L@�@�@��@ �@�;@�@|�@\)@K�@+@�y@��@��@��@ff@E�@5?@$�@@�@@�h@p�@/@�@�/@�/@�j@��@j@Z@I�@9X@9X@(�@1@�
@t�@S�@33@�@~�@=q@=q@-@�@��@�@��@�^@�7@hs@G�@��@�@Q�@1'@ �@b@  @�;@�w@��@�P@l�@K�@K�@+@��@�y@v�@{@��@�-@�h@`B@�@�@��@�D@z�@j@Z@Z@I�@(�@ƨ@��@�@dZ@
��@
n�@
^5@
^5@
M�@
=q@	�@	�7@	X@	�@	%@	%@Ĝ@��@�u@�u@�@r�@Q�@1'@|�@K�@;d@;d@;d@;d@+@�y@�+@V@@@�-@�h@O�@/@�@��@Z@(�@�m@�
@��@t�@S�@C�@C�@"�@o@�@��@��@��@��@��@^5@-@��@�#@�^@��@��@x�@X@7L@7L@&�@%@ ��@ Ĝ@ �9@ ��@ bN@ A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1'A�33A�5?A�=qA�=qA�;dA�;dA�=qA�?}A�A�A�A�A�A�A�?}A�C�A�C�A�E�A��A�jAΕ�A�1A��;A� �A��A��A�p�A�-A�G�A�9XA�+A��`A���A��A��!A��
A��FA���A��A��A�A�C�A��/A��+A�v�A�jA�-A��!A��A���A���A� �A��PA�;dA��yA��A���A��jA��A��A�9XA�\)A���A���A�&�A�7LA�ĜA�z�A��A�VA�7LA�v�A�VA��A���A���A�dZA�bNA��A��A���A���A��\A�p�A�E�A�^5A�ƨA�5?A��#A��9A�\)A���A�A�A���A��A��A�wA|�+Ay��Ay�AwAw�Av��Au�Au�7At��At�!Ar��Aq?}Am�
Alr�AkXAj~�Ai��Ah�9AhI�Af��Ae/Ac�^Ab�/Ab�!AbjAa�mAa33A`�!A_�A^n�A]ƨA]|�A]C�A\��A\A�A[hsAY��AX��AW33AT��AR��AQ�AQ�AP~�APn�APZAP1'AO�7ANbNAN1AMl�AL�RAL5?AJ�`AI��AH��AHM�AH{AG�PAF��AFA�AE�FAD��AC�AC��AC/AB�AA�A@�uA?��A>ffA>  A=��A<�A<Q�A;�-A:bNA9��A9�A8�!A7�PA5��A4��A3��A3"�A2��A1�
A0�A/��A.�/A-�7A+A*�\A*n�A*M�A*  A)��A(�A(A�A'�FA'A&=qA%�A%��A%?}A#�mA!�^A!K�A!VA ��A ��A �DA �+A n�A�A��A �A�A�A33A�uAZA$�A`BA�A�hA{A��A��A\)A��A5?A�PAoA�`AQ�A��AG�AVA�`A�9AjAdZA��A�AjAI�AI�A �Al�A�A=qA�mA��A+A	A�-A  A�7A�\A/A%A�/AĜA�\A?}A bN@��@��@���@���@�7L@�Q�@��@��F@�l�@��`@��@�v�@�r�@�J@��@��@�33@�+@�@���@�w@�o@�\@��@�^@�%@�bN@ߕ�@�b@��@�hs@�A�@�K�@�"�@�"�@��H@�ff@���@��;@ϥ�@�+@�@̣�@��@�"�@��T@���@�1'@��y@�{@��@��@Ĵ9@�|�@�@�7L@��@��@��7@��@�o@�M�@�+@��H@�v�@�/@��;@��P@���@��@�A�@�dZ@�M�@���@�I�@��@�n�@���@��h@��u@�1'@�C�@���@�=q@�@��@�?}@���@�I�@��;@�\)@��@��H@���@��\@�n�@�E�@��@���@�O�@��@��@��@���@�I�@�
=@�V@��7@��@�Ĝ@�1@�dZ@�@���@�@��h@��j@��@��w@���@�dZ@��@�-@���@��@�%@�(�@��P@��y@�~�@�5?@�hs@��@���@��D@�(�@���@���@�"�@��@��R@�~�@��@���@�V@��/@��9@��@�j@�1'@�  @���@��@��@�v�@��-@��j@�I�@���@��;@��;@���@�S�@�@�ȴ@�E�@�J@��@���@��@���@��`@��j@��j@�r�@�9X@��@���@�\)@��@��@��!@�~�@�V@�=q@�5?@�-@�@���@�G�@���@�A�@��m@�l�@���@��+@�v�@�V@�-@��T@��7@�/@��/@��9@��@���@�r�@��@�1@��@\)@;d@~��@}�@|�@|�@|Z@|I�@|I�@|1@z�@z�\@y��@yX@x��@x�u@x1'@x  @w�@w�;@w��@w|�@v��@up�@u/@t�@tz�@tj@t1@s��@s33@sS�@sS�@so@r��@rn�@r�@q�#@p�`@pQ�@p �@pb@o��@o�P@o\)@nȴ@n5?@m�@m/@l�@l�j@l��@l�@kdZ@j�H@jn�@i�^@i�@h�9@hr�@g�@f��@f��@f�+@fv�@fff@fE�@e�T@d�@dz�@cƨ@ct�@ct�@b�@b�\@b~�@b^5@bM�@b=q@b-@a��@a�^@a��@a��@`Ĝ@_�@^�@^��@^E�@^{@]�T@]�-@]�@]V@\z�@[��@[33@[@Z��@Z�\@Z~�@Z�@Yhs@X��@Xr�@X1'@W�;@W��@W\)@W�@W
=@V��@V�@V��@VV@U�h@UO�@Up�@U��@U`B@UV@T��@T��@T��@T�/@T�D@TI�@T�@S�
@S��@St�@SC�@S33@S"�@S"�@R�@R��@R��@R��@RJ@Q��@Q&�@P�`@P�@Pb@O�@O\)@O+@N�y@N�R@N5?@M@M�@M/@L�/@L�D@LZ@L9X@L�@K�m@Kt�@KS�@KC�@K"�@Ko@J�@J��@J^5@I��@I��@Ix�@Ix�@Ihs@Ihs@I7L@H��@Hb@G��@Gl�@G;d@F��@F��@F��@F��@F�y@F��@F��@F�+@Fv�@FE�@F{@E�T@D�@D(�@Cƨ@Ct�@CS�@B�H@BM�@A�@A��@A�7@Ax�@A�@@�`@@��@@ �@?�@?�;@?�P@>�y@>�y@>ȴ@>�R@>�R@>ff@=��@=�h@=p�@=O�@=V@<�@<�j@<j@;��@;��@;t�@;o@9��@9��@9X@9�@8��@8�@8Q�@8 �@8  @7�@7K�@7�@6�y@6�@6��@6V@6@5�T@5��@5��@5`B@5�@4�/@4�j@4�D@3��@3�@2�H@2�\@2=q@1��@1&�@0��@0�u@01'@/�;@/��@/\)@/;d@.��@.V@.{@-�T@-�h@-/@-V@,�j@,��@,z�@,(�@+S�@*�@*�!@*��@*�\@*^5@*�@)��@)X@)%@(�@'��@'�@'��@'�P@'�P@'|�@'\)@'+@&��@&ȴ@&�+@&E�@%@%��@%�@%`B@%V@$�@$j@$Z@$9X@$1@#ƨ@#t�@#o@"��@"��@"�!@"~�@"M�@"�@!��@!�^@!�@ ��@ ��@ ��@ ��@ bN@ A�@  �@�;@|�@+@�y@ȴ@��@v�@V@$�@@�T@@�-@��@�h@`B@/@V@��@(�@ƨ@ƨ@��@dZ@S�@C�@"�@�!@M�@�@��@�@�7@7L@�@�@��@ �@�;@�@|�@\)@K�@+@�y@��@��@��@ff@E�@5?@$�@@�@@�h@p�@/@�@�/@�/@�j@��@j@Z@I�@9X@9X@(�@1@�
@t�@S�@33@�@~�@=q@=q@-@�@��@�@��@�^@�7@hs@G�@��@�@Q�@1'@ �@b@  @�;@�w@��@�P@l�@K�@K�@+@��@�y@v�@{@��@�-@�h@`B@�@�@��@�D@z�@j@Z@Z@I�@(�@ƨ@��@�@dZ@
��@
n�@
^5@
^5@
M�@
=q@	�@	�7@	X@	�@	%@	%@Ĝ@��@�u@�u@�@r�@Q�@1'@|�@K�@;d@;d@;d@;d@+@�y@�+@V@@@�-@�h@O�@/@�@��@Z@(�@�m@�
@��@t�@S�@C�@C�@"�@o@�@��@��@��@��@��@^5@-@��@�#@�^@��@��@x�@X@7L@7L@&�@%@ ��@ Ĝ@ �9@ ��@ bN@ A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B1B1B1B	7B	7B	7B1B	7B	7B	7B	7B	7B	7B	7B	7B	7B+B��B�/B�HB�HB�/B�B��B��B��B��B��B��B��BȴBĜB��B�dB�?B�B�B��B��B��B��B��B��B��B�uB�DB�B{�Bt�Bp�Bl�BhsBdZBR�B:^B)�B�BB��B�B�#B��B�}B��B��B��B��B�7B��B��B�3Br�Bu�BbNBC�B8RB%�B$�B'�B(�B)�B(�B�BJBB
��B
��B
�B
�B
��B
�XB
�B
��B
�oB
�PB
u�B
cTB
\)B
S�B
N�B
K�B
F�B
C�B
?}B
<jB
/B
"�B
PB
B	��B	��B	�B	�sB	�ZB	�)B	��B	��B	ŢB	ĜB	B	�wB	�^B	�LB	�'B	�B	��B	��B	��B	��B	��B	��B	�bB	�7B	�B	t�B	jB	e`B	`BB	^5B	]/B	\)B	ZB	VB	O�B	K�B	I�B	G�B	D�B	?}B	8RB	5?B	33B	2-B	0!B	,B	)�B	&�B	"�B	�B	�B	�B	�B	oB	JB	+B	B��B��B��B��B��B�B�B�yB�fB�HB�B�B��B��B��BƨBB�wB�XB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�DB�=B�7B�7B�1B�1B�+B�%B�B�B�B� B� B~�B}�B{�Bz�Bv�Bp�Bk�BjBiyBe`Be`BcTB`BB]/B[#BZBYBW
BVBT�BT�BS�BR�BR�BQ�BQ�BQ�BP�BP�BP�BN�BM�BK�BJ�BH�BE�BA�B>wB;dB9XB8RB6FB6FB5?B5?B33B33B2-B1'B0!B.B.B.B.B.B.B,B,B+B)�B'�B%�B#�B!�B!�B!�B �B �B �B�B�B�B�B�B�B�B�B�B�B �B#�B%�B%�B'�B(�B+B+B)�B(�B+B)�B(�B,B+B'�B%�B$�B'�B+B-B-B.B.B/B0!B2-B6FB8RB5?B5?B>wB@�B?}B@�BB�BD�BE�BD�BB�BC�BD�BF�BL�BT�BZB[#B[#B\)B]/B_;B`BB_;B_;B_;B`BBaHBdZBffBhsBiyBiyBiyBiyBiyBjBk�Bl�Bm�Bm�Bo�Bo�Bn�Bp�Bv�By�B|�B}�B� B�B�B�%B�+B�7B�DB�VB�hB�uB�uB�{B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�?B�LB�RB�^B�jB�qB�wB�}B��BĜBȴBɺB��B��B��B��B��B��B��B�B�B�5B�NB�fB�yB�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	%B		7B	JB	PB	bB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	'�B	-B	0!B	1'B	2-B	33B	5?B	5?B	7LB	:^B	;dB	<jB	<jB	=qB	A�B	B�B	C�B	D�B	E�B	F�B	G�B	J�B	K�B	M�B	O�B	O�B	Q�B	VB	XB	ZB	[#B	]/B	]/B	^5B	_;B	_;B	aHB	bNB	cTB	dZB	jB	k�B	m�B	n�B	n�B	p�B	r�B	t�B	x�B	y�B	z�B	|�B	}�B	~�B	�B	�B	�%B	�+B	�+B	�1B	�=B	�=B	�JB	�VB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�9B	�LB	�RB	�XB	�jB	�jB	�qB	�qB	�qB	�wB	�wB	�wB	�}B	�}B	�}B	�}B	�}B	�}B	��B	��B	ÖB	ĜB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�ZB	�ZB	�`B	�`B	�`B	�`B	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
	7B
	7B
	7B
DB
JB
PB
VB
VB
VB
\B
\B
bB
hB
hB
oB
oB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
$�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
,B
,B
.B
.B
.B
/B
0!B
1'B
1'B
2-B
33B
33B
49B
49B
49B
5?B
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
9XB
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
>wB
>wB
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
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
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
Q�B
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
T�B
VB
VB
VB
VB
W
B
XB
XB
XB
XB
XB
XB
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
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
`BB
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
dZB
e`B
e`B
e`B
ffB
ffB
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
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
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
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
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
z�B
z�B
z�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B1B1B1B	7B	7B	7B1B	7B	7B	7B	7B	7B	7B	lB	�B)B6BjB�FB��B�B�fB��B��B�B�B��B�bB�BϑB�)B��B�-B�}B�fB�UB�IB��B�
B�4B�QB��B�B��B��B��B�tB}�Bu�Bq�Bm�Bi�Bg�BV�B=B,�B�BtB�0B��B�~BοB�UB��B��B��B��B��B��B�eB��Bt�BxlBd�BF?B:�B&�B%B(
B)_B+B+�B�B�BMB
��B
��B
�nB
�B
��B
��B
��B
��B
�B
��B
xlB
d�B
]�B
T�B
O�B
L~B
GEB
DgB
@�B
>�B
1[B
&2B
B
aB	�B	��B	�B	�yB	�fB	��B	ӏB	ˬB	��B	�B	�GB	�cB	�JB	��B	�aB	��B	�XB	�fB	��B	�|B	�B	��B	�B	�xB	��B	v�B	k�B	fLB	`�B	^jB	]dB	\�B	[#B	W?B	P}B	L�B	J�B	H�B	F?B	AB	9rB	5�B	3�B	2�B	1AB	,�B	*�B	(
B	#�B	!B	�B	kB	�B	B	�B	�B	�B��B�B��B��B�FB�B�WB�eB�$B�TB�WB�YB�BΊB�B��B��B�4B�dB��B��B�DB�DB�sB��B��B��B��B��B��B�B�1B��B�mB��B��B��B��B��B�fB��B��B�B��B��B��B�iB��B�B~]B|�B|BxBr-Bm�BlWBj�BfBf2Bd@Ba-B^B[�B[	BY�BW�BVmBUgBU�BT�BT,BS�BRTBR:BR BQBQhBQ�BO�BN�BLdBKxBI�BG�BD3B@iB<PB:�B9�B6�B6�B5�B5�B4�B4TB2�B1�B1[B/�B/B.�B.}B.}B.�B-�B-B,"B+�B)�B'�B%zB#:B"hB"hB!�B!|B!HB 'B 'B 'B \B�B�BVB�B�B �B!bB$B&B&LB(�B)�B+�B+kB*�B)�B+�B*�B)�B,�B+�B(�B&�B%`B($B+kB-�B-�B/ B/B0B1'B3B6�B9XB5�B5B>�BAB@�BAoBCBEmBF�BESBCaBD�BE�BG_BM�BU�BZ�B[�B[�B\�B]�B_�B`�B_�B_�B_�B`�Ba�Bd�Bf�Bh�Bi�Bi�Bi�Bi�Bi�Bj�Bk�Bl�Bm�Bm�Bo�Bo�BoBq�BwLBzxB}<B~]B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B��B�/B�OB�TB��B�KB�WB��B�oB�|B��B��B��B��B��B��B��B��B��B��B�B��B��B��B�B�B�B�.B� B�[BּBںB��B�B�B�B�B��B��B� B��B�B��B��B�B�$B�B�B	 4B	AB	[B	SB	tB		�B	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	#B	!B	"B	&LB	(XB	-CB	0UB	1[B	2aB	3�B	5�B	5�B	7�B	:xB	;B	<�B	<�B	=�B	A�B	B�B	C�B	D�B	E�B	F�B	HB	J�B	K�B	M�B	O�B	PB	RTB	V9B	X_B	ZQB	[WB	]~B	]dB	^OB	_VB	_VB	abB	b�B	c�B	d�B	j�B	k�B	m�B	n�B	n�B	p�B	r�B	t�B	x�B	y�B	{B	}B	~BB	HB	�UB	�gB	�?B	�EB	�EB	�KB	�XB	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�LB	�6B	�CB	�OB	�hB	�TB	�fB	�lB	�rB	�jB	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�B	� B	�B	�B	�B	�B	�B	�2B	�9B	�EB	�7B	�#B	�CB	�OB	�;B	�;B	�\B	�\B	�\B	�bB	�bB	�B	�nB	�nB	�tB	�tB	�`B	�tB	�tB	�zB	�zB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	�B	�B	�B	�B	�<B	�.B
 B
 B
 B
B
B
B
AB
'B
�B
'B
-B
-B
-B
GB
{B
YB
KB
	lB
	RB
	lB
�B
dB
jB
pB
pB
�B
�B
vB
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
$�B
%�B
'B
'B
(
B
($B
)B
)B
)B
)*B
*B
)�B
*B
+QB
,=B
,"B
./B
.IB
.IB
/OB
0;B
1AB
1vB
2aB
3MB
3MB
4TB
4TB
4nB
5tB
6`B
6`B
6`B
7fB
7�B
8lB
8lB
8�B
9�B
:xB
;B
;dB
;B
;B
<�B
<�B
<�B
=�B
>�B
>�B
@�B
@�B
@�B
@iB
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
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
M�B
M�B
M�B
M�B
M�B
OB
N�B
N�B
N�B
N�B
N�B
PB
PB
O�B
O�B
PB
RB
Q�B
RB
RB
R�B
SB
SB
S&B
SB
TB
TB
TB
UB
UB
V9B
VB
VB
V9B
W$B
X+B
X+B
X+B
X+B
X+B
X+B
Y1B
YB
YB
Y1B
ZQB
ZB
ZB
Z7B
Z7B
Z7B
Z7B
[=B
[=B
[=B
\)B
\)B
\CB
\]B
\CB
]IB
]/B
]/B
]/B
]/B
]/B
]IB
]IB
^OB
^OB
^OB
_pB
`vB
`BB
`BB
`'B
`\B
`\B
`\B
`\B
abB
abB
abB
a|B
bhB
bhB
cnB
cTB
cTB
cnB
cnB
cnB
cnB
dZB
dtB
dtB
dZB
dtB
dtB
ezB
e�B
e�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
iyB
iyB
i�B
i�B
i�B
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
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
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
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
xB
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
z�B
z�B
z�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<we�<#�
<#�
<#�
<#�
<[��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201910110032272019101100322720191011003227202211182140342022111821403420221118214034201910290018232019102900182320191029001823  JA  ARFMdecpA19c                                                                20191001003725  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190930153732  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190930153735  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190930153735  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190930153736  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190930153736  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190930153736  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20190930153736  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20190930153736  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190930153736  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20190930153738  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190930153738                      G�O�G�O�G�O�                JA  ARUP                                                                        20190930155408                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20190930153207  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20190930153125  CV  JULD            G�O�G�O�F�                JM  ARCAJMQC2.0                                                                 20191010153227  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191010153227  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191028151823  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124034  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                