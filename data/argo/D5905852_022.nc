CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-09-10T06:37:56Z creation;2019-09-10T06:37:58Z conversion to V3.1;2022-08-02T05:12:07Z update;     
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
_FillValue                 �  ]t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ad   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ϥ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190910063756  20220818081508  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_022                    2C  D   APEX                            8420                            2.11.2                          846 @��h"�P�1   @��h��ƀ@.!:��S�c��Ϫ͟1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @   @s33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�ffB���B�ffB���B�ffB�33B�  B�  B�  B�ffB���B���B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\�C^  C`  Cb  Cd�Ce�fCg�fCi�fCk�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}fD}�fD~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�3D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�3D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@{@fff@�ff@�\)A�
A;�A[�A|(�A�{A�ffA�=qA�=qA��A�{A��
A��B{B{B
=B
=B'
=B/(�B7�B?{BG
=BO{BV�B_  Bg(�Bo{Bw
=B{B�� B�z�B��B���B���B�33B���B�aHB���B�ǮB�u�B�u�B��=B��B�Q�B�Q�B�ffB�u�Bˊ=Bπ B�p�B�ffB�z�B�u�B� B�=B� B�B�B��qB��B�ffC� CǮC� C�qC	C��C��CC� C� C� C� CǮC�CC�C!C#� C%��C'�=C)ٚC+�C-�C/�C1�=C3� C5�C7��C9�3C;� C=C?� CA� CC�CE�CG�qCI��CK� CM� CO� CQ�qCS��CU��CW� CYC[�{C]��C_Ca��Cc�{Ce��Cg�Ci�3Ck�3Cm�RCo��Cq�qCs� Cu� Cw��Cy��C{�C}�=CǮC��C��HC�޸C��HC�� C��HC��fC��C���C���C�޸C��qC�޸C�޸C��HC��C��qC��HC�� C��qC��C�޸C��)C�� C��HC��qC�޸C��HC��)C��)C�޸C���C��C�� C��HC��C��C�� C���C��HC�޸C��C��C��C���C�޸C��qC��HC��C��HC��)C��HC��C�� C�޸C�޸C��)C�޸C��fC��C�޸C���C��HC��HC���C��C�� C�޸C�޸C�� C��HC�޸C��HC��HC��qC��)C�޸C�� C�� C�� C��)C��)C��HC�� C��)C��C��C�� C�޸C��HC�� C��qC��qC�� C�޸C�� C���C��C��C���C��HC�� C��HC��C�� C��)C��HC��HC���C��fC�޸C�� C��C��C��fC��C��HC��C��C���C��C�� C�� C�޸C�޸C��HC��HC��D r�D �Do\D�Dq�D�HDp D�HDq�D�HDp�D�HDqHD�Do\D�DqHD�D	qHD	�D
q�D
� Dp D�HDo\D�\DqHD�Do\D�\DqHD�Dp D�HDp�D�Dq�D�HDo\D�DnD�HDqHD� DqHD�HDn�D�Dp D� Dp D�HDs3D�HDp D�\Do\D�Dp�D�3Dq�D�HDp D�\D r�D ��D!q�D!��D"o\D"� D#q�D#� D$qHD$��D%o\D%�\D&q�D&��D'qHD'�HD(r�D(� D)p�D)� D*p D*�D+r�D+��D,p�D,�\D-p�D-��D.s3D.�HD/p�D/��D0n�D0�D1r�D1�D2p�D2�D3qHD3�D4qHD4�D5o\D5�D6o\D6� D7qHD7�D8p�D8�\D9p D9�D:p D:� D;n�D;� D<qHD<� D=p D=�D>o\D>�\D?qHD?�HD@p D@�\DAo\DA�\DBp�DB�DCo\DC�HDDqHDD�DEn�DE�\DFqHDF�DGp�DG�DHq�DH�DIuDI�HDJp�DJ� DKn�DK�\DLo\DL�DMo\DM� DNp DN� DOqHDO�DPnDP�DQp�DQ��DRp�DR� DSnDS� DTq�DT�HDUo\DU� DVq�DV��DWp�DW�\DXo\DX�\DYp�DY� DZp DZ�HD[s3D[�3D\p�D\�HD]s3D]�3D^s3D^� D_p�D_� D`mqD`�Dap Da�DbqHDb�3Dcr�Dc�3Dds3Dd�HDep De� Dfp�Df�Dgq�Dg� DhmqDh� Dip Di�Djo\Dj�HDkqHDk�HDlqHDl�Dmp�Dm�\DnqHDn� Don�Do�Dps3Dp�3Dqp�Dq�HDrqHDr�\DsqHDs�Dtq�Dt�Duo\Du�\Dvo\Dv�\Dwo\Dw� Dxp�Dx��Dys3Dy�HDzp Dz�D{p�D{�HD|s�D|�D}t{D}�HD~p�D~�HDq�D� D�8RD�x�D�� D��RD�8�D�x�D���D���D�8�D�x�D��HD��RD�7\D�w�D���D�� D�8�D�x�D��RD���D�9HD�yHD�� D���D�8 D�xRD���D��RD�8�D�yHD���D���D�7\D�x�D���D��RD�7�D�w
D��RD�� D�7
D�x D��RD�� D�8 D�x�D���D��\D�7
D�vfD��\D���D�9HD�x�D��RD�� D�8RD�x�D���D���D�8 D�x�D���D���D�8�D�xRD��\D��
D�6�D�w
D���D�� D�8�D�x�D��HD���D�7�D�x�D�� D�� D�8�D�w�D��RD���D�8 D�x�D��=D��HD�8�D�x�D�� D�� D�7�D�w�D��RD�� D�8 D�x�D�� D���D�7�D�w�D���D���D�8�D�yHD���D��HD�8RD�x D���D�� D�9HD�y�D���D��HD�8 D�w\D���D���D�8RD�x D���D��RD�8 D�w�D��RD���D�7�D�x�D���D��RD�8RD�xRD�� D���D�8�D�x D�� D�� D�8�D�yHD��RD�� D�7�D�w\D��\D��\D�8 D�x D��RD��\D�7\D�x�D���D��\D�7�D�w�D���D���D�7�D�xRD���D��RD�7\D�xRD�� D��
D�7\D�x D���D���D�9�D�y�D���D��RD�7
D�w�D���D��RD�8 D�x D���D��HD�9�D�x D���D���D�9�D�y�D��HD��HD�9HD�x�D���D��HD�9HD�x D��RD�� D�8RD�w�D��\D�� D�8 D�x D���D��RD�7�D�xRD���D���D�8RD�x�D�� D�� D�9HD�x�D���D���D�7�D�x D��RD�� D�8RD�x�D���D��RD�8RD�w
D���D���D�8�D�x�D��RD�� D�8�D�x�D��HD���D�8 D�w�D��RD��RD�7�D�x�D���D��\D�7�D�x�D��RD���D�8 D�x D���D���D�7\D�w
D�� D���D�8 D�x�D���D���D�7�D�xRD¹�D��HD�7�D�w
Dù�D���D�9HD�x�Dĸ�D���D�8�D�x DŸ D�� D�7�D�x DƸRD��HD�8�D�x�DǸ�D��RD�7�D�xRDȸ D��RD�8RD�x Dɸ�D���D�8 D�x Dʷ\D���D�8�D�x�D˸ D���D�7�D�x D̸ D�� D�8 D�x�D͸�D��HD�9HD�x Dη\D��\D�7�D�xRDϹHD���D�8�D�w�Dи D���D�8�D�xRDѸ D��RD�8 D�xRDҸ�D���D�6�D�w\DӸRD��RD�7
D�v�DԸ D��RD�7\D�x Dո�D��RD�7�D�w�Dַ�D���D�8RD�x�D׸ D��\D�7\D�w\Dط�D���D�8 D�xRDٸ�D���D�8�D�yHDڹHD�� D�8RD�x�D۷�D��\D�7
D�v�Dܷ
D���D�7�D�x�Dݹ�D��=D�9HD�x�D޸RD��\D�7�D�x D߸RD��RD�8�D�xRD�RD��RD�8 D�xRDḤD�� D�7�D�x D⸤D��RD�8RD�x�D�RD�� D�8RD�w\D�
D�� D�8RD�w�D�RD���D�9HD�xRD渤D���D�8�D�x�D繚D���D�8 D�x D�
D��
D�7\D�x�D� D��RD�8�D�w\D�
D���D�7
D�v�D븤D���D�7
D�w
D�
D���D�8RD�w�D��\D���D�7�D�xRDD��\D�7�D�xRD﹚D���D�8�D�xRD�D��HD�8RD�w�D�D���D�7�D�w\D�\D��RD�9HD�x�D�D���D�9HD�yHD��HD���D�8�D�x D���D���D�8�D�xRD���D��\D�7\D�yHD��HD�� D�7
D�x D���D��RD�8 D�x D�� D��RD�8�D�yHD��RD��
D�7�D�xRD��HD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�G�A�HKA�R A�h
A�m)A�i�A�h
A�l�A�h>A�m)A�v�A�xA�x�A�yrA�y�A�{JA�|�A�}VA�}�A�.A〝A�}�A�y>A�u�A�w2A�pA�o�A�jA�f�A�gmA�h�A�ffA�c�A�f2A�W�A��A�Z�A�OvAͼ6A��)A�@A�w�A���A��XA��xA���A��A�9�A��MA�ޞA�W�A�یA��0A�9XA�i�A���A�^�A�˒A��-A��MA���A��kA�:�A�5�A���A�	7A�U�A���A��
A���A�	A��EA��A��aA�I�A��A��"A��SA�,=A��A���A:�A~�Ayb�AtoAr�jAq��Ao��Ak�Ai�CAe	Ac$tA_�NAYe�AX8�AUMjAQ�)AO]dALM�AKAI��AH�oAG��AF�LAC�]AA��A@�A>�^A=�A<ZA;�A87�A6�UA4��A3�A/l"A,��A+U2A+xA*�cA*˒A*n/A)�A)"�A(��A'��A'-wA&�zA&2�A%��A$VA#*�A"�)A"Q�A!��A VmACAPHA�A�rA��A��A?A��A��A�AC-AY�AzA�AA��A=A��A��A�A��A@�A?�A��A?�AhsAE�A�A�5A��AhsAO�A1AE9A�.A�&A �A/Ay�A��A�As�AJA��As�AS�AF�A�A��ATaAFA
��A
�NA\)Ag8A5�A7�A2aA(�A�A�A/�A
ߤA
QA	�&A�KA8�A�RA{AJAیA=�A�AAP�A��A�,AXA*0A�AbNA�xArGA �YA 7@�B�@��@��@��^@���@���@��r@��@��5@�+@��@�i�@���@��s@�Ft@���@�x@�X�@�!-@���@��Q@�=�@�r�@��@�u�@�j@�ȴ@��@��@�D@�1@�Ov@�@�r�@���@��H@�b@�k@��@�"�@�o�@�^5@��@�ی@��@�z�@�@�/@�Z@ߣn@߁�@�#�@�q@ݽ�@�+@�~(@�	@�}�@��@�҉@���@���@�h�@��@٠'@�
=@ؚ@آ4@�N�@�,�@ֺ�@�͟@��@�B�@ԂA@ӵt@�[W@��@��[@�i�@��>@р4@�6z@Й1@�R�@�9X@��@ϫ�@�w2@�E9@�o@έ�@�Q�@�J@ͯ�@�]�@�"�@���@��p@�Ov@�G@˜�@���@�@�@���@ȋD@�J�@��@��]@Ǽ�@Ƿ�@�n/@ƛ�@���@��9@���@ŷ@�u�@��@ğ�@�oi@�_@Å@���@��9@�o�@�8�@�Ĝ@��.@���@���@�W�@�@�Dg@�,�@���@��$@���@�D�@��X@��@���@�l"@��6@�a�@�Y@��@��f@��[@��L@��]@�\)@��@��,@��e@�oi@�L0@�5?@���@�8�@���@��.@�7�@��@���@��"@�=�@�ѷ@�Z�@��@�ƨ@�X�@���@��@�\�@�-@��W@��C@�Dg@�֡@�C�@���@�=@�ȴ@���@�l"@�{@��q@�/@��@�oi@�@�@��@��W@�dZ@��E@��r@��@���@���@�J�@�@��@�~(@�l"@�e�@�>B@� �@���@���@�j�@�U�@�7L@��@���@��@��x@�g8@���@��9@���@�e,@�5�@��@���@�V@��A@��N@��@�@O@��@�͟@���@���@�D�@�-@��@���@��"@�hs@�9�@���@�� @�C-@�7@��r@��>@��j@���@�N<@�@@��@��u@�Q@�J@��@���@��n@��@�n/@�;d@�	l@���@�v�@�!�@�_@���@���@��@��@��V@�zx@�33@��E@�l"@�b@��@��@���@�dZ@�A�@��@��j@�oi@�PH@��D@��K@�\)@��@���@�V�@�+k@�	@�� @���@��	@�n/@�@��@��@���@�GE@�@��>@��@��@��@�Z�@��M@�Ɇ@�c�@�J�@�?@�7�@��@��k@�;d@��@�֡@��!@�n�@�)�@�{@�1@���@��@��m@��@�e�@��p@�\�@��@��@��S@�x@�6z@���@��@��R@���@�z�@�4n@��&@���@�S�@�5�@��@�~(@�'R@��V@�Vm@�=@�$t@�ȴ@���@�L0@��@��a@�zx@�J�@�S@��H@���@�K^@��@��9@���@�o�@�>�@�V@��8@��$@�xl@�]d@�V@�:�@�	@��@���@�C�@���@���@�c @�Xy@�/�@�g@@~z@~�@}�7@}:�@}	l@|>B@|@{��@{�a@{~�@{H�@{�@z��@z#:@y�@y��@y��@y�@x�@xPH@xQ�@x�@w��@v��@v1�@u�@uu�@u%F@t�K@te�@t�@s�V@s=@r��@r@q��@qY�@q/@p�@pbN@o��@o1�@oY@n�r@n0U@mT�@lS�@k� @k��@k��@kb�@k8@j��@jOv@j@i�)@iԕ@izx@i+�@h�@h�u@hN�@h �@g�	@gW?@g(@f�\@fM�@f�@e��@eT�@e%@d�K@d�e@dz�@dV�@dM@c��@c�@c+@b��@a��@a`B@`�@`e�@`V�@`/�@_خ@_W?@^��@^}V@^4@]�j@]@]5�@\�$@\�.@\�@[�w@[b�@Z�x@Zc @ZJ�@Z!�@Y��@Y��@Y[W@X�@X1'@W��@W�@V��@Vs�@V�@U��@U�t@U^�@U�@T��@Th�@T,=@Tx@S�m@SiD@S�@R��@R{@Q��@Q��@Q�7@P�`@P>B@O˒@O��@O]�@O
=@N��@N)�@M}�@M=�@Lی@L_@K��@K1�@J�2@J��@J�@I=�@H�@H1@Gj�@G@F��@Fh
@E�j@Ehs@D�@Dq@C�@C�@CH�@B��@B�<@B�@Bq�@A��@A8�@@��@@��@@�_@@U2@?�@?�a@?��@?U�@? i@>͟@>�!@>q�@=�3@=j@=?}@=@@<��@<r�@<V�@<K^@<6@;��@;�@:��@:�\@:1�@9��@9�H@9��@9:�@8��@8�@7�@7y�@6�1@6e@5�#@5G�@4��@4�I@4�u@4z�@4h�@41'@3��@3�@3��@3�f@3_p@3"�@3Y@2�"@2��@2��@2?@1�>@1��@1@1%@1�@0�P@0�|@0�v@0�p@0�?@0z�@0!@/�]@/��@/|�@/8@.��@-�.@-��@-��@-`B@,��@,[�@+��@+o�@+H�@+=@+,�@+!-@+�@+ i@*�H@*�1@*xl@*�@)��@)��@)�@)�M@)�M@)w2@(��@(�u@(M@'�@'8@&�M@&�B@&��@&�r@&_�@&�@&�@%�T@%�@%p�@%G�@%V@$�/@$�e@$oi@$D�@$'R@$�@#��@#��@#˒@#�@@#x@#@"�@"��@"��@"�A@";�@!�T@!�z@!��@!�@!:�@!	l@ �@ ��@ ��@ ��@ Z@ $@��@��@|�@dZ@O@�@��@?@@�d@��@j@-w@�@�@m�@�@�w@�@�@C�@33@'�@�M@�h@�+@GE@@��@ϫ@�X@X@%@�p@�e@��@�D@��@e�@�A@~�@�@��@^5@_@��@Y�@#�@�@�@b@�}@�:@$t@�y@��@GE@��@�@�S@\�@�@��@��@e�@Z@H@"h@�@b�@S�@J#@@O@9�@"�@�@��@�B@��@��@�1@H�@�@�T@�t@X@@@�5@�/@�u@Xy@'R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�G�A�HKA�R A�h
A�m)A�i�A�h
A�l�A�h>A�m)A�v�A�xA�x�A�yrA�y�A�{JA�|�A�}VA�}�A�.A〝A�}�A�y>A�u�A�w2A�pA�o�A�jA�f�A�gmA�h�A�ffA�c�A�f2A�W�A��A�Z�A�OvAͼ6A��)A�@A�w�A���A��XA��xA���A��A�9�A��MA�ޞA�W�A�یA��0A�9XA�i�A���A�^�A�˒A��-A��MA���A��kA�:�A�5�A���A�	7A�U�A���A��
A���A�	A��EA��A��aA�I�A��A��"A��SA�,=A��A���A:�A~�Ayb�AtoAr�jAq��Ao��Ak�Ai�CAe	Ac$tA_�NAYe�AX8�AUMjAQ�)AO]dALM�AKAI��AH�oAG��AF�LAC�]AA��A@�A>�^A=�A<ZA;�A87�A6�UA4��A3�A/l"A,��A+U2A+xA*�cA*˒A*n/A)�A)"�A(��A'��A'-wA&�zA&2�A%��A$VA#*�A"�)A"Q�A!��A VmACAPHA�A�rA��A��A?A��A��A�AC-AY�AzA�AA��A=A��A��A�A��A@�A?�A��A?�AhsAE�A�A�5A��AhsAO�A1AE9A�.A�&A �A/Ay�A��A�As�AJA��As�AS�AF�A�A��ATaAFA
��A
�NA\)Ag8A5�A7�A2aA(�A�A�A/�A
ߤA
QA	�&A�KA8�A�RA{AJAیA=�A�AAP�A��A�,AXA*0A�AbNA�xArGA �YA 7@�B�@��@��@��^@���@���@��r@��@��5@�+@��@�i�@���@��s@�Ft@���@�x@�X�@�!-@���@��Q@�=�@�r�@��@�u�@�j@�ȴ@��@��@�D@�1@�Ov@�@�r�@���@��H@�b@�k@��@�"�@�o�@�^5@��@�ی@��@�z�@�@�/@�Z@ߣn@߁�@�#�@�q@ݽ�@�+@�~(@�	@�}�@��@�҉@���@���@�h�@��@٠'@�
=@ؚ@آ4@�N�@�,�@ֺ�@�͟@��@�B�@ԂA@ӵt@�[W@��@��[@�i�@��>@р4@�6z@Й1@�R�@�9X@��@ϫ�@�w2@�E9@�o@έ�@�Q�@�J@ͯ�@�]�@�"�@���@��p@�Ov@�G@˜�@���@�@�@���@ȋD@�J�@��@��]@Ǽ�@Ƿ�@�n/@ƛ�@���@��9@���@ŷ@�u�@��@ğ�@�oi@�_@Å@���@��9@�o�@�8�@�Ĝ@��.@���@���@�W�@�@�Dg@�,�@���@��$@���@�D�@��X@��@���@�l"@��6@�a�@�Y@��@��f@��[@��L@��]@�\)@��@��,@��e@�oi@�L0@�5?@���@�8�@���@��.@�7�@��@���@��"@�=�@�ѷ@�Z�@��@�ƨ@�X�@���@��@�\�@�-@��W@��C@�Dg@�֡@�C�@���@�=@�ȴ@���@�l"@�{@��q@�/@��@�oi@�@�@��@��W@�dZ@��E@��r@��@���@���@�J�@�@��@�~(@�l"@�e�@�>B@� �@���@���@�j�@�U�@�7L@��@���@��@��x@�g8@���@��9@���@�e,@�5�@��@���@�V@��A@��N@��@�@O@��@�͟@���@���@�D�@�-@��@���@��"@�hs@�9�@���@�� @�C-@�7@��r@��>@��j@���@�N<@�@@��@��u@�Q@�J@��@���@��n@��@�n/@�;d@�	l@���@�v�@�!�@�_@���@���@��@��@��V@�zx@�33@��E@�l"@�b@��@��@���@�dZ@�A�@��@��j@�oi@�PH@��D@��K@�\)@��@���@�V�@�+k@�	@�� @���@��	@�n/@�@��@��@���@�GE@�@��>@��@��@��@�Z�@��M@�Ɇ@�c�@�J�@�?@�7�@��@��k@�;d@��@�֡@��!@�n�@�)�@�{@�1@���@��@��m@��@�e�@��p@�\�@��@��@��S@�x@�6z@���@��@��R@���@�z�@�4n@��&@���@�S�@�5�@��@�~(@�'R@��V@�Vm@�=@�$t@�ȴ@���@�L0@��@��a@�zx@�J�@�S@��H@���@�K^@��@��9@���@�o�@�>�@�V@��8@��$@�xl@�]d@�V@�:�@�	@��@���@�C�@���@���@�c @�Xy@�/�@�g@@~z@~�@}�7@}:�@}	l@|>B@|@{��@{�a@{~�@{H�@{�@z��@z#:@y�@y��@y��@y�@x�@xPH@xQ�@x�@w��@v��@v1�@u�@uu�@u%F@t�K@te�@t�@s�V@s=@r��@r@q��@qY�@q/@p�@pbN@o��@o1�@oY@n�r@n0U@mT�@lS�@k� @k��@k��@kb�@k8@j��@jOv@j@i�)@iԕ@izx@i+�@h�@h�u@hN�@h �@g�	@gW?@g(@f�\@fM�@f�@e��@eT�@e%@d�K@d�e@dz�@dV�@dM@c��@c�@c+@b��@a��@a`B@`�@`e�@`V�@`/�@_خ@_W?@^��@^}V@^4@]�j@]@]5�@\�$@\�.@\�@[�w@[b�@Z�x@Zc @ZJ�@Z!�@Y��@Y��@Y[W@X�@X1'@W��@W�@V��@Vs�@V�@U��@U�t@U^�@U�@T��@Th�@T,=@Tx@S�m@SiD@S�@R��@R{@Q��@Q��@Q�7@P�`@P>B@O˒@O��@O]�@O
=@N��@N)�@M}�@M=�@Lی@L_@K��@K1�@J�2@J��@J�@I=�@H�@H1@Gj�@G@F��@Fh
@E�j@Ehs@D�@Dq@C�@C�@CH�@B��@B�<@B�@Bq�@A��@A8�@@��@@��@@�_@@U2@?�@?�a@?��@?U�@? i@>͟@>�!@>q�@=�3@=j@=?}@=@@<��@<r�@<V�@<K^@<6@;��@;�@:��@:�\@:1�@9��@9�H@9��@9:�@8��@8�@7�@7y�@6�1@6e@5�#@5G�@4��@4�I@4�u@4z�@4h�@41'@3��@3�@3��@3�f@3_p@3"�@3Y@2�"@2��@2��@2?@1�>@1��@1@1%@1�@0�P@0�|@0�v@0�p@0�?@0z�@0!@/�]@/��@/|�@/8@.��@-�.@-��@-��@-`B@,��@,[�@+��@+o�@+H�@+=@+,�@+!-@+�@+ i@*�H@*�1@*xl@*�@)��@)��@)�@)�M@)�M@)w2@(��@(�u@(M@'�@'8@&�M@&�B@&��@&�r@&_�@&�@&�@%�T@%�@%p�@%G�@%V@$�/@$�e@$oi@$D�@$'R@$�@#��@#��@#˒@#�@@#x@#@"�@"��@"��@"�A@";�@!�T@!�z@!��@!�@!:�@!	l@ �@ ��@ ��@ ��@ Z@ $@��@��@|�@dZ@O@�@��@?@@�d@��@j@-w@�@�@m�@�@�w@�@�@C�@33@'�@�M@�h@�+@GE@@��@ϫ@�X@X@%@�p@�e@��@�D@��@e�@�A@~�@�@��@^5@_@��@Y�@#�@�@�@b@�}@�:@$t@�y@��@GE@��@�@�S@\�@�@��@��@e�@Z@H@"h@�@b�@S�@J#@@O@9�@"�@�@��@�B@��@��@�1@H�@�@�T@�t@X@@@�5@�/@�u@Xy@'R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	�B	�B	�%B	�?B	�%B	�ZB	�%B	�?B	�%B	�%B	�%B	�?B	�?B	�?B	�ZB	�?B	�?B	�ZB	�?B	��B	��B	��B	��B	��B	�nB	�9B	�9B	�9B	�TB	�B	��B	��B	�@B	tB	v�B	1'B	O(B	|�B	��B
��B�BL�B6�B�B
�}B�B&�BqvBuZBy�B}�Bv�BgRBk�B��BoOBI�B4�B0�B./B �B �B
��B
�]BB�BKB
�B
�}B
�aB
�VB
��B
�B
HB
g�B
PHB
8�B
B
�B	�B	ϫB	�6B	�BB	��B	�7B	n/B	aB	D�B	5tB	#B	�B	�B��B�_B�'BՁB�NBѝB�#B�!B�2B�B��B�jBڠB�
B��B�TBбB��B�DB�_B�B�7B�#B�B�5B�;B��B��B�*B�'B�$B	�B	�B	0B	�B	$B	2B	'B	/�B	)�B	'mB	)*B	+�B	-]B	.�B	7LB	<B	A�B	E�B	O(B	iDB	{�B	��B	��B	�B	��B	xRB	t�B	q�B	sB	u?B	xB	�BB	�/B	�+B	�\B	�sB	�QB	��B	��B	��B	��B	��B	�?B	�'B	��B	��B	��B	��B	�B	�B	�=B	��B	�B	��B	�4B	��B	��B	ȴB	ȀB	�=B	ЗB	�FB	��B	�@B	�B	�B	�B	��B	��B	�`B	�B
B
�B
�B
�B	�0B	��B	�BB	�B	ںB	��B	�B	�B	��B	�B	�!B	�;B	�5B	��B	�B	�B	�UB	�B	�B	� B	�!B	��B	�ZB	��B	�B	��B	��B	�?B	��B	�B	�B	��B	��B	�oB	��B	�B	�B	�B	��B	�B	�B	�B	��B	�QB	�5B	�CB	�5B	�!B	�;B	�cB	�hB	�aB	�B	�)B	�_B	�B	�B	�B	�B	�6B	�*B	�B	�0B	�B	�:B	�B	�4B	��B	ߤB	��B	��B	�B	��B	�dB	�~B	��B	��B	�~B	��B	�VB	��B	�-B	�B	�|B	�vB	�B	�bB	�hB	�vB	�B	�hB	�-B	��B	��B	��B	ބB	�VB	�BB	��B	�HB	�BB	�-B	�B	�4B	�hB	��B	�nB	�B	�tB	�FB	��B	��B	�B	�2B	�B	�sB	��B	�kB	�B	��B	��B	�6B	��B	�B	�B	�B	�B	�XB	�DB	��B	��B	�"B	�B	��B	��B	��B	��B	�B	��B	�OB	�B	�B	��B	�OB	� B	�B	�B	��B	�|B	�B	�'B	�UB	��B	�B	�[B	�B	��B	��B	�AB	�vB	�B	�B	�GB	��B	�|B	��B	��B	��B	��B	�B	��B	��B	�B	�8B	�	B	��B	��B	��B	�JB	�<B	�}B
 B
�B
AB
[B
�B
�B
B
-B
{B
�B
�B
�B
?B
YB
�B
YB
�B
_B
�B
%B
%B
�B
�B
�B
�B
_B
zB
�B
�B

rB

�B
�B
�B
�B
B
�B
�B
�B
<B
�B
B
�B
�B
�B
HB
}B
�B
}B
}B
�B
NB
NB
B
hB
NB
�B
�B
�B
�B
�B
�B
�B
�B
 B
:B
TB
�B
B
�B
B
B
{B
�B
�B
2B
MB
gB
gB
�B
�B
�B
�B
�B
�B
�B
gB
�B
gB
MB
gB
�B
B
mB
mB
�B
�B
mB
9B
B
B
�B
�B
SB
�B
�B
?B
$B
�B
1B
B
�B
EB
�B
yB
+B
�B
�B
�B
B
B
�B
qB
�B
�B
B
B
�B
B
VB
VB
�B
 \B
 �B
!HB
"�B
!�B
"B
!�B
"hB
#B
#�B
#�B
#�B
$B
$&B
$@B
#�B
$�B
%B
%FB
%`B
%`B
%zB
%�B
%�B
&2B
&�B
&�B
&�B
&�B
&2B
&B
&2B
(�B
*eB
*KB
*B
)�B
)DB
(sB
(XB
(�B
(�B
)yB
*B
*�B
+�B
,B
,=B
,�B
-CB
-]B
-wB
-�B
.cB
.IB
./B
.�B
/B
/5B
/B
/iB
/iB
0UB
0;B
0oB
0oB
0oB
0�B
0�B
1'B
2�B
3�B
3�B
3�B
3�B
4B
3�B
3�B
4nB
4nB
4�B
4TB
4�B
4�B
4�B
5?B
5%B
5B
5tB
5�B
5tB
5tB
5ZB
5�B
5ZB
4�B
4�B
4�B
4TB
4�B
5ZB
6+B
6`B
6�B
72B
7�B
88B
8RB
8B
8B
8RB
8lB
8�B
8�B
8�B
8�B
8�B
9rB
9rB
9>B
9	B
8�B
8�B
9	B
9$B
9rB
9�B
:B
:�B
:�B
;0B
<6B
<jB
<B
<6B
;�B
<6B
<jB
<6B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
>(B
>�B
>�B
?.B
?B
?cB
?�B
?}B
?�B
?�B
?�B
@iB
@OB
@iB
@�B
@�B
@�B
@�B
A;B
AB
AB
A;B
A;B
A�B
BAB
B�B
CB
CB
CaB
C�B
DB
DgB
D�B
DMB
DgB
DgB
D�B
D�B
D�B
ESB
E9B
E9B
E�B
FB
F%B
FtB
FYB
F�B
GEB
GEB
GEB
G_B
G_B
GzB
G�B
HB
HfB
H�B
IB
IlB
I�B
I�B
I�B
I�B
J	B
J=B
JrB
JrB
J�B
J�B
J�B
KB
K)B
KDB
K�B
K�B
K�B
LB
L�B
L�B
MB
MPB
M�B
M�B
M�B
NVB
N�B
N�B
N�B
N�B
OB
O\B
OvB
OvB
O�B
O�B
P.B
P�B
QNB
Q�B
Q�B
Q�B
RB
R B
RoB
R�B
S@B
S@B
S�B
S�B
TaB
T{B
T�B
T�B
T�B
UB
U�B
U�B
U�B
VSB
V9B
VSB
W
B
WYB
WYB
WYB
WYB
W�B
W�B
W�B
W�B
XEB
X_B
XyB
XyB
X_B
X�B
Y�B
Y�B
Y�B
Z7B
Z7B
ZkB
Z�B
Z�B
Z�B
[WB
[WB
[�B
\CB
\�B
\�B
]B
]dB
]~B
]~B
]�B
]�B
]�B
^B
^B
^B
^OB
^OB
^jB
^jB
^jB
^�B
^�B
_B
_;B
_pB
_�B
_�B
_�B
_�B
`B
`B
_�B
_�B
`BB
`vB
`vB
`�B
`�B
`�B
a|B
a�B
a�B
bB
a�B
b�B
b�B
c:B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
dB
dZB
d�B
d�B
d�B
d�B
d�B
d�B
eB
eB
e�B
e�B
fLB
f�B
f�B
f�B
f�B
gB
gRB
gRB
g�B
g�B
g�B
g�B
h>B
hsB
h�B
h�B
h�B
h�B
iB
i*B
iDB
iDB
iyB
iyB
i�B
j0B
jB
j0B
jKB
jB
j�B
j�B
j�B
kB
kkB
kkB
k�B
k�B
k�B
k�B
lB
l"B
lqB
l�B
l�B
l�B
l�B
l�B
mwB
m�B
m�B
nB
nB
n}B
n�B
n�B
n�B
o5B
o�B
pB
p;B
p;B
poB
pUB
pUB
p�B
p�B
p�B
q'B
qAB
q�B
q�B
q�B
q�B
r-B
r|B
r�B
r|B
r�B
r�B
r�B
r�B
s�B
s�B
tB
t�B
t�B
u?B
utB
u�B
u�B
vB
v�B
v�B
wB
wfB
w�B
xB
x8B
x�B
x�B
x�B
y	B
y�B
y�B
y�B
zB
zB
zB
z*B
z�B
{B
{B
{0B
{0B
{0B
{0B
{0B
{dB
{B
{�B
{�B
{�B
|B
|6B
|PB
|�B
|�B
}B
}"B
}<B
}�B
}�B
~111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	�%B	�?B	�?B	�ZB	�ZB	�tB	�?B	�ZB	�?B	�?B	�?B	�?B	�ZB	�ZB	�tB	�ZB	�?B	��B	�tB	�%B	�B	��B	��B	��B	��B	�9B	�nB	�TB	��B	��B	�+B	��B	��B	|B	��B	B�B	cB	}B	�7B
��B�BOB9�B�B �B�B*�BtBxlB}�B��Bz�Bj�Bp�B�xBuZBL�B5�B2�B1�B%�B�B
�B
�B�B�BjB
��B
�TB
�9B
��B
�CB
��B
�B
k�B
UgB
=�B
xB
tB	�8B	��B	��B	�4B	��B	��B	q�B	f2B	HB	:xB	)_B	
XB	�B��B��B�B�sB�&B�[BܒB�HB�B�B�B�\B��B�yB�YB��B��B͹BοB��B�4BۦBۦB�~B޸B�B��B��B�0B�|B��B	�B		�B	jB	uB	�B	9B	'�B	1B	+�B	)B	*KB	,�B	.}B	/�B	7�B	<�B	BuB	E�B	N"B	h�B	{�B	�AB	��B	��B	�9B	y>B	u�B	r�B	tB	u�B	xB	��B	�B	��B	�vB	��B	��B	�]B	�HB	�IB	��B	�+B	��B	��B	��B	��B	�FB	�\B	�zB	ܒB	�0B	��B	��B	��B	��B	�B	�XB	�RB	�B	ʦB	�B	�aB	֡B	�ZB	��B	��B	��B	��B	��B	��B	�jB
�B
�B
	�B
�B	�qB	� B	��B	�9B	�B	��B	�[B	�B	�B	�]B	��B	�B	��B	��B	�B	�B	�vB	��B	��B	�B	�B	�|B	�B	�+B	��B	��B	��B	�`B	�GB	��B	��B	�fB	�hB	��B	�B	�B	�ZB	��B	�`B	�%B	��B	��B	�B	�B	�B	�wB	�iB	�UB	�B	�B	�B	�3B	�;B	��B	��B	��B	�B	�fB	�B	�"B	�B	�*B	�B	��B	��B	�ZB	��B	�-B	��B	�\B	��B	��B	ݘB	��B	�B	�OB	�5B	��B	�B	ߊB	�\B	�B	�B	�B	��B	��B	��B	�:B	��B	�bB	� B	��B	�-B	�vB	�VB	��B	ߤB	�B	�4B	�B	�B	�B	�hB	�B	�B	�TB	�B	�ZB	��B	�B	�LB	�B	�B	�B	��B	��B	�KB	��B	�kB	�kB	�B	�B	�B	�
B	��B	�B	��B	�B	�yB	�_B	�B	�B	��B	��B	�5B	�OB	�oB	�oB	�!B	�B	��B	�[B	�B	�B	�iB	�B	�B	�B	�MB	�!B	�B	�B	�B	��B	�B	�[B	�AB	�vB	��B	��B	�B	�B	�B	�-B	��B	��B	�B	��B	�8B	��B	�B	�B	�lB	��B	�>B	�DB	�*B	�B	��B	��B
  B
oB
'B
�B
�B
�B
-B
aB
�B
�B
�B
�B
%B
�B
�B
B
�B
_B
�B
zB
�B
�B
B
�B
B
+B
�B
�B
B
	7B

�B

�B
dB
B
6B
�B
"B
�B
"B
�B
�B
\B
�B
B
HB
�B
�B
�B
�B
�B
�B
�B
�B
NB
�B
�B
�B
B
:B
 B
�B
�B
 B
TB
�B
�B
�B
&B
[B
B
FB
aB
�B
�B
2B
�B
�B
�B
�B
�B
9B
�B
B
�B
�B
�B
�B
B
�B
�B
�B
B
mB
�B
�B
�B
�B
�B
�B
mB
�B
9B
B
�B
�B
$B
sB
YB
�B
B
eB
1B
�B
�B
�B
yB
1B
B
B
kB
�B
	B
�B
)B
IB
�B
jB
B
pB
�B
�B
�B
 �B
 �B
!|B
"�B
"B
"�B
"4B
"�B
#TB
#�B
$B
$B
$tB
$�B
$�B
$ZB
$�B
%FB
%zB
%�B
%�B
%�B
&fB
&LB
&�B
'B
&�B
'B
&�B
&fB
&LB
&fB
(�B
*�B
*�B
*�B
*B
)�B
(�B
(�B
)B
)B
)�B
*eB
+B
,B
,=B
,�B
,�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
/5B
/OB
/iB
/iB
/�B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
1'B
1�B
3B
4B
3�B
4B
4TB
4nB
4B
4TB
4�B
4�B
4�B
4�B
4�B
4�B
5B
5�B
5tB
5�B
5�B
5�B
5�B
5�B
5�B
5�B
5�B
5%B
4�B
4�B
4�B
5B
5�B
6`B
6�B
7B
7�B
8B
8�B
8�B
8RB
8RB
8�B
8�B
8�B
8�B
8�B
8�B
9>B
9�B
9�B
9�B
9>B
9	B
9	B
9XB
9rB
9�B
9�B
:xB
:�B
;B
;dB
<jB
<�B
<�B
<�B
<B
<�B
<�B
<�B
<�B
=�B
=�B
>BB
=�B
>B
>BB
>wB
>�B
?B
?cB
?HB
?�B
?�B
?�B
@ B
@B
@B
@�B
@�B
@�B
@�B
AB
AB
AUB
AoB
AUB
A;B
A�B
AoB
BB
B�B
CGB
CaB
CaB
C�B
D3B
DgB
D�B
D�B
D�B
D�B
D�B
EB
EB
EB
E�B
EmB
E�B
FB
FYB
FtB
F�B
F�B
F�B
G�B
GzB
GzB
G�B
G�B
G�B
HB
H�B
H�B
IB
IlB
I�B
I�B
I�B
I�B
J	B
J=B
J�B
J�B
J�B
J�B
J�B
J�B
K^B
K^B
K�B
K�B
L0B
L0B
L~B
L�B
MB
M6B
M�B
M�B
N"B
N"B
N�B
N�B
N�B
O(B
O(B
OvB
O�B
O�B
O�B
PB
PHB
P}B
Q B
Q�B
Q�B
R B
R:B
RoB
R�B
R�B
SB
S�B
SuB
TB
T,B
T�B
T�B
T�B
T�B
T�B
UMB
U�B
U�B
VB
V�B
VmB
V�B
W?B
W�B
W�B
W�B
W�B
XB
XB
XEB
X+B
X�B
X�B
X�B
X�B
X�B
YKB
Y�B
ZB
ZB
ZkB
ZkB
Z�B
Z�B
Z�B
[=B
[�B
[�B
\B
\�B
\�B
\�B
]IB
]�B
]�B
]�B
]�B
]�B
^B
^5B
^5B
^jB
^�B
^�B
^�B
^�B
^�B
^�B
_B
_;B
_�B
_�B
`B
`B
`B
`B
`'B
`BB
`'B
`B
`�B
`�B
`�B
`�B
a-B
aHB
a�B
bB
bB
bNB
bhB
b�B
c:B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
dtB
d@B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e`B
e`B
e�B
fB
f�B
f�B
f�B
f�B
gB
gRB
g�B
g�B
g�B
g�B
h
B
h$B
hsB
h�B
h�B
h�B
iB
iB
i*B
iyB
iyB
i�B
i�B
i�B
jB
jeB
jKB
jeB
jB
j�B
kB
k6B
k6B
k6B
k�B
k�B
k�B
k�B
k�B
l"B
l=B
lWB
l�B
l�B
l�B
l�B
mB
m]B
m�B
m�B
nB
nIB
nIB
n�B
n�B
n�B
oB
o�B
pB
p;B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q[B
qvB
q�B
q�B
q�B
rB
raB
r�B
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
tnB
t�B
t�B
u�B
u�B
u�B
vB
v`B
v�B
v�B
wfB
w�B
w�B
x8B
x�B
x�B
x�B
y$B
yXB
y�B
y�B
z*B
z*B
zDB
zDB
z�B
{B
{0B
{0B
{dB
{dB
{JB
{JB
{dB
{�B
{�B
{�B
{�B
{�B
|jB
|jB
|�B
|�B
}"B
}VB
}VB
}qB
}�B
~B
~(331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<4;@<#�
<�:<�D3<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.24(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201909210058042019092100580420190921005804202207271132432022072711324320220727113243202207271535172022072715351720220727153517  JA  ARFMdecpA30a                                                                20190910063717  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190910063756  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190910063756  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190910063757  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190910063757  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190910063757  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190910063758                      G�O�G�O�G�O�                JA  ARUP                                                                        20190910065511                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190911000000  CF  PSAL_ADJUSTED_QC@p�@uG�O�                JM  ARCAJMQC2.0                                                                 20190920155804  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190920155804  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023243  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063517  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081508                      G�O�G�O�G�O�                