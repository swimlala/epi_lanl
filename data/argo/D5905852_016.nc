CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-07-12T09:39:36Z creation;2019-07-12T09:39:37Z conversion to V3.1;2022-08-02T05:12:23Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190712093936  20220818081508  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_016                    2C  D   APEX                            8420                            2.11.2                          846 @��Q�� 1   @��R`��@,�l�C���dG��Z��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBPffBW��B`  Bh  Bo��Bw��B�  B�  B�  B�  B�  B�  B�  B���B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33BЙ�B���B�  Bۙ�B���B�  B�  B�33B�33B���B�  B�  C   C  C�C33C  C	�fC�fC  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4�C6�C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C[�3C^  C`  Ca�fCc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@  @fff@�33@�A��A9G�AYG�AyG�A��RA���A��RA���A��HA���A��HA���B�Bp�B�\Bz�B&�B.�B6G�B>=qBFBO  BV�B^33BfQ�Bn(�Bv(�B~=qB�(�B�(�B��B�.B�.B�=qB�\B���B��B�\B�.B�.B�(�B�.B�.B�.B�33B�.B�Q�B���B�\B�=qB��HB�{B�8RB�8RB�\)B�aHB�  B��B�(�B�8RC�
C�fCٚC�\C	��C��C��C�)C�)C�
C�\C�\C�
C��C�\C�
C!�{C#��C%�\C'�{C)�\C+�)C-��C/��C1�)C3�qC5�qC7xRC9�\C;��C=�{C?��CA�)CC��CE�HCG�)CI�
CK�{CM�
CO�)CQ�HCS��CU�
CW��CY��C[W
C]�
C_�)Ca��Cc��Ce�\Cg�\Ci��Ck��Cm��Co��Cq�
Cs�\Cu�{Cw�
Cy�{C{�
C}�HC�)C��\C���C�ФC��C��C��C��=C�ǮC�˅C�˅C�ǮC�˅C�˅C���C���C��=C�˅C���C��=C�˅C�˅C���C�ФC��C���C���C���C��C�ФC��\C���C�˅C��C��RC��\C�˅C���C�˅C��=C���C���C��C��C��C���C���C��=C���C�˅C��C��\C���C���C���C��fC�˅C�ФC�˅C���C���C��C��C��\C�ФC���C��=C���C��C���C�˅C���C��\C���C�ФC���C���C�˅C��C�˅C��=C��C��C��=C�˅C��\C�ФC��C��=C��C�˅C�˅C��C��\C���C���C���C�˅C���C��=C�˅C��\C��3C��3C���C���C���C���C�ФC��\C��C��C�˅C�˅C�˅C��=C���C��=C���C���C��=C���C�˅C��C���C�˅C��C��C�˅D d{D �De�D��DeD�DeD�
Dg
D��Dg
D��DeD�
Dg�D��DeD�{D	d{D	��D
ffD
��Dh�D�RDhRD�De�D�DffD�
Dg�D�
Dg
D�
De�D�DeD�{DffD�RDhRD�
Dg
D�RDg
D�fDffD�
Dg�D�
DffD�RDh�D�Dg
D�
De�D��DffD�DffD�RDi�D�D ffD �
D!ffD!�D"g
D"��D#e�D#�D$g�D$��D%g
D%�fD&e�D&�fD'g
D'�D(ffD(��D)hRD)�D*g�D*�RD+j=D+�RD,g
D,�D-g
D-�D.e�D.�D/e�D/�D0ffD0��D1g
D1�D2ffD2��D3g
D3�fD4eD4�D5d{D5�
D6g
D6�D7eD7�D8eD8��D9g
D9�RD:g
D:�fD;ffD;�D<ffD<�RD=g
D=��D>e�D>�fD?eD?��D@hRD@��DAffDA�{DBg
DB��DCc�DC�fDDg�DD�
DEg�DE�fDFffDF��DGffDG�RDHg
DH�
DIffDI�DJeDJ�DKh�DK�
DLg
DL�
DMg�DM�fDNe�DN�fDOc�DO�DPg
DP�fDQffDQ�DReDR��DSffDS��DThRDT�RDUh�DU�
DVe�DV�DWd{DW�{DXg�DX�DYd{DY�DZe�DZ�fD[g�D[�D\c3D\�{D]eD]�fD^eD^�
D_g�D_��D`g
D`�Dac�Da��Dbe�Db�fDcg
Dc�RDdg�Dd�fDehRDe�Dfe�Df�fDgg
Dg�fDhffDh�fDig
Di�Djg
Dj��Dkg�Dk�RDlg
Dl�fDmg�Dm��DneDn�DoeDo�
Dpg
Dp��Dqg�Dq�fDrd{Dr��Dsg
Ds�fDteDt�{DueDu�{Dve�Dv�DwffDw��DxffDx�DyffDy�Dze�Dz�D{eD{�fD|g
D|�
D}g
D}�fD~d{D~�De�D�{D�2�D�r�D��3D��D�3�D�s�D���D��3D�33D�s3D��3D��3D�33D�r=D���D��D�3�D�t)D���D��D�1�D�r=D���D���D�33D�s�D���D��D�2�D�r�D���D��=D�2�D�r�D���D��=D�2�D�r�D���D��)D�3�D�r�D���D��=D�3�D�s�D���D���D�3�D�t)D���D���D�3�D�s�D��=D��D�3�D�r�D��3D��3D�3�D�s�D��3D��D�2=D�q�D���D��3D�33D�r�D���D���D�1�D�r�D���D��3D�2�D�r=D���D��D�2�D�s3D���D��)D�3�D�s�D��{D��)D�3�D�s3D���D��3D�3�D�r�D��3D��)D�3�D�s3D���D��D�2=D�r�D��=D���D�3�D�t)D��3D���D�33D�s�D��3D��=D�2=D�r�D��3D��3D�3�D�t)D���D��)D�3�D�s3D���D���D�2=D�r=D��3D��D�2�D�r�D���D���D�2�D�s3D���D��3D�3�D�s3D���D��3D�33D�s�D���D��=D�1�D�r=D��3D��D�2=D�s�D��3D���D�2=D�r�D��=D��3D�4{D�t)D���D��D�33D�r�D���D���D�4{D�t�D���D��=D�2=D�r�D��3D���D�33D�r�D��3D��D�4)D�s3D���D��)D�33D�r�D��=D���D�1�D�r�D��3D��{D�3�D�s3D���D��3D�4)D�t{D���D��D�3�D�s�D��3D��D�3�D�t)D��)D��{D�3�D�s�D���D��D�2�D�s�D���D��=D�33D�s3D���D��D�33D�s3D���D��{D�3�D�s3D���D���D�33D�s�D���D��3D�3�D�r�D���D���D�2�D�r�D��3D��3D�3�D�s�D��3D��3D�33D�r�D���D��D�1�D�r�D���D��3D�3�D�s�D��3D��=D�2�D�s�D���D��D�2�D�s�D���D��3D�4{D�s3D��=D��3D�2�D�r�D³�D��)D�3�D�r�Dó3D��3D�33D�s�DĲ�D��D�2=D�r�Dų3D��D�4)D�t)DƳ�D��=D�2=D�r�Dǳ3D���D�2�D�r�DȲ�D���D�33D�r�Dɳ3D��D�33D�s3Dʳ�D���D�2�D�s3D˳3D��3D�2�D�r�D̲=D��D�2�D�s3Dͳ3D��=D�2�D�r�Dβ�D���D�4)D�t)Dϳ3D���D�33D�r�Dб�D��D�33D�s3DѲ=D��=D�2�D�s�Dҳ�D��D�1�D�s�Dӳ�D��D�33D�r�DԲ�D��D�3�D�s�Dղ�D��=D�1�D�r=Dֲ�D��3D�4)D�s�D׳3D��D�33D�s�Dس�D��D�33D�s�Dٴ{D��)D�4)D�t)Dڴ)D��{D�33D�r�D۳�D��)D�33D�q�Dܱ�D���D�2=D�r=Dݲ=D��=D�2�D�r�D޲�D��D�33D�s3D߳�D��)D�3�D�r�D��D���D�3�D�r�D��D��D�2�D�s3DⳅD���D�3�D�s3D�3D��D�1�D�s3D��D��{D�3�D�r�D��D��D�2�D�s�D�)D���D�3�D�r�D粏D��D�2�D�t)D��D���D�33D�r�D鲏D��3D�2�D�r�D겏D��3D�4)D�s�D�)D���D�3�D�s�D��D��3D�2�D�s�D���D���D�2=D�s�DD��D�3�D�r�D�3D��D�1�D�s�D�)D��D�33D�s�D�)D���D�3�D�s�D�D���D�3�D�s�D��D���D�33D�s�D��3D���D�2�D�r�D���D��3D�33D�s3D���D���D�3�D�t)D���D��3D�3�D�r�D��=D���D�3�D�r�D���D��D�2�D�s�D��D��{D�3�D�s�D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��(A��]A�SA�fA��A��A�%A�A��A�fA�
�A��A�~A���A���A��;A�چA���A�ԕA��jA�ɺA��	A�.A�}"A�r|Aؠ�A�8�A��A���A�D�A�RTAӏ�A�A��DA�%�A�S&AΨXAͅ�A��NA��A�A��A�ZQA���A�!-AŋDA���A�Y�A�EmA�q�A�� Aú�A�~�A��RA�6FA���A�l"A�33A��>A��mA��WA�ƨA�� A��wA�-CA��A���A���A���A�a�A�бA�h�A�ÖA��A�� A��`A���A��/A��A�7�A�g�A��A��,A���A�c A��kA���A��A���A�bAAx��AuW?ArX�AnخAk�DAg.IAe8�Ad�AcHAa�A_�}A]K�AX�HAR��AP3�ANC-AI�AF�ZAE�AD��AA�~A=�OA:I�A8QA7�A5��A4�$A4��A4VmA4)�A3�SA2ZA1�6A0ݘA0aA/a|A-�}A-��A-��A-`�A-SA-8�A-qvA-QA-�A,�-A+�A*�rA(�)A&A�A%�xA%MA$�A$��A$\�A#�A#zxA"��A"�+A"�A"��A"PHA!��A ��A��A��A4A��A1�A)_A��A�A��A��AXA�A�oA6A�#A��A��A*0A�+A33A�2A	A��A��ATaA��A˒AS�A�A�EAl"A�AK�A��Az�AFtA�]A��AH�A�A�A��A�A��Aa|A-wA�2AU�A��A�hA��Aw�AU�A��A$tA
�A
��A
7A	�nA	]�A�A��AHA�A��A'�A��A��AK^A�A��AbA�EAu�A!A��A8�AuA�7A7LA�AA�A7LA+A ɆA ��A ��A �rA @@��@��}@�&�@���@�c@�	l@�N�@���@���@�Z�@���@���@���@��@�#�@��@���@�z@�=q@��j@�=�@���@�-@�[W@�M�@��m@��@�S@��@�R�@��@��@��@�V�@� �@�4@��	@��@�1@��@�@�n/@��@�q�@�-�@�t�@��2@���@�R@�H@��]@���@�b�@�5?@�H@�o @�@�z�@�S�@ߎ�@��@ޚ�@��@�33@��@�@�Vm@�m�@�Q�@؏\@�xl@�)�@��r@�s�@��U@ִ9@�tT@��&@ԍ�@�Z�@�3�@�0�@��W@�6z@��p@ίO@Ίr@�PH@͘�@�_@ˋ�@�dZ@˃{@��"@�Ta@�6�@�x@�ԕ@ɭC@�X@���@ȟ�@��@�IR@�@O@�Y@��[@ƂA@��#@�'�@��j@�c@�Mj@��@�֡@+@���@��@��E@���@�z�@�!@�=@�R�@���@�҉@�M�@��j@�o @�;d@��H@���@�-@��*@�W?@��v@���@��@��@�qv@���@��\@�_@�0U@��3@�hs@���@��L@���@��4@�j@�J@��]@��a@���@�RT@�(�@��@��@��+@�@���@���@���@�Xy@�9X@�ݘ@�|@�e�@�Mj@���@��@��@�7�@��@��S@�9�@� i@�l�@�m]@�)_@��	@���@�j@�N�@��]@���@�Z�@�1�@��y@���@�bN@�PH@�E�@�2�@�_@��X@�;d@�Y@���@�	@�~@��@���@�'�@��X@�a|@���@���@�Y�@���@��r@�c�@�:*@�خ@�|@�q@��@���@�\�@�2�@��@��}@���@���@�-w@��f@��@�  @��@��r@�6@���@���@��@��@��x@��D@��S@�`B@�L�@��@�d�@�8�@��z@�f�@�F@��/@��D@��@��@��4@�A�@��	@���@�xl@�_�@�Ft@�@�s@�	l@��@���@�~�@�'R@���@��@���@�_p@��@��H@��X@���@���@���@�S�@�GE@�M@���@���@��@���@�}V@��@�@�Y�@��6@�	@���@���@��@�f�@�S&@�H�@�A @�(@���@��@�}V@�tT@�V�@�M@�>B@�x@���@��7@�a�@�J�@�Dg@�&�@��@�֡@���@�~�@�_�@�V�@�8�@�	@���@��#@��$@�k�@�S&@�:�@�%@��@��Y@��@�PH@���@���@�o @�b�@�*0@��8@�|�@�6@��@��a@�~�@�@@���@��@�q�@�bN@�R�@�O@�4�@��@�_�@�=q@�e@�@"�@~�s@~��@~6�@~ �@}��@};@|�o@|�@{��@{�{@{>�@zߤ@z�R@z~�@z8�@z_@y�3@yx�@y�@x�p@x�.@xPH@x2�@w\)@w@vYK@uu�@uY�@tی@s�A@se�@s�@r�s@r��@r#:@q�3@q�@pS�@o�r@oiD@o�@n�,@nkQ@n&�@m�3@m7L@m�@l��@l~(@lx@k�;@k�q@kn/@k@j�X@i��@i`B@h�o@g�r@g��@g'�@fC�@e�N@e�S@ehs@dی@dl"@d�@c/�@b�@a@a�@aj@`�5@`��@`��@`H@`b@`  @_�+@_��@_\)@_6z@_/�@_&@_S@^�H@^��@^@]-w@\Ft@[�F@[33@Z�<@Z\�@Y�@Y��@Y`B@Y0�@X�K@X��@XI�@X1@W��@W��@W��@Wg�@W1�@V�M@V��@VZ�@V�@U�t@UDg@T�?@T��@T�z@Te�@S~�@S!-@R�8@R�R@R�@Q�9@QL�@P��@P�z@O��@O]�@N�y@N6�@M��@Mhs@M7L@LS�@K��@K�@K6z@Jxl@Ju@I�@I��@I8�@HĜ@Hz�@H	�@G��@GZ�@G
=@F�@F?@Eԕ@EX@EG�@E	l@DɆ@D��@D�4@D��@C��@C|�@C�@B��@A��@@��@@y>@@h�@@'R@?�@?��@?F�@>�H@>u%@>!�@=�@=��@<��@<_@<�@;�+@;ݘ@;�@;��@:�}@:_�@:!�@9��@9��@9!�@8`�@7�&@7��@6�r@6.�@5�j@5�^@5��@5c@5p�@5/@5%@4�)@4��@4PH@41@3ݘ@3�*@3,�@2�y@2҉@2͟@2��@2GE@1��@1s�@1	l@0�f@0�@0��@0I�@0b@/��@/�&@/˒@/�{@/]�@/+@.��@.h
@.�@-�@-��@-c@-G�@- \@-V@,�`@,�4@,A�@+�m@+�@+��@+Z�@+�@*�@*��@*��@*z@*_�@*H�@*1�@*u@)ԕ@)��@)s�@)T�@)7L@)	l@(�`@(w�@("h@(7@'�+@'v`@'E9@&�@&�s@&�]@&�@&�@&��@&c @&=q@& �@%ԕ@%��@%}�@%S&@%!�@%@%�@$��@$m�@$2�@#��@#�
@#g�@"��@"�,@"�!@"��@"z@"?@!�>@!��@!�@ ��@ ��@ /�@�F@{J@>�@�@��@�@��@��@�\@J�@��@��@o @\�@/@��@g8@j@tT@*�@�@@O@�@�@^5@6�@�@c@`B@A @5�@�P@ی@��@|�@q@m�@Q�@?�@Ft@G@��@|�@J#@C@@��@C�@6�@0U@+k@_@�^@�@k�@�|@w�@��@l�@��@�X@��@�}@��@E�@�@zx@<6@(�@q@�@��@��@�.@g8@/�@@�+@خ@�F@�@n/@dZ@_p@RT@A�@$t@S@ߤ@�@8�@1�@1�@��@�@f�@/@��@Ɇ@�$@�@oi@Xy@I�@-�@��@��@�:@P�@�@(111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��(A��]A�SA�fA��A��A�%A�A��A�fA�
�A��A�~A���A���A��;A�چA���A�ԕA��jA�ɺA��	A�.A�}"A�r|Aؠ�A�8�A��A���A�D�A�RTAӏ�A�A��DA�%�A�S&AΨXAͅ�A��NA��A�A��A�ZQA���A�!-AŋDA���A�Y�A�EmA�q�A�� Aú�A�~�A��RA�6FA���A�l"A�33A��>A��mA��WA�ƨA�� A��wA�-CA��A���A���A���A�a�A�бA�h�A�ÖA��A�� A��`A���A��/A��A�7�A�g�A��A��,A���A�c A��kA���A��A���A�bAAx��AuW?ArX�AnخAk�DAg.IAe8�Ad�AcHAa�A_�}A]K�AX�HAR��AP3�ANC-AI�AF�ZAE�AD��AA�~A=�OA:I�A8QA7�A5��A4�$A4��A4VmA4)�A3�SA2ZA1�6A0ݘA0aA/a|A-�}A-��A-��A-`�A-SA-8�A-qvA-QA-�A,�-A+�A*�rA(�)A&A�A%�xA%MA$�A$��A$\�A#�A#zxA"��A"�+A"�A"��A"PHA!��A ��A��A��A4A��A1�A)_A��A�A��A��AXA�A�oA6A�#A��A��A*0A�+A33A�2A	A��A��ATaA��A˒AS�A�A�EAl"A�AK�A��Az�AFtA�]A��AH�A�A�A��A�A��Aa|A-wA�2AU�A��A�hA��Aw�AU�A��A$tA
�A
��A
7A	�nA	]�A�A��AHA�A��A'�A��A��AK^A�A��AbA�EAu�A!A��A8�AuA�7A7LA�AA�A7LA+A ɆA ��A ��A �rA @@��@��}@�&�@���@�c@�	l@�N�@���@���@�Z�@���@���@���@��@�#�@��@���@�z@�=q@��j@�=�@���@�-@�[W@�M�@��m@��@�S@��@�R�@��@��@��@�V�@� �@�4@��	@��@�1@��@�@�n/@��@�q�@�-�@�t�@��2@���@�R@�H@��]@���@�b�@�5?@�H@�o @�@�z�@�S�@ߎ�@��@ޚ�@��@�33@��@�@�Vm@�m�@�Q�@؏\@�xl@�)�@��r@�s�@��U@ִ9@�tT@��&@ԍ�@�Z�@�3�@�0�@��W@�6z@��p@ίO@Ίr@�PH@͘�@�_@ˋ�@�dZ@˃{@��"@�Ta@�6�@�x@�ԕ@ɭC@�X@���@ȟ�@��@�IR@�@O@�Y@��[@ƂA@��#@�'�@��j@�c@�Mj@��@�֡@+@���@��@��E@���@�z�@�!@�=@�R�@���@�҉@�M�@��j@�o @�;d@��H@���@�-@��*@�W?@��v@���@��@��@�qv@���@��\@�_@�0U@��3@�hs@���@��L@���@��4@�j@�J@��]@��a@���@�RT@�(�@��@��@��+@�@���@���@���@�Xy@�9X@�ݘ@�|@�e�@�Mj@���@��@��@�7�@��@��S@�9�@� i@�l�@�m]@�)_@��	@���@�j@�N�@��]@���@�Z�@�1�@��y@���@�bN@�PH@�E�@�2�@�_@��X@�;d@�Y@���@�	@�~@��@���@�'�@��X@�a|@���@���@�Y�@���@��r@�c�@�:*@�خ@�|@�q@��@���@�\�@�2�@��@��}@���@���@�-w@��f@��@�  @��@��r@�6@���@���@��@��@��x@��D@��S@�`B@�L�@��@�d�@�8�@��z@�f�@�F@��/@��D@��@��@��4@�A�@��	@���@�xl@�_�@�Ft@�@�s@�	l@��@���@�~�@�'R@���@��@���@�_p@��@��H@��X@���@���@���@�S�@�GE@�M@���@���@��@���@�}V@��@�@�Y�@��6@�	@���@���@��@�f�@�S&@�H�@�A @�(@���@��@�}V@�tT@�V�@�M@�>B@�x@���@��7@�a�@�J�@�Dg@�&�@��@�֡@���@�~�@�_�@�V�@�8�@�	@���@��#@��$@�k�@�S&@�:�@�%@��@��Y@��@�PH@���@���@�o @�b�@�*0@��8@�|�@�6@��@��a@�~�@�@@���@��@�q�@�bN@�R�@�O@�4�@��@�_�@�=q@�e@�@"�@~�s@~��@~6�@~ �@}��@};@|�o@|�@{��@{�{@{>�@zߤ@z�R@z~�@z8�@z_@y�3@yx�@y�@x�p@x�.@xPH@x2�@w\)@w@vYK@uu�@uY�@tی@s�A@se�@s�@r�s@r��@r#:@q�3@q�@pS�@o�r@oiD@o�@n�,@nkQ@n&�@m�3@m7L@m�@l��@l~(@lx@k�;@k�q@kn/@k@j�X@i��@i`B@h�o@g�r@g��@g'�@fC�@e�N@e�S@ehs@dی@dl"@d�@c/�@b�@a@a�@aj@`�5@`��@`��@`H@`b@`  @_�+@_��@_\)@_6z@_/�@_&@_S@^�H@^��@^@]-w@\Ft@[�F@[33@Z�<@Z\�@Y�@Y��@Y`B@Y0�@X�K@X��@XI�@X1@W��@W��@W��@Wg�@W1�@V�M@V��@VZ�@V�@U�t@UDg@T�?@T��@T�z@Te�@S~�@S!-@R�8@R�R@R�@Q�9@QL�@P��@P�z@O��@O]�@N�y@N6�@M��@Mhs@M7L@LS�@K��@K�@K6z@Jxl@Ju@I�@I��@I8�@HĜ@Hz�@H	�@G��@GZ�@G
=@F�@F?@Eԕ@EX@EG�@E	l@DɆ@D��@D�4@D��@C��@C|�@C�@B��@A��@@��@@y>@@h�@@'R@?�@?��@?F�@>�H@>u%@>!�@=�@=��@<��@<_@<�@;�+@;ݘ@;�@;��@:�}@:_�@:!�@9��@9��@9!�@8`�@7�&@7��@6�r@6.�@5�j@5�^@5��@5c@5p�@5/@5%@4�)@4��@4PH@41@3ݘ@3�*@3,�@2�y@2҉@2͟@2��@2GE@1��@1s�@1	l@0�f@0�@0��@0I�@0b@/��@/�&@/˒@/�{@/]�@/+@.��@.h
@.�@-�@-��@-c@-G�@- \@-V@,�`@,�4@,A�@+�m@+�@+��@+Z�@+�@*�@*��@*��@*z@*_�@*H�@*1�@*u@)ԕ@)��@)s�@)T�@)7L@)	l@(�`@(w�@("h@(7@'�+@'v`@'E9@&�@&�s@&�]@&�@&�@&��@&c @&=q@& �@%ԕ@%��@%}�@%S&@%!�@%@%�@$��@$m�@$2�@#��@#�
@#g�@"��@"�,@"�!@"��@"z@"?@!�>@!��@!�@ ��@ ��@ /�@�F@{J@>�@�@��@�@��@��@�\@J�@��@��@o @\�@/@��@g8@j@tT@*�@�@@O@�@�@^5@6�@�@c@`B@A @5�@�P@ی@��@|�@q@m�@Q�@?�@Ft@G@��@|�@J#@C@@��@C�@6�@0U@+k@_@�^@�@k�@�|@w�@��@l�@��@�X@��@�}@��@E�@�@zx@<6@(�@q@�@��@��@�.@g8@/�@@�+@خ@�F@�@n/@dZ@_p@RT@A�@$t@S@ߤ@�@8�@1�@1�@��@�@f�@/@��@Ɇ@�$@�@oi@Xy@I�@-�@��@��@�:@P�@�@(111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	u�B	u�B	vB	vFB	v+B	u�B	vB	vB	v+B	vFB	vzB	v`B	u�B	u%B	s�B	r�B	r�B	r�B	raB	q�B	q�B	n�B	mB	gB	RB	@�B	=<B	<�B	K�B	`�B	v�B	��B	�4B	��B	��B	��B	��B	��B	�{B	�B	�B	��B
�B
#B
!HB
!B
8B
6�B
4nB
cnB
�$B
�&B
��BaBBSB#�B-�BB[B`�Bx�B��B�FB�)B��B�cB�3B��B�0B�8B��Bv�Bn�Bg�B`�BO�B4B�B�B
�B
�9B
�QB
�HB
|jB
z�B
fB
PHB
@OB
0UB
�B
	�B	ݘB	��B	�B	��B	� B	~]B	s3B	lB	gRB	^B	TaB	K�B	6�B	%�B	=B	!bB	+�B	9�B	;�B	G�B	E�B	6+B	=�B	F�B	R�B	\�B	q�B	wfB	{�B	~wB	��B	�'B	�SB	�PB	�B	�MB	�mB	��B	��B	��B	�8B	��B	�	B	�\B	��B	ңB	�B	ɺB	��B	��B	�AB	�MB	�SB	ǔB	�DB	�}B	�dB	ʦB	�pB	οB	ٴB	�WB	�)B	�5B	�B	��B	�4B	�B	�KB	��B
3B
�B
�B

�B
0B
B
oB
�B
}B
}B
4B
�B
?B
B
1B
CB
IB
dB
OB
�B
 �B
#B
#�B
#�B
$�B
%`B
'�B
)*B
)�B
*�B
+�B
-)B
-�B
.cB
.�B
.�B
/�B
0;B
0�B
0�B
0�B
2-B
2|B
2�B
2�B
2�B
2aB
33B
4�B
4B
3�B
3�B
3�B
3hB
4B
3�B
3�B
3�B
4B
3�B
2�B
2�B
3B
2�B
3�B
3B
2-B
2GB
1�B
2|B
1�B
1�B
1�B
1[B
1AB
0�B
0B
0B
0B
0;B
0UB
/�B
/�B
.�B
.}B
-�B
-�B
-]B
-CB
,�B
,=B
+�B
+kB
*eB
*�B
)�B
)�B
)_B
)DB
(�B
(�B
(�B
(XB
'�B
'RB
'B
&�B
%�B
%zB
%FB
$�B
$tB
#�B
#�B
#nB
"�B
"hB
"hB
"4B
!�B
!|B
!-B
 �B
 �B
 'B
�B
VB
�B
5B
�B
�B
	B
kB
�B
B
EB
yB
�B
sB
sB
�B
�B
�B
�B
�B
MB
�B
�B
�B
�B
&B
�B
B
�B
�B
 B
hB
<B
VB
�B
�B
 B
�B
"B
JB

�B
�B
_B
�B
_B
_B
�B
�B
 �B
aB
?B
�B
�B
	�B

	B

	B

	B

#B
	RB
KB
+B
�B
�B
�B
�B
�B
gB
gB
AB
UB
 B
;B
 �B
 OB
 �B
 �B
 �B
 �B
 B	�}B	��B	�6B	��B	�XB	��B	��B	��B	�8B	��B	��B	��B	�xB	��B	�B	�JB	�PB	�jB	��B	��B	��B	��B	��B	�VB	�B	�cB
 �B
B
_B
fB
+B
	7B
�B
KB
�B
+B
�B
�B
�B
mB
mB
9B
B
B
�B
%B
zB
�B
�B
	7B
	B
	7B
xB
�B
PB
�B
jB
"B
�B
B
�B
�B
�B
jB
�B
�B
pB
"B
pB
VB
�B
�B
�B
�B
�B
(B
�B
HB
�B
�B
�B
�B
�B
@B
uB
�B
&B
&B
&B
uB
FB
aB
gB
�B
�B
�B
�B
�B

B

B
?B
�B
�B
�B
QB
qB
QB
qB
�B
�B
�B
�B
xB
CB
)B
�B
�B
�B
/B
�B
�B
 \B
 B
 B
�B
pB
�B
�B
 �B
!�B
!�B
!�B
"�B
# B
#TB
#nB
#nB
#�B
$�B
%zB
%�B
%zB
%�B
&B
&2B
&B
%�B
%�B
&LB
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'B
'B
'B
'�B
'�B
(
B
(
B
(�B
(�B
)DB
)_B
)_B
)DB
)�B
)�B
)�B
)�B
)�B
)yB
)�B
)�B
)�B
)�B
)�B
*B
*0B
*0B
*�B
+kB
,WB
,qB
,qB
,�B
-CB
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.B
.�B
.�B
/iB
/�B
0oB
0�B
0oB
0�B
0UB
0�B
1AB
1�B
2�B
2�B
4B
4�B
4�B
5tB
5?B
5�B
5�B
6+B
6FB
6zB
6`B
6+B
6B
7�B
7�B
8B
8B
7�B
88B
8lB
8�B
8�B
8�B
8lB
8�B
8lB
8�B
8�B
9XB
9XB
9rB
9�B
9�B
9�B
:B
:*B
:*B
:^B
:�B
:�B
:�B
:�B
:�B
;dB
;dB
;�B
<6B
<B
<jB
=<B
=�B
=�B
=�B
=�B
>B
>BB
>]B
>wB
>�B
?HB
?HB
?cB
?}B
?cB
?�B
@ B
@ B
@4B
@�B
A B
A B
AUB
A�B
A�B
A�B
A�B
A�B
B�B
CB
CaB
C�B
DgB
D�B
D�B
D�B
EB
ESB
EB
E�B
FYB
F?B
FYB
F�B
F�B
GB
GB
G_B
GEB
GEB
GEB
G�B
G�B
G�B
G�B
G�B
G�B
GzB
G�B
G�B
H�B
H�B
I�B
I�B
I�B
J	B
JXB
J�B
J�B
J�B
J�B
K^B
K^B
K�B
K�B
K�B
K�B
L0B
LJB
LJB
L�B
L�B
L�B
L�B
MB
MPB
MPB
MB
M6B
NB
N<B
N"B
N<B
N�B
N�B
O\B
O�B
O�B
P�B
P�B
P�B
QhB
Q�B
Q�B
Q�B
RoB
R�B
R�B
R�B
SuB
S�B
S�B
S�B
TB
TaB
T{B
T�B
UB
UMB
UMB
UgB
U�B
VB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W?B
W$B
W�B
X+B
XyB
X�B
X�B
X�B
YeB
Y�B
Y�B
ZB
ZkB
Z�B
[=B
[=B
[WB
\B
\�B
\�B
\�B
\�B
\�B
]B
]�B
]IB
]/B
\�B
]�B
]�B
]�B
]IB
\�B
]�B
]�B
^5B
^5B
^jB
^�B
^�B
^�B
^�B
_B
_!B
_;B
_�B
_�B
_�B
`B
`BB
`BB
`'B
`�B
a|B
a�B
b4B
b4B
bhB
bhB
cB
cnB
c�B
c�B
c�B
c�B
d&B
d&B
d@B
d�B
d�B
d�B
d�B
eB
eFB
e`B
e�B
e�B
fB
e�B
ffB
f�B
f�B
g8B
gRB
g�B
g�B
g�B
g�B
h>B
h$B
h>B
hsB
h�B
h�B
h�B
iB
iB
i*B
iDB
iDB
i�B
jB
i�B
i�B
i�B
j0B
jB
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
kkB
kQB
kkB
k�B
k�B
l�B
l�B
l�B
l�B
l=B
l=B
mB
l�B
l�B
l�B
l�B
l�B
l�B
lqB
lWB
l�B
m)B
mB
m�B
m�B
m�B
m�B
m�B
m�B
m�B
nB
n�B
o�B
p;B
p�B
poB
poB
poB
p�B
p�B
p�B
p�B
qvB
qvB
p�B
poB
p�B
p�B
p�B
p�B
p�B
q'B
qAB
qAB
qAB
q�B
q�B
q�B
rB
rB
rB
rGB
raB
r�B
r�B
s3B
s3B
sMB
shB
shB
tB
t9B
t9B
t9B
t9B
tB
tTB
tTB
tTB
t�B
u%B
u�B
vB
v�B
v�B
v�B
v�B
v�B
w2B
w�B
xB
x8B
xRB
xRB
xlB
xlB
x�B
x�B
x�B
y$B
y$B
y>B
yXB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
zDB
z�B
z�B
{B
z�B
z�B
{B
{0B
{�B
{�B
|B
|PB
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}B
}VB
}�B
~B
~111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	vB	u�B	v+B	vzB	v`B	v+B	vFB	v+B	vFB	v`B	v�B	v�B	vFB	utB	s�B	r�B	r�B	r�B	r�B	raB	r|B	p;B	p�B	m�B	V�B	BB	>wB	>�B	M�B	c:B	zxB	��B	��B	��B	��B	��B	�hB	��B	��B	�B	�OB	�cB
B
�B
"�B
"�B
8�B
6�B
5%B
d�B
�XB
�`B
��ByB1B�B*0B7LBG+Bf�BB�pB�dB�BB�B��B�+B�B�]B��B��Bx�BpoBj0Bd�BUMB8�B#nB)B
��B
ɆB
�OB
�B
~]B
~�B
i�B
S�B
C�B
3�B
$�B
�B	�B	�B	��B	�@B	��B	��B	t�B	m�B	jKB	`�B	XyB	Q�B	=VB	)*B	�B	&fB	/5B	<6B	>(B	LB	J�B	:^B	@�B	H�B	T{B	^B	rGB	w�B	|jB	�B	�[B	�GB	��B	�pB	��B	�$B	��B	�/B	�oB	�rB	�8B	��B	ʌB	�B	ҽB	�B	�TB	�JB	B	��B	��B	ĶB	��B	�KB	�B	�NB	�6B	��B	��B	�BB	�kB	�]B	�~B	ߊB	�hB	��B	�B	��B	�B	�]B
�B
YB
�B
�B
PB
.B
[B
}B
4B
�B
 B
�B
�B
�B
QB
�B
�B
B
B
 vB
!�B
#�B
$tB
$�B
%zB
&fB
(XB
)�B
*B
+QB
,�B
-�B
.}B
.�B
/OB
/�B
0�B
0�B
1[B
1[B
1�B
2�B
2�B
2�B
2�B
33B
33B
4nB
5B
4�B
4nB
4�B
4�B
49B
4�B
4nB
4TB
4�B
4�B
49B
3�B
3�B
3�B
3�B
4�B
3�B
2�B
2�B
2�B
33B
2aB
2aB
2GB
1�B
1�B
1[B
0�B
0�B
0oB
0�B
0�B
0�B
0�B
/OB
/B
.cB
.B
-�B
-�B
-]B
,�B
,�B
,"B
+QB
+QB
*B
*eB
)�B
)�B
)_B
)DB
)DB
(�B
(�B
'�B
'�B
'RB
&2B
%�B
%�B
%FB
$�B
$ZB
$@B
#�B
#B
"�B
#B
"�B
"4B
!�B
!|B
!|B
!|B
 �B
 vB
�B
!B
�B
�B
�B
�B
=B
kB
�B
1B
�B
+B
�B
B
YB
YB
$B
mB
�B
B
�B
aB
,B
�B
B
{B
[B
@B
�B
�B
B
�B
�B
�B
�B
B
�B
(B
6B
DB
	B
�B
B
�B
1B
�B
MB
 B
�B
�B
�B
	B
	�B

rB

rB

�B

�B
	�B
	B
�B
?B
YB
�B
9B
mB
9B
SB
�B
�B
�B
�B
UB
B
oB
B
;B
 B
 �B
 OB	��B	��B	�dB	��B	�	B	�>B	��B	��B	�	B	�rB	�B	��B	�0B	��B	��B	��B	��B	�B	�<B	�VB	�<B	�<B	��B	��B	��B
;B
gB
�B
�B
�B
	�B
	7B
�B
B
�B
+B
�B
?B
�B
�B
�B
�B
mB
SB
�B
�B
�B
KB
	�B
	�B
	�B
�B
~B
�B
"B
<B
�B
6B
~B
B
JB
0B
�B
VB
�B
�B
�B
�B
�B
�B
�B
�B
B
vB
�B
B
�B
&B
&B
�B
gB
{B
�B
�B
aB
�B
�B
�B
�B
�B
�B
�B
B
9B
B
B
SB
YB
YB
�B
EB
KB
7B
�B
�B
�B
CB
CB
)B
CB
�B
�B
�B
�B
�B
dB
dB
�B
B
 vB
 �B
 vB
 �B
 B
�B
 \B
 \B
!B
"B
"B
"4B
"�B
#nB
#�B
#�B
#�B
$tB
%FB
%�B
%�B
%�B
&LB
&fB
&�B
&�B
&fB
&fB
&�B
&�B
&�B
&�B
&�B
&�B
'8B
'B
'�B
'�B
'�B
'�B
($B
(�B
(sB
)*B
)_B
)�B
)�B
)�B
)�B
)�B
*0B
*0B
)�B
)�B
)�B
)�B
*0B
*B
)�B
*KB
*eB
*�B
*�B
+B
+�B
,�B
,�B
,�B
-)B
-wB
-�B
-�B
.B
./B
.B
./B
./B
.IB
.}B
.�B
/5B
/�B
0UB
0�B
1B
0�B
1'B
0�B
0�B
1�B
2GB
2�B
3hB
4�B
5ZB
5ZB
5�B
5�B
6B
6`B
6�B
6�B
6�B
6�B
6�B
6�B
8B
8B
8RB
8RB
8RB
8�B
8�B
9	B
8�B
8�B
8�B
8�B
8�B
9$B
9>B
9�B
9�B
9�B
:*B
:B
:DB
:^B
:xB
:xB
:�B
:�B
;JB
;JB
;JB
;JB
;�B
;�B
<6B
<�B
<jB
<�B
=�B
=�B
=�B
=�B
>BB
>wB
>�B
>�B
>�B
?}B
?�B
?�B
?�B
?�B
?�B
@ B
@OB
@4B
@iB
AB
AoB
AoB
A�B
A�B
BB
BAB
B'B
B[B
C-B
C{B
C�B
DB
D�B
D�B
EB
EB
E�B
E�B
E�B
F%B
F�B
F�B
F�B
G+B
GEB
G_B
G_B
G�B
GzB
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H1B
HKB
IB
IRB
I�B
J	B
J#B
JrB
J�B
J�B
KB
K)B
KDB
K�B
K�B
K�B
LJB
LJB
L0B
L�B
L�B
L�B
L�B
L�B
MB
MPB
MjB
M�B
M�B
M�B
M�B
NVB
N�B
NpB
N�B
O(B
O(B
O�B
PB
PB
Q4B
Q B
QNB
Q�B
R B
R B
RTB
R�B
R�B
SB
S[B
S�B
S�B
S�B
TFB
T{B
T�B
T�B
UB
UgB
U�B
U�B
U�B
VB
VSB
V�B
V�B
W$B
W?B
W?B
W?B
W?B
W�B
W�B
X+B
X�B
X�B
YKB
YKB
YKB
Y�B
Y�B
Z7B
Z�B
Z�B
[=B
[�B
[�B
[�B
\xB
]B
]/B
]IB
]IB
]dB
]�B
^B
]�B
]dB
]IB
^OB
^jB
]�B
]�B
]~B
^B
^5B
^�B
^�B
^�B
^�B
^�B
_B
_!B
_VB
_pB
_�B
_�B
_�B
`B
`\B
`vB
`�B
`vB
aB
a�B
b4B
b�B
b�B
b�B
b�B
cTB
c�B
c�B
c�B
c�B
c�B
dtB
dtB
d�B
d�B
d�B
e,B
e,B
eFB
e�B
e�B
e�B
e�B
ffB
fLB
f�B
f�B
g8B
g�B
g�B
g�B
h
B
h$B
h>B
h�B
h�B
h�B
h�B
h�B
h�B
iDB
i_B
i_B
iyB
i�B
i�B
j0B
jKB
j0B
jKB
jKB
j�B
j�B
j�B
j�B
k6B
kB
k�B
l"B
lB
k�B
k�B
k�B
k�B
k�B
k�B
lB
mCB
mCB
l�B
l�B
l�B
l�B
m]B
m)B
m)B
m)B
m)B
m)B
m)B
l�B
l�B
l�B
mwB
mwB
m�B
m�B
m�B
m�B
nB
n/B
nB
ncB
oB
pB
p�B
p�B
p�B
p�B
p�B
qB
qB
p�B
qB
q�B
q�B
q[B
p�B
p�B
p�B
p�B
p�B
q'B
qvB
q�B
qvB
q�B
q�B
rB
r-B
rGB
rGB
r|B
raB
r�B
r�B
s3B
s�B
s�B
s�B
s�B
s�B
tTB
tnB
tnB
tnB
t�B
tnB
t�B
t�B
t�B
u?B
u�B
v`B
v`B
v�B
wB
wB
wB
wLB
w�B
w�B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y	B
y>B
yrB
yrB
y�B
y�B
y�B
zB
y�B
y�B
zB
zB
z*B
z^B
z�B
z�B
{B
{JB
z�B
{JB
{dB
{B
|B
|B
|jB
|jB
|�B
|�B
|�B
|�B
|�B
}B
}<B
}VB
}�B
~B
~BB
~B331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.4(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201907230051432019072300514320190723005143202207271131552022072711315520220727113155202207271534342022072715343420220727153434  JA  ARFMdecpA30a                                                                20190712093912  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190712093936  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190712093936  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190712093937  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190712093937  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190712093937  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190712093937                      G�O�G�O�G�O�                JA  ARUP                                                                        20190712095807                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190713000000  CF  PSAL_ADJUSTED_QC@)��@�  G�O�                JM  ARCAJMQC2.0                                                                 20190722155143  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190722155143  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023155  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063434  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081508                      G�O�G�O�G�O�                