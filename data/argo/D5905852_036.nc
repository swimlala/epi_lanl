CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-29T09:38:46Z creation;2020-01-29T09:38:48Z conversion to V3.1;2022-08-02T05:11:29Z update;     
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
_FillValue                 �  ]�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  at   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ߠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20200129093846  20220818091504  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               $A   JA  A30_8420_036                    2C  D   APEX                            8420                            2.11.2                          846 @���R�? 1   @����;� @.J=p��
�c��#��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @&ff@�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�ffB�33B�  B���B�ffB���B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C�C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�#31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�R@xQ�@�p�@��HA�A>{A^{A}�A��HA��HA�G�A��A�
=A��A�
=A�\)B�\BffB�\B�B'�\B/�B7�B?��BGp�BO�BWp�B_�\Bg�\Boz�BwffB��B��
B��)B���B��RB���B���B�B��qB�B���B���B��fB��
B��RB�ǮB���B���B��
B̨�B�{B���B׮Bܔ{B��B��B�ǮB�qB���B�k�B�B��B��C޸CٚCٚC޸C	�)C��C�HC�)C��C��C�CC޸C�\C޸C��C!��C#�HC%޸C'޸C)�HC+��C-��C/�fC1�fC3�HC5�HC7��C9�fC;�fC=�)C?�)CA�HCC�HCE޸CG�)CI�)CK޸CM�)CO�HCQ�RCS�CU�fCW��CY޸C[�HC]��C_�HCa��Cc�fCe��Cg�fCi�fCk��Cm�fCo��Cq��Cs�fCu�fCw�HCyٚC{�HC}��C�HC���C��3C��\C���C��C��\C��3C��\C���C��C���C��3C��\C��\C��3C���C��C���C���C���C��\C��\C���C��\C��3C��3C���C��3C��3C��{C��{C��{C��3C��C��C���C��C���C��3C���C���C��C��
C���C��{C��3C���C��3C��3C���C��3C��{C���C��3C���C���C��{C��C��\C��3C���C��\C��C��\C��\C��3C���C��\C��\C���C��{C��C���C���C��3C��{C��3C���C��
C���C��C��C��\C��3C��\C��\C��3C��{C���C��3C��3C��{C��
C��3C��C��C��=C��C��{C��3C��C��C��\C��3C��RC��
C��{C��C���C���C��
C���C���C���C���C��C���C���C��C��3C��
C��3C��3C��3C��C��\C���C��\D xRD ��DxRD��DxRD��Dx�D��Dy�D�RDw
D�
Dw
D��Dx�D�RDw
D��D	xRD	��D
y�D
�=Dz�D�=Dw�D��DxRD�RDw
D�
Dw
D�
Dw�D�
DxRD�RDw�D��DxRD��Dx�D�=Dz=D�RDxRD��Dy�D��Dw�D��DxRD�=Dy�D�=Dy�D��Dx�D��Dz�D�=Dz�D�RDxRD��D w
D �RD!z�D!��D"x�D"�RD#x�D#��D$w�D$�=D%z=D%��D&y�D&�=D'w�D'��D(xRD(�RD)z=D)��D*x�D*��D+z=D+��D,z=D,��D-x�D-��D.z�D.��D/y�D/�=D0y�D0��D1y�D1��D2xRD2��D3xRD3��D4x�D4��D5z�D5��D6w
D6�fD7vfD7�
D8xRD8��D9z=D9��D:y�D:��D;x�D;��D<x�D<��D=z=D=�=D>z�D>��D?x�D?��D@w
D@�RDAz=DA��DBw
DB�fDCu�DC�RDDx�DD�
DEx�DE��DF{�DF��DGw
DG�RDHy�DH��DIy�DI��DJy�DJ��DKw�DK��DL{�DL�)DMy�DM��DNxRDN��DOy�DO�=DPx�DP�RDQx�DQ��DRxRDR��DSz=DS��DTx�DT�RDUw�DU��DVxRDV��DWz=DW��DXw�DX��DYx�DY�RDZy�DZ��D[y�D[��D\{�D\�=D]w�D]�RD^x�D^��D_z�D_��D`y�D`�=Daz=Da��Dbx�Db�RDcx�Dc��DdxRDd�RDe{�De�=Dfw�Df��Dgx�Dg�
Dhw�Dh�RDiw
Di��DjxRDj�
Dkx�Dk�=DlxRDl��DmxRDm��DnxRDn�
Dow
Do��Dpw�Dp�RDqxRDq�
Drw
Dr�
DsxRDs��Dty�Dt��Duy�Du��Dvz=Dv�)DwxRDw��DxxRDx�RDyz�Dy��DzxRDz��D{xRD{��D|xRD|��D}x�D}��D~x�D~�RDxRD��D�=D�|{D���D��)D�=qD�|)D���D��D�<{D�{3D���D���D�=D�|{D��)D��)D�<{D�}D��)D���D�;�D�{3D��3D���D�<)D�}D���D��{D�<{D�|{D��{D��)D�<)D�{3D���D��{D�;�D�{�D���D���D�;�D�|�D��{D��)D�=D�|{D���D���D�<�D�|{D��{D���D�;�D�|�D��)D���D�<{D�{�D���D��D�<�D�|{D��)D���D�<)D�{3D���D��3D�;�D�|)D��{D��qD�<{D�{�D��{D��D�<{D�|{D��)D��3D�<�D�~D��D���D�=qD�|�D���D��D�<)D�{�D��)D��)D�<)D�|)D��)D��3D�;�D�|)D��D��qD�<)D�|�D��{D��)D�;�D�{�D���D���D�<{D�{�D���D��{D�<�D�|)D��)D��{D�<{D�}D��{D��{D�;�D�|)D���D���D�<)D�|{D���D��qD�=D�}D��qD��qD�<{D�{3D���D��{D�=qD�|�D��)D��qD�=qD�}D��qD��{D�<�D�}qD���D��{D�<)D�|)D��)D��{D�=D�|)D���D��qD�=qD�|�D��)D��{D�=D�|�D��)D���D�;3D�{�D��D��qD�<{D�|�D��{D���D�;�D�|�D��{D��{D�<�D�}D��qD��D�<{D�|)D���D���D�;�D�|{D��D���D�>D�}D��)D��)D�<)D�}D��{D��)D�;�D�|)D���D���D�=D�}qD���D���D�<{D�|�D��D���D�<�D�}D��)D���D�=D�|{D��qD��)D�<)D�|)D���D��{D�<)D�|{D���D��{D�=D�|)D���D��qD�<�D�|{D��qD��qD�;�D�{3D��)D��{D�<�D�}qD��D���D�<)D�}D���D���D�<)D�|)D���D��)D�<)D�|{D���D��)D�<{D�{�D���D��)D�;�D�|)D��D���D�<�D�{�D��3D���D�=D�|)D»�D���D�<)D�}Dû�D��3D�<{D�}DĽqD��qD�;�D�{3Dż)D���D�<�D�}DƼ{D���D�=qD�|)Dǻ�D���D�<{D�|�Dȼ�D��)D�;�D�|�DɽqD���D�<�D�}qDʽD���D�;�D�|)D˻�D��)D�<{D�|�D̼{D��{D�<{D�|)Dͻ3D���D�=qD�|�Dλ�D���D�<)D�{�Dϼ{D���D�=qD�|�Dм{D���D�;�D�|)DѽD���D�;�D�{�DҼ{D���D�;�D�|�DӾD��D�<{D�|)DԻ�D���D�=qD�}Dռ)D��3D�;�D�|�Dּ)D���D�;�D�|{D׻�D���D�<�D�}Dؼ�D��)D�;�D�{�Dٻ�D���D�<{D�}Dڼ{D��)D�<)D�|{D۽qD���D�<�D�}DܽqD��{D�;�D�}DݽD��)D�;�D�|{D޽qD��D�=qD�|{D߻�D��{D�=D�}D�{D���D�;�D�|)D�D���D�=qD�}D�D��{D�<�D�}D�{D��{D�<{D�}D�D��)D�<{D�}qD��D��{D�<{D�|{D�D��{D�<)D�|�D�qD��{D�<)D�}D�D��)D�<{D�|{D�{D��{D�<�D�|�D�{D��{D�<{D�|{D��D���D�<)D�}D�qD��qD�<)D�{�D��{D��D�<)D�|)DD���D�<{D�{�D��D���D�<{D�{�D�D��{D�<�D�|�D�{D��{D�<�D�|{D�{D���D�<)D�|�D�{D��)D�<{D�|{D���D���D�<�D�~D��{D��)D�=qD�}D��{D���D�=D�|)D��3D��{D�;�D�{�D���D��D�<�D�}�D���D��)D�<)D�|�D��D��)D�=D�}�D��qD���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�wfA�v`A�v�A�poA�{�A�|�A�}�A�|�A�|�A�zxA�zDA�{�A�}�A�}�A�}"A�~(A��A΀ A΁A΁�A΃GA΃�A΄MA΅�A΅SAΆ%AΆYAΈfAΉ7AΉ�AΊ�A΋AΌAΏ(AΏ(AΎ�AΑ4AΒ�AΏ�AΎ"Aΐ�AΒAΑ4AΎ�AΏ�AΑ�AΛ	AΛ=AΘ�AΒ:A�VA��A��2A�GEA�y	A�d�A�8RA�S�A�b�A��4A��)A�A��`A���A�
	A��A��A�aHA��$A�~(A|��Az@�Axl"Au��Aq�Aj�5AdD�Aa��A]��A[�zAZxAXF�AWq�AU�\AQ��AK��AH��AE�`ABs�A?��A?($A==�A:��A9i�A6�)A0�6A.VmA,�nA,B[A+�	A+�A)�A)e�A*bA)�A%	�A$33A#�A!(�A �hA 7�A +A �A��A}VA`�Aq�Aw2AA�A˒A�bAB[A	A��A�A�A��A_A�TAl�A
=A��As�AA�AOA�|Av�A	A�Aa|A��Aj�A|�A5�A�0A�"A7A��AuA�CAp�AVmAخA�eA�A��A;A�dA� AA-�A�LA-A�AdZA�AߤA�\A[WA
��A
��A
�$A
XyA	�#A	e�A��A^�AuA�A|�AhsAo Ai�A,=A�Af�A��AN�A�#A{�A �A خA �LA �FA ��A 6z@��t@��@���@���@�7@��>@��*@�'�@�s�@��n@���@�$�@�o�@�ff@���@��Y@�S&@��@���@��@�1�@���@�C�@�ߤ@��@�h�@�#:@���@�t@�RT@��M@�C-@��[@쀝@���@�@O@�C@�(�@�g�@蒣@��@縻@�\@���@�p�@��v@䀝@㧇@⿱@� �@ᝲ@���@ߘ�@�-w@��@�oi@ݥ�@��	@ܜx@���@�j�@��'@�'R@�~�@�A @�y>@���@ׅ@�-w@֢4@��@�Z�@���@Ԋr@�c @���@�rG@��@ҡb@Ғ�@�~(@�-@ѷ@�@O@��@��'@�PH@���@ϑh@�C@΋D@��@�u�@�V@�oi@˱[@ˊ	@��	@�xl@�B[@��j@�y�@��f@�h�@�L0@�6@�7@ǧ�@�Y@��@ƃ�@�D�@��d@�H�@���@Ļ�@�kQ@��@Æ�@�Vm@°�@�@¢4@�YK@�3�@���@��9@��t@���@�V@��E@���@�u%@�#:@��@@�V@�l"@�@���@�F�@��@�S@���@���@���@���@�"h@��f@�1�@��@��@��@���@�YK@�"h@�_@��@�8@��|@���@�\�@�\�@���@�b�@�2a@�y>@��@��:@�U�@���@�� @�?�@�ԕ@��q@�N<@���@�u%@�M@�M@���@�+�@��6@�J�@�!@��@��w@�>�@�;@���@���@���@��o@�.�@��@��@���@�J#@��9@�&�@��>@��=@�?}@�'�@�+@�l"@��Z@�w2@�Q�@�?}@�0�@��c@���@�oi@��@��5@���@�_@�<�@��@��@�� @�J�@���@�($@�:�@��@���@���@��@�GE@��N@���@�$t@��v@��j@��+@�q@�Z@�Q@�H�@�{@���@���@�_p@�"�@���@��R@���@�z�@�e�@�!�@���@��F@�qv@�Y�@�@��@��r@��'@�P�@���@� �@���@�J#@���@��u@���@��+@�~(@�>B@��j@�'�@���@�<�@��.@���@��'@��)@���@�K^@�$�@��@��&@���@�f�@�RT@�*0@���@��@��@���@���@���@��@�@��f@�.I@���@���@��@�Z�@��A@���@�iD@��@��`@��6@�v�@��@��F@���@���@���@��^@���@�K�@�*0@�'�@�#�@�ȴ@���@�ff@�*�@��Z@���@���@��4@�a�@�8�@�!-@�	l@��	@��E@���@���@�V�@�;�@���@��V@��{@�g�@�8@���@��@��/@���@�h�@�-�@��Q@�j�@�%F@��@��@��'@��@�oi@�I�@�=q@�:�@�8�@�'R@�ϫ@���@�|�@�%F@��@��,@�r�@�~@�u@��;@���@�E9@�
=@��@��z@���@�l�@�,=@���@���@�{J@��@���@���@��@���@�kQ@�:*@�@�W@~�h@~
�@}@|��@|�@zȴ@zl�@zW�@zC�@y�@y|@y%F@x�@xS�@w�@w��@w��@w��@w��@w�@v�@v �@u�=@u8�@u�@t��@t�@t�@st�@sO@s�@r��@r�!@rz@rO@q�C@q��@qc�@qF@qF@q&�@p�f@p�$@py>@o��@o>�@ns�@m�@m`B@l�@l9X@k˒@j�<@j?@j
�@iVm@h��@h<�@g�[@g1�@f_�@fe@e��@e��@e��@eIR@d�|@dѷ@dtT@c�]@ce�@c@O@c@b�s@b�h@bOv@a��@a	l@`�u@`(�@_�}@_�$@_K�@^�@^��@^��@^0U@]�@]�X@]��@\�@\�@\�I@\�Y@\|�@\�@[�@@[9�@[ i@Zh
@Y��@Y?}@X��@X]d@W@O@V��@V�1@V^5@U�@UY�@T�|@T1@S�@S�@S�@R�@R�A@R@QG�@P�z@P�o@P6@O�4@N�@N8�@M�@M��@M�7@Mm]@MJ�@M#�@MV@L�/@L�9@L��@L1@Ke�@K&@J�,@Ju%@J{@I��@IT�@H֡@H>B@G�@G.I@F��@FL0@E��@E��@E��@Ezx@EL�@E+�@D�@D>B@C�
@Ce�@B�@B�@BE�@A�M@A-w@@�/@@h�@@Q�@@H@@<�@@1'@@�@?��@?��@?g�@?�@>��@=�.@=j@<�4@;�@;�P@;>�@:�x@9�o@9��@9k�@8�@8q@8U2@8H@8�@7�@7��@7�F@7��@7j�@7$t@7S@6�!@6�+@6s�@6V@6E�@6:*@6�@5�3@5�"@5a�@55�@5�@4��@4�@3��@3�$@3l�@3]�@2�"@2l�@2&�@1�@1�h@1`B@1�@0�5@0�@0�`@0Ɇ@0tT@/��@/x@/>�@/(@.p;@-�@-��@-w2@-+@,�5@,��@,b@+��@+l�@+.I@*�@*��@*�@)�9@)��@)��@)0�@(1'@((�@('R@(	�@'��@'6z@'�@&��@&͟@&�'@&͟@&�h@&��@&W�@&#:@&@&
�@%��@%��@%m]@%�@$��@$��@$l"@$M@$$@$�@#��@#�0@#�@#X�@#�@"ȴ@"�@"��@"v�@"�@!��@!m]@!!�@ ��@ ��@ ��@ |�@ PH@ %�@�&@|�@\)@E9@'�@(@�@i�@1�@_@�>@�^@��@x�@\�@5�@�E@�e@�D@y>@N�@�@�@�
@��@iD@C�@/�@@͟@Q@�@��@��@��@��@c@O�@4@@@�4@Q�@~@@�Q@�@g�@O@K�@6z@�@�X@�!@�}@�}@��@i�@?@�@�j@`B@@@�@�P@�E@�@1'@�@1@�@�q@a@U�@.I@�@��@}V@h
@GE@
�@ԕ@�n@s�@IR@�@��@N�@$@@ݘ@�@e�@�"@��@��@��@W�@0U@ �@�9@�7@F@*0@(�@	l@�5@��@H@4n@-�@"h@��@�[@��@t�@J#@�@
�X@
{�@
C�@	��@	�^@	�~@	��@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�wfA�v`A�v�A�poA�{�A�|�A�}�A�|�A�|�A�zxA�zDA�{�A�}�A�}�A�}"A�~(A��A΀ A΁A΁�A΃GA΃�A΄MA΅�A΅SAΆ%AΆYAΈfAΉ7AΉ�AΊ�A΋AΌAΏ(AΏ(AΎ�AΑ4AΒ�AΏ�AΎ"Aΐ�AΒAΑ4AΎ�AΏ�AΑ�AΛ	AΛ=AΘ�AΒ:A�VA��A��2A�GEA�y	A�d�A�8RA�S�A�b�A��4A��)A�A��`A���A�
	A��A��A�aHA��$A�~(A|��Az@�Axl"Au��Aq�Aj�5AdD�Aa��A]��A[�zAZxAXF�AWq�AU�\AQ��AK��AH��AE�`ABs�A?��A?($A==�A:��A9i�A6�)A0�6A.VmA,�nA,B[A+�	A+�A)�A)e�A*bA)�A%	�A$33A#�A!(�A �hA 7�A +A �A��A}VA`�Aq�Aw2AA�A˒A�bAB[A	A��A�A�A��A_A�TAl�A
=A��As�AA�AOA�|Av�A	A�Aa|A��Aj�A|�A5�A�0A�"A7A��AuA�CAp�AVmAخA�eA�A��A;A�dA� AA-�A�LA-A�AdZA�AߤA�\A[WA
��A
��A
�$A
XyA	�#A	e�A��A^�AuA�A|�AhsAo Ai�A,=A�Af�A��AN�A�#A{�A �A خA �LA �FA ��A 6z@��t@��@���@���@�7@��>@��*@�'�@�s�@��n@���@�$�@�o�@�ff@���@��Y@�S&@��@���@��@�1�@���@�C�@�ߤ@��@�h�@�#:@���@�t@�RT@��M@�C-@��[@쀝@���@�@O@�C@�(�@�g�@蒣@��@縻@�\@���@�p�@��v@䀝@㧇@⿱@� �@ᝲ@���@ߘ�@�-w@��@�oi@ݥ�@��	@ܜx@���@�j�@��'@�'R@�~�@�A @�y>@���@ׅ@�-w@֢4@��@�Z�@���@Ԋr@�c @���@�rG@��@ҡb@Ғ�@�~(@�-@ѷ@�@O@��@��'@�PH@���@ϑh@�C@΋D@��@�u�@�V@�oi@˱[@ˊ	@��	@�xl@�B[@��j@�y�@��f@�h�@�L0@�6@�7@ǧ�@�Y@��@ƃ�@�D�@��d@�H�@���@Ļ�@�kQ@��@Æ�@�Vm@°�@�@¢4@�YK@�3�@���@��9@��t@���@�V@��E@���@�u%@�#:@��@@�V@�l"@�@���@�F�@��@�S@���@���@���@���@�"h@��f@�1�@��@��@��@���@�YK@�"h@�_@��@�8@��|@���@�\�@�\�@���@�b�@�2a@�y>@��@��:@�U�@���@�� @�?�@�ԕ@��q@�N<@���@�u%@�M@�M@���@�+�@��6@�J�@�!@��@��w@�>�@�;@���@���@���@��o@�.�@��@��@���@�J#@��9@�&�@��>@��=@�?}@�'�@�+@�l"@��Z@�w2@�Q�@�?}@�0�@��c@���@�oi@��@��5@���@�_@�<�@��@��@�� @�J�@���@�($@�:�@��@���@���@��@�GE@��N@���@�$t@��v@��j@��+@�q@�Z@�Q@�H�@�{@���@���@�_p@�"�@���@��R@���@�z�@�e�@�!�@���@��F@�qv@�Y�@�@��@��r@��'@�P�@���@� �@���@�J#@���@��u@���@��+@�~(@�>B@��j@�'�@���@�<�@��.@���@��'@��)@���@�K^@�$�@��@��&@���@�f�@�RT@�*0@���@��@��@���@���@���@��@�@��f@�.I@���@���@��@�Z�@��A@���@�iD@��@��`@��6@�v�@��@��F@���@���@���@��^@���@�K�@�*0@�'�@�#�@�ȴ@���@�ff@�*�@��Z@���@���@��4@�a�@�8�@�!-@�	l@��	@��E@���@���@�V�@�;�@���@��V@��{@�g�@�8@���@��@��/@���@�h�@�-�@��Q@�j�@�%F@��@��@��'@��@�oi@�I�@�=q@�:�@�8�@�'R@�ϫ@���@�|�@�%F@��@��,@�r�@�~@�u@��;@���@�E9@�
=@��@��z@���@�l�@�,=@���@���@�{J@��@���@���@��@���@�kQ@�:*@�@�W@~�h@~
�@}@|��@|�@zȴ@zl�@zW�@zC�@y�@y|@y%F@x�@xS�@w�@w��@w��@w��@w��@w�@v�@v �@u�=@u8�@u�@t��@t�@t�@st�@sO@s�@r��@r�!@rz@rO@q�C@q��@qc�@qF@qF@q&�@p�f@p�$@py>@o��@o>�@ns�@m�@m`B@l�@l9X@k˒@j�<@j?@j
�@iVm@h��@h<�@g�[@g1�@f_�@fe@e��@e��@e��@eIR@d�|@dѷ@dtT@c�]@ce�@c@O@c@b�s@b�h@bOv@a��@a	l@`�u@`(�@_�}@_�$@_K�@^�@^��@^��@^0U@]�@]�X@]��@\�@\�@\�I@\�Y@\|�@\�@[�@@[9�@[ i@Zh
@Y��@Y?}@X��@X]d@W@O@V��@V�1@V^5@U�@UY�@T�|@T1@S�@S�@S�@R�@R�A@R@QG�@P�z@P�o@P6@O�4@N�@N8�@M�@M��@M�7@Mm]@MJ�@M#�@MV@L�/@L�9@L��@L1@Ke�@K&@J�,@Ju%@J{@I��@IT�@H֡@H>B@G�@G.I@F��@FL0@E��@E��@E��@Ezx@EL�@E+�@D�@D>B@C�
@Ce�@B�@B�@BE�@A�M@A-w@@�/@@h�@@Q�@@H@@<�@@1'@@�@?��@?��@?g�@?�@>��@=�.@=j@<�4@;�@;�P@;>�@:�x@9�o@9��@9k�@8�@8q@8U2@8H@8�@7�@7��@7�F@7��@7j�@7$t@7S@6�!@6�+@6s�@6V@6E�@6:*@6�@5�3@5�"@5a�@55�@5�@4��@4�@3��@3�$@3l�@3]�@2�"@2l�@2&�@1�@1�h@1`B@1�@0�5@0�@0�`@0Ɇ@0tT@/��@/x@/>�@/(@.p;@-�@-��@-w2@-+@,�5@,��@,b@+��@+l�@+.I@*�@*��@*�@)�9@)��@)��@)0�@(1'@((�@('R@(	�@'��@'6z@'�@&��@&͟@&�'@&͟@&�h@&��@&W�@&#:@&@&
�@%��@%��@%m]@%�@$��@$��@$l"@$M@$$@$�@#��@#�0@#�@#X�@#�@"ȴ@"�@"��@"v�@"�@!��@!m]@!!�@ ��@ ��@ ��@ |�@ PH@ %�@�&@|�@\)@E9@'�@(@�@i�@1�@_@�>@�^@��@x�@\�@5�@�E@�e@�D@y>@N�@�@�@�
@��@iD@C�@/�@@͟@Q@�@��@��@��@��@c@O�@4@@@�4@Q�@~@@�Q@�@g�@O@K�@6z@�@�X@�!@�}@�}@��@i�@?@�@�j@`B@@@�@�P@�E@�@1'@�@1@�@�q@a@U�@.I@�@��@}V@h
@GE@
�@ԕ@�n@s�@IR@�@��@N�@$@@ݘ@�@e�@�"@��@��@��@W�@0U@ �@�9@�7@F@*0@(�@	l@�5@��@H@4n@-�@"h@��@�[@��@t�@J#@�@
�X@
{�@
C�@	��@	�^@	�~@	��@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B+QB+6B+QB+�B+B+6B+B+6B+QB+QB+QB+QB+kB+QB+QB+QB+kB+QB+�B+kB+kB+kB+QB+QB+kB+QB+�B+kB+kB+�B+kB+�B+kB+kB+�B+�B+�B+�B,qB,�B,�B,WB,�B,�B,�B,�B+�B+�B,B,�B6FB��B	�FB
%B
n}B
c�B
FB
1[B
RB
=VB
3�B
�B	��B	�TB	�DB	�4B	�B	ªB	�B	��B	�7B	�vB	�SB	zDB	h$B	L0B	?�B	8�B	/B	*�B	'B	#�B	 vB	!|B	"�B	}B		�B	zB	9B	�B	SB	�B	�B	�B	 �B��B�#BÖB�^B��B�B�:B�iB	GB	-B��B�B�"B��B�7B�B�vB�cB	)B	HB	B	&�B	6zB	="B	?.B	IRB	T�B	[�B	\�B	\�B	`'B	xRB	�B	�&B	��B	�hB	�0B	� B	�ZB	��B	�(B	�4B	��B	��B	��B	��B	�gB	��B	�"B	�LB	�)B	�wB	��B	��B	�oB	�vB	��B	�UB	��B	��B	��B	��B	��B	�B	�B	ȴB	�lB	��B	�B	��B	��B	�mB	��B	��B	ٚB	��B	��B	�qB	یB	ܬB	��B	��B	ݲB	�B	�dB	چB	�9B	�B	� B	�B	�NB	� B	οB	ϑB	�.B	�}B	�(B	��B	�4B	�:B	� B	�4B	��B	�B	��B	��B	ԕB	�2B	�B	ևB	��B	��B	�?B	�9B	��B	�@B	�[B	��B	ΊB	̘B	ˬB	��B	�PB	�~B	�jB	�6B	̈́B	�B	�jB	�PB	�6B	�B	�6B	�jB	͟B	ΥB	�B	��B	��B	҉B	�BB	��B	�B	��B	ʌB	��B	˒B	̈́B	��B	ϑB	͟B	̘B	�jB	�B	�B	�NB	�:B	�TB	ԕB	��B	�aB	��B	�2B	՛B	��B	�9B	�9B	��B	רB	��B	�+B	��B	�eB	��B	�7B	ںB	ںB	�	B	�qB	��B	�CB	�]B	�]B	��B	�/B	�dB	�/B	ݘB	�B	��B	�5B	�jB	�jB	�B	�pB	�\B	��B	�bB	��B	�-B	�|B	�:B	�ZB	��B	�B	��B	�B	��B	�B	�8B	�$B	��B	��B	�_B	�eB	�B	��B	�B	�6B	�B	�yB	�B	�6B	�kB	��B	��B	� B	� B	� B	�B	�/B	�IB	��B	�!B	�[B	��B	�AB	��B	�B	�B	�-B	�GB	��B	�B	�9B	��B	�B	�B	��B	��B	��B	�B	�B	�B	��B	�B	��B	��B	��B	��B	�B	��B	�LB	��B	��B	��B	�RB	�lB	��B	�$B	��B	�B	��B	�PB	��B	��B	��B	��B	��B	��B	��B
 4B
 �B
;B
�B
B
'B
[B
�B
B
�B
�B
�B
SB
B
�B
YB
	�B
	B
zB
1B
	�B
B
B
"B
�B
(B
(B
�B
�B
bB
B
�B
�B
�B
[B
�B
 B
 B
�B
NB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
B
FB
B
�B
�B
B
B
B
B
B
?B

B
�B
�B
B
�B
mB
�B
YB
YB
YB
�B
?B
�B
�B
�B
�B
B
_B
�B
sB
YB
$B
SB
9B
�B
?B
�B
�B
�B
1B
eB
B
�B
�B
B
+B
�B
�B
�B
	B
CB
�B
�B
jB
5B
OB
OB
OB
5B
�B
�B
OB
;B
 �B
 vB
 vB
�B
!HB
"B
"�B
#nB
#�B
#�B
$&B
#�B
#TB
#B
"�B
"�B
"�B
#B
#�B
$ZB
%�B
%�B
&�B
&�B
&�B
&fB
'�B
'�B
(XB
(�B
(�B
)B
)_B
)�B
*KB
*B
*�B
*�B
*�B
+B
+QB
+6B
+QB
+kB
,WB
,�B
,�B
,�B
,�B
,�B
,qB
,WB
,"B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
,B
,=B
,"B
,B
,B
,qB
,qB
,qB
,�B
,�B
,�B
-]B
-�B
-�B
-�B
.B
./B
.cB
.�B
.�B
.�B
.�B
/5B
/OB
/�B
0UB
0�B
0�B
1B
1AB
1vB
1�B
1�B
1�B
1�B
2�B
2�B
3�B
3MB
4B
4�B
5B
4�B
4�B
5?B
5?B
5�B
6FB
6FB
6�B
6�B
6�B
6�B
6�B
6�B
7LB
7�B
7�B
88B
88B
8�B
8�B
8�B
9XB
9rB
9�B
9�B
9�B
:B
:�B
:�B
:�B
;0B
;dB
;0B
;dB
;B
;�B
;�B
<�B
<�B
=qB
=�B
=�B
>BB
>]B
>wB
?cB
?HB
?.B
?}B
?�B
?}B
@B
@OB
AB
AB
A;B
AUB
AoB
A�B
A�B
A�B
BB
BAB
B�B
B�B
B�B
B�B
B�B
B�B
CaB
C�B
C�B
D3B
DgB
D�B
D�B
EB
EB
ESB
E�B
E�B
E�B
E�B
FtB
F�B
F�B
F�B
F�B
GB
G+B
GzB
G_B
G�B
H1B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
JrB
JrB
J�B
KDB
KDB
K�B
LB
LdB
LJB
L�B
M�B
N<B
N"B
NVB
N�B
O�B
O�B
PHB
P}B
P�B
P�B
P�B
P�B
Q B
QNB
Q�B
Q�B
R:B
R:B
RoB
R�B
R�B
S&B
S[B
S&B
TB
T{B
T{B
U2B
UgB
U�B
U�B
U�B
VB
VB
VB
VB
VB
W?B
WYB
W�B
W�B
XB
XEB
X�B
X�B
YB
YeB
YeB
YeB
YeB
YeB
Y�B
Y�B
Y�B
Y�B
Y�B
Z7B
Z�B
[=B
[�B
\CB
\)B
\]B
\�B
]IB
]IB
]IB
]�B
]�B
]�B
]�B
^B
^5B
^jB
^5B
^OB
^�B
^�B
^�B
_B
_!B
_!B
_VB
_�B
_�B
_�B
`'B
`vB
`�B
`�B
`�B
a|B
a�B
bB
bB
b4B
b4B
bhB
b�B
cB
c:B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
dtB
d�B
d�B
d�B
eFB
e�B
e�B
e�B
fB
f2B
f2B
f�B
gB
g8B
gmB
g�B
g�B
h>B
hXB
hsB
hsB
h�B
i�B
i�B
i�B
i�B
j0B
j�B
j�B
j�B
j�B
j�B
j�B
kB
kB
kQB
k�B
k�B
k�B
k�B
k�B
k�B
l"B
lWB
lWB
l�B
l�B
l�B
l�B
mB
mB
mCB
m]B
m�B
m�B
m�B
m�B
m�B
ncB
n�B
o B
oB
o5B
o�B
oiB
o�B
o�B
o�B
pB
p�B
p�B
poB
p�B
p�B
p�B
p�B
q'B
q'B
q[B
qvB
q�B
q�B
q�B
q�B
r-B
rGB
rGB
rGB
raB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
shB
s�B
s�B
s�B
tB
tB
tB
tB
tB
tB
t�B
t�B
uB
u%B
uZB
utB
u�B
u�B
u�B
u�B
u�B
vFB
v`B
v`B
vFB
v`B
v�B
v�B
v�B
v�B
w2B
w�B
wfB
w�B
w�B
w�B
x8B
x8B
x8B
xRB
x�B
x�B
x�B
y	B
yXB
y�B
y�B
y�B
y�B
zB
zDB
z^B
z�B
z�B
z�B
{dB
{�B
{�B
{�B
|B
|PB
|�B
}B
}"B
}VB
}qB
}�B
}�B
~B
~BB
~�B
~�B
~�B
~�B
HB
.B
�B
� B
�B
�B
�B
�4B
��B
��B
��B
��B
�B
�UB
��B
��B
�'B
�[B
�uB
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B+QB+6B+QB+kB+B+6B+6B+6B+QB+QB+kB+QB+kB+QB+QB+QB+�B+QB+�B+kB+kB+kB+QB+kB+kB+QB+�B+�B+kB+�B+�B+�B+kB+�B+�B+�B+�B+�B,qB,�B,�B,WB,�B,�B,�B,�B+�B,=B-�B1[BG�B�aB	�KB
1�B
w�B
l�B
O�B
8�B
XB
C{B
;�B
%FB
SB	��B	�=B	��B	��B	��B	��B	��B	�IB	�TB	��B	��B	o�B	S�B	C�B	=B	1�B	-)B	)*B	%`B	#�B	'B	)B	{B	�B	�B	B	�B	B	�B	)B	HB	+B�B�B�MB�BٴB�B��B�B	SB	zB�`B�>B�B��BںB�VB��B��B	�B	}B	@B	&�B	6�B	=VB	?�B	I�B	UMB	\CB	]�B	]�B	`�B	xlB	�6B	��B	��B	��B	��B	�iB	��B	�dB	��B	��B	� B	�4B	��B	ĜB	�SB	�MB	��B	��B	��B	�IB	��B	�hB	�B	��B	�[B	�AB	�;B	��B	��B	��B	�MB	��B	�aB	�lB	�#B	��B	͟B	�}B	�FB	��B	�1B	�QB	�7B	�)B	�)B	��B	�]B	�IB	ݲB	��B	�5B	�jB	�OB	��B	׍B	�,B	уB	ѷB	� B	��B	ϑB	�HB	��B	�B	�vB	�HB	�hB	҉B	ҽB	��B	�4B	�@B	�&B	�FB	��B	՛B	֡B	�
B	רB	�yB	��B	��B	��B	�B	�,B	��B	ϑB	�6B	�B	�0B	͟B	�B	��B	�jB	͹B	�PB	͟B	͟B	͟B	�jB	��B	�B	�pB	�B	ϑB	�.B	уB	�&B	��B	�<B	�xB	ˬB	�B	�XB	�B	�B	�bB	�HB	�<B	�B	�"B	��B	�\B	ѷB	ңB	��B	�B	�MB	��B	�MB	��B	�B	�SB	ևB	��B	�YB	��B	�EB	خB	�B	��B	�7B	چB	�	B	�#B	یB	��B	��B	�xB	�xB	ܬB	�/B	ݘB	ݲB	ݘB	�B	�OB	�5B	ޞB	��B	�B	�pB	��B	��B	�HB	�B	�HB	�B	��B	�B	��B	�`B	��B	�B	�8B	�B	�B	�B	�sB	�DB	��B	��B	��B	��B	�0B	�B	�B	�kB	��B	��B	�kB	�B	�/B	� B	�OB	�B	�B	��B	�B	�B	�B	�oB	�B	�-B	��B	�vB	�vB	��B	�|B	�|B	�B	��B	�nB	�B	��B	�nB	�TB	�9B	�B	��B	�B	��B	�B	��B	��B	��B	�%B	��B	�`B	�B	��B	�8B	�2B	�LB	��B	��B	�	B	�rB	�B	��B	�6B	��B	��B	��B	��B	�BB	�.B	��B	�B
 �B
 B
�B
'B
[B
uB
�B
GB
3B
�B
�B
B
�B
SB
�B
�B

=B
	�B
�B
�B
	�B
dB
"B
pB
�B
�B
vB
�B
�B
�B
NB
�B
�B
,B
B
&B
TB
:B
B
�B
4B
�B
NB
 B
 B
MB
,B
�B
�B
,B
{B
{B
mB
B
B
B
9B
B
9B
9B
�B
?B
$B
�B
9B
9B
�B

B
sB
�B
�B
�B
�B
�B
�B
+B
_B
�B
�B
B
�B
�B
sB
�B
�B
�B
YB
�B
�B
EB
B
B
�B
B
B
_B
�B
eB
7B
B
=B
xB
�B
B
�B
jB
�B
�B
jB
OB
�B
�B
�B
�B
 �B
 �B
 �B
 B
!�B
"NB
#:B
#�B
$B
$ZB
$tB
#�B
#�B
#TB
#B
#B
"�B
#B
#�B
$ZB
%�B
&2B
&�B
&�B
&�B
&�B
'�B
(>B
(�B
)*B
)*B
)DB
)yB
*B
*B
*�B
*�B
+B
+B
+B
+kB
+�B
+�B
+�B
,�B
-B
,�B
-B
-B
,�B
,�B
,�B
,WB
,=B
,"B
,B
,B
+�B
+�B
+�B
+�B
,B
,B
,"B
,WB
,WB
,=B
,WB
,�B
,�B
,�B
-B
,�B
-B
-�B
-�B
-�B
-�B
.IB
.cB
.�B
.�B
.�B
.�B
/ B
/iB
/�B
/�B
0�B
1'B
1'B
1AB
1vB
1�B
1�B
2B
2B
2B
3B
3MB
3�B
3�B
4�B
5B
5?B
5%B
5?B
5tB
5tB
5�B
6`B
6zB
6�B
6�B
6�B
6�B
6�B
7LB
7�B
8B
8B
8RB
8lB
8�B
8�B
9>B
9�B
9�B
9�B
9�B
:B
:DB
:�B
;B
;B
;JB
;dB
;dB
;B
;�B
;�B
<B
<�B
<�B
=�B
=�B
>(B
>wB
>�B
>�B
?�B
?}B
?�B
?�B
?�B
?�B
@iB
@�B
A;B
A;B
AUB
AoB
A�B
A�B
B'B
B'B
BAB
B�B
B�B
B�B
B�B
B�B
B�B
CaB
C�B
C�B
D3B
DMB
D�B
D�B
D�B
E9B
ESB
EmB
E�B
E�B
F%B
F?B
F�B
F�B
F�B
F�B
F�B
GEB
G_B
G�B
G�B
HB
HfB
H�B
H�B
I7B
J	B
J	B
J	B
J#B
J�B
J�B
K)B
K^B
KxB
K�B
LJB
L�B
L�B
MB
M�B
NVB
NpB
N�B
O(B
O�B
O�B
PbB
P�B
P�B
P�B
P�B
QB
Q4B
QhB
Q�B
Q�B
RoB
RoB
R�B
R�B
SB
S[B
S�B
SuB
TFB
T�B
T�B
UgB
U�B
U�B
U�B
VB
V9B
VB
VB
V9B
V�B
W�B
W�B
W�B
X+B
XEB
X�B
X�B
YB
YKB
YeB
YeB
YeB
YeB
YB
Y�B
Y�B
Y�B
ZB
ZB
ZkB
Z�B
[�B
\)B
\�B
\xB
\�B
]B
]~B
]~B
]~B
^B
^B
^B
]�B
^OB
^jB
^�B
^jB
^�B
^�B
^�B
^�B
_;B
_;B
_;B
_VB
_�B
_�B
`B
`BB
`�B
`�B
`�B
aHB
a�B
a�B
b4B
bNB
bNB
bhB
b�B
c B
c B
cTB
c�B
c�B
dB
c�B
c�B
dB
d@B
dZB
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
f2B
ffB
f�B
gB
gRB
gmB
g�B
g�B
h
B
hsB
hsB
h�B
h�B
i*B
i�B
i�B
i�B
i�B
jKB
j�B
j�B
j�B
kB
kB
j�B
k6B
k6B
k�B
k�B
k�B
k�B
k�B
k�B
lB
l=B
l�B
l�B
l�B
l�B
l�B
m)B
mCB
mCB
mwB
mwB
m�B
nB
nB
n/B
n/B
n�B
n�B
oB
o5B
oOB
o�B
o�B
o�B
p!B
o�B
p!B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q[B
qAB
qvB
q�B
q�B
q�B
q�B
q�B
raB
raB
raB
r|B
r�B
r�B
r�B
r�B
r�B
sB
sB
sB
r�B
sMB
s�B
s�B
s�B
s�B
tB
tB
t9B
t9B
tTB
tTB
t�B
t�B
u%B
u?B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
vFB
vzB
v`B
v`B
vzB
v�B
v�B
wB
w2B
wLB
w�B
w�B
w�B
w�B
w�B
xRB
xlB
xRB
xlB
x�B
x�B
x�B
y>B
yrB
y�B
y�B
y�B
zB
z*B
z^B
zxB
z�B
z�B
{0B
{B
|B
|B
|B
|6B
|�B
|�B
}"B
}<B
}qB
}�B
}�B
}�B
~(B
~]B
~�B
~�B
~�B
.B
}B
HB
� B
�B
�B
�4B
�OB
�OB
��B
��B
��B
�B
�;B
��B
��B
��B
�[B
��B
�uB
��B
��3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��<��}<� �<L��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202002090046492020020900464920200209004649202207271134342022072711343420220727113434202207271536572022072715365720220727153657  JA  ARFMdecpA30a                                                                20200129093750  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200129093846  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200129093846  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200129093847  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200129093847  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200129093847  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200129093848                      G�O�G�O�G�O�                JA  ARUP                                                                        20200129095448                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200130000000  CF  PSAL_ADJUSTED_QC@%@\)G�O�                JM  ARCAJMQC2.0                                                                 20200208154649  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200208154649  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023434  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063657  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091504                      G�O�G�O�G�O�                