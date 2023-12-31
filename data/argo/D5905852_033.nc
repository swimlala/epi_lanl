CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-06T09:47:58Z creation;2020-01-06T09:48:00Z conversion to V3.1;2022-08-02T05:11:37Z update;     
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
_FillValue                 �  ]l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200106094758  20220818091504  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               !A   JA  A30_8420_033                    2C  D   APEX                            8420                            2.11.2                          846 @���� 1   @��So�@,ȴ9X�cF����?1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  BxffB�  B���B���B�33B�  B�  B�  B�33B�  B�ffB���B���B�  B�  B�  B�  B�  B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(33C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @G�@y��@�(�@��A{A>�RA^�RA~ffA��A�
=A���A��HA��A�p�A�\)A���BffB�\B��B�B'ffB/p�B7�\B?BG�BO�\BW�
B_�Bg��Bo�Bw�HB��qB�� B�k�B�
=B��3B�ǮB��
B���B���B��B�\)B���B��3B�ǮB�ǮB���B��
BîBǨ�B˽qBϨ�BӮB׽qB۽qB߽qB���B���B�qB�B��
B���B��HB��
C�fC�)C޸C��C	�HC��C��C�HC�fC��C��C�HC�HC޸C�fC�)C!�HC#��C%�C(\C)�fC+�HC-�{C/ٚC1ٚC3ٚC5�)C7��C9�HC;�HC=��C?�HCA޸CC��CE�HCG�HCI�fCK�CM�HCO�)CQ�HCS޸CU�CW�RCYٚC[��C]�)C_޸Ca��Cc޸Ce޸Cg޸Ci�)Ck�HCm�)Co�HCq��Cs޸CuٚCw�HCy��C{�HC}޸C�)C��{C��3C��\C��\C��C��{C��
C���C���C��C���C��C��{C��
C���C���C���C��C���C���C��{C��3C��
C��
C���C��
C���C���C��C��3C��3C��C���C��3C���C���C��
C��3C���C��3C��C��C��C���C��{C��3C��C��C��\C��C���C���C��C��3C��3C��{C��3C��3C��3C��\C��\C��3C��C��\C��3C���C��3C��C���C��C���C��C��C��C��C��{C��{C��
C��
C���C��
C��{C��\C��C��C���C���C��C���C���C��C���C��
C���C���C��C���C���C��C���C���C��3C��C��C��\C��C��{C���C��\C��C��C��
C���C���C��
C���C��C���C��{C��\C��\C��C��3C���C��C���C��3C��\D w�D �RDy�D��DxRD��Dx�D�RDw
D��Dz=D��DxRD��Dx�D�
DxRD�=D	z=D	�RD
x�D
�=D{�D��Dz=D��DxRD�RDy�D��Du�D�fDvfD�RDx�D��Dz�D��DvfD�
Dw�D�RDy�D��Dz�D�=Dz�D��DvfD��Dy�D��Dx�D��Dx�D�RDw�D��Dz�D�=D{�D�qDy�D��D xRD ��D!x�D!��D"z=D"�=D#x�D#��D$x�D$��D%z=D%��D&x�D&�RD'w�D'�RD(y�D(��D)y�D)�=D*y�D*�RD+y�D+��D,x�D,��D-z=D-��D.z�D.�RD/w�D/��D0y�D0�=D1y�D1��D2z=D2�=D3z�D3�=D4z=D4��D5z=D5��D6x�D6��D7y�D7��D8x�D8�RD9z=D9��D:xRD:��D;xRD;�RD<y�D<�RD=x�D=�=D>w�D>�
D?w
D?�RD@x�D@�fDAw�DA��DBx�DB�=DCx�DC��DDxRDD��DExRDE�RDFw
DF��DGxRDG��DHz�DH�=DIz=DI��DJz�DJ��DKz�DK��DLy�DL��DMz�DM��DNxRDN��DO{�DO��DPy�DP�=DQz�DQ�=DRz�DR��DSxRDS��DTxRDT��DUy�DU��DVz�DV�=DWy�DW��DXz=DX�RDYw
DY��DZxRDZ�RD[w�D[�
D\w�D\��D]z=D]�RD^vfD^�RD_y�D_��D`z=D`�=DaxRDa��Dby�Db�RDcx�Dc��Ddw�Dd��Dez�De�=Dfw�Df�
Dgz=Dg��Dhy�Dh�RDivfDi��Djy�Dj�RDkw
Dk��Dlw
Dl��Dmx�Dm��Dnz=Dn�RDow
Do�fDpvfDp�
DqxRDq��Drw�Dr��DsvfDs��Dt{�Dt��Duw�Du��Dvz�Dv�=Dww�Dw��DxxRDx��Dyy�Dy�RDzvfDz��D{y�D{�RD|w�D|��D}w
D}�fD~y�D~��Dz�D�=D�=qD�}D��qD��qD�=�D�~D��{D��{D�=qD�|�D��qD��{D�<)D�{�D���D���D�<)D�{�D��)D���D�<{D�|)D���D���D�<)D�|�D��{D���D�<{D�|)D��)D���D�<{D�|{D��3D���D�=D�|)D��)D���D�<{D�|�D��qD��)D�;�D�|�D���D���D�=�D�}D��{D���D�<)D�{�D��)D���D�<{D�{�D���D��)D�=qD�|{D���D��)D�<)D�}D��D��D�<�D�|)D��D���D�<�D�|{D���D��)D�<{D�}D���D��)D�;�D�{�D���D���D�;�D�|�D��{D��{D�=D�}D��)D���D�=qD�|{D���D���D�<�D�|�D��)D���D�=�D�}D��{D���D�;�D�{�D���D��{D�<{D�|�D���D��D�=D�|{D���D��)D�=D�|)D��3D��)D�<)D�|{D���D���D�;�D�{�D���D��{D�;�D�{�D��3D��3D�<�D�~D���D���D�>D�|�D���D��{D�<)D�{�D���D��{D�;�D�|�D��{D��{D�<{D�|�D��qD��)D�;�D�|)D���D��{D�<{D�}qD��D���D�<�D�}D��qD��{D�=D�}�D��D���D�<�D�}D��)D���D�<�D�~D��D���D�;�D�|)D���D��{D�<{D�|{D��)D��)D�<)D�|�D��)D���D�<�D�}qD���D���D�=D�{�D��)D��{D�<�D�|{D��)D��)D�<�D�|�D���D��qD�=D�}qD��D��D�<{D�|{D��)D���D�<�D�|�D��)D���D�=D�|)D��)D���D�<)D�{�D���D���D�=�D�|{D���D��D�<�D�}D���D��{D�<{D�|)D���D���D�<)D�|�D��{D��)D�;�D�{�D���D���D�;�D�{�D��)D��D�=D�|�D��qD��D�<{D�|)D��{D��{D�<)D�|�D��)D��)D�=D�}D��)D���D�<)D�|)D¼)D��qD�>D�|�Dü{D��)D�<{D�|{DĽD��qD�=D�}Dż{D���D�;�D�{�DƼ)D��{D�<�D�|{Dǻ3D���D�<)D�|)DȽD��qD�=D�|)Dɻ�D���D�<{D�|{Dʻ�D���D�<)D�{�D˼)D��D�;�D�{3D̻3D���D�<�D�}DͽqD���D�<�D�|)DνD���D�;�D�|)Dϼ{D���D�<{D�|�Dл�D��{D�=qD�}DѽD��D�;�D�{�DҼ)D���D�<)D�}qDӽqD��qD�=D�|�DԼ)D��3D�;�D�{�Dջ�D���D�<)D�|{DֽD��D�<{D�{�D׽D���D�<)D�}�Dؽ�D���D�=�D�|�Dٻ�D���D�<)D�|�DھD��D�=D�}D۽qD��qD�=qD�|)Dܺ�D��3D�;�D�|)Dݼ)D��{D�=D�}qD޼{D���D�<{D�|�D߼�D���D�<{D�|�D�D���D�<{D�|{D��D��qD�<�D�|)D��D��)D�<�D�|{D��D��{D�<{D�{�D�{D��D�<�D�|�D��D��{D�<)D�|)D�D��)D�<)D�|{D绅D��{D�=qD�|{D�)D��{D�<)D�|)D��D��D�<{D�{�D�D���D�;�D�}D�qD��)D�;�D�{�D�)D��D�<{D�|)D��{D���D�:�D�{3D��D��{D�<)D�|{D�qD���D�<�D�}D�D��D�<)D�{�D��D��{D�=qD�|{D�D��3D�;3D�{3D��D��{D�=D�}D���D��)D�;�D�{�D���D��qD�=qD�|)D���D���D�=D�|)D��D���D�=D�|{D���D��D�;�D�|)D��{D��3D�;3D�{3D���D���D�;�D�{�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A�՛A��&A�҉A��,A��A��mA��A��mA��2A���A�ԕA�ԕA���A�՛A��9A���A�یA��5A���A���A��NA��ZA��ZA���A���A��NA��2A��ZA��A��mA��
A���A��A�W�A�t�A�A��mA��EA�OA��	A��A��A�G�A�{A�q�A�b�A��A��A�ĜA�5�A�+�A��)A�VA�rA���A��iA���A��A�GzA��A�#:A�@OA���A���A�$A��oA��oA��A��A��(A��nA���A���A�XEA��DA�.A��OA��A}A Az:*At��Aq��An�AiIRAd�zA_��AZ�AW�#AT8�AP�EAL�$AJ}�AH=�AB1�A=�	A:�xA5{A2��A0�<A.r�A+�IA)��A(9XA'g�A(�A(|�A'��A%͟A$�+A"��A�zAqAp;A�4A>�A�9A+kA��ADgAh
A�Ap;A��A�[A�OA��AkQA�YAJ#A�A�ZA��AV�A�A�fA�A:�AjAxlA�AA�A�A��A�rA,=A�dAa�AƨA�AA`BA�AQ�A��A�$A|AF�A>�A�SA
��A
MjA
�oA
p�A	�TA	�2A	��A	u�A	jA	�A�:A�AA��AdZA)�A�KA?�A��A��A�uA��A�HA��A��A>�A�oA}VAn/As�AXAɆA�]AMAW�A�AYA �}A ��A Dg@��@�|@�2a@��@���@�m�@�x@���@��@��$@���@���@�~@���@���@�kQ@��@��Q@�S�@��@�=q@��@�@���@�?}@��@��@�P�@��@�=q@��w@�J#@��@���@쟾@���@�9@��@�C@�J#@�ں@�u%@�
�@�6z@思@�/�@��Q@�|@���@�L0@�"@��@�1@�C@���@�
�@�G�@��K@�E�@�W?@�W�@ۻ0@�B�@�V@��T@�p�@��@�`�@׮�@�7L@��@ְ!@�)�@ղ�@�/�@Ԍ@ӓ@���@�1'@љ�@�0�@�m�@ϑh@�H�@��@�l�@��+@͓@�=@��B@�q�@�,=@��@�o�@�$t@ʲ�@�O@ɢ�@��v@Ⱦ�@�C�@���@�@O@��@�rG@ķ�@�)�@Õ�@���@+@�H@�@���@�C�@��@���@��@��p@�g8@�  @���@�a�@��@�e@���@��@�w2@��	@��?@��Y@�ff@��@���@��4@�6z@��j@�n�@��@��@�U�@�o@��|@���@��.@���@�y�@�V@���@��$@��9@�xl@�Q@�	�@���@�<6@� i@��@��@�u�@��@���@�U�@��@���@�}V@��K@�t�@�9�@�S@��@��)@��@��@��@��@��@��s@��_@�4@��@��@���@���@�O@��,@���@��9@���@�w�@�?@�@��7@�4@���@��R@�*�@�@�ѷ@��h@�{�@�6�@���@��n@�;d@�	@��'@�6z@���@�R�@��@�@�7@��T@�s�@�;@���@��z@��D@���@�RT@�	l@�u%@�Z@�!@��g@���@�)_@��h@�q@�<�@�7@�4@��@���@�j�@�!-@��@��2@�p;@�e@��3@�j�@�$t@��@��@��)@���@���@�~(@�]d@��@��@���@�B�@�+�@�+@��P@���@�PH@�!@��@���@��@��@�x@�L�@��M@��x@�bN@��@� �@�� @��4@�m]@�W?@��\@�<�@�Ov@�Z@�*�@�	�@��#@�w2@��@��@�xl@��@�ƨ@��Q@��@��@�&@��@�-w@�!�@�V@�ߤ@�� @�J�@�_@���@���@��"@�[W@�'�@��|@��@�r�@�-�@���@��@���@��3@��@�x@�F�@��@���@��@�O@���@��@�e,@�>�@��@���@�`�@��@���@��S@�hs@�L�@��y@�u%@�,=@��&@���@�hs@�A @��@��5@��x@�s�@�A�@�"h@���@���@�7L@��@��	@�ی@�͟@��\@��@��0@��:@�Y�@�>�@��@��"@��j@�g8@�,=@��@�@&@~M�@}�@}�@|��@|~(@|2�@{��@{��@{8@z�h@y�z@yA @x��@xK^@w9�@v��@vd�@v
�@t�U@t@s�K@sC�@r�A@r.�@r@q�@q��@qO�@q%F@pbN@o�+@o�0@o�@o�P@o{J@o�@n�@m�@m�7@m`B@mIR@l��@k�@k�g@k�F@k�k@ke�@k8@k�@j@i�'@iA @i�@h��@h��@g�@g��@g\)@g!-@f͟@f�+@fh
@f?@e��@e?}@e#�@e�@d��@d�$@dD�@d	�@c�a@c�F@cl�@b��@b��@be@a��@a!�@aV@`�K@`�p@`��@`]d@`%�@_��@_�@_e�@_&@^��@^�]@^�@^1�@]�>@]��@]L�@\��@[�A@[v`@Z��@Z�<@Zi�@Y�d@YS&@Y@X�_@X�@W�m@W��@WRT@V͟@V��@U��@UIR@T�K@Tѷ@T�@TC-@S�+@S��@S��@So@R҉@Q�@Q�@Q�H@Qhs@P��@PXy@O�&@O��@Og�@N�"@Na|@M�z@M��@M=�@L��@L��@L|�@LS�@L�@K��@KiD@K i@Ju%@I�@I<6@H�@HD�@H4n@H7@GiD@G�@F�X@F��@F4@E�j@E�t@E5�@Dh�@D�@Cݘ@C|�@C;d@C�@C�@C i@B��@BW�@A�@A��@A�@Au�@ADg@@�p@@oi@?��@?W?@?/�@>�2@>�@>E�@=�3@=F@=@<�f@<�v@<��@<?�@<1@;��@;C�@:C�@9��@9^�@9q@8��@8c�@7خ@7�w@7�[@7dZ@7S@6�y@6�<@6�@6+k@5��@5-w@4��@4��@4r�@4 �@3�&@3��@3y�@3l�@3O@3@O@3/�@2�@2��@2$�@1�C@1s�@0�@0�@0	�@/o�@/�@.��@.�+@.@-�9@-c�@-�@,��@,֡@,��@,r�@, �@+��@+�@+�@*��@*҉@*��@*��@*� @*�A@*l�@*\�@*e@)��@)�S@)7L@(�@(u�@(c�@(D�@(�@'�@'��@'�4@'E9@'=@'/�@&��@&�L@&ff@%�@%��@%(�@$�`@$~(@$Q�@$1'@$�@#خ@#��@#H�@# i@"�6@"s�@"GE@!�@!�z@!�^@!��@!|@!N<@ �P@ ��@ V�@ �@ �@��@+@�s@�!@��@��@Z�@u@�'@\�@7L@q@��@�@�/@֡@��@b@��@�V@j�@�'@�@{�@kQ@C�@:*@4@�@�T@�@�z@��@!�@�@�@�.@�@u�@`�@/�@�F@l�@!-@ i@�M@��@��@W�@�Z@�@��@x�@k�@[W@B�@0�@#�@@�K@��@�@e�@�@˒@��@�q@�k@_p@�8@��@��@��@�L@��@s�@YK@6�@��@�d@��@�'@u�@`B@:�@�@��@�@�@9X@x@�6@��@j�@A�@,�@!-@@ں@��@-@u@�@�3@��@�X@rG@*0@�v@Ɇ@��@�@��@j@Xy@PH@<�@,=@��@ݘ@�6@�0@�V@�$@��@\)@@O@�@
�h@
Ta@
H�@
)�@	��@	�@	�@	u�@	X@	Q�@	N<@	(�@��@�`@��@��@m�@`�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A�՛A��&A�҉A��,A��A��mA��A��mA��2A���A�ԕA�ԕA���A�՛A��9A���A�یA��5A���A���A��NA��ZA��ZA���A���A��NA��2A��ZA��A��mA��
A���A��A�W�A�t�A�A��mA��EA�OA��	A��A��A�G�A�{A�q�A�b�A��A��A�ĜA�5�A�+�A��)A�VA�rA���A��iA���A��A�GzA��A�#:A�@OA���A���A�$A��oA��oA��A��A��(A��nA���A���A�XEA��DA�.A��OA��A}A Az:*At��Aq��An�AiIRAd�zA_��AZ�AW�#AT8�AP�EAL�$AJ}�AH=�AB1�A=�	A:�xA5{A2��A0�<A.r�A+�IA)��A(9XA'g�A(�A(|�A'��A%͟A$�+A"��A�zAqAp;A�4A>�A�9A+kA��ADgAh
A�Ap;A��A�[A�OA��AkQA�YAJ#A�A�ZA��AV�A�A�fA�A:�AjAxlA�AA�A�A��A�rA,=A�dAa�AƨA�AA`BA�AQ�A��A�$A|AF�A>�A�SA
��A
MjA
�oA
p�A	�TA	�2A	��A	u�A	jA	�A�:A�AA��AdZA)�A�KA?�A��A��A�uA��A�HA��A��A>�A�oA}VAn/As�AXAɆA�]AMAW�A�AYA �}A ��A Dg@��@�|@�2a@��@���@�m�@�x@���@��@��$@���@���@�~@���@���@�kQ@��@��Q@�S�@��@�=q@��@�@���@�?}@��@��@�P�@��@�=q@��w@�J#@��@���@쟾@���@�9@��@�C@�J#@�ں@�u%@�
�@�6z@思@�/�@��Q@�|@���@�L0@�"@��@�1@�C@���@�
�@�G�@��K@�E�@�W?@�W�@ۻ0@�B�@�V@��T@�p�@��@�`�@׮�@�7L@��@ְ!@�)�@ղ�@�/�@Ԍ@ӓ@���@�1'@љ�@�0�@�m�@ϑh@�H�@��@�l�@��+@͓@�=@��B@�q�@�,=@��@�o�@�$t@ʲ�@�O@ɢ�@��v@Ⱦ�@�C�@���@�@O@��@�rG@ķ�@�)�@Õ�@���@+@�H@�@���@�C�@��@���@��@��p@�g8@�  @���@�a�@��@�e@���@��@�w2@��	@��?@��Y@�ff@��@���@��4@�6z@��j@�n�@��@��@�U�@�o@��|@���@��.@���@�y�@�V@���@��$@��9@�xl@�Q@�	�@���@�<6@� i@��@��@�u�@��@���@�U�@��@���@�}V@��K@�t�@�9�@�S@��@��)@��@��@��@��@��@��s@��_@�4@��@��@���@���@�O@��,@���@��9@���@�w�@�?@�@��7@�4@���@��R@�*�@�@�ѷ@��h@�{�@�6�@���@��n@�;d@�	@��'@�6z@���@�R�@��@�@�7@��T@�s�@�;@���@��z@��D@���@�RT@�	l@�u%@�Z@�!@��g@���@�)_@��h@�q@�<�@�7@�4@��@���@�j�@�!-@��@��2@�p;@�e@��3@�j�@�$t@��@��@��)@���@���@�~(@�]d@��@��@���@�B�@�+�@�+@��P@���@�PH@�!@��@���@��@��@�x@�L�@��M@��x@�bN@��@� �@�� @��4@�m]@�W?@��\@�<�@�Ov@�Z@�*�@�	�@��#@�w2@��@��@�xl@��@�ƨ@��Q@��@��@�&@��@�-w@�!�@�V@�ߤ@�� @�J�@�_@���@���@��"@�[W@�'�@��|@��@�r�@�-�@���@��@���@��3@��@�x@�F�@��@���@��@�O@���@��@�e,@�>�@��@���@�`�@��@���@��S@�hs@�L�@��y@�u%@�,=@��&@���@�hs@�A @��@��5@��x@�s�@�A�@�"h@���@���@�7L@��@��	@�ی@�͟@��\@��@��0@��:@�Y�@�>�@��@��"@��j@�g8@�,=@��@�@&@~M�@}�@}�@|��@|~(@|2�@{��@{��@{8@z�h@y�z@yA @x��@xK^@w9�@v��@vd�@v
�@t�U@t@s�K@sC�@r�A@r.�@r@q�@q��@qO�@q%F@pbN@o�+@o�0@o�@o�P@o{J@o�@n�@m�@m�7@m`B@mIR@l��@k�@k�g@k�F@k�k@ke�@k8@k�@j@i�'@iA @i�@h��@h��@g�@g��@g\)@g!-@f͟@f�+@fh
@f?@e��@e?}@e#�@e�@d��@d�$@dD�@d	�@c�a@c�F@cl�@b��@b��@be@a��@a!�@aV@`�K@`�p@`��@`]d@`%�@_��@_�@_e�@_&@^��@^�]@^�@^1�@]�>@]��@]L�@\��@[�A@[v`@Z��@Z�<@Zi�@Y�d@YS&@Y@X�_@X�@W�m@W��@WRT@V͟@V��@U��@UIR@T�K@Tѷ@T�@TC-@S�+@S��@S��@So@R҉@Q�@Q�@Q�H@Qhs@P��@PXy@O�&@O��@Og�@N�"@Na|@M�z@M��@M=�@L��@L��@L|�@LS�@L�@K��@KiD@K i@Ju%@I�@I<6@H�@HD�@H4n@H7@GiD@G�@F�X@F��@F4@E�j@E�t@E5�@Dh�@D�@Cݘ@C|�@C;d@C�@C�@C i@B��@BW�@A�@A��@A�@Au�@ADg@@�p@@oi@?��@?W?@?/�@>�2@>�@>E�@=�3@=F@=@<�f@<�v@<��@<?�@<1@;��@;C�@:C�@9��@9^�@9q@8��@8c�@7خ@7�w@7�[@7dZ@7S@6�y@6�<@6�@6+k@5��@5-w@4��@4��@4r�@4 �@3�&@3��@3y�@3l�@3O@3@O@3/�@2�@2��@2$�@1�C@1s�@0�@0�@0	�@/o�@/�@.��@.�+@.@-�9@-c�@-�@,��@,֡@,��@,r�@, �@+��@+�@+�@*��@*҉@*��@*��@*� @*�A@*l�@*\�@*e@)��@)�S@)7L@(�@(u�@(c�@(D�@(�@'�@'��@'�4@'E9@'=@'/�@&��@&�L@&ff@%�@%��@%(�@$�`@$~(@$Q�@$1'@$�@#خ@#��@#H�@# i@"�6@"s�@"GE@!�@!�z@!�^@!��@!|@!N<@ �P@ ��@ V�@ �@ �@��@+@�s@�!@��@��@Z�@u@�'@\�@7L@q@��@�@�/@֡@��@b@��@�V@j�@�'@�@{�@kQ@C�@:*@4@�@�T@�@�z@��@!�@�@�@�.@�@u�@`�@/�@�F@l�@!-@ i@�M@��@��@W�@�Z@�@��@x�@k�@[W@B�@0�@#�@@�K@��@�@e�@�@˒@��@�q@�k@_p@�8@��@��@��@�L@��@s�@YK@6�@��@�d@��@�'@u�@`B@:�@�@��@�@�@9X@x@�6@��@j�@A�@,�@!-@@ں@��@-@u@�@�3@��@�X@rG@*0@�v@Ɇ@��@�@��@j@Xy@PH@<�@,=@��@ݘ@�6@�0@�V@�$@��@\)@@O@�@
�h@
Ta@
H�@
)�@	��@	�@	�@	u�@	X@	Q�@	N<@	(�@��@�`@��@��@m�@`�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�*B�DB�DB��B��B��B��B��B��B��B�XB�>B�XB�>B�XB�XB��B�^B��B��B��B��B�B�PB��B�6B��B�6B��B��B��B��B�B	�]B	�B
��B
t�B
|�B
}�B
�GB
��B
��B
��B
�B
�(B�B
�B�B+B,"B4B�B�B�BtB
�dB2-B�"B�[B�gB޸B�4B��B�KB�8B�?BhsB4B,WBB
�$B
�B
�@B
��B
��B
i�B
HB
/B	ѷB	�QB	��B	y	B	lB	]IB	C�B	2�B	qB	B�B�9B�B�B�B�B�B�GB��B��B�@B��B��B�B��B��B��B��B�JB��B�"B�NB�B�kB��B̘BΊBڠB�NB�"B��B	 �B	B	5%B	ESB	l�B	�B	�SB	�B	�vB	�;B	��B	��B	�%B	��B	��B	ȀB	�PB	̈́B	��B	�<B	��B	��B	�JB	��B	ȚB	�B	��B	��B	�0B	��B	�
B	�B	�TB	�B	�|B	��B	�bB	��B	�B	��B	��B	��B	��B	��B	�.B	��B	�B	�%B	�VB	̘B	�	B	�B	�"B	��B	�B	�B	�=B	��B	�bB	�B	��B	׍B	��B	� B	�B	�B	�B	ޞB	�VB	�OB	�B	��B	��B	��B	��B	��B	ѝB	�B	�TB	ңB	��B	�{B	�gB	�B	ؓB	��B	�hB	οB	��B	�"B	��B	��B	ΥB	�\B	�HB	�B	��B	��B	�B	յB	�B	ՁB	�,B	��B	ԯB	�,B	�B	՛B	�2B	�gB	�MB	յB	�B	�MB	՛B	�?B	ևB	��B	�B	��B	�sB	�YB	�sB	רB	�+B	�EB	�yB	��B	��B	�B	�eB	�B	�QB	�QB	��B	�7B	چB	�kB	�B	�xB	ܒB	ܒB	��B	�B	�/B	�/B	ݲB	�IB	ݘB	��B	�B	�5B	�5B	��B	�B	޸B	�B	��B	�B	�B	�|B	�|B	��B	�`B	�sB	�DB	��B	�B	�B	�B	�B	��B	��B	��B	�]B	��B	��B	�/B	��B	�B	�wB	�CB	�WB	�}B	�UB	�B	�AB	�B	�'B	�vB	�B	��B	��B	��B	�nB	�B	�tB	�fB	��B	�lB	��B	�rB	�$B	�XB	�$B	��B	�DB	�0B	��B	��B	�B	�B	��B	�B	��B	�<B	��B	��B	�(B	��B	�<B	��B	�BB	�wB	��B	��B	�}B
  B
 4B
 �B
 �B
 �B
B
[B
�B
�B
�B
�B
�B
aB
{B
�B
MB
B
MB
gB
�B
mB
mB
�B
�B
�B
1B

=B
	�B
	�B
	�B

�B

�B
B

�B

�B

�B

�B

�B

�B

rB

�B

�B
B

�B
B

�B

�B
�B

�B
^B
�B
�B
�B
�B
�B
�B
B
	�B
	�B
	�B

rB

�B
DB
�B
0B
�B
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
"B
VB
�B
\B
�B
�B
�B
}B
}B
�B
@B
FB
FB
,B
�B
gB
�B
MB
2B
2B
gB
B
�B
$B
?B
�B
yB
�B
B
�B
�B
B
B
�B
�B
�B
B
B
CB
CB
�B
�B
/B
~B
�B
�B
�B
B
�B
�B
�B
�B
�B
 BB
!B
!B
!HB
!bB
!|B
 vB
 'B
�B
�B
�B
�B
#nB
#�B
"�B
"NB
$tB
%B
%B
%,B
%FB
%zB
&B
&fB
&fB
'B
'�B
(
B
(>B
(�B
)*B
)_B
)�B
)�B
)�B
)�B
*B
*B
*0B
*eB
*�B
+B
+B
+�B
+�B
+�B
+�B
,WB
,qB
-]B
./B
-�B
-�B
-CB
-)B
-�B
-)B
,�B
,qB
,WB
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-]B
./B
.cB
.}B
.�B
.�B
.�B
/�B
0;B
0oB
0�B
0�B
1B
0�B
1[B
1�B
1�B
2B
2-B
2GB
2�B
2�B
3�B
3MB
3�B
4B
4B
4TB
4�B
4�B
5?B
5ZB
5�B
5�B
6�B
6�B
6�B
6�B
8B
8RB
8�B
8�B
9>B
9�B
9�B
9�B
9�B
:B
:*B
:�B
;B
;JB
;dB
;dB
;dB
;�B
;�B
<jB
<�B
<�B
<�B
=VB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
?HB
?cB
?cB
?}B
@ B
@B
@OB
@iB
@�B
AB
@�B
AB
A�B
B'B
BB
B'B
BB
B[B
B�B
B�B
B�B
B�B
CB
CGB
C{B
C�B
C�B
DB
DB
DMB
D�B
D�B
D�B
D�B
EB
EB
E�B
E�B
E�B
E�B
FB
F?B
FtB
FtB
F�B
F�B
G+B
G+B
GEB
G+B
G+B
G�B
HB
HKB
H�B
H�B
H�B
I7B
IRB
I�B
I�B
I�B
JXB
JXB
J�B
J�B
K)B
KxB
K�B
K�B
LB
K�B
M6B
L�B
L�B
MB
MPB
N"B
N<B
N<B
NpB
N�B
N�B
OB
OBB
O\B
O�B
O�B
O�B
P.B
P}B
P�B
P�B
P�B
QB
QNB
RB
RTB
R�B
R�B
RoB
S&B
S&B
SuB
SuB
S�B
T,B
T,B
T�B
UB
UMB
UMB
U�B
U�B
U�B
U�B
U�B
U�B
VB
V9B
V9B
V9B
VSB
VSB
V�B
V�B
W?B
W�B
W�B
W�B
W�B
XB
X�B
X�B
X�B
YB
YKB
YB
YB
Z�B
[WB
Z�B
[qB
[WB
[�B
\xB
]B
\�B
]B
]B
\�B
]dB
]�B
^�B
^�B
^�B
_B
_pB
_�B
_�B
_�B
`'B
`BB
`BB
`B
`�B
a�B
a�B
a�B
a�B
bB
bNB
b�B
b�B
cB
cTB
c�B
d@B
d�B
d�B
d�B
d�B
e`B
e�B
e�B
ffB
f�B
ffB
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
h
B
h$B
h$B
h$B
h>B
h$B
h�B
h�B
h�B
iB
h�B
h�B
iB
i*B
iDB
i*B
iDB
i*B
i*B
iDB
i*B
i_B
iB
i*B
i�B
jB
jKB
jB
j�B
j�B
j�B
kB
kB
kQB
kkB
k�B
k�B
l"B
l�B
mB
m)B
m)B
m)B
mCB
m]B
m�B
m�B
nB
n/B
n�B
oB
p!B
p;B
pB
pB
pB
pB
p!B
poB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
qvB
qvB
q�B
q�B
r�B
r�B
s3B
s3B
shB
shB
s�B
s�B
s�B
s�B
s�B
tTB
t�B
t�B
uB
utB
u�B
u�B
u�B
vB
v�B
v�B
w�B
w�B
w�B
w�B
xB
xRB
x�B
x�B
y	B
y	B
y$B
y$B
yXB
yrB
yrB
y�B
y�B
zB
z*B
zDB
zDB
zxB
zxB
z�B
z^B
zxB
z�B
z�B
z�B
z�B
{B
{B
{JB
{dB
{B
{�B
|B
|PB
|PB
|jB
|jB
|�B
}"B
}<B
}VB
}qB
}�B
}�B
~BB
~�B
~�B
~�B
~�B
~�B
~�B
B
cB
� B
� B
�OB
�OB
�OB
�4B
�iB
��B
� B
� B
�UB
��B
��B
��B
��B
��B
��B
�B
�AB
�AB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�-B
�GB
��B
��B
��B
��B
�3B
�3B
�B
�3B
�MB
��B
��B
�gB
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�^B�^B�^B��B��B�B�B�B��B��B�rB�XB�rB�XB�rB��B��B�xB��B�*B��B��B�6B�jB��B�PB�B�PB��B�PB�qB��B��B	��B	�?B
��B
zDB
}�B
B
�?B
�B
�*B
��B
�OB-B�B
�rBKB�B0�B�B	B
�B�B�B
�dB1B�B�aB�
B�B��B�/B��B�kB��Bm�B6zB/5B"�B
�PB
�B
֡B
�cB
�B
n/B
NVB
&�B	֡B	�B	��B	}qB	p�B	dZB	J#B	8�B	 �B	bB	�B��B�B�>B��B	�BޞB��B��B��B�SB�@B�hB��B�AB�;B�SB�B��B�tB�.B��B�tB��B�:B͟B�BB�#B�NB�VB��B	�B	}B	4nB	DMB	l�B	�4B	��B	�lB	��B	��B	�`B	�B	��B	�B	�oB	�RB	�<B	ΊB	��B	ϑB	�B	ΊB	�6B	�jB	��B	�gB	��B	��B	�6B	��B	��B	��B	�B	��B	��B	�-B	��B	��B	�fB	�XB	�fB	��B	�0B	�BB	�}B	�_B	�tB	�tB	οB	�B	�XB	�6B	�pB	�VB	͟B	��B	��B	�6B	�}B	� B	��B	רB	�OB	�B	�&B	�B	�;B	޸B	߾B	�;B	�;B	�7B	��B	� B	ңB	�oB	� B	҉B	ҽB	�B	�&B	ԯB	յB	ؓB	�KB	��B	�B	�BB	ΥB	��B	�\B	�\B	�BB	ϫB	ЗB	�bB	�HB	�4B	өB	�B	��B	�B	��B	ԕB	�B	ԕB	՛B	�B	յB	յB	ՁB	�B	�mB	��B	ևB	רB	��B	�YB	�_B	�EB	��B	�B	��B	�B	ؓB	ؓB	��B	�KB	�eB	ٴB	�B	�kB	��B	�	B	�kB	ڠB	�	B	�#B	��B	��B	�B	�IB	�IB	�dB	ݘB	ݲB	�B	ݲB	��B	�B	ބB	޸B	޸B	�jB	��B	�;B	ߤB	�BB	�-B	��B	�B	��B	�TB	�B	��B	�B	�B	��B	��B	�QB	��B	�=B	�B	�wB	��B	�IB	�cB	�}B	�}B	�B	�B	�B	��B	�B	��B	��B	�B	��B	�[B	�B	�-B	�MB	�B	�B	��B	�ZB	��B	��B	�8B	��B	�XB	�B	�rB	��B	�XB	��B	��B	�dB	��B	�B	�PB	�PB	�6B	�PB	�<B	��B	�B	��B	�wB	��B	��B	�(B	�wB	��B	�HB	��B	��B
 B
 iB
 �B
 �B
 B
AB
�B
�B
�B
�B
�B
GB
�B
�B
�B
�B
mB
�B
�B
9B
�B
�B
%B
EB
+B
�B

rB
	�B
	�B
	�B

�B
)B
DB
B
B

�B
B

�B

�B

�B

�B
B
DB
DB
xB
)B
xB
�B
)B
�B
B
0B
�B
6B
B
dB
xB

XB
	�B
	�B

�B

�B
xB
0B
~B
dB
�B
�B
B
PB
B
PB
jB
�B
�B
<B
pB
�B
(B
�B
�B
�B
�B
�B
�B
B
uB
{B
{B
�B
MB
�B
B
�B
gB
gB
�B
B
�B
?B
sB
�B
�B
KB
eB
�B
�B
�B
kB
�B
)B
�B
CB
)B
xB
xB
�B
IB
~B
�B
�B
B
B
jB
�B
!B
pB
�B
�B
 \B
!HB
!HB
!|B
!�B
!�B
 �B
 �B
�B
�B
�B
 B
#�B
$@B
# B
"hB
$�B
%FB
%FB
%zB
%�B
%�B
&LB
&�B
&�B
'RB
'�B
(XB
(�B
(�B
)yB
)yB
)�B
)�B
)�B
*B
*eB
*eB
*B
*�B
*�B
+kB
+kB
+�B
,B
,"B
,"B
,�B
,�B
-�B
.}B
.B
-�B
-wB
-�B
./B
-]B
,�B
,�B
,�B
,�B
,�B
,�B
-CB
,�B
,�B
,�B
-B
-CB
-�B
.IB
.}B
.�B
.�B
.�B
/OB
0;B
0UB
0�B
0�B
0�B
1'B
1'B
1�B
2B
2-B
2GB
2|B
2�B
2�B
3MB
3�B
3�B
3�B
49B
4TB
4�B
4�B
5B
5tB
5�B
6B
6zB
6�B
6�B
7B
7fB
8RB
8�B
8�B
9$B
9XB
9�B
9�B
9�B
:B
:DB
:xB
:�B
;0B
;B
;B
;�B
;�B
<B
<6B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>(B
?B
?B
?cB
?�B
?�B
?�B
@4B
@OB
@iB
@�B
@�B
A;B
AB
A;B
A�B
BAB
BAB
B[B
BAB
B�B
B�B
B�B
CB
B�B
CaB
C{B
C�B
C�B
D3B
D3B
D3B
D�B
D�B
D�B
EB
D�B
EB
E9B
E�B
E�B
F%B
FB
FYB
FtB
F�B
F�B
F�B
GEB
G_B
GzB
G_B
GzB
G_B
G�B
HKB
H�B
H�B
IB
IB
IRB
I�B
I�B
I�B
J#B
JrB
J�B
J�B
J�B
K^B
K�B
K�B
LB
LdB
LJB
MPB
MB
MB
MPB
M�B
NpB
NpB
NpB
N�B
N�B
N�B
OBB
OvB
OvB
O�B
PB
P.B
PbB
P�B
P�B
QB
Q4B
QhB
Q�B
R:B
R�B
R�B
R�B
R�B
S[B
S[B
S�B
S�B
TB
TaB
TaB
T�B
U2B
UgB
U�B
U�B
U�B
U�B
U�B
U�B
U�B
V9B
VSB
VmB
VSB
VmB
V�B
W
B
W$B
W�B
W�B
W�B
X+B
XEB
X_B
X�B
X�B
Y1B
YKB
YeB
Y�B
Y�B
[	B
[�B
[=B
[�B
[qB
[�B
\�B
]IB
]IB
]/B
]/B
]/B
]~B
^B
^�B
^�B
_B
_;B
_�B
_�B
_�B
`B
`\B
`\B
`\B
`'B
`�B
a�B
bB
b4B
bB
bNB
b�B
b�B
cB
cTB
c�B
d&B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
f2B
f�B
f�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
g�B
g�B
h
B
h>B
h>B
hXB
hsB
h>B
h�B
h�B
iB
iDB
h�B
iB
iDB
iDB
i_B
iDB
i_B
iDB
iDB
iyB
i_B
i�B
i*B
iyB
i�B
jeB
jeB
j�B
j�B
kB
kB
k6B
k6B
k�B
k�B
k�B
lB
l=B
l�B
mCB
m)B
mCB
mCB
m]B
m�B
m�B
m�B
n/B
ncB
n�B
o5B
pUB
pUB
p;B
p!B
p;B
p;B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qAB
q�B
q�B
q�B
q�B
r�B
r�B
sMB
shB
s�B
s�B
s�B
s�B
s�B
s�B
tB
tnB
t�B
t�B
u?B
u�B
u�B
u�B
u�B
v`B
v�B
wB
w�B
w�B
xB
xB
xRB
xlB
x�B
x�B
y>B
y$B
y>B
y$B
yXB
yrB
y�B
y�B
y�B
zDB
z^B
z^B
z^B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{0B
{B
{�B
{�B
{�B
|6B
|jB
|jB
|�B
|�B
|�B
}<B
}qB
}�B
}�B
}�B
~B
~wB
~�B
~�B
~�B
~�B
.B
~�B
.B
�B
�B
�4B
�iB
�OB
�iB
�iB
��B
�B
�;B
�UB
�UB
��B
��B
��B
��B
��B
�B
�AB
�[B
�[B
��B
��B
��B
��B
��B
�B
��B
�GB
�-B
�-B
�aB
�{B
��B
��B
��B
�B
�MB
�3B
�MB
�MB
�MB
��B
��B
��B
��B
��33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<:�<#�
<AV�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202001200052382020012000523820200120005238202207271134102022072711341020220727113410202207271536362022072715363620220727153636  JA  ARFMdecpA30a                                                                20200106093932  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200106094758  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200106094759  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200106094759  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200106094759  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200106094800  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200106094800                      G�O�G�O�G�O�                JA  ARUP                                                                        20200106095434                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200110000000  CF  PSAL_ADJUSTED_QC@Q�@�Q�G�O�                JM  ARCAJMQC2.0                                                                 20200119155238  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200119155238  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023410  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063636  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091504                      G�O�G�O�G�O�                