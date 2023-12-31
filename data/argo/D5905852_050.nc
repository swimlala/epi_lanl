CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-06-17T12:38:01Z creation;2020-06-17T12:38:03Z conversion to V3.1;2022-08-02T05:10:52Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200617123801  20220818091505  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               2A   JA  A30_8420_050                    2C  D   APEX                            8420                            2.11.2                          846 @�!��1   @�!�w؎�@0������b�Ʌ�oi1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B���B���B�  B�  B�33B�  B���C�fC  C�fC�fC
  C  C  C  C  C33C�fC  C�fC�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:33C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dxy�Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ D�|�D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @\)@u@��H@��AG�A=�A^=qA~=qA��HA���A���A���A�33A�G�A���A���Bz�B�\B��B�B'\)B/\)B7z�B?z�BGz�BO�BWz�B_p�Bgz�Boz�BwffBz�B�B��qB��qB��RB��qB�B�B���B�B�B���B��B��RB��B��qB���B���BýqBǮBˮBϮB�Bأ�Bۙ�B�B��HB瞸B랸B�3B���B�  B��qB���C�C�
C��C��C	�
C޸C�)CٚC�HC�C��C�)C�=C�\C�HC�HC!�)C#�
C%�)C'�
C)�
C+޸C-�)C/�)C1޸C3�)C5޸C7�3C:
=C;޸C=�\C?�{CAٚCCٚCE�fCG�HCI�
CK�)CM�)CO�)CQ�HCSٚCUٚCW޸CY�HC[�HC]�fC_޸CaٚCc޸Ce�HCg��Ci޸Ck�)Cm�Co�HCq�)CsٚCuٚCw�)CyٚC{�)C}޸C޸C��C��\C��C���C��\C��{C���C���C��C��C��C��\C��C��C��C���C��\C��C��C���C��C��C��C���C��C��\C���C��\C���C��C���C��=C��C��C��=C���C��C��C��C��C��3C��{C��\C���C��{C��\C��C��C��C��C���C��3C��\C��\C��\C��\C��C��C��C��\C��C��C��C��\C��C��C���C��C���C���C���C��\C���C���C��=C��\C��\C��C��C��C��C��C��C���C���C��\C��C��3C���C���C��\C���C��C���C���C��C���C��\C��C��C���C��C���C���C��C��3C���C���C���C��3C���C��\C��\C��\C��C��C��C��3C��C��C���C��\C��3C��C��C��C��C��D uD ��Dx�D��Dw
D��Dy�D��DvfD��DvfD�
DxRD�fDxRD��Dw�D��D	w
D	�fD
w
D
��Dy�D��DxRD��Dz=D��Dw
D��Dw�D��DvfD�fDw
D��Dw�D�fDw
D�
Dw�D��Dx�D�
Dw
D�
DuD�fDxRD��DxRD��Du�D�
Dt{D�DvfD�
Dw
D�fDvfD��Dw
D��D xRD ��D!xRD!��D"xRD"��D#z=D#��D$xRD$�RD%w
D%�
D&uD&��D'y�D'��D(w�D(�
D)x�D)�=D*xRD*��D+vfD+�RD,x�D,�=D-w�D-�D.xRD.�RD/u�D/��D0w
D0�RD1y�D1��D2xRD2�
D3xRD3�RD4w
D4�=D5z�D5�RD6x�D6��D7y�D7�
D8u�D8��D9x�D9�RD:x�D:�
D;w
D;�RD<w�D<�RD=w�D=��D>w
D>��D?w
D?�
D@xRD@��DAy�DA�RDBxRDB�=DCxRDC��DDvfDD�
DEw
DE�fDFuDF��DGw
DG�fDHw
DH��DIy�DI�fDJuDJ�
DKw
DK�RDL|)DL��DMw�DM�
DNw
DN��DOvfDO�DPw�DP��DQw
DQ�RDRw�DR�fDSw
DS�
DTw
DT��DUw�DU�RDVxRDV��DWxRDW��DXw
DX�
DYu�DY�DZu�DZ��D[z=D[�RD\w
D\��D]u�D]�
D^w�D^�fD_uD_�
D`w�D`��Daw�Da�
Dbw
Db�fDcw
Dc��Ddz=Dd��Dew�De��DfxRDf��DgxRDg��Dhx�Dh�
Diw�Di�RDjx�Dj�=Dkx�Dk�
Dlw
Dl�
DmvfDm�fDnw�Dn�
DovfDo�
DpxRDp�=Dqy�Dq�fDrxRDr�
Dsw
Ds��Dtu�Dt�
Duw
Du�
Dvx�Dv�
DwxRDw��Dxs�Dx�fDyy�Dy�
DzuDz�
D{y�D{��D|w�D|��D}u�D}��D~xRD~�fDw�D��D�;�D�{�D���D���D�<{D�|�D��)D��3D�;�D�{�D���D��D�;�D�|)D��)D���D�;�D�|)D���D��)D�;�D�{�D���D���D�;�D�{3D���D���D�<{D�|)D��)D��)D�;�D�|)D���D��3D�;�D�{3D���D��3D�;�D�{�D���D��3D�;�D�|)D��)D��)D�<)D�|�D���D���D�<)D�}D���D���D�;3D�|�D���D���D�;�D�|{D���D��3D�:�D�z�D��3D��{D�<�D�|�D���D���D�9�D�{�D��D��{D�;�D�|�D��)D���D�<)D�|)D��)D���D�;3D�|)D��)D��{D�;�D�{�D��{D���D�;�D�|)D��)D���D�;�D�{3D��)D��D�<)D�|{D��D��)D�<{D�|)D��3D��3D�;3D�z�D���D���D�<)D�{�D��3D���D�<{D�{�D���D���D�;�D�|{D��)D���D�;�D�{�D���D���D�;�D�|�D���D���D�<)D�{�D���D���D�;�D�{�D���D���D�;�D�z�D��3D���D�;�D�{�D���D���D�;3D�z�D��3D���D�<)D�|)D��)D���D�;�D�{�D���D���D�:�D�|)D��{D���D�;�D�{�D���D��)D�<)D�|)D��{D��{D�;�D�z�D���D��3D�;3D�{�D��3D���D�<)D�{3D���D���D�:�D�z�D��3D��)D�<�D�{�D���D��)D�<)D�|)D��)D��)D�<)D�|{D��D��)D�<)D�|)D���D��3D�;�D�{�D��{D��{D�<{D�|�D���D���D�<)D�|)D��3D���D�;3D�{3D��)D��)D�<)D�|)D���D���D�<)D�|{D��3D���D�;�D�{�D��3D��3D�<)D�|{D���D���D�<{D�{�D���D���D�;3D�{�D��)D��)D�;�D�{�D���D���D�<�D�{�D���D���D�<)D�|�D���D���D�;�D�{�D���D��3D�:�D�{3D���D���D�<�D�{�Dº=D��=D�;�D�{�Dú�D���D�=D�}qDļ{D��{D�<)D�{�DŻ�D��)D�<�D�{�Dƻ3D��{D�<)D�{�Dǻ�D���D�;3D�z�DȻ3D���D�;3D�z�Dɼ)D��{D�;�D�{�Dʼ)D���D�;�D�|{D˼)D���D�;�D�|)D̻�D��)D�<)D�{3Dͺ�D���D�;�D�{�Dκ�D��3D�;�D�{3DϺ�D���D�;�D�|)DнD��{D�<{D�|)Dѻ3D��)D�;�D�y�Dһ�D��{D�<�D�{�Dӻ3D���D�;�D�z�DԺ�D��=D�:�D�{3Dռ{D���D�;3D�{3Dֺ�D���D�<{D�|)D׺�D���D�;�D�{�Dؼ)D���D�;�D�|{Dٻ�D��)D�<{D�{�Dں�D��3D�;�D�|)D۽D���D�:=D�z�Dܻ3D��{D�=qD�}Dݼ{D���D�;3D�{�D޻�D��3D�;3D�{3Dߺ�D���D�<)D�}qD��D���D�:�D�{�D�)D��3D�;�D�|{D��D���D�;�D�|�D�{D��{D�<)D�{�D仅D���D�<�D�}D�{D��)D�<)D�{�D��D��{D�<{D�|�D�{D��3D�;�D�{�D�3D��)D�<{D�|)D�{D���D�<)D�}D�)D���D�;3D�z�D뻅D��3D�:�D�{3D��D���D�;�D�{�D��3D���D�;�D�{�D�3D��3D�;�D�{�D��D���D�:�D�z=D�D���D�<{D�}D�{D��)D�<{D�|{D�D��)D�=D�|�D�D��3D�<)D�{3D��)D���D�<{D�|�D��D��)D�;3D�z�D��=D���D�=�D�|)D��3D���D�<�D�{�D���D���D�:�D�{�D��D��{D�<)D�|)D��{D��D�<)D�{�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aλ�Aλ�AνqA��EA���A���A���A��3Aκ�AηLAθ�Aκ�Aμ�Aι�Aλ�AζAγ�Aη�Aα'AΧRAΗYA·�A΁;A�s�A�poA�o�A�l�A�l�A�m�A�m)A�l�A�jA�e�A�b�A�^�A�\]A�[�A�W?A�A Aͼ6A��A�@�A1A���A���A�4�A��A�
�A��0A��oA�ӏA�E�A��A��_A�rA�YA���A��MA�lWA��VA���A��hA�?}A�y>A��A��ZA���A�t�A���A���A�NpA��[A�3�A���A��vA�IRA��A�q�A�ĜA���A��A�MA���A��=A���A��"A�~(A���A~��Ayr�Arq�Ao� Al�YAj�LAe�~A`�A^#:A]ѷAZ�AX�\AV�jAT%FARS&AN�AL�jAKxAIMAE��AC�AAt�A?xA>��A=E�A<*�A;�@A:�A9Y�A7��A5�tA4�=A1�@A.�~A-Z�A-*0A-#�A- �A-G�A,��A*�)A)�A)��A'�TA"��A!m�A!-A �A�2A��A8�A2aA��Am]AMjA_A^�A;A��A1'A�ArGA-wA�4A?�AA�A��A��A!�A�A��AXA��AP�A�:AA�A�A��A6zA��Az�AG�A0�A��A|�AA��A-A�wAy>A%�A��AzA9XA�?AVA�A�YA iA�At�A�6AFA	�A
��A
�=A
E�A	��A	�4A	�A�=A_pA�Aq�Ah
AP�A�6Ab�A:�A1�A(�A��A�A�-A�Ap;AMA_AA��A8�A.IAR�A��A��A��A;dA �6A n�A =@��z@�U�@�?�@�A�@�u%@���@���@��;@�=�@�9X@�"�@�Ɇ@��@��@���@�)_@��z@�'R@��@��d@�=@��@�=@�33@�4@�)�@�4@�$@킪@�҉@�f�@�@��m@��2@�y>@�� @�g�@��@�a|@��@�t�@�A @��@�@�@�0@�-w@�x@�x@�U�@��M@�l�@�	�@ߐ�@���@�I�@ݘ�@��y@�C�@۱[@�C�@�.I@��@��'@��.@�:�@��M@�~�@�)_@֞@��o@Ձ�@��@�z�@��D@Ӟ�@�]�@�)_@�6@Ѭq@�X@�z�@�	@��@ύP@�7L@���@�c�@��@���@̃�@�:�@˺^@�j�@��@�9X@��A@�8�@Ȓ�@�@Ǘ$@�C@��,@�v�@�7@�Q�@��c@���@�1'@ù�@�4@��@�\�@�$�@�x�@�tT@�s@�?}@��@��2@���@��@��U@���@�N�@���@��M@���@�%@�4�@�)_@��@���@�&�@�S&@�Mj@�h�@���@�7L@���@�ƨ@�Mj@��!@��@���@��{@�=�@���@��<@��@���@��.@��^@�Xy@�e�@�+�@��0@�V@�2�@�qv@�U�@�}V@��#@�33@�(�@��@�c�@�J#@�/@�	l@���@���@���@��I@�l"@�5?@��@�0�@�+@�@���@�Z@��K@��@�>B@�s�@�V@��'@�|�@�R�@�Ta@��W@���@�k�@�B�@�V@��@�Ɇ@�\�@��@��@��w@�U�@��@��[@���@��<@��@��@���@�iD@�+@���@���@�tT@�H@��@��a@��	@�'�@��@�%@��h@�n�@�Xy@�!@��@��@��6@���@���@�%F@��?@�e�@�e@��r@�خ@�s@��m@���@���@�_�@�<�@��@��P@�4@��@��@�B[@��@���@���@�:�@���@�bN@�=q@��@�
�@���@���@�zx@�@��@�PH@��A@�ϫ@��H@�f�@�%F@���@��$@�w�@�  @���@���@���@���@���@���@��p@��@��\@�v�@�d�@��@���@���@�J#@���@��@�Ov@�	@��]@��@��3@��@@���@��4@�Q�@�)_@��)@���@�p;@���@��@���@��@���@���@�l�@�.�@��@��H@���@�Mj@�0�@��@���@��x@�N�@�x@�ԕ@��@�a�@�33@��P@��X@��}@���@�~(@�c�@�!�@��}@���@�U�@��@�ȴ@��@�(�@��@��w@��V@�u�@�=�@���@��@��@�Q�@�!�@���@��=@�|@�Dg@���@�d�@��@�k@$t@~��@~�@~V@~@}B�@|�@|PH@{�w@{|�@{)_@z҉@z��@zYK@z+k@z
�@y��@y`B@x��@x�@x@wRT@vߤ@vd�@v+k@uԕ@uDg@t�@u;@t�U@tFt@s��@s�@rE�@r{@q��@qp�@q*0@p�K@p��@pFt@o�m@oE9@o$t@n��@n��@n�@mo @m�@l�e@l�z@lh�@l �@k�g@kt�@j�@j�h@jh
@jJ�@i��@i�T@i�@iw2@h�@h�I@h%�@g��@gx@g+@fd�@f@e��@ej@dr�@d-�@c��@c�@c]�@c!-@b�@bGE@a�T@a�@a��@aa�@a=�@`��@`A�@_�g@_�4@_=@^��@^��@^GE@]�^@]T�@\�O@\<�@\!@[��@[�{@[�@Z�@Z�'@Z��@Z��@ZW�@Y�D@Y��@Y`B@X�?@X��@X�Y@XQ�@X�@W��@W�$@W��@WJ#@W�@V�s@V��@VL0@V	@U��@U5�@T�@T�.@T>B@S�@Sqv@Rȴ@R��@R��@R#:@Q�z@Q��@Q2a@Q�@P��@P  @Oݘ@Ot�@O)_@N��@N�6@N��@M�.@M��@Mm]@M@L��@Lh�@L@Kl�@J�@J�@JC�@I�X@IVm@I�@H�j@HA�@H"h@H�@G�q@GiD@GP�@G@F{�@E�@D�f@D'R@C��@C��@C��@CU�@B��@Bn�@B0U@Ap�@AF@A*0@@��@@6@@  @?��@?�4@?]�@?�@>��@>h
@>�@=}�@=(�@<�@<��@<oi@<b@;��@;�$@;C�@;�@:��@:.�@9��@9��@9%@8�@8�@8m�@8<�@8(�@7�+@7�@7�}@7��@7�[@7v`@7\)@7S�@7"�@6�]@6��@6�}@6��@6kQ@6#:@6_@5��@5��@5p�@5 \@4��@4m�@4[�@46@3��@3��@3Z�@333@2��@2��@2q�@2H�@2O@1�@1�T@1��@1ϫ@1�3@1��@1s�@1Dg@0�|@0Ɇ@0�D@0"h@/�A@/ݘ@/��@/�[@/y�@/>�@.�M@.�]@.��@.~�@.:*@.J@-��@-�^@-��@-x�@-5�@,�	@,��@,,=@+��@+/�@*�m@*��@*E�@)�H@)[W@)&�@(��@(��@(��@(�@(q@(I�@(M@'�@'�@@'�@'K�@&ں@&�@&Ta@&J�@&$�@%�z@%s�@%0�@$��@$�K@$�@$�)@$��@$/�@#خ@#��@#�{@#j�@#RT@#C@"�@"��@"�x@"�@":*@!�@!�t@!��@!IR@ �@ �z@ _@ 6@  �@�@��@��@�f@s@W?@�@($@�T@��@L�@�@ѷ@l"@@�@�a@U�@+@$t@�@�H@��@��@#:@�@��@�@�=@Q�@<6@�5@[�@!@�@��@��@~�@Z�@8@
=@��@��@��@��@q�@)�@u@�#@��@u�@5�@q@�?@y>@C-@G@�a@��@U�@�@�M@��@��@��@p;@6�@$�@)�@$�@�@e@{@_@��@�j@��@c@G�@�@�@��@Z@A�@��@��@��@�P@_p@/�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aλ�Aλ�AνqA��EA���A���A���A��3Aκ�AηLAθ�Aκ�Aμ�Aι�Aλ�AζAγ�Aη�Aα'AΧRAΗYA·�A΁;A�s�A�poA�o�A�l�A�l�A�m�A�m)A�l�A�jA�e�A�b�A�^�A�\]A�[�A�W?A�A Aͼ6A��A�@�A1A���A���A�4�A��A�
�A��0A��oA�ӏA�E�A��A��_A�rA�YA���A��MA�lWA��VA���A��hA�?}A�y>A��A��ZA���A�t�A���A���A�NpA��[A�3�A���A��vA�IRA��A�q�A�ĜA���A��A�MA���A��=A���A��"A�~(A���A~��Ayr�Arq�Ao� Al�YAj�LAe�~A`�A^#:A]ѷAZ�AX�\AV�jAT%FARS&AN�AL�jAKxAIMAE��AC�AAt�A?xA>��A=E�A<*�A;�@A:�A9Y�A7��A5�tA4�=A1�@A.�~A-Z�A-*0A-#�A- �A-G�A,��A*�)A)�A)��A'�TA"��A!m�A!-A �A�2A��A8�A2aA��Am]AMjA_A^�A;A��A1'A�ArGA-wA�4A?�AA�A��A��A!�A�A��AXA��AP�A�:AA�A�A��A6zA��Az�AG�A0�A��A|�AA��A-A�wAy>A%�A��AzA9XA�?AVA�A�YA iA�At�A�6AFA	�A
��A
�=A
E�A	��A	�4A	�A�=A_pA�Aq�Ah
AP�A�6Ab�A:�A1�A(�A��A�A�-A�Ap;AMA_AA��A8�A.IAR�A��A��A��A;dA �6A n�A =@��z@�U�@�?�@�A�@�u%@���@���@��;@�=�@�9X@�"�@�Ɇ@��@��@���@�)_@��z@�'R@��@��d@�=@��@�=@�33@�4@�)�@�4@�$@킪@�҉@�f�@�@��m@��2@�y>@�� @�g�@��@�a|@��@�t�@�A @��@�@�@�0@�-w@�x@�x@�U�@��M@�l�@�	�@ߐ�@���@�I�@ݘ�@��y@�C�@۱[@�C�@�.I@��@��'@��.@�:�@��M@�~�@�)_@֞@��o@Ձ�@��@�z�@��D@Ӟ�@�]�@�)_@�6@Ѭq@�X@�z�@�	@��@ύP@�7L@���@�c�@��@���@̃�@�:�@˺^@�j�@��@�9X@��A@�8�@Ȓ�@�@Ǘ$@�C@��,@�v�@�7@�Q�@��c@���@�1'@ù�@�4@��@�\�@�$�@�x�@�tT@�s@�?}@��@��2@���@��@��U@���@�N�@���@��M@���@�%@�4�@�)_@��@���@�&�@�S&@�Mj@�h�@���@�7L@���@�ƨ@�Mj@��!@��@���@��{@�=�@���@��<@��@���@��.@��^@�Xy@�e�@�+�@��0@�V@�2�@�qv@�U�@�}V@��#@�33@�(�@��@�c�@�J#@�/@�	l@���@���@���@��I@�l"@�5?@��@�0�@�+@�@���@�Z@��K@��@�>B@�s�@�V@��'@�|�@�R�@�Ta@��W@���@�k�@�B�@�V@��@�Ɇ@�\�@��@��@��w@�U�@��@��[@���@��<@��@��@���@�iD@�+@���@���@�tT@�H@��@��a@��	@�'�@��@�%@��h@�n�@�Xy@�!@��@��@��6@���@���@�%F@��?@�e�@�e@��r@�خ@�s@��m@���@���@�_�@�<�@��@��P@�4@��@��@�B[@��@���@���@�:�@���@�bN@�=q@��@�
�@���@���@�zx@�@��@�PH@��A@�ϫ@��H@�f�@�%F@���@��$@�w�@�  @���@���@���@���@���@���@��p@��@��\@�v�@�d�@��@���@���@�J#@���@��@�Ov@�	@��]@��@��3@��@@���@��4@�Q�@�)_@��)@���@�p;@���@��@���@��@���@���@�l�@�.�@��@��H@���@�Mj@�0�@��@���@��x@�N�@�x@�ԕ@��@�a�@�33@��P@��X@��}@���@�~(@�c�@�!�@��}@���@�U�@��@�ȴ@��@�(�@��@��w@��V@�u�@�=�@���@��@��@�Q�@�!�@���@��=@�|@�Dg@���@�d�@��@�k@$t@~��@~�@~V@~@}B�@|�@|PH@{�w@{|�@{)_@z҉@z��@zYK@z+k@z
�@y��@y`B@x��@x�@x@wRT@vߤ@vd�@v+k@uԕ@uDg@t�@u;@t�U@tFt@s��@s�@rE�@r{@q��@qp�@q*0@p�K@p��@pFt@o�m@oE9@o$t@n��@n��@n�@mo @m�@l�e@l�z@lh�@l �@k�g@kt�@j�@j�h@jh
@jJ�@i��@i�T@i�@iw2@h�@h�I@h%�@g��@gx@g+@fd�@f@e��@ej@dr�@d-�@c��@c�@c]�@c!-@b�@bGE@a�T@a�@a��@aa�@a=�@`��@`A�@_�g@_�4@_=@^��@^��@^GE@]�^@]T�@\�O@\<�@\!@[��@[�{@[�@Z�@Z�'@Z��@Z��@ZW�@Y�D@Y��@Y`B@X�?@X��@X�Y@XQ�@X�@W��@W�$@W��@WJ#@W�@V�s@V��@VL0@V	@U��@U5�@T�@T�.@T>B@S�@Sqv@Rȴ@R��@R��@R#:@Q�z@Q��@Q2a@Q�@P��@P  @Oݘ@Ot�@O)_@N��@N�6@N��@M�.@M��@Mm]@M@L��@Lh�@L@Kl�@J�@J�@JC�@I�X@IVm@I�@H�j@HA�@H"h@H�@G�q@GiD@GP�@G@F{�@E�@D�f@D'R@C��@C��@C��@CU�@B��@Bn�@B0U@Ap�@AF@A*0@@��@@6@@  @?��@?�4@?]�@?�@>��@>h
@>�@=}�@=(�@<�@<��@<oi@<b@;��@;�$@;C�@;�@:��@:.�@9��@9��@9%@8�@8�@8m�@8<�@8(�@7�+@7�@7�}@7��@7�[@7v`@7\)@7S�@7"�@6�]@6��@6�}@6��@6kQ@6#:@6_@5��@5��@5p�@5 \@4��@4m�@4[�@46@3��@3��@3Z�@333@2��@2��@2q�@2H�@2O@1�@1�T@1��@1ϫ@1�3@1��@1s�@1Dg@0�|@0Ɇ@0�D@0"h@/�A@/ݘ@/��@/�[@/y�@/>�@.�M@.�]@.��@.~�@.:*@.J@-��@-�^@-��@-x�@-5�@,�	@,��@,,=@+��@+/�@*�m@*��@*E�@)�H@)[W@)&�@(��@(��@(��@(�@(q@(I�@(M@'�@'�@@'�@'K�@&ں@&�@&Ta@&J�@&$�@%�z@%s�@%0�@$��@$�K@$�@$�)@$��@$/�@#خ@#��@#�{@#j�@#RT@#C@"�@"��@"�x@"�@":*@!�@!�t@!��@!IR@ �@ �z@ _@ 6@  �@�@��@��@�f@s@W?@�@($@�T@��@L�@�@ѷ@l"@@�@�a@U�@+@$t@�@�H@��@��@#:@�@��@�@�=@Q�@<6@�5@[�@!@�@��@��@~�@Z�@8@
=@��@��@��@��@q�@)�@u@�#@��@u�@5�@q@�?@y>@C-@G@�a@��@U�@�@�M@��@��@��@p;@6�@$�@)�@$�@�@e@{@_@��@�j@��@c@G�@�@�@��@Z@A�@��@��@��@�P@_p@/�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	5�B	6B	5�B	5�B	5�B	5�B	5�B	5�B	5�B	5B	5?B	5%B	5?B	5%B	5%B	5%B	4�B	4�B	4�B	4�B	49B	3�B	2�B	2aB	2B	1�B	1�B	1�B	2B	1�B	1�B	1vB	1B	0�B	0!B	/�B	0!B	/�B	,�B	 �BĜB�B��B�B��B�OB��B�B	KB	$@B	?.B	ZkB	� B	��B
aB
	B
�B
_�B
�FB
�kB
��B
�JB
��B
�QB
��B
��B
�UB�B
�cB�BB�BoB
��B
��B
�IB
�nB
�B
�8B
�B
jeB
N�B
)�B

	B	��B	�B	�tB	��B	��B	�1B	f�B	cTB	^B	W$B	?HB	%B	�B	+B	�B�.B�LB�LB�B�KB�"B��B�%B��B�B��B�B��B	  B	�B	�B	xB	B	0B	�B	=B	3�B	2�B	0�B	0�B	1'B	2�B	<�B	@OB	G�B	G�B	E9B	6�B	B	NB	B	 'B	(�B	0�B	5�B	A�B	X+B	c�B	f�B	lqB	y	B	|�B	}�B	�AB	��B	�{B	��B	�B	�TB	��B	�)B	�iB	�5B	�GB	�B	��B	��B	��B	��B	ÖB	�{B	��B	��B	�B	��B	�B	�aB	��B	˒B	��B	�B	��B	�"B	οB	οB	ϑB	�.B	� B	��B	��B	�{B	՛B	ּB	��B	�mB	֡B	�$B	֡B	��B	�+B	خB	��B	�qB	��B	�CB	ܬB	��B	�B	ބB	ߊB	�B	��B	��B	��B	��B	�B	� B	�ZB	�B	�B	�,B	�8B	�B	�B	�B	��B	��B	�5B	��B	�]B	��B	��B	��B	�B	�aB	��B	�B	�B	�3B	�GB	�B	�cB	�B	��B	��B	�cB	�cB	�;B	�UB	��B	�OB	�}B	�/B	�B	��B	�B	�]B	�qB	�WB	�qB	�B	�WB	��B	�"B	��B	�B	��B	��B	�sB	�$B	�
B	�B	��B	�RB	�B	�8B	�8B	�B	��B	��B	�B	�B	�fB	��B	�B	�B	�B	��B	�fB	�B	�B	�8B	��B	��B	��B	�B	�B	�2B	�B	�B	�2B	�2B	�B	�LB	�B	�mB	�B	�$B	�>B	�$B	��B	�mB	�sB	�XB	�sB	��B	��B	�B	��B	��B	��B	��B	��B	�KB	��B	�0B	�B	�0B	��B	�B	��B	�B	��B	�B	�B	�eB	��B	�B	�B	�]B	�CB	��B	��B	�IB	�cB	�iB	�!B	�B	��B	��B	�KB	�KB	��B	�B	�;B	��B	�nB	�+B	��B	��B	�+B	��B	��B	��B	�PB	��B	��B	�(B	�PB	��B	��B	�xB	�>B	��B	��B	�>B	��B	��B	��B	�6B	�dB	�DB	�B	��B
 �B
�B	��B	��B	��B	��B	��B
3B
�B
B
B
 �B	�.B	�cB	�B	��B	��B	�B	��B	�B
 �B
B
�B
�B
�B
�B
uB
�B
�B
�B
�B
gB
aB
B
  B	��B	��B	�wB	��B	�}B
 �B
 �B
 �B
;B
�B
�B
gB
�B
B
EB
	RB

�B

	B

	B

�B
xB
�B
jB
PB
B
B
6B
6B
�B
0B
�B
B
6B
pB
}B
B
oB
B
hB
B
4B
�B
B
[B
uB
�B
uB
9B
$B
�B
�B
sB
YB
�B
B
�B
�B
�B
�B
1B
�B
B
KB
KB
�B
�B
7B
B
B
B
�B
WB
�B
]B
]B
�B
IB
IB
�B
�B
~B
IB
/B
dB
IB
/B
�B
jB
�B
 B
 BB
 BB
 B
 \B
!HB
!-B
!bB
!�B
!�B
"NB
"4B
!�B
"B
"�B
"�B
#�B
#�B
#�B
$B
$�B
%B
%B
%B
%B
$�B
%B
$�B
$�B
$tB
#�B
$tB
$�B
%FB
%FB
%FB
%`B
&B
%�B
%�B
'8B
'RB
($B
(�B
)*B
)�B
)�B
)�B
*eB
*�B
+B
+kB
+�B
,B
,B
+�B
+�B
,�B
-]B
-�B
.IB
.�B
/ B
.�B
.�B
.B
.}B
.�B
/B
/OB
/ B
/B
0;B
0;B
0;B
0�B
1AB
1AB
1AB
1[B
0�B
/�B
/�B
/�B
0;B
1'B
1�B
2-B
2�B
2�B
33B
33B
33B
3B
33B
3�B
4nB
4�B
5ZB
6B
5�B
5B
4�B
5�B
6zB
6`B
6zB
6�B
6�B
72B
7LB
88B
8�B
8�B
8�B
8�B
9XB
9�B
:^B
:�B
:�B
:�B
;0B
;�B
;�B
;�B
<�B
<�B
<�B
=B
=VB
=qB
=�B
=�B
=�B
>B
>(B
>BB
>]B
>�B
>�B
>�B
?B
>�B
>�B
?B
?�B
?cB
?�B
@B
@OB
@OB
A B
A B
A;B
AUB
BAB
B'B
B[B
B�B
B�B
B�B
CB
C{B
C�B
C�B
C�B
C�B
DB
DB
D�B
D�B
EB
EB
ESB
ESB
EmB
E�B
E�B
F�B
F�B
F�B
F�B
F�B
GEB
GEB
GzB
G_B
GEB
G_B
G�B
G�B
G�B
H�B
H�B
H�B
IlB
I�B
J	B
J	B
I�B
J=B
J�B
J=B
JrB
J�B
J�B
K�B
K�B
K�B
K�B
L�B
MB
M�B
NB
M�B
NB
N"B
N<B
NpB
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
OB
O(B
OvB
O�B
O�B
PB
P�B
Q4B
QNB
Q�B
Q�B
R B
Q�B
Q�B
R B
RoB
R�B
RoB
R�B
SB
R�B
SB
S�B
T�B
UMB
U�B
V9B
V9B
V9B
V�B
WsB
XEB
YKB
Y�B
Y�B
YB
Y�B
Z�B
Z�B
[=B
[#B
[=B
[qB
[�B
[�B
[�B
\]B
\�B
\�B
\�B
\�B
]IB
]dB
]~B
]�B
]�B
]�B
^�B
^�B
_B
_�B
_�B
`B
`'B
`\B
`�B
`�B
aB
aB
aB
a-B
aHB
abB
aHB
a|B
a�B
a�B
a�B
a�B
bB
bhB
bhB
bNB
b�B
cB
cTB
c�B
dB
dB
d&B
d@B
d�B
d�B
d�B
d�B
eB
eFB
e`B
ezB
e�B
e�B
e�B
e�B
e�B
e�B
fB
f2B
f�B
f�B
f�B
gB
gRB
g8B
g8B
gRB
gmB
g�B
g�B
g�B
g�B
g�B
h$B
h$B
h>B
hXB
hXB
hsB
h�B
h�B
i*B
iDB
i�B
jB
jeB
jKB
jB
kQB
k�B
k�B
k�B
k�B
lB
lB
lB
l=B
lWB
l�B
l�B
l�B
l�B
l�B
l�B
lqB
l=B
lWB
l�B
l�B
m]B
n/B
ncB
nIB
nIB
nIB
n�B
n}B
n}B
n}B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
oiB
oiB
oOB
oOB
oOB
oOB
o�B
o�B
pB
pUB
poB
p�B
p�B
p�B
p�B
p�B
q'B
q�B
rB
raB
r�B
r�B
r�B
sB
sB
s3B
s�B
tB
tB
t9B
tnB
t�B
t�B
uB
utB
u�B
u�B
u�B
u�B
u�B
u�B
vFB
vFB
vzB
v�B
v�B
v�B
wB
wLB
wfB
w�B
xB
xB
xB
xB
x8B
xlB
x�B
x�B
x�B
x�B
x�B
y	B
y>B
yrB
y�B
y�B
zB
zDB
z�B
z�B
z�B
{B
{JB
{dB
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|PB
|PB
|�B
|�B
|�B
}"B
}VB
}qB
}VB
}�B
}�B
}�B
}�B
}�B
~BB
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	5�B	6B	5�B	5�B	5�B	5�B	5�B	5�B	5�B	5%B	5ZB	5ZB	5ZB	5ZB	5?B	5ZB	5B	5B	5B	4�B	4�B	4B	33B	2|B	2GB	2-B	2-B	2-B	2GB	2-B	2B	1�B	1AB	0�B	0;B	0!B	0�B	1'B	1B	,�B�B�dB��B��B��B��B��B�B	�B	%�B	@iB	[�B	�[B	��B
�B
dB
$�B
e,B
�+B
�B
�cB
�\B
�"B
�/B
�5B
��B
��B�B
�-B�B�BJBB
�]B
�B
�\B
�B
�!B
��B
�-B
o B
TFB
.�B
(B	��B	ѷB	�xB	��B	�TB	�\B	jB	gB	a�B	]IB	D�B	'�B	/B	�B	KB	�B�xB�_BޞB�WB�}B�8B�rB��B��B�!B��B��B	UB	�B	�B	�B	pB	�B	B	;B	6�B	49B	1'B	0�B	1[B	2�B	>B	BuB	IRB	H�B	HKB	;�B	�B	B	�B	 �B	)_B	1B	5�B	B[B	X�B	dZB	gRB	m]B	y�B	}<B	~�B	��B	�RB	��B	��B	��B	��B	�>B	��B	�oB	��B	��B	��B	�B	�oB	B	��B	�B	��B	�gB	āB	ðB	�-B	�aB	��B	�zB	�0B	�~B	͟B	�pB	��B	�BB	�BB	�HB	бB	ѝB	�uB	ԯB	�2B	�SB	�sB	�mB	��B	�sB	��B	�
B	�EB	ؓB	�1B	ڠB	��B	ܒB	��B	�B	�~B	ޞB	��B	��B	�`B	�B	�:B	�B	�:B	�nB	�ZB	��B	��B	��B	�zB	�B	�>B	�XB	�B	�IB	�B	��B	�qB	��B	�oB	�B	�vB	�B	��B	�B	��B	��B	��B	��B	�[B	�B	�iB	�B	�oB	�B	�B	��B	�B	�UB	��B	��B	�}B	��B	�IB	�B	�B	��B	��B	�B	��B	�B	�qB	��B	��B	�6B	�B	�B	��B	�B	�sB	�
B	�sB	�B	�RB	�B	�B	�B	�RB	�mB	�B	�B	��B	�2B	�B	�fB	�B	�B	��B	�RB	�B	�B	�mB	�RB	�B	�B	��B	�B	�8B	��B	�B	�B	�B	��B	�B	��B	�$B	�sB	�B	�sB	�$B	�
B	��B	�B	�B	�DB	�B	��B	�_B	�*B	�B	�B	�B	�B	�eB	�B	��B	�B	�QB	�kB	�kB	�B	�B	�B	��B	��B	�B	�"B	�B	��B	�B	�/B	�/B	�B	��B	��B	�oB	�B	�B	�}B	�B	�eB	�B	�B	�UB	�B	��B	��B	�2B	�fB	�FB	��B	��B	��B	��B	��B	�]B	��B	��B	�BB	�<B	��B	��B	�RB	�B	��B	�>B	��B	��B	�jB	��B	�DB	�6B	��B
 B
[B	��B	�^B	��B	��B	�]B
gB
�B
AB
�B
UB	��B
  B	�jB	��B	��B	�<B	�"B	�(B
 �B
AB
�B
'B
�B
[B
�B
B
�B
-B
�B
B
�B
�B
 �B	��B	�(B	��B	��B	��B
UB
 �B
 �B
oB
�B
�B
�B
B
SB
zB
	�B

�B

XB

=B

�B
�B
�B
�B
�B
jB
jB
�B
jB
B
~B
0B
6B
�B
�B
�B
4B
�B
:B
�B
NB
NB
�B
@B
uB
�B
�B
�B
�B
sB
�B
�B
�B
�B
�B
+B
�B
B
B
KB
B
KB
B
B
�B
B
B
�B
kB
�B
QB
	B
�B
�B
�B
�B
B
�B
�B
/B
�B
�B
�B
~B
�B
~B
~B
B
�B
�B
 BB
 vB
 vB
 \B
 �B
!|B
!HB
!|B
!�B
!�B
"�B
"hB
"4B
"NB
"�B
# B
#�B
$B
$&B
$&B
$�B
%,B
%FB
%FB
%`B
%`B
%FB
%FB
%zB
$�B
#�B
$�B
%B
%`B
%�B
%�B
%�B
&LB
%�B
&2B
'mB
'�B
(XB
(�B
)_B
)�B
*B
*KB
*�B
*�B
+kB
+�B
,"B
,"B
,=B
,"B
,"B
-B
-�B
./B
.�B
.�B
/5B
/B
.�B
.IB
.�B
/5B
/iB
/�B
/5B
/iB
0oB
0�B
0�B
0�B
1vB
1�B
1�B
1�B
0�B
0B
/�B
0B
0UB
1vB
1�B
2�B
3B
33B
3�B
3MB
3MB
3MB
3MB
3�B
4�B
5%B
5�B
6`B
5�B
5?B
5?B
5�B
6�B
6�B
6�B
6�B
72B
7fB
7LB
8lB
9>B
8�B
8�B
9$B
9�B
:*B
:�B
:�B
;B
;0B
;dB
<B
<B
;�B
=B
=B
=<B
=VB
=�B
=�B
=�B
=�B
=�B
>(B
>wB
>�B
>�B
>�B
?B
>�B
?HB
?.B
?B
?HB
?�B
?�B
@ B
@OB
@�B
@�B
A;B
AUB
A�B
A�B
BuB
BAB
B�B
B�B
B�B
CB
CaB
C�B
C�B
C�B
C�B
D3B
DgB
DgB
D�B
EB
E9B
EmB
EmB
E�B
E�B
E�B
FB
F�B
F�B
F�B
GB
GB
GzB
GzB
G�B
G�B
GzB
G�B
G�B
G�B
H1B
H�B
H�B
H�B
I�B
I�B
J=B
J#B
J#B
JXB
J�B
JXB
J�B
J�B
KB
K�B
K�B
L0B
L0B
L�B
MPB
NB
N<B
NB
N<B
NVB
NpB
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O(B
OBB
O�B
O�B
P.B
PbB
P�B
QhB
Q�B
Q�B
R B
R:B
Q�B
RB
RoB
R�B
R�B
R�B
S&B
S@B
S&B
S[B
TFB
T�B
U�B
VB
V9B
VSB
VmB
V�B
W�B
X_B
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
[qB
[WB
[qB
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
]B
]dB
]�B
]�B
]�B
^B
^5B
^�B
_B
_;B
_�B
_�B
`BB
`BB
`�B
`�B
aB
a-B
a-B
aHB
aHB
a|B
a|B
a|B
a�B
a�B
a�B
a�B
bB
bNB
b�B
b�B
bhB
b�B
c:B
c�B
c�B
d@B
d@B
dZB
dtB
d�B
d�B
d�B
e,B
eFB
ezB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
fB
f2B
ffB
f�B
f�B
f�B
gB
gmB
gRB
gmB
gmB
g�B
g�B
g�B
g�B
h
B
h
B
hXB
h>B
hsB
hsB
hsB
h�B
h�B
h�B
iyB
iyB
i�B
jeB
jB
jeB
j�B
kkB
k�B
k�B
k�B
lB
l"B
l=B
l=B
lWB
l�B
l�B
l�B
l�B
m)B
mB
l�B
l�B
lWB
l�B
l�B
mB
mwB
nIB
n}B
n}B
n}B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o B
o�B
o�B
o�B
o�B
oiB
oiB
pB
p!B
p;B
poB
p�B
p�B
p�B
p�B
qB
p�B
q[B
q�B
rGB
r�B
r�B
r�B
r�B
sMB
s3B
sMB
tB
t9B
t9B
tTB
t�B
t�B
t�B
u?B
u�B
u�B
u�B
u�B
u�B
u�B
vFB
vzB
vzB
v�B
v�B
v�B
v�B
wLB
wfB
w�B
w�B
x8B
x8B
x8B
x8B
xRB
x�B
x�B
x�B
x�B
x�B
x�B
y>B
yrB
y�B
y�B
z*B
z*B
zxB
z�B
z�B
{B
{0B
{B
{B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|B
|6B
|jB
|�B
|�B
|�B
}B
}<B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~(B
~wB
~�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.13(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202006290041142020062900411420200629004114202006290200492020062902004920200629020049202207271538392022072715383920220727153839  JA  ARFMdecpA30a                                                                20200617123800  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200617123801  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200617123802  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200617123802  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200617123802  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200617123803  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200617123803                      G�O�G�O�G�O�                JA  ARUP                                                                        20200617125347                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200619000000  CF  PSAL_ADJUSTED_QC@�@~{G�O�                JM  ARCAJMQC2.0                                                                 20200628154114  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200628154114  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20200628170049  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063839  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091505                      G�O�G�O�G�O�                