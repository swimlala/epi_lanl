CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-12-28T09:37:21Z creation;2019-12-28T09:37:25Z conversion to V3.1;2023-06-29T05:50:22Z update;     
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
_FillValue                 �  ]8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ά   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191228093721  20230705031507  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_199                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @����f�1   @��ѿ�� @7����o�b��~($1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�3D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D�� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�	�D�0 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @hQ�@�(�@�(�A
{A*{AJ{Aj{A�
=A�
=A��
A�
=A�
=A�
=A�
=A�
=B�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �HC�HC�HC�HC�HC
�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC �HC"�HC$�HC&�HC(�HC*�HC,�HC.�HC0�HC2�HC4�HC6�HC8�HC:�HC<�HC>�HC@�HCB�HCD�HCF��CH�HCJ�HCL�HCN�HCP�HCR�HCT�HCV�HCX�HCZ�HC\�HC^�HC`�HCb�HCd�HCf�HCh�HCj�HCl�HCn�HCp�HCr�HCt�HCv�HCx�HCz�HC|�HC~�HC�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�C�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�C�P�D (RD �RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD	(RD	�RD
(RD
�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD(RD�RD (RD �RD!(RD!�RD"(RD"�RD#(RD#�RD$(RD$�RD%(RD%�RD&(RD&�RD'(RD'�RD((RD(�RD)(RD)�RD*(RD*�RD+(RD+�RD,(RD,�RD-(RD-�RD.(RD.��D/(RD/�RD0(RD0�RD1(RD1�RD2(RD2�RD3(RD3�RD4(RD4�RD5(RD5�RD6(RD6�RD7.�D7�RD8(RD8�RD9(RD9�RD:(RD:�RD;(RD;�RD<(RD<�RD=(RD=�RD>(RD>�RD?(RD?�RD@(RD@�RDA(RDA�RDB(RDB�RDC(RDC�RDD(RDD�RDE(RDE�RDF(RDF�RDG(RDG�RDH(RDH�RDI(RDI�RDJ(RDJ�RDK(RDK�RDL(RDL�RDM(RDM�RDN(RDN�RDO(RDO�RDP(RDP�RDQ(RDQ�RDR(RDR�RDS(RDS�RDT(RDT�RDU(RDU�RDV(RDV�RDW(RDW�RDX(RDX�RDY(RDY�RDZ(RDZ�RD[(RD[�RD\(RD\�RD](RD]�RD^(RD^�RD_(RD_�RD`(RD`�RDa(RDa�RDb(RDb�RDc(RDc�RDd(RDd�RDe(RDe�RDf(RDf�RDg(RDg�RDh(RDh�RDi(RDi�RDj(RDj�RDk(RDk�RDl(RDl�RDm(RDm�RDn(RDn�RDo(RDo�RDp(RDp�RDq(RDq�RDr(RDr�RDs(RDs�RDt(RDt�RDu(RDu�RDv(RDv�RDw(RDw�RDx(RDx�RDy(RDy�RDz(RDz�RD{(RD{�RD|(RD|�RD}(RD}�RD~(RD~�RD(RD�RD�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��\D��)D�)D�T)D��)D���D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D��D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D)D��)D�)D�T)DÔ)D��)D�)D�T)DĔ)D��)D�)D�T)DŔ)D��)D�)D�T)DƔ)D��)D�)D�T)Dǔ)D��)D�)D�T)DȔ)D��)D�)D�T)Dɔ)D��)D�)D�T)Dʔ)D��)D�\D�T)D˔)D��)D�)D�T)D̔)D��)D�)D�T)D͔)D��)D�)D�T)DΔ)D��)D�)D�T)Dϔ)D��)D�)D�T)DД)D��)D�)D�T)Dє)D��)D�)D�T)DҔ)D��)D�)D�T)DӔ)D��)D�)D�T)DԔ)D��)D�)D�T)DՔ)D��)D�)D�T)D֔)D��)D�)D�T)Dה)D��)D�)D�T)Dؔ)D��)D�)D�T)Dٔ)D��)D�)D�T)Dڔ)D��)D�)D�T)D۔)D��)D�)D�T)Dܔ)D��)D�)D�T)Dݔ)D��)D�)D�T)Dޔ)D��)D�)D�T)Dߔ)D��)D�)D�T)D��)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�P�D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�\D�T)D�)D��)D�)D�T)D�)D��\D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D�)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D�)D�T)D��)D��)D��D�D)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��TA��HA��;A��TA��yA��yA��mA��`A��;A�ȴA�ĜAɬAɮAɮAɮAɮAɮAɬAɬAɬAɮAɮAɮAɰ!Aɰ!Aɰ!Aɰ!Aɲ-Aɲ-Aɲ-Aɴ9Aɴ9Aɴ9Aɴ9Aɴ9Aɴ9Aɴ9AɶFAɶFAɶFAɶFAɸRAɶFAɶFAɴ9AɮA�;dAƴ9A�E�A��wA�x�A�=qA�  A�dZA���A�ĜA�x�A��A�x�A���A�  A�O�A�\)A��A�v�A���A�hsA��A�&�A���A���A�\)A�
=A�A��7A��A��A���A�1'A���A�ȴA�l�A���A��A��
A��A�hsA�A��HA�O�A��A��RA��\A���A���A���A�(�A�v�A�{A�-A���A�Q�A�oA��wA���A��A�I�A��`A��HA��/A���A�v�A�~�A��7A���A�A��A���A/A{;dAw|�Atz�Au�TAv~�Ar  An�Aj �Ag�TAgS�Ae%A`�uA`(�A_33AYƨAV��AVbNAT�+AQAO�AM&�AJ�AGƨAE|�AB�uAA��A@bNA>n�A>ZA=x�A<$�A:�yA:��A9�PA7�FA6�!A6JA4�A3�7A2��A2VA2A1��A0  A,��A+�PA+A)t�A)�#A'ƨA&jA%l�A$��A#O�A"  A��A�AI�A~�A?}A~�A��A��A=qA�PAz�A��A��A�A�!AA�A�TAA�AA�A�-AK�A
=A�+A1'A�^A`BA
�uA
A	��A	�FA�AA��A=qA{A�PAn�Ap�A?}A��A-A�^AdZA ��A ��A ��@���@�C�@�/@�F@�@�^5@�{@�x�@���@�\@�j@�|�@��#@��/@�9X@���@���@睲@�33@���@�z�@�D@�9@��/@�A�@�
=@���@��#@�O�@�I�@�I�@�V@ج@�Z@��H@ԛ�@ҸR@�-@�{@��T@�`B@���@�1'@�@ΰ!@�
=@�o@·+@���@̬@��m@�/@�1'@�b@ȓu@�bN@�C�@ļj@å�@å�@Õ�@�K�@ļj@ũ�@��@�@��@�bN@�33@+@���@��D@��@�j@�9X@�\)@��P@�ȴ@��P@�S�@�O�@�\)@�5?@��D@��P@��;@��@���@��@��P@��@�G�@��@��@��@���@�|�@��w@���@���@�5?@�{@��^@���@�@���@��y@�K�@��!@�ȴ@��+@�bN@�K�@� �@��u@��/@�7L@�?}@�7L@��@���@�o@�ȴ@�33@�K�@�t�@�l�@�  @��@��D@�@��T@��@���@��!@�\)@�b@��w@�hs@�hs@���@�v�@�b@�V@�j@�l�@�~�@��#@���@��H@��H@���@��!@���@�
=@�Z@�X@��@��F@�;d@���@�X@��P@��P@�
=@�M�@�@�@�@�z�@��;@�  @��@���@���@�(�@��
@�t�@��@���@�n�@��R@���@�~�@�n�@�@��#@�x�@��9@�ƨ@�S�@�|�@�+@��@�+@�+@�+@�o@�E�@��@���@���@�hs@��@���@���@��@�Ĝ@��@��@��F@�t�@�C�@�33@��H@���@�ff@�$�@��@���@��7@�x�@�&�@�I�@��@�1@��w@��@���@���@��@�l�@�S�@�o@���@��!@�ff@�5?@��@�@���@�?}@�%@��`@��j@�r�@�1'@�(�@�b@��
@���@�S�@��@��y@��+@�v�@�n�@�5?@���@��T@��#@�@���@�O�@��@��j@��@�9X@��@�ƨ@���@�K�@��H@��R@�~�@�=q@��@��@��^@���@�7L@���@���@�Ĝ@��@�z�@�A�@��@��@�@~��@~$�@}��@}�h@}�@}?}@|�D@|(�@{�
@{dZ@z�H@z^5@y��@y�#@y��@y7L@x�9@xQ�@xA�@xb@w��@w��@wl�@w�@v�@vff@v{@u��@u�@uO�@t��@t��@tZ@s�
@s��@sdZ@s@r��@r�\@r=q@q��@q��@qX@q�@p��@pQ�@o�;@o�@o\)@oK�@o�@n��@nV@n$�@m�h@mO�@l�@l��@lI�@kƨ@kC�@j�@j�!@j=q@i��@iG�@hĜ@hA�@h �@h  @g��@g�w@g|�@f�y@e�T@e�@ep�@ep�@e?}@e/@e�@d�@dI�@c�
@c�F@c��@ct�@cS�@co@b�\@a�#@ahs@a�@`��@`A�@_�@_�w@_�P@_+@^�y@^��@^ff@^$�@]�@]��@]`B@]?}@]V@\��@\��@[��@[��@Z��@Z=q@Y��@Y&�@XĜ@X�@XQ�@Xb@W��@Wl�@W+@V�y@V�R@Vv�@V@U�-@U`B@U/@T�@T�@TZ@T�@S��@S33@R�H@R~�@R�@Q��@Q&�@Q%@P�u@P �@O�;@O�w@O;d@N�@N�R@N�+@NV@N{@M�@L��@LZ@K��@K��@K�@KS�@Ko@J��@Jn�@I��@I��@H��@H�u@HA�@Hb@G��@Gl�@F��@F��@FV@F5?@F$�@E�@E�T@E��@EO�@EV@D��@D�D@DI�@C��@C�F@Ct�@C"�@B��@B^5@B�@A��@Ax�@@��@@�u@@b@?��@?�P@?+@>�@>�+@>E�@>$�@=�@=�T@=��@=@=p�@=�@<�@<I�@<(�@;�m@;��@;C�@;33@;@:�\@:^5@:=q@:�@9��@9�#@9��@9��@9hs@9�@8��@8��@8bN@8Q�@81'@7�@7�@6ȴ@6��@6�+@6V@5�@5@5@5�-@5�h@5�@4�@4j@4I�@4(�@3�m@3�F@3�@3dZ@3C�@3"�@2��@2�!@2��@2n�@2-@1��@1�#@1��@1�^@1x�@1X@1�@0�`@0Ĝ@0�u@0r�@0A�@/��@/�P@/\)@/
=@.�@.��@.E�@-�@-p�@-O�@,�j@,Z@,1@+�F@+dZ@+C�@+33@+"�@*~�@*-@)�7@)7L@)%@(�`@(��@(�u@(b@'�;@'�P@'K�@'�@&�y@&��@&V@%�@%@%p�@$�@$��@$j@$9X@$(�@$�@$1@#��@#�F@#dZ@#S�@#"�@"��@"~�@"n�@"M�@"-@"�@!��@!��@!��@!x�@!hs@!X@!�@!%@ �`@ �u@ bN@ 1'@�@�@�P@l�@
=@��@�@��@V@��@@�-@`B@�@�@��@�@��@z�@I�@I�@��@1@1@�m@�F@�@S�@33@�H@~�@^5@^5@M�@�@�@��@�7@7L@��@�@bN@A�@1'@ �@  @�;@�w@�P@\)@+@
=@�y@�R@��@�+@E�@E�@$�@@�T@��@p�@O�@V@��@I�@9X@(�@��@�@S�@33@"�@�@��@^5@-@�^@X@&�@�@��@Ĝ@��@�@r�@bN@bN@Q�@1'@  @�w@|�@K�@
=@��@ff@5?@$�@@��@@�-@�@`B@`B@�@�@`B@�@�@��@�@��@��@j@I�@I�@I�@(�@�@�
@ƨ@��@�@S�@C�@33@"�@o@@
��@
��@
�!@
�\@
^5@
=q@
�@
J@	��@	�@	��@	��@	��@	��@	G�@	7L@	7L@	&�@	%@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��TA��HA��;A��TA��yA��yA��mA��`A��;A�ȴA�ĜAɬAɮAɮAɮAɮAɮAɬAɬAɬAɮAɮAɮAɰ!Aɰ!Aɰ!Aɰ!Aɲ-Aɲ-Aɲ-Aɴ9Aɴ9Aɴ9Aɴ9Aɴ9Aɴ9Aɴ9AɶFAɶFAɶFAɶFAɸRAɶFAɶFAɴ9AɮA�;dAƴ9A�E�A��wA�x�A�=qA�  A�dZA���A�ĜA�x�A��A�x�A���A�  A�O�A�\)A��A�v�A���A�hsA��A�&�A���A���A�\)A�
=A�A��7A��A��A���A�1'A���A�ȴA�l�A���A��A��
A��A�hsA�A��HA�O�A��A��RA��\A���A���A���A�(�A�v�A�{A�-A���A�Q�A�oA��wA���A��A�I�A��`A��HA��/A���A�v�A�~�A��7A���A�A��A���A/A{;dAw|�Atz�Au�TAv~�Ar  An�Aj �Ag�TAgS�Ae%A`�uA`(�A_33AYƨAV��AVbNAT�+AQAO�AM&�AJ�AGƨAE|�AB�uAA��A@bNA>n�A>ZA=x�A<$�A:�yA:��A9�PA7�FA6�!A6JA4�A3�7A2��A2VA2A1��A0  A,��A+�PA+A)t�A)�#A'ƨA&jA%l�A$��A#O�A"  A��A�AI�A~�A?}A~�A��A��A=qA�PAz�A��A��A�A�!AA�A�TAA�AA�A�-AK�A
=A�+A1'A�^A`BA
�uA
A	��A	�FA�AA��A=qA{A�PAn�Ap�A?}A��A-A�^AdZA ��A ��A ��@���@�C�@�/@�F@�@�^5@�{@�x�@���@�\@�j@�|�@��#@��/@�9X@���@���@睲@�33@���@�z�@�D@�9@��/@�A�@�
=@���@��#@�O�@�I�@�I�@�V@ج@�Z@��H@ԛ�@ҸR@�-@�{@��T@�`B@���@�1'@�@ΰ!@�
=@�o@·+@���@̬@��m@�/@�1'@�b@ȓu@�bN@�C�@ļj@å�@å�@Õ�@�K�@ļj@ũ�@��@�@��@�bN@�33@+@���@��D@��@�j@�9X@�\)@��P@�ȴ@��P@�S�@�O�@�\)@�5?@��D@��P@��;@��@���@��@��P@��@�G�@��@��@��@���@�|�@��w@���@���@�5?@�{@��^@���@�@���@��y@�K�@��!@�ȴ@��+@�bN@�K�@� �@��u@��/@�7L@�?}@�7L@��@���@�o@�ȴ@�33@�K�@�t�@�l�@�  @��@��D@�@��T@��@���@��!@�\)@�b@��w@�hs@�hs@���@�v�@�b@�V@�j@�l�@�~�@��#@���@��H@��H@���@��!@���@�
=@�Z@�X@��@��F@�;d@���@�X@��P@��P@�
=@�M�@�@�@�@�z�@��;@�  @��@���@���@�(�@��
@�t�@��@���@�n�@��R@���@�~�@�n�@�@��#@�x�@��9@�ƨ@�S�@�|�@�+@��@�+@�+@�+@�o@�E�@��@���@���@�hs@��@���@���@��@�Ĝ@��@��@��F@�t�@�C�@�33@��H@���@�ff@�$�@��@���@��7@�x�@�&�@�I�@��@�1@��w@��@���@���@��@�l�@�S�@�o@���@��!@�ff@�5?@��@�@���@�?}@�%@��`@��j@�r�@�1'@�(�@�b@��
@���@�S�@��@��y@��+@�v�@�n�@�5?@���@��T@��#@�@���@�O�@��@��j@��@�9X@��@�ƨ@���@�K�@��H@��R@�~�@�=q@��@��@��^@���@�7L@���@���@�Ĝ@��@�z�@�A�@��@��@�@~��@~$�@}��@}�h@}�@}?}@|�D@|(�@{�
@{dZ@z�H@z^5@y��@y�#@y��@y7L@x�9@xQ�@xA�@xb@w��@w��@wl�@w�@v�@vff@v{@u��@u�@uO�@t��@t��@tZ@s�
@s��@sdZ@s@r��@r�\@r=q@q��@q��@qX@q�@p��@pQ�@o�;@o�@o\)@oK�@o�@n��@nV@n$�@m�h@mO�@l�@l��@lI�@kƨ@kC�@j�@j�!@j=q@i��@iG�@hĜ@hA�@h �@h  @g��@g�w@g|�@f�y@e�T@e�@ep�@ep�@e?}@e/@e�@d�@dI�@c�
@c�F@c��@ct�@cS�@co@b�\@a�#@ahs@a�@`��@`A�@_�@_�w@_�P@_+@^�y@^��@^ff@^$�@]�@]��@]`B@]?}@]V@\��@\��@[��@[��@Z��@Z=q@Y��@Y&�@XĜ@X�@XQ�@Xb@W��@Wl�@W+@V�y@V�R@Vv�@V@U�-@U`B@U/@T�@T�@TZ@T�@S��@S33@R�H@R~�@R�@Q��@Q&�@Q%@P�u@P �@O�;@O�w@O;d@N�@N�R@N�+@NV@N{@M�@L��@LZ@K��@K��@K�@KS�@Ko@J��@Jn�@I��@I��@H��@H�u@HA�@Hb@G��@Gl�@F��@F��@FV@F5?@F$�@E�@E�T@E��@EO�@EV@D��@D�D@DI�@C��@C�F@Ct�@C"�@B��@B^5@B�@A��@Ax�@@��@@�u@@b@?��@?�P@?+@>�@>�+@>E�@>$�@=�@=�T@=��@=@=p�@=�@<�@<I�@<(�@;�m@;��@;C�@;33@;@:�\@:^5@:=q@:�@9��@9�#@9��@9��@9hs@9�@8��@8��@8bN@8Q�@81'@7�@7�@6ȴ@6��@6�+@6V@5�@5@5@5�-@5�h@5�@4�@4j@4I�@4(�@3�m@3�F@3�@3dZ@3C�@3"�@2��@2�!@2��@2n�@2-@1��@1�#@1��@1�^@1x�@1X@1�@0�`@0Ĝ@0�u@0r�@0A�@/��@/�P@/\)@/
=@.�@.��@.E�@-�@-p�@-O�@,�j@,Z@,1@+�F@+dZ@+C�@+33@+"�@*~�@*-@)�7@)7L@)%@(�`@(��@(�u@(b@'�;@'�P@'K�@'�@&�y@&��@&V@%�@%@%p�@$�@$��@$j@$9X@$(�@$�@$1@#��@#�F@#dZ@#S�@#"�@"��@"~�@"n�@"M�@"-@"�@!��@!��@!��@!x�@!hs@!X@!�@!%@ �`@ �u@ bN@ 1'@�@�@�P@l�@
=@��@�@��@V@��@@�-@`B@�@�@��@�@��@z�@I�@I�@��@1@1@�m@�F@�@S�@33@�H@~�@^5@^5@M�@�@�@��@�7@7L@��@�@bN@A�@1'@ �@  @�;@�w@�P@\)@+@
=@�y@�R@��@�+@E�@E�@$�@@�T@��@p�@O�@V@��@I�@9X@(�@��@�@S�@33@"�@�@��@^5@-@�^@X@&�@�@��@Ĝ@��@�@r�@bN@bN@Q�@1'@  @�w@|�@K�@
=@��@ff@5?@$�@@��@@�-@�@`B@`B@�@�@`B@�@�@��@�@��@��@j@I�@I�@I�@(�@�@�
@ƨ@��@�@S�@C�@33@"�@o@@
��@
��@
�!@
�\@
^5@
=q@
�@
J@	��@	�@	��@	��@	��@	��@	G�@	7L@	7L@	&�@	%@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ÖB
ÖB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ǮB
ȴB
ȴB
��B
�B<jB8RBB�BD�BF�BP�B]/Bk�Bu�B{�B�{BǮB�B��B�;B��B�B�B�B�B$�B+B2-BE�BM�BK�BL�BL�BH�BP�BO�BQ�BVBW
BQ�BC�B+BVBA�BE�B<jB2-B+B�BoBuBPB��B�yB�B��B��B��BŢBǮBB�'Br�BgmBffBN�B&�BB
�HB
��B
s�B
hsB
<jB
JB	��B	��B	ǮB	��B	�bB	��B	�mB	��B	�B	�JB	w�B	q�B	l�B	M�B	G�B	A�B	.B	�B	oB	%B�yB�B��B�-B��B�bB�%B�%B�+B|�B�B�=B~�Bz�B{�B}�B{�B|�B~�B�B�DB�PB�VB�VB�\B�oBy�Br�B� Br�B�Bx�Bt�Br�Bq�Bq�Bo�BffBaHBcTBaHB_;BaHBe`BgmBdZB`BB^5B\)BZBW
BVBVBVBVBVBXBXBYBZBZBYBYB_;BaHB_;B]/B_;BhsBgmBdZBaHB`BBaHBcTBbNB`BB`BB_;B`BBhsBgmBgmBe`BbNBYBT�BP�B@�B:^B:^B9XB=qB>wB=qB;dB?}BC�BE�BF�BJ�BN�BT�BS�BR�BN�BZB]/BbNBdZBbNB^5BZBXBXB^5BZBR�BVBVBM�BJ�BQ�B\)B\)B[#B\)B[#BYBZBcTBgmBiyBo�Bs�Bs�Bo�Bk�Bl�Bt�Bw�Bt�Bo�Bm�Bo�Bq�Bs�B~�B�%B�uB��B�uB�{B��B��B��B��B��B�-B�9B�XBŢBɺB��B��B��BŢBÖB�wB�qBÖBɺBƨBƨB��B�;B�NB�ZB�ZB�TB�yB�B�B��B��B��B��B	B��B��B��B��B�yB�sB�B�B�B�sB�B��B��B	B	B	%B	+B		7B		7B	\B	oB	�B	�B	!�B	'�B	1'B	5?B	49B	33B	49B	2-B	.B	49B	<jB	@�B	7LB	9XB	/B	/B	:^B	I�B	E�B	C�B	C�B	D�B	C�B	O�B	T�B	W
B	[#B	\)B	_;B	ffB	n�B	l�B	m�B	n�B	m�B	k�B	gmB	k�B	m�B	n�B	o�B	q�B	r�B	p�B	q�B	s�B	{�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�7B	�=B	�JB	�JB	�\B	�\B	�VB	�JB	�DB	�VB	�\B	�bB	�oB	�uB	�uB	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�?B	�RB	�XB	�^B	�^B	�jB	�qB	�qB	�qB	�wB	�wB	�}B	��B	��B	B	ÖB	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�;B	�BB	�HB	�NB	�TB	�ZB	�ZB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
JB
PB
PB
VB
VB
VB
\B
\B
bB
bB
bB
oB
uB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
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
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
M�B
L�B
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
P�B
P�B
P�B
P�B
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
S�B
T�B
T�B
T�B
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
XB
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
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
bNB
bNB
aHB
bNB
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
e`B
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
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
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
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
o�B
p�B
p�B
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
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
t�B
t�B
t�B
t�B
t�B
t�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�gB
�gB
�gB
�gB
�gB
�gB
�gB
āB
ĜB
�{B
ÖB
�gB
�gB
�gB
�gB
�gB
�gB
�gB
�gB
�gB
�gB
�gB
�gB
�gB
�gB
�gB
�mB
�mB
�mB
�mB
�mB
�mB
�tB
�tB
�tB
�tB
�zB
�zB
�zB
�zB
�zB
ǔB
ǔB
��B
�lB
˒B
յB
��B?HB:^BB�BE9BG�BRoB^�Bk�Bu�B|�B��B�)B�jBѷB�'B�BjB�B�B�B'B,�B3MBF%BN<BLdBN"BNVBI�BQ�BQ4BS[BV�BX_BS�BF?B-�B�BBuBG+B=�B3hB-]B!�B�B{B\B�B��B�7BԕB��B�JB�YB��B�SB��Bt�Bh�Bi*BR�B)�B�B
�mB
��B
vzB
mB
B'B
vB
 iB	��B	�xB	��B	��B	�AB	��B	�mB	�3B	��B	yXB	t�B	p�B	N�B	J#B	F�B	0�B	�B	�B		�B�BܬB��B�ZB��B�@B�_B��B��B}qB�?B��B�4B{B}<B�B}B}�B�iB�YB��B��B��B�BB��B��B{0Bs�B�UBsB�Bz^Bu�Bs�Bs�Bs�BrBh�BcnBeFBb�B`BbBfLBh>Be,BabB_B]IB[�BW�BV�BU�BVBV9BV�BX�BX�BYeBZQBZ�BYBY�B_�Ba�B_�B]~B_�Bi�BhsBeFBa�B`vBa�BdtBc B`vB`�B_�B`�Bh�Bg�Bg�BfBdtBZQBV�BSuBAB:xB:xB9�B>]B?HB>]B<B@4BC�BE�BF�BJ�BN�BUBT�BS[BN�BY�B]Bb�BeFBc�B_!BZkBXyBX_B_!BZ�BS[BV�BW$BN�BJ�BQ�B\B\CB[WB\xB[�BX�BY�Bc:Bg�Bj0Bo�Bt9Bt�Bo�BkQBl"Bt�Bx�Bu�BpBm]BoOBq[Br�B~(B��B��B��B��B��B��B�B�B��B��B�B�nB�$BŢB�RB�"B��B��B�YB�MB��B�"B�aB�#B�tBňB��B��B�NB�B��B� B��B�UB�B�B��B��B�HB	�B��B�B�B�dB�B�XB�B�}B�B��B�!B�lB��B	�B	�B	�B	_B		�B		B	�B	 B	B	KB	!-B	'mB	1[B	5�B	4�B	3�B	5B	2GB	-�B	3�B	<�B	A�B	7�B	:DB	.�B	-�B	9XB	JrB	FB	C�B	C�B	D�B	BuB	O�B	T�B	V�B	Z�B	[�B	^B	e�B	n�B	l�B	m�B	n�B	nB	l"B	gRB	k�B	m�B	n}B	oiB	q�B	s3B	p�B	qAB	sB	{�B	�B	�-B	�B	�B	�'B	��B	��B	��B	��B	�B	�#B	�dB	�B	�vB	��B	��B	�JB	��B	�<B	�(B	�B	� B	�@B	�[B	��B	�{B	�aB	�SB	�yB	�B	�xB	�]B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�B	�?B	��B	�>B	�*B	�DB	�6B	�"B	�"B	�<B	�(B	�(B	�cB	�iB	�UB	�uB	�aB	�gB	�YB	ǔB	ȴB	ˬB	˒B	̳B	͹B	οB	бB	бB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	�/B	�!B	�'B	�-B	�4B	�:B	�@B	�ZB	�fB	�RB	�XB	�_B	�KB	�KB	�kB	�kB	�B	�]B	�IB	�cB	�cB	�B	�B	��B	�vB	�B	�B	�B	�B	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
B
	B
	B
	B
�B
	B

#B

	B

	B
)B
B
0B
B
6B
<B
<B
<B
B
BB
HB
.B
.B
TB
&B
@B
@B
@B
[B
uB
aB
FB
B
,B
,B
FB
FB
FB
aB
aB
MB
MB
MB
MB
MB
gB
�B
mB
SB
mB
YB
YB
EB
EB
EB
_B
_B
eB
eB
eB
kB
WB
qB
WB
WB
qB
�B
xB
�B
�B
~B
�B
pB
�B
�B
�B
 �B
 �B
!�B
!�B
!|B
!|B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
-�B
/ B
/B
/�B
/�B
/�B
/�B
1B
1B
0�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
4B
4B
5B
5B
4�B
5B
5�B
6B
6�B
72B
8B
8B
9$B
9>B
:DB
:DB
;0B
;0B
;JB
<6B
<6B
<6B
<6B
<6B
<B
<B
=<B
=<B
>BB
>]B
>BB
?HB
?HB
?HB
?.B
?HB
?HB
?cB
@OB
@OB
@OB
@OB
AUB
A;B
AUB
AUB
AUB
B[B
B[B
BAB
B[B
B[B
BuB
BuB
CaB
CaB
DgB
DgB
DgB
EmB
ESB
EmB
EmB
EmB
F�B
FtB
FtB
FtB
GzB
GzB
GzB
GzB
GzB
GzB
G_B
H�B
H�B
HfB
HfB
H�B
H�B
HKB
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
M�B
L�B
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
P�B
P�B
P�B
P�B
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
S�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
\�B
]B
^B
]�B
^�B
_B
^�B
_B
_B
^�B
^�B
_B
_B
^�B
`B
`�B
`�B
`�B
aB
bB
bB
aB
bB
c B
cB
cB
c B
c B
c B
d&B
d&B
d&B
d&B
eB
e,B
e,B
eB
eB
e,B
e,B
eB
e,B
e,B
fB
f2B
f2B
f2B
fB
f2B
f2B
fB
f2B
g8B
g8B
gB
g8B
g8B
g8B
g8B
g8B
g8B
gB
gRB
g8B
g8B
h>B
h>B
h>B
h>B
h>B
h>B
i_B
iDB
jKB
j0B
j0B
jKB
jKB
jKB
jB
j0B
j0B
j0B
jKB
k6B
k6B
kQB
kQB
kQB
kQB
lWB
lWB
l=B
lWB
lWB
l=B
lWB
m]B
mCB
mCB
mCB
n/B
ncB
oOB
nIB
nIB
oOB
oOB
oOB
oiB
oiB
pUB
pUB
poB
poB
poB
oiB
poB
poB
qvB
q[B
qAB
q[B
raB
raB
r|B
raB
raB
r�B
r|B
qvB
qvB
q[B
raB
raB
r|B
raB
r|B
raB
t�B
tnB
tnB
tnB
tTB
tnB
tn11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<FF<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.63(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202001020034172020010200341720200102003417202306231719492023062317194920230623171949202001030023462020010300234620200103002346  JA  ARFMdecpA19c                                                                20191228183720  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191228093721  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191228093724  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191228093724  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191228093725  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191228093725  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191228093725  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191228093725  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191228093725  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191228093725                      G�O�G�O�G�O�                JA  ARUP                                                                        20191228095316                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191228153559  CV  JULD            G�O�G�O�FǶ�                JM  ARCAJMQC2.0                                                                 20200101153417  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200101153417  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200102152346  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081949  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031507                      G�O�G�O�G�O�                