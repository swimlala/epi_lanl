CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:15:01Z creation;2022-06-04T19:15:01Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191501  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��HpB�1   @�ảEg�@/�/��w�d�+J1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B���B�33B�33B�ffB���B�  Bԙ�B���B�  B�ffB���B�33B���B���B�  B�  B�  C   C  C  C  C33C	�fC�fC�fC  C  C  C  C  C  C  C  C   C"  C$  C&�C(L�C)��C,  C.  C0  C2  C4  C6  C8  C:  C<�C>�C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ�C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D��3D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�C3Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @  @vff@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��fB��3B��3B�L�B��3B��3B��3B�� B��fB��fB��Bʀ Bϳ3B�L�B׀ Bڳ3B��B� B��fB� B� B�3B��3B��3B��3CٚCٚCٚC�C	� C� C� CٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%�4C(&gC)�gC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;�4C=�4C?ٚCA� CCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCW�4CY�4C[ٚC]� C_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu��DvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�~fD��fD��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��fD��fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˾fD��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�>fD�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�8 D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�6FA�1�A�1'A�4nA�49A�1'A�+A�*�A�)_A�(�A�)*A�)_A�)*A�(XA�)�A�(XA�#�A�
�A��HA䀝A�A�IA�[WA��A��AⱐA⍄A�P�A�3hA��A�A��A���A��pA�A��YA�_A�D3A�%FA���A�^�A�aHA��HA��A��A���Aؑ4A��A�9XA͐.A�|PA�N<A�уA��A�ϫA��"A���A�IA��aA��YA��-A��A�!A��A�9�A���A�B'A�3�A��"A��RA���A�\�A�33A��>A��A�C�A��aA�A�A��uA��bA���A��A��AA�	lA��A�aAy�Aw	Av7Au��Au1At3�Arm]AoX�Akl"AeݘAasA\MAWm]AU1�AS;�AQ�	AP^5AM��AL�MALOAJ�AE�AC�A@��A>@A=1'A<�A;�A:�A9Q�A8=�A5��A4�|A4L0A3�cA3w�A1o�A/|�A.��A-��A,�rA+�MA+4A*�cA+-A*�TA*{JA)�A)w2A)�fA*��A*�OA*cA*-A)�HA(��A'�A%g�A%OA#�'A!��A ��A�A�EAA A\)A��A�Ae�AYA�^A��A�A?�A!AuA%FAtTAtTAjAJ�A�A��A	A_A�A�A~(A:*A��A��A�A�HA�AAVmA�A
y�A
��A
�DA
�+A	��A	H�A	YA�rA)�AYKA1�Aw�A1'AA��A��A�AA�+A�A}�A($A��A��AN<A�3ARTA8�A�.Aw�AA �A ��A �@��/@��@���@���@�-w@���@�7�@���@��@��P@�S&@��@��@�1�@��~@��f@�~(@�N�@�8�@��@��3@�5�@�p;@��@���@�iD@�z@��@�<6@��@�Ft@�($@�c@��&@�D@�[@��3@���@�:*@��@��@�I@��@��@�T�@�$@�V@��@�7@�@��@ݺ^@��@܀�@��@��@��3@�%F@��@�Mj@��m@�b@�Y�@�Ɇ@��@Ӭq@�[W@�.I@���@ң�@�Xy@��@ѻ0@�x@�)_@�;@иR@�7�@϶F@Η�@�@́@�;d@��@̽<@̵@̛�@�g8@�'R@˓�@���@ʹ$@ʫ6@ʑ @�z@�^5@�K^@�	@���@�^�@�ff@�7�@�  @�zx@��@Ʋ�@�N�@�J�@��5@�S�@��@ñ[@Ã{@�(�@³h@�u%@�M�@� �@�H�@��@�_p@���@�(�@���@�&�@���@�4n@���@�1�@���@�4n@�$@���@���@��f@�rG@�o @�>�@��8@��`@���@�tT@�Ov@���@��@���@���@��@�#:@��@��^@��k@���@�.�@���@�_p@��m@�h�@��W@��t@�x@�>�@�	l@��u@�tT@�:�@���@��f@���@��!@�xl@�K^@�b@��@��	@��M@�Z@�@�C�@��@�W�@��@���@�B�@�"�@��e@�=q@��[@�x@�@O@�@���@�h
@�Ft@��@�p�@�T�@��@�YK@�2�@���@�[W@�ں@���@�y>@�{@��[@�~�@�IR@� i@���@�l�@�YK@�_@��@���@�s@�4@���@��}@���@�W�@���@���@���@��@�6@��@���@�*0@��@��$@�|�@�D�@��@��[@�O@�+@��@�1'@�خ@���@�&�@��@�Ĝ@��b@�j@�B[@��@���@���@�y�@�F�@�2a@��2@��@�l"@�@�@��D@��[@���@��@�l�@�7@��W@���@���@�~�@�4@�+@���@��'@���@���@��_@�z�@�9X@��:@�A @���@�c @�0U@��o@��@�	l@���@�I�@��Z@���@�!�@���@���@�M@��@���@���@�y�@�y�@�y�@�j�@�/�@���@���@�oi@�Q@�b@�˒@��C@��M@�o�@�O�@��@��@���@�~�@�n�@�5?@��K@�}�@�O@��@��@��?@���@�*�@��]@��Q@��q@�e�@�F@���@��?@���@���@���@�i�@�($@��@���@�.I@��|@��\@��+@��O@�c @�.�@˒@~8�@}�@~ �@}��@|��@|�j@|��@{�@zZ�@yrG@y�@x�`@x�e@xbN@w˒@wW?@wRT@vZ�@u/@t�@s@s$t@r� @q7L@p��@o�@o;d@n��@nM�@n�@m�@m�@nO@n_�@n�,@n�H@n�s@n�@nd�@n;�@mj@lѷ@l��@l@kdZ@ko@k+@k�K@k˒@k�F@k��@k��@k��@kU�@k�@jxl@jV@jJ�@jE�@jO@i��@i�@i�~@i5�@h��@i;@hĜ@h�@h<�@g�@g��@gn/@ge�@g\)@gX�@gS�@g�@f{�@f0U@fJ@e�Z@e��@ea�@e�@d�@d�@d2�@c�]@c�g@cW?@b�@b��@b)�@a��@aX@a!�@`�z@`Ft@_�@_\)@_�@^�<@^��@^}V@^s�@^8�@]�9@]�~@]c@]e,@\�K@\��@\Ft@[��@[j�@[@Zں@Zl�@Y��@YT�@Y�@X��@X�@W��@WP�@Vȴ@V�@V.�@Us�@U%F@T��@T��@S�@S|�@SU�@S�@R�@Rv�@Q�9@QQ�@P��@P��@PA�@O�@O@N��@Na|@N)�@M@M8�@L�@L"h@K��@Kj�@J�2@JGE@I�-@I�@H��@HH@G��@G��@G��@G~�@G_p@G!-@F�m@F�x@F;�@E�@E�'@Ezx@EVm@E-w@E \@E;@D�@D��@DU2@C��@C�P@CU�@C6z@C!-@B��@B�X@Bu@A�9@A�@A��@A:�@@��@@�[@@��@@��@@`�@@	�@?�K@?P�@>��@>� @>Ov@=�D@=��@=IR@<ѷ@<�z@<C-@<"h@;�6@;t�@;1�@;�@:��@:�@:d�@:J@9ϫ@9s�@9Y�@9J�@97L@9�@8�o@8�@7��@7n/@71�@6�@6��@6��@6^5@6)�@5�@5�d@5�"@52a@5�@4�|@4�@46@3��@3˒@3dZ@3Mj@3Y@2�H@2҉@2�@2Q@1��@1<6@0�P@0��@0��@0r�@0]d@0�@/�V@/��@/b�@.��@.�r@.c @.1�@.($@.e@-�@-��@-\�@-%F@,��@,�)@,`�@+ƨ@+�:@*��@*��@*�@)hs@)(�@(�@(��@(C-@(�@'�a@'o�@&�X@&i�@&�@%�S@%@@$z�@$	�@#�$@#F�@"��@"�@"a|@"!�@!�n@!rG@!J�@ ��@ @�w@RT@�y@�F@��@~�@{�@_�@($@��@�-@�~@a�@Q�@=�@Ɇ@[�@7�@ �@��@��@��@Ta@:*@��@��@hs@��@�E@��@��@w�@Z@�@˒@��@��@l�@\)@Y@�8@�H@�b@�A@V@B[@�)@�~@e,@0�@��@��@��@�I@�@�@�@�Y@Q�@G@��@��@b�@E9@@O@8@C@��@��@�x@��@{�@h
@V@GE@+k@	@�@�#@��@m]@(�@�v@�p@�U@�@e�@Ft@@�+@�@�W@خ@��@��@A�@8@.I@�M@��@l�@L0@1�@O@�Z@��@�N@��@|@A @#�@�@��@�?@��@�@~(@!@�@�@�A@��@ݘ@�g@˒@��@��@�	@33@
��@
��@
��@
�6@
�+@
ff@
E�@	��@	�N@	@	�h11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�6FA�1�A�1'A�4nA�49A�1'A�+A�*�A�)_A�(�A�)*A�)_A�)*A�(XA�)�A�(XA�#�A�
�A��HA䀝A�A�IA�[WA��A��AⱐA⍄A�P�A�3hA��A�A��A���A��pA�A��YA�_A�D3A�%FA���A�^�A�aHA��HA��A��A���Aؑ4A��A�9XA͐.A�|PA�N<A�уA��A�ϫA��"A���A�IA��aA��YA��-A��A�!A��A�9�A���A�B'A�3�A��"A��RA���A�\�A�33A��>A��A�C�A��aA�A�A��uA��bA���A��A��AA�	lA��A�aAy�Aw	Av7Au��Au1At3�Arm]AoX�Akl"AeݘAasA\MAWm]AU1�AS;�AQ�	AP^5AM��AL�MALOAJ�AE�AC�A@��A>@A=1'A<�A;�A:�A9Q�A8=�A5��A4�|A4L0A3�cA3w�A1o�A/|�A.��A-��A,�rA+�MA+4A*�cA+-A*�TA*{JA)�A)w2A)�fA*��A*�OA*cA*-A)�HA(��A'�A%g�A%OA#�'A!��A ��A�A�EAA A\)A��A�Ae�AYA�^A��A�A?�A!AuA%FAtTAtTAjAJ�A�A��A	A_A�A�A~(A:*A��A��A�A�HA�AAVmA�A
y�A
��A
�DA
�+A	��A	H�A	YA�rA)�AYKA1�Aw�A1'AA��A��A�AA�+A�A}�A($A��A��AN<A�3ARTA8�A�.Aw�AA �A ��A �@��/@��@���@���@�-w@���@�7�@���@��@��P@�S&@��@��@�1�@��~@��f@�~(@�N�@�8�@��@��3@�5�@�p;@��@���@�iD@�z@��@�<6@��@�Ft@�($@�c@��&@�D@�[@��3@���@�:*@��@��@�I@��@��@�T�@�$@�V@��@�7@�@��@ݺ^@��@܀�@��@��@��3@�%F@��@�Mj@��m@�b@�Y�@�Ɇ@��@Ӭq@�[W@�.I@���@ң�@�Xy@��@ѻ0@�x@�)_@�;@иR@�7�@϶F@Η�@�@́@�;d@��@̽<@̵@̛�@�g8@�'R@˓�@���@ʹ$@ʫ6@ʑ @�z@�^5@�K^@�	@���@�^�@�ff@�7�@�  @�zx@��@Ʋ�@�N�@�J�@��5@�S�@��@ñ[@Ã{@�(�@³h@�u%@�M�@� �@�H�@��@�_p@���@�(�@���@�&�@���@�4n@���@�1�@���@�4n@�$@���@���@��f@�rG@�o @�>�@��8@��`@���@�tT@�Ov@���@��@���@���@��@�#:@��@��^@��k@���@�.�@���@�_p@��m@�h�@��W@��t@�x@�>�@�	l@��u@�tT@�:�@���@��f@���@��!@�xl@�K^@�b@��@��	@��M@�Z@�@�C�@��@�W�@��@���@�B�@�"�@��e@�=q@��[@�x@�@O@�@���@�h
@�Ft@��@�p�@�T�@��@�YK@�2�@���@�[W@�ں@���@�y>@�{@��[@�~�@�IR@� i@���@�l�@�YK@�_@��@���@�s@�4@���@��}@���@�W�@���@���@���@��@�6@��@���@�*0@��@��$@�|�@�D�@��@��[@�O@�+@��@�1'@�خ@���@�&�@��@�Ĝ@��b@�j@�B[@��@���@���@�y�@�F�@�2a@��2@��@�l"@�@�@��D@��[@���@��@�l�@�7@��W@���@���@�~�@�4@�+@���@��'@���@���@��_@�z�@�9X@��:@�A @���@�c @�0U@��o@��@�	l@���@�I�@��Z@���@�!�@���@���@�M@��@���@���@�y�@�y�@�y�@�j�@�/�@���@���@�oi@�Q@�b@�˒@��C@��M@�o�@�O�@��@��@���@�~�@�n�@�5?@��K@�}�@�O@��@��@��?@���@�*�@��]@��Q@��q@�e�@�F@���@��?@���@���@���@�i�@�($@��@���@�.I@��|@��\@��+@��O@�c @�.�@˒@~8�@}�@~ �@}��@|��@|�j@|��@{�@zZ�@yrG@y�@x�`@x�e@xbN@w˒@wW?@wRT@vZ�@u/@t�@s@s$t@r� @q7L@p��@o�@o;d@n��@nM�@n�@m�@m�@nO@n_�@n�,@n�H@n�s@n�@nd�@n;�@mj@lѷ@l��@l@kdZ@ko@k+@k�K@k˒@k�F@k��@k��@k��@kU�@k�@jxl@jV@jJ�@jE�@jO@i��@i�@i�~@i5�@h��@i;@hĜ@h�@h<�@g�@g��@gn/@ge�@g\)@gX�@gS�@g�@f{�@f0U@fJ@e�Z@e��@ea�@e�@d�@d�@d2�@c�]@c�g@cW?@b�@b��@b)�@a��@aX@a!�@`�z@`Ft@_�@_\)@_�@^�<@^��@^}V@^s�@^8�@]�9@]�~@]c@]e,@\�K@\��@\Ft@[��@[j�@[@Zں@Zl�@Y��@YT�@Y�@X��@X�@W��@WP�@Vȴ@V�@V.�@Us�@U%F@T��@T��@S�@S|�@SU�@S�@R�@Rv�@Q�9@QQ�@P��@P��@PA�@O�@O@N��@Na|@N)�@M@M8�@L�@L"h@K��@Kj�@J�2@JGE@I�-@I�@H��@HH@G��@G��@G��@G~�@G_p@G!-@F�m@F�x@F;�@E�@E�'@Ezx@EVm@E-w@E \@E;@D�@D��@DU2@C��@C�P@CU�@C6z@C!-@B��@B�X@Bu@A�9@A�@A��@A:�@@��@@�[@@��@@��@@`�@@	�@?�K@?P�@>��@>� @>Ov@=�D@=��@=IR@<ѷ@<�z@<C-@<"h@;�6@;t�@;1�@;�@:��@:�@:d�@:J@9ϫ@9s�@9Y�@9J�@97L@9�@8�o@8�@7��@7n/@71�@6�@6��@6��@6^5@6)�@5�@5�d@5�"@52a@5�@4�|@4�@46@3��@3˒@3dZ@3Mj@3Y@2�H@2҉@2�@2Q@1��@1<6@0�P@0��@0��@0r�@0]d@0�@/�V@/��@/b�@.��@.�r@.c @.1�@.($@.e@-�@-��@-\�@-%F@,��@,�)@,`�@+ƨ@+�:@*��@*��@*�@)hs@)(�@(�@(��@(C-@(�@'�a@'o�@&�X@&i�@&�@%�S@%@@$z�@$	�@#�$@#F�@"��@"�@"a|@"!�@!�n@!rG@!J�@ ��@ @�w@RT@�y@�F@��@~�@{�@_�@($@��@�-@�~@a�@Q�@=�@Ɇ@[�@7�@ �@��@��@��@Ta@:*@��@��@hs@��@�E@��@��@w�@Z@�@˒@��@��@l�@\)@Y@�8@�H@�b@�A@V@B[@�)@�~@e,@0�@��@��@��@�I@�@�@�@�Y@Q�@G@��@��@b�@E9@@O@8@C@��@��@�x@��@{�@h
@V@GE@+k@	@�@�#@��@m]@(�@�v@�p@�U@�@e�@Ft@@�+@�@�W@خ@��@��@A�@8@.I@�M@��@l�@L0@1�@O@�Z@��@�N@��@|@A @#�@�@��@�?@��@�@~(@!@�@�@�A@��@ݘ@�g@˒@��@��@�	@33@
��@
��@
��@
�6@
�+@
ff@
E�@	��@	�N@	@	�h11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�B	B	B	B	&B	&B	�B	�B	�B	�B	B	B	@B	�B	B	aB	�B	�B	0�B	Q B	poB	}B	�B	��B	�+B	��B	�B	z�B	y�B	{�B	��B	��B	��B	�$B	��B	�sB	�B	��B	�XB	��B	��B	��B	�^B	��B	v�B	n�B	b�B	OB	K�B	K)B	G�B	K�B	QhB	W�B	z�B
6+B
;�B
R:B
q'B
mB
|�B
cB
�Bg�Bh�B_pB^�BTFB^BN<BF?B6B'8B"�B2B
��B
ݘB
�cB
�B
��B
��B
kB
J	B
!-B
	B	ؓB	��B	��B	�1B	��B	�@B	��B	��B	v�B	c:B	I�B	2�B	�B	oB	jB	DB	�B	+B	B��B��B��B�<B�VB�dB	GB	\B	$�B	.�B	G�B	QB	[�B	e�B	h>B	iB	h�B	e�B	aB	g�B	o�B	m�B	nB	x�B	�3B	�&B	�TB	�RB	��B	�|B	��B	��B	�B	�QB	��B	�B	��B	��B	�B	�"B	��B	�gB	�B	��B	�}B	��B	��B	�FB	�=B	�'B	�FB	��B	�RB	�iB	�oB	|�B	_�B	q�B	w�B	~]B	�fB	��B	�tB	��B	��B	�PB	�#B	�RB	��B	��B	�}B	�
B	��B	�5B	�B	�hB	��B	�NB	�B	��B	�B	�B	�nB	��B	��B	�B	��B	�B	�`B	�aB	�aB	��B	�B	�HB	��B	�YB	�%B	ŢB	ƎB	��B	�JB	��B	�6B	�dB	�B	��B	�B	�:B	�hB	�\B	�B	ˬB	�)B	�xB	��B	��B	̈́B	��B	�6B	��B	�lB	ȴB	�B	�_B	��B	��B	ǔB	��B	�B	�dB	�B	�VB	�(B	ϑB	οB	�pB	ϑB	�HB	��B	�VB	̈́B	�(B	҉B	ҽB	οB	�B	�xB	ΥB	�B	�rB	˒B	��B	�B	��B	�	B	�B	�6B	��B	�xB	ʌB	�^B	��B	�dB	ˬB	�B	��B	�B	οB	�vB	бB	бB	�hB	� B	��B	ЗB	ѷB	��B	��B	��B	�[B	��B	��B	ԯB	�B	ՁB	յB	ՁB	�B	�B	�2B	�SB	ևB	�?B	�?B	׍B	�+B	�B	�yB	�1B	�7B	�WB	��B	یB	ۦB	یB	��B	��B	ۦB	�)B	ܒB	��B	�;B	��B	��B	�dB	�~B	�B	�pB	�-B	�B	�B	��B	�B	��B	��B	�zB	��B	�LB	�fB	�8B	�$B	��B	�yB	�B	�kB	��B	��B	��B	�B	�WB	�)B	��B	��B	�/B	�}B	� B	�B	��B	��B	�vB	�'B	�B	��B	�GB	�3B	�B	�hB	�B	�9B	��B	�TB	�9B	��B	��B	�ZB	�tB	�FB	�+B	�B	��B	��B	��B	��B	�2B	�fB	�fB	��B	��B	�$B	��B	��B	�XB	��B	��B	�*B	�xB	�0B	��B	�xB	��B	��B	��B	��B	�B	�(B	�B	�BB	�wB	�wB	��B	��B	�B
  B
  B	��B
 OB
 �B
 �B
 �B
 �B
 �B
UB
�B
�B
�B
�B
�B
aB
�B
�B
B
gB
gB
�B
�B
�B
�B
B
9B
�B
B
�B
�B
fB
B
1B
fB
	�B

�B

�B
�B
0B
0B
JB
�B
�B
6B
jB
jB
�B
�B
B
B
.B
}B
�B
�B
�B
 B
hB
�B
�B
B
 B
 B
�B
�B
@B
@B
�B
�B
�B
�B
�B
�B
�B
2B
B
�B
SB
�B
�B
$B
?B
$B
$B
$B
YB
_B
�B
�B
�B
�B
B
�B
B
xB
�B
�B
IB
�B
�B
jB
;B
pB
�B
�B
�B
�B
�B
�B
 \B
 �B
!-B
!�B
!�B
"B
"�B
"�B
#B
#B
"�B
"4B
"�B
# B
#TB
#nB
#�B
$@B
$tB
$�B
$tB
$�B
%`B
&LB
'�B
'�B
'�B
'�B
($B
(�B
(�B
)B
)*B
)_B
)DB
)_B
)�B
*KB
)�B
(�B
(sB
(XB
(�B
+�B
,qB
,=B
,"B
+6B
+kB
-wB
-�B
.IB
.cB
.�B
-�B
-B
-wB
-]B
.IB
/�B
0�B
0�B
1[B
2�B
2|B
1�B
/�B
.�B
/�B
/�B
-�B
,�B
,=B
,=B
,�B
,qB
,�B
-B
./B
/�B
1AB
3�B
4nB
4�B
5%B
5?B
5ZB
5�B
5�B
5tB
4�B
4�B
4�B
5%B
6�B
7�B
7�B
7�B
7�B
7�B
7�B
8�B
9rB
9�B
9�B
9�B
9�B
:B
:B
9�B
:*B
:^B
;JB
;JB
;JB
:�B
;dB
;�B
<B
<B
;�B
;�B
;�B
<B
<�B
<�B
=B
<�B
=B
=<B
="B
=VB
=�B
>(B
>BB
>BB
>]B
>]B
>�B
?}B
@ B
@ B
@ B
@�B
A B
A�B
A�B
BB
B[B
B'B
BB
BB
BB
B[B
B�B
C-B
CaB
C�B
C{B
C�B
C�B
D3B
DgB
D�B
D�B
D�B
EB
E9B
EmB
E�B
FB
F%B
F?B
FYB
F�B
GB
G�B
HB
HKB
H�B
IB
I�B
I�B
I�B
JrB
K)B
K�B
LdB
LdB
L�B
L�B
L�B
MB
MjB
MB
LJB
LdB
L�B
MB
M6B
M�B
M�B
N"B
NpB
N�B
O(B
OvB
O�B
O�B
O�B
O�B
O�B
PB
PbB
PbB
P�B
Q B
QB
Q4B
QNB
QNB
QB
QNB
QhB
Q�B
Q�B
R B
RoB
R�B
R�B
R�B
R�B
S[B
S�B
S�B
S�B
TB
T{B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UB
U2B
UgB
U�B
VB
VB
V�B
V�B
W
B
WsB
WsB
W�B
XB
XEB
X_B
X�B
X�B
X�B
Y1B
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
[	B
[qB
[�B
[�B
[�B
[�B
\B
\CB
\xB
\]B
\�B
\�B
\�B
\�B
]/B
]~B
]~B
]dB
]�B
]�B
^B
^5B
^5B
^jB
^�B
_B
_pB
_�B
_�B
_�B
_�B
_�B
`'B
`\B
`BB
`\B
aHB
abB
a|B
a�B
a�B
a�B
a�B
bB
a�B
bNB
bNB
b4B
b�B
c B
c B
c�B
c�B
d@B
d�B
d�B
eB
e`B
e�B
e�B
e�B
e�B
fLB
f�B
f�B
gB
gmB
g�B
h$B
h�B
h�B
h�B
iDB
i�B
i�B
j�B
j�B
j�B
kB
kQB
k�B
lB
lB
lqB
lWB
lWB
lWB
lqB
l�B
l�B
l�B
l�B
mB
mB
l�B
m�B
nB
n/B
n/B
ncB
n}B
o�B
o�B
o�B
o�B
pB
pUB
p�B
p�B
p�B
p�B
p�B
qB
qAB
q�B
q�B
q�B
q�B
q�B
r-B
r-B
r-B
r|B
r�B
r�B
r�B
sB
shB
shB
s�B
s�B
s�B
tB
tB
t9B
tB
tB
t9B
tTB
t�B
t�B
uB
u?B
utB
uZB
uZB
u�B
u�B
u�B
u�B
u�B
v+B
vFB
vFB
vFB
v`B
vzB
v�B
w2B
wLB
w�B
w�B
xB
xB
xB
x8B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y$B
y�B
y�B
y�B
y�B
zDB
z�B
z�B
z�B
z�B
z�B
{B
{B
{0B
{B
{�B
{�B
{�B
|B
|6B
|6B
|B
|�B
|�B
|�B
|�B
|�B
}B
}B
}B
}"B
}"B
}B
}<B
}�B
~B
}�B
~B
~B
~BB
~BB
~wB
~�B
~�B
~�B
c11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�B	B	B	B	&B	&B	�B	�B	�B	�B	B	B	@B	�B	B	aB	�B	�B	0�B	Q B	poB	}B	�B	��B	�+B	��B	�B	z�B	y�B	{�B	��B	��B	��B	�$B	��B	�sB	�B	��B	�XB	��B	��B	��B	�^B	��B	v�B	n�B	b�B	OB	K�B	K)B	G�B	K�B	QhB	W�B	z�B
6+B
;�B
R:B
q'B
mB
|�B
cB
�Bg�Bh�B_pB^�BTFB^BN<BF?B6B'8B"�B2B
��B
ݘB
�cB
�B
��B
��B
kB
J	B
!-B
	B	ؓB	��B	��B	�1B	��B	�@B	��B	��B	v�B	c:B	I�B	2�B	�B	oB	jB	DB	�B	+B	B��B��B��B�<B�VB�dB	GB	\B	$�B	.�B	G�B	QB	[�B	e�B	h>B	iB	h�B	e�B	aB	g�B	o�B	m�B	nB	x�B	�3B	�&B	�TB	�RB	��B	�|B	��B	��B	�B	�QB	��B	�B	��B	��B	�B	�"B	��B	�gB	�B	��B	�}B	��B	��B	�FB	�=B	�'B	�FB	��B	�RB	�iB	�oB	|�B	_�B	q�B	w�B	~]B	�fB	��B	�tB	��B	��B	�PB	�#B	�RB	��B	��B	�}B	�
B	��B	�5B	�B	�hB	��B	�NB	�B	��B	�B	�B	�nB	��B	��B	�B	��B	�B	�`B	�aB	�aB	��B	�B	�HB	��B	�YB	�%B	ŢB	ƎB	��B	�JB	��B	�6B	�dB	�B	��B	�B	�:B	�hB	�\B	�B	ˬB	�)B	�xB	��B	��B	̈́B	��B	�6B	��B	�lB	ȴB	�B	�_B	��B	��B	ǔB	��B	�B	�dB	�B	�VB	�(B	ϑB	οB	�pB	ϑB	�HB	��B	�VB	̈́B	�(B	҉B	ҽB	οB	�B	�xB	ΥB	�B	�rB	˒B	��B	�B	��B	�	B	�B	�6B	��B	�xB	ʌB	�^B	��B	�dB	ˬB	�B	��B	�B	οB	�vB	бB	бB	�hB	� B	��B	ЗB	ѷB	��B	��B	��B	�[B	��B	��B	ԯB	�B	ՁB	յB	ՁB	�B	�B	�2B	�SB	ևB	�?B	�?B	׍B	�+B	�B	�yB	�1B	�7B	�WB	��B	یB	ۦB	یB	��B	��B	ۦB	�)B	ܒB	��B	�;B	��B	��B	�dB	�~B	�B	�pB	�-B	�B	�B	��B	�B	��B	��B	�zB	��B	�LB	�fB	�8B	�$B	��B	�yB	�B	�kB	��B	��B	��B	�B	�WB	�)B	��B	��B	�/B	�}B	� B	�B	��B	��B	�vB	�'B	�B	��B	�GB	�3B	�B	�hB	�B	�9B	��B	�TB	�9B	��B	��B	�ZB	�tB	�FB	�+B	�B	��B	��B	��B	��B	�2B	�fB	�fB	��B	��B	�$B	��B	��B	�XB	��B	��B	�*B	�xB	�0B	��B	�xB	��B	��B	��B	��B	�B	�(B	�B	�BB	�wB	�wB	��B	��B	�B
  B
  B	��B
 OB
 �B
 �B
 �B
 �B
 �B
UB
�B
�B
�B
�B
�B
aB
�B
�B
B
gB
gB
�B
�B
�B
�B
B
9B
�B
B
�B
�B
fB
B
1B
fB
	�B

�B

�B
�B
0B
0B
JB
�B
�B
6B
jB
jB
�B
�B
B
B
.B
}B
�B
�B
�B
 B
hB
�B
�B
B
 B
 B
�B
�B
@B
@B
�B
�B
�B
�B
�B
�B
�B
2B
B
�B
SB
�B
�B
$B
?B
$B
$B
$B
YB
_B
�B
�B
�B
�B
B
�B
B
xB
�B
�B
IB
�B
�B
jB
;B
pB
�B
�B
�B
�B
�B
�B
 \B
 �B
!-B
!�B
!�B
"B
"�B
"�B
#B
#B
"�B
"4B
"�B
# B
#TB
#nB
#�B
$@B
$tB
$�B
$tB
$�B
%`B
&LB
'�B
'�B
'�B
'�B
($B
(�B
(�B
)B
)*B
)_B
)DB
)_B
)�B
*KB
)�B
(�B
(sB
(XB
(�B
+�B
,qB
,=B
,"B
+6B
+kB
-wB
-�B
.IB
.cB
.�B
-�B
-B
-wB
-]B
.IB
/�B
0�B
0�B
1[B
2�B
2|B
1�B
/�B
.�B
/�B
/�B
-�B
,�B
,=B
,=B
,�B
,qB
,�B
-B
./B
/�B
1AB
3�B
4nB
4�B
5%B
5?B
5ZB
5�B
5�B
5tB
4�B
4�B
4�B
5%B
6�B
7�B
7�B
7�B
7�B
7�B
7�B
8�B
9rB
9�B
9�B
9�B
9�B
:B
:B
9�B
:*B
:^B
;JB
;JB
;JB
:�B
;dB
;�B
<B
<B
;�B
;�B
;�B
<B
<�B
<�B
=B
<�B
=B
=<B
="B
=VB
=�B
>(B
>BB
>BB
>]B
>]B
>�B
?}B
@ B
@ B
@ B
@�B
A B
A�B
A�B
BB
B[B
B'B
BB
BB
BB
B[B
B�B
C-B
CaB
C�B
C{B
C�B
C�B
D3B
DgB
D�B
D�B
D�B
EB
E9B
EmB
E�B
FB
F%B
F?B
FYB
F�B
GB
G�B
HB
HKB
H�B
IB
I�B
I�B
I�B
JrB
K)B
K�B
LdB
LdB
L�B
L�B
L�B
MB
MjB
MB
LJB
LdB
L�B
MB
M6B
M�B
M�B
N"B
NpB
N�B
O(B
OvB
O�B
O�B
O�B
O�B
O�B
PB
PbB
PbB
P�B
Q B
QB
Q4B
QNB
QNB
QB
QNB
QhB
Q�B
Q�B
R B
RoB
R�B
R�B
R�B
R�B
S[B
S�B
S�B
S�B
TB
T{B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UB
U2B
UgB
U�B
VB
VB
V�B
V�B
W
B
WsB
WsB
W�B
XB
XEB
X_B
X�B
X�B
X�B
Y1B
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
[	B
[qB
[�B
[�B
[�B
[�B
\B
\CB
\xB
\]B
\�B
\�B
\�B
\�B
]/B
]~B
]~B
]dB
]�B
]�B
^B
^5B
^5B
^jB
^�B
_B
_pB
_�B
_�B
_�B
_�B
_�B
`'B
`\B
`BB
`\B
aHB
abB
a|B
a�B
a�B
a�B
a�B
bB
a�B
bNB
bNB
b4B
b�B
c B
c B
c�B
c�B
d@B
d�B
d�B
eB
e`B
e�B
e�B
e�B
e�B
fLB
f�B
f�B
gB
gmB
g�B
h$B
h�B
h�B
h�B
iDB
i�B
i�B
j�B
j�B
j�B
kB
kQB
k�B
lB
lB
lqB
lWB
lWB
lWB
lqB
l�B
l�B
l�B
l�B
mB
mB
l�B
m�B
nB
n/B
n/B
ncB
n}B
o�B
o�B
o�B
o�B
pB
pUB
p�B
p�B
p�B
p�B
p�B
qB
qAB
q�B
q�B
q�B
q�B
q�B
r-B
r-B
r-B
r|B
r�B
r�B
r�B
sB
shB
shB
s�B
s�B
s�B
tB
tB
t9B
tB
tB
t9B
tTB
t�B
t�B
uB
u?B
utB
uZB
uZB
u�B
u�B
u�B
u�B
u�B
v+B
vFB
vFB
vFB
v`B
vzB
v�B
w2B
wLB
w�B
w�B
xB
xB
xB
x8B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y$B
y�B
y�B
y�B
y�B
zDB
z�B
z�B
z�B
z�B
z�B
{B
{B
{0B
{B
{�B
{�B
{�B
|B
|6B
|6B
|B
|�B
|�B
|�B
|�B
|�B
}B
}B
}B
}"B
}"B
}B
}<B
}�B
~B
}�B
~B
~B
~BB
~BB
~wB
~�B
~�B
~�B
c11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105231  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191501  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191501  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191501                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041509  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041509  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                