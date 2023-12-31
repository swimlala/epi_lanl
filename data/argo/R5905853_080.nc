CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:37:04Z creation;2022-06-04T17:37:04Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604173704  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               PA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�mXSo�1   @�mX�ʆB@/և+J�c��-V1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C��C  C  C  C  C  C   C"  C$  C&  C(L�C*  C+�3C-��C0  C2  C4  C6  C8  C:  C<33C>  C?�fCA�fCD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch33Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-�fD.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @	��@vff@�33@�33A��A=��A]��A}��A���A���A���A���A���A���AA���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffB  B�� B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚC�4C�gCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC(&gC)ٚC+��C-�gC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC<�C=ٚC?� CA� CCٚCE� CGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCh�CiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C�� C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-|�D-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��OA�� A�ĜA��tA��A���A��'A��dA��KA�ȴA���A��HA���A���A�҉A�҉A���A�҉A�ҽA��TA��NA��,A�ƨAǬ�Aǭ�Aǡ�AǛ	AǙ�AǛ=AǜAǜ�AǛ=AǙeAǚ�AǜAǜxAǚ7Aǖ�Aǒ:AǑ4Aǐ.AǊrAǃ�A�pA�m�A�n�A�r|A�sMA�pA�pA�gA�C�A�4�A�+�A��A�:A��A��rA��BAƿAƵ�AƩ�Aƛ	A�sA�1�A���A��A���A���A�.�A�A���A���A�ԕAĚA�.�A�A���A�{JA�Z�A�h�A���A�}�A�/OA�� A�?�A�NA���A���A�^jA��CA���A�R�A�sMA��9A�AA��^A���A�B'A��A���A�M6A���A��eA��GA�H�A�4A���A���A��9A��-A��?A��-A��cA���A�8�A�v�A���A�A���A��A�-�A���A�-wAU2Az�4Av/�AsW?AqxAn��AkU�Af��Acm]Ab��AaXyA_E9A]�hAZ�ATl"AM#�AJ7AH?AF�AE�_AD��AD9�AC:�AA�A?4A=�'A<�A;PHA9-A8_A8�gA8>�A7�'A8�*A8��A8e�A6��A5)_A4�qA4rGA4l"A2_A/\�A(0UA&cA$��A$b�A$OvA$rGA#�A#�A!�A �rA!-�A"8A"JA!�OA!�9A!S&A I�A��A��A�A��AqvA �A�rA �$A"��A#RTA"�TA"�~A" �A!�}A!��A!A qvA�yA$�A��AA�9A�2A�hAA�A�OA�"AB[A	�A	A �A�AJ�A.IA�KA�A��A9�A�TA��A�A�A^�A8Ax�AY�A�A�HA>�A�DA��AVA�jA�A�AJA
�VA
H�A	��A	w�A	YA��A�A	�A	:�A�rA	"hA	tTA	h�A�A��A��As�A(�A�WA�AiDA \A��A�qAsA>BAeA
=A��A��A_A:�AF�A�A:�A��A	A�Ar�Ae�A�hA9XAA��Ao�A �t@���@�n�@�;�@��j@�s�@�!�@�z�@��^@���@��X@��@��@��K@�(�@�S�@�!@��F@��@��t@���@�oi@�[W@�D�@�j@�7�@��@�{J@���@�bN@��T@��@@�!�@�GE@�-@�y>@�@�$t@���@��@��Z@�n@�|�@�,�@�<�@�j�@��/@�c @�ԕ@��p@���@��E@��@��@��a@�8@�|�@߳�@��@ށo@��@���@�
�@���@ݥ@���@ܻ�@�B[@�p�@��2@�	@���@�s@�~�@���@� \@�.�@��@ӥ@�+@�u�@�C-@���@�Dg@ѡ�@��+@�4n@й�@ϱ[@���@ύP@ϓ�@�"�@���@ͼ�@���@���@��@˩�@��@���@ȩ�@Ǿw@��@ƅ�@��@�N<@Ė�@�8�@��D@���@�V@�PH@�.�@�|�@ÖS@�/@�|�@��F@���@��7@�J�@�ߤ@���@�!�@���@�iD@�L�@�)_@��@���@�\�@���@�K�@�&@��]@��+@�@���@���@�y�@�?}@��|@���@�u%@��w@���@��A@���@�hs@�q@�S�@�{@��@��@���@��@���@�*�@���@���@��~@�X@�<6@�&@��@��	@��@�g8@�0U@���@�
=@���@��@��@��@���@�u@���@�?}@��@�ں@�j@��@��@���@�N<@��"@���@�a|@��Z@�W?@��@���@�Ta@��6@��@�&�@��#@�hs@��I@�@�m�@���@���@�Dg@��@���@�?@�u�@�j�@�8�@��@��I@�u%@�E�@��>@�v`@��@��@��+@�r�@�@�rG@��@���@��6@���@�@@��?@���@�^5@�@��@��@���@�?}@�+@��@���@�+k@�خ@���@�=�@���@���@�{�@�$�@��Q@���@�&�@��f@���@�%�@�@���@�/@��8@��,@�q�@��A@���@�)_@���@�!@��>@��@@�e,@��@�Ɇ@��$@���@���@�B[@�O@�@��Q@�F�@��@���@��D@��@�u@�ϫ@�qv@�C@���@���@�l"@�:�@��@�ϫ@�`B@��@��,@���@���@��_@���@�M�@��T@�p�@�9�@�%F@�	l@���@�z@�$�@���@��"@�;d@�ی@��O@��u@�u�@�9X@���@�Vm@��@���@�a|@���@��V@�^�@� \@��@��[@�tT@�1'@��@���@�4@��_@��Y@��A@�d�@�/�@��+@���@�RT@��s@���@�z�@�l�@�(�@�K@ i@~�s@~��@~�h@~��@~�6@~Ov@}��@|��@|-�@{��@{�@{�@{O@z�<@yϫ@yN<@x��@x$@wE9@vn�@u�3@t��@t/�@s\)@r��@r;�@r@�@r�@q�@qk�@qk�@qs�@qc�@q&�@pu�@p  @o�r@ox@n�c@nc @m�n@mV@l�@l��@l�_@lC-@l6@l�@k�@k��@k�$@k�	@k��@k��@kg�@k��@kx@kY@jd�@jO@i�j@i��@i7L@hĜ@h��@h  @g��@g��@gW?@f��@fH�@e��@ej@d��@c��@b�s@bv�@a�.@a�@a��@au�@`��@`�_@`y>@`D�@`1@_�+@_�f@_33@_�@^�@^M�@^e@]s�@]f�@\ѷ@\I�@[��@Z�8@Z&�@Yϫ@Y��@Y[W@Y0�@X��@X�9@X��@XI�@X�@W��@W�@W�f@V��@V$�@U��@Uk�@U;@T�e@Tl"@T7�@TM@T  @S��@S~�@S.I@RQ@Q��@P�I@P?�@PM@O�K@OS�@N�c@N�!@Na|@M��@Mq@L�e@K��@K��@K$t@J��@JQ@I�#@I��@I\�@I2a@I+@I%@H�@Hz�@G��@G�K@G��@G�*@G��@Go�@G33@F�y@F�<@F\�@E��@EF@D�/@Dg8@D�@C��@B�y@B��@B}V@B+k@Aԕ@A�'@@��@@��@@�@@w�@@m�@@_@@6@?��@?�F@?U�@?,�@?$t@?o@>�X@>�@=��@=|@=k�@=e,@=[W@=IR@=@<�E@<�[@<�$@<h�@< �@;ݘ@;��@;��@;��@;|�@;RT@;o@:�,@:�@:B[@: �@9�T@9�@97L@8��@8S�@7��@7iD@7.I@7 i@6�,@6��@6=q@5�@5�C@5J�@4�	@4��@4Z@4'R@3��@3��@3&@2�8@2͟@2��@2��@2Q@2+k@2	@2�@2
�@1��@1��@1c@1-w@0��@0�I@/ݘ@//�@.��@.�h@.Ta@-�d@-�C@-��@-�M@-w2@-5�@,�O@,$@+�+@+ݘ@+�
@+��@+~�@+S�@+33@+ i@*�@*ȴ@*��@*s�@*{@)w2@(�@(��@(`�@(/�@(�@'�]@'ƨ@'S�@'�@&�@&��@&W�@&	@%��@%�n@%�M@%G�@$�f@$��@$m�@$*�@#�@#O@"��@"�x@"d�@"B[@")�@!�D@!�"@!IR@!�@ �@ �@ w�@ I�@ 7�@ !@�P@�@�@��@J@�H@c�@V@Ɇ@�@h�@6@�@�@�@��@6z@�"@��@��@��@xl@YK@+k@J@�D@��@��@a�@�@֡@��@~(@u�@4n@�@M@@b@�A@�4@E9@'�@C@͟@YK@C�@��@�d@��@|@�v@�D@��@oi@?�@(�@��@��@�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��OA�� A�ĜA��tA��A���A��'A��dA��KA�ȴA���A��HA���A���A�҉A�҉A���A�҉A�ҽA��TA��NA��,A�ƨAǬ�Aǭ�Aǡ�AǛ	AǙ�AǛ=AǜAǜ�AǛ=AǙeAǚ�AǜAǜxAǚ7Aǖ�Aǒ:AǑ4Aǐ.AǊrAǃ�A�pA�m�A�n�A�r|A�sMA�pA�pA�gA�C�A�4�A�+�A��A�:A��A��rA��BAƿAƵ�AƩ�Aƛ	A�sA�1�A���A��A���A���A�.�A�A���A���A�ԕAĚA�.�A�A���A�{JA�Z�A�h�A���A�}�A�/OA�� A�?�A�NA���A���A�^jA��CA���A�R�A�sMA��9A�AA��^A���A�B'A��A���A�M6A���A��eA��GA�H�A�4A���A���A��9A��-A��?A��-A��cA���A�8�A�v�A���A�A���A��A�-�A���A�-wAU2Az�4Av/�AsW?AqxAn��AkU�Af��Acm]Ab��AaXyA_E9A]�hAZ�ATl"AM#�AJ7AH?AF�AE�_AD��AD9�AC:�AA�A?4A=�'A<�A;PHA9-A8_A8�gA8>�A7�'A8�*A8��A8e�A6��A5)_A4�qA4rGA4l"A2_A/\�A(0UA&cA$��A$b�A$OvA$rGA#�A#�A!�A �rA!-�A"8A"JA!�OA!�9A!S&A I�A��A��A�A��AqvA �A�rA �$A"��A#RTA"�TA"�~A" �A!�}A!��A!A qvA�yA$�A��AA�9A�2A�hAA�A�OA�"AB[A	�A	A �A�AJ�A.IA�KA�A��A9�A�TA��A�A�A^�A8Ax�AY�A�A�HA>�A�DA��AVA�jA�A�AJA
�VA
H�A	��A	w�A	YA��A�A	�A	:�A�rA	"hA	tTA	h�A�A��A��As�A(�A�WA�AiDA \A��A�qAsA>BAeA
=A��A��A_A:�AF�A�A:�A��A	A�Ar�Ae�A�hA9XAA��Ao�A �t@���@�n�@�;�@��j@�s�@�!�@�z�@��^@���@��X@��@��@��K@�(�@�S�@�!@��F@��@��t@���@�oi@�[W@�D�@�j@�7�@��@�{J@���@�bN@��T@��@@�!�@�GE@�-@�y>@�@�$t@���@��@��Z@�n@�|�@�,�@�<�@�j�@��/@�c @�ԕ@��p@���@��E@��@��@��a@�8@�|�@߳�@��@ށo@��@���@�
�@���@ݥ@���@ܻ�@�B[@�p�@��2@�	@���@�s@�~�@���@� \@�.�@��@ӥ@�+@�u�@�C-@���@�Dg@ѡ�@��+@�4n@й�@ϱ[@���@ύP@ϓ�@�"�@���@ͼ�@���@���@��@˩�@��@���@ȩ�@Ǿw@��@ƅ�@��@�N<@Ė�@�8�@��D@���@�V@�PH@�.�@�|�@ÖS@�/@�|�@��F@���@��7@�J�@�ߤ@���@�!�@���@�iD@�L�@�)_@��@���@�\�@���@�K�@�&@��]@��+@�@���@���@�y�@�?}@��|@���@�u%@��w@���@��A@���@�hs@�q@�S�@�{@��@��@���@��@���@�*�@���@���@��~@�X@�<6@�&@��@��	@��@�g8@�0U@���@�
=@���@��@��@��@���@�u@���@�?}@��@�ں@�j@��@��@���@�N<@��"@���@�a|@��Z@�W?@��@���@�Ta@��6@��@�&�@��#@�hs@��I@�@�m�@���@���@�Dg@��@���@�?@�u�@�j�@�8�@��@��I@�u%@�E�@��>@�v`@��@��@��+@�r�@�@�rG@��@���@��6@���@�@@��?@���@�^5@�@��@��@���@�?}@�+@��@���@�+k@�خ@���@�=�@���@���@�{�@�$�@��Q@���@�&�@��f@���@�%�@�@���@�/@��8@��,@�q�@��A@���@�)_@���@�!@��>@��@@�e,@��@�Ɇ@��$@���@���@�B[@�O@�@��Q@�F�@��@���@��D@��@�u@�ϫ@�qv@�C@���@���@�l"@�:�@��@�ϫ@�`B@��@��,@���@���@��_@���@�M�@��T@�p�@�9�@�%F@�	l@���@�z@�$�@���@��"@�;d@�ی@��O@��u@�u�@�9X@���@�Vm@��@���@�a|@���@��V@�^�@� \@��@��[@�tT@�1'@��@���@�4@��_@��Y@��A@�d�@�/�@��+@���@�RT@��s@���@�z�@�l�@�(�@�K@ i@~�s@~��@~�h@~��@~�6@~Ov@}��@|��@|-�@{��@{�@{�@{O@z�<@yϫ@yN<@x��@x$@wE9@vn�@u�3@t��@t/�@s\)@r��@r;�@r@�@r�@q�@qk�@qk�@qs�@qc�@q&�@pu�@p  @o�r@ox@n�c@nc @m�n@mV@l�@l��@l�_@lC-@l6@l�@k�@k��@k�$@k�	@k��@k��@kg�@k��@kx@kY@jd�@jO@i�j@i��@i7L@hĜ@h��@h  @g��@g��@gW?@f��@fH�@e��@ej@d��@c��@b�s@bv�@a�.@a�@a��@au�@`��@`�_@`y>@`D�@`1@_�+@_�f@_33@_�@^�@^M�@^e@]s�@]f�@\ѷ@\I�@[��@Z�8@Z&�@Yϫ@Y��@Y[W@Y0�@X��@X�9@X��@XI�@X�@W��@W�@W�f@V��@V$�@U��@Uk�@U;@T�e@Tl"@T7�@TM@T  @S��@S~�@S.I@RQ@Q��@P�I@P?�@PM@O�K@OS�@N�c@N�!@Na|@M��@Mq@L�e@K��@K��@K$t@J��@JQ@I�#@I��@I\�@I2a@I+@I%@H�@Hz�@G��@G�K@G��@G�*@G��@Go�@G33@F�y@F�<@F\�@E��@EF@D�/@Dg8@D�@C��@B�y@B��@B}V@B+k@Aԕ@A�'@@��@@��@@�@@w�@@m�@@_@@6@?��@?�F@?U�@?,�@?$t@?o@>�X@>�@=��@=|@=k�@=e,@=[W@=IR@=@<�E@<�[@<�$@<h�@< �@;ݘ@;��@;��@;��@;|�@;RT@;o@:�,@:�@:B[@: �@9�T@9�@97L@8��@8S�@7��@7iD@7.I@7 i@6�,@6��@6=q@5�@5�C@5J�@4�	@4��@4Z@4'R@3��@3��@3&@2�8@2͟@2��@2��@2Q@2+k@2	@2�@2
�@1��@1��@1c@1-w@0��@0�I@/ݘ@//�@.��@.�h@.Ta@-�d@-�C@-��@-�M@-w2@-5�@,�O@,$@+�+@+ݘ@+�
@+��@+~�@+S�@+33@+ i@*�@*ȴ@*��@*s�@*{@)w2@(�@(��@(`�@(/�@(�@'�]@'ƨ@'S�@'�@&�@&��@&W�@&	@%��@%�n@%�M@%G�@$�f@$��@$m�@$*�@#�@#O@"��@"�x@"d�@"B[@")�@!�D@!�"@!IR@!�@ �@ �@ w�@ I�@ 7�@ !@�P@�@�@��@J@�H@c�@V@Ɇ@�@h�@6@�@�@�@��@6z@�"@��@��@��@xl@YK@+k@J@�D@��@��@a�@�@֡@��@~(@u�@4n@�@M@@b@�A@�4@E9@'�@C@͟@YK@C�@��@�d@��@|@�v@�D@��@oi@?�@(�@��@��@�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
&�B
&�B
&fB
&fB
&fB
&�B
&�B
&LB
&fB
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
%�B
#�B
#nB
#B
"hB
"NB
"NB
"NB
"hB
"4B
!�B
"B
"B
"B
!�B
!bB
!B
 �B
 �B
 B
�B
OB
�B
B
B
B
B
�B
B
�B
eB
�B
sB
�B
�B
�B
�B
HB
vB
�B
6B

�B
�B
 �B	��B
�B
<B
8�B
<B
;0B
I7B
T�B
��B
�_B
�B
�fBsB%�B0B>BBI�BO�BR�BVB^jBc�B��B�B��B��B�B�B�IB�B��B�BtB 4B�B�*B��B�eB�\B��B��B��B��B�GB�B�hBTaB-B�B
��B
�_B
�B
�,B
�MB
lWB
`�B
L~B
2�B
�B	�aB	�mB	�B	��B	�B	�IB	��B	}�B	x�B	s3B	hsB	_�B	Q�B	8�B	�B	 �B	-]B	/�B	0oB	0�B	1�B	5�B	:�B	:�B	8�B	5�B	5B	8�B	<6B	Q�B	i�B	}�B	�nB	�7B	��B	�NB	�B
  B
fB
0B
�B	�B	��B	tnB	ezB	gB	tB	�AB	�+B	��B	��B	��B	��B	��B	�FB	��B	��B	�+B	�4B	��B	��B	�OB	�BB	�iB
�B
1B
gB
6`B
F?B
C-B
J�B
K^B
KxB
L0B
K�B
H1B
F�B
C�B
B[B
?�B
<B
4B
,"B
!-B
�B
9B
B
@B
}B

�B
B	�jB	��B	�cB	��B	�=B	��B	ΊB	̈́B	��B	�1B	�FB	�\B	ѝB	�sB	��B	چB	�qB	��B	�}B
�B
 B	��B	�rB	�FB	��B	��B	��B	�nB	�nB	�nB	��B	��B
�B
�B
�B
4B
B
�B
sB
�B
�B
�B
�B
qB
QB
�B
B
�B
�B
qB
�B
�B
/B
/B
qB
_B

B
TB
�B
BB
}B
vB
}B
.B
�B
?B
# B
 �B
 �B
$�B
#�B
�B
xB
�B
�B
�B
�B
VB
�B
�B
]B
B
�B
dB
�B
�B
�B
�B
B
QB
�B
9B
�B
B
�B
	B
�B
�B
	7B
	7B
�B
B
KB
zB
?B
MB
�B
�B
'B
�B
aB
�B
�B
 4B	�wB	��B	�dB	�B	��B	�B	��B
B
 �B
B
UB
  B	�<B	�B	��B	��B	��B	��B	�(B	�wB	��B	��B	�}B	�(B	�B	��B	�0B	��B	�B	�2B	�zB	�`B	��B	��B	�ZB	�?B	�B	�hB	�B	�-B	�rB	��B
'B
 B	��B	��B	��B	��B	��B	�VB	��B	��B	�LB	��B	��B	�RB	��B	��B	�B	�-B	��B	�B	�B	�B	�B	�9B	��B	��B	��B	��B	�*B	�B	�B	�xB	�fB	��B	�LB	��B	��B	��B	�B	��B	�lB	��B	��B	�XB	��B	��B	��B	�rB	�^B	�JB	�dB	��B	��B
  B
 B
uB
B
B
 �B	��B	��B	�B	�xB	�xB	�B	�jB	�B	�wB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�wB	�wB	��B	�HB
 iB
�B
	B
B
�B
�B
	lB
	�B
�B
	B

�B
)B
DB
�B
�B
B
0B
~B
B
^B

�B

�B

�B
DB
dB
�B
dB
�B
�B
�B
B
 B
<B
NB
�B
�B
TB
B
�B
�B
�B
.B
B
NB
�B
�B
�B
}B
�B
4B
4B
�B
�B
 B
NB
 B
B
&B
�B
B
B
�B
B
�B
�B
�B
�B
FB
�B
�B
gB
�B
�B
�B
�B
9B
SB
9B
SB
�B
�B
�B
?B
�B

B
sB
�B
�B
B
B
�B
�B
�B
QB
�B
#B
�B
QB
QB
�B
�B
	B
	B
=B
WB
�B
WB
�B
/B
�B
B
OB
�B
�B
VB
�B
 �B
 �B
 \B
 �B
!�B
!�B
!�B
"hB
#B
#nB
#TB
#�B
#�B
#�B
$&B
$�B
$�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'B
'8B
'�B
'�B
($B
'�B
(�B
)�B
)_B
)�B
*eB
*�B
+B
+B
+�B
+kB
+6B
*B
+B
-�B
-]B
.B
.cB
.�B
.IB
.�B
/OB
.�B
/�B
/ B
/iB
/5B
/�B
0�B
0oB
0�B
0�B
0!B
0�B
0�B
/iB
0�B
1vB
1�B
1�B
1�B
1�B
1�B
1�B
1AB
0�B
0�B
/�B
/OB
-�B
-�B
-�B
,�B
-�B
-�B
-]B
.B
./B
/ B
/�B
/iB
/�B
/5B
/�B
0B
1AB
1B
0oB
0B
0UB
0;B
1B
1AB
3MB
3�B
4nB
5%B
5ZB
5�B
6+B
6�B
7LB
8lB
:�B
<PB
<�B
<�B
<B
<�B
<6B
="B
=�B
=VB
=�B
>BB
=�B
=�B
>BB
=VB
=VB
>(B
>�B
>B
=<B
<jB
;�B
<6B
<PB
<B
<�B
<jB
;�B
<�B
<�B
<jB
=�B
="B
=<B
<�B
=VB
=<B
>(B
=VB
>�B
>�B
>]B
?B
?�B
?B
?.B
?�B
?�B
@�B
@iB
@�B
@�B
@�B
A�B
@�B
@�B
A�B
A�B
A�B
BB
B�B
B�B
B�B
C-B
CB
B�B
CGB
CGB
C-B
CaB
B�B
C�B
C{B
CGB
C�B
C{B
D3B
DMB
D�B
D�B
EmB
EmB
E�B
E�B
F?B
FYB
F%B
F�B
F�B
F�B
GB
F�B
F�B
F�B
GEB
G�B
HfB
H�B
H�B
H�B
IB
IRB
I�B
JXB
KxB
KxB
LJB
M6B
M�B
M�B
N<B
N�B
N�B
N�B
OB
O(B
O\B
O\B
O�B
P}B
O�B
O�B
P}B
P�B
P}B
Q4B
Q�B
R�B
RoB
RTB
SuB
UMB
UgB
U�B
VB
VSB
V�B
V�B
W�B
W�B
W�B
W�B
XyB
XyB
X�B
YeB
YeB
YB
YB
Y�B
ZB
ZB
Z�B
[�B
[�B
[�B
\xB
\�B
\CB
]B
]IB
]/B
]~B
]�B
]~B
]IB
]dB
]�B
]�B
]�B
\�B
]�B
^B
^B
]�B
]�B
^�B
^OB
^�B
^�B
^jB
_VB
^�B
_VB
_B
_B
_;B
_;B
_�B
_�B
_�B
_�B
`�B
a|B
a�B
a|B
bB
b4B
b4B
b4B
bhB
bNB
cB
c�B
dZB
d�B
d�B
d�B
d�B
eB
e`B
ezB
e�B
eFB
ezB
e�B
e�B
fB
fLB
f�B
gB
gRB
g�B
h>B
g�B
g�B
hsB
h�B
g�B
hXB
hXB
h�B
h�B
i�B
iyB
i�B
i�B
i�B
j�B
jB
kQB
k�B
k�B
lWB
lqB
lqB
l�B
l�B
l�B
mB
m�B
nB
n/B
nIB
n�B
n}B
nIB
n�B
o�B
oOB
p;B
pB
p�B
p�B
qB
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
raB
r�B
r�B
r�B
sMB
r�B
r�B
s3B
s3B
shB
shB
s�B
s�B
tB
tB
tnB
t�B
t9B
uB
t�B
t�B
t�B
t�B
t�B
uZB
utB
utB
utB
u�B
u�B
u�B
vFB
vFB
vFB
v+B
v�B
v�B
v�B
w�B
w�B
w�B
xB
xRB
x81111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
&�B
&�B
&fB
&fB
&fB
&�B
&�B
&LB
&fB
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
%�B
#�B
#nB
#B
"hB
"NB
"NB
"NB
"hB
"4B
!�B
"B
"B
"B
!�B
!bB
!B
 �B
 �B
 B
�B
OB
�B
B
B
B
B
�B
B
�B
eB
�B
sB
�B
�B
�B
�B
HB
vB
�B
6B

�B
�B
 �B	��B
�B
<B
8�B
<B
;0B
I7B
T�B
��B
�_B
�B
�fBsB%�B0B>BBI�BO�BR�BVB^jBc�B��B�B��B��B�B�B�IB�B��B�BtB 4B�B�*B��B�eB�\B��B��B��B��B�GB�B�hBTaB-B�B
��B
�_B
�B
�,B
�MB
lWB
`�B
L~B
2�B
�B	�aB	�mB	�B	��B	�B	�IB	��B	}�B	x�B	s3B	hsB	_�B	Q�B	8�B	�B	 �B	-]B	/�B	0oB	0�B	1�B	5�B	:�B	:�B	8�B	5�B	5B	8�B	<6B	Q�B	i�B	}�B	�nB	�7B	��B	�NB	�B
  B
fB
0B
�B	�B	��B	tnB	ezB	gB	tB	�AB	�+B	��B	��B	��B	��B	��B	�FB	��B	��B	�+B	�4B	��B	��B	�OB	�BB	�iB
�B
1B
gB
6`B
F?B
C-B
J�B
K^B
KxB
L0B
K�B
H1B
F�B
C�B
B[B
?�B
<B
4B
,"B
!-B
�B
9B
B
@B
}B

�B
B	�jB	��B	�cB	��B	�=B	��B	ΊB	̈́B	��B	�1B	�FB	�\B	ѝB	�sB	��B	چB	�qB	��B	�}B
�B
 B	��B	�rB	�FB	��B	��B	��B	�nB	�nB	�nB	��B	��B
�B
�B
�B
4B
B
�B
sB
�B
�B
�B
�B
qB
QB
�B
B
�B
�B
qB
�B
�B
/B
/B
qB
_B

B
TB
�B
BB
}B
vB
}B
.B
�B
?B
# B
 �B
 �B
$�B
#�B
�B
xB
�B
�B
�B
�B
VB
�B
�B
]B
B
�B
dB
�B
�B
�B
�B
B
QB
�B
9B
�B
B
�B
	B
�B
�B
	7B
	7B
�B
B
KB
zB
?B
MB
�B
�B
'B
�B
aB
�B
�B
 4B	�wB	��B	�dB	�B	��B	�B	��B
B
 �B
B
UB
  B	�<B	�B	��B	��B	��B	��B	�(B	�wB	��B	��B	�}B	�(B	�B	��B	�0B	��B	�B	�2B	�zB	�`B	��B	��B	�ZB	�?B	�B	�hB	�B	�-B	�rB	��B
'B
 B	��B	��B	��B	��B	��B	�VB	��B	��B	�LB	��B	��B	�RB	��B	��B	�B	�-B	��B	�B	�B	�B	�B	�9B	��B	��B	��B	��B	�*B	�B	�B	�xB	�fB	��B	�LB	��B	��B	��B	�B	��B	�lB	��B	��B	�XB	��B	��B	��B	�rB	�^B	�JB	�dB	��B	��B
  B
 B
uB
B
B
 �B	��B	��B	�B	�xB	�xB	�B	�jB	�B	�wB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�wB	�wB	��B	�HB
 iB
�B
	B
B
�B
�B
	lB
	�B
�B
	B

�B
)B
DB
�B
�B
B
0B
~B
B
^B

�B

�B

�B
DB
dB
�B
dB
�B
�B
�B
B
 B
<B
NB
�B
�B
TB
B
�B
�B
�B
.B
B
NB
�B
�B
�B
}B
�B
4B
4B
�B
�B
 B
NB
 B
B
&B
�B
B
B
�B
B
�B
�B
�B
�B
FB
�B
�B
gB
�B
�B
�B
�B
9B
SB
9B
SB
�B
�B
�B
?B
�B

B
sB
�B
�B
B
B
�B
�B
�B
QB
�B
#B
�B
QB
QB
�B
�B
	B
	B
=B
WB
�B
WB
�B
/B
�B
B
OB
�B
�B
VB
�B
 �B
 �B
 \B
 �B
!�B
!�B
!�B
"hB
#B
#nB
#TB
#�B
#�B
#�B
$&B
$�B
$�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'B
'8B
'�B
'�B
($B
'�B
(�B
)�B
)_B
)�B
*eB
*�B
+B
+B
+�B
+kB
+6B
*B
+B
-�B
-]B
.B
.cB
.�B
.IB
.�B
/OB
.�B
/�B
/ B
/iB
/5B
/�B
0�B
0oB
0�B
0�B
0!B
0�B
0�B
/iB
0�B
1vB
1�B
1�B
1�B
1�B
1�B
1�B
1AB
0�B
0�B
/�B
/OB
-�B
-�B
-�B
,�B
-�B
-�B
-]B
.B
./B
/ B
/�B
/iB
/�B
/5B
/�B
0B
1AB
1B
0oB
0B
0UB
0;B
1B
1AB
3MB
3�B
4nB
5%B
5ZB
5�B
6+B
6�B
7LB
8lB
:�B
<PB
<�B
<�B
<B
<�B
<6B
="B
=�B
=VB
=�B
>BB
=�B
=�B
>BB
=VB
=VB
>(B
>�B
>B
=<B
<jB
;�B
<6B
<PB
<B
<�B
<jB
;�B
<�B
<�B
<jB
=�B
="B
=<B
<�B
=VB
=<B
>(B
=VB
>�B
>�B
>]B
?B
?�B
?B
?.B
?�B
?�B
@�B
@iB
@�B
@�B
@�B
A�B
@�B
@�B
A�B
A�B
A�B
BB
B�B
B�B
B�B
C-B
CB
B�B
CGB
CGB
C-B
CaB
B�B
C�B
C{B
CGB
C�B
C{B
D3B
DMB
D�B
D�B
EmB
EmB
E�B
E�B
F?B
FYB
F%B
F�B
F�B
F�B
GB
F�B
F�B
F�B
GEB
G�B
HfB
H�B
H�B
H�B
IB
IRB
I�B
JXB
KxB
KxB
LJB
M6B
M�B
M�B
N<B
N�B
N�B
N�B
OB
O(B
O\B
O\B
O�B
P}B
O�B
O�B
P}B
P�B
P}B
Q4B
Q�B
R�B
RoB
RTB
SuB
UMB
UgB
U�B
VB
VSB
V�B
V�B
W�B
W�B
W�B
W�B
XyB
XyB
X�B
YeB
YeB
YB
YB
Y�B
ZB
ZB
Z�B
[�B
[�B
[�B
\xB
\�B
\CB
]B
]IB
]/B
]~B
]�B
]~B
]IB
]dB
]�B
]�B
]�B
\�B
]�B
^B
^B
]�B
]�B
^�B
^OB
^�B
^�B
^jB
_VB
^�B
_VB
_B
_B
_;B
_;B
_�B
_�B
_�B
_�B
`�B
a|B
a�B
a|B
bB
b4B
b4B
b4B
bhB
bNB
cB
c�B
dZB
d�B
d�B
d�B
d�B
eB
e`B
ezB
e�B
eFB
ezB
e�B
e�B
fB
fLB
f�B
gB
gRB
g�B
h>B
g�B
g�B
hsB
h�B
g�B
hXB
hXB
h�B
h�B
i�B
iyB
i�B
i�B
i�B
j�B
jB
kQB
k�B
k�B
lWB
lqB
lqB
l�B
l�B
l�B
mB
m�B
nB
n/B
nIB
n�B
n}B
nIB
n�B
o�B
oOB
p;B
pB
p�B
p�B
qB
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
raB
r�B
r�B
r�B
sMB
r�B
r�B
s3B
s3B
shB
shB
s�B
s�B
tB
tB
tnB
t�B
t9B
uB
t�B
t�B
t�B
t�B
t�B
uZB
utB
utB
utB
u�B
u�B
u�B
vFB
vFB
vFB
v+B
v�B
v�B
v�B
w�B
w�B
w�B
xB
xRB
x81111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104917  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173704  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173704  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173704                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023712  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023712  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                