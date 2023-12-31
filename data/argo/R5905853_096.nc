CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:39:50Z creation;2022-06-04T17:39:51Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ͱ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20220604173950  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               `A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ٕ�+<M1   @ٕ�rX�&@/!���o�c>fffff1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A^ffA�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx��B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B���B���B�  B�  B���B�  B�  B왚B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU�fDVfDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�<�DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @  @vff@�33@�33A��A=��A\  A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBx33B��B�� B�� B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B�L�B��3B��3Bó3Bǳ3B��fBπ BӀ B׳3B۳3B߀ B�3B�3B�L�B��fB�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚC�4CٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCA�4CCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCu�4Cw�4CyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT��DU|�DU��DVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�~fD��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��fD��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�8 D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�x 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��XA��XA��A���Aؾ�Aؿ�A�ŢA�ɺA�ɺA�ɺA��XA�ȀAشAح�Aؗ�A�f�A�V�A�T,A�OBA�OA�K�A�>BA�5?A��A��]A��A׶A׭�AעhA�J�A�!�Aլ=A�FAӫkA�=<A�N<A��9A�gmA�:A��A�0UAΤ�A͙eA˯�AɞOA��A�?A��A�}�A�?�A���A�/�A�e,A�OvA�k�A�:A�,A�%A�z�A�C�A�f�A�/A��A�I�A��\A�_pA��hA���A���A�K^A��A���A�Y�A��PA�*�A��7A�}"A��%A��A�A��A���A���A�� A��xA���A�?�A���A��%A�ɆA�7A�+�A�?}A�7�A�Q�A��A�B�A�4�A��A}�Aw�4AtVmAp�Am��Al�6Aj�Ag	Ab�+A_4nA\ĜA[SAW�AT�hAR�VAPTaAK͟AJ��AI�AGg8AFCAEu�AD \ABy>A@f�A=T�A;�A:jA9��A8�"A8��A7Q�A3J#A/��A.C-A+OA)�'A(v�A)'RA+CA-r�A-�A.#�A-��A-��A,�CA*�A)�"A)��A)�A(҉A(C�A&kQA%ߤA%��A$�WA#Z�A!�A �A�A�A��Ar�AخA�zAE�A��A��A��AA�A�AZ�AA9XA��A6zA�RA�"A��A�A�IA�A��AX�A�AA��A,=A]�AѷA�Av�A1�A�A~(AQAA
�DA
VmA
qA	��A	�A	��A	N<A�8A�nA-Am]A&�A$tA-�A�=A[WA�>AA�A�NARTAYA�6Ap�A9XA�}A��A�A��Ah
ADgA�A �QA خA �qA ��A %F@�p�@�X�@��@�bN@�4@���@��7@��A@��A@�e,@��1@��S@�9�@���@�V�@��@�;@�YK@�c�@�?�@��=@���@�'R@�&@�J�@�@��B@��@�F@�u%@��d@�@���@�z@�
�@�F@�q�@�S@��5@�h@�;�@禵@�J�@��@�\�@�}V@�@�?�@�خ@�ߤ@�*�@�0�@�PH@�ϫ@᯸@�Y�@�S�@�e�@�2a@໙@�#:@�Z�@ީ�@݊	@�S@�Xy@�Z�@�s�@�Ɇ@٦�@�Q�@��@ظR@�5?@׬q@��@֫6@�2�@�b�@Բ�@��T@�B�@ҳh@��@��m@�'R@ϨX@�>�@ξ�@�q@�9X@��@;w@��j@�`B@��5@˚k@�+�@ʡb@�� @Ɂ�@ɒ:@�^�@��@ȩ�@ȂA@�*�@�7@�s@ƥz@��@�YK@�G@���@�J�@��@ĵ�@�=q@��@�y�@�@O@���@�@���@�j�@���@���@�h
@�	@��]@���@���@��@���@�x�@���@�z@�$�@��@�s@��@��$@�@���@�a@��f@��_@��)@�@O@��|@��1@�[�@�)�@���@�ߤ@�c�@��@��"@�+@��@��@��'@��e@���@�c�@��.@���@�g�@�@���@�)�@��@�a�@� \@���@��@��@�ی@�ѷ@��@��$@�8�@�J#@���@��@���@�B�@��@��@��@�x@�Y�@���@�/�@��@��"@�Mj@��f@��m@�8�@�@��)@��@�@��2@���@�Ĝ@��+@�M�@�@��j@���@�L�@�!-@���@���@��1@�Xy@�@�ԕ@�p�@�5�@�'�@�(@���@��9@��@��@�(�@�خ@�X�@���@�	@���@�T�@��@�[�@��W@�@���@��@@��{@�/�@���@�1'@�+k@���@�s@�.�@�j�@�W?@�C@�@��M@��2@��@�1�@�j@�_p@�P�@�?}@�4�@�'�@�"�@�@@��@�@���@��@��v@���@��U@��@��O@���@�}V@�:�@�1@��3@��$@�X@��@���@���@�B[@��@��0@�m]@�(@���@��!@��L@��@�~�@��@���@���@�6z@��@���@�H@�($@�x@���@��^@�|�@�>�@��@���@���@�|�@��@���@���@�J�@���@���@�'R@��Q@���@�k�@�/@��m@�1@���@��@�҉@�v�@�� @���@��M@�|@�t�@�o�@�8�@�S@��]@��@�kQ@��@��F@�}�@�_p@��c@���@�z@�M�@�5?@��@�$@��@���@�e,@�7L@��@���@���@��@���@�Z�@�7@~��@~�R@~e@}��@|��@|<�@{�[@{J#@{�@z�m@z��@y��@y�@xZ@w��@w�}@w�0@w��@w��@w�4@w�@v�\@v&�@u\�@t�)@t�@t4n@t"h@s�k@r��@r�@q�j@q��@p�@o�r@o�a@o�@o�@n�8@n�1@nR�@m[W@l?�@l1@k�W@k��@kC�@k�@j��@j+k@i�@i��@i��@iJ�@i�@h�@gݘ@g�4@f�2@f �@ej@dѷ@d�@c�w@c�4@c+@b��@bZ�@b	@a�n@aX@a�@`��@`r�@_��@_�	@_qv@_Z�@_�@^?@]�-@\��@\H@[�@[X�@Z�@Z�1@ZZ�@Y��@Yhs@X�|@X�4@X|�@X	�@W�@V��@U��@U|@U%F@T��@TG@S��@S1�@R�@R��@R;�@Q��@Q��@Qp�@P��@Oخ@OK�@NE�@M�C@M}�@M`B@M�@LV�@K�;@Ky�@J��@J)�@I�Z@I�n@Ic@IQ�@I&�@I�@H�f@H��@Hw�@G,�@Fl�@F@�@F
�@E��@Ej@E/@D�@Cƨ@C��@Cqv@C9�@C�@B�+@BGE@A�T@A��@A#�@@�K@@֡@@�@@�$@@�u@@q@@"h@?��@?��@>�8@>�}@>�A@>.�@>�@=�z@=f�@=+@<�e@<Q�@<-�@;��@;��@;�	@;g�@;�@:=q@:�@:4@:
�@9�@9a�@8�	@8Ɇ@8��@8�.@8l"@8%�@7�}@7�k@7�:@7iD@6�@6��@6~�@6B[@5��@4�f@4��@4Ft@4@3��@3�@3Z�@2��@2��@28�@2�@1�^@1c�@1O�@0��@0]d@0$@/�@/�@.�@.��@.($@-��@-�=@-f�@,�P@,��@,��@,��@,r�@,Q�@,�@+�g@+�f@+/�@+@*��@*E�@)��@)A @(��@(��@(tT@(7�@'�g@'�@'�k@'n/@'E9@'4�@&�@&�F@&W�@&8�@&	@&@%�@%��@%�9@%��@%��@%�n@%��@%=�@$�P@$�@$�U@$��@$b@#�}@#�q@#_p@"��@"�m@"�@"Q@"J�@"@�@"�@!�7@!e,@!8�@! \@ ��@ �K@ ��@ �9@ �@ tT@ Xy@ D�@ Ft@ A�@ �@��@J#@�@�2@��@��@��@�!@��@=q@�@�@��@�-@�"@ \@�@�O@��@�I@K^@�@	�@�
@��@RT@��@��@��@{�@s�@H�@��@�t@��@�h@|@J�@!�@�[@��@h�@~@��@�@��@��@o�@F�@33@$t@@�<@ff@@�@	@�@{@ �@��@Dg@��@�$@��@r�@(�@1@�g@�*@l�@>�@@��@i�@H�@@�@1�@�j@��@Q�@%F@��@Ɇ@��@�@��@h�@(�@1@��@�@�,@��@s�@V@�@�@�@��@��@��@p�@A @�	@�@�O@oi@PH@�@�@�f@j�@W?@6z@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��XA��XA��A���Aؾ�Aؿ�A�ŢA�ɺA�ɺA�ɺA��XA�ȀAشAح�Aؗ�A�f�A�V�A�T,A�OBA�OA�K�A�>BA�5?A��A��]A��A׶A׭�AעhA�J�A�!�Aլ=A�FAӫkA�=<A�N<A��9A�gmA�:A��A�0UAΤ�A͙eA˯�AɞOA��A�?A��A�}�A�?�A���A�/�A�e,A�OvA�k�A�:A�,A�%A�z�A�C�A�f�A�/A��A�I�A��\A�_pA��hA���A���A�K^A��A���A�Y�A��PA�*�A��7A�}"A��%A��A�A��A���A���A�� A��xA���A�?�A���A��%A�ɆA�7A�+�A�?}A�7�A�Q�A��A�B�A�4�A��A}�Aw�4AtVmAp�Am��Al�6Aj�Ag	Ab�+A_4nA\ĜA[SAW�AT�hAR�VAPTaAK͟AJ��AI�AGg8AFCAEu�AD \ABy>A@f�A=T�A;�A:jA9��A8�"A8��A7Q�A3J#A/��A.C-A+OA)�'A(v�A)'RA+CA-r�A-�A.#�A-��A-��A,�CA*�A)�"A)��A)�A(҉A(C�A&kQA%ߤA%��A$�WA#Z�A!�A �A�A�A��Ar�AخA�zAE�A��A��A��AA�A�AZ�AA9XA��A6zA�RA�"A��A�A�IA�A��AX�A�AA��A,=A]�AѷA�Av�A1�A�A~(AQAA
�DA
VmA
qA	��A	�A	��A	N<A�8A�nA-Am]A&�A$tA-�A�=A[WA�>AA�A�NARTAYA�6Ap�A9XA�}A��A�A��Ah
ADgA�A �QA خA �qA ��A %F@�p�@�X�@��@�bN@�4@���@��7@��A@��A@�e,@��1@��S@�9�@���@�V�@��@�;@�YK@�c�@�?�@��=@���@�'R@�&@�J�@�@��B@��@�F@�u%@��d@�@���@�z@�
�@�F@�q�@�S@��5@�h@�;�@禵@�J�@��@�\�@�}V@�@�?�@�خ@�ߤ@�*�@�0�@�PH@�ϫ@᯸@�Y�@�S�@�e�@�2a@໙@�#:@�Z�@ީ�@݊	@�S@�Xy@�Z�@�s�@�Ɇ@٦�@�Q�@��@ظR@�5?@׬q@��@֫6@�2�@�b�@Բ�@��T@�B�@ҳh@��@��m@�'R@ϨX@�>�@ξ�@�q@�9X@��@;w@��j@�`B@��5@˚k@�+�@ʡb@�� @Ɂ�@ɒ:@�^�@��@ȩ�@ȂA@�*�@�7@�s@ƥz@��@�YK@�G@���@�J�@��@ĵ�@�=q@��@�y�@�@O@���@�@���@�j�@���@���@�h
@�	@��]@���@���@��@���@�x�@���@�z@�$�@��@�s@��@��$@�@���@�a@��f@��_@��)@�@O@��|@��1@�[�@�)�@���@�ߤ@�c�@��@��"@�+@��@��@��'@��e@���@�c�@��.@���@�g�@�@���@�)�@��@�a�@� \@���@��@��@�ی@�ѷ@��@��$@�8�@�J#@���@��@���@�B�@��@��@��@�x@�Y�@���@�/�@��@��"@�Mj@��f@��m@�8�@�@��)@��@�@��2@���@�Ĝ@��+@�M�@�@��j@���@�L�@�!-@���@���@��1@�Xy@�@�ԕ@�p�@�5�@�'�@�(@���@��9@��@��@�(�@�خ@�X�@���@�	@���@�T�@��@�[�@��W@�@���@��@@��{@�/�@���@�1'@�+k@���@�s@�.�@�j�@�W?@�C@�@��M@��2@��@�1�@�j@�_p@�P�@�?}@�4�@�'�@�"�@�@@��@�@���@��@��v@���@��U@��@��O@���@�}V@�:�@�1@��3@��$@�X@��@���@���@�B[@��@��0@�m]@�(@���@��!@��L@��@�~�@��@���@���@�6z@��@���@�H@�($@�x@���@��^@�|�@�>�@��@���@���@�|�@��@���@���@�J�@���@���@�'R@��Q@���@�k�@�/@��m@�1@���@��@�҉@�v�@�� @���@��M@�|@�t�@�o�@�8�@�S@��]@��@�kQ@��@��F@�}�@�_p@��c@���@�z@�M�@�5?@��@�$@��@���@�e,@�7L@��@���@���@��@���@�Z�@�7@~��@~�R@~e@}��@|��@|<�@{�[@{J#@{�@z�m@z��@y��@y�@xZ@w��@w�}@w�0@w��@w��@w�4@w�@v�\@v&�@u\�@t�)@t�@t4n@t"h@s�k@r��@r�@q�j@q��@p�@o�r@o�a@o�@o�@n�8@n�1@nR�@m[W@l?�@l1@k�W@k��@kC�@k�@j��@j+k@i�@i��@i��@iJ�@i�@h�@gݘ@g�4@f�2@f �@ej@dѷ@d�@c�w@c�4@c+@b��@bZ�@b	@a�n@aX@a�@`��@`r�@_��@_�	@_qv@_Z�@_�@^?@]�-@\��@\H@[�@[X�@Z�@Z�1@ZZ�@Y��@Yhs@X�|@X�4@X|�@X	�@W�@V��@U��@U|@U%F@T��@TG@S��@S1�@R�@R��@R;�@Q��@Q��@Qp�@P��@Oخ@OK�@NE�@M�C@M}�@M`B@M�@LV�@K�;@Ky�@J��@J)�@I�Z@I�n@Ic@IQ�@I&�@I�@H�f@H��@Hw�@G,�@Fl�@F@�@F
�@E��@Ej@E/@D�@Cƨ@C��@Cqv@C9�@C�@B�+@BGE@A�T@A��@A#�@@�K@@֡@@�@@�$@@�u@@q@@"h@?��@?��@>�8@>�}@>�A@>.�@>�@=�z@=f�@=+@<�e@<Q�@<-�@;��@;��@;�	@;g�@;�@:=q@:�@:4@:
�@9�@9a�@8�	@8Ɇ@8��@8�.@8l"@8%�@7�}@7�k@7�:@7iD@6�@6��@6~�@6B[@5��@4�f@4��@4Ft@4@3��@3�@3Z�@2��@2��@28�@2�@1�^@1c�@1O�@0��@0]d@0$@/�@/�@.�@.��@.($@-��@-�=@-f�@,�P@,��@,��@,��@,r�@,Q�@,�@+�g@+�f@+/�@+@*��@*E�@)��@)A @(��@(��@(tT@(7�@'�g@'�@'�k@'n/@'E9@'4�@&�@&�F@&W�@&8�@&	@&@%�@%��@%�9@%��@%��@%�n@%��@%=�@$�P@$�@$�U@$��@$b@#�}@#�q@#_p@"��@"�m@"�@"Q@"J�@"@�@"�@!�7@!e,@!8�@! \@ ��@ �K@ ��@ �9@ �@ tT@ Xy@ D�@ Ft@ A�@ �@��@J#@�@�2@��@��@��@�!@��@=q@�@�@��@�-@�"@ \@�@�O@��@�I@K^@�@	�@�
@��@RT@��@��@��@{�@s�@H�@��@�t@��@�h@|@J�@!�@�[@��@h�@~@��@�@��@��@o�@F�@33@$t@@�<@ff@@�@	@�@{@ �@��@Dg@��@�$@��@r�@(�@1@�g@�*@l�@>�@@��@i�@H�@@�@1�@�j@��@Q�@%F@��@Ɇ@��@�@��@h�@(�@1@��@�@�,@��@s�@V@�@�@�@��@��@��@p�@A @�	@�@�O@oi@PH@�@�@�f@j�@W?@6z@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
�B
�B
�B
�8B
�8B
�mB
�B
�8B
�RB
�RB
�mB
��B
��B
��B
��B
��B
��B
�]B
��B
�'B
�xB
�.B
�~B
�HB
�B
�B
�aB
��B
�BZ�BbBZkBESB3hB(XB'�B# B �B5B_B�B�B;B
�B
�5B
��B
��B
�B B�BjBN�B� B�2B�B�$B�TB��B�0B��B B�B"�B,�B-�B+�B:^B7�B,�B�B�B�BoB�B�}BޞB�eB�2B�B�`B�B��B�=By	Be,BK^B �B
ևB
�AB
��B
Z�B
=�B
1vB
# B
�B
�B
B
.B	��B	��B	��B	��B	��B	�)B	�xB	v�B	a�B	M6B	?�B	6zB	'�B	B	�B	
�B	&�B	)B	"�B	qB	�B	B	�B	+B	$�B	,WB	3hB	?�B	ZkB	tnB	�aB	�zB	��B	nB	c�B	U�B	YB	d�B	~wB	��B	��B
�B
�B
4B
�B
FB
0B
B
�B
�B
~B
�B
YB
�B
�B
B
�B	�JB	�0B	�sB	�B	�SB	�B	�2B	�mB	ּB	�gB	өB	֡B	��B	�B	�@B	ҽB	��B	��B	�;B	��B	�B	�DB	�yB	�fB	�B	�B	�B	�BB	�-B	߾B	�B	چB	��B	ۦB	�kB	�yB	��B	�$B	��B	��B	�QB	�	B	چB	��B	�1B	��B	��B	�IB	��B	�]B	�	B	�SB	�9B	خB	�B	ބB	�B	߾B	�~B	�)B	�WB	��B	�B	�?B	�9B	�B	�B	�sB	��B	�#B	�	B	޸B	�B	�B	�B	�CB	�B	��B	�B	�vB	�B	��B	�^B	�xB	�0B	��B	�B	��B	��B	��B	�B	�JB	�^B	��B	�B	��B	��B	��B	��B	�^B	��B	�%B	�TB	�MB	��B	�3B	�B	��B	� B	�}B	�/B	��B	�WB	�B	��B	�sB	�B	��B	�sB	��B	�B	��B	��B	�B	��B	�GB	�B	�vB	��B	��B	�B	�B	�B	��B	��B	�B	��B	��B	�rB	��B	�B	�?B	�ZB	�B	�`B	�fB	��B	��B	�RB	�^B	�xB	�B	��B	�B
 B
B	�B	�(B	�<B	��B	�JB	�rB	�B	��B	�+B	��B	��B	�tB	��B	��B	��B	��B	��B	�zB	��B	�B	��B	�|B	�B	��B	��B	��B	�JB	�dB	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	��B
 4B
 iB
 �B
 iB
 �B
 �B
 OB
 B	��B
  B	��B	�cB
 B	��B	��B	�}B	�cB	�HB	�HB	�.B	�B	�B	��B	��B	�HB	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B
 OB
 �B
�B
'B
�B
aB
aB
{B
�B
�B
�B
{B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
'B
uB
B
B
�B
-B
 4B	�]B	�B	��B	�B	��B	��B	�B	��B	�B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�qB	��B	��B	��B	�BB	��B	�HB	�cB	�}B
 4B
 iB
 �B
 �B
B
oB
�B
�B
[B
�B
�B
-B
3B
3B
3B
MB
�B
�B
B
�B
tB
�B
�B
�B
B
+B
B
�B
�B
	�B
B
�B
^B
�B
JB
�B

#B
�B
�B
�B
�B
B
�B

�B
dB
(B
bB
�B
�B
4B
B
NB
�B
�B
B
oB
�B
&B
�B
�B
�B
EB
�B
kB
qB
�B
]B
CB
�B
�B
�B
~B
B
OB
OB
�B
!B
VB
VB
VB
;B
VB
�B
 'B
 �B
 �B
 �B
!�B
"4B
"NB
"hB
"hB
"�B
# B
#nB
#�B
#nB
#�B
#�B
$ZB
$ZB
$ZB
$�B
$�B
%B
%`B
%`B
%FB
%`B
%zB
%�B
%�B
&2B
'B
'B
&�B
'�B
(>B
(XB
(sB
(sB
(XB
(�B
)*B
)DB
)�B
)�B
*KB
*B
*�B
*�B
+kB
,=B
,qB
,�B
-CB
-�B
.}B
/�B
0!B
0�B
0�B
0�B
0�B
1'B
1'B
1'B
1B
1vB
2|B
2|B
2�B
2�B
33B
3MB
3�B
3�B
3�B
3�B
3hB
4�B
5B
5�B
6B
6+B
6`B
6FB
6`B
6`B
6�B
6�B
72B
8B
88B
8RB
8�B
8�B
9�B
:*B
:�B
:�B
:�B
;JB
;�B
;�B
;�B
;�B
<6B
<PB
<6B
="B
=�B
=�B
=�B
>(B
>(B
>]B
>�B
>�B
?.B
?B
?.B
?HB
?�B
?�B
@ B
@ B
@�B
AB
AUB
A�B
A�B
A�B
AoB
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B[B
C{B
C�B
C�B
C�B
DB
D�B
EB
E�B
E�B
FtB
F�B
F�B
F�B
F�B
GEB
G�B
G�B
HB
HB
HKB
IB
H�B
H�B
H�B
H�B
H�B
H1B
HB
HfB
IlB
I�B
J	B
JXB
J�B
J�B
KDB
K�B
K�B
LB
LB
K�B
K�B
K�B
K^B
KB
K)B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
MjB
MjB
M�B
M�B
NVB
N<B
N�B
OBB
O\B
OBB
OvB
O�B
O�B
P.B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R B
R�B
RoB
SB
S�B
S�B
S�B
TFB
TaB
T�B
U�B
U�B
VB
V9B
VSB
V�B
WYB
W�B
XB
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
X�B
X�B
X�B
X�B
X�B
Y1B
YKB
YeB
YB
Y�B
ZQB
Z�B
Z�B
Z�B
[�B
\�B
\�B
]B
\�B
]/B
]/B
]dB
]�B
]�B
]�B
]�B
^�B
^�B
^�B
_B
_�B
_�B
_�B
`B
a-B
aHB
a�B
bNB
bhB
b�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d@B
d�B
d�B
d�B
e�B
e�B
e�B
f�B
f�B
f�B
gB
gRB
g�B
g�B
g�B
g�B
g�B
g�B
h$B
hXB
hsB
h�B
i*B
i_B
iyB
jKB
jB
j�B
kB
kB
kB
kB
kQB
kQB
kkB
k�B
lB
lB
l=B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
mB
mCB
mCB
m]B
mwB
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
nB
ncB
n}B
n}B
n�B
o�B
o�B
o�B
o�B
pB
pUB
poB
poB
poB
p�B
p�B
qAB
q�B
q�B
q�B
q�B
q�B
q�B
rB
r-B
r-B
rGB
raB
r�B
r�B
r�B
r�B
sB
s3B
s�B
s�B
s�B
tB
tTB
tTB
t�B
t�B
t�B
u%B
uB
u%B
uZB
utB
u�B
u�B
u�B
u�B
u�B
u�B
v+B
vFB
v�B
v�B
v�B
v�B
w�B
wfB
w�B
w�B
wfB
xB
x�B
x�B
y$B
y>B
y>B
yXB
y�B
y�B
z^B
zxB
z�B
z�B
{B
{0B
{�B
|�B
|�B
|�B
}B
}B
}B
}<B
}qB
}�B
}�B
~�B
~�B
~�B
B
.B
cB
�B
�B
� B
�iB
��B
��B
� B
�oB
�oB
��B
��B
��B
�B
�uB
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
�B
�B
�B
�8B
�8B
�mB
�B
�8B
�RB
�RB
�mB
��B
��B
��B
��B
��B
��B
�]B
��B
�'B
�xB
�.B
�~B
�HB
�B
�B
�aB
��B
�BZ�BbBZkBESB3hB(XB'�B# B �B5B_B�B�B;B
�B
�5B
��B
��B
�B B�BjBN�B� B�2B�B�$B�TB��B�0B��B B�B"�B,�B-�B+�B:^B7�B,�B�B�B�BoB�B�}BޞB�eB�2B�B�`B�B��B�=By	Be,BK^B �B
ևB
�AB
��B
Z�B
=�B
1vB
# B
�B
�B
B
.B	��B	��B	��B	��B	��B	�)B	�xB	v�B	a�B	M6B	?�B	6zB	'�B	B	�B	
�B	&�B	)B	"�B	qB	�B	B	�B	+B	$�B	,WB	3hB	?�B	ZkB	tnB	�aB	�zB	��B	nB	c�B	U�B	YB	d�B	~wB	��B	��B
�B
�B
4B
�B
FB
0B
B
�B
�B
~B
�B
YB
�B
�B
B
�B	�JB	�0B	�sB	�B	�SB	�B	�2B	�mB	ּB	�gB	өB	֡B	��B	�B	�@B	ҽB	��B	��B	�;B	��B	�B	�DB	�yB	�fB	�B	�B	�B	�BB	�-B	߾B	�B	چB	��B	ۦB	�kB	�yB	��B	�$B	��B	��B	�QB	�	B	چB	��B	�1B	��B	��B	�IB	��B	�]B	�	B	�SB	�9B	خB	�B	ބB	�B	߾B	�~B	�)B	�WB	��B	�B	�?B	�9B	�B	�B	�sB	��B	�#B	�	B	޸B	�B	�B	�B	�CB	�B	��B	�B	�vB	�B	��B	�^B	�xB	�0B	��B	�B	��B	��B	��B	�B	�JB	�^B	��B	�B	��B	��B	��B	��B	�^B	��B	�%B	�TB	�MB	��B	�3B	�B	��B	� B	�}B	�/B	��B	�WB	�B	��B	�sB	�B	��B	�sB	��B	�B	��B	��B	�B	��B	�GB	�B	�vB	��B	��B	�B	�B	�B	��B	��B	�B	��B	��B	�rB	��B	�B	�?B	�ZB	�B	�`B	�fB	��B	��B	�RB	�^B	�xB	�B	��B	�B
 B
B	�B	�(B	�<B	��B	�JB	�rB	�B	��B	�+B	��B	��B	�tB	��B	��B	��B	��B	��B	�zB	��B	�B	��B	�|B	�B	��B	��B	��B	�JB	�dB	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	��B
 4B
 iB
 �B
 iB
 �B
 �B
 OB
 B	��B
  B	��B	�cB
 B	��B	��B	�}B	�cB	�HB	�HB	�.B	�B	�B	��B	��B	�HB	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B
 OB
 �B
�B
'B
�B
aB
aB
{B
�B
�B
�B
{B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
'B
uB
B
B
�B
-B
 4B	�]B	�B	��B	�B	��B	��B	�B	��B	�B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�qB	��B	��B	��B	�BB	��B	�HB	�cB	�}B
 4B
 iB
 �B
 �B
B
oB
�B
�B
[B
�B
�B
-B
3B
3B
3B
MB
�B
�B
B
�B
tB
�B
�B
�B
B
+B
B
�B
�B
	�B
B
�B
^B
�B
JB
�B

#B
�B
�B
�B
�B
B
�B

�B
dB
(B
bB
�B
�B
4B
B
NB
�B
�B
B
oB
�B
&B
�B
�B
�B
EB
�B
kB
qB
�B
]B
CB
�B
�B
�B
~B
B
OB
OB
�B
!B
VB
VB
VB
;B
VB
�B
 'B
 �B
 �B
 �B
!�B
"4B
"NB
"hB
"hB
"�B
# B
#nB
#�B
#nB
#�B
#�B
$ZB
$ZB
$ZB
$�B
$�B
%B
%`B
%`B
%FB
%`B
%zB
%�B
%�B
&2B
'B
'B
&�B
'�B
(>B
(XB
(sB
(sB
(XB
(�B
)*B
)DB
)�B
)�B
*KB
*B
*�B
*�B
+kB
,=B
,qB
,�B
-CB
-�B
.}B
/�B
0!B
0�B
0�B
0�B
0�B
1'B
1'B
1'B
1B
1vB
2|B
2|B
2�B
2�B
33B
3MB
3�B
3�B
3�B
3�B
3hB
4�B
5B
5�B
6B
6+B
6`B
6FB
6`B
6`B
6�B
6�B
72B
8B
88B
8RB
8�B
8�B
9�B
:*B
:�B
:�B
:�B
;JB
;�B
;�B
;�B
;�B
<6B
<PB
<6B
="B
=�B
=�B
=�B
>(B
>(B
>]B
>�B
>�B
?.B
?B
?.B
?HB
?�B
?�B
@ B
@ B
@�B
AB
AUB
A�B
A�B
A�B
AoB
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B[B
C{B
C�B
C�B
C�B
DB
D�B
EB
E�B
E�B
FtB
F�B
F�B
F�B
F�B
GEB
G�B
G�B
HB
HB
HKB
IB
H�B
H�B
H�B
H�B
H�B
H1B
HB
HfB
IlB
I�B
J	B
JXB
J�B
J�B
KDB
K�B
K�B
LB
LB
K�B
K�B
K�B
K^B
KB
K)B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
MjB
MjB
M�B
M�B
NVB
N<B
N�B
OBB
O\B
OBB
OvB
O�B
O�B
P.B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R B
R�B
RoB
SB
S�B
S�B
S�B
TFB
TaB
T�B
U�B
U�B
VB
V9B
VSB
V�B
WYB
W�B
XB
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
X�B
X�B
X�B
X�B
X�B
Y1B
YKB
YeB
YB
Y�B
ZQB
Z�B
Z�B
Z�B
[�B
\�B
\�B
]B
\�B
]/B
]/B
]dB
]�B
]�B
]�B
]�B
^�B
^�B
^�B
_B
_�B
_�B
_�B
`B
a-B
aHB
a�B
bNB
bhB
b�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d@B
d�B
d�B
d�B
e�B
e�B
e�B
f�B
f�B
f�B
gB
gRB
g�B
g�B
g�B
g�B
g�B
g�B
h$B
hXB
hsB
h�B
i*B
i_B
iyB
jKB
jB
j�B
kB
kB
kB
kB
kQB
kQB
kkB
k�B
lB
lB
l=B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
mB
mCB
mCB
m]B
mwB
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
nB
ncB
n}B
n}B
n�B
o�B
o�B
o�B
o�B
pB
pUB
poB
poB
poB
p�B
p�B
qAB
q�B
q�B
q�B
q�B
q�B
q�B
rB
r-B
r-B
rGB
raB
r�B
r�B
r�B
r�B
sB
s3B
s�B
s�B
s�B
tB
tTB
tTB
t�B
t�B
t�B
u%B
uB
u%B
uZB
utB
u�B
u�B
u�B
u�B
u�B
u�B
v+B
vFB
v�B
v�B
v�B
v�B
w�B
wfB
w�B
w�B
wfB
xB
x�B
x�B
y$B
y>B
y>B
yXB
y�B
y�B
z^B
zxB
z�B
z�B
{B
{0B
{�B
|�B
|�B
|�B
}B
}B
}B
}<B
}qB
}�B
}�B
~�B
~�B
~�B
B
.B
cB
�B
�B
� B
�iB
��B
��B
� B
�oB
�oB
��B
��B
��B
�B
�uB
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104923  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173950  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173951  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173951                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023958  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023958  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                