CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-29T00:35:40Z creation;2018-01-29T00:35:44Z conversion to V3.1;2019-12-19T07:46:32Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180129003540  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_205                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�H5���1   @�H6www�@3�qu��dd��)_1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D]��D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @|��@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fD|�D�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD��DvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/��D0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]� D^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�~fD��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�~fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�{A�{A�oA�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�bA�-A�x�A�AŬA�ffA�C�A�=qA�+A��A���A���Aĩ�A�p�A�XA�A�A�"�A�JA�
=A�
=A��A��Aç�A�VA�  A���A\A�z�A�bNA��hA��jA��RA���A�/A��9A�XA�VA�(�A���A��A���A�A��jA���A�^5A�A�Q�A�5?A�VA�bA�9XA��A�^5A��A�A�A�  A�VA�(�A�bNA���A�+A�bA��A��A��A���A�O�A��RA���A��A�A���A���A�x�A�jA�\)A�5?A�A��^A�p�A��7A��;A�|�A�;dA|  A{oAz9XAy��AyG�Ax�HAw��At�Aq��ApjAl��Ahr�Ae��Ac7LAb5?Aa��Aa�#Aa�^Aa��Aa
=A_XAZ  AW�AV�/AU��AT��AQ|�AN�jAN=qAM/AI�AH5?AGoAE�ADv�AC+AA33A@  A>�\A=A<��A<A�A:VA9O�A8��A8�A7�A6�yA5�A2VA1K�A0M�A/`BA.-A-�TA,5?A+?}A)�PA(�\A'�FA&�A$JA#"�A"��A"z�A!�PA �RA bA�uA+A&�A�A`BAAv�A��AhsA
=AVA�#AA�FA�A?}A�DA�hA��A��Ap�A-AoA
jA	�7A��A�/A�RA �A�A�A�9Ax�AVA��A�\A��AA ��A z�@���@�n�@��j@�;d@��@���@�v�@�5?@��T@���@@웦@�@�hs@��@�t�@噚@��@�7@ݙ�@��;@���@ڸR@� �@�@�j@�
=@�p�@�z�@�
=@��@�V@�A�@˶F@�33@��@�=q@��T@�p�@���@�Ĝ@Ȭ@ȋD@ǍP@�V@��@�hs@ģ�@�bN@� �@î@���@�^5@��#@���@��
@�o@�^5@���@���@�Z@�9X@�1@���@�o@��@�V@��@���@�\)@���@�ff@���@��@���@�@�$�@��^@��@��
@�dZ@�ȴ@�n�@�5?@��@��#@���@�hs@��@��/@��@�9X@��@�1@��w@���@�@���@�I�@�1@��;@�t�@��!@�E�@���@�?}@��@�Ĝ@���@�Q�@�  @��F@��P@�t�@�dZ@�K�@�S�@�l�@�C�@�o@�ȴ@���@�ff@�5?@�M�@�ff@�E�@�@�hs@�O�@��@�z�@�Q�@�1@�b@�  @���@�S�@�@��H@��!@���@�v�@�ff@��-@�5?@�M�@�$�@���@��T@��^@�hs@�X@��@��9@�z�@�Z@�bN@��
@���@���@�|�@�+@�o@���@��H@��H@��H@��@���@��@��H@��H@���@�ff@�-@��@��T@��^@��^@��-@�x�@�z�@��
@��;@���@�S�@�C�@�C�@���@���@�ƨ@���@�S�@�K�@�C�@���@��R@�ff@�$�@��#@�@��@�`B@�&�@��j@�A�@��m@���@��
@�ƨ@��@���@��P@�C�@�o@���@��@�ȴ@��!@���@�^5@��@���@���@���@�G�@�&�@��@���@���@���@��@�K�@�33@��H@�ȴ@���@���@�V@�J@��@��#@���@�O�@�&�@�7L@�O�@�G�@��@��D@�Q�@��F@��m@��@��
@��F@��P@�t�@�\)@�+@�ȴ@�v�@�5?@���@�@��h@�&�@���@�1@�K�@�
=@��R@�~�@�V@�J@���@�X@�7L@��@��`@��/@���@��@���@�b@���@��
@��w@��@�;d@�ȴ@���@�ff@�-@��@��h@�X@�O�@�&�@���@��@�j@�Z@�9X@�j@��@K�@~E�@}�@}�T@}�T@|�@{��@{t�@{@y�#@xĜ@x�u@x �@w|�@w�@v��@v��@w
=@w�@v��@u��@uO�@tI�@t�@s�@r�H@r~�@rM�@q��@q�7@qhs@qG�@q&�@p�`@pbN@p  @o�w@o��@o�P@o;d@nff@n{@m��@m`B@mV@l�@l1@kt�@kC�@k@j��@j^5@jJ@i�@i��@iG�@i%@hĜ@h�9@h�@hbN@h �@g�;@g�@g��@g�P@g;d@g
=@f��@f�y@f��@fE�@e�-@ep�@eO�@d��@dI�@d�@c��@c��@c�@ct�@b�@b��@bn�@b=q@a��@a��@a�^@a��@aX@`��@`��@`�u@`�@`�@`bN@`b@_�w@_�@_;d@_
=@^ȴ@^@]p�@]�@\��@\�@\�D@\(�@[�F@Z�!@Z�@Y�#@Y�7@Y&�@XĜ@XA�@W�;@W\)@V��@VE�@V$�@U@Up�@UV@T��@T�@S��@S�
@S�F@S��@S33@R�H@R��@R^5@R-@Q��@Q7L@Q%@PĜ@PbN@O��@OK�@N��@N�R@Nv�@Nff@NE�@N@M�-@M�@M`B@M�@L��@L�j@L��@LZ@L�@KC�@J��@J�!@J��@J�\@JM�@I�#@I��@Ix�@I7L@H�`@H1'@Gl�@F�y@F��@F�+@FV@F{@E��@EV@D�/@D�j@Dz�@D9X@D(�@D(�@D�@C��@C�F@CS�@C"�@B��@B�@A��@Ahs@AG�@A�@@�9@@bN@?�@?\)@?+@>�y@>��@>ff@>V@>5?@=�T@=�T@=@=p�@=`B@=?}@<��@<�@<j@<�@;��@;�F@;t�@;"�@;@:�@:�H@:�\@:M�@9�@9��@9x�@9G�@8�`@8Ĝ@8�@8A�@8  @7��@7�P@7K�@7�@6��@6�@6��@6v�@65?@6@5�@5�T@5�-@5p�@5`B@5�@4��@4�@4��@4z�@4j@4Z@4Z@49X@4(�@4�@3��@3�F@3�@3C�@2�@2~�@2-@1��@1�@1�#@1�^@1��@1x�@1G�@1&�@1&�@1%@0�u@0bN@0Q�@0Q�@0A�@0 �@0  @/��@/l�@/+@/�@.�y@.�R@.v�@.@-��@-��@-O�@-?}@,��@,z�@,I�@,(�@+ƨ@+33@+@*��@*~�@*^5@*=q@)�@)hs@)%@(Ĝ@(��@(�u@(bN@(1'@(b@'�w@'|�@';d@'
=@&�y@&��@&E�@&5?@&{@%��@%�-@%��@%�@%O�@%/@%V@$��@$Z@$1@#ƨ@#��@#C�@#"�@"�@"��@"M�@!�@!�#@!�^@!��@!�7@!G�@!%@ �@ b@�@��@|�@+@��@�y@�y@�R@v�@E�@$�@�@��@V@z�@j@Z@Z@I�@I�@1@�
@ƨ@�F@��@��@dZ@"�@@@�H@~�@�@��@�7@&�@��@�u@ �@�w@l�@�@�@��@��@�+@E�@{@�T@��@?}@/@/@��@�j@�D@j@I�@I�@(�@�@��@�
@�F@��@t�@S�@"�@�@��@�!@��@�\@n�@^5@J@��@��@�7@�7@hs@7L@��@�u@bN@ �@��@�@l�@;d@+@�@
=@ȴ@��@��@�+@ff@E�@$�@�@��@�-@�@?}@?}@?}@/@�@�@�@��@Z@(�@1@��@��@�m@ƨ@�
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�{A�{A�oA�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�bA�-A�x�A�AŬA�ffA�C�A�=qA�+A��A���A���Aĩ�A�p�A�XA�A�A�"�A�JA�
=A�
=A��A��Aç�A�VA�  A���A\A�z�A�bNA��hA��jA��RA���A�/A��9A�XA�VA�(�A���A��A���A�A��jA���A�^5A�A�Q�A�5?A�VA�bA�9XA��A�^5A��A�A�A�  A�VA�(�A�bNA���A�+A�bA��A��A��A���A�O�A��RA���A��A�A���A���A�x�A�jA�\)A�5?A�A��^A�p�A��7A��;A�|�A�;dA|  A{oAz9XAy��AyG�Ax�HAw��At�Aq��ApjAl��Ahr�Ae��Ac7LAb5?Aa��Aa�#Aa�^Aa��Aa
=A_XAZ  AW�AV�/AU��AT��AQ|�AN�jAN=qAM/AI�AH5?AGoAE�ADv�AC+AA33A@  A>�\A=A<��A<A�A:VA9O�A8��A8�A7�A6�yA5�A2VA1K�A0M�A/`BA.-A-�TA,5?A+?}A)�PA(�\A'�FA&�A$JA#"�A"��A"z�A!�PA �RA bA�uA+A&�A�A`BAAv�A��AhsA
=AVA�#AA�FA�A?}A�DA�hA��A��Ap�A-AoA
jA	�7A��A�/A�RA �A�A�A�9Ax�AVA��A�\A��AA ��A z�@���@�n�@��j@�;d@��@���@�v�@�5?@��T@���@@웦@�@�hs@��@�t�@噚@��@�7@ݙ�@��;@���@ڸR@� �@�@�j@�
=@�p�@�z�@�
=@��@�V@�A�@˶F@�33@��@�=q@��T@�p�@���@�Ĝ@Ȭ@ȋD@ǍP@�V@��@�hs@ģ�@�bN@� �@î@���@�^5@��#@���@��
@�o@�^5@���@���@�Z@�9X@�1@���@�o@��@�V@��@���@�\)@���@�ff@���@��@���@�@�$�@��^@��@��
@�dZ@�ȴ@�n�@�5?@��@��#@���@�hs@��@��/@��@�9X@��@�1@��w@���@�@���@�I�@�1@��;@�t�@��!@�E�@���@�?}@��@�Ĝ@���@�Q�@�  @��F@��P@�t�@�dZ@�K�@�S�@�l�@�C�@�o@�ȴ@���@�ff@�5?@�M�@�ff@�E�@�@�hs@�O�@��@�z�@�Q�@�1@�b@�  @���@�S�@�@��H@��!@���@�v�@�ff@��-@�5?@�M�@�$�@���@��T@��^@�hs@�X@��@��9@�z�@�Z@�bN@��
@���@���@�|�@�+@�o@���@��H@��H@��H@��@���@��@��H@��H@���@�ff@�-@��@��T@��^@��^@��-@�x�@�z�@��
@��;@���@�S�@�C�@�C�@���@���@�ƨ@���@�S�@�K�@�C�@���@��R@�ff@�$�@��#@�@��@�`B@�&�@��j@�A�@��m@���@��
@�ƨ@��@���@��P@�C�@�o@���@��@�ȴ@��!@���@�^5@��@���@���@���@�G�@�&�@��@���@���@���@��@�K�@�33@��H@�ȴ@���@���@�V@�J@��@��#@���@�O�@�&�@�7L@�O�@�G�@��@��D@�Q�@��F@��m@��@��
@��F@��P@�t�@�\)@�+@�ȴ@�v�@�5?@���@�@��h@�&�@���@�1@�K�@�
=@��R@�~�@�V@�J@���@�X@�7L@��@��`@��/@���@��@���@�b@���@��
@��w@��@�;d@�ȴ@���@�ff@�-@��@��h@�X@�O�@�&�@���@��@�j@�Z@�9X@�j@��@K�@~E�@}�@}�T@}�T@|�@{��@{t�@{@y�#@xĜ@x�u@x �@w|�@w�@v��@v��@w
=@w�@v��@u��@uO�@tI�@t�@s�@r�H@r~�@rM�@q��@q�7@qhs@qG�@q&�@p�`@pbN@p  @o�w@o��@o�P@o;d@nff@n{@m��@m`B@mV@l�@l1@kt�@kC�@k@j��@j^5@jJ@i�@i��@iG�@i%@hĜ@h�9@h�@hbN@h �@g�;@g�@g��@g�P@g;d@g
=@f��@f�y@f��@fE�@e�-@ep�@eO�@d��@dI�@d�@c��@c��@c�@ct�@b�@b��@bn�@b=q@a��@a��@a�^@a��@aX@`��@`��@`�u@`�@`�@`bN@`b@_�w@_�@_;d@_
=@^ȴ@^@]p�@]�@\��@\�@\�D@\(�@[�F@Z�!@Z�@Y�#@Y�7@Y&�@XĜ@XA�@W�;@W\)@V��@VE�@V$�@U@Up�@UV@T��@T�@S��@S�
@S�F@S��@S33@R�H@R��@R^5@R-@Q��@Q7L@Q%@PĜ@PbN@O��@OK�@N��@N�R@Nv�@Nff@NE�@N@M�-@M�@M`B@M�@L��@L�j@L��@LZ@L�@KC�@J��@J�!@J��@J�\@JM�@I�#@I��@Ix�@I7L@H�`@H1'@Gl�@F�y@F��@F�+@FV@F{@E��@EV@D�/@D�j@Dz�@D9X@D(�@D(�@D�@C��@C�F@CS�@C"�@B��@B�@A��@Ahs@AG�@A�@@�9@@bN@?�@?\)@?+@>�y@>��@>ff@>V@>5?@=�T@=�T@=@=p�@=`B@=?}@<��@<�@<j@<�@;��@;�F@;t�@;"�@;@:�@:�H@:�\@:M�@9�@9��@9x�@9G�@8�`@8Ĝ@8�@8A�@8  @7��@7�P@7K�@7�@6��@6�@6��@6v�@65?@6@5�@5�T@5�-@5p�@5`B@5�@4��@4�@4��@4z�@4j@4Z@4Z@49X@4(�@4�@3��@3�F@3�@3C�@2�@2~�@2-@1��@1�@1�#@1�^@1��@1x�@1G�@1&�@1&�@1%@0�u@0bN@0Q�@0Q�@0A�@0 �@0  @/��@/l�@/+@/�@.�y@.�R@.v�@.@-��@-��@-O�@-?}@,��@,z�@,I�@,(�@+ƨ@+33@+@*��@*~�@*^5@*=q@)�@)hs@)%@(Ĝ@(��@(�u@(bN@(1'@(b@'�w@'|�@';d@'
=@&�y@&��@&E�@&5?@&{@%��@%�-@%��@%�@%O�@%/@%V@$��@$Z@$1@#ƨ@#��@#C�@#"�@"�@"��@"M�@!�@!�#@!�^@!��@!�7@!G�@!%@ �@ b@�@��@|�@+@��@�y@�y@�R@v�@E�@$�@�@��@V@z�@j@Z@Z@I�@I�@1@�
@ƨ@�F@��@��@dZ@"�@@@�H@~�@�@��@�7@&�@��@�u@ �@�w@l�@�@�@��@��@�+@E�@{@�T@��@?}@/@/@��@�j@�D@j@I�@I�@(�@�@��@�
@�F@��@t�@S�@"�@�@��@�!@��@�\@n�@^5@J@��@��@�7@�7@hs@7L@��@�u@bN@ �@��@�@l�@;d@+@�@
=@ȴ@��@��@�+@ff@E�@$�@�@��@�-@�@?}@?}@?}@/@�@�@�@��@Z@(�@1@��@��@�m@ƨ@�
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�B�!B�B�B��B��B�BDB!�B+B1'B33B1'B2-B49B33B33B7LB;dB=qB=qB?}B?}B=qBA�BA�BC�BK�BW
B[#B`BB\)BQ�BO�BW
BjBS�BQ�BD�B�BK�B@�B:^B)�B{BoB�B#�B�B�BhB�BoBB�sB�TB�;B�)B��BɺB�^B�!B��By�BdZB]/BVBF�B-B�BB
�B
�B
��B
�uB
��B
��B
��B
��B
��B
�DB
jB
VB
7LB
J�B
D�B
+B	��B
�B
�B
bB
VB
+B	�B	��B	�RB	�^B	��B	�B	�+B	�B	�%B	�VB	�PB	�1B	� B	o�B	P�B	�B	%�B	33B	'�B	�B	B�B	+B��B�B�5B�NB�B��BĜB�^B�RB�LB�jB�LB�FB�B��B�3B�B�!B��B��B�B��B��B�uB�bB�{B�+B�1B� B�B�Bt�Bm�Bv�B�B|�Bt�Bq�Bn�BaHB]/BW
B^5BdZBffBdZB]/Be`BbNB]/B`BBdZBbNBVBC�BN�BC�B>wB2-B;dB:^BC�BN�BL�BO�BXBR�BL�B@�BF�BJ�BI�BXBYBW
BN�BS�B^5B[#BR�BZBN�B=qBL�B`BBiyBgmBdZB]/BW
BYB^5BaHBiyBaHB]/BW
B[#BL�B[#BaHBbNBQ�BN�BZB[#B]/BdZBe`BjBn�Bt�By�B{�B}�B|�B� B�B�B�B�B�B{�B~�B�1B�=B�=B�hB�hB�bB�\B�hB�{B�hB��B��B��B��B��B�B�'B�'B�B�B�B�-B�^B�qB��BÖBȴBȴBƨBȴB�B�#B�NB�TB�ZB�B��B��B��B��B	B	B	B	B	1B	DB	VB	hB	{B	{B	uB	�B	�B	$�B	,B	.B	2-B	49B	7LB	8RB	9XB	@�B	C�B	D�B	F�B	F�B	I�B	K�B	M�B	O�B	P�B	R�B	YB	YB	\)B	]/B	_;B	aHB	bNB	dZB	ffB	ffB	cTB	e`B	k�B	n�B	o�B	u�B	w�B	z�B	|�B	z�B	{�B	{�B	~�B	� B	�B	�B	�+B	�%B	�JB	�bB	�\B	�oB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�3B	�9B	�?B	�RB	�RB	�RB	�RB	�wB	�}B	�}B	�wB	��B	ÖB	ÖB	ŢB	ŢB	ƨB	ŢB	ÖB	�}B	�}B	ŢB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�B	�/B	�NB	�TB	�ZB	�ZB	�`B	�ZB	�TB	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
%B
%B
B
B
B
%B
B
B
B
B
B
B
B
B
B
  B
B
B
%B
1B
1B
	7B
1B
1B

=B
JB
PB
JB
PB
\B
DB
\B
{B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
 �B
�B
 �B
!�B
"�B
!�B
 �B
�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
'�B
(�B
)�B
)�B
(�B
'�B
'�B
)�B
)�B
)�B
'�B
,B
,B
,B
-B
-B
+B
-B
-B
-B
-B
.B
/B
.B
.B
-B
/B
0!B
0!B
0!B
/B
/B
/B
0!B
/B
/B
/B
.B
/B
1'B
2-B
2-B
1'B
0!B
0!B
/B
2-B
33B
33B
33B
33B
33B
49B
33B
33B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
9XB
9XB
9XB
9XB
8RB
8RB
9XB
9XB
9XB
8RB
9XB
;dB
:^B
:^B
9XB
;dB
<jB
=qB
=qB
>wB
>wB
=qB
=qB
>wB
?}B
>wB
?}B
?}B
?}B
>wB
>wB
=qB
?}B
A�B
A�B
A�B
@�B
@�B
B�B
A�B
A�B
@�B
?}B
?}B
B�B
C�B
D�B
D�B
C�B
C�B
C�B
E�B
F�B
F�B
F�B
G�B
H�B
G�B
G�B
F�B
F�B
G�B
F�B
F�B
G�B
H�B
I�B
I�B
H�B
H�B
G�B
I�B
J�B
J�B
J�B
K�B
L�B
K�B
K�B
L�B
L�B
K�B
L�B
L�B
L�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
M�B
N�B
M�B
N�B
N�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
R�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
S�B
S�B
S�B
T�B
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
W
B
W
B
W
B
W
B
VB
W
B
XB
XB
XB
XB
W
B
W
B
XB
XB
YB
YB
YB
XB
XB
ZB
ZB
YB
ZB
YB
ZB
[#B
[#B
ZB
YB
\)B
\)B
\)B
]/B
\)B
\)B
[#B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
`BB
`BB
aHB
bNB
bNB
bNB
cTB
cTB
bNB
cTB
cTB
e`B
e`B
e`B
dZB
dZB
dZB
cTB
dZB
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
ffB
ffB
ffB
iyB
jB
jB
jB
iyB
iyB
iyB
jB
jB
jB
jB
iyB
iyB
jB
jB
jB
hsB
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
l�B
l�B
n�B
n�B
n�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
p�B
p�B
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
r�B
r�B
r�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
u�B
v�B
w�B
w�B
x�B
x�B
x�B
y�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�)B�B�B�B�B�5B�B�5B�5B�B�B�5B�B�5B�B�B�B�5B�B�5B�5B�B�!B�;B�5B�;B�OB��B�KB�MB�B�B"NB+6B1[B3�B1[B2|B4�B3�B3�B7�B;�B=�B=�B?�B?�B=�BA�BB'BDgBL�BW�B[�B`�B\�BS�BQ�BYBl�BXBU�BJ#B%�BN"BDB>]B.�B�B2B�B%`B�B�BB�hB�B�B�CB�8B�4B�BңB˒B�B�|B�4B~]Bh�B_�BX_BIB0�B B�B
��B
��B
��B
�$B
��B
�,B
�&B
�B
�KB
�~B
m�B
YKB
;B
LJB
F?B
./B	�}B
�B
�B
NB
�B
1B	��B	�sB	�B	�"B	��B	�7B	�rB	��B	�+B	��B	��B	��B	��B	qB	S�B	 �B	(XB	4�B	)�B	 �B		B�B	1B��B�B�vB��B�B՛BƨB��B��B�$B��B��B��B�]B�yB�B��B�B��B��B�B�
B��B��B�B��B�lB��B�AB��B��Bw2Bp!Bw�B��B}�Bv+Br�Bo�BcnB_VBYB_�Be`BgBe,B^�Be�Bc B^OB`�Bd�Bb�BWsBE�BO�BE9B@OB4�B=VB<PBEBO�BM�BP�BXEBS�BM�BB�BH�BK�BKDBX�BY�BW�BPBT�B^�B[�BTaBZ�BP�BA BN�Ba-Bi�Bg�Bd�B^OBX�BZ�B_VBb�BjKBb�B^�BYB\�BO\B\]Ba�Bb�BS�BP�B[	B\]B^OBe,Bf�BkQBoiButBz^B|PB~]B}qB�iB��B��B�gB�mB�aB|�B�B��B��B��B��B��B��B�B��B�B�TB�QB�OB�|B�zB�zB�wB�AB�[B��B��B� B�B��B��B�B�3B�B�7B�zB��B�yB��B��B�B�`B��B�%B�DB�(B�HB	'B	[B	{B	�B	�B	�B	�B	�B	�B	�B	,B	mB	xB	%FB	,=B	.IB	2�B	4�B	7�B	8�B	9�B	@�B	C�B	D�B	F�B	F�B	J	B	K�B	NB	PB	QB	S&B	Y1B	YKB	\xB	]~B	_�B	a|B	b�B	dtB	f�B	f�B	c�B	e�B	k�B	n�B	p!B	u�B	xB	z�B	}B	{JB	|B	|PB	.B	�4B	�GB	�SB	�EB	��B	�B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�>B	�CB	�5B	�UB	�oB	�MB	�nB	�tB	�lB	�RB	��B	��B	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�OB	��B	żB	��B	��B	��B	��B	ϫB	��B	�B	�B	�4B	�&B	�B	�[B	�2B	�gB	�YB	�YB	�QB	�eB	�kB	�kB	�B	چB	�~B	�hB	�B	�B	�B	�B	�tB	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	�"B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�(B	�6B	�PB
 4B	�]B
%B
?B
SB
SB
SB
YB
SB
MB
{B
gB
MB
SB
MB
gB
�B
 �B
{B
�B
YB
�B
�B
	�B
fB
�B

�B
dB
�B
~B
jB
vB
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
B
B
�B
�B
�B
B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
B
�B
�B
�B
�B
 �B
!�B
 �B
 �B
 B
 �B
!�B
#B
!�B
!B
 'B
#B
#B
#�B
$&B
$&B
$B
$&B
&B
%�B
&B
&B
%�B
'B
'B
&B
'B
($B
(
B
($B
($B
($B
)B
)*B
)*B
)B
(
B
)*B
*B
*B
)B
($B
(>B
*0B
*0B
*0B
(>B
,=B
,=B
,=B
-CB
-CB
+6B
-CB
-)B
-)B
-)B
.IB
/B
.IB
./B
-]B
/OB
0!B
0;B
0;B
/OB
/5B
/OB
0;B
/iB
/5B
/OB
.}B
/OB
1[B
2aB
2aB
1vB
0oB
0oB
/�B
2|B
3hB
3MB
3�B
3hB
3�B
4�B
3hB
3�B
5tB
6zB
6zB
6zB
6zB
6�B
7�B
9rB
9�B
9rB
9rB
8�B
8lB
9�B
9rB
9rB
8�B
9�B
;�B
:�B
:�B
9�B
;�B
<�B
=�B
=�B
>�B
>�B
=�B
=�B
>�B
?�B
>�B
?�B
?�B
?�B
>�B
>�B
=�B
?�B
A�B
A�B
A�B
@�B
@�B
B�B
A�B
A�B
@�B
?�B
?�B
B�B
C�B
D�B
D�B
C�B
C�B
C�B
E�B
F�B
F�B
F�B
G�B
H�B
G�B
G�B
F�B
F�B
G�B
F�B
F�B
G�B
H�B
I�B
I�B
H�B
IB
G�B
I�B
J�B
J�B
J�B
K�B
L�B
K�B
K�B
L�B
MB
K�B
L�B
L�B
L�B
LB
MB
MB
NB
NB
NB
NB
OB
N�B
OB
NB
OB
N"B
OB
N�B
N�B
N�B
PB
OB
PB
O�B
PB
PB
QB
Q B
R B
RB
R B
R B
R B
RB
SB
S&B
S&B
S&B
S&B
SB
T,B
T,B
T,B
S&B
UB
UB
UB
U2B
UB
U2B
U2B
T,B
TB
T,B
TB
TFB
U2B
V9B
W$B
W$B
W?B
W?B
W$B
W$B
W?B
W$B
W?B
VSB
W?B
X+B
X+B
X+B
X+B
W?B
W$B
X+B
X+B
YKB
Y1B
Y1B
XEB
X_B
ZQB
ZQB
Y1B
ZQB
YKB
Z7B
[WB
[WB
ZkB
YeB
\CB
\CB
\CB
]dB
\CB
\]B
[WB
\]B
]dB
^OB
^jB
^OB
^OB
^OB
^jB
^OB
^jB
_pB
_pB
_pB
_VB
`\B
`\B
`vB
abB
abB
a|B
abB
abB
a|B
`vB
`�B
a|B
b�B
b�B
b�B
c�B
cnB
b�B
c�B
c�B
e`B
e�B
e`B
d�B
dtB
d�B
c�B
d�B
ezB
e�B
f�B
f�B
f�B
g�B
gmB
g�B
f�B
g�B
g�B
g�B
f�B
f�B
f�B
iyB
j�B
jB
j�B
i�B
i�B
i�B
jB
j�B
j�B
jB
i�B
i�B
j�B
j�B
j�B
h�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
l�B
l�B
n�B
n�B
n�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
p�B
p�B
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
r�B
r�B
r�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
u�B
v�B
w�B
w�B
x�B
x�B
x�B
y�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111131111111111131111111111113331111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.15(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802020036302018020200363020180202003630201806221325332018062213253320180622132533201804050729012018040507290120180405072901  JA  ARFMdecpA19c                                                                20180129093520  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180129003540  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180129003542  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180129003543  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180129003543  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180129003543  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180129003543  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180129003543  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180129003544  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180129003544                      G�O�G�O�G�O�                JA  ARUP                                                                        20180129005621                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180129153334  CV  JULD            G�O�G�O�F�A�                JM  ARSQJMQC2.0                                                                 20180131000000  CF  PSAL_ADJUSTED_QCCV  C�  G�O�                JM  ARCAJMQC2.0                                                                 20180201153630  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180201153630  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222901  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042533  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                