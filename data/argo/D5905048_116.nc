CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-05-07T00:35:30Z creation;2017-05-07T00:35:33Z conversion to V3.1;2019-12-19T08:08:20Z update;     
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
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΄   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170507003530  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               tA   JA  I2_0577_116                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�tk$�1   @�uW; @2��_o��dzd��7�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� DzfDz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�33@�  A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_  BgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bó3B��fB˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B��3B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDp D�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1� D2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK�fDLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\vfD\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDcvfDc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy��DzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~vfD~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�~fD��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�{3Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�x D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D�� D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AҋDA�l�A�ffA�ffA�bNA�`BA�^5A�\)A�ZA�S�A�S�A�VA�S�A�VA�M�A�G�A�1'A�+A�JA���A�z�A�VA� �A�p�A�ĜA�(�A�A�1A̶FA��TA��#A̮A�p�A�;dA���A˸RA˝�A˛�Aˣ�A�p�A�"�A���Aʰ!A�z�A�~�A�JAɸRAɧ�A���A�E�A��#Aɉ7A��;A�$�AǼjAǏ\AǍPA�1AƼjAƋDA�=qA�ĜA�ƨA��yAÏ\A�A�M�A��^A�M�A�&�A�=qA�1'A�ĜA�l�A���A��wA��A�I�A��^A��A��wA�O�A��7A��RA�^5A��yA�VA�$�A��/A��DA��A�ZA�?}A��A��A��+A�z�A���A���A���A��-A��A�jA��yA�&�A���A�A���A��A���A�+A��^A��DA�`BA��A�G�A�ĜA��A��jA��A�I�A�Q�A~�A|1Ayx�Aw
=Asx�AsVAq�^Al�yAj�AiC�AfE�Ab��Aa�A`��A^��A[�AY+AW`BAU��AQG�AO�PAM��AK�AHr�AF�jAEVAC�AA��A>��A=\)A<1'A:ZA9�A933A6�DA4z�A3�PA2��A1��A/��A.jA,�RA+��A)��A)��A)l�A(jA&��A%33A$��A$ĜA#�A#&�A!t�A n�Av�A�AS�A�HAr�A��A1A�A~�AA�A`BAA�A�A;dA�yA-A�FAC�A��AjA7LAZA�AoAz�A�A��A
�`A	��A	XA(�AXA��AffA;dA�-AjA�^A �HA 9X@��@��R@��7@�r�@���@���@��`@�@��@�V@�D@�@�|�@��;@�(�@�~�@��@��@�^5@�R@��#@�j@�$�@�G�@��@���@�%@��@�@�@�\@�1@���@�w@�F@�1@���@�@�C�@�n�@�n�@��@݉7@�ƨ@�"�@�o@�~�@��@���@�?}@�1@�n�@Л�@���@�hs@�E�@ӍP@�o@Ұ!@�~�@��@Ѻ^@ЋD@�\)@�"�@ͺ^@�G�@�r�@ˮ@��@�{@��#@�Ĝ@�"�@Ɨ�@ř�@ģ�@î@�
=@§�@��7@��@��@��F@���@��T@�x�@�V@���@�ƨ@�C�@���@��@�p�@���@��j@���@�+@��R@�V@�E�@��@�J@�hs@�z�@�I�@�Q�@�r�@��@�t�@�o@��R@���@���@���@�`B@��@�bN@��F@�+@��y@��!@�5?@�x�@�Ĝ@�9X@���@��+@�5?@��#@�hs@�V@��/@���@��F@���@�A�@��9@���@���@�{@���@���@�Q�@��@�l�@�K�@�;d@��@��H@�ȴ@�~�@���@��@�G�@�7L@�G�@���@��u@��D@�Z@�I�@�9X@��m@�dZ@���@���@��!@���@�-@�v�@�33@�E�@��-@�p�@���@��T@�@��#@���@��T@���@���@��@�9X@�ƨ@��F@�9X@�b@��w@��w@��@�33@��@��!@��R@�n�@�^5@���@��@�&�@���@�Ĝ@��@��@�&�@�/@��@���@���@��@�b@� �@���@��@���@�n�@��@���@�J@�5?@�5?@���@��@�&�@���@��D@� �@��P@��@��@�33@���@��!@��y@��@�@��@��H@���@���@�V@��@��7@�O�@��@���@��/@�Ĝ@��@�j@�I�@�b@��;@��@��P@�\)@�@���@��\@�M�@�J@���@�O�@��9@��u@��@�r�@�Z@��m@�l�@�"�@���@��H@��@��R@�E�@�-@�J@��@�@�O�@��/@��D@�Q�@� �@��@�ƨ@���@�+@��y@�ȴ@�ff@��T@��@��@�Ĝ@��u@�j@�Q�@�1'@��@��@���@�C�@���@���@���@���@�v�@�^5@�-@��T@���@�O�@�%@���@��@�Z@� �@�w@~��@~{@}��@|��@|�@|I�@|�@{�m@{t�@{o@{@z�@zn�@z=q@y��@y��@yX@y�@x�9@xb@w��@w+@v�R@v$�@v{@u��@up�@u`B@u?}@t�@t�j@s��@s��@sdZ@s@r��@r��@r�!@r��@r^5@rJ@qhs@q%@p��@pbN@p �@o�@o\)@o+@nȴ@nv�@nE�@n$�@m�T@mO�@l�@l��@l(�@l1@k��@kS�@k"�@j��@jM�@j�@j�@i�@i��@i7L@h�`@h�9@h�u@hQ�@hb@g�@g\)@g+@f�R@f5?@e��@ep�@eV@d�j@dz�@d�@c��@ct�@cdZ@c33@co@c@b��@b~�@bn�@bJ@a��@a&�@`�`@`Q�@`b@`b@`  @_�w@_l�@_�@_
=@^��@^ȴ@^�+@]��@]p�@]/@]�@[�m@[33@[@Z�!@Z�@Y�#@Y%@X�@Xb@W�P@V�y@V��@V$�@U��@T��@Tz�@T9X@Sƨ@SS�@S33@So@R�@R��@Rn�@RJ@Q7L@P��@PĜ@PbN@P �@OK�@N�y@Nȴ@N��@N��@NV@N@M�@Lj@K��@K@J��@J-@I��@H��@H1'@G��@GK�@F�R@F{@F@E�h@E?}@EV@D��@D��@C�
@C�@C�@CS�@B��@B~�@BM�@B-@A�#@@�`@@1'@?�@?�w@?\)@?�@>�R@>{@=�@=��@=@=p�@<��@<Z@;��@;��@;"�@;@:��@:��@:=q@9��@9X@97L@9%@8�9@8��@8�@8A�@8b@7��@7��@7��@7l�@6�@6�+@6$�@5�T@5p�@5/@5V@4�/@4Z@4I�@4�@3�m@3��@3��@3o@2��@2~�@2M�@2�@2J@1�^@1X@0��@0��@0��@0�9@0�9@0��@0��@0�u@0r�@01'@/�@/��@/�P@/+@.�@.��@.v�@.E�@.$�@.$�@.{@-�T@-�-@-�h@-`B@-?}@,�@,�j@,z�@,I�@,(�@,1@+��@+��@+t�@+dZ@+S�@+@*�\@*n�@*^5@*-@)�@)��@)7L@)%@(��@(�9@(r�@(bN@(1'@(1'@'�@'��@'��@'\)@'K�@';d@'
=@&��@&�+@&v�@&v�@&E�@%@%p�@%/@$��@$��@$�j@$��@$��@$�D@$j@$9X@$�@#�m@#��@#o@"��@"��@"~�@"-@"J@!��@!�@!��@!�^@!x�@!X@!7L@ ��@ ��@ ��@ bN@ Q�@�@��@+@�R@�+@ff@V@{@�-@O�@?}@V@�/@�@j@�
@�F@t�@o@��@��@��@M�@�^@�7@x�@X@G�@&�@��@�u@1'@ �@  @�P@\)@�@��@ȴ@ff@V@V@V@{@�T@@�h@`B@/@V@��@�/@�D@I�@9X@�@1@�
@dZ@S�@33@�H@��@^5@�@��@�@�^@��@�7@G�@�@�9@bN@A�@  @�w@|�@l�@;d@
=@�y@��@��@ff@5?@�@�T@��@��@p�@O�@?}@/@�@��@��@�/@��@�@j@Z@9X@�@��@�m@��@dZ@o@
�H@
��@
�!@
�!@
��@
n�@
=q@
J@	�@	��@	�7@	x�@	hs@	X@	G�@	�@��@��@��@Ĝ@��@1'@  @�@�@�;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AҋDA�l�A�ffA�ffA�bNA�`BA�^5A�\)A�ZA�S�A�S�A�VA�S�A�VA�M�A�G�A�1'A�+A�JA���A�z�A�VA� �A�p�A�ĜA�(�A�A�1A̶FA��TA��#A̮A�p�A�;dA���A˸RA˝�A˛�Aˣ�A�p�A�"�A���Aʰ!A�z�A�~�A�JAɸRAɧ�A���A�E�A��#Aɉ7A��;A�$�AǼjAǏ\AǍPA�1AƼjAƋDA�=qA�ĜA�ƨA��yAÏ\A�A�M�A��^A�M�A�&�A�=qA�1'A�ĜA�l�A���A��wA��A�I�A��^A��A��wA�O�A��7A��RA�^5A��yA�VA�$�A��/A��DA��A�ZA�?}A��A��A��+A�z�A���A���A���A��-A��A�jA��yA�&�A���A�A���A��A���A�+A��^A��DA�`BA��A�G�A�ĜA��A��jA��A�I�A�Q�A~�A|1Ayx�Aw
=Asx�AsVAq�^Al�yAj�AiC�AfE�Ab��Aa�A`��A^��A[�AY+AW`BAU��AQG�AO�PAM��AK�AHr�AF�jAEVAC�AA��A>��A=\)A<1'A:ZA9�A933A6�DA4z�A3�PA2��A1��A/��A.jA,�RA+��A)��A)��A)l�A(jA&��A%33A$��A$ĜA#�A#&�A!t�A n�Av�A�AS�A�HAr�A��A1A�A~�AA�A`BAA�A�A;dA�yA-A�FAC�A��AjA7LAZA�AoAz�A�A��A
�`A	��A	XA(�AXA��AffA;dA�-AjA�^A �HA 9X@��@��R@��7@�r�@���@���@��`@�@��@�V@�D@�@�|�@��;@�(�@�~�@��@��@�^5@�R@��#@�j@�$�@�G�@��@���@�%@��@�@�@�\@�1@���@�w@�F@�1@���@�@�C�@�n�@�n�@��@݉7@�ƨ@�"�@�o@�~�@��@���@�?}@�1@�n�@Л�@���@�hs@�E�@ӍP@�o@Ұ!@�~�@��@Ѻ^@ЋD@�\)@�"�@ͺ^@�G�@�r�@ˮ@��@�{@��#@�Ĝ@�"�@Ɨ�@ř�@ģ�@î@�
=@§�@��7@��@��@��F@���@��T@�x�@�V@���@�ƨ@�C�@���@��@�p�@���@��j@���@�+@��R@�V@�E�@��@�J@�hs@�z�@�I�@�Q�@�r�@��@�t�@�o@��R@���@���@���@�`B@��@�bN@��F@�+@��y@��!@�5?@�x�@�Ĝ@�9X@���@��+@�5?@��#@�hs@�V@��/@���@��F@���@�A�@��9@���@���@�{@���@���@�Q�@��@�l�@�K�@�;d@��@��H@�ȴ@�~�@���@��@�G�@�7L@�G�@���@��u@��D@�Z@�I�@�9X@��m@�dZ@���@���@��!@���@�-@�v�@�33@�E�@��-@�p�@���@��T@�@��#@���@��T@���@���@��@�9X@�ƨ@��F@�9X@�b@��w@��w@��@�33@��@��!@��R@�n�@�^5@���@��@�&�@���@�Ĝ@��@��@�&�@�/@��@���@���@��@�b@� �@���@��@���@�n�@��@���@�J@�5?@�5?@���@��@�&�@���@��D@� �@��P@��@��@�33@���@��!@��y@��@�@��@��H@���@���@�V@��@��7@�O�@��@���@��/@�Ĝ@��@�j@�I�@�b@��;@��@��P@�\)@�@���@��\@�M�@�J@���@�O�@��9@��u@��@�r�@�Z@��m@�l�@�"�@���@��H@��@��R@�E�@�-@�J@��@�@�O�@��/@��D@�Q�@� �@��@�ƨ@���@�+@��y@�ȴ@�ff@��T@��@��@�Ĝ@��u@�j@�Q�@�1'@��@��@���@�C�@���@���@���@���@�v�@�^5@�-@��T@���@�O�@�%@���@��@�Z@� �@�w@~��@~{@}��@|��@|�@|I�@|�@{�m@{t�@{o@{@z�@zn�@z=q@y��@y��@yX@y�@x�9@xb@w��@w+@v�R@v$�@v{@u��@up�@u`B@u?}@t�@t�j@s��@s��@sdZ@s@r��@r��@r�!@r��@r^5@rJ@qhs@q%@p��@pbN@p �@o�@o\)@o+@nȴ@nv�@nE�@n$�@m�T@mO�@l�@l��@l(�@l1@k��@kS�@k"�@j��@jM�@j�@j�@i�@i��@i7L@h�`@h�9@h�u@hQ�@hb@g�@g\)@g+@f�R@f5?@e��@ep�@eV@d�j@dz�@d�@c��@ct�@cdZ@c33@co@c@b��@b~�@bn�@bJ@a��@a&�@`�`@`Q�@`b@`b@`  @_�w@_l�@_�@_
=@^��@^ȴ@^�+@]��@]p�@]/@]�@[�m@[33@[@Z�!@Z�@Y�#@Y%@X�@Xb@W�P@V�y@V��@V$�@U��@T��@Tz�@T9X@Sƨ@SS�@S33@So@R�@R��@Rn�@RJ@Q7L@P��@PĜ@PbN@P �@OK�@N�y@Nȴ@N��@N��@NV@N@M�@Lj@K��@K@J��@J-@I��@H��@H1'@G��@GK�@F�R@F{@F@E�h@E?}@EV@D��@D��@C�
@C�@C�@CS�@B��@B~�@BM�@B-@A�#@@�`@@1'@?�@?�w@?\)@?�@>�R@>{@=�@=��@=@=p�@<��@<Z@;��@;��@;"�@;@:��@:��@:=q@9��@9X@97L@9%@8�9@8��@8�@8A�@8b@7��@7��@7��@7l�@6�@6�+@6$�@5�T@5p�@5/@5V@4�/@4Z@4I�@4�@3�m@3��@3��@3o@2��@2~�@2M�@2�@2J@1�^@1X@0��@0��@0��@0�9@0�9@0��@0��@0�u@0r�@01'@/�@/��@/�P@/+@.�@.��@.v�@.E�@.$�@.$�@.{@-�T@-�-@-�h@-`B@-?}@,�@,�j@,z�@,I�@,(�@,1@+��@+��@+t�@+dZ@+S�@+@*�\@*n�@*^5@*-@)�@)��@)7L@)%@(��@(�9@(r�@(bN@(1'@(1'@'�@'��@'��@'\)@'K�@';d@'
=@&��@&�+@&v�@&v�@&E�@%@%p�@%/@$��@$��@$�j@$��@$��@$�D@$j@$9X@$�@#�m@#��@#o@"��@"��@"~�@"-@"J@!��@!�@!��@!�^@!x�@!X@!7L@ ��@ ��@ ��@ bN@ Q�@�@��@+@�R@�+@ff@V@{@�-@O�@?}@V@�/@�@j@�
@�F@t�@o@��@��@��@M�@�^@�7@x�@X@G�@&�@��@�u@1'@ �@  @�P@\)@�@��@ȴ@ff@V@V@V@{@�T@@�h@`B@/@V@��@�/@�D@I�@9X@�@1@�
@dZ@S�@33@�H@��@^5@�@��@�@�^@��@�7@G�@�@�9@bN@A�@  @�w@|�@l�@;d@
=@�y@��@��@ff@5?@�@�T@��@��@p�@O�@?}@/@�@��@��@�/@��@�@j@Z@9X@�@��@�m@��@dZ@o@
�H@
��@
�!@
�!@
��@
n�@
=q@
J@	�@	��@	�7@	x�@	hs@	X@	G�@	�@��@��@��@Ĝ@��@1'@  @�@�@�;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
u�B
t�B
u�B
t�B
t�B
t�B
s�B
s�B
s�B
r�B
q�B
q�B
q�B
r�B
p�B
o�B
l�B
k�B
hsB
dZB
^5B
`BB
_;B
]/B
[#B
\)B
dZB
�DB
��B
�B
�B
�B
��B
�B
�B
ÖB
��B
�B
�;B
�NB
�NB
�ZB
�B
��BbB�B�B$�BE�Bs�Bs�B~�Bz�Bs�B�B�VB��B�B�'B�3B�FB�LBĜB��B��B��B�mB��B��B1B\B�B�B�B-B-B/B5?B;dB=qB9XB9XB<jB0!B-B)�B#�B�B
=B%B�BB�B;dB8RB33B�B\BB��B�;BB��B�JB�BcTB[#Bz�B�B�=By�BP�B%�BbBJB+B
��B
�B
�dB
��B
v�B
n�B
^5B
N�B
=qB
'�B
�B
B	��B	��B	�)B	��B	�dB	��B	�+B	�B	�+B	�B	p�B	^5B	[#B	S�B	=qB	0!B	(�B	"�B	�B	bB		7B	B��B�B�`B�NB�B�B��B��BĜB��B�wB�dB�?B�-B�B�B�B�B�B�!B�B��B��B�!B�qB�}BB��B�qB�LB�FB�?B�?B�?B�-B�B��B��B��B��B�7B�B� By�Bu�Br�Bo�Bn�Bk�BcTBcTBe`BiyBl�Bk�Bl�BgmBk�Be`BaHB`BB]/BR�BI�BF�BD�BE�BF�BF�BH�BK�BQ�BZB`BBdZBjBjBjBjBm�Br�Bw�B~�B�B�+B�JB�\B�uB�{B�{B�VB�=B�=B�DB�PB�oB�\B�PB��B��B�3B�3B�FB�jB��BĜBǮBɺBŢBɺB��B��B��B��B��B��B��BɺBƨBĜBÖB��B�#B�HB�B�B�B�B�B�B�B��B��B��B	B	+B	
=B	JB	VB	VB	oB	oB	oB	uB	uB	uB	oB	hB	\B	\B	\B	{B	�B	�B	�B	�B	!�B	#�B	%�B	$�B	$�B	%�B	'�B	+B	-B	-B	.B	0!B	33B	5?B	8RB	>wB	<jB	=qB	A�B	D�B	E�B	G�B	H�B	I�B	I�B	J�B	K�B	L�B	M�B	N�B	Q�B	Q�B	S�B	T�B	VB	YB	XB	YB	ZB	^5B	^5B	aHB	e`B	gmB	iyB	l�B	m�B	o�B	t�B	y�B	y�B	z�B	z�B	y�B	{�B	|�B	~�B	�B	�B	�%B	�+B	�=B	�JB	�VB	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�XB	�XB	�FB	�LB	�qB	�}B	B	ĜB	ƨB	ǮB	ǮB	ǮB	ȴB	ǮB	ɺB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�B	�B	�)B	�/B	�5B	�BB	�TB	�`B	�fB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B

=B
DB
DB
DB
JB
JB
JB
DB
DB
DB

=B

=B

=B
JB
JB
JB
PB
VB
\B
bB
bB
\B
\B
VB
\B
hB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
!�B
!�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
(�B
)�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
-B
-B
-B
-B
-B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
B�B
B�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
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
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
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
Q�B
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
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
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
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
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
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
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
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
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
q�B
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
r�B
s�B
s�B
s�B
s�B
t�B
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
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
vB
t�B
u�B
t�B
t�B
t�B
s�B
s�B
s�B
r�B
q�B
q�B
q�B
r�B
p�B
o�B
l�B
lB
iB
d�B
^�B
aB
`�B
^�B
\�B
^�B
fB
��B
��B
�B
�wB
��B
�B
��B
��B
��B
�B
�B
߾B
��B
�B
��B
�B
�"BNB/B�B$�BE�BtnBt�B�OB|6Bt�B�{B��B��B��B��B�9B��B�rB�YB��BοBԯB�eB�DB�wB
=BhB�B�B!�B/�B/iB3B8�B>]B>�B;JB="B?�B1[B.�B,=B&�B"4B6BB�BD3B<�B;�B7�B5B�BKB��B�B��B�fB�pB��BfB[#B{B�EB�(BHBU�B(>B4B�B
XBUB
�~B
��B
��B
x�B
q'B
`�B
RTB
@�B
+6B
/B
�B
�B	��B	��B	�jB	�.B	��B	��B	��B	�#B	�_B	s�B	`�B	^5B	X�B	@ B	2�B	,qB	%�B	�B	�B	^B	?B�B�B�8B�tB�=BٴB�$B�(B��B��B�iB��B�2B�nB��B��B��B��B��B�aB��B��B�tB�[B��B��B�3B��B�wB�B�B�+B��B�2B��B��B��B�mB��B�1B�XB��B�Bz�BvzBs�Bp�Bp;Bl�BdtBd&BffBjeBm]Bl�Bm�BhsBm)Bf�BbBabB^�BUBK�BG�BE�BF�BGzBGzBI�BL�BS@B[	BaBe�BkkBkBkBk6Bm�Br�BxB�4B�[B��B��B��B�aB��B��B��B�rB��B��B��B��B�vB��B��B��B��B�hB�`B��B��B�9B��B��B�?B�rB��B�PB�<B�{B��BӏBЗB��B��BżBðB�	BڠB��B��B�B�B�B�;B�iB�B�RB��B�xB	�B	�B	
�B	B	�B	BB	[B	�B	&B	,B	,B	B	B	TB	B	�B	�B	2B	$B	B	)B	!B	"hB	$ZB	&�B	%`B	%`B	&fB	(sB	+�B	-�B	-wB	.cB	0;B	3�B	5tB	8�B	>�B	<�B	=�B	A�B	EB	F?B	HB	IB	J#B	J	B	KB	L0B	MPB	NVB	O\B	RTB	R:B	TaB	U�B	V�B	Y�B	X�B	Y�B	Z�B	^�B	^�B	a�B	e�B	g�B	i�B	m)B	m�B	oiB	t�B	z�B	z�B	{dB	{dB	z�B	|jB	}<B	HB	�-B	�SB	�?B	�zB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�HB	� B	�B	��B	�B	�DB	��B	�B	��B	��B	�zB	�LB	��B	��B	ªB	ĜB	��B	��B	��B	�1B	�7B	�B	��B	��B	� B	�2B	�2B	�SB	�_B	�eB	�EB	�EB	�eB	�KB	��B	چB	�QB	�]B	�IB	�5B	�BB	�nB	�`B	�B	��B	�B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	��B	��B	�B	��B	��B	�B	��B	��B	�	B	�*B	�B	��B	�B
;B
 B
AB
AB
AB
aB
gB
�B
YB
zB
fB
fB
	lB
	�B

rB
xB
�B
xB
~B
dB
~B
�B
�B
�B

�B

rB

�B
�B
�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
 B
�B
 B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
"�B
"�B
$&B
%B
%B
%B
%B
&B
&2B
&B
&B
'B
'8B
'8B
($B
($B
($B
($B
($B
($B
($B
(XB
)*B
)*B
)*B
)B
*B
)*B
*0B
)B
*KB
*KB
+6B
+6B
+QB
+6B
+6B
+QB
,"B
,WB
-CB
-)B
-)B
-CB
-CB
.IB
.IB
/OB
/OB
/OB
/OB
0UB
0UB
0;B
0UB
0;B
0UB
0oB
1[B
1[B
1AB
1AB
1AB
2aB
2|B
2GB
2aB
2|B
3�B
3�B
3hB
3�B
4nB
4nB
4�B
5tB
5tB
5tB
5tB
5tB
5tB
5tB
5tB
6zB
5tB
6zB
6�B
6zB
6�B
7fB
7LB
7�B
8�B
8�B
8lB
8lB
8lB
8�B
8�B
8�B
9�B
9rB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:�B
:�B
:�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
;�B
;�B
;�B
;�B
;B
;B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
=�B
>�B
?�B
?�B
?�B
?�B
?�B
@�B
BB
A�B
B�B
B�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
H�B
H�B
H�B
H�B
IB
I�B
I�B
I�B
J�B
J�B
J�B
J�B
KB
K)B
LB
MB
L�B
MB
MB
MB
NB
NB
NB
M�B
N"B
O(B
OB
O(B
PB
P.B
PB
PB
QB
Q4B
Q4B
QB
Q B
R B
R B
R B
R B
RB
R B
R B
S&B
SB
SB
S&B
S&B
T,B
TB
T,B
U2B
U2B
U2B
UMB
U2B
UB
UB
UB
V9B
VSB
V9B
W?B
W?B
W?B
W?B
W$B
WYB
XEB
X+B
X+B
X+B
XB
X+B
X+B
X+B
XEB
XEB
XEB
Y1B
YKB
Y1B
YKB
ZQB
ZQB
ZQB
ZQB
ZB
ZQB
Z7B
[=B
[=B
[=B
[=B
[WB
\]B
\]B
\]B
\]B
\]B
\CB
]/B
]dB
]IB
]dB
]dB
]dB
^OB
^jB
^jB
^jB
^jB
^OB
_VB
_pB
_pB
_pB
_pB
`vB
`\B
`vB
`\B
`vB
`vB
`BB
`vB
`vB
abB
aHB
abB
aHB
a|B
a|B
b�B
bhB
b�B
c�B
cnB
cnB
cnB
cnB
c�B
cnB
cnB
c�B
c�B
d�B
d�B
e�B
e�B
ezB
e�B
e`B
ezB
ezB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
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
q�B
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
r�B
s�B
s�B
s�B
s�B
t�B
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
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
xB
xB
w�B
w�B
xB
y	B
y	B
y�B
y�B
y�B
zB
y�B
y�B
y�B
y�B
zB
z�B
{B
z�B
{B
{B
{B
|B
|B
|B
|B
|B
|B
|B
|B
}B
}B
}"B
}"B
}B
~B
~B
}�B
~B
~(B
~(B
~B
~(B
~B
~(B
~(B
B
~�B
B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.15(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201705110034392017051100343920170511003439201806221313052018062213130520180622131305201804050714312018040507143120180405071431  JA  ARFMdecpA19c                                                                20170507093510  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170507003530  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170507003531  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170507003532  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170507003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170507003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170507003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170507003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170507003533  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170507003533                      G�O�G�O�G�O�                JA  ARUP                                                                        20170507010810                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170507153614  CV  JULD            G�O�G�O�F�+�                JM  ARCAJMQC2.0                                                                 20170510153439  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170510153439  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221431  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041305  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                