CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-10-19T03:36:30Z creation;2016-10-19T03:36:33Z conversion to V3.1;2019-12-19T08:24:10Z update;     
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
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `l   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20161019033630  20200116211516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               0A   JA  I2_0577_048                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��u�Y��1   @��v����@3�vȴ9X�d��f�A�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fD  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ D�|�D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @)��@vff@�33@�33A��A=��A]��A}��A���A���A���A���A���A���A���A���BffBffBffBffB'ffB/ffB7ffB?ffBGffBOffBWffB_ffBgffBoffBwffBffB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��fB��3B��3B��3Bó3Bǳ3B˳3Bϳ3Bӳ3B׳3B۳3B߳3B�3B�3B�3B�3B�3B��3B�� B��3CٚCٚCٚCٚC	ٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚCٚC!ٚC#ٚC%ٚC'ٚC)ٚC+ٚC-ٚC/ٚC1ٚC3ٚC5ٚC7ٚC9ٚC;ٚC=ٚC?ٚCAٚCCٚCEٚCGٚCIٚCKٚCMٚCOٚCQٚCSٚCUٚCWٚCYٚC[ٚC]ٚC_ٚCaٚCcٚCeٚCgٚCiٚCkٚCmٚCoٚCqٚCsٚCuٚCwٚCyٚC{ٚC}ٚCٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D vfD �fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fD	vfD	�fD
vfD
�fDp D�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD�fDvfD��DvfD�fDvfD�fDvfD�fD vfD �fD!vfD!�fD"vfD"�fD#vfD#�fD$vfD$�fD%vfD%�fD&vfD&�fD'vfD'�fD(vfD(�fD)vfD)�fD*vfD*�fD+vfD+�fD,vfD,�fD-vfD-�fD.vfD.�fD/vfD/�fD0vfD0�fD1vfD1�fD2vfD2�fD3vfD3�fD4vfD4�fD5vfD5�fD6vfD6�fD7vfD7�fD8vfD8�fD9vfD9�fD:vfD:�fD;vfD;�fD<vfD<�fD=vfD=�fD>vfD>�fD?vfD?�fD@vfD@�fDAvfDA�fDBvfDB�fDCvfDC�fDDvfDD�fDEvfDE�fDFvfDF�fDGvfDG�fDHvfDH�fDIvfDI�fDJvfDJ�fDKvfDK��DLvfDL�fDMvfDM�fDNvfDN�fDOvfDO�fDPvfDP�fDQvfDQ�fDRvfDR�fDSvfDS�fDTvfDT�fDUvfDU�fDVvfDV�fDWvfDW�fDXvfDX�fDYvfDY�fDZvfDZ�fD[vfD[�fD\p D\�fD]vfD]�fD^vfD^�fD_vfD_�fD`vfD`�fDavfDa�fDbvfDb�fDc|�Dc�fDdvfDd�fDevfDe�fDfvfDf�fDgvfDg�fDhvfDh�fDivfDi�fDjvfDj�fDkvfDk�fDlvfDl�fDmvfDm�fDnvfDn�fDovfDo�fDpvfDp�fDqvfDq�fDrvfDr�fDsvfDs�fDtvfDt�fDuvfDu�fDvvfDv�fDwvfDw�fDxvfDx�fDyvfDy�fDzvfDz�fD{vfD{�fD|vfD|�fD}vfD}�fD~|�D~�fDvfD�fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�8 D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�>fD�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�>fD�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��fD�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D»3D��3D�;3D�{3Dû3D��3D�;3D�{3DĻ3D��3D�;3D�{3DŻ3D��3D�;3D�x Dƻ3D��3D�;3D�{3Dǻ3D��3D�;3D�{3DȻ3D��3D�;3D�{3Dɻ3D��3D�;3D�{3Dʻ3D��3D�;3D�{3D˻3D��3D�;3D�{3D̻3D��3D�;3D�{3Dͻ3D��3D�;3D�{3Dλ3D��3D�;3D�{3Dϻ3D��3D�;3D�{3Dл3D��3D�;3D�{3Dѻ3D��3D�;3D�{3Dһ3D��3D�;3D�{3Dӻ3D��3D�;3D�{3DԻ3D��3D�;3D�{3Dջ3D��3D�;3D�{3Dֻ3D��3D�;3D�{3D׻3D��3D�;3D�{3Dػ3D��3D�;3D�{3Dٻ3D��3D�;3D�{3Dڻ3D��3D�;3D�{3Dۻ3D��3D�;3D�{3Dܻ3D��3D�;3D�{3Dݻ3D��3D�;3D�{3D޻3D��3D�;3D�{3D߻3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D�3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��3D�;3D�{3D��3D��fD�8 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��#A��A��#A��#A��#A��#A��#A��#A��/A��/A��/A��#A��#A��/A��#A���Aާ�A�^5A�bA݁A���A�7LAۅA���Aڗ�Aډ7A�p�A���A��Aְ!AԾwA�7LA�Q�A�A�K�A�AΩ�A�S�Ḁ�A�O�A�|�A��A�ȴAāA�A�A�A�A���A��jA��A��A��A��FA�{A�ffA�ȴA��A���A���A���A��9A�JA��A�n�A��A�bNA��^A��`A��wA�%A�bNA��A��A��/A��`A�A�9XA���A�n�A���A�ĜA��`A�VA�E�A��
A���A�r�A��A��jA���A���A�K�A���A�x�A���A��mA���A�^5A���A�^5A��A���A��A�l�A�+A��PA�K�A���A�XA���A�#A|��A|A{K�Az�DAx �ArȴAol�An�\An9XAmdZAmAl$�Ai��Af�jAe��Ae�PAe&�Ad��A`v�A]��A]+A\�A\�AZ��AXQ�AV�AU�#AU�AT��AT��AS�#AQ�#APAN�RAM�-AL�+AKAI�-AHbNAGS�AFbNACx�ABE�AAƨA@ZA>��A;�hA9�A7��A6ffA5�FA533A5A4A�A3%A1�A0~�A.I�A-�A+x�A*�A*-A)l�A(�uA'�A&�A%t�A$�DA#�mA"�A ~�A I�A��Av�A~�A��Ax�AC�A�uA�FA�A  AffA�A1A`BA&�AĜAv�A{A�`A��A�A�A�!AbNAI�A(�A�-A
�+A	dZA��A?}A�`A��AQ�A��AȴA�A �D@�\)@�V@�$�@�hs@�S�@�$�@�~�@�;d@�-@�J@��@��@�"�@�ff@���@�j@�\)@�^@�E�@�x�@��#@�$�@�~�@�j@��@⟾@�@�@ޏ\@ܓu@�33@�n�@��`@�(�@��m@�+@��@���@֟�@�~�@��@�;d@љ�@���@�r�@ύP@�33@�-@́@��@�|�@���@�^5@���@ȴ9@�A�@�33@�5?@Ų-@�O�@�1'@� �@��@Ý�@��y@�V@�9X@�33@���@��\@�V@�-@���@�Ĝ@�(�@��
@��P@��@��P@��P@�dZ@��y@��!@�=q@�{@�@�x�@�%@��`@���@���@�Ĝ@���@���@�Q�@�l�@�"�@��H@���@�=q@���@�x�@�G�@��@��@��j@�I�@��m@�ƨ@��F@�S�@��@�E�@���@�X@���@��9@�A�@�|�@�"�@�;d@�;d@�;d@�C�@�C�@��@��R@��#@��/@��@�bN@�I�@�1@�t�@�@���@���@��+@�5?@���@��@��T@��^@��h@�p�@�G�@�&�@��@��/@���@�bN@�  @��w@���@���@��P@�|�@�\)@��@�=q@�@��^@��@��@���@��u@��D@��@�z�@�bN@�9X@��
@���@�;d@���@��H@���@��\@���@��@���@��`@���@�bN@���@�
=@��y@���@�n�@�$�@�$�@��@�J@�J@���@�`B@���@��@�b@��F@�|�@�t�@�+@��\@�E�@�5?@��@��@��@���@���@��j@�z�@�1'@���@�ƨ@��@�l�@�
=@���@�V@�-@�$�@�J@���@���@��7@��7@�x�@��`@��u@��D@�I�@��F@�t�@���@�n�@�J@���@���@�O�@�&�@��@��/@���@�I�@��w@��@�|�@�l�@�;d@��@���@�v�@�=q@��T@�X@�&�@���@���@�1'@�  @�S�@��@��!@�$�@���@��@�7L@��9@�1'@��F@�l�@�l�@�K�@�33@�
=@��@���@�V@�=q@�{@�@���@���@��h@��@�x�@�p�@�7L@��/@��D@�r�@�1'@���@���@�|�@�o@�@��@���@��+@�V@�=q@��@�{@���@��7@�/@��@��/@���@��j@�bN@� �@K�@~��@~V@}�h@|I�@{�F@{�@{dZ@{33@z�\@y��@y��@yx�@x��@x��@x  @w��@w+@v�R@v$�@t�@t��@t�/@t�@t�D@t(�@s��@s"�@r��@rM�@rJ@q��@rJ@q�^@p��@p �@o�@n��@n�+@nE�@m`B@l9X@k��@k�
@k�F@k��@kdZ@k33@j^5@i��@i&�@h��@h��@hr�@hQ�@hb@g�@g;d@g�@fȴ@f��@f��@f��@fff@fV@f5?@ep�@e/@d�@d��@d�/@d�@d�D@d�D@dj@dI�@d(�@c�m@c�@cS�@c"�@c@b��@a��@ax�@`Q�@`b@_�;@_l�@^��@^�y@^�R@^E�@^@]�h@]p�@]/@\��@\��@\�@\9X@\1@[��@[�m@[��@[o@Z�@YG�@Y%@X�9@Xb@W+@W
=@W
=@V�@V�@V�@V�@V�R@V��@V�+@V{@U�-@U�@T9X@S��@St�@SS�@S"�@R�\@RJ@Q7L@P�`@P�@P1'@O��@OK�@M@M`B@M/@L�/@L�j@LI�@Kt�@K33@K@J�!@J��@J=q@JJ@I�7@I%@HĜ@H1'@G��@G|�@G|�@Gl�@G\)@G;d@F��@Fv�@FV@FE�@F@E@Ep�@E?}@D�/@DI�@D(�@D1@C��@CdZ@Co@B��@BM�@A��@A�^@A�7@AX@@��@@bN@@Q�@@ �@?�;@?�w@?�@?+@>ff@>5?@>{@>@=@=?}@<�/@<�j@<z�@;��@;t�@;C�@:~�@9��@9�^@9hs@9�@8��@8�`@8�@8b@7�;@7|�@7K�@7+@6�y@6�+@6$�@5�@5��@5��@5p�@4�D@3�m@3o@2��@2�\@2n�@2M�@2=q@2-@2�@1��@1�#@1��@1��@1��@1x�@1&�@0Ĝ@0r�@0A�@0b@/�;@/��@/��@/\)@/;d@/�@.�@.�R@.ff@.5?@.{@-�-@,�@,�D@,j@,I�@,1@+ƨ@+��@+t�@+S�@+o@*��@*^5@*=q@)�@)��@)hs@)�@(bN@(b@( �@'�@'�w@'�P@'K�@'�@&ȴ@&��@&V@&@%��@%@%��@%��@%��@%�h@%p�@%O�@%/@$�@$�@$z�@$�@#�
@#�@#S�@#33@#"�@"��@"~�@"M�@!��@!�@ �`@ Ĝ@ �9@ ��@ �u@ �@ A�@�w@
=@�@��@ff@ff@ff@ff@ff@V@5?@�-@p�@/@/@��@�@�@z�@Z@I�@I�@I�@(�@(�@�@�m@�@C�@"�@@��@^5@�@��@G�@&�@%@��@�9@�@r�@A�@b@�@�@\)@+@�@��@��@��@��@��@�+@ff@ff@E�@5?@$�@@��@�-@�h@O�@��@��@�F@33@�H@�!@��@~�@n�@n�@^5@M�@�@�#@��@�7@X@7L@�@�`@��@�u@�@bN@1'@1'@ �@ �@b@�w@��@�P@l�@;d@�y@��@��@ff@V@$�@�@�T@@�-@��@`B@?}@/@�@�@��@�j@�j@�j@�j@�@�@�@z�@j@Z@I�@(�@�
@��@t�@S�@C�@o@
�@
�H@
��@
��@
�!@
��@
��@
n�@
=q@
=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��#A��A��#A��#A��#A��#A��#A��#A��/A��/A��/A��#A��#A��/A��#A���Aާ�A�^5A�bA݁A���A�7LAۅA���Aڗ�Aډ7A�p�A���A��Aְ!AԾwA�7LA�Q�A�A�K�A�AΩ�A�S�Ḁ�A�O�A�|�A��A�ȴAāA�A�A�A�A���A��jA��A��A��A��FA�{A�ffA�ȴA��A���A���A���A��9A�JA��A�n�A��A�bNA��^A��`A��wA�%A�bNA��A��A��/A��`A�A�9XA���A�n�A���A�ĜA��`A�VA�E�A��
A���A�r�A��A��jA���A���A�K�A���A�x�A���A��mA���A�^5A���A�^5A��A���A��A�l�A�+A��PA�K�A���A�XA���A�#A|��A|A{K�Az�DAx �ArȴAol�An�\An9XAmdZAmAl$�Ai��Af�jAe��Ae�PAe&�Ad��A`v�A]��A]+A\�A\�AZ��AXQ�AV�AU�#AU�AT��AT��AS�#AQ�#APAN�RAM�-AL�+AKAI�-AHbNAGS�AFbNACx�ABE�AAƨA@ZA>��A;�hA9�A7��A6ffA5�FA533A5A4A�A3%A1�A0~�A.I�A-�A+x�A*�A*-A)l�A(�uA'�A&�A%t�A$�DA#�mA"�A ~�A I�A��Av�A~�A��Ax�AC�A�uA�FA�A  AffA�A1A`BA&�AĜAv�A{A�`A��A�A�A�!AbNAI�A(�A�-A
�+A	dZA��A?}A�`A��AQ�A��AȴA�A �D@�\)@�V@�$�@�hs@�S�@�$�@�~�@�;d@�-@�J@��@��@�"�@�ff@���@�j@�\)@�^@�E�@�x�@��#@�$�@�~�@�j@��@⟾@�@�@ޏ\@ܓu@�33@�n�@��`@�(�@��m@�+@��@���@֟�@�~�@��@�;d@љ�@���@�r�@ύP@�33@�-@́@��@�|�@���@�^5@���@ȴ9@�A�@�33@�5?@Ų-@�O�@�1'@� �@��@Ý�@��y@�V@�9X@�33@���@��\@�V@�-@���@�Ĝ@�(�@��
@��P@��@��P@��P@�dZ@��y@��!@�=q@�{@�@�x�@�%@��`@���@���@�Ĝ@���@���@�Q�@�l�@�"�@��H@���@�=q@���@�x�@�G�@��@��@��j@�I�@��m@�ƨ@��F@�S�@��@�E�@���@�X@���@��9@�A�@�|�@�"�@�;d@�;d@�;d@�C�@�C�@��@��R@��#@��/@��@�bN@�I�@�1@�t�@�@���@���@��+@�5?@���@��@��T@��^@��h@�p�@�G�@�&�@��@��/@���@�bN@�  @��w@���@���@��P@�|�@�\)@��@�=q@�@��^@��@��@���@��u@��D@��@�z�@�bN@�9X@��
@���@�;d@���@��H@���@��\@���@��@���@��`@���@�bN@���@�
=@��y@���@�n�@�$�@�$�@��@�J@�J@���@�`B@���@��@�b@��F@�|�@�t�@�+@��\@�E�@�5?@��@��@��@���@���@��j@�z�@�1'@���@�ƨ@��@�l�@�
=@���@�V@�-@�$�@�J@���@���@��7@��7@�x�@��`@��u@��D@�I�@��F@�t�@���@�n�@�J@���@���@�O�@�&�@��@��/@���@�I�@��w@��@�|�@�l�@�;d@��@���@�v�@�=q@��T@�X@�&�@���@���@�1'@�  @�S�@��@��!@�$�@���@��@�7L@��9@�1'@��F@�l�@�l�@�K�@�33@�
=@��@���@�V@�=q@�{@�@���@���@��h@��@�x�@�p�@�7L@��/@��D@�r�@�1'@���@���@�|�@�o@�@��@���@��+@�V@�=q@��@�{@���@��7@�/@��@��/@���@��j@�bN@� �@K�@~��@~V@}�h@|I�@{�F@{�@{dZ@{33@z�\@y��@y��@yx�@x��@x��@x  @w��@w+@v�R@v$�@t�@t��@t�/@t�@t�D@t(�@s��@s"�@r��@rM�@rJ@q��@rJ@q�^@p��@p �@o�@n��@n�+@nE�@m`B@l9X@k��@k�
@k�F@k��@kdZ@k33@j^5@i��@i&�@h��@h��@hr�@hQ�@hb@g�@g;d@g�@fȴ@f��@f��@f��@fff@fV@f5?@ep�@e/@d�@d��@d�/@d�@d�D@d�D@dj@dI�@d(�@c�m@c�@cS�@c"�@c@b��@a��@ax�@`Q�@`b@_�;@_l�@^��@^�y@^�R@^E�@^@]�h@]p�@]/@\��@\��@\�@\9X@\1@[��@[�m@[��@[o@Z�@YG�@Y%@X�9@Xb@W+@W
=@W
=@V�@V�@V�@V�@V�R@V��@V�+@V{@U�-@U�@T9X@S��@St�@SS�@S"�@R�\@RJ@Q7L@P�`@P�@P1'@O��@OK�@M@M`B@M/@L�/@L�j@LI�@Kt�@K33@K@J�!@J��@J=q@JJ@I�7@I%@HĜ@H1'@G��@G|�@G|�@Gl�@G\)@G;d@F��@Fv�@FV@FE�@F@E@Ep�@E?}@D�/@DI�@D(�@D1@C��@CdZ@Co@B��@BM�@A��@A�^@A�7@AX@@��@@bN@@Q�@@ �@?�;@?�w@?�@?+@>ff@>5?@>{@>@=@=?}@<�/@<�j@<z�@;��@;t�@;C�@:~�@9��@9�^@9hs@9�@8��@8�`@8�@8b@7�;@7|�@7K�@7+@6�y@6�+@6$�@5�@5��@5��@5p�@4�D@3�m@3o@2��@2�\@2n�@2M�@2=q@2-@2�@1��@1�#@1��@1��@1��@1x�@1&�@0Ĝ@0r�@0A�@0b@/�;@/��@/��@/\)@/;d@/�@.�@.�R@.ff@.5?@.{@-�-@,�@,�D@,j@,I�@,1@+ƨ@+��@+t�@+S�@+o@*��@*^5@*=q@)�@)��@)hs@)�@(bN@(b@( �@'�@'�w@'�P@'K�@'�@&ȴ@&��@&V@&@%��@%@%��@%��@%��@%�h@%p�@%O�@%/@$�@$�@$z�@$�@#�
@#�@#S�@#33@#"�@"��@"~�@"M�@!��@!�@ �`@ Ĝ@ �9@ ��@ �u@ �@ A�@�w@
=@�@��@ff@ff@ff@ff@ff@V@5?@�-@p�@/@/@��@�@�@z�@Z@I�@I�@I�@(�@(�@�@�m@�@C�@"�@@��@^5@�@��@G�@&�@%@��@�9@�@r�@A�@b@�@�@\)@+@�@��@��@��@��@��@�+@ff@ff@E�@5?@$�@@��@�-@�h@O�@��@��@�F@33@�H@�!@��@~�@n�@n�@^5@M�@�@�#@��@�7@X@7L@�@�`@��@�u@�@bN@1'@1'@ �@ �@b@�w@��@�P@l�@;d@�y@��@��@ff@V@$�@�@�T@@�-@��@`B@?}@/@�@�@��@�j@�j@�j@�j@�@�@�@z�@j@Z@I�@(�@�
@��@t�@S�@C�@o@
�@
�H@
��@
��@
�!@
��@
��@
n�@
=q@
=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BN�BN�BN�BN�BO�BO�BO�BO�BO�BO�BO�BP�BP�BP�BR�B^5Bu�B��BĜB�B�B'�B1'BL�BYB]/BaHB_;BR�BF�B;dBD�BN�BK�BaHB~�B��B��B��B��B��B�BÖB��B��BɺB�
B�
B��B��B�B�/B�5B�BB�HB�NB�ZB�ZB�NB�HB�5B�B�B��B��B��BƨB��B�jB�-B�B��B��B��B��B�oB�PB�1B~�Bt�Bn�BbNB^5BS�BF�B6FB33B �B\B�B�`B�;B�B�qB��B�{Bv�BbNB<jB.B�BbB
��B
�B
�}B
�?B
�B
�B
��B
�hB
�1B
o�B
dZB
\)B
Q�B
A�B
�B	��B	��B	�B	�B	��B	��B	�B	�;B	�B	��B	��B	��B	�dB	��B	��B	��B	��B	�\B	�B	y�B	u�B	s�B	q�B	o�B	jB	_;B	W
B	M�B	I�B	A�B	=qB	49B	.B	&�B	!�B	�B	PB	
=B	B��B�B�`B�5B�B�
B��B��B��B��BƨBB�qB�XB�9B�'B�B�B�B�'B�!B�B��B�B��B��B��B��B��B��B��B��B��B��B�{B�uB�VB�7B�B�B�B�B�B�B~�B~�B~�B�B�%B�B�B�B� B}�Bx�Bv�Bt�Bs�Bu�Bs�Bs�Bx�Bu�Bx�Bt�Br�Br�Bv�Bv�Bz�B�B�oB�{B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B�?B�jB�}B��BBŢBĜBȴB��B��B��B��B�/B�ZB�ZB�B��B��B	  B	%B	JB	\B	uB	{B	�B	�B	�B	 �B	!�B	$�B	.B	1'B	5?B	8RB	8RB	8RB	9XB	<jB	@�B	B�B	D�B	E�B	F�B	F�B	F�B	H�B	N�B	P�B	Q�B	P�B	P�B	T�B	YB	[#B	]/B	_;B	bNB	dZB	ffB	k�B	t�B	x�B	z�B	{�B	|�B	|�B	}�B	}�B	~�B	� B	� B	�B	�%B	�+B	�+B	�7B	�DB	�oB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�3B	�3B	�FB	�LB	�LB	�RB	�XB	�^B	�dB	�dB	�jB	�qB	�wB	��B	��B	��B	B	ƨB	ɺB	��B	��B	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�TB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�sB	�sB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
+B
1B
1B
1B
	7B
	7B

=B

=B

=B
	7B
DB
JB
JB
PB
PB
PB
PB
JB
DB
DB
DB
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
\B
bB
bB
bB
hB
hB
hB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
$�B
%�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
'�B
'�B
'�B
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
)�B
)�B
)�B
+B
)�B
+B
+B
+B
+B
+B
,B
-B
-B
,B
,B
-B
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
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
9XB
:^B
:^B
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
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
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
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
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
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
R�B
R�B
R�B
S�B
T�B
T�B
T�B
VB
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
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
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
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
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
hsB
hsB
iyB
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
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
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
x�B
x�B
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
y�B
y�B
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
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BN�BN�BN�BN�BO�BO�BO�BO�BO�BO�BO�BP�BQ BQBS[B^�Bv�B��B�B�?B�B)DB2�BMBYB]�Bb�Bb4BW�BJ�B>�BFtBPBMjBdtB��B��B�8B� B�nB�B�TB�_BˬB̳B�VB�WB�#B��BּBܒB�'B�HB��B��B�@B�8B��B�B�B�'B��B��B�TB�\B̘B�B�gB��B��B��B�eB�B�B��B��B�(B��B�Bv�Bp�Bd�B`�BW$BI�B9�B6�B$&B�B��B�B��B�B��B�*B��B{Bg�B?�B0�B7B�B B
��B
��B
�+B
�UB
��B
��B
��B
��B
qB
e�B
^B
U�B
GB
!-B
 OB	��B	��B	��B	�zB	��B	�vB	�\B	��B	�B	��B	�(B	��B	��B	��B	�B	��B	�B	�B	z�B	vzB	t�B	r�B	qB	mB	a�B	X�B	O\B	K^B	CB	?�B	6B	/�B	(�B	$�B	B	�B	dB	�B��B��B�RB߾B�#B��BԕB�,BҽB��BȀB�9B�HB�JB�tB�B�5B�CB�OB��B�B�cB�kB�]B��B��B��B�B��B��B�#B�7B��B�EB��B�9B�}B�)B�tB��B��B��B��B��B��B�OB�B��B��B��B�uB��B� B�Bz�Bx�Bu�Bt�BwfBu�BuBz*BwLBz�BvFBtnBtnBw�Bx8B{�B��B�,B�2B��B�=B��B��B�]B�]B�qB��B�B�mB�B��B��B�!B�$B��B�B��B�B�eB�XB��B��B��B�fB�_B��B��B��B��B�B��B��BżB�RB�DB�~B�bBҽB��B��B�zB�B�`B�xB	 �B	�B	B	B	�B	�B	KB	�B	 B	!HB	"�B	&B	.�B	1�B	5�B	8�B	8�B	8�B	9�B	=B	@�B	B�B	D�B	E�B	F�B	F�B	F�B	I7B	O(B	QNB	R:B	Q4B	QhB	UMB	YKB	[WB	]dB	_pB	b�B	d�B	gB	l"B	uB	y$B	{JB	|6B	}VB	}<B	~(B	~(B	HB	�OB	�iB	�gB	�YB	�zB	�zB	��B	��B	��B	��B	��B	��B	�B	�5B	��B	��B	��B	��B	��B	�B	�_B	�kB	��B	��B	�[B	�GB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	��B	��B	��B	��B	�B	�JB	�<B	� B	�@B	�FB	�gB	�MB	�?B	�?B	�?B	�$B	�?B	�YB	�yB	�eB	چB	�qB	�WB	�qB	یB	��B	ݘB	�IB	�IB	ބB	ޞB	߾B	�B	�bB	�B	�B	�B	�zB	�zB	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�B	�	B	�	B	�	B	�*B	�^B	�*B	��B	�JB	�jB	�<B	�cB	�}B	�cB
 4B
UB
;B
AB
AB
AB
GB
gB
�B
_B
fB
fB
�B
	�B
	lB

�B

rB

�B
	�B
�B
~B
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
�B
VB
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
!B
!B
�B
�B
�B
 B
 �B
!B
 �B
 �B
!B
 �B
"B
"B
!�B
"B
#:B
#:B
#�B
%B
%B
&B
%B
%,B
%,B
%B
%,B
$�B
%B
%�B
(>B
(>B
(XB
'RB
($B
($B
(>B
(XB
(XB
)*B
)*B
)*B
)*B
*0B
*KB
*eB
+QB
*KB
+6B
+B
+B
+B
+B
,WB
-CB
-)B
,=B
,=B
-B
,"B
-CB
-)B
-CB
-]B
-CB
.IB
./B
/B
/OB
/OB
/5B
/OB
0;B
0UB
0UB
0UB
1[B
1[B
1[B
1vB
1�B
1vB
2�B
3hB
3hB
3�B
4nB
4nB
4TB
4nB
4nB
5tB
5ZB
5ZB
5ZB
6zB
6zB
6�B
6`B
6`B
6zB
6�B
6�B
7�B
7�B
8�B
8�B
8�B
9�B
:xB
:^B
9rB
:xB
:xB
:xB
:�B
:�B
:xB
:�B
:�B
:�B
:�B
;�B
;�B
;B
;�B
;�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>B
?�B
?�B
?�B
?�B
?�B
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
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
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
H�B
H�B
H�B
H�B
H�B
IB
I�B
I�B
I�B
I�B
J�B
J�B
KB
KB
K�B
K�B
K�B
K�B
LB
L�B
MB
MB
MB
N"B
NB
N<B
O(B
N�B
N�B
O�B
PB
O�B
P.B
P.B
QB
QB
QB
QB
Q B
R B
R B
RB
RB
SB
S&B
S@B
TFB
UMB
U2B
U2B
VB
VB
VB
VB
VB
V9B
W?B
W
B
W?B
W?B
W?B
W?B
W?B
X+B
XEB
XEB
XEB
XEB
X+B
X+B
YKB
YKB
Y1B
YKB
Y1B
Z7B
Z7B
ZkB
ZkB
[WB
[WB
[WB
[WB
\]B
\]B
\]B
\]B
\]B
]dB
]dB
]IB
]dB
^jB
^OB
^jB
^�B
_pB
_VB
_pB
_pB
_pB
_pB
`vB
`vB
`vB
`vB
a|B
a|B
abB
abB
abB
abB
aHB
a|B
a|B
abB
bhB
bhB
b�B
b�B
c�B
c�B
cnB
cnB
c�B
d�B
dtB
d�B
d�B
e�B
ezB
ezB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
gmB
g�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
iyB
i�B
i�B
i�B
j�B
j�B
jB
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
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
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
rB
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
xB
w�B
xB
xB
xB
y	B
x�B
x�B
x�B
x�B
y	B
y�B
y�B
y�B
zB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
y�B
y�B
zB
zB
zB
zB
y�B
z�B
z�B
{B
{B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
|B
|111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.15(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201610190040392016101900403920161019004039201806221303342018062213033420180622130334201804050703142018040507031420180405070314  JA  ARFMdecpA19c                                                                20161019123515  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161019033630  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161019033631  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161019033631  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161019033632  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161019033632  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161019033632  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161019033632  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20161019033633                      G�O�G�O�G�O�                JA  ARUP                                                                        20161019043709                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161017155236  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161018154039  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161018154039  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220314  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040334  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211516                      G�O�G�O�G�O�                